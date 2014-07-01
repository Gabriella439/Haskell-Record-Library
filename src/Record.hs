{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative ((<$>), (<*>))
import Control.Concurrent.Async (race_)
import Control.Exception (SomeException, catch, finally, throwIO)
import qualified Data.ByteString as ByteString
import Data.Foldable (forM_)
import Filesystem (getHomeDirectory, withFile)
import Filesystem.Path ((</>))
import Pipes (runEffect, for, lift)
import Pipes.ByteString (fromHandle)
import System.Environment (getArgs)
import System.Process
import qualified System.IO as IO

format :: String -> String
format = unlines . transform . lines
  where
    transform (l:ls) = l:map ("         " ++) ls
    transform  []    = [""]

main :: IO ()
main = do
    args <- getArgs
    home <- getHomeDirectory
    withFile (home </> ".record") IO.AppendMode $ \handle -> ((do
        let run label cmd args' = do
                let trim (_, x, y) = (x, y)

                IO.hPutStr handle (label ++ ": ")
                (i, e) <- fmap trim (readProcessWithExitCode cmd args' "")
                    `catch` (\e -> do
                        let _ = e :: SomeException
                        return ("", show e) )
                IO.hPutStr handle (if null e then format i else "N/A\n")

        run "Date   " "date" []
        run "User   " "whoami" []
        run "Host   " "hostname" []
        run "PWD    " "pwd" []
        run "Branch " "git" ["rev-parse", "--abbrev-ref", "HEAD"]
        run "SHA    " "git" ["rev-parse", "--verify", "HEAD"]
        run "Diff   " "git" ["diff"]

        IO.hPutStrLn handle ("Command: " ++ unwords args)
        case args of
            []       -> putStrLn "Usage: record COMMAND [ARGUMENT ...]"
            cmd:rest -> do
                (_, mOut , mErr, _) <- createProcess (proc cmd rest)
                    { std_out = CreatePipe, std_err = CreatePipe }
                forM_ ((,) <$> mOut <*> mErr) $ \(hOut, hErr) -> do
                    let redirect hSrc hDest = do
                            IO.hSetBuffering hSrc  IO.NoBuffering
                            IO.hSetBuffering hDest IO.NoBuffering
                            runEffect $ for (fromHandle hSrc) $ \bs -> lift $ do
                                ByteString.hPutStr hDest  bs
                                ByteString.hPutStr handle bs
                    redirect hOut IO.stdout `race_` redirect hErr IO.stderr )
        `finally` (IO.hPutStrLn handle "") )
        `catch` (\e -> do
            IO.hPrint handle (e :: SomeException)
            throwIO e )

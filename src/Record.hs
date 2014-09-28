{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent.Async (race_)
import Control.Exception (SomeException, catch, finally, throwIO)
import Control.Monad (forM_)
import Filesystem (getHomeDirectory, withFile)
import Filesystem.Path ((</>))
import System.Environment (getArgs)
import System.Process
import System.IO hiding (withFile)

indent :: String -> String
indent = unlines . transform . lines
  where
    transform (l:ls) = l:map ("         " ++) ls
    transform  []    = [""]

-- | Run the supplied command, logging useful context and output to `~/.record`
main :: IO ()
main = do
    args <- getArgs
    home <- getHomeDirectory
    withFile (home </> ".record") AppendMode $ \handle -> ((do
        let run label cmd args' = do
                let trim (_, x, y) = (x, y)

                hPutStr handle (label ++ ": ")
                (i, e) <- fmap trim (readProcessWithExitCode cmd args' "")
                    `catch` (\e -> do
                        let _ = e :: SomeException
                        return ("", show e) )
                hPutStr handle (if null e then indent i else "N/A\n")

        -- Log context
        run "Date   " "date"     []
        run "User   " "whoami"   []
        run "Host   " "hostname" []
        run "PWD    " "pwd"      []
        run "Branch " "git"      ["rev-parse", "--abbrev-ref", "HEAD"]
        run "SHA    " "git"      ["rev-parse", "--verify", "HEAD"]
        run "Diff   " "git"      ["diff"]

        hPutStrLn handle ("Command: " ++ unwords args)

        case args of
            []       -> putStrLn "Usage: record COMMAND [ARGUMENT ...]"
            cmd:rest -> do
                -- Log output
                (_, Just out, Just err, _) <- createProcess (proc cmd rest) { std_out = CreatePipe, std_err = CreatePipe }

                let redirect src dest = do
                        hSetBuffering src  NoBuffering
                        hSetBuffering dest NoBuffering
                        bs <- hGetContents src
                        forM_ (lines bs) $ \line -> do
                            hPutStrLn dest   line
                            hPutStrLn handle line

                redirect out stdout `race_` redirect err stderr )
        `finally` (hPutStrLn handle "") )
        `catch` (\e -> do
            hPrint handle (e :: SomeException)
            throwIO e )

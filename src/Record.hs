{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative ((<$>), (<*>))
import Control.Category (id)
import Control.Exception (SomeException, catch, throwIO)
import Control.Monad.Trans.Reader (ReaderT(ReaderT), runReaderT)
import Filesystem (getHomeDirectory, withFile)
import Filesystem.Path ((</>))
import MVC (runMVC, Buffer(Single), asSink, (<>))
import MVC.Prelude (producer, stdoutLines)
import qualified Pipes.Prelude as Pipes
import System.Environment (getArgs)
import System.Process
import System.IO (Handle, IOMode(AppendMode), hPrint, hPutStr, hPutStrLn)
import Prelude hiding (id, FilePath)

run :: String -> String -> [String] -> ReaderT Handle IO ()
run label cmd args = ReaderT $ \handle -> do
    hPutStr handle (label ++ ": ")
    (i, e) <- fmap (\(_, x, y) -> (x, y)) (readProcessWithExitCode cmd args "")
        `catch` (\e -> do
            let _ = e :: SomeException
            return ("", show e) )
    hPutStr handle (if null e then format i else "N/A\n")

branch :: ReaderT Handle IO ()
branch = run "Branch " "git" ["rev-parse", "--abbrev-ref", "HEAD"]

sha :: ReaderT Handle IO ()
sha = run "SHA    " "git" ["rev-parse", "--verify", "HEAD"]

diff :: ReaderT Handle IO ()
diff = run "Diff   " "git" ["diff"]

hostname :: ReaderT Handle IO ()
hostname = run "Host   " "hostname" []

pwd :: ReaderT Handle IO ()
pwd = run "PWD    " "pwd" []

time :: ReaderT Handle IO ()
time = run "Date   " "date" []

user :: ReaderT Handle IO ()
user = run "User   " "whoami" []

format :: String -> String
format = unlines . transform . lines
  where
    transform (l:ls) = l:map ("         " ++) ls
    transform  x     = x

-- The command to run
main :: IO ()
main = do
    args <- getArgs
    home <- getHomeDirectory
    withFile (home </> ".record") AppendMode $ \handle -> (do
        flip runReaderT handle $ do
            time
            user
            hostname
            pwd
            branch
            sha
            diff
        hPutStrLn handle ("Command: " ++ unwords args)
        case args of
            []       -> return ()
            cmd:rest -> do
                (_, mOut , mErr, _) <- createProcess (proc cmd rest)
                    { std_out = CreatePipe, std_err = CreatePipe }
                case (,) <$> mOut <*> mErr of
                    Nothing           -> return ()
                    Just (hOut, hErr) -> do
                        runMVC () id $ do
                            c <- fmap (fmap ("O: " ++)) (producer Single (Pipes.fromHandle hOut))
                              <> fmap (fmap ("E: " ++)) (producer Single (Pipes.fromHandle hErr))
                            let v = asSink (hPutStrLn handle)
                                 <> stdoutLines
                            return (v, c)

        hPutStrLn handle "" )
        `catch` (\e -> do
            hPrint handle (e :: SomeException)
            hPutStrLn handle ""
            throwIO e )

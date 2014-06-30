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
import qualified System.IO as IO
import Prelude hiding (id, FilePath)

run :: String -> String -> [String] -> ReaderT IO.Handle IO ()
run label cmd args = ReaderT $ \handle -> do
    IO.hPutStr handle (label ++ ": ")
    (i, e) <- fmap (\(_, x, y) -> (x, y)) (readProcessWithExitCode cmd args "")
        `catch` (\e -> do
            let _ = e :: SomeException
            return ("", show e) )
    IO.hPutStr handle (if null e then format i else "N/A\n")

branch :: ReaderT IO.Handle IO ()
branch = run "Branch " "git" ["rev-parse", "--abbrev-ref", "HEAD"]

sha :: ReaderT IO.Handle IO ()
sha = run "SHA    " "git" ["rev-parse", "--verify", "HEAD"]

diff :: ReaderT IO.Handle IO ()
diff = run "Diff   " "git" ["diff"]

hostname :: ReaderT IO.Handle IO ()
hostname = run "Host   " "hostname" []

pwd :: ReaderT IO.Handle IO ()
pwd = run "PWD    " "pwd" []

time :: ReaderT IO.Handle IO ()
time = run "Date   " "date" []

user :: ReaderT IO.Handle IO ()
user = run "User   " "whoami" []

format :: String -> String
format = unlines . transform . lines
  where
    transform (l:ls) = l:map ("         " ++) ls
    transform  []    = [""]

-- The command to run
main :: IO ()
main = do
    IO.hSetBuffering IO.stdout IO.NoBuffering
    IO.hSetBuffering IO.stderr IO.NoBuffering
    args <- getArgs
    home <- getHomeDirectory
    withFile (home </> ".record") IO.AppendMode $ \handle -> (do
        flip runReaderT handle $ do
            time
            user
            hostname
            pwd
            branch
            sha
            diff
        IO.hPutStrLn handle ("Command: " ++ unwords args)
        case args of
            []       -> return ()
            cmd:rest -> do
                (_, mOut , mErr, _) <- createProcess (proc cmd rest)
                    { std_out = CreatePipe, std_err = CreatePipe }
                case (,) <$> mOut <*> mErr of
                    Nothing           -> return ()
                    Just (hOut, hErr) -> do
                        runMVC () id $ do
                            let stdOut = producer Single (Pipes.fromHandle hOut)
                                stdErr = producer Single (Pipes.fromHandle hErr)
                            c <- fmap (fmap ("O: " ++)) stdOut
                              <> fmap (fmap ("E: " ++)) stdErr
                            let v = asSink (IO.hPutStrLn handle)
                                 <> stdoutLines
                            return (v, c)

        IO.hPutStrLn handle "" )
        `catch` (\e -> do
            IO.hPrint handle (e :: SomeException)
            IO.hPutStrLn handle ""
            throwIO e )

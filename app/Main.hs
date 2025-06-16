{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Main (main) where

import System.Environment (getArgs)
import Logging (defaultConfig, initLogging, closeLogging, LogLevel(..), logLevel)
import Control.Monad.Except (runExceptT)
import Control.Monad.Reader (runReaderT)
import CLI.Command (parseArgs)
import CLI.Runner (runCommand, AppEnv(..))
import System.Exit (exitWith, ExitCode(..))

main :: IO ()
main = do
  args <- getArgs
  logState <- initLogging defaultConfig { logLevel = INFO }
  let env = AppEnv logState
  result <- runExceptT $ runReaderT (runCommand $ parseArgs args) env
  case result of
    Left err -> do
      putStrLn $ "Error: " ++ err
      closeLogging logState
      exitWith (ExitFailure 1)
    Right _ -> do
      closeLogging logState
      exitWith ExitSuccess


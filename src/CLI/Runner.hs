{-# LANGUAGE LambdaCase #-}

module CLI.Runner (
    runCommand,
    AppEnv(..)
) where

import CLI.Command (Command(..), GenerateOptions(..), AppEnv(..), App)
import CLI.Generate (runGenerate, runMinimalGenerate, runFullGenerate)
import Logging (logInfo, closeLogging)
import CLI.Help (helpMessage, manualMessage, version)
import qualified CLI.Wizard as W
import System.Directory (getCurrentDirectory)
import Control.Monad.Reader (ask)
import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as T

runCommand :: Command -> App ()
runCommand = \case
  Help -> liftIO $ putStrLn helpMessage
  Version -> liftIO $ putStr version
  Manual -> liftIO $ putStrLn manualMessage
  Init -> runInit
  GenerateGrammar out -> runGenerateGrammar out
  Generate opts -> runGenerate opts

runInit :: App ()
runInit = do
  currentDir <- liftIO getCurrentDirectory
  liftIO $ putStrLn "Initializing hix configuration..."
  liftIO $ W.createDefaultConfig currentDir
  liftIO $ putStrLn "Configuration created successfully!"
  liftIO $ putStrLn "You can now customize the layers and templates in .hix/config.yaml"

runGenerateGrammar :: Maybe String -> App ()
runGenerateGrammar = \case
  Nothing -> liftIO $ putStrLn "Please provide an output file, e.g. hix --gen-grammar hix.tmLanguage.json"
  Just out -> do
    env <- ask
    liftIO $ logInfo (logState env) "Grammar" (T.pack ("Generating grammar file: " ++ out)) Nothing
    liftIO $ putStrLn $ "Grammar file generated: " ++ out 
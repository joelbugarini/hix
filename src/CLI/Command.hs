{-# LANGUAGE LambdaCase #-}

module CLI.Command (
    Command(..),
    GenerateOptions(..),
    AppEnv(..),
    App,
    parseArgs
) where

import Logging (LogState)
import qualified Config.Config as C
import Data.List (dropWhile)
import Control.Monad.Reader (ReaderT)
import Control.Monad.Except (ExceptT)

-- Command data type
data Command = Help
             | Version
             | Manual
             | Init
             | GenerateGrammar (Maybe String)
             | Generate GenerateOptions
             deriving (Show)

data GenerateOptions = GenerateOptions
  { modelPath :: Maybe String
  , layerName :: Maybe String
  , templatePath :: Maybe String
  } deriving (Show)

-- Environment for our application
data AppEnv = AppEnv
  { logState :: LogState
  }

-- Application monad stack
type App a = ReaderT AppEnv (ExceptT String IO) a

-- Parse command line arguments into Command type
parseArgs :: [String] -> Command
parseArgs = \case
  [] -> Help
  ["--help"] -> Help
  ["help"] -> Help
  ["--version"] -> Version
  ["version"] -> Version
  ["man"] -> Manual
  ["init"] -> Init
  ["--gen-grammar"] -> GenerateGrammar Nothing
  ["generate-grammar"] -> GenerateGrammar Nothing
  ["--gen-grammar", out] -> GenerateGrammar (Just out)
  ["generate-grammar", out] -> GenerateGrammar (Just out)
  "generate":rest -> Generate $ parseGenerateOptions rest
  _ -> Help

-- Parse generate command options
parseGenerateOptions :: [String] -> GenerateOptions
parseGenerateOptions args = GenerateOptions
  { modelPath = findArg "--model" args
  , layerName = findArg "--layer" args
  , templatePath = findArg "--template" args
  }

-- Helper to find argument value
findArg :: String -> [String] -> Maybe String
findArg flag xs = case dropWhile (/= flag) xs of
  (_:value:_) -> Just value
  _ -> Nothing 
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Config where

import Data.Yaml (FromJSON(..), decodeFileEither, withObject, (.:), (.:?))
import GHC.Generics (Generic)
import Data.Text (Text)
import Data.Maybe (fromMaybe)
import Data.Aeson (genericParseJSON, defaultOptions)

-- Data types for the config schema
data Config = Config
  { architecture :: Text
  , output_root :: Maybe FilePath
  , layers :: [Layer]
  , models_path :: Maybe FilePath
  , templates_root :: Maybe FilePath
  , naming :: Maybe Naming
  , options :: Maybe Options
  } deriving (Show, Generic)

data Layer = Layer
  { name :: Text
  , path :: FilePath
  , description :: Text
  , templates :: [Template]
  } deriving (Show, Generic)

data Template = Template
  { template :: FilePath
  , filename :: Text
  , output_by :: Text
  } deriving (Show, Generic)

data Naming = Naming
  { model :: Text
  , property :: Text
  } deriving (Show, Generic)

data Options = Options
  { overwrite_existing :: Bool
  , dry_run :: Bool
  } deriving (Show, Generic)

-- Instances for YAML parsing
instance FromJSON Config where
  parseJSON = genericParseJSON defaultOptions

instance FromJSON Layer where
  parseJSON = genericParseJSON defaultOptions

instance FromJSON Template where
  parseJSON = genericParseJSON defaultOptions

instance FromJSON Naming where
  parseJSON = genericParseJSON defaultOptions

instance FromJSON Options where
  parseJSON = genericParseJSON defaultOptions

-- Function to load the config file
loadConfig :: FilePath -> IO (Either String Config)
loadConfig path = do
  result <- decodeFileEither path
  return $ case result of
    Left err -> Left $ "Failed to parse config: " ++ show err
    Right config -> Right config 
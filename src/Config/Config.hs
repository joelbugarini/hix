{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Config.Config where

import Data.Yaml (FromJSON(..), decodeFileEither, withObject, (.:), (.:?))
import GHC.Generics (Generic)
import Data.Text (Text)
import Data.Maybe (fromMaybe)
import Data.Aeson (genericParseJSON, defaultOptions)
import Config.ModuleTransform (transformModulePath, isValidModuleName)

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
  } deriving (Show, Generic, Eq)

data Template = Template
  { template :: FilePath
  , filename :: Text
  , output_by :: Text
  , module_transform :: Maybe Text  -- New field for module transformation
  } deriving (Show, Generic, Eq)

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
  parseJSON = withObject "Template" $ \v -> Template
    <$> v .: "template"
    <*> v .: "filename"
    <*> v .: "output_by"
    <*> v .:? "module_transform"

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

-- Function to transform a module name in a template
transformTemplateModule :: Template -> Text -> Text
transformTemplateModule template moduleName = case module_transform template of
  Just func -> transformModulePath func moduleName
  Nothing -> moduleName 
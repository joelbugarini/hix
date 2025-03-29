{-# LANGUAGE OverloadedStrings #-}

module Model
  ( Model(..)
  , Property(..)
  , loadModel
  ) where

import Data.Aeson
import qualified Data.ByteString.Lazy as B
import Data.Text (Text)

data Property = Property
  { name :: Text
  , type_ :: Text
  } deriving (Show)

instance FromJSON Property where
  parseJSON = withObject "Property" $ \v ->
    Property <$> v .: "name"
             <*> v .: "type"

data Model = Model
  { className :: Text
  , properties :: [Property]
  } deriving (Show)

instance FromJSON Model where
  parseJSON = withObject "Model" $ \v ->
    Model <$> v .: "className"
          <*> v .: "properties"

loadModel :: FilePath -> IO (Either String Model)
loadModel path = eitherDecode <$> B.readFile path

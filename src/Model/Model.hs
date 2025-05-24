{-# LANGUAGE OverloadedStrings #-}

module Model.Model
  ( Model(..)
  , Property(..)
  , PropertyType(..)
  , loadModel
  ) where

import Data.Aeson
import qualified Data.ByteString.Lazy as B
import Data.Text (Text)
import qualified Data.Text as T
import Data.Char (toLower)

data PropertyType = IntType | StringType | DateTimeType | BoolType | FloatType | DoubleType | DecimalType | TextType | UUIDType | DateType | TimeType | BinaryType
  deriving (Eq)

instance Show PropertyType where
  show IntType = "int"
  show StringType = "string"
  show DateTimeType = "datetime"
  show BoolType = "bool"
  show FloatType = "float"
  show DoubleType = "double"
  show DecimalType = "decimal"
  show TextType = "text"
  show UUIDType = "uuid"
  show DateType = "date"
  show TimeType = "time"
  show BinaryType = "binary"

instance FromJSON PropertyType where
  parseJSON = withText "PropertyType" $ \v -> case T.toLower v of
    "int"      -> pure IntType
    "string"   -> pure StringType
    "datetime" -> pure DateTimeType
    "bool"     -> pure BoolType
    "float"    -> pure FloatType
    "double"   -> pure DoubleType
    "decimal"  -> pure DecimalType
    "text"     -> pure TextType
    "uuid"     -> pure UUIDType
    "date"     -> pure DateType
    "time"     -> pure TimeType
    "binary"   -> pure BinaryType
    _ -> fail $ "Unknown property type: " ++ show v

data Property = Property
  { propName :: Text
  , propType :: PropertyType
  } deriving (Show, Eq)

instance FromJSON Property where
  parseJSON = withObject "Property" $ \v ->
    Property <$> v .: "name"
             <*> v .: "type"

data Model = Model
  { className :: Text
  , properties :: [Property]
  } deriving (Show, Eq)

instance FromJSON Model where
  parseJSON = withObject "Model" $ \v ->
    Model <$> v .: "className"
          <*> v .: "properties"

loadModel :: FilePath -> IO (Either String Model)
loadModel path = eitherDecode <$> B.readFile path

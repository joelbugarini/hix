{-# LANGUAGE OverloadedStrings #-}

module Config.ModuleTransform where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Char (isUpper, isLower, toLower)
import Template.RenderProp (toSnake, toKebab, toLowerFirst)

-- Transform a module name using the specified function
transformModuleName :: Text -> Text -> Text
transformModuleName func moduleName
  | func == "snake_case" = toSnake moduleName
  | func == "kebab_case" = toKebab moduleName
  | func == "lower" = T.toLower moduleName
  | func == "upper" = T.toUpper moduleName
  | func == "lowerFirst" = toLowerFirst moduleName
  | otherwise = moduleName

-- Split a module path into parts and transform each part
transformModulePath :: Text -> Text -> Text
transformModulePath func path = T.intercalate "." $ map (transformModuleName func) $ T.split (== '.') path

-- Validate if a module name is valid after transformation
isValidModuleName :: Text -> Bool
isValidModuleName name = not (T.null name) && T.all isValidChar name
  where
    isValidChar c = isUpper c || isLower c || c == '_' || c == '.' || c == '-' 
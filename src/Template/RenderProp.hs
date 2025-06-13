module Template.RenderProp (renderPropNode, renderPropBlock, propertyTypeToText, applyFunc, toSnake, toKebab, toLowerFirst) where

import Data.Text (Text)
import qualified Data.Text as T
import Template.AST (AST(..))
import Model.Model (Model(..), Property(..), PropertyType(..))
import Data.Char (isUpper, isLower, toLower)
import Debug.Trace (trace)

propertyTypeToText :: PropertyType -> Text
propertyTypeToText = T.pack . show

renderPropBlock :: (Model -> AST -> Text) -> Model -> Property -> [AST] -> Text
renderPropBlock modelRenderer model prop asts = T.concat $ map (renderPropNode modelRenderer model prop) asts

renderPropNode :: (Model -> AST -> Text) -> Model -> Property -> AST -> Text
renderPropNode _ _ _ (Literal t) = t
renderPropNode _ _ prop (ModelValue t) | t == T.pack "prop.name" = propName prop
renderPropNode _ _ prop (ModelValue t) | t == T.pack "prop.type" = propertyTypeToText (propType prop)
renderPropNode modelRenderer model prop (ModelValue t)
  | t == T.pack "prop.name" = propName prop
  | t == T.pack "prop.type" = propertyTypeToText (propType prop)
  | otherwise = modelRenderer model (ModelValue t)
renderPropNode _ _ _ (UnknownTag _) = T.pack ""
renderPropNode _ _ _ (PropLoop _ _) = T.pack ""
renderPropNode modelRenderer model prop (FuncCall fn arg)
  | arg == T.pack "prop.name" = applyFunc fn (propName prop)
  | arg == T.pack "prop.type" = applyFunc fn (propertyTypeToText (propType prop))
  | arg == T.pack "model.className" = applyFunc fn (className model)
  | otherwise = T.pack ""
renderPropNode modelRenderer model prop (IfBlock (k, v) trueBody mElse) =
  let val = case k of
              t | t == T.pack "prop.name" -> propName prop
              t | t == T.pack "prop.type" -> propertyTypeToText (propType prop)
              _ -> T.pack ""
  in if T.strip val == T.strip v
     then T.concat $ map (renderPropNode modelRenderer model prop) trueBody
     else maybe (T.pack "") (T.concat . map (renderPropNode modelRenderer model prop)) mElse

applyFunc :: Text -> Text -> Text
applyFunc fn t
  | fn == T.pack "upper" = T.toUpper t
  | fn == T.pack "lower" = T.toLower t
  | fn == T.pack "snake_case" = toSnake t
  | fn == T.pack "kebab_case" = toKebab t
  | fn == T.pack "lowerFirst" = toLowerFirst t
  | otherwise = T.pack "--[[Unknown func]] " <> t

toSnake :: Text -> Text
toSnake = T.intercalate (T.pack "_") . map T.toLower . camelSplit

toKebab :: Text -> Text
toKebab = T.intercalate (T.pack "-") . map T.toLower . camelSplit

toLowerFirst :: Text -> Text
toLowerFirst t = case T.uncons t of
  Nothing -> t
  Just (c, rest) -> T.cons (toLower c) rest

camelSplit :: Text -> [Text]
camelSplit = go . T.unpack
  where
    go [] = []
    go s@(x:xs)
      | isUpper x =
          let (uppers, rest) = span isUpper s
              (lowers, rest') = span isLower rest
              token = uppers ++ lowers
          in T.pack token : go rest'
      | isLower x =
          let (chunk, rest) = span isLower s
          in T.pack chunk : go rest
      | otherwise =  -- Handle special characters like dots
          let (special, rest) = span (\c -> not (isUpper c || isLower c)) s
          in T.pack special : go rest 
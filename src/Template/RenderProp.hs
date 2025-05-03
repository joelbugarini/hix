module Template.RenderProp (renderPropNode, renderPropBlock, propertyTypeToText) where

import Data.Text (Text)
import qualified Data.Text as T
import Template.AST (AST(..))
import Model.Model (Property(..), PropertyType(..))
import Data.Char (isUpper, isLower)

propertyTypeToText :: PropertyType -> Text
propertyTypeToText = T.pack . show

renderPropBlock :: Property -> [AST] -> Text
renderPropBlock prop asts = T.concat $ map (renderPropNode prop) asts

renderPropNode :: Property -> AST -> Text
renderPropNode _ (Literal t) = t
renderPropNode prop (ModelValue t) | t == T.pack "prop.name" = propName prop
renderPropNode prop (ModelValue t) | t == T.pack "prop.type" = propertyTypeToText (propType prop)
renderPropNode prop (FuncCall fn arg)
  | arg == T.pack "prop.name" = applyFunc fn (propName prop)
  | arg == T.pack "prop.type" = applyFunc fn (propertyTypeToText (propType prop))
  | otherwise = T.pack ""
renderPropNode _ (ModelValue _) = T.pack ""
renderPropNode _ (UnknownTag _) = T.pack ""
renderPropNode _ (PropLoop _ _) = T.pack ""
renderPropNode prop (IfBlock (k, v) trueBody mElse) =
  let val = case k of
              t | t == T.pack "prop.name" -> propName prop
              t | t == T.pack "prop.type" -> propertyTypeToText (propType prop)
              _ -> T.pack ""
  in if T.strip val == T.strip v
     then T.concat $ map (renderPropNode prop) trueBody
     else maybe (T.pack "") (T.concat . map (renderPropNode prop)) mElse

applyFunc :: Text -> Text -> Text
applyFunc fn t
  | fn == T.pack "upper" = T.toUpper t
  | fn == T.pack "lower" = T.toLower t
  | fn == T.pack "snake_case" = toSnake t
  | otherwise = T.pack "--[[Unknown func]] " <> t

toSnake :: Text -> Text
toSnake = T.intercalate (T.pack "_") . map T.toLower . camelSplit

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
      | otherwise =
          let (chunk, rest) = span isLower s
          in T.pack chunk : go rest 
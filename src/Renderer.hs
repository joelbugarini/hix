{-# LANGUAGE OverloadedStrings #-}

module Renderer
  ( renderAST
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Char (isUpper, isLower)
import TemplateAST (AST(..))
import Model (Model(..), Property(..))

-- Entry point: render list of AST nodes with full model
renderAST :: [AST] -> Model -> Text
renderAST asts model = T.concat $ map (renderNode model) asts

-- Render a single AST node (outside [[prop]] loop)
renderNode :: Model -> AST -> Text
renderNode _ (Literal t) = t
renderNode model (ModelValue "model.className") = className model
renderNode model (FuncCall fn arg)
  | "model." `T.isPrefixOf` arg = applyFunc fn (getModelValue model arg)
  | otherwise = "--[[Unsupported func arg: " <> arg <> "]]"
renderNode _ (ModelValue key) = "--[[Unknown model key: " <> key <> "]]"
renderNode _ (UnknownTag t) = "--[[Unknown tag: " <> t <> "]]"
renderNode model (PropLoop mFilter body) =
  let propsToRender = case mFilter of
        Just ("type", t)   -> filter (\p -> T.strip (type_ p) == T.strip t) (properties model)
        Just ("ignore", n) -> filter (\p -> T.strip (name p) /= T.strip n) (properties model)
        _ -> properties model
  in T.concat $ map (\p -> renderPropBlock p body) propsToRender

-- Render all AST nodes in a [[prop]] block for a given property
renderPropBlock :: Property -> [AST] -> Text
renderPropBlock prop = T.concat . map (renderPropNode prop)

-- Render inner nodes inside a [[prop]] loop
renderPropNode :: Property -> AST -> Text
renderPropNode _ (Literal t) = t
renderPropNode prop (ModelValue "prop.name") = name prop
renderPropNode prop (ModelValue "prop.type") = type_ prop
renderPropNode prop (FuncCall fn arg)
  | arg == "prop.name" = applyFunc fn (name prop)
  | arg == "prop.type" = applyFunc fn (type_ prop)
  | otherwise = "--[[Unsupported func arg: " <> arg <> "]]"
renderPropNode _ (ModelValue key) = "--[[Unknown prop key: " <> key <> "]]"
renderPropNode _ (UnknownTag t) = "--[[Unknown tag: " <> t <> "]]"
renderPropNode prop (IfBlock (k, v) trueBody mElse) =
  let val = case k of
              "prop.name" -> name prop
              "prop.type" -> type_ prop
              _ -> ""
  in if T.strip val == T.strip v
     then T.concat $ map (renderPropNode prop) trueBody
     else maybe "" (T.concat . map (renderPropNode prop)) mElse

-- 🔧 Helpers

getModelValue :: Model -> Text -> Text
getModelValue model "model.className" = className model
getModelValue _ key = "--[[Unknown model key: " <> key <> "]]"

applyFunc :: Text -> Text -> Text
applyFunc "upper" = T.toUpper
applyFunc "lower" = T.toLower
applyFunc "snake_case" = toSnake
applyFunc _ = \t -> "--[[Unknown func]] " <> t

toSnake :: Text -> Text
toSnake = T.intercalate "_" . map T.toLower . camelSplit

-- Proper CamelCase splitter
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

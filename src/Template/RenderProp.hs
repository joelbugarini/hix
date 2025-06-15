module Template.RenderProp (renderPropNode, renderPropBlock, propertyTypeToText, applyFunc, toSnake, toKebab, toLowerFirst) where

import Data.Text (Text)
import qualified Data.Text as T
import Template.AST (AST(..))
import Model.Model (Model(..), Property(..), PropertyType(..))
import Data.Char (isUpper, isLower, toLower, isDigit)
import Debug.Trace (trace)
import Logging (LogState, logInfo)
import Control.Monad.Writer

propertyTypeToText :: PropertyType -> Text
propertyTypeToText = T.pack . show

renderPropBlock :: (Model -> AST -> Writer [Text] Text) -> Model -> Property -> [AST] -> Writer [Text] Text
renderPropBlock modelRenderer model prop asts = do
  tell [T.pack $ "Rendering prop block for property: " ++ T.unpack (propName prop)]
  rendered <- mapM (renderPropNode modelRenderer model prop) asts
  return $ T.concat rendered

renderPropNode :: (Model -> AST -> Writer [Text] Text) -> Model -> Property -> AST -> Writer [Text] Text
renderPropNode _ _ _ (Literal t) = return t
renderPropNode _ _ prop (ModelValue t) | t == T.pack "prop.name" = return $ propName prop
renderPropNode _ _ prop (ModelValue t) | t == T.pack "prop.type" = return $ propertyTypeToText (propType prop)
renderPropNode modelRenderer model prop (ModelValue t)
  | t == T.pack "prop.name" = return $ propName prop
  | t == T.pack "prop.type" = return $ propertyTypeToText (propType prop)
  | otherwise = modelRenderer model (ModelValue t)
renderPropNode _ _ _ (UnknownTag _) = return $ T.pack ""
renderPropNode _ _ _ (PropLoop _ _) = return $ T.pack ""
renderPropNode modelRenderer model prop (FuncCall fn arg)
  | arg == T.pack "prop.name" = return $ applyFunc fn (propName prop)
  | arg == T.pack "prop.type" = return $ applyFunc fn (propertyTypeToText (propType prop))
  | arg == T.pack "model.className" = return $ applyFunc fn (className model)
  | otherwise = modelRenderer model (FuncCall fn arg)
renderPropNode modelRenderer model prop (IfBlock (k, v) trueBody mElse) =
  let val = case k of
              t | t == T.pack "prop.name" -> propName prop
              t | t == T.pack "prop.type" -> propertyTypeToText (propType prop)
              _ -> T.pack ""
  in if T.strip val == T.strip v
     then do
       rendered <- mapM (renderPropNode modelRenderer model prop) trueBody
       return $ T.concat rendered
     else maybe (return $ T.pack "") (\elseBody -> do
       rendered <- mapM (renderPropNode modelRenderer model prop) elseBody
       return $ T.concat rendered) mElse

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
              (lowers, rest') = span (\c -> isLower c || isDigit c) rest
              token = uppers ++ lowers
          in T.pack token : go rest'
      | isLower x =
          let (chunk, rest) = span (\c -> isLower c || isDigit c) s
          in T.pack chunk : go rest
      | x == '-' = go (dropWhile (== '-') s) -- skip all consecutive hyphens
      | otherwise =  -- Handle other special characters
          let (special, rest) = span (\c -> not (isUpper c || isLower c || isDigit c || c == '-')) s
          in if null special then go rest else T.pack special : go rest 
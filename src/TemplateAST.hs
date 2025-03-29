{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module TemplateAST
  ( AST(..)
  , parseTokens
  ) where

import qualified Data.Text as T
import Data.Text (Text)
import Lexer (Token(..))

-- AST data types
data AST
  = Literal Text                               -- raw text
  | ModelValue Text                            -- e.g. [[model.className]]
  | PropLoop (Maybe (Text, Text)) [AST]        -- [[prop]], optional filter, inner nodes
  | IfBlock (Text, Text) [AST] (Maybe [AST])   -- [[if prop.type=bool]] ... [[/if]]
  | UnknownTag Text                            -- anything unrecognized
  | FuncCall Text Text                         -- e.g. [[upper prop.name]]
  deriving (Show)

parseTokens :: [Token] -> [AST]
parseTokens = go
  where
    go [] = []
    go (TextChunk t : rest) = Literal t : go rest

    go (Tag content _ _ : rest)
      -- ✅ Handle [[prop]] and filtered variants
      | content == "prop"
        || "prop type=" `T.isPrefixOf` content
        || "prop.ignore=" `T.isPrefixOf` content =
          let contentTrimmed = T.strip content
              (tagType, restTag) = T.breakOn " " contentTrimmed
              filterPart = T.strip $ T.drop (T.length tagType + 1) contentTrimmed
              mFilter = if T.null restTag
                        then Nothing
                        else case T.splitOn "=" filterPart of
                               [k, v] -> Just (T.strip k, T.strip v)
                               _ -> Nothing
              (body, after) = collectUntil "/prop" rest
              inner = parseTokens body
          in PropLoop mFilter inner : go after

      -- ✅ Handle [[if key=value]] ... [[/if]]
      | "if " `T.isPrefixOf` content =
          let condText = T.strip $ T.drop (T.length "if ") content
              (k, v) = breakKV condText
              (body, afterIf) = collectUntilAny ["/if", "else"] rest
              (trueBody, falseBody, after) = case afterIf of
                (Tag tagText _ _ : restAfterTag)
                  | T.strip tagText == "else" ->
                      let (elseBody, afterEnd) = collectUntil "/if" restAfterTag
                      in (parseTokens body, Just (parseTokens elseBody), afterEnd)
                  | T.strip tagText == "/if" -> (parseTokens body, Nothing, restAfterTag)
                _ -> (parseTokens body, Nothing, afterIf)
          in IfBlock (k, v) trueBody falseBody : go after

      -- ✅ Handle [[upper prop.name]], [[lower prop.name]], etc.
      | any (`T.isPrefixOf` content) ["upper ", "lower ", "snake_case "] =
          let (fn, arg) = T.breakOn " " (T.strip content)
          in FuncCall fn (T.strip arg) : go rest

      -- ✅ Clean up known model/property references
      | otherwise =
          let clean = T.strip content
          in case clean of
              "/prop"           -> go rest
              "/if"             -> go rest
              "model.className" -> ModelValue clean : go rest
              "prop.name"       -> ModelValue clean : go rest
              "prop.type"       -> ModelValue clean : go rest
              _                 -> UnknownTag clean : go rest

    -- 📦 Collect tokens inside a loop until [[/tag]]
    collectUntil :: Text -> [Token] -> ([Token], [Token])
    collectUntil endTag = span (\t -> case t of
      Tag c _ _ -> T.strip c /= endTag
      _ -> True)

    -- 🔍 Parse "key=value" to (key, value)
    breakKV :: Text -> (Text, Text)
    breakKV txt = case T.splitOn "=" txt of
      [k, v] -> (T.strip k, T.strip v)
      _ -> ("", "")
    
    collectUntilAny :: [Text] -> [Token] -> ([Token], [Token])
    collectUntilAny endTags = span (\case
      Tag c _ _ -> T.strip c `notElem` endTags
      _ -> True)


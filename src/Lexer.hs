{-# LANGUAGE OverloadedStrings #-}

module Lexer
  ( Token(..)
  , tokenize
  ) where

import Data.Text (Text)
import qualified Data.Text as T

-- Token data type
data Token
  = TextChunk Text
  | Tag Text Int Int    -- tag content, line, column
  deriving (Show)

-- Main tokenizer function
tokenize :: Text -> [Token]
tokenize input = go input 1 1 []
  where
    go :: Text -> Int -> Int -> [Token] -> [Token]
    go txt line col acc
      | T.null txt = reverse acc
      | "[[" `T.isPrefixOf` txt =
          let (tagBody, rest) = T.breakOn "]]" (T.drop 2 txt)
          in if T.isPrefixOf "]]" (T.drop (T.length tagBody) (T.drop 2 txt))
             then
               let tag = Tag tagBody line col
                   txtAfter = T.drop (T.length tagBody + 4) txt  -- 2 for [[, 2 for ]]
                   col' = col + T.length tagBody + 4
               in go txtAfter line col' (tag : acc)
             else
               -- unclosed tag, treat as plain text
               let (chunk, rest') = T.splitAt 2 txt
               in go rest' line (col + 2) (TextChunk chunk : acc)

      | otherwise =
          let (chunk, rest) = T.breakOn "[[" txt
              newLines = T.count "\n" chunk
              col' = if newLines > 0
                     then T.length (last (T.splitOn "\n" chunk)) + 1
                     else col + T.length chunk
              line' = line + newLines
          in go rest line' col' (TextChunk chunk : acc)

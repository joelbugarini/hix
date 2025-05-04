{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Template.AST
  ( AST(..)
  ) where

import qualified Data.Text as T
import Data.Text (Text)
import Template.Lexer (Token(..))

-- AST data types
data AST
  = Literal Text                               -- raw text
  | ModelValue Text                            -- e.g. [[model.className]]
  | PropLoop (Maybe (Text, Text)) [AST]        -- [[prop]], optional filter, inner nodes
  | IfBlock (Text, Text) [AST] (Maybe [AST])   -- [[if prop.type=bool]] ... [[/if]]
  | UnknownTag Text                            -- anything unrecognized
  | FuncCall Text Text                         -- e.g. [[upper prop.name]]
  deriving (Show, Eq)


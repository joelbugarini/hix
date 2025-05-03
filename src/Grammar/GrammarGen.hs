{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Grammar.GrammarGen (writeGrammarFile) where

import Data.Aeson (ToJSON, encode, ToJSON(..), object, (.=))
import GHC.Generics (Generic)
import qualified Data.ByteString.Lazy as BL

-- Root of the grammar file
data Grammar = Grammar
  { scopeName :: String
  , fileTypes :: [String]
  , grammarName :: String
  , uuid :: String
  , patterns :: [Pattern]
  } deriving (Generic, ToJSON)

data Pattern = Pattern
  { patternName :: String
  , match :: String
  }

-- Manually define ToJSON for Pattern so it exports `"name"` not `"patternName"`
instance ToJSON Pattern where
  toJSON (Pattern pname regex) =
    object [ "name" .= pname
           , "match" .= regex
           ]

writeGrammarFile :: FilePath -> IO ()
writeGrammarFile outputPath = do
  let grammar = Grammar
        { scopeName = "source.hix"
        , fileTypes = ["hix"]
        , grammarName = "Hix"
        , uuid = "hix-template-language"
        , patterns =
          [ Pattern "keyword.control.hix.block.begin" "\\[\\[\\s*prop\\s*\\]\\]"
          , Pattern "keyword.control.hix.block.end" "\\[\\[\\s*/prop\\s*\\]\\]"
          , Pattern "keyword.control.hix.conditional.begin" "\\[\\[\\s*if\\b[^\\]]*\\]\\]"
          , Pattern "keyword.control.hix.conditional.end" "\\[\\[\\s*/if\\s*\\]\\]"
          , Pattern "keyword.control.hix.conditional.else" "\\[\\[\\s*else\\s*\\]\\]"
          , Pattern "variable.other.hix" "\\[\\[\\s*(?:model|prop)\\.[a-zA-Z0-9_]+\\s*\\]\\]"
          , Pattern "support.function.hix.upper" "\\[\\[\\s*upper\\s+(?:model|prop)\\.[a-zA-Z0-9_]+\\s*\\]\\]"
          , Pattern "support.function.hix.lower" "\\[\\[\\s*lower\\s+(?:model|prop)\\.[a-zA-Z0-9_]+\\s*\\]\\]"
          , Pattern "support.function.hix.snake_case" "\\[\\[\\s*snake_case\\s+(?:model|prop)\\.[a-zA-Z0-9_]+\\s*\\]\\]"
          ]
        }
  BL.writeFile outputPath (encode grammar)
  putStrLn $ "Grammar written to: " ++ outputPath

{-# LANGUAGE OverloadedStrings #-}

module CLI.Help
  ( helpMessage
  , manualMessage
  , version
  ) where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Text.Encoding (encodeUtf8)
import qualified Data.ByteString.Char8 as BS

-- Version information
version :: String
version = "hix version 0.3.2.0"

-- Help message
helpMessage :: String
helpMessage = init $ unlines
  [ "hix - A code generation tool"
  , ""
  , "Usage: hix [command] [options]"
  , ""
  , "Commands:"
  , "  init           Initialize a new hix project"
  , "  generate       Generate files from templates"
  , "  help           Show this help message"
  , "  man           Show detailed manual"
  , "  version        Show version information"
  , ""
  , "Options:"
  , "  -h, --help     Show this help message"
  , ""
  , "Examples:"
  , "  hix init       Initialize a new hix project"
  , "  hix generate --model ./models/user.json"
  , "  hix generate --model ./models/user.json --layer Domain"
  , "  hix generate --model ./models/user.json --template ./templates/domain/Archive.hix"
  , ""
  , "  hix help       Show this help message"
  , "  hix version    Show version information"
  ]

-- Manual message
manualMessage :: String
manualMessage = init $ unlines
  [ "hix - A code generation tool"
  , ""
  , "Usage: hix [command] [options]"
  , ""
  , "Description:"
  , "  hix is a code generation tool that helps you create and maintain"
  , "  a clean architecture project structure."
  , ""
  , "Commands:"
  , "  init"
  , "      Initialize a new hix project with the following structure:"
  , "          .hix/               # Configuration directory"
  , "              config.yaml     # Project configuration"
  , "          models/             # Model files"
  , "          output/             # Generated code"
  , "          templates/          # Template files"
  , ""
  , "  generate"
  , "      Generate files from templates based on a model."
  , "      Options:"
  , "        --model <path>         Path to the model file (required)"
  , "        --layer <n>         Generate files only for specified layer"
  , "        --template <path>      Generate file only for specified template"
  , ""
  , "  help"
  , "      Show a brief help message."
  , ""
  , "  man"
  , "      Show this detailed manual."
  , ""
  , "  version"
  , "      Show version information."
  , ""
  , "Options:"
  , "  -h, --help"
  , "      Show a brief help message."
  , ""
  , "Examples:"
  , "  Initialize a new project:"
  , "      hix init"
  , ""
  , "  Generate files from a model:"
  , "      hix generate --model ./models/user.json"
  , "      hix generate --model ./models/user.json --layer Domain"
  , "      hix generate --model ./models/user.json --template ./templates/domain/Archive.hix"
  , ""
  , "  Show help:"
  , "      hix help"
  , "      hix --help"
  , "      hix -h"
  , ""
  , "  Show manual:"
  , "      hix man"
  , ""
  , "  Show version:"
  , "      hix version"
  ] 
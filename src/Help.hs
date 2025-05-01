{-# LANGUAGE OverloadedStrings #-}

module Help
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
version = "0.2.0.0"

-- Help message
helpMessage :: String
helpMessage = T.unpack $ T.stripEnd $ T.pack $ unlines
  [ "hix - A code generation tool for clean architecture"
  , ""
  , "Usage:"
  , "  hix [command] [options]"
  , ""
  , "Commands:"
  , "  init           Initialize a new hix project"
  , "  help           Show this help message"
  , "  man            Show detailed manual"
  , "  version        Show version information"
  , ""
  , "Options:"
  , "  --help         Show this help message"
  , "  --version      Show version information"
  , ""
  , "Examples:"
  , "  hix init       Initialize a new hix project"
  , "  hix help       Show this help message"
  , "  hix version    Show version information"
  ]

-- Manual message
manualMessage :: String
manualMessage = T.unpack $ T.stripEnd $ T.pack $ unlines
  [ "HIX MANUAL"
  , "=========="
  , ""
  , "NAME"
  , "    hix - A code generation tool for clean architecture"
  , ""
  , "SYNOPSIS"
  , "    hix [command] [options]"
  , ""
  , "DESCRIPTION"
  , "    hix is a code generation tool that helps you create and maintain clean architecture"
  , "    projects. It supports multiple architecture patterns and allows you to define custom"
  , "    architectures and templates."
  , ""
  , "COMMANDS"
  , "    init"
  , "        Initialize a new hix project. This will create a .hix directory with the"
  , "        following structure:"
  , "        .hix/"
  , "        ├── config.yaml           # Configuration file"
  , "        ├── .gitignore           # Git ignore rules"
  , "        ├── models/              # Model files"
  , "        ├── output/              # Generated code"
  , "        └── templates/           # Template files"
  , ""
  , "    help"
  , "        Show a brief help message."
  , ""
  , "    man"
  , "        Show this detailed manual."
  , ""
  , "    version"
  , "        Show version information."
  , ""
  , "OPTIONS"
  , "    --help"
  , "        Show a brief help message."
  , ""
  , "    --version"
  , "        Show version information."
  , ""
  , "ARCHITECTURES"
  , "    hix supports the following architecture patterns:"
  , ""
  , "    Onion Architecture"
  , "        - Domain: Business logic and models"
  , "        - Application: Services and use cases"
  , "        - Infrastructure: Technical implementations"
  , "        - Presentation: UI and controllers"
  , ""
  , "    Clean Architecture"
  , "        - Entities: Business entities and value objects"
  , "        - UseCases: Application use cases and rules"
  , "        - InterfaceAdapters: Controllers and presenters"
  , "        - FrameworksAndDrivers: External interfaces"
  , ""
  , "    Hexagonal Architecture"
  , "        - Domain: Business logic and models"
  , "        - Application: Services and ports"
  , "        - Adapters: External system adapters"
  , "        - Infrastructure: Technical implementations"
  , ""
  , "    Custom Architecture"
  , "        You can also define your own architecture with custom layers."
  , ""
  , "TEMPLATES"
  , "    Templates are used to generate code for each layer. They support the following"
  , "    features:"
  , ""
  , "    - Model properties: [[prop]] ... [[/prop]]"
  , "    - Model values: [[model.className]]"
  , "    - Property values: [[prop.name]], [[prop.type]]"
  , ""
  , "EXAMPLES"
  , "    Initialize a new project:"
  , "        hix init"
  , ""
  , "    Show help:"
  , "        hix help"
  , ""
  , "    Show version:"
  , "        hix version"
  ] 
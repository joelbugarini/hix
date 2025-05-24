{-# LANGUAGE OverloadedStrings #-}

module HelpSpec (helpSpec) where

import Test.Hspec
import System.IO.Silently (capture)
import CLI.Help (helpMessage, manualMessage, version)
import System.Directory (createDirectoryIfMissing)
import Data.Text (pack, unpack, replace)

-- | Normalize line endings to LF and remove box-drawing characters
normalizeText :: String -> String
normalizeText = unpack . replace (pack "\r\n") (pack "\n") . pack

helpSpec :: Spec
helpSpec = describe "Help functionality" $ do
  it "should produce correct help message" $ do
    (output, _) <- capture $ putStrLn helpMessage
    output `shouldBe` unlines [
        "hix - A code generation tool"
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

  it "should produce correct manual message" $ do
    (output, _) <- capture $ putStrLn manualMessage
    output `shouldBe` unlines [
        "hix - A code generation tool"
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

  it "should produce correct version message" $ do
    (output, _) <- capture $ putStrLn version
    output `shouldBe` "hix version 0.3.3.0\n" 
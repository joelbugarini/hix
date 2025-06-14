{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Lazy as BL
import System.FilePath ((</>))
import Data.Char (isSpace)
import System.Directory (createDirectoryIfMissing, removeDirectoryRecursive, removeFile)
import System.IO.Silently (capture_)
import System.Process (readProcess, readProcessWithExitCode)
import System.Exit (ExitCode(ExitSuccess, ExitFailure))
import Control.Monad.Writer (runWriter)

import Template.AST (AST(..))
import Model.Model (Model(..), Property(..), PropertyType(..))
import Template.Renderer (renderAST)
import qualified Data.Aeson as Aeson
import HelpSpec (helpSpec)
import GenerateSpec (generateSpec)
import WizardSpec (wizardSpec)
import qualified ParserSpec
import qualified ModuleTransformSpec
import Template.Parser (parseTemplate)
import Transform.TransformSpec

helpMessage :: String
helpMessage = init $ unlines
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
  , ""
  ]

main :: IO ()
main = hspec $ do
  helpSpec
  generateSpec
  wizardSpec
  ParserSpec.spec
  ModuleTransformSpec.spec
  Transform.TransformSpec.spec

  describe "TemplateAST" $ do
    it "parses prop loop with prop.name" $
      case parseTemplate "[[prop]]int [[prop.name]];[[/prop]]" of
        Right [PropLoop _ [Literal "int ", ModelValue "prop.name", Literal ";"]] -> True `shouldBe` True
        _ -> expectationFailure "Failed to parse prop loop with prop.name"

  describe "Renderer" $ do
    it "renders a model class with props" $ do
      let model = Model "Thing"
            [ Property "Id" IntType
            , Property "Title" StringType
            ]
      case parseTemplate "public class [[model.className]] {\n[[prop]][[prop.type]] [[prop.name]];\n[[/prop]]}" of
        Right ast -> do
          let (output, _) = runWriter $ renderAST ast model
          output `shouldBe` T.stripEnd "public class Thing {\nint Id;\nstring Title;\n}"
        Left err -> expectationFailure $ "Parse error: " ++ err

    it "renders if/else blocks correctly" $ do
      let model = Model "TestModel"
            [ Property "IsAdmin" StringType
            , Property "Name" StringType
            ]
          template = T.unlines
            [ "[[prop]]"
            , "[[if prop.type=string]]"
            , "Input: [[prop.name]]"
            , "[[else]]"
            , "Other: [[prop.name]]"
            , "[[/if]]"
            , "[[/prop]]"
            ]
      case parseTemplate template of
        Right ast -> do
          let (output, _) = runWriter $ renderAST ast model
              expected = T.stripEnd $ T.unlines
                [ "Input: IsAdmin"
                , "Input: Name"
                ]
          removeBlankLines output `shouldBe` removeBlankLines expected
        Left err -> expectationFailure $ "Parse error: " ++ err

  describe "CLI" $ do
    it "handles model name replacement correctly" $ do
      let dir = "test/data/cli"
      template <- TIO.readFile (dir </> "template.hix")
      modelJson <- TIO.readFile (dir </> "model.json")
      expected <- TIO.readFile (dir </> "expected.txt")
      case decodeModel modelJson of
        Right model ->
          case parseTemplate template of
            Right ast ->
              let (output, _) = runWriter $ renderAST ast model
               in removeBlankLines output `shouldBe` removeBlankLines expected
            Left err -> expectationFailure $ "Parse error: " ++ err
        Left err -> expectationFailure $ "Failed to decode model: " ++ err

    it "shows help message when no arguments provided" $ do
      (exitCode, output, _) <- readProcessWithExitCode "stack" ["exec", "hix", "--"] ""
      exitCode `shouldBe` ExitSuccess
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

  describe "Golden rendering" $ do
    goldenTest "example"
    goldenTest "form"
    goldenTest "functions"
    goldenTest "cli"

-- 🔍 Golden test for output comparison
goldenTest :: FilePath -> Spec
goldenTest name = it ("matches golden output for " ++ name) $ do
  let dir = "test/data" </> name
  template <- TIO.readFile (dir </> "template.hix")
  modelJson <- TIO.readFile (dir </> "model.json")
  expected <- TIO.readFile (dir </> "expected.txt")
  case decodeModel modelJson of
    Right model ->
      case parseTemplate template of
        Right ast ->
          let (output, _) = runWriter $ renderAST ast model
           in removeBlankLines output `shouldBe` removeBlankLines expected
        Left err -> expectationFailure $ "Parse error: " ++ err
    Left err -> expectationFailure $ "Failed to decode model: " ++ err

-- 🧠 Decode JSON model from Text
decodeModel :: T.Text -> Either String Model
decodeModel txt = Aeson.eitherDecode (BL.fromStrict (TE.encodeUtf8 txt))

removeBlankLines :: T.Text -> T.Text
removeBlankLines = T.unlines . filter (not . T.all isSpace) . T.lines

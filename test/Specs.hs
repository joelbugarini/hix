{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

import Test.Hspec
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Lazy as BL
import System.FilePath ((</>))
import Data.Char (isSpace)

import Lexer (tokenize)
import TemplateAST (parseTokens, AST(..))
import Model (Model(..), Property(..))
import Renderer (renderAST)
import qualified Data.Aeson as Aeson

main :: IO ()
main = hspec $ do

  describe "Lexer" $ do
    it "tokenizes simple template" $
      let tokens = tokenize "Hello [[model.className]]!" in
      length tokens `shouldBe` 3

  describe "TemplateAST" $ do
    it "parses prop loop with prop.name" $
      let tokens = tokenize "[[prop]]int [[prop.name]];[[/prop]]"
          ast = parseTokens tokens
      in ast `shouldSatisfy` \case
           [PropLoop _ [Literal "int ", ModelValue "prop.name", Literal ";"]] -> True
           _ -> False

  describe "Renderer" $ do
    it "renders a model class with props" $ do
      let model = Model "Thing"
            [ Property "Id" "int"
            , Property "Title" "string"
            ]
          tokens = tokenize "public class [[model.className]] {\n[[prop]][[prop.type]] [[prop.name]];\n[[/prop]]}"
          ast = parseTokens tokens
          output = renderAST ast model
      output `shouldBe` T.stripEnd "public class Thing {\nint Id;\nstring Title;\n}"

    it "renders if/else blocks correctly" $ do
      let model = Model "TestModel"
            [ Property "IsAdmin" "bool"
            , Property "Name" "string"
            ]
          template = T.unlines
            [ "[[prop]]"
            , "[[if prop.type=bool]]"
            , "Checkbox: [[prop.name]]"
            , "[[else]]"
            , "Input: [[prop.name]]"
            , "[[/if]]"
            , "[[/prop]]"
            ]
          tokens = tokenize template
          ast = parseTokens tokens
          output = renderAST ast model
          expected = T.stripEnd $ T.unlines
            [ "Checkbox: IsAdmin"
            , "Input: Name"
            ]
      removeBlankLines output `shouldBe` removeBlankLines expected


  describe "Golden rendering" $ do
    goldenTest "example"
    goldenTest "form"
    goldenTest "functions"


-- 🔍 Golden test for output comparison
goldenTest :: FilePath -> Spec
goldenTest name = it ("matches golden output for " ++ name) $ do
  let dir = "test/data" </> name
  template <- TIO.readFile (dir </> "template.hix")
  modelJson <- TIO.readFile (dir </> "model.json")
  expected <- TIO.readFile (dir </> "expected.txt")
  case decodeModel modelJson of
    Right model ->
      let output = renderAST (parseTokens (tokenize template)) model
       in removeBlankLines output `shouldBe` removeBlankLines expected
    Left err -> expectationFailure $ "Failed to decode model: " ++ err

-- 🧠 Decode JSON model from Text
decodeModel :: T.Text -> Either String Model
decodeModel txt = Aeson.eitherDecode (BL.fromStrict (TE.encodeUtf8 txt))

removeBlankLines :: T.Text -> T.Text
removeBlankLines = T.unlines . filter (not . T.all isSpace) . T.lines

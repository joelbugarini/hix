{-# LANGUAGE OverloadedStrings #-}

module Transform.TransformSpec (spec) where

import Test.Hspec
import qualified Data.Text as T
import Config.ModuleTransform (transformModuleName, transformModulePath, isValidModuleName)
import Template.RenderProp (applyFunc)
import Template.Renderer (renderAST)
import Template.Parser (parseTemplate)
import Model.Model (Model(..), Property(..), PropertyType(..))

spec :: Spec
spec = do
  describe "Case Transformations" $ do
    describe "Basic Transformations" $ do
      it "converts to upper case" $ do
        applyFunc (T.pack "upper") (T.pack "test") `shouldBe` T.pack "TEST"
        transformModuleName "upper" "TestModule" `shouldBe` "TESTMODULE"

      it "converts to lower case" $ do
        applyFunc (T.pack "lower") (T.pack "TEST") `shouldBe` T.pack "test"
        transformModuleName "lower" "TestModule" `shouldBe` "testmodule"

      it "converts to snake case" $ do
        applyFunc (T.pack "snake_case") (T.pack "testCase") `shouldBe` T.pack "test_case"
        transformModuleName "snake_case" "TestModule" `shouldBe` "test_module"

      it "converts to kebab case" $ do
        applyFunc (T.pack "kebab_case") (T.pack "testCase") `shouldBe` T.pack "test-case"
        transformModuleName "kebab_case" "TestModule" `shouldBe` "test-module"

      it "converts first letter to lowercase" $ do
        applyFunc (T.pack "lowerFirst") (T.pack "TestCase") `shouldBe` T.pack "testCase"
        transformModuleName "lowerFirst" "TestModule" `shouldBe` "testModule"

    describe "Module Path Transformations" $ do
      it "transforms each part of the path" $ do
        transformModulePath "snake_case" "Test.Module.Name" `shouldBe` "test.module.name"
        transformModulePath "kebab_case" "Test.Module.Name" `shouldBe` "test.module.name"
        transformModulePath "lower" "Test.Module.Name" `shouldBe` "test.module.name"
        transformModulePath "upper" "Test.Module.Name" `shouldBe` "TEST.MODULE.NAME"

    describe "Template Transformations" $ do
      it "transforms model.className in templates" $ do
        let model = Model "TestModule" []
        let template = "[[upper model.className]]"
        case parseTemplate (T.pack template) of
          Right ast -> renderAST ast model `shouldBe` "TESTMODULE"
          Left err -> fail $ "Failed to parse template: " ++ err

      it "transforms model.className in prop loops" $ do
        let model = Model "TestModule" [Property "Id" IntType]
        let template = "[[prop]][[lowerFirst model.className]][[/prop]]"
        case parseTemplate (T.pack template) of
          Right ast -> renderAST ast model `shouldBe` "testModule"
          Left err -> fail $ "Failed to parse template: " ++ err

      it "handles multiple transformations in prop loops" $ do
        let model = Model "TestModule" [Property "Id" IntType]
        let template = T.unlines
              [ "[[prop]]"
              , "  [[lowerFirst model.className]]"
              , "  [[upper model.className]]"
              , "  [[snake_case model.className]]"
              , "[[/prop]]"
              ]
        case parseTemplate template of
          Right ast -> renderAST ast model `shouldBe` T.unlines ["testModule", "TESTMODULE", "test_module"]
          Left err -> fail $ "Failed to parse template: " ++ err

    describe "Edge Cases" $ do
      it "handles empty strings" $ do
        applyFunc (T.pack "lowerFirst") (T.pack "") `shouldBe` T.pack ""
        transformModuleName "lowerFirst" "" `shouldBe` ""

      it "handles single character strings" $ do
        applyFunc (T.pack "upper") (T.pack "a") `shouldBe` T.pack "A"
        transformModuleName "upper" "a" `shouldBe` "A"

      it "handles strings with numbers" $ do
        applyFunc (T.pack "snake_case") (T.pack "Test123Module") `shouldBe` T.pack "test123_module"
        transformModuleName "snake_case" "Test123Module" `shouldBe` "test123_module"

      it "handles strings with special characters" $ do
        applyFunc (T.pack "kebab_case") (T.pack "Test-Module") `shouldBe` T.pack "test-module"
        transformModuleName "kebab_case" "Test-Module" `shouldBe` "test-module"

    describe "Validation" $ do
      it "validates module names" $ do
        isValidModuleName "TestModule" `shouldBe` True
        isValidModuleName "test_module" `shouldBe` True
        isValidModuleName "test-module" `shouldBe` True
        isValidModuleName "Test.Module" `shouldBe` True
        isValidModuleName "" `shouldBe` False
        isValidModuleName "Test@Module" `shouldBe` False 
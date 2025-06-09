{-# LANGUAGE OverloadedStrings #-}

module ModuleTransformSpec (spec) where

import Test.Hspec
import qualified Data.Text as T
import Config.ModuleTransform (transformModuleName, transformModulePath, isValidModuleName)
import Config.Config (Template(..), transformTemplateModule)
import System.CPUTime (getCPUTime)
import Template.Renderer (renderAST)
import Template.Parser (parseTemplate)
import Model.Model (Model(..), Property(..), PropertyType(..))

spec :: Spec
spec = do
  describe "transformModuleName" $ do
    it "converts to snake_case" $ do
      start <- getCPUTime
      let result = transformModuleName "snake_case" "TestModule"
      end <- getCPUTime
      let diff = fromIntegral (end - start) / (10^12)  -- Convert to seconds
      putStrLn $ "Time taken: " ++ show diff ++ " seconds"
      result `shouldBe` "test_module"
      transformModuleName "snake_case" "Test.Module" `shouldBe` "test.module"

    it "converts to kebab_case" $ do
      transformModuleName "kebab_case" "TestModule" `shouldBe` "test-module"
      transformModuleName "kebab_case" "Test.Module" `shouldBe` "test.module"

    it "converts to lower case" $ do
      transformModuleName "lower" "TestModule" `shouldBe` "testmodule"
      transformModuleName "lower" "Test.Module" `shouldBe` "test.module"

    it "converts to upper case" $ do
      transformModuleName "upper" "TestModule" `shouldBe` "TESTMODULE"
      transformModuleName "upper" "Test.Module" `shouldBe` "TEST.MODULE"

    it "converts first letter to lowercase" $ do
      transformModuleName "lowerFirst" "TestModule" `shouldBe` "testModule"
      transformModuleName "lowerFirst" "Test.Module" `shouldBe` "test.Module"

  describe "transformModulePath" $ do
    it "transforms each part of the path" $ do
      transformModulePath "snake_case" "Test.Module.Name" `shouldBe` "test.module.name"
      transformModulePath "kebab_case" "Test.Module.Name" `shouldBe` "test-module-name"
      transformModulePath "lower" "Test.Module.Name" `shouldBe` "test.module.name"
      transformModulePath "upper" "Test.Module.Name" `shouldBe` "TEST.MODULE.NAME"

  describe "isValidModuleName" $ do
    it "validates module names" $ do
      isValidModuleName "TestModule" `shouldBe` True
      isValidModuleName "test_module" `shouldBe` True
      isValidModuleName "test-module" `shouldBe` True
      isValidModuleName "Test.Module" `shouldBe` True
      isValidModuleName "" `shouldBe` False
      isValidModuleName "Test@Module" `shouldBe` False

  describe "transformTemplateModule" $ do
    it "transforms module names according to template configuration" $ do
      let template = Template "template.hix" "output.cs" "model" (Just "snake_case")
      transformTemplateModule template "Test.Module" `shouldBe` "test.module"

    it "returns original name when no transformation is specified" $ do
      let template = Template "template.hix" "output.cs" "model" Nothing
      transformTemplateModule template "Test.Module" `shouldBe` "Test.Module"

  describe "Template Rendering with Module Transform" $ do
    it "transforms module names in template" $ do
      let template = "[[module_transform kebab_case model.className]]-detail.component.ts"
      let model = Model "TestModule" []
      case parseTemplate (T.pack template) of
        Right asts -> renderAST asts model `shouldBe` "test-module-detail.component.ts"
        Left err -> fail $ "Failed to parse template: " ++ err 
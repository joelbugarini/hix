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
import System.IO (putStrLn, hFlush, stdout)
import Control.Concurrent (threadDelay)
import Control.Exception (evaluate)
import Debug.Trace (trace)
import Data.Text (Text)
import Template.AST (AST(..))
import Control.Monad.Writer (runWriter)

spec :: Spec
spec = do
  describe "transformModuleName" $ do
    it "converts to snake_case" $ do
      transformModuleName "snake_case" "TestModule" `shouldBe` "test_module"
    it "converts to kebab_case (single segment)" $ do
      transformModuleName "kebab_case" "TestModule" `shouldBe` "test-module"

  describe "transformModulePath" $ do
    it "converts to kebab_case with dots" $ do
      transformModulePath "kebab_case" "Test.Module" `shouldBe` "test.module"

  describe "isValidModuleName" $ do
    it "validates module names" $ do
      putStrLn "Starting isValidModuleName tests..." >> hFlush stdout
      result1 <- evaluate $ isValidModuleName "TestModule"
      putStrLn "First isValidModuleName test computed" >> hFlush stdout
      result1 `shouldBe` True
      putStrLn "First isValidModuleName test passed" >> hFlush stdout
      
      result2 <- evaluate $ isValidModuleName "test_module"
      putStrLn "Second isValidModuleName test computed" >> hFlush stdout
      result2 `shouldBe` True
      putStrLn "Second isValidModuleName test passed" >> hFlush stdout
      
      result3 <- evaluate $ isValidModuleName "test-module"
      putStrLn "Third isValidModuleName test computed" >> hFlush stdout
      result3 `shouldBe` True
      putStrLn "Third isValidModuleName test passed" >> hFlush stdout
      
      result4 <- evaluate $ isValidModuleName "Test.Module"
      putStrLn "Fourth isValidModuleName test computed" >> hFlush stdout
      result4 `shouldBe` True
      putStrLn "Fourth isValidModuleName test passed" >> hFlush stdout
      
      result5 <- evaluate $ isValidModuleName ""
      putStrLn "Fifth isValidModuleName test computed" >> hFlush stdout
      result5 `shouldBe` False
      putStrLn "Fifth isValidModuleName test passed" >> hFlush stdout
      
      result6 <- evaluate $ isValidModuleName "Test@Module"
      putStrLn "Sixth isValidModuleName test computed" >> hFlush stdout
      result6 `shouldBe` False
      putStrLn "Sixth isValidModuleName test passed" >> hFlush stdout

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
        Right asts -> let (result, _) = runWriter $ renderAST asts model
                      in result `shouldBe` "test-module-detail.component.ts"
        Left err -> fail $ "Failed to parse template: " ++ err 

  describe "Module Transform" $ do
    it "transforms module name correctly" $ do
      let template = "[[module_transform kebab_case model.className]].component.ts"
          model = Model "test-module" [Property "name" StringType]
      case parseTemplate template of
        Left err -> expectationFailure $ "Parse error: " ++ err
        Right asts -> do
          let (result, _) = runWriter $ renderAST asts model
          result `shouldBe` "test-module.component.ts" 
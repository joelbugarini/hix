module TemplateSpec (spec) where

import Test.Hspec
import Template.RenderProp (applyFunc)
import qualified Data.Text as T

spec :: Spec
spec = do
  describe "applyFunc" $ do
    it "converts to upper case" $
      applyFunc (T.pack "upper") (T.pack "test") `shouldBe` T.pack "TEST"

    it "converts to lower case" $
      applyFunc (T.pack "lower") (T.pack "TEST") `shouldBe` T.pack "test"

    it "converts to snake case" $ do
      applyFunc (T.pack "snake_case") (T.pack "testCase") `shouldBe` T.pack "test_case"
      applyFunc (T.pack "snake_case") (T.pack "TestCase") `shouldBe` T.pack "test_case"
      applyFunc (T.pack "snake_case") (T.pack "test_case") `shouldBe` T.pack "test_case"

    it "converts to kebab case" $ do
      applyFunc (T.pack "kebab_case") (T.pack "testCase") `shouldBe` T.pack "test-case"
      applyFunc (T.pack "kebab_case") (T.pack "TestCase") `shouldBe` T.pack "test-case"
      applyFunc (T.pack "kebab_case") (T.pack "test-case") `shouldBe` T.pack "test-case"
      applyFunc (T.pack "kebab_case") (T.pack "test_case") `shouldBe` T.pack "test-case"

    it "converts first letter to lowercase" $ do
      applyFunc (T.pack "lowerFirst") (T.pack "TestCase") `shouldBe` T.pack "testCase"
      applyFunc (T.pack "lowerFirst") (T.pack "TEST") `shouldBe` T.pack "tEST"
      applyFunc (T.pack "lowerFirst") (T.pack "test") `shouldBe` T.pack "test"
      applyFunc (T.pack "lowerFirst") (T.pack "") `shouldBe` T.pack ""

    it "handles unknown functions" $
      applyFunc (T.pack "unknown") (T.pack "test") `shouldBe` T.pack "--[[Unknown func]] test" 
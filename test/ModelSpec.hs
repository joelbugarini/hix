module ModelSpec where

import Test.Hspec
import Model
import Data.Aeson
import qualified Data.ByteString.Lazy as B

spec :: Spec
spec = do
  describe "Model" $ do
    it "should parse model from JSON" $ do
      modelJson <- B.readFile "test/data/cli/model.json"
      let maybeModel = decode modelJson :: Maybe Model
      case maybeModel of
        Nothing -> expectationFailure "Failed to parse model JSON"
        Just model -> do
          modelName model `shouldBe` "User"
          length (modelProperties model) `shouldBe` 3
          let props = modelProperties model
          propertyName (head props) `shouldBe` "Id"
          propertyType (head props) `shouldBe` "int"
          propertyName (props !! 1) `shouldBe` "Name"
          propertyType (props !! 1) `shouldBe` "string"
          propertyName (props !! 2) `shouldBe` "Email"
          propertyType (props !! 2) `shouldBe` "string" 
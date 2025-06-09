import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import qualified Template.RenderNode as T
import Model.Model (Model(..), Property(..), PropertyType(..))
import Data.Text (Text)
import qualified Data.Text as T

main :: IO ()
main = hspec $ do
  describe "RenderNodeTest" $ do
    it "module_transform function" $ do
      let model = Model "TestModule" []
      let template = [FuncCall "module_transform" "kebab_case model.className"]
      let expected = "test-module"
      T.unpack (T.renderNode model (head template)) `shouldBe` expected

 
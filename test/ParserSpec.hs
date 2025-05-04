{-# LANGUAGE OverloadedStrings #-}

module ParserSpec where

import Test.Hspec
import qualified Data.Text as T
import Template.Parser (parseTemplate)
import Template.AST (AST(..))
import Data.List (isInfixOf)

spec :: Spec
spec = do
  describe "Template.Parser" $ do
    it "parses literal text only" $
      parseTemplate "Hello world!" `shouldBe`
        Right [Literal "Hello world!"]

    it "parses a single variable tag" $
      parseTemplate "Hello [[model.className]]!" `shouldBe`
        Right [Literal "Hello ", ModelValue "model.className", Literal "!"]

    it "parses multiple variable tags" $
      parseTemplate "[[prop.name]] = [[prop.type]];" `shouldBe`
        Right [ModelValue "prop.name", Literal " = ", ModelValue "prop.type", Literal ";"]

    it "parses unknown tags as UnknownTag" $
      parseTemplate "Hello [[unknown.tag]]!" `shouldBe`
        Right [Literal "Hello ", UnknownTag "unknown.tag", Literal "!"]

    it "handles text before and after tags" $
      parseTemplate "A [[model.className]] B" `shouldBe`
        Right [Literal "A ", ModelValue "model.className", Literal " B"]

    it "parses nested prop inside if" $
      parseTemplate "[[if model.className=Thing]][[prop]]X[[/prop]][[/if]]" `shouldBe`
        Right [IfBlock ("model.className", "Thing") [PropLoop Nothing [Literal "X"]] Nothing]

    it "parses nested if inside prop" $
      parseTemplate "[[prop]][[if prop.type=string]]S[[else]]N[[/if]][[/prop]]" `shouldBe`
        Right [PropLoop Nothing [IfBlock ("prop.type", "string") [Literal "S"] (Just [Literal "N"])]]

    it "parses an if-else block" $
      parseTemplate "[[if prop.type=string]]A[[else]]B[[/if]]" `shouldBe`
        Right [IfBlock ("prop.type", "string") [Literal "A"] (Just [Literal "B"])]

    it "parses an if block" $
      parseTemplate "[[if prop.type=string]]String![[/if]]" `shouldBe`
        Right [IfBlock ("prop.type", "string") [Literal "String!"] Nothing]

    it "parses a nested prop block with text" $
      parseTemplate "A [[prop]]B [[prop.name]];[[/prop]] C" `shouldBe`
        Right [Literal "A ", PropLoop Nothing [Literal "B ", ModelValue "prop.name", Literal ";"], Literal " C"]

    it "parses a simple prop block" $
      parseTemplate "[[prop]]int [[prop.name]];[[/prop]]" `shouldBe`
        Right [PropLoop Nothing [Literal "int ", ModelValue "prop.name", Literal ";"]] 

    it "parses a function call with model value" $
      parseTemplate "Hello [[upper model.className]]!" `shouldBe`
        Right [Literal "Hello ", FuncCall "upper" "model.className", Literal "!"]

    it "parses a function call with prop value" $
      parseTemplate "[[lower prop.name]]" `shouldBe`
        Right [FuncCall "lower" "prop.name"]

    it "parses multiple function calls" $
      parseTemplate "[[upper model.className]] [[lower prop.name]]" `shouldBe`
        Right [FuncCall "upper" "model.className", Literal " ", FuncCall "lower" "prop.name"]

    it "parses unknown function calls as FuncCall" $
      parseTemplate "[[snake_case prop.name]]" `shouldBe`
        Right [FuncCall "snake_case" "prop.name"]

    it "fails with a clear error for unclosed tag" $
      parseTemplate "Hello [[model.className" `shouldSatisfy`
        isLeftWith "unexpected end of input"

    it "fails with a clear error for unclosed prop block" $
      parseTemplate "[[prop]]X" `shouldSatisfy`
        isLeftWith "end of input"

    it "fails with a clear error for unclosed if block" $
      parseTemplate "[[if prop.type=string]]X" `shouldSatisfy`
        isLeftWith "end of input"

    it "fails with a clear error for unclosed if-else block" $
      parseTemplate "[[if prop.type=string]]A[[else]]B" `shouldSatisfy`
        isLeftWith "end of input"

    it "fails with a clear error for unexpected else" $
      parseTemplate "[[else]]" `shouldSatisfy`
        isLeftWith "unexpected"

isLeftWith :: String -> Either String a -> Bool
isLeftWith msg (Left err) = msg `isInfixOf` err || "unclosed tag" `isInfixOf` err
isLeftWith _   _          = False
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import System.Environment (getArgs)
import qualified Data.Text.IO as TIO
import qualified Data.Text as T
import Data.Text (Text)
import Data.Char (isSpace)
import System.FilePath (takeBaseName)

import Model (loadModel)
import Renderer (renderAST)
import Lexer (tokenize, Token(..))
import TemplateAST (parseTokens)
import qualified GrammarGen (writeGrammarFile)

main :: IO ()
main = do
  args <- getArgs
  case args of    
    ["--gen-grammar", out] -> 
      GrammarGen.writeGrammarFile out

    [templatePath, modelPath, outputPath] ->
      runHix templatePath modelPath (Just outputPath)

    [templatePath, modelPath] ->
      runHix templatePath modelPath Nothing

    _ -> putStrLn "Usage: hix <template.hix> <model.json> [output.txt]"

-- 🔧 Main runner with optional output path logic
runHix :: FilePath -> FilePath -> Maybe FilePath -> IO ()
runHix templatePath modelPath mOut = do
  -- 📄 Load template
  template <- TIO.readFile templatePath

  -- 🔍 Tokenize and check for suspicious tags
  let tokens = tokenize template
  mapM_ reportTagProblem tokens

  -- 🌲 Parse tokens into AST
  let ast = parseTokens tokens
  -- putStrLn "\n Parsed AST:"
  -- mapM_ print ast

  -- 📦 Load model
  result <- loadModel modelPath
  case result of
    Right model -> do
      -- 🧾 Render output
      let output = removeBlankLines $ renderAST ast model
          outputFile = case mOut of
            Just path -> path
            Nothing ->
              let base = takeBaseName modelPath
              in case getTemplateOutputExt templatePath of
                   Just ext -> base ++ "." ++ ext
                   Nothing  -> base ++ ".txt"
      TIO.writeFile outputFile output
      putStrLn $ "Code written to " ++ outputFile
    Left err -> putStrLn $ "Failed to parse model JSON: " ++ err

-- 📂 Extract extension before .hix (e.g. .cs from template.cs.hix)
getTemplateOutputExt :: FilePath -> Maybe String
getTemplateOutputExt file =
  case reverse (wordsWhen (== '.') file) of
    ("hix" : ext : _) -> Just ext
    _ -> Nothing


-- 🔍 Helper: split by a delimiter
wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s = case dropWhile p s of
  "" -> []
  s' -> w : wordsWhen p s''
    where (w, s'') = break p s'

-- 🛠 Report suspicious tags
reportTagProblem :: Token -> IO ()
reportTagProblem (Tag content line col) =
  if isValidTag content
     then pure ()
     else putStrLn $
       "[!] Suspicious tag '[[" ++ T.unpack content ++ "]]' at line "
       ++ show line ++ ", column " ++ show col
reportTagProblem _ = pure ()

-- ✅ Known tags
isValidTag :: Text -> Bool
isValidTag tag =
  tag `elem`
    [ "model.className"
    , "prop"
    , "/prop"
    , "prop.name"
    , "prop.type"
    , "/if"
    ] || any (`T.isPrefixOf` tag)
         [ "prop type="
         , "prop.ignore="
         , "if "
         , "upper "
         , "lower "
         , "snake_case "
         ]

removeBlankLines :: T.Text -> T.Text
removeBlankLines = T.unlines . filter (not . T.all isSpace) . T.lines


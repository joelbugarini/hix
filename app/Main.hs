{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.FilePath ((</>))
import Data.Maybe (fromMaybe)
import qualified Config as C
import Model (loadModel, Model(..))
import Renderer (renderAST)
import Lexer (tokenize)
import TemplateAST (parseTokens)
import Data.Text (replace)
import System.Environment (getArgs)
import qualified Wizard as W
import System.Directory (getCurrentDirectory)
import Help (helpMessage, manualMessage, version)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> putStrLn helpMessage
    ["--help"] -> putStrLn helpMessage
    ["help"] -> putStrLn helpMessage
    ["--version"] -> putStrLn version
    ["version"] -> putStrLn version
    ["man"] -> putStrLn manualMessage
    ["init"] -> do
      currentDir <- getCurrentDirectory
      putStrLn "Initializing hix configuration..."
      W.createDefaultConfig currentDir
      putStrLn "Configuration created successfully!"
      putStrLn "You can now customize the layers and templates in .hix/config.yaml"
    
    _ -> do
      modelName <- case args of
        (modelArg:_) -> return $ modelArg ++ ".json"
        [] -> do
          putStrLn "Please enter the model name (without .json extension):"
          modelInput <- getLine
          return $ modelInput ++ ".json"
      
      -- Load the model
      modelResult <- loadModel modelName
      case modelResult of
        Left err -> putStrLn $ "Error loading model: " ++ err
        Right model -> do
          -- Load the config file
          configResult <- C.loadConfig ".hix/config.yaml"
          case configResult of
            Left err -> putStrLn $ "Error loading config: " ++ err
            Right config -> do
              -- Print architecture info
              putStrLn $ "Architecture: " ++ T.unpack (C.architecture config)
              putStrLn $ "Output Root: " ++ fromMaybe "./src" (C.output_root config)
              
              -- Print layer info and generate code
              mapM_ (\layer -> do
                putStrLn $ T.unpack (C.name layer) ++ " Layer: " ++ C.path layer
                putStrLn $ "Description: " ++ T.unpack (C.description layer)
                generateCodeForLayer layer model) (C.layers config)

-- Function to generate code for a layer
generateCodeForLayer :: C.Layer -> Model -> IO ()
generateCodeForLayer layer model = mapM_ (generateTemplate layer model) (C.templates layer)

-- Helper function to generate a template
generateTemplate :: C.Layer -> Model -> C.Template -> IO ()
generateTemplate layer model tmpl = do
    -- Load the template
    templateContent <- TIO.readFile (C.template tmpl)
    -- Replace {{model.name}} in template content
    let processedContent = replace "{{model.name}}" (className model) templateContent
    -- Tokenize and parse the template
    let tokens = tokenize processedContent
        ast = parseTokens tokens
        renderedOutput = renderAST ast model
        -- Replace {{model.name}} with actual model name in filename
        outputFilename = T.unpack $ replace "{{model.name}}" (className model) (C.filename tmpl)
        outputPath = C.path layer </> outputFilename
    -- Write the output to the file
    TIO.writeFile outputPath renderedOutput
    putStrLn $ "Generated " ++ outputPath


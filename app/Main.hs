{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Data.Text as T
import System.FilePath ((</>))
import Data.Maybe (fromMaybe, isJust)
import qualified Config.Config as C
import Model.Model (loadModel, Model(..), Property(..))
import System.Environment (getArgs)
import qualified CLI.Wizard as W
import System.Directory (getCurrentDirectory, createDirectoryIfMissing, doesFileExist)
import CLI.Help (helpMessage, manualMessage, version)
import Data.List (find)
import System.Exit (exitFailure)
import Control.Monad (when)
import Template.Lexer (tokenize)
import Template.AST (parseTokens)
import Template.Renderer (renderAST)
import qualified Data.Text.IO as TIO

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> putStrLn helpMessage
    ["--help"] -> putStrLn helpMessage
    ["help"] -> putStrLn helpMessage
    ["--version"] -> putStr version
    ["version"] -> putStr version
    ["man"] -> putStrLn manualMessage
    ["init"] -> do
      currentDir <- getCurrentDirectory
      putStrLn "Initializing hix configuration..."
      W.createDefaultConfig currentDir
      putStrLn "Configuration created successfully!"
      putStrLn "You can now customize the layers and templates in .hix/config.yaml"
    
    "generate":restArgs -> handleGenerateCommand restArgs
    
    (modelArg:_) -> do
      let modelName = modelArg ++ ".json"
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

-- Handle the generate command with its arguments
handleGenerateCommand :: [String] -> IO ()
handleGenerateCommand cmdArgs = do
  let modelArg = findArg "--model" cmdArgs
      layerArg = findArg "--layer" cmdArgs
      templateArg = findArg "--template" cmdArgs
  configExists <- doesFileExist ".hix/config.yaml"
  -- Minimal mode ONLY if both --model and --template are present AND config does not exist
  if isJust modelArg && isJust templateArg && not configExists then do
    let Just modelPath = modelArg
        Just templatePath = templateArg
    modelResult <- loadModel modelPath
    case modelResult of
      Left err -> do
        putStrLn $ "Error loading model: " ++ err
        exitFailure
      Right modelData -> do
        fileExists <- doesFileExist templatePath
        if not fileExists
          then do
            putStrLn $ "Error: Template '" ++ templatePath ++ "' not found"
            exitFailure
          else do
            templateContent <- readFile templatePath
            let tokens = tokenize (T.pack templateContent)
                ast = parseTokens tokens
                code = renderAST ast modelData
            TIO.putStrLn code
  else do
    let modelArg = findArg "--model" cmdArgs
        layerArg = findArg "--layer" cmdArgs
        templateArg = findArg "--template" cmdArgs
    if isJust layerArg && isJust templateArg then do
      putStrLn "Error: Cannot specify both --layer and --template"
      exitFailure
    else case modelArg of
      Nothing -> do
        putStrLn "Error: --model parameter is required"
        exitFailure
      Just modelPath -> do
        modelResult <- loadModel modelPath
        case modelResult of
          Left err -> do
            putStrLn $ "Error loading model: " ++ err
            exitFailure
          Right modelData -> do
            configResult <- C.loadConfig ".hix/config.yaml"
            case configResult of
              Left err -> do
                putStrLn $ "Error loading config: " ++ err
                exitFailure
              Right config -> do
                case (layerArg, templateArg) of
                  (Just layer, Nothing) -> generateForLayer config modelData layer
                  (Nothing, Just tmpl) -> generateForTemplate config modelData tmpl
                  (Nothing, Nothing) -> generateForAllLayers config modelData
                  _ -> do
                    putStrLn "Error: Cannot specify both --layer and --template"
                    exitFailure

  where
    findArg :: String -> [String] -> Maybe String
    findArg flag xs = case dropWhile (/= flag) xs of
      (_:value:_) -> Just value
      _ -> Nothing

-- Generate code for all layers
generateForAllLayers :: C.Config -> Model -> IO ()
generateForAllLayers config modelData = do
  putStrLn $ "Architecture: " ++ T.unpack (C.architecture config)
  putStrLn $ "Output Root: " ++ fromMaybe "./src" (C.output_root config)
  mapM_ (\layer -> do
    putStrLn $ T.unpack (C.name layer) ++ " Layer: " ++ C.path layer
    putStrLn $ "Description: " ++ T.unpack (C.description layer)
    generateCodeForLayer layer modelData) (C.layers config)

-- Generate code for a specific layer
generateForLayer :: C.Config -> Model -> String -> IO ()
generateForLayer config modelData layerName = do
  case find (\layer -> T.unpack (C.name layer) == layerName) (C.layers config) of
    Nothing -> do
      putStrLn $ "Error: Layer '" ++ layerName ++ "' not found"
      putStrLn "Available layers:"
      mapM_ (\layer -> putStrLn $ "  - " ++ T.unpack (C.name layer)) (C.layers config)
      exitFailure
    Just layer -> do
      putStrLn $ "Generating code for layer: " ++ T.unpack (C.name layer)
      generateCodeForLayer layer modelData

-- Generate code for a specific template
generateForTemplate :: C.Config -> Model -> String -> IO ()
generateForTemplate config modelData userTemplatePath = do
  let normInput = normalizeFilePath (trim userTemplatePath)
      matchingTemplates = concatMap (\layer -> 
        filter (\tmpl -> normalizeFilePath (trim (C.template tmpl)) == normInput) (C.templates layer)) 
        (C.layers config)
  case matchingTemplates of
    [] -> do
      putStrLn $ "Error: Template '" ++ userTemplatePath ++ "' not found"
      exitFailure
    (tmpl:_) -> do
      putStrLn $ "Looking for template: " ++ userTemplatePath
      putStrLn "Available templates:"
      mapM_ (\layer -> do
        putStrLn $ "  Layer: " ++ T.unpack (C.name layer)
        mapM_ (\tmpl' -> do
          putStrLn $ "    Template path: " ++ C.template tmpl'
          putStrLn $ "    Normalized template path: " ++ normalizeFilePath (C.template tmpl')
          ) (C.templates layer)
        ) (C.layers config)
      putStrLn $ "Normalized search path: " ++ normInput
      case find (\l -> tmpl `elem` C.templates l) (C.layers config) of
        Nothing -> error "Internal error: template found but layer not found"
        Just layer -> do
          putStrLn $ "Generating code for template: " ++ userTemplatePath
          generateTemplateWithUserPath layer modelData tmpl userTemplatePath
  where
    normalizeFilePath :: FilePath -> FilePath
    normalizeFilePath = map (\c -> if c == '\\' then '/' else c)
    trim = f . f
      where f = reverse . dropWhile (== ' ')

-- Helper function to generate a template, with user-supplied path for error reporting
generateTemplateWithUserPath :: C.Layer -> Model -> C.Template -> String -> IO ()
generateTemplateWithUserPath layer modelData template userTemplatePath = do
  let templatePath = C.template template
  fileExists <- doesFileExist templatePath
  if not fileExists
    then do
      putStrLn $ "Error: Template '" ++ userTemplatePath ++ "' not found"
      exitFailure
    else do
      let outputPath = C.path layer </> T.unpack (T.replace "[[model.className]]" (className modelData) $ C.filename template)
      createDirectoryIfMissing True (C.path layer)
      templateContent <- readFile templatePath
      let tokens = tokenize (T.pack templateContent)
          ast = parseTokens tokens
          code = renderAST ast modelData
      TIO.writeFile outputPath code

-- Function to generate code for a layer
generateCodeForLayer :: C.Layer -> Model -> IO ()
generateCodeForLayer layer modelData = do
  putStrLn $ "Generating code for layer: " ++ T.unpack (C.name layer)
  mapM_ (\tmpl -> generateTemplateWithUserPath layer modelData tmpl (C.template tmpl)) (C.templates layer)


{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Data.Text as T
import System.FilePath ((</>))
import Data.Maybe (fromMaybe, isJust)
import qualified Config as C
import Model (loadModel, Model(..), Property(..))
import System.Environment (getArgs)
import qualified Wizard as W
import System.Directory (getCurrentDirectory, createDirectoryIfMissing)
import Help (helpMessage, manualMessage, version)
import Data.List (find)
import System.Exit (exitFailure)
import Control.Monad (when)

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
  -- Parse arguments
  let modelArg = findArg "--model" cmdArgs
      layerArg = findArg "--layer" cmdArgs
      templateArg = findArg "--template" cmdArgs
  
  -- Check for required model argument
  case modelArg of
    Nothing -> do
      putStrLn "Error: --model parameter is required"
      exitFailure
    Just modelPath -> do
      -- Check for mutually exclusive arguments
      when (isJust layerArg && isJust templateArg) $ do
        putStrLn "Error: Cannot specify both --layer and --template"
        exitFailure
      
      -- Load model
      modelResult <- loadModel modelPath
      case modelResult of
        Left err -> do
          putStrLn $ "Error loading model: " ++ err
          exitFailure
        Right modelData -> do
          -- Load config
          configResult <- C.loadConfig ".hix/config.yaml"
          case configResult of
            Left err -> do
              putStrLn $ "Error loading config: " ++ err
              exitFailure
            Right config -> do
              -- Generate based on arguments
              case (layerArg, templateArg) of
                (Just layer, Nothing) -> generateForLayer config modelData layer
                (Nothing, Just tmpl) -> generateForTemplate config modelData tmpl
                (Nothing, Nothing) -> generateForAllLayers config modelData
                _ -> error "Unreachable case: layer and template both specified"

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
generateForTemplate config modelData templatePath = do
  putStrLn $ "Looking for template: " ++ templatePath
  putStrLn "Available templates:"
  mapM_ (\layer -> do
    putStrLn $ "  Layer: " ++ T.unpack (C.name layer)
    mapM_ (\tmpl -> do
      putStrLn $ "    Template path: " ++ C.template tmpl
      putStrLn $ "    Normalized template path: " ++ normalizeFilePath (C.template tmpl)
      ) (C.templates layer)
    ) (C.layers config)
  putStrLn $ "Normalized search path: " ++ normalizeFilePath templatePath
  let matchingTemplates = concatMap (\layer -> 
        filter (\tmpl -> normalizeFilePath (C.template tmpl) == normalizeFilePath templatePath) (C.templates layer)) 
        (C.layers config)
  case matchingTemplates of
    [] -> do
      putStrLn $ "Error: Template '" ++ templatePath ++ "' not found"
      putStrLn "Available templates:"
      mapM_ (\layer -> do
        putStrLn $ "  Layer: " ++ T.unpack (C.name layer)
        mapM_ (\tmpl -> putStrLn $ "    - " ++ C.template tmpl) (C.templates layer)
        ) (C.layers config)
      exitFailure
    (tmpl:_) -> do
      case find (\l -> tmpl `elem` C.templates l) (C.layers config) of
        Nothing -> error "Internal error: template found but layer not found"
        Just layer -> do
          putStrLn $ "Generating code for template: " ++ templatePath
          generateTemplate layer modelData tmpl
  where
    normalizeFilePath :: FilePath -> FilePath
    normalizeFilePath = map (\c -> if c == '\\' then '/' else c)

-- Function to generate code for a layer
generateCodeForLayer :: C.Layer -> Model -> IO ()
generateCodeForLayer layer modelData = do
  putStrLn $ "Generating code for layer: " ++ T.unpack (C.name layer)
  mapM_ (generateTemplate layer modelData) (C.templates layer)

-- Helper function to generate a template
generateTemplate :: C.Layer -> Model -> C.Template -> IO ()
generateTemplate layer modelData template = do
  let outputPath = C.path layer </> T.unpack (T.replace "[[model.className]]" (className modelData) $ C.filename template)
  putStrLn $ "Generating file: " ++ outputPath
  
  -- Create output directory if it doesn't exist
  createDirectoryIfMissing True (C.path layer)
  
  -- Read template file
  templateContent <- readFile (C.template template)
  
  -- Generate code
  let code = renderTemplate templateContent modelData
  writeFile outputPath code
  where
    renderTemplate :: String -> Model -> String
    renderTemplate content m = T.unpack $ T.replace "[[model.className]]" (className m) $
                             T.replace "[[prop]]" propLoop $
                             T.replace "[[/prop]]" "" $
                             T.pack content
    
    propLoop :: T.Text
    propLoop = T.unlines $ map (\prop -> 
      T.replace "[[prop.type]]" (T.pack $ show $ propType prop) $
      T.replace "[[prop.name]]" (propName prop) $
      "  public [[prop.type]] [[prop.name]] { get; set; }") (properties modelData)


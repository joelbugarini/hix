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
import Template.Renderer (renderAST, warnOnUnhandledTokens)
import qualified Data.Text.IO as TIO
import Template.Parser (parseTemplate)
import qualified Grammar.GrammarGen as GrammarGen
import Logging (LogConfig(..), LogLevel(..), LogState, defaultConfig, initLogging, closeLogging, logInfo, logError, logWarning)
import Control.Monad.Writer (runWriter)
import System.IO (hFlush, stdout)

main :: IO ()
main = do
  args <- getArgs
  -- Initialize logging with default config
  logState <- initLogging defaultConfig { logLevel = INFO }
  
  -- Handle logging cleanup
  let cleanup = closeLogging logState
  
  case args of
    [] -> do
      putStrLn helpMessage
      cleanup
    ["--help"] -> do
      putStrLn helpMessage
      cleanup
    ["help"] -> do
      putStrLn helpMessage
      cleanup
    ["--version"] -> do
      putStr version
      cleanup
    ["version"] -> do
      putStr version
      cleanup
    ["man"] -> do
      putStrLn manualMessage
      cleanup
    ["init"] -> do
      currentDir <- getCurrentDirectory
      putStrLn "Initializing hix configuration..."
      W.createDefaultConfig currentDir
      putStrLn "Configuration created successfully!"
      putStrLn "You can now customize the layers and templates in .hix/config.yaml"
      cleanup
    ["--gen-grammar", out] -> do
      logInfo logState "Grammar" (T.pack $ "Generating grammar file: " ++ out) Nothing
      GrammarGen.writeGrammarFile out
      cleanup
    ["generate-grammar", out] -> do
      logInfo logState "Grammar" (T.pack $ "Generating grammar file: " ++ out) Nothing
      GrammarGen.writeGrammarFile out
      cleanup
    ["--gen-grammar"] -> do
      putStrLn "Please provide an output file, e.g. hix --gen-grammar hix.tmLanguage.json"
      cleanup
    ["generate-grammar"] -> do
      putStrLn "Please provide an output file, e.g. hix generate-grammar hix.tmLanguage.json"
      cleanup
    "generate":restArgs -> do
      newState <- handleGenerateCommand logState restArgs
      closeLogging newState
    (modelArg:_) -> do
      let modelName = modelArg ++ ".json"
      -- Load the model
      modelResult <- loadModel modelName
      case modelResult of
        Left err -> do
          logError logState "Model" (T.pack $ "Error loading model: " ++ err) Nothing
          putStrLn $ "Error loading model: " ++ err
          hFlush stdout
          cleanup
        Right model -> do
          -- Load the config file
          configResult <- C.loadConfig ".hix/config.yaml"
          case configResult of
            Left err -> do
              logError logState "Config" (T.pack $ "Error loading config: " ++ err) Nothing
              putStrLn $ "Error loading config: " ++ err
              hFlush stdout
              cleanup
            Right config -> do
              -- Print architecture info
              putStrLn $ "Architecture: " ++ T.unpack (C.architecture config)
              putStrLn $ "Output Root: " ++ fromMaybe "./src" (C.output_root config)
              -- Print layer info and generate code
              newState <- mapM_ (\layer -> do
                putStrLn $ T.unpack (C.name layer) ++ " Layer: " ++ C.path layer
                putStrLn $ "Description: " ++ T.unpack (C.description layer)
                generateCodeForLayer logState layer model) (C.layers config)
              cleanup

-- Handle the generate command with its arguments
handleGenerateCommand :: LogState -> [String] -> IO LogState
handleGenerateCommand logState cmdArgs = do
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
        newState <- logError logState "Model" (T.pack $ "Error loading model: " ++ err) Nothing
        putStrLn $ "Error loading model: " ++ err
        hFlush stdout
        exitFailure
        return newState
      Right modelData -> do
        fileExists <- doesFileExist templatePath
        if not fileExists
          then do
            newState <- logError logState "Template" (T.pack $ "Template not found: " ++ templatePath) Nothing
            putStrLn $ "Error: Template '" ++ templatePath ++ "' not found"
            hFlush stdout
            exitFailure
            return newState
          else do
            templateContent <- readFile templatePath
            case parseTemplate (T.pack templateContent) of
              Left err -> do
                newState <- logError logState "Parser" (T.pack $ "Template parse error: " ++ err) Nothing
                putStrLn ("Template parse error: " ++ err)
                hFlush stdout
                exitFailure
                return newState
              Right ast -> do
                let (code, logs) = runWriter (renderAST ast modelData)
                    logFilePath = ".hix/log.txt"
                createDirectoryIfMissing True ".hix"
                TIO.writeFile logFilePath (T.unlines logs)
                warnOnUnhandledTokens code
                TIO.putStrLn code
                return logState
  else do
    let modelArg = findArg "--model" cmdArgs
        layerArg = findArg "--layer" cmdArgs
        templateArg = findArg "--template" cmdArgs
    if isJust layerArg && isJust templateArg then do
      putStrLn "Error: Cannot specify both --layer and --template"
      hFlush stdout
      exitFailure
      return logState
    else case modelArg of
      Nothing -> do
        putStrLn "Error: --model parameter is required"
        hFlush stdout
        exitFailure
        return logState
      Just modelPath -> do
        modelResult <- loadModel modelPath
        case modelResult of
          Left err -> do
            putStrLn $ "Error loading model: " ++ err
            hFlush stdout
            exitFailure
            return logState
          Right modelData -> do
            configResult <- C.loadConfig ".hix/config.yaml"
            case configResult of
              Left err -> do
                putStrLn $ "Error loading config: " ++ err
                hFlush stdout
                exitFailure
                return logState
              Right config -> do
                case (layerArg, templateArg) of
                  (Just layer, Nothing) -> generateForLayer logState config modelData layer
                  (Nothing, Just tmpl) -> generateForTemplate logState config modelData tmpl
                  (Nothing, Nothing) -> generateForAllLayers logState config modelData
                  _ -> do
                    putStrLn "Error: Cannot specify both --layer and --template"
                    hFlush stdout
                    exitFailure
                    return logState

  where
    findArg :: String -> [String] -> Maybe String
    findArg flag xs = case dropWhile (/= flag) xs of
      (_:value:_) -> Just value
      _ -> Nothing

-- Generate code for all layers
generateForAllLayers :: LogState -> C.Config -> Model -> IO LogState
generateForAllLayers logState config modelData = do
  putStrLn $ "Architecture: " ++ T.unpack (C.architecture config)
  putStrLn $ "Output Root: " ++ fromMaybe "./src" (C.output_root config)
  mapM_ (\layer -> do
    putStrLn $ T.unpack (C.name layer) ++ " Layer: " ++ C.path layer
    putStrLn $ "Description: " ++ T.unpack (C.description layer)
    generateCodeForLayer logState layer modelData) (C.layers config)
  return logState

-- Generate code for a specific layer
generateForLayer :: LogState -> C.Config -> Model -> String -> IO LogState
generateForLayer logState config modelData layerName = do
  case find (\layer -> T.unpack (C.name layer) == layerName) (C.layers config) of
    Nothing -> do
      putStrLn $ "Error: Layer '" ++ layerName ++ "' not found"
      hFlush stdout
      putStrLn "Available layers:"
      hFlush stdout
      mapM_ (\layer -> putStrLn $ "  - " ++ T.unpack (C.name layer)) (C.layers config)
      exitFailure
      return logState
    Just layer -> do
      putStrLn $ "Generating code for layer: " ++ T.unpack (C.name layer)
      generateCodeForLayer logState layer modelData
      return logState

-- Generate code for a specific template
generateForTemplate :: LogState -> C.Config -> Model -> String -> IO LogState
generateForTemplate logState config modelData userTemplatePath = do
  let normInput = normalizeFilePath (trim userTemplatePath)
      matchingTemplates = concatMap (\layer -> 
        filter (\tmpl -> normalizeFilePath (trim (C.template tmpl)) == normInput) (C.templates layer)) 
        (C.layers config)
  case matchingTemplates of
    [] -> do
      putStrLn $ "Error: Template '" ++ userTemplatePath ++ "' not found"
      hFlush stdout
      exitFailure
      return logState
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
        Nothing -> do
          newState <- logError logState "Internal" (T.pack "Internal error: template found but layer not found") Nothing
          error "Internal error: template found but layer not found"
          return newState
        Just layer -> do
          putStrLn $ "Generating code for template: " ++ userTemplatePath
          generateTemplateWithUserPath logState layer modelData tmpl userTemplatePath
          return logState
  where
    normalizeFilePath :: FilePath -> FilePath
    normalizeFilePath = map (\c -> if c == '\\' then '/' else c)
    trim = f . f
      where f = reverse . dropWhile (== ' ')

-- Helper function to generate a template, with user-supplied path for error reporting
generateTemplateWithUserPath :: LogState -> C.Layer -> Model -> C.Template -> String -> IO LogState
generateTemplateWithUserPath logState layer modelData template userTemplatePath = do
  let templatePath = C.template template
  fileExists <- doesFileExist templatePath
  if not fileExists
    then do
      putStrLn $ "Error: Template '" ++ userTemplatePath ++ "' not found"
      hFlush stdout
      exitFailure
      return logState
    else do
      templateContent <- readFile templatePath
      case parseTemplate (T.pack templateContent) of
        Left err -> do
          newState <- logError logState "Parser" (T.pack $ "Template parse error: " ++ err) Nothing
          putStrLn ("Template parse error: " ++ err)
          hFlush stdout
          exitFailure
          return newState
        Right ast -> do
          let filenameTemplate = C.filename template
          case parseTemplate filenameTemplate of
            Left err -> do
              newState <- logError logState "Parser" (T.pack $ "Filename template parse error: " ++ err) Nothing
              putStrLn ("Filename template parse error: " ++ err)
              hFlush stdout
              exitFailure
              return newState
            Right filenameAst -> do
              let (renderedFilename, _) = runWriter (renderAST filenameAst modelData)
                  outputPath = C.path layer </> T.unpack renderedFilename
              createDirectoryIfMissing True (C.path layer)
              let (code, logs) = runWriter (renderAST ast modelData)
                  logFilePath = ".hix/log.txt"
              createDirectoryIfMissing True ".hix"
              TIO.writeFile logFilePath (T.unlines logs)
              warnOnUnhandledTokens code
              TIO.writeFile outputPath code
              return logState

-- Function to generate code for a layer
generateCodeForLayer :: LogState -> C.Layer -> Model -> IO LogState
generateCodeForLayer logState layer modelData = do
  putStrLn $ "Generating code for layer: " ++ T.unpack (C.name layer)
  mapM_ (\tmpl -> generateTemplateWithUserPath logState layer modelData tmpl (C.template tmpl)) (C.templates layer)
  return logState


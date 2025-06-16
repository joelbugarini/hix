{-# LANGUAGE OverloadedStrings #-}

module CLI.Generate (
    runGenerate,
    runMinimalGenerate,
    runFullGenerate
) where

import CLI.Command (GenerateOptions(..), AppEnv(..), App)
import qualified Config.Config as C
import Model.Model (loadModel, Model)
import Template.Renderer (renderAST, warnOnUnhandledTokens)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory (doesFileExist, createDirectoryIfMissing)
import Control.Monad.Except (throwError)
import Control.Monad.Reader (ask)
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (fromMaybe)
import Data.List (find)
import Control.Monad.Writer (runWriter)
import System.FilePath ((</>))
import Template.Parser (parseTemplate)

runGenerate :: GenerateOptions -> App ()
runGenerate opts = do
  env <- ask
  configExists <- liftIO $ doesFileExist ".hix/config.yaml"
  case (modelPath opts, templatePath opts, configExists) of
    (Just model, Just template, False) -> runMinimalGenerate env model template
    _ -> runFullGenerate env opts

runMinimalGenerate :: AppEnv -> String -> String -> App ()
runMinimalGenerate env modelPath templatePath = do
  model <- loadModelWithError modelPath
  templateExists <- liftIO $ doesFileExist templatePath
  if not templateExists
    then throwError $ "Template not found: " ++ templatePath
    else do
      templateContent <- liftIO $ readFile templatePath
      case parseTemplate (T.pack templateContent) of
        Left err -> throwError $ "Template parse error: " ++ err
        Right ast -> do
          let (code, logs) = runWriter (renderAST ast model)
          liftIO $ do
            createDirectoryIfMissing True ".hix"
            TIO.writeFile ".hix/log.txt" (T.unlines logs)
            warnOnUnhandledTokens code
            TIO.putStrLn code

runFullGenerate :: AppEnv -> GenerateOptions -> App ()
runFullGenerate env opts = do
  case modelPath opts of
    Nothing -> throwError "--model parameter is required"
    Just modelPath -> do
      model <- loadModelWithError modelPath
      config <- loadConfigWithError ".hix/config.yaml"
      case (layerName opts, templatePath opts) of
        (Just layer, Nothing) -> generateForLayer env config model layer
        (Nothing, Just tmpl) -> generateForTemplate env config model tmpl
        (Nothing, Nothing) -> generateForAllLayers env config model
        _ -> throwError "Cannot specify both --layer and --template"

loadModelWithError :: String -> App Model
loadModelWithError path = do
  result <- liftIO $ loadModel path
  either (throwError . ("Error loading model: " ++)) return result

loadConfigWithError :: FilePath -> App C.Config
loadConfigWithError path = do
  result <- liftIO $ C.loadConfig path
  either (throwError . ("Error loading config: " ++)) return result

generateForAllLayers :: AppEnv -> C.Config -> Model -> App ()
generateForAllLayers env config modelData = do
  liftIO $ do
    putStrLn $ "Architecture: " ++ T.unpack (C.architecture config)
    putStrLn $ "Output Root: " ++ fromMaybe "./src" (C.output_root config)
  mapM_ (\layer -> do
    liftIO $ do
      putStrLn $ T.unpack (C.name layer) ++ " Layer: " ++ C.path layer
      putStrLn $ "Description: " ++ T.unpack (C.description layer)
    generateCodeForLayer env layer modelData) (C.layers config)

generateForLayer :: AppEnv -> C.Config -> Model -> String -> App ()
generateForLayer env config modelData layerName = do
  case find (\layer -> T.unpack (C.name layer) == layerName) (C.layers config) of
    Nothing -> do
      liftIO $ do
        putStrLn $ "Error: Layer '" ++ layerName ++ "' not found"
        putStrLn "Available layers:"
        mapM_ (\layer -> putStrLn $ "  - " ++ T.unpack (C.name layer)) (C.layers config)
      throwError $ "Layer '" ++ layerName ++ "' not found"
    Just layer -> do
      liftIO $ putStrLn $ "Generating code for layer: " ++ T.unpack (C.name layer)
      generateCodeForLayer env layer modelData

generateForTemplate :: AppEnv -> C.Config -> Model -> String -> App ()
generateForTemplate env config modelData userTemplatePath = do
  let normInput = normalizeFilePath (trim userTemplatePath)
      matchingTemplates = concatMap (\layer -> 
        filter (\tmpl -> normalizeFilePath (trim (C.template tmpl)) == normInput) (C.templates layer)) 
        (C.layers config)
  case matchingTemplates of
    [] -> throwError $ "Template '" ++ userTemplatePath ++ "' not found"
    (tmpl:_) -> do
      liftIO $ do
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
        Nothing -> throwError "Internal error: template found but layer not found"
        Just layer -> do
          liftIO $ putStrLn $ "Generating code for template: " ++ userTemplatePath
          generateTemplateWithUserPath env layer modelData tmpl userTemplatePath

generateTemplateWithUserPath :: AppEnv -> C.Layer -> Model -> C.Template -> String -> App ()
generateTemplateWithUserPath env layer modelData template userTemplatePath = do
  let templatePath = C.template template
  fileExists <- liftIO $ doesFileExist templatePath
  if not fileExists
    then throwError $ "Template '" ++ userTemplatePath ++ "' not found"
    else do
      templateContent <- liftIO $ readFile templatePath
      case parseTemplate (T.pack templateContent) of
        Left err -> throwError $ "Template parse error: " ++ err
        Right ast -> do
          let filenameTemplate = C.filename template
          case parseTemplate filenameTemplate of
            Left err -> throwError $ "Filename template parse error: " ++ err
            Right filenameAst -> do
              let (renderedFilename, _) = runWriter (renderAST filenameAst modelData)
                  outputPath = C.path layer </> T.unpack renderedFilename
              liftIO $ createDirectoryIfMissing True (C.path layer)
              let (code, logs) = runWriter (renderAST ast modelData)
              liftIO $ do
                createDirectoryIfMissing True ".hix"
                TIO.writeFile ".hix/log.txt" (T.unlines logs)
                warnOnUnhandledTokens code
                TIO.writeFile outputPath code

generateCodeForLayer :: AppEnv -> C.Layer -> Model -> App ()
generateCodeForLayer env layer modelData = do
  liftIO $ putStrLn $ "Generating code for layer: " ++ T.unpack (C.name layer)
  mapM_ (\tmpl -> generateTemplateWithUserPath env layer modelData tmpl (C.template tmpl)) (C.templates layer)

normalizeFilePath :: FilePath -> FilePath
normalizeFilePath = map (\c -> if c == '\\' then '/' else c)

trim :: String -> String
trim = f . f
  where f = reverse . dropWhile (== ' ')

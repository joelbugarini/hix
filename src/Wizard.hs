{-# LANGUAGE OverloadedStrings #-}

module Wizard
  ( createDefaultConfig
  , createExampleTemplate
  ) where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory
import System.FilePath
import Data.Maybe (fromMaybe)
import Control.Monad (when)
import qualified Config as C
import System.IO (hFlush, stdout)
import Data.List (intercalate)

-- Architecture presets
data Architecture = Architecture
  { name :: String
  , layers :: [(String, String)]
  }

-- Available architectures
architectures :: [Architecture]
architectures =
  [ Architecture "onion"
      [ ("Domain", "Contains business logic and domain models")
      , ("Application", "Contains application services and use cases")
      , ("Infrastructure", "Contains technical implementations")
      , ("Presentation", "Contains UI and API controllers")
      ]
  , Architecture "clean"
      [ ("Entities", "Contains business entities and value objects")
      , ("UseCases", "Contains application use cases and business rules")
      , ("InterfaceAdapters", "Contains controllers, gateways, and presenters")
      , ("FrameworksAndDrivers", "Contains frameworks, tools, and external interfaces")
      ]
  , Architecture "hexagonal"
      [ ("Domain", "Contains business logic and domain models")
      , ("Application", "Contains application services and ports")
      , ("Adapters", "Contains adapters for external systems")
      , ("Infrastructure", "Contains technical implementations")
      ]
  , Architecture "custom" []
  ]

-- Create default configuration
createDefaultConfig :: FilePath -> IO ()
createDefaultConfig root = do
  -- Create .hix directory structure
  let hixRoot = root </> ".hix"
  createDirectoryIfMissing True hixRoot
  createDirectoryIfMissing True (hixRoot </> "templates")
  createDirectoryIfMissing True (hixRoot </> "output")
  createDirectoryIfMissing True (hixRoot </> "models")
  
  -- Select architecture
  putStrLn "\nAvailable architectures:"
  mapM_ (\(i, arch) -> putStrLn $ show i ++ ". " ++ name arch) (zip [1..] architectures)
  putStrLn $ "\nSelect an architecture (1-" ++ show (length architectures) ++ "):"
  hFlush stdout
  archIndex <- readLn :: IO Int
  let selectedArch = architectures !! (archIndex - 1)
  
  -- Get layers based on selected architecture
  defaultLayers <- if name selectedArch == "custom"
    then getCustomLayers
    else return $ map (\(n, d) -> C.Layer (T.pack n) (hixRoot </> "output" </> n) (T.pack d) []) (layers selectedArch)
  
  -- Create layer directories in output folder
  mapM_ (\layer -> createDirectoryIfMissing True (C.path layer)) defaultLayers
  
  -- Ask user if they want to configure templates
  putStrLn "\nWould you like to configure templates for each layer? (y/n)"
  hFlush stdout
  configureTemplates <- getLine
  let layersWithTemplates = if configureTemplates == "y"
        then mapM (configureLayerTemplates hixRoot) defaultLayers
        else return defaultLayers
  
  layers <- layersWithTemplates
  
  let config = C.Config
        { C.architecture = T.pack $ name selectedArch
        , C.output_root = Just (hixRoot </> "output")
        , C.layers = layers
        , C.models_path = Just (hixRoot </> "models")
        , C.templates_root = Just (hixRoot </> "templates")
        , C.naming = Just $ C.Naming "PascalCase" "camelCase"
        , C.options = Just $ C.Options False False
        }
  
  -- Write config file
  TIO.writeFile (hixRoot </> "config.yaml") $ T.unlines
    [ "# hix configuration file"
    , "# This file contains the configuration for the hix code generator"
    , ""
    , "# The type of architecture (purely for documentation purposes)"
    , "architecture: " <> C.architecture config
    , ""
    , "# Output root"
    , "output_root: " <> T.pack (fromMaybe (hixRoot </> "output") (C.output_root config))
    , ""
    , "# Layers configuration"
    , "layers:"
    ] <> T.concat (map formatLayer layers)
  
  -- Create .gitignore in .hix directory
  TIO.writeFile (hixRoot </> ".gitignore") $ T.unlines
    [ "# Ignore generated files"
    , "output/*"
    , "!output/.gitkeep"
    , ""
    , "# Keep the output directory in git"
    , "!.gitkeep"
    ]
  
  -- Create .gitkeep in output directory
  TIO.writeFile (hixRoot </> "output" </> ".gitkeep") ""

-- Get custom layers from user input
getCustomLayers :: IO [C.Layer]
getCustomLayers = do
  putStrLn "\nEnter custom layer names and descriptions (one per line, empty line to finish):"
  putStrLn "Format: LayerName:Description"
  hFlush stdout
  layers <- getCustomLayers'
  return $ map (\(name, desc) -> C.Layer (T.pack name) (name) (T.pack desc) []) layers
  where
    getCustomLayers' :: IO [(String, String)]
    getCustomLayers' = do
      line <- getLine
      if null line
        then return []
        else do
          let (name, desc) = case break (== ':') line of
                (n, ':':d) -> (n, d)
                (n, _) -> (n, "Custom layer")
          rest <- getCustomLayers'
          return ((name, desc) : rest)

-- Configure templates for a layer
configureLayerTemplates :: FilePath -> C.Layer -> IO C.Layer
configureLayerTemplates hixRoot layer = do
  putStrLn $ "\nConfiguring templates for " ++ T.unpack (C.name layer) ++ " layer"
  putStrLn "Enter template names (one per line, empty line to finish):"
  hFlush stdout
  templates <- getTemplates
  let templateFiles = map (\name -> C.Template 
        (hixRoot </> "templates" </> toLower (T.unpack (C.name layer)) </> name <.> "hix")
        (T.pack $ "[[model.className]]" ++ name <.> "cs")
        "model") templates
  return layer { C.templates = templateFiles }
  where
    getTemplates :: IO [String]
    getTemplates = do
      line <- getLine
      if null line
        then return []
        else do
          rest <- getTemplates
          return (line : rest)

-- Helper function to format a layer for YAML output
formatLayer :: C.Layer -> T.Text
formatLayer layer = T.unlines $
  [ "  - name: " <> C.name layer
  , "    path: " <> T.pack (C.path layer)
  , "    description: " <> C.description layer
  , "    templates:"
  ] ++ map formatTemplate (C.templates layer)

-- Helper function to format a template for YAML output
formatTemplate :: C.Template -> T.Text
formatTemplate tmpl = T.unlines
  [ "      - template: " <> T.pack (C.template tmpl)
  , "        filename: " <> C.filename tmpl
  , "        output_by: " <> C.output_by tmpl
  ]

-- Create example template for a layer
createExampleTemplate :: FilePath -> String -> IO ()
createExampleTemplate templateDir layer = do
  let templateFile = templateDir </> toLower layer </> "Entity.hix"
  createDirectoryIfMissing True (templateDir </> toLower layer)
  let content = T.unlines
        [ "-- Example template for " <> T.pack layer <> " layer"
        , "-- This is a sample template that will be used to generate code"
        , "-- You can modify this template to match your needs"
        , ""
        , "public class [[model.className]] {"
        , "    [[prop]]"
        , "    public [[prop.type]] [[prop.name]] { get; set; }"
        , "    [[/prop]]"
        , "}"
        ]
  TIO.writeFile templateFile content

toLower :: String -> String
toLower = map toLowerChar
  where
    toLowerChar c
      | c >= 'A' && c <= 'Z' = toEnum (fromEnum c + 32)
      | otherwise = c 
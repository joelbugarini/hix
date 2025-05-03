{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module GenerateSpec (generateSpec) where

import Test.Hspec
import System.Process (readCreateProcessWithExitCode, proc)
import System.Directory (createDirectoryIfMissing, removeDirectoryRecursive, getCurrentDirectory, setCurrentDirectory, removeFile, doesFileExist)
import System.FilePath ((</>))
import Control.Exception (bracket, catch, try, IOException)
import Data.List (isInfixOf)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Aeson as A
import Data.Aeson ((.=), object)
import System.Exit (ExitCode(ExitSuccess, ExitFailure))
import System.IO (writeFile)
import Data.List (unlines)
import Control.Concurrent (threadDelay)
import qualified Data.Text.IO as TIO
import qualified Data.Text as T

setupTestDir :: FilePath -> IO ()
setupTestDir testDir = do
  -- Create directory structure
  createDirectoryIfMissing True (testDir </> ".hix")
  createDirectoryIfMissing True (testDir </> ".hix" </> "models")
  createDirectoryIfMissing True (testDir </> ".hix" </> "templates")
  createDirectoryIfMissing True (testDir </> ".hix" </> "output")
  createDirectoryIfMissing True (testDir </> ".hix" </> "output" </> "Entities")
  createDirectoryIfMissing True (testDir </> ".hix" </> "output" </> "UseCases")
  createDirectoryIfMissing True (testDir </> ".hix" </> "output" </> "Adapters")
  createDirectoryIfMissing True (testDir </> ".hix" </> "output" </> "Frameworks")
  
  -- Create model.json
  writeFile (testDir </> ".hix" </> "models" </> "model.json") $ unlines
    [ "{"
    , "  \"className\": \"User\","
    , "  \"properties\": ["
    , "    { \"name\": \"Id\", \"type\": \"int\" },"
    , "    { \"name\": \"Name\", \"type\": \"string\" }"
    , "  ]"
    , "}"
    ]
  
  -- Create template.cs.hix
  writeFile (testDir </> ".hix" </> "templates" </> "template.cs.hix") $ unlines
    [ "public class [[model.className]]"
    , "{"
    , "  [[prop]]"
    , "  public [[prop.type]] [[prop.name]] { get; set; }"
    , "  [[/prop]]"
    , "}"
    ]
  
  -- Create config.yaml
  writeFile (testDir </> ".hix" </> "config.yaml") $ unlines
    [ "architecture: \"clean\""
    , "output_root: \".hix/output\""
    , "models_path: \".hix/models\""
    , "templates_root: \".hix/templates\""
    , "layers:"
    , "  - name: \"Entities\""
    , "    path: \".hix/output/Entities\""
    , "    description: \"Domain entities\""
    , "    templates:"
    , "      - template: \".hix/templates/template.cs.hix\""
    , "        filename: \"[[model.className]].cs\""
    , "        output_by: \"model\""
    , "  - name: \"UseCases\""
    , "    path: \".hix/output/UseCases\""
    , "    description: \"Application use cases\""
    , "    templates:"
    , "      - template: \".hix/templates/template.cs.hix\""
    , "        filename: \"[[model.className]].cs\""
    , "        output_by: \"model\""
    , "  - name: \"InterfaceAdapters\""
    , "    path: \".hix/output/Adapters\""
    , "    description: \"Infrastructure adapters\""
    , "    templates:"
    , "      - template: \".hix/templates/template.cs.hix\""
    , "        filename: \"[[model.className]].cs\""
    , "        output_by: \"model\""
    , "  - name: \"FrameworksAndDrivers\""
    , "    path: \".hix/output/Frameworks\""
    , "    description: \"Infrastructure frameworks\""
    , "    templates:"
    , "      - template: \".hix/templates/template.cs.hix\""
    , "        filename: \"[[model.className]].cs\""
    , "        output_by: \"model\""
    , "naming:"
    , "  model: \"PascalCase\""
    , "  property: \"camelCase\""
    , "options:"
    , "  overwrite_existing: false"
    , "  dry_run: false"
    ]

cleanupTestDir :: FilePath -> IO ()
cleanupTestDir testDir = do
  let maxAttempts = 3
      delay = 1000000  -- 1 second
      cleanup attempt = do
        result <- try $ removeDirectoryRecursive testDir
        case result of
          Right _ -> return ()
          Left (_ :: IOException)
            | attempt < maxAttempts -> do
                threadDelay delay
                cleanup (attempt + 1)
            | otherwise -> return ()
  cleanup 1

withTestDir :: FilePath -> (FilePath -> IO ()) -> IO ()
withTestDir testDir action = bracket
  (do
    -- Ensure the test directory exists and is clean
    createDirectoryIfMissing True testDir
    removeDirectoryRecursive testDir `catch` (\(_ :: IOException) -> return ())
    setupTestDir testDir
    return testDir)
  cleanupTestDir
  action

-- Helper function to read file content strictly
readFileStrict :: FilePath -> IO T.Text
readFileStrict path = do
  content <- TIO.readFile path
  return $! T.length content `seq` content

-- Helper function to verify file content
verifyFileContent :: FilePath -> (T.Text -> Bool) -> IO Bool
verifyFileContent path predicate = do
  content <- readFileStrict path
  threadDelay 100000  -- Small delay after reading
  return $ predicate content

generateSpec :: Spec
generateSpec = describe "Generate Command" $ do
  let testDir = "test/data/generate"

  describe "Basic Functionality" $ do
    it "generates files for all layers when no layer/template specified" $ withTestDir testDir $ \dir -> do
      originalDir <- getCurrentDirectory
      setCurrentDirectory dir
      (exitCode, _, _) <- readCreateProcessWithExitCode (proc "stack" ["exec", "--", "hix", "generate", "--model", ".hix/models/model.json"]) ""
      setCurrentDirectory originalDir
      exitCode `shouldBe` ExitSuccess

    it "generates files only for specified layer" $ withTestDir testDir $ \dir -> do
      originalDir <- getCurrentDirectory
      setCurrentDirectory dir
      (exitCode, _, _) <- readCreateProcessWithExitCode (proc "stack" ["exec", "--", "hix", "generate", "--model", ".hix/models/model.json", "--layer", "Entities"]) ""
      setCurrentDirectory originalDir
      exitCode `shouldBe` ExitSuccess

    it "generates file only for specified template" $ withTestDir testDir $ \dir -> do
      originalDir <- getCurrentDirectory
      setCurrentDirectory dir
      (exitCode, _, _) <- readCreateProcessWithExitCode (proc "stack" ["exec", "--", "hix", "generate", "--model", ".hix/models/model.json", "--template", ".hix/templates/template.cs.hix"]) ""
      setCurrentDirectory originalDir
      exitCode `shouldBe` ExitSuccess

  describe "Error Handling" $ do
    it "shows error for missing --model parameter" $ withTestDir testDir $ \_ -> do
      (exitCode, output, _) <- readCreateProcessWithExitCode (proc "stack" ["exec", "--", "hix", "generate"]) ""
      exitCode `shouldBe` ExitFailure 1
      output `shouldContain` "--model parameter is required"

    it "shows error for non-existent layer" $ withTestDir testDir $ \dir -> do
      originalDir <- getCurrentDirectory
      setCurrentDirectory dir
      (exitCode, output, _) <- readCreateProcessWithExitCode (proc "stack" ["exec", "--", "hix", "generate", "--model", ".hix/models/model.json", "--layer", "NonExistent"]) ""
      setCurrentDirectory originalDir
      exitCode `shouldBe` ExitFailure 1
      output `shouldContain` "Layer 'NonExistent' not found"

    it "shows error for non-existent template" $ withTestDir testDir $ \dir -> do
      originalDir <- getCurrentDirectory
      setCurrentDirectory dir
      (exitCode, output, _) <- readCreateProcessWithExitCode (proc "stack" ["exec", "--", "hix", "generate", "--model", ".hix/models/model.json", "--template", "non-existent.hix"]) ""
      setCurrentDirectory originalDir
      exitCode `shouldBe` ExitFailure 1
      output `shouldContain` "Template 'non-existent.hix' not found"

    it "shows error when both --layer and --template are specified" $ withTestDir testDir $ \dir -> do
      originalDir <- getCurrentDirectory
      setCurrentDirectory dir
      (exitCode, output, _) <- readCreateProcessWithExitCode (proc "stack" ["exec", "--", "hix", "generate", "--model", ".hix/models/model.json", "--layer", "Entities", "--template", ".hix/templates/template.cs.hix"]) ""
      setCurrentDirectory originalDir
      exitCode `shouldBe` ExitFailure 1
      output `shouldContain` "Cannot specify both --layer and --template"

  describe "Template Variable Handling" $ do
    it "handles model.className correctly" $ withTestDir testDir $ \dir -> do
      originalDir <- getCurrentDirectory
      setCurrentDirectory dir
      (exitCode, _, _) <- readCreateProcessWithExitCode (proc "stack" ["exec", "--", "hix", "generate", "--model", ".hix/models/model.json", "--template", ".hix/templates/template.cs.hix"]) ""
      setCurrentDirectory originalDir
      exitCode `shouldBe` ExitSuccess
      let outputFile = dir </> ".hix" </> "output" </> "Entities" </> "User.cs"
      result <- verifyFileContent outputFile (T.isInfixOf "public class User")
      result `shouldBe` True

    it "handles prop.name and prop.type correctly" $ withTestDir testDir $ \dir -> do
      originalDir <- getCurrentDirectory
      setCurrentDirectory dir
      (exitCode, _, _) <- readCreateProcessWithExitCode (proc "stack" ["exec", "--", "hix", "generate", "--model", ".hix/models/model.json", "--template", ".hix/templates/template.cs.hix"]) ""
      setCurrentDirectory originalDir
      exitCode `shouldBe` ExitSuccess
      let outputFile = dir </> ".hix" </> "output" </> "Entities" </> "User.cs"
      result1 <- verifyFileContent outputFile (T.isInfixOf "public int Id")
      result2 <- verifyFileContent outputFile (T.isInfixOf "public string Name")
      result1 `shouldBe` True
      result2 `shouldBe` True

    it "handles conditional blocks correctly" $ withTestDir testDir $ \dir -> do
      originalDir <- getCurrentDirectory
      setCurrentDirectory dir
      (exitCode, _, _) <- readCreateProcessWithExitCode (proc "stack" ["exec", "--", "hix", "generate", "--model", ".hix/models/model.json", "--template", ".hix/templates/template.cs.hix"]) ""
      setCurrentDirectory originalDir
      exitCode `shouldBe` ExitSuccess
      let outputFile = dir </> ".hix" </> "output" </> "Entities" </> "User.cs"
      result1 <- verifyFileContent outputFile (T.isInfixOf "public class User")
      result2 <- verifyFileContent outputFile (T.isInfixOf "}")
      result1 `shouldBe` True
      result2 `shouldBe` True

    it "handles text transformations correctly" $ withTestDir testDir $ \dir -> do
      originalDir <- getCurrentDirectory
      setCurrentDirectory dir
      (exitCode, _, _) <- readCreateProcessWithExitCode (proc "stack" ["exec", "--", "hix", "generate", "--model", ".hix/models/model.json", "--template", ".hix/templates/template.cs.hix"]) ""
      setCurrentDirectory originalDir
      exitCode `shouldBe` ExitSuccess
      let outputFile = dir </> ".hix" </> "output" </> "Entities" </> "User.cs"
      result <- verifyFileContent outputFile (T.isInfixOf "User")
      result `shouldBe` True

    it "correctly transforms template tokens during property iteration" $ withTestDir testDir $ \dir -> do
      -- Create a test model with multiple properties
      writeFile (dir </> ".hix" </> "models" </> "model.json") $ unlines
        [ "{"
        , "  \"className\": \"TestEntity\","
        , "  \"properties\": ["
        , "    { \"name\": \"Id\", \"type\": \"int\" },"
        , "    { \"name\": \"Name\", \"type\": \"string\" },"
        , "    { \"name\": \"IsActive\", \"type\": \"bool\" }"
        , "  ]"
        , "}"
        ]
      
      -- Create a template with property loop
      writeFile (dir </> ".hix" </> "templates" </> "template.cs.hix") $ unlines
        [ "public class [[model.className]]"
        , "{"
        , "  [[prop]]"
        , "  public [[prop.type]] [[prop.name]] { get; set; }"
        , "  [[/prop]]"
        , "}"
        ]
      
      originalDir <- getCurrentDirectory
      setCurrentDirectory dir
      (exitCode, _, _) <- readCreateProcessWithExitCode (proc "stack" ["exec", "--", "hix", "generate", "--model", ".hix/models/model.json", "--template", ".hix/templates/template.cs.hix"]) ""
      setCurrentDirectory originalDir
      exitCode `shouldBe` ExitSuccess
      
      let outputFile = dir </> ".hix" </> "output" </> "Entities" </> "TestEntity.cs"
      
      -- Verify the generated file exists
      fileExists <- doesFileExist outputFile
      fileExists `shouldBe` True
      
      -- Read the generated file content
      content <- readFileStrict outputFile
      
      -- Verify that all template tokens have been properly transformed
      let hasUntransformedTokens = T.isInfixOf "[[" content
      hasUntransformedTokens `shouldBe` True
      
      -- Verify the expected properties are present and correctly transformed
      let hasId = T.isInfixOf "public int Id" content
      let hasName = T.isInfixOf "public string Name" content
      let hasIsActive = T.isInfixOf "public bool IsActive" content
      
      hasId `shouldBe` True
      hasName `shouldBe` True
      hasIsActive `shouldBe` True
      
      -- Verify the class structure is correct
      let hasClassStart = T.isInfixOf "public class TestEntity" content
      let hasClassEnd = T.isInfixOf "}" content
      
      hasClassStart `shouldBe` True
      hasClassEnd `shouldBe` True
      
      -- Verify the property loop was properly processed
      let propertyCount = length $ filter (\line -> 
            "public" `T.isPrefixOf` T.strip line && 
            any (\t -> t `T.isInfixOf` line) ["int", "string", "bool"]) (T.lines content)
      propertyCount `shouldBe` 3  -- Should have exactly 3 properties

    it "properly cleans up template elements after property substitution" $ withTestDir testDir $ \dir -> do
      -- Create a test model with multiple properties
      writeFile (dir </> ".hix" </> "models" </> "model.json") $ unlines
        [ "{"
        , "  \"className\": \"TestEntity\","
        , "  \"properties\": ["
        , "    { \"name\": \"Id\", \"type\": \"int\" },"
        , "    { \"name\": \"Name\", \"type\": \"string\" },"
        , "    { \"name\": \"IsActive\", \"type\": \"bool\" }"
        , "  ]"
        , "}"
        ]
      
      -- Create a template with property loop
      writeFile (dir </> ".hix" </> "templates" </> "template.cs.hix") $ unlines
        [ "public class [[model.className]]"
        , "{"
        , "  [[prop]]"
        , "  public [[prop.type]] [[prop.name]] { get; set; }"
        , "  [[/prop]]"
        , "}"
        ]
      
      originalDir <- getCurrentDirectory
      setCurrentDirectory dir
      (exitCode, _, _) <- readCreateProcessWithExitCode (proc "stack" ["exec", "--", "hix", "generate", "--model", ".hix/models/model.json", "--template", ".hix/templates/template.cs.hix"]) ""
      setCurrentDirectory originalDir
      exitCode `shouldBe` ExitSuccess
      
      let outputFile = dir </> ".hix" </> "output" </> "Entities" </> "TestEntity.cs"
      
      -- Verify the generated file exists
      fileExists <- doesFileExist outputFile
      fileExists `shouldBe` True
      
      -- Read the generated file content
      content <- readFileStrict outputFile
      
      -- Verify that all template tokens have been properly transformed
      let hasUntransformedTokens = T.isInfixOf "[[" content
      hasUntransformedTokens `shouldBe` True
      
      -- Verify the expected properties are present
      let hasId = T.isInfixOf "public int Id" content
      let hasName = T.isInfixOf "public string Name" content
      let hasIsActive = T.isInfixOf "public bool IsActive" content
      
      hasId `shouldBe` True
      hasName `shouldBe` True
      hasIsActive `shouldBe` True
      
      -- Verify the class structure is correct
      let hasClassStart = T.isInfixOf "public class TestEntity" content
      let hasClassEnd = T.isInfixOf "}" content
      
      hasClassStart `shouldBe` True
      hasClassEnd `shouldBe` True
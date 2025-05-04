{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module WizardSpec (wizardSpec) where

import Test.Hspec
import System.Process (createProcess, proc, StdStream(..), CreateProcess(..), waitForProcess)
import System.Directory (createDirectoryIfMissing, removeDirectoryRecursive, getCurrentDirectory, setCurrentDirectory, removeFile, doesFileExist)
import System.FilePath ((</>))
import Control.Exception (bracket, catch, try, IOException)
import Data.List (isInfixOf)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Aeson as A
import Data.Aeson ((.=), object, Value)
import System.Exit (ExitCode(ExitSuccess, ExitFailure))
import System.IO (writeFile, hPutStrLn, Handle, hClose, hGetContents, putStrLn)
import Data.List (unlines)
import Control.Concurrent (threadDelay)
import qualified Data.Text.IO as TIO
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Yaml as Y
import Data.HashMap.Strict (HashMap, keys)

wizardSpec :: Spec
wizardSpec = describe "Wizard Configuration" $ do
  it "generates valid YAML configuration" $ do
    testDir <- getCurrentDirectory
    let tempDir = testDir </> "temp_wizard_test"
        hixPath = testDir </> ".stack-work" </> "install" </> "72d79149" </> "bin" </> "hix.exe"
    
    bracket
      (do
        createDirectoryIfMissing True tempDir
        setCurrentDirectory tempDir
        return tempDir)
      (\_ -> do
        setCurrentDirectory testDir
        removeDirectoryRecursive tempDir)
      (\_ -> do
        let processConfig = (proc hixPath ["init"])
              { std_in = CreatePipe
              , std_out = CreatePipe
              , std_err = CreatePipe
              }
        (Just stdin, Just stdout, Just stderr, processHandle) <- createProcess processConfig
        
        -- Send input to select architecture type (2 for clean architecture)
        hPutStrLn stdin "2"
        hPutStrLn stdin "n"  -- No to configuring templates
        hClose stdin
        
        -- Wait for the process to complete
        exitCode <- waitForProcess processHandle
        exitCode `shouldBe` ExitSuccess
        
        configPath <- getCurrentDirectory >>= \dir -> return (dir </> ".hix" </> "config.yaml")
        configExists <- doesFileExist configPath
        configExists `shouldBe` True
        
        configContent <- TIO.readFile configPath
        case Y.decodeEither' (TE.encodeUtf8 configContent) :: Either Y.ParseException Value of
          Left err -> expectationFailure $ "Failed to parse YAML: " ++ show err
          Right _ -> return ()
        
        -- Check for invalid characters in keys (not values)
        let hasInvalidKeys = any (\line -> 
              let trimmed = T.strip line
              in not (T.null trimmed) && T.head trimmed /= '#' && -- Skip comments and empty lines
                 not ("template:" `T.isInfixOf` line) && -- Skip template values
                 not ("filename:" `T.isInfixOf` line) && -- Skip filename values
                 not ("output_by:" `T.isInfixOf` line) && -- Skip output_by values
                 not ("path:" `T.isInfixOf` line) && -- Skip path values
                 ("[" `T.isInfixOf` line || "]" `T.isInfixOf` line)) (T.lines configContent)
        hasInvalidKeys `shouldBe` False
        
        let lines = T.lines configContent
        let indentationLevels = map (T.length . T.takeWhile (== ' ')) lines
        let isConsistent = all (\level -> level `mod` 2 == 0) indentationLevels
        isConsistent `shouldBe` True)

  it "generates configuration with required fields" $ do
    testDir <- getCurrentDirectory
    let tempDir = testDir </> "temp_wizard_test_fields"
        hixPath = testDir </> ".stack-work" </> "install" </> "72d79149" </> "bin" </> "hix.exe"
    
    bracket
      (do
        createDirectoryIfMissing True tempDir
        setCurrentDirectory tempDir
        return tempDir)
      (\_ -> do
        setCurrentDirectory testDir
        removeDirectoryRecursive tempDir)
      (\_ -> do
        let processConfig = (proc hixPath ["init"])
              { std_in = CreatePipe
              , std_out = CreatePipe
              , std_err = CreatePipe
              }
        (Just stdin, Just stdout, Just stderr, processHandle) <- createProcess processConfig
        
        -- Send input to select architecture type (2 for clean architecture)
        hPutStrLn stdin "2"
        hPutStrLn stdin "n"  -- No to configuring templates
        hClose stdin
        
        -- Wait for the process to complete
        exitCode <- waitForProcess processHandle
        exitCode `shouldBe` ExitSuccess
        
        configPath <- getCurrentDirectory >>= \dir -> return (dir </> ".hix" </> "config.yaml")
        configContent <- TIO.readFile configPath
        
        case Y.decodeEither' (TE.encodeUtf8 configContent) :: Either Y.ParseException (HashMap T.Text Value) of
          Left err -> expectationFailure $ "Failed to parse YAML: " ++ show err
          Right config -> do
            let hasArchitecture = "architecture" `elem` keys config
            let hasLayers = "layers" `elem` keys config
            let hasOutputRoot = "output_root" `elem` keys config
            
            hasArchitecture `shouldBe` True
            hasLayers `shouldBe` True
            hasOutputRoot `shouldBe` True) 
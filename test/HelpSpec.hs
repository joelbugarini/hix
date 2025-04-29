{-# LANGUAGE OverloadedStrings #-}

module HelpSpec where

import Test.Hspec
import Test.Hspec.Golden
import System.IO.Silently (capture)
import Help (helpMessage, manualMessage, version)
import System.Directory (createDirectoryIfMissing)
import qualified Data.Text.IO as TIO
import qualified Data.Text as T
import Data.Char (isAscii)

-- | Normalize line endings to LF and remove box-drawing characters
normalizeText :: String -> String
normalizeText = T.unpack . T.stripEnd . T.filter isAscii . T.pack

helpSpec :: Spec
helpSpec = describe "Help functionality" $ do
  it "should produce correct help message" $ do
    let goldenFile = "test/data/help.golden"
    createDirectoryIfMissing True "test/data"
    (output, _) <- capture $ putStrLn helpMessage
    expected <- readFile goldenFile
    normalizeText output `shouldBe` normalizeText expected

  it "should produce correct manual message" $ do
    let goldenFile = "test/data/man.golden"
    createDirectoryIfMissing True "test/data"
    (output, _) <- capture $ putStrLn manualMessage
    expected <- readFile goldenFile
    normalizeText output `shouldBe` normalizeText expected

  it "should produce correct version message" $ do
    let goldenFile = "test/data/version.golden"
    createDirectoryIfMissing True "test/data"
    (output, _) <- capture $ putStrLn version
    expected <- readFile goldenFile
    normalizeText output `shouldBe` normalizeText expected 
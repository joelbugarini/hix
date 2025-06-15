{-# LANGUAGE OverloadedStrings #-}

module Template.Renderer (renderAST, warnOnUnhandledTokens) where

import Data.Text (Text)
import Template.AST (AST)
import Model.Model (Model)
import Template.RenderNode (renderNode)
import qualified Data.Text as T
import System.IO (hPutStrLn, stderr)
import Data.List (nub)
import Control.Monad.Writer

-- Entry point: render list of AST nodes with full model
renderAST :: [AST] -> Model -> Writer [Text] Text
renderAST asts model = do
  rendered <- mapM (renderNode model) asts
  return $ removeUntransformedTokens (mconcat rendered)

-- Remove any remaining [[...]] tokens from the output
removeUntransformedTokens :: T.Text -> T.Text
removeUntransformedTokens = T.pack . removeTokens . T.unpack
  where
    removeTokens [] = []
    removeTokens ('[':'[':xs) = skipToken xs
    removeTokens (x:xs) = x : removeTokens xs
    skipToken (']':']':ys) = removeTokens ys
    skipToken (_:ys) = skipToken ys
    skipToken [] = []

-- Scan for unhandled [[...]] tokens and print a warning if any are found
warnOnUnhandledTokens :: T.Text -> IO ()
warnOnUnhandledTokens txt =
  let tokens = findTokens (T.unpack txt)
  in if null tokens
       then return ()
       else hPutStrLn stderr $ "[hix warning] Unhandled template tokens found and removed: " ++ show (nub tokens)
  where
    findTokens [] = []
    findTokens ('[':'[':xs) = let (tok, rest) = spanNotEnd xs in tok : findTokens rest
    findTokens (_:xs) = findTokens xs
    spanNotEnd xs = go "" xs
      where
        go acc (']':']':ys) = (reverse acc, ys)
        go acc (y:ys) = go (y:acc) ys
        go acc [] = (reverse acc, [])

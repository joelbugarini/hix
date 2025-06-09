{-# LANGUAGE OverloadedStrings #-}

import Model.Model (Model(..))
import qualified Template.Parser as Parser
import qualified Template.Renderer as Renderer
import qualified Data.Text as T
import System.Exit (exitFailure)

main :: IO ()
main = do
  let model = Model "TestModule" []
      templateStr = "[[module_transform kebab_case model.className]]"
  case Parser.parseTemplate (T.pack templateStr) of
    Left err -> putStrLn ("Parse error: " ++ err) >> exitFailure
    Right ast -> do
      let output = Renderer.renderAST ast model
      putStrLn $ "Rendered output: " ++ T.unpack output 
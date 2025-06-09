{-# LANGUAGE OverloadedStrings #-}

import Model.Model (Model(..))
import qualified Template.Parser as Parser
import qualified Template.Renderer as Renderer
import qualified Data.Text as T
import System.Exit (exitFailure)

main :: IO ()
main = do
  let model = Model "TestModule" []
      filenameTemplate = "[[module_transform kebab_case model.className]].cs"
  case Parser.parseTemplate (T.pack filenameTemplate) of
    Left err -> putStrLn ("Parse error: " ++ err) >> exitFailure
    Right ast -> do
      let outputFilename = Renderer.renderAST ast model
      putStrLn $ "Rendered filename: " ++ T.unpack outputFilename 
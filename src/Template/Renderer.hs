{-# LANGUAGE OverloadedStrings #-}

module Template.Renderer (renderAST) where

import Data.Text (Text)
import Template.AST (AST)
import Model.Model (Model)
import Template.RenderNode (renderNode)

-- Entry point: render list of AST nodes with full model
renderAST :: [AST] -> Model -> Text
renderAST asts model = mconcat $ map (renderNode model) asts

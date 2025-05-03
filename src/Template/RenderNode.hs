module Template.RenderNode (renderNode) where

import Data.Text (Text)
import qualified Data.Text as T
import Template.AST (AST(..))
import Model.Model (Model(..), Property(..), PropertyType(..))
import Template.RenderProp (renderPropBlock, propertyTypeToText)

renderNode :: Model -> AST -> Text
renderNode _ (Literal t) = t
renderNode model (ModelValue t) | t == T.pack "model.className" = className model
renderNode model (FuncCall fn arg)
  | T.pack "model." `T.isPrefixOf` arg = T.pack ("--[[Unsupported func arg: " ++ T.unpack arg ++ "]]" )
  | otherwise = T.pack ("--[[Unsupported func arg: " ++ T.unpack arg ++ "]]" )
renderNode _ (ModelValue key) = T.pack ("--[[Unknown model key: " ++ T.unpack key ++ "]]" )
renderNode _ (UnknownTag t) = T.pack ("--[[Unknown tag: " ++ T.unpack t ++ "]]" )
renderNode model (PropLoop mFilter body) =
  let propsToRender = case mFilter of
        Just (k, t) | k == T.pack "type"   -> filter (\p -> T.strip (propertyTypeToText (propType p)) == T.strip t) (properties model)
        Just (k, n) | k == T.pack "ignore" -> filter (\p -> T.strip (propName p) /= T.strip n) (properties model)
        _ -> properties model
  in if null propsToRender 
     then T.empty
     else T.concat $ map (\p -> renderPropBlock p body) propsToRender

getModelValue :: Model -> Text -> Text
getModelValue model t | t == T.pack "model.className" = className model
getModelValue _ key = T.pack ("--[[Unknown model key: " ++ T.unpack key ++ "]]" )

 
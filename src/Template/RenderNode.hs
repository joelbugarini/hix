module Template.RenderNode (renderNode) where

import Data.Text (Text)
import qualified Data.Text as T
import Template.AST (AST(..))
import Model.Model (Model(..), Property(..), PropertyType(..))
import Template.RenderProp (renderPropBlock, propertyTypeToText, applyFunc)

renderNode :: Model -> AST -> Text
renderNode _ (Literal t) = t
renderNode model (ModelValue t) | t == T.pack "model.className" = className model
renderNode model (FuncCall fn arg)
  | arg == T.pack "model.className" = applyFunc fn (className model)
  | T.pack "model." `T.isPrefixOf` arg = T.pack ("--[[Unknown func arg: " ++ T.unpack arg ++ "]]" )
  | otherwise = T.pack ("--[[Unsupported func arg: " ++ T.unpack arg ++ "]]" )
renderNode _ (ModelValue _) = T.empty
renderNode _ (UnknownTag t) = T.pack ("--[[Unknown tag: " ++ T.unpack t ++ "]]" )
renderNode model (PropLoop mFilter body) =
  let propsToRender = case mFilter of
        Just (k, t) | k == T.pack "type"   -> filter (\p -> T.strip (propertyTypeToText (propType p)) == T.strip t) (properties model)
        Just (k, n) | k == T.pack "ignore" -> filter (\p -> T.strip (propName p) /= T.strip n) (properties model)
        _ -> properties model
  in if null propsToRender 
     then T.empty
     else T.concat $ map (\p -> renderPropBlock renderNode model p body) propsToRender

getModelValue :: Model -> Text -> Text
getModelValue model t | t == T.pack "model.className" = className model
getModelValue _ key = T.pack ("--[[Unknown model key: " ++ T.unpack key ++ "]]" )

 
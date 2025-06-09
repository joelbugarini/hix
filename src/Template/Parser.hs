{-# LANGUAGE OverloadedStrings #-}

module Template.Parser (
    parseTemplate,
    Parser
) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
import Text.Megaparsec (Parsec, parse, errorBundlePretty, manyTill, (<|>), try, lookAhead, eof, some, choice, failure, label, optional, notFollowedBy, (<?>))
import qualified Text.Megaparsec as MP
import Text.Megaparsec.Char (char, string)
import Text.Megaparsec (anySingle)
import Control.Monad (void)
import Data.List (break)

import Template.AST (AST(..))

-- | Type alias for our parser
--   We parse Text, no custom state, no custom error type
--   (can be changed later if needed)
type Parser = Parsec Void Text

-- | Entry point for parsing a template into an AST
parseTemplate :: Text -> Either String [AST]
parseTemplate input =
    if hasUnclosedTag input
      then Left "Parse error: unclosed tag, expected closing ']]'"
      else case MP.parse (astsParser <* eof) "template" input of
        Left err -> Left (errorBundlePretty err)
        Right asts -> Right asts

-- | Detects if there is an unclosed tag (i.e., a '[[' with no matching ']]')
hasUnclosedTag :: Text -> Bool
hasUnclosedTag t =
  let opens = T.count "[[" t
      closes = T.count "]]" t
  in opens > closes

-- | Parser for a list of AST nodes (literals and simple tags)
astsParser :: Parser [AST]
astsParser = MP.many (try propBlockParser <|> try ifBlockParser <|> try tagParser <|> literalParser)

-- | Parser for literal text (until next tag or end of input)
literalParser :: Parser AST
literalParser = do
    -- Fail if we see an unclosed tag at the end of input
    notFollowedBy (string "[[") <?> "literal text or tag"
    txt <- MP.someTill anySingle (lookAhead (void (string "[[")) <|> eof)
    return $ Literal (T.pack txt)

-- | Parser for simple variable tags and function calls
-- Fails if closing ']]' is not found
-- Does NOT match block tags (prop, if, else)
tagParser :: Parser AST
tagParser = label "tag" $ MP.withRecovery recoverUnclosedTag $ do
    _ <- string "[["
    content <- MP.someTill anySingle (string "]]") <?> "closing ']]' for tag"
    let trimmed = T.strip (T.pack content)
    if trimmed == "prop" || T.isPrefixOf "if " trimmed || trimmed == "else"
      then fail $ "Unexpected block tag '" ++ T.unpack trimmed ++ "' outside of block context"
      else case parseFuncCall trimmed of
        Just (fn, arg) -> return $ FuncCall fn arg
        Nothing -> case trimmed of
          "model.className" -> return $ ModelValue "model.className"
          "prop.name"       -> return $ ModelValue "prop.name"
          "prop.type"       -> return $ ModelValue "prop.type"
          _                 -> return $ UnknownTag trimmed
  where
    recoverUnclosedTag err = fail "unclosed tag, expected closing ']]'"

-- | Helper: parse function call pattern, e.g. "upper model.className"
parseFuncCall :: T.Text -> Maybe (T.Text, T.Text)
parseFuncCall txt =
  let ws = T.words txt
  in case ws of
    (fn:arg:[]) -> Just (fn, arg)
    (fn:style:arg:[]) | fn == "module_transform" -> Just (fn, T.unwords [style, arg])
    (fn:style:arg:rest) | fn == "module_transform" -> Just (fn, T.unwords (style:arg:rest))
    _           -> Nothing

-- | Parser for [[prop]] ... [[/prop]] blocks
propBlockParser :: Parser AST
propBlockParser = label "prop block" $ do
    _ <- string "[[prop]]"
    inner <- astsParserTill "[[/prop]]"
    end <- optional (string "[[/prop]]")
    case end of
      Just _  -> return $ PropLoop Nothing inner
      Nothing -> fail "Unclosed [[prop]] block: expected [[/prop]]"

-- | Parser for [[if key=value]] ... [[/if]] and [[if key=value]] ... [[else]] ... [[/if]] blocks
ifBlockParser :: Parser AST
ifBlockParser = label "if block" $ do
    _ <- string "[[if "
    cond <- MP.someTill anySingle (string "]]")
    let (k, v) = breakKV (T.pack cond)
    trueBranch <- astsParserTillEither ["[[else]]", "[[/if]]"]
    next <- optional (lookAhead (string "[[else]]" <|> string "[[/if]]"))
    case next of
      Just "[[else]]" -> do
        _ <- string "[[else]]"
        fb <- astsParserTill "[[/if]]"
        end <- optional (string "[[/if]]")
        case end of
          Just _  -> return $ IfBlock (k, v) trueBranch (Just fb)
          Nothing -> fail "Unclosed [[if ...]] block: expected [[/if]] after [[else]]"
      Just "[[/if]]" -> do
        _ <- string "[[/if]]"
        return $ IfBlock (k, v) trueBranch Nothing
      _ -> fail "Malformed [[if ...]] block: expected [[else]] or [[/if]]"

-- | Helper: parse until a given end tag
astsParserTill :: String -> Parser [AST]
astsParserTill endTag = MP.manyTill (try propBlockParser <|> try ifBlockParser <|> try tagParser <|> literalParser) (lookAhead (string (T.pack endTag)))

-- | Helper: parse until any of the given end tags, return which one was found
astsParserTillEither :: [String] -> Parser [AST]
astsParserTillEither endTags = MP.manyTill (try propBlockParser <|> try ifBlockParser <|> try tagParser <|> literalParser) (lookAhead (choice (map (string . T.pack) endTags)))

breakKV :: T.Text -> (T.Text, T.Text)
breakKV txt = case T.splitOn "=" txt of
  [k, v] -> (T.strip k, T.strip v)
  _      -> ("", "") 
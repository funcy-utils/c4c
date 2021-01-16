{-# LANGUAGE OverloadedStrings #-}

module C4C.Substitute(substitute, parseConfigLines) where

import           Data.Either          (fromRight)
import           Data.Maybe           (catMaybes)
import           Data.Text            (Text)
import qualified Data.Text            as T
import           Text.Megaparsec
import           Text.Megaparsec.Char

import           C4C.Entries
import           C4C.Error
import           C4C.Utils

substitute :: Entries -> Text -> Either Error Text
substitute entries = fromRight (Left ErrorParser) . parse (combinedP entries) ""

combinedP :: Entries -> Parser (Either Error Text)
combinedP entries = tokenizers
  where
    tokenizers = fmap (fmap T.concat . sequenceA) $ many $ choice $ fmap (\f -> try (f entries)) [inputP, oneCharP]

inputP :: Entries -> Parser (Either Error Text)
inputP entries = do
  char '#'
  char '{'
  k <- fmap T.pack $ many $ anySingleBut '}'
  char '}'
  case lookupEntry k entries of
    Nothing -> return $ Left $ ErrorEntryNotDefined k
    Just v  -> return $ Right v

oneCharP :: Entries -> Parser (Either Error Text)
oneCharP _ = do
  c <- anySingle
  return $ Right $ T.singleton c

parseConfigLines :: [Text] -> Either Error Entries
parseConfigLines = fmap (mkEntries . catMaybes) . traverse parseConfigLine

parseConfigLine :: Text -> Either Error (Maybe Entry)
parseConfigLine inp = case T.words inp of
  []          -> Right Nothing -- Handle empty lines
  -- (('#':_):_) -> Right Nothing -- Handle comments starting with '#'
  [x]         -> Left (ErrorIncorrectConfigEntry x)    -- Fails when line contain only one word (eg. "FOO")
  (x:xs)      -> Right (Just (Entry (x, T.unwords xs)))  -- Handle valid entry (eg. "FOO bar")

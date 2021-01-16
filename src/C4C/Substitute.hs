module C4C.Substitute(substitute, parseConfigLines) where

import           Data.Either          (fromRight)
import           Data.Maybe           (catMaybes)
import           Text.Megaparsec
import           Text.Megaparsec.Char

import           C4C.Entries
import           C4C.Error
import           C4C.Utils

substitute :: Entries -> String -> Either Error String
substitute entries = fromRight (Left ErrorParser) . parse (combinedP entries) ""

combinedP :: Entries -> Parser (Either Error String)
combinedP entries = tokenizers
  where
    tokenizers = fmap (fmap concat . sequenceA) $ many $ choice $ fmap (\f -> try (f entries)) [inputP, oneCharP]

inputP :: Entries -> Parser (Either Error String)
inputP entries = do
  char '#'
  char '{'
  k <- many $ anySingleBut '}'
  char '}'
  case lookupEntry k entries of
    Nothing -> return $ Left $ ErrorEntryNotDefined k
    Just v  -> return $ Right v

oneCharP :: Entries -> Parser (Either Error String)
oneCharP _ = do
  c <- anySingle
  return $ Right [c]

parseConfigLines :: [String] -> Either Error Entries
parseConfigLines = fmap (mkEntries . catMaybes) . traverse parseConfigLine

parseConfigLine :: String -> Either Error (Maybe Entry)
parseConfigLine inp = case words inp of
  []          -> Right Nothing -- Handle empty lines
  (('#':_):_) -> Right Nothing -- Handle comments starting with '#'
  [x]         -> Left (ErrorIncorrectConfigEntry x)    -- Fails when line contain only one word (eg. "FOO")
  (x:xs)      -> Right (Just (Entry (x, unwords xs)))  -- Handle valid entry (eg. "FOO bar")

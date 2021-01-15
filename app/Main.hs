{-# LANGUAGE LambdaCase #-}
module Main where

import Data.Maybe ( catMaybes )
import Text.Megaparsec
import Data.Void (Void)
import qualified Data.Map as M
import Text.Megaparsec.Char
import Data.Either (fromRight)
import System.Environment

type Parser a = Parsec Void String a

data Error 
  = ErrorIncorrectConfigEntry String
  | ErrorEntryNotDefined String
  | ErrorParser
  deriving Show

newtype Entry = Entry {unEntry :: (String, String)}
  deriving Show

newtype Entries = Entries {unEntries :: M.Map String String}

mkEntries :: [Entry] -> Entries
mkEntries = Entries . M.fromList . fmap unEntry

lookupEntry :: String -> Entries -> Maybe String
lookupEntry k = M.lookup k . unEntries

main :: IO ()
main = getArgs >>= run

run :: [FilePath] -> IO ()
run args
  | [confFP, inpFP] <- args
  = do
    config <- readFile confFP
    input  <- readFile inpFP
    let newFP = removeExt inpFP
    case replaceContent config input of
      Left e  -> print e
      Right x -> writeFile newFP x
run _ = do
  putStr "Usage: "
  getProgName >>= putStr
  putStrLn " <config.conf> <filename.ext.c4c>"

replaceContent :: String -> String -> Either Error String
replaceContent config input = do
  entries <- parseConfigLines $ lines config
  substitute entries input

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

parseConfigFile :: FilePath -> IO (Either Error Entries)
parseConfigFile fp = parseConfigLines . lines <$> readFile fp

parseConfigLines :: [String] -> Either Error Entries
parseConfigLines = fmap (mkEntries . catMaybes) . traverse parseConfigLine

parseConfigLine :: String -> Either Error (Maybe Entry)
parseConfigLine inp = case words inp of
  []          -> Right Nothing -- Handle empty lines
  (('#':_):_) -> Right Nothing -- Handle comments starting with '#'
  [x]         -> Left (ErrorIncorrectConfigEntry x)    -- Fails when line contain only one word (eg. "FOO")
  (x:xs)      -> Right (Just (Entry (x, unwords xs)))  -- Handle valid entry (eg. "FOO bar")

removeExt :: String -> String
removeExt = concat . joinWith "." .  init . wordsWhen (=='.') 

joinWith :: a -> [a] -> [a]
joinWith = go
  where
    go _ []     = []
    go _ [x]    = [x]
    go s (x:xs) = x : s : go s xs

wordsWhen     :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
  "" -> []
  s' -> w : wordsWhen p s''
    where (w, s'') = break p s'

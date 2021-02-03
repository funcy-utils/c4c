{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}

module C4C.Entries(Entry(..), Entries(..), mkEntries, lookupEntry, parseEntries, applySelf) where

import qualified Data.Map as M
import Data.Text
import C4C.Error
import Data.Maybe
import qualified Data.Text as T

newtype Entry = Entry {unEntry :: (Text, Text)}
  
newtype Entries = Entries {unEntries :: M.Map Text Text}
  deriving (Eq, Show)

mkEntries :: [Entry] -> Entries
mkEntries = Entries . M.fromList . fmap unEntry

entriesToLst :: Entries -> [(Text, Text)]
entriesToLst = M.toList . unEntries

lookupEntry :: Text -> Entries -> Either Error Text
lookupEntry k = maybe (Left (ErrorEntryNotDefined k)) Right . M.lookup k . unEntries

parseEntry :: Text -> Either Error (Maybe Entry)
parseEntry inp = case T.words inp of
  []          -> Right Nothing -- Handle empty lines
--   (('#':_):_) -> Right Nothing -- Handle comments starting with '#'
  [x]         -> Left (ErrorIncorrectConfigEntry x)    -- Fails when line contain only one word (eg. "FOO")
  (x:xs)      -> Right (Just (Entry (x, T.unwords xs)))  -- Handle valid entry (eg. "FOO bar")

parseEntries :: Text -> Either Error Entries
parseEntries s = applyLoop . mkEntries . catMaybes =<< traverse parseEntry (T.lines s)

isVar :: Text -> Bool
isVar s = T.isPrefixOf "#{" s && T.isSuffixOf "}" s

getVar :: Text -> Text
getVar = T.drop 2 . T.reverse . T.drop 1 . T.reverse

applySelf :: Entries -> Either Error Entries
applySelf envs = fmap (mkEntries . fmap Entry) $ traverse (\v -> if isVar $ snd v then (fst v,) <$> lookupEntry (getVar $ snd v) envs else Right v) $ entriesToLst envs

applyLoop :: Entries -> Either Error Entries
applyLoop envs = do
  envs' <- applySelf envs
  if envs == envs' 
    then return envs'
    else applyLoop envs'

module C4C.Entries(Entry(..), Entries(..), mkEntries, lookupEntry, parseEntries) where

import qualified Data.Map as M
import Data.Text
import C4C.Error
import Data.Maybe
import qualified Data.Text as T

newtype Entry = Entry {unEntry :: (Text, Text)}

newtype Entries = Entries {unEntries :: M.Map Text Text}

mkEntries :: [Entry] -> Entries
mkEntries = Entries . M.fromList . fmap unEntry

lookupEntry :: Text -> Entries -> Either Error Text
lookupEntry k = maybe (Left (ErrorEntryNotDefined k)) Right . M.lookup k . unEntries

parseEntry :: Text -> Either Error (Maybe Entry)
parseEntry inp = case T.words inp of
  []          -> Right Nothing -- Handle empty lines
--   (('#':_):_) -> Right Nothing -- Handle comments starting with '#'
  [x]         -> Left (ErrorIncorrectConfigEntry x)    -- Fails when line contain only one word (eg. "FOO")
  (x:xs)      -> Right (Just (Entry (x, T.unwords xs)))  -- Handle valid entry (eg. "FOO bar")

parseEntries :: Text -> Either Error Entries
parseEntries = fmap (mkEntries . catMaybes) . traverse parseEntry . T.lines
module C4C.Entries(Entry(..), Entries(..), mkEntries, lookupEntry) where

import qualified Data.Map as M
import Data.Text

newtype Entry = Entry {unEntry :: (Text, Text)}

newtype Entries = Entries {unEntries :: M.Map Text Text}

mkEntries :: [Entry] -> Entries
mkEntries = Entries . M.fromList . fmap unEntry

lookupEntry :: Text -> Entries -> Maybe Text
lookupEntry k = M.lookup k . unEntries

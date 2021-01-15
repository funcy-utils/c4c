module C4C.Entries where

import qualified Data.Map as M

newtype Entry = Entry {unEntry :: (String, String)}

newtype Entries = Entries {unEntries :: M.Map String String}

mkEntries :: [Entry] -> Entries
mkEntries = Entries . M.fromList . fmap unEntry

lookupEntry :: String -> Entries -> Maybe String
lookupEntry k = M.lookup k . unEntries

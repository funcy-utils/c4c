module C4C.Utils(Parser, removeExt) where

import           Data.Text       (Text)
import           Data.Void       (Void)
import           Text.Megaparsec (Parsec)

type Parser a = Parsec Void Text a

removeExt :: String -> String
removeExt inp = reverse $ drop len $ reverse inp
  where
    len = (+1) $ length $ takeWhile ('.' /=) $ reverse inp

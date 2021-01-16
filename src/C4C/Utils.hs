module C4C.Utils where

import           Data.Void       (Void)
import           Text.Megaparsec (Parsec)

type Parser a = Parsec Void String a

removeExt :: String -> String
removeExt inp = reverse $ drop len $ reverse inp
  where
    len = (+1) $ length $ takeWhile ('.' /=) $ reverse inp

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

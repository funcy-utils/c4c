{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-deferred-type-errors #-}

module C4C.Input where

import           Data.List            (groupBy)
import           Data.Maybe           (fromJust)
import           Data.Text            (Text)
import qualified Data.Text            as T
import           Text.Megaparsec
import           Text.Megaparsec.Char

import           C4C.Utils

newtype Input = Input {unInput :: [InputPart]}
  deriving Show

data InputPart =
    InputText Text
  | InputVar  Text
  deriving Show


instance Semigroup InputPart where
  (InputText v1) <> (InputText v2) = InputText (v1<>v2)
  (InputText _)  <> (InputVar v)   = InputVar v
  (InputVar v)   <> (InputText _)  = InputVar v
  (InputVar _)   <> (InputVar _)   = undefined

instance Monoid InputPart where
  mempty = InputText ""

isText :: InputPart -> InputPart -> Bool
isText (InputText _) (InputText _) = True
isText _ _                         = False

parseInput :: Text -> Input
parseInput = Input . map mconcat . groupBy isText . fromJust . parseMaybe inpP

inpP :: Parser [InputPart]
inpP = many $ choice [try varP, try textP]

varP :: Parser InputPart
varP = string "#{" *> (InputVar . T.pack <$> many (anySingleBut '}')) <* char '}'

textP :: Parser InputPart
textP = InputText . T.singleton <$> anySingle

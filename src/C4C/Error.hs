module C4C.Error(Error(..)) where

import Data.Text

data Error
  = ErrorIncorrectConfigEntry Text
  | ErrorEntryNotDefined Text
  | ErrorParser
  deriving Show

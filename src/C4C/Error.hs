module C4C.Error(Error(..)) where

data Error
  = ErrorIncorrectConfigEntry String
  | ErrorEntryNotDefined String
  | ErrorParser
  deriving Show

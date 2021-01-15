module C4C.Error where

data Error
  = ErrorIncorrectConfigEntry String
  | ErrorEntryNotDefined String
  | ErrorParser
  deriving Show

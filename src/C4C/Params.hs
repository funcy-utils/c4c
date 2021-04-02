{-# LANGUAGE DeriveDataTypeable #-}

module C4C.Params (Parameters(..), getParams) where

import           System.Console.CmdArgs

data Parameters = Parameters
  { pOutputDir       :: FilePath
  , pRemoveExtension :: Bool
  , pConfig          :: FilePath
  , pFiles           :: [FilePath]
  }
  deriving (Show, Data, Typeable)

params :: Parameters
params =
  Parameters
    { pOutputDir       = "."  &= name "o" &= name "output" &= explicit
    , pRemoveExtension = True &= name "remove-ext" &= explicit
    , pConfig = def &= name "config" &= explicit
    , pFiles = [] &= args &= typ "FILES"
    }
    &= summary "c4c by t4ccer"
    &= program "c4c"
    &= helpArg [explicit, name "help", name "h", help "Take a guess"]
    &= versionArg [ignore]

getParams :: IO Parameters
getParams = cmdArgs params


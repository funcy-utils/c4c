module C4C(run) where

import           System.Environment

import           C4C.Error
import           C4C.Substitute
import           C4C.Utils

run :: [FilePath] -> IO ()
run args
  | [confFP, inpFP] <- args
  = do
    config <- readFile confFP
    input  <- readFile inpFP
    let newFP = removeExt inpFP
    case replaceContent config input of
      Left e  -> print e
      Right x -> writeFile newFP x
run _ = do
  putStr "Usage: "
  getProgName >>= putStr
  putStrLn " <config.conf> <filename.ext.c4c>"

replaceContent :: String -> String -> Either Error String
replaceContent config input = do
  entries <- parseConfigLines $ lines config
  substitute entries input

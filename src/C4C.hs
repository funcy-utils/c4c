module C4C(run) where

import           System.Environment
import qualified Data.Text.IO.Utf8 as UTF8
import qualified Data.Text as T
import Data.Text (Text)

import           C4C.Error
import           C4C.Substitute
import           C4C.Utils

run :: [FilePath] -> IO ()
run args
  | [confFP, inpFP] <- args
  = do
    config <- UTF8.readFile confFP
    input  <- UTF8.readFile inpFP
    let newFP = removeExt inpFP
    case replaceContent config input of
      Left e  -> print e
      Right x -> UTF8.writeFile newFP x
run _ = do
  putStr "Usage: "
  getProgName >>= putStr
  putStrLn " <config.conf> <filename.ext.c4c>"

replaceContent :: Text -> Text -> Either Error Text
replaceContent config input = do
  entries <- parseConfigLines $ T.lines config
  substitute entries input

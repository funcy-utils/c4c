{-# LANGUAGE OverloadedStrings #-}

module C4C(run) where

import           System.Environment
import           System.Exit
import qualified Data.Text.IO.Utf8 as UTF8
import qualified Data.Text as T
import Data.Text (Text)

import           C4C.Error
import           C4C.Substitute
import           C4C.Utils
import Control.Monad (when)
import Data.List (isPrefixOf)

run :: [FilePath] -> IO ()
run args
  | (confFP:fs) <- args
  = do
    config <- UTF8.readFile confFP
    let fpaths = filter (not . isPrefixOf "-") fs
    let printPath = "--print-paths" `elem` fs
    mapM_ (runOne config printPath) fpaths
    when printPath (putStrLn "")
run _ = do
  putStr "Usage: "
  getProgName >>= putStr
  putStrLn " <config.conf> [OPTIONS] <filename.ext.c4c>"
  putStrLn "OPTIONS: "
  putStrLn "\t--print-paths"

runOne :: Text -> Bool -> FilePath -> IO ()
runOne config printPath fpath = do
  input  <- UTF8.readFile fpath
  let newFP = removeExt fpath
  case replaceContent config input of
      Left e  -> do
        print e
        exitFailure 
      Right x -> do
        UTF8.writeFile newFP x
        when printPath (putStr (newFP <> " "))

replaceContent :: Text -> Text -> Either Error Text
replaceContent config input = do
  entries <- parseConfigLines $ T.lines config
  substitute entries input

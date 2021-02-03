{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module C4C(run) where

import           Control.Monad      (when)
import           Data.List          (isPrefixOf)
import           Data.Text          (Text)
import qualified Data.Text          as T
import qualified Data.Text.IO.Utf8  as UTF8
import           System.Environment
import           System.Exit

import           C4C.Entries
import           C4C.Error
import           C4C.Input
import           C4C.Utils

run :: [FilePath] -> IO ()
run args
  | (confFP:fs) <- args
  = do
    config <- parseEntries <$> UTF8.readFile confFP
    case config of
      Left e -> print e
      Right config' -> do
        let fpaths = filter (not . isPrefixOf "-") fs
            printPath = "--print-paths" `elem` fs
        mapM_ (runOne config' printPath) fpaths
        when printPath (putStrLn "")
run _ = do
  putStr "Usage: "
  getProgName >>= putStr
  putStrLn " <config.conf> [OPTIONS] <filename.ext.c4c>"
  putStrLn "OPTIONS: "
  putStrLn "\t--print-paths"

runOne :: Entries -> Bool -> FilePath -> IO ()
runOne config printPath fpath = do
  input <- parseInput <$> UTF8.readFile fpath
  let newFP = removeExt fpath
  case replaceContent config input of
      Left e  -> do
        putStrLn ""
        print e
        exitFailure
      Right x -> do
        UTF8.writeFile newFP x
        when printPath (putStr (newFP <> " "))

replaceContent :: Entries -> Input -> Either Error Text
replaceContent config Input{..} = T.concat <$> traverse (replaceOne config) unInput

replaceOne :: Entries -> InputPart -> Either Error Text
replaceOne _ (InputText v)   = Right v
replaceOne vars (InputVar v) = lookupEntry v vars

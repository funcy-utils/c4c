{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module C4C(run) where

import qualified Data.Map          as M
import           Data.Text         (Text)
import qualified Data.Text         as T
import qualified Data.Text.IO.Utf8 as UTF8
import           System.Exit
import           Text.Printf

import           C4C.Entries
import           C4C.Error
import           C4C.Input
import           C4C.Params
import           C4C.Utils
import           Control.Monad

run :: Parameters -> IO ()
run Parameters{..} = do
    config <- parseEntries <$> UTF8.readFile pConfig
    case config of
      Left e -> print e
      Right config' -> do
        when pTestConfig $ forM_ (M.toList $ unEntries config') $ \(label, val) -> do
          putStrLn $ printf "%s: %s" label val
        forM_ pFiles $ \f -> do
          input <- parseInput <$> UTF8.readFile f
          let newFP = pOutputDir <> "/" <> if pRemoveExtension then removeExt f else f
          case replaceContent config' input of
              Left e  -> do
                putStrLn ""
                print e
                exitFailure
              Right x -> do
                UTF8.writeFile newFP x

replaceContent :: Entries -> Input -> Either Error Text
replaceContent config Input{..} = T.concat <$> traverse (replaceOne config) unInput

replaceOne :: Entries -> InputPart -> Either Error Text
replaceOne _ (InputText v)   = Right v
replaceOne vars (InputVar v) = lookupEntry v vars

module Main where

import           System.Environment

import           C4C

main :: IO ()
main = getArgs >>= run

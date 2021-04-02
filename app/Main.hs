module Main where
import           C4C
import           C4C.Params

main :: IO ()
main = getParams >>= run

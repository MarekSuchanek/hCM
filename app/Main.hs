module Main where

import System.Environment
import CM.Visualization

main :: IO ()
main = do
  args <- getArgs
  filecontent <- readFile . head $ args
  putStrLn . modelToDot $ filecontent

module Main where

import System.Environment

import qualified Example
import CM.Visualization
import CM.Metamodel

main :: IO ()
main = do
  --args <- getArgs
  --filecontent <- readFile . head $ args
  --putStrLn . modelToDot $ filecontent
  --putStrLn . elementToDotModel $ model
  putStrLn . modelToDotInstance $ Example.model

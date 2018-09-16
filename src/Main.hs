module Main where

import           Lexer
import           Parser
import           Grammar
import           System.IO

outputFile :: String
outputFile = "output.txt"

inputFile :: String
inputFile = "code.cpp"

main :: IO()
main = do
  s <- readFile inputFile
  -- putStrLn s
  let tokens = alexScanTokens s
  print tokens
  let syntax = parseCode tokens
  print syntax

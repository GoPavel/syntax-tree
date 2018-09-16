module Main where


import           Data.List
import           Lexer
import           Parser
import           Grammar
import           System.IO
import           CallGraph (syntaxToGraph, getRecursive)
import qualified Data.Map as Map

outputFile :: String
outputFile = "output.txt"

inputFile :: String
inputFile = "code.cpp"

fromRight :: Either String b -> b
fromRight (Left str) = error str
fromRight (Right b) = b

main :: IO()
main = do
  s <- readFile inputFile
  -- putStrLn s
  let tokens = alexScanTokens s
  print "\ntokens: "
  print tokens
  print "\nsyntax tree: "
  let syntax = fromRight $ parseCode tokens
  print syntax
  let isRec = getRecursive $ syntaxToGraph syntax
  putStrLn $ intercalate "\n"
           $ map (\(k, v) -> k ++ if v then " -> yes" else " -> no")
           $ Map.toList isRec

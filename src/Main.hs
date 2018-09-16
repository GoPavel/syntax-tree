module Main where


import           CallGraph (getRecursive, syntaxToGraph)
import           Data.List
import qualified Data.Map  as Map
import           Grammar
import           Lexer
import           Parser
import           System.IO

outputFile :: String
outputFile = "output.txt"

inputFile :: String
inputFile = "code.cpp"

fromRight :: Either String b -> b
fromRight (Left str) = error str
fromRight (Right b)  = b

main :: IO()
main = do
  putStrLn ">> Enter your code (example: code.cpp): <<"
  input <- getLine
  let fileName = head $ words input
  s <- readFile fileName
  let tokens = alexScanTokens s
  let syntax = fromRight $ parseCode tokens
  let isRec = getRecursive $ syntaxToGraph syntax
  putStrLn "Functions causing a recursive function call: "
  putStrLn $ intercalate "\n"
           $ map fst
           $ filter snd
           $ Map.toList isRec

module CallGraph where

import qualified Data.Map as Map
import qualified Data.Set as Set
import           Grammar

data Graph = Graph {
  nodes :: Set.Set String,
  edges :: Map.Map String [String]
} deriving (Eq, Show)

syntaxToGraph :: Code -> Graph
syntaxToGraph (Code fs) = Graph{nodes = getNodes, edges = getEdges} where
  getNodes = Set.fromList $ map (\(DefFunc name _ _ _) -> name) fs

  getEdges = Map.fromList $ map getPair fs

  getPair (DefFunc name _ _ sts) = (name, concatMap extractFromStatement sts)

  extractFromStatement (CalcExpr e) = extractFromExpr e
  extractFromStatement (Def defvar) = extractFromDefVar defvar
  extractFromStatement (Ret e) = extractFromExpr e
  extractFromStatement (Mov _ e) = extractFromExpr e
  extractFromStatement (Branch cond sts1 sts2) = extractFromCond cond ++
                                                 concatMap extractFromStatement sts1 ++
                                                 concatMap extractFromStatement sts2

  extractFromDefVar (DefVarAndInit _ _ e) = extractFromExpr e
  extractFromDefVar _                     = []

  extractFromCond (Cond _ e1 e2) = extractFromExpr e1 ++ extractFromExpr e2

  extractFromExpr (Call name exprs) = name : concatMap extractFromExpr exprs
  extractFromExpr (Binary _ e1 e2)  = extractFromExpr e1 ++ extractFromExpr e2
  extractFromExpr  _                = []

getRecursive :: Graph -> Map.Map String Bool
getRecursive Graph{edges = g, nodes = xs} = startDfs xs where
  startDfs = foldr (\x used -> fst $ dfs x used Set.empty) Map.empty

  dfs :: String -> Map.Map String Bool -> Set.Set String -> (Map.Map String Bool, Bool)
  dfs v used stack
    | v `Map.member` used = (used, used Map.! v)
    | v `Set.member` stack = (Map.insert v True used, True)
    | otherwise =
         let tr c (a, b) = (a, b || c)
             (newUsed, flag) = foldr (\x (used, b) -> b `tr` dfs x used (Set.insert v stack)) (used, False) (g Map.! v)
         in  (Map.insert v flag newUsed, flag)

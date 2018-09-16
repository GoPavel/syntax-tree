module Grammar where

data Binop =  Mul
            | Sub
            | Add
            | Div
            deriving (Eq, Show)

data Expr = Val String
          | Var String
          | Binary Binop Expr Expr
          | Call String [Expr]
          deriving (Eq, Show)

data Condop = L
            -- | LE
            | G
            -- | GE
            | E
            -- | NE
            deriving (Eq, Show)

data Cond = Cond Condop Expr Expr
  deriving (Eq, Show)

data Type = IntType
          | VoidType
          deriving (Eq, Show)

data DefVar = DefVar Type String
            | DefVarAndInit Type String Expr
  deriving (Eq, Show)

data Statement = CalcExpr Expr
               | Def DefVar
               | Ret Expr
               | Mov String Expr
               | Branch Cond [Statement] [Statement]
               deriving (Eq, Show)

data DefFunc = DefFunc String Type [DefVar] [Statement]
  deriving (Eq, Show)

data Code = Code [DefFunc]
  deriving (Eq, Show)

{
module Parser where

import Grammar
import Lexer

}

%name parseCode
%tokentype { Token }
%error  { parseError }
%monad  { Either String }{ >>= }{ return }

%token PLUS { PlusT }                   {- "+" -}
%token MINUS { MinusT }                 {- "-" -}
%token DIV { DivT }                     {- "/" -}
%token MUL { MulT }                     {- "*" -}
%token ASSIGN { AssignT }               {- "=" -}
%token GREATER { GreaterT }             {- ">" -}
%token LESS { LessT }                   {- "<" -}
%token EQ { EqualT }                    {- "==" -}
%token NAME { NameT $$ }
%token VALUE { ValueT $$ }
%token TYPEINT { TypeIntT }             {- "int" -}
%token TYPEVOID { TypeVoidT }           {- "void" -}
%token SEMICOLON { SemiColonT }         {- ";" -}
%token COMMA { CommaT }                 {- "," -}
%token IF { IfT }                       {- "if" -}
%token ELSE { ElseT }                   {- "else" -}
%token LEFTBRACKET { LBracketT }        {- "(" -}
%token RIGHTBRACKET { RBracketT }       {- ")" -}
%token BEGIN { BeginT }                 {- "{" -}
%token END { EndT }                     {- "}" -}
%token RETURN { ReturnT }               {- "return" -}

%nonassoc LESS GREATER EQ IF ELSE
%left COMMA SEMICOLON
%left PLUS MINUS
%left MUL DIV

%%

Start
  : Code {Code $1}

Code
  : DefFunc Code { $1 : $2 }
  | DefFunc { [$1] }

DefFunc
  : Type NAME LEFTBRACKET ArgsInDef RIGHTBRACKET BEGIN SeqStatement END { DefFunc $2 $1 $4 $7 }

ArgsInDef
  : DefVar COMMA ArgsInDef { $1 : $3 }
  | DefVar { [$1] }
  | {- empty -} { [] }

Statement
  : Expr { CalcExpr $1 }
  | DefVar { Def $1 }
  | RETURN Expr { Ret $2 }
  | NAME ASSIGN Expr { Mov $1 $3 }
  | IF LEFTBRACKET Condition RIGHTBRACKET BEGIN SeqStatement END ELSE BEGIN SeqStatement END { Branch $3 $6 $10}

DefVar
  : Type NAME { DefVar $1 $2 }
  | Type NAME ASSIGN Expr { DefVarAndInit $1 $2 $4}

SeqStatement
  : Statement SEMICOLON SeqStatement { $1 : $3 }
  | Statement { [$1] }
  | {- empty -} { [] }

Condition
  : Expr EQ Expr { Cond E $1 $3 }
  | Expr LESS Expr { Cond L $1 $3 }
  | Expr GREATER Expr { Cond G $1 $3 }

Type
  : TYPEINT { IntType }
  | TYPEVOID { VoidType }

Expr
  : NAME LEFTBRACKET ArgsInCall RIGHTBRACKET { Call $1 $3 }
  | LEFTBRACKET Expr RIGHTBRACKET { $2 }
  | Expr PLUS Expr { Binary Add $1 $3 }
  | Expr MINUS Expr { Binary Sub $1 $3 }
  | Expr MUL Expr { Binary Mul $1 $3 }
  | Expr DIV Expr { Binary Div $1 $3 }
  | NAME { Var $1 }
  | VALUE { Val $1 }

ArgsInCall
  : {- empty -}    { [] }
  | Expr            { [$1] }
  | Expr COMMA ArgsInCall { $1 : $3 }

{
parseError = fail "Parse error"
}

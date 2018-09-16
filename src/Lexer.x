{
module Lexer where
}

%wrapper "basic"

$digit = 0-9
$alpha = [A-Z a-z]

tokens :-

  $white+                    ;
  "//".*                     ;
  "int"                      { \_ -> TypeIntT}
  "void"                     { \_ -> TypeVoidT}
  ";"                        { \_ -> SemiColonT}
  ","                        { \_ -> CommaT}
  "if"                       { \_ -> IfT}
  "else"                     { \_ -> ElseT}
  "return"                   { \_ -> ReturnT}
  "("                        { \_ -> LBracketT}
  ")"                        { \_ -> RBracketT}
  "{"                        { \_ -> BeginT}
  "}"                        { \_ -> EndT}
  "="                        { \_ -> AssignT}
  "*"                        { \_ -> MulT}
  "\\"                       { \_ -> DivT}
  "+"                        { \_ -> PlusT}
  "-"                        { \_ -> MinusT}
  ">"                        { \_ -> GreaterT}
  "<"                        { \_ -> LessT}
  "=="                        { \_ -> EqualT}
  $alpha [$alpha $digit]*    { \s -> NameT s}
  [$digit]+                  { \s -> ValueT s}
{

data Token = PlusT
           | MinusT
           | DivT
           | MulT
           | AssignT
           | GreaterT
           | LessT
           | EqualT
           | NameT String
           | ValueT String
           | TypeIntT
           | TypeVoidT
           | SemiColonT
           | CommaT
           | IfT
           | ElseT
           | LBracketT
           | RBracketT
           | BeginT
           | EndT
           | ReturnT
           deriving (Show, Eq)

}

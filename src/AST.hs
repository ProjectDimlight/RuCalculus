module AST where

type Variable = String

data Value 
  = ValInt Integer
  | ValNum Float
  deriving (Show, Eq, Ord)

data Expr
  = ExprVar Variable
  | ExprLambda Variable Expr
  | ExprApply Expr Expr
  | ExprValue Value
  | ExprHostFunc ([Expr] -> Expr)


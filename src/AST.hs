module AST (Variable, Value(..), Expr(..)) where

type Variable = String

data Value 
  = ValInt Int
  | ValNum Float
  deriving (Show, Eq, Ord)

data Expr
  = ExprVar Variable
  | ExprLambda Variable Expr
  | ExprApply Expr Expr
  | ExprValue Value
  | ExprHostFunc ([Expr] -> Expr)




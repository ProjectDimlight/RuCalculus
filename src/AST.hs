module AST where
import Text.Show.Unicode

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
  | ExprHostFunc String (Value -> IO Expr)

instance Show Expr where
    show (ExprValue val) = "(" ++ ushow val ++ ")"
    show (ExprVar var) = ushow var
    show (ExprLambda var exp) = "lambda " ++  ushow var ++ " . " ++ ushow exp
    show (ExprApply exp1 exp2) = "(" ++ ushow exp1 ++ ") " ++ ushow exp2
    show (ExprHostFunc name _) = "<host-func: " ++ name ++ ">"

module AST where
import Text.Show.Unicode

type Variable = String

data ValueType
  = ValTypeInt
  | ValTypeNum
  deriving Eq

instance Show ValueType where
  show ValTypeInt = "Int"
  show ValTypeNum = "Num"

data Type
  = TypeVal ValueType
  | TypeFunc Type Type
  | TypeFuncDelay Variable Expr
  | TypeFuncDelayWithResultType Variable Expr Type

instance Show Type where
  show (TypeVal v) = show v
  show (TypeFunc a b) = "(" ++ show a ++ " -> " ++ show b ++ ")"
  show (TypeFuncDelay _ _) = "(? -> ?)"
  show (TypeFuncDelayWithResultType _ _ t) = "(? -> " ++ show t ++ ")"

data Value
  = ValInt Integer
  | ValNum Float
  deriving (Show, Eq, Ord)

data Expr
  = ExprVar Variable
  | ExprLambda Variable Expr
  | ExprApply Expr Expr
  | ExprValue Value
  | ExprHostFunc String Type (Expr -> IO (Either String Expr))

instance Show Expr where
    show (ExprValue val) = "(" ++ ushow val ++ ")"
    show (ExprVar var) = ushow var
    show (ExprLambda var exp) = "lambda " ++  ushow var ++ " . " ++ ushow exp
    show (ExprApply exp1 exp2) = "(" ++ ushow exp1 ++ ") " ++ ushow exp2
    show (ExprHostFunc name t _) = "<host-func: " ++ show t ++ ">"

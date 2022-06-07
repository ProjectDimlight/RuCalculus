module AST where
import Text.Show.Unicode

type Variable = String

data ValueType
  = ValTypeInt
  | ValTypeNum
  | ValTypeUnit
  deriving Eq

instance Show ValueType where
  show ValTypeInt = "整"
  show ValTypeNum = "实"
  show ValTypeUnit = "元"

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
  | ValNum Double
  | ValUnit
  | ValPair Value Value
  deriving (Show, Eq, Ord)

type IsLazy = Bool

data Expr
  = ExprVar Variable
  | ExprLambda Variable Expr
  | ExprApply Expr Expr
  | ExprValue Value
  | ExprHostFunc String Type IsLazy (Expr -> IO (Either String Expr))

instance Show Expr where
    show (ExprValue val) = "(" ++ ushow val ++ ")"
    show (ExprVar var) = ushow var
    show (ExprLambda var exp) = "(lambda " ++  ushow var ++ " . " ++ ushow exp ++ ")"
    show (ExprApply exp1 exp2) = "(apply " ++ ushow exp1 ++ " " ++ ushow exp2 ++ ")"
    show (ExprHostFunc name t _ _) = "<" ++ name ++ ": " ++ show t ++ ">"

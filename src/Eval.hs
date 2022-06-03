module Eval where
import AST

data EvalError
  = Atom
  | UnboundedVariable String
  | CannotApply Expr Expr
  deriving Show

apply' :: Variable -> Expr -> Expr -> Expr
apply' var val (ExprVar v)
  | v == var = val
  | otherwise = ExprVar v
apply' var val (ExprLambda v e)
  | var == v = ExprLambda v e
  | otherwise = ExprLambda v $ apply' var val e
apply' var val (ExprApply a b) = ExprApply (apply' var val a) (apply' var val b)
apply' _ _ (ExprValue v) = ExprValue v
apply' _ _ (ExprHostFunc name t f) = ExprHostFunc name t f

apply :: Variable -> Value -> Expr -> Expr
apply var val = apply' var $ ExprValue val

injectHostFunctions :: [(String, Type, Value -> IO Expr)] -> Expr -> Expr
injectHostFunctions ls e = foldl (\e (name, t, f) -> apply' name (ExprHostFunc name t f) e) e ls

step :: Expr -> Either EvalError (IO Expr)
step (ExprVar v) = Left $ UnboundedVariable v
step (ExprLambda s v) = Left Atom
step (ExprApply (ExprLambda v lambda) (ExprValue val)) = Right $ pure $ apply v val lambda
step (ExprApply (ExprHostFunc name _ f) (ExprValue val)) = Right $ f val
step (ExprApply left right) =
  case step left of
    Right expr -> Right $ expr >>= \e -> pure $ ExprApply e right
    Left Atom ->
      case step right of
        Right expr -> Right $ expr >>= \e -> pure $ ExprApply left e
        Left Atom -> Left $ CannotApply left right
        Left e -> Left e
    Left err -> Left err
step (ExprValue _) = Left Atom
step (ExprHostFunc {}) = Left Atom

runStepByStep :: Expr -> IO ()
runStepByStep expr = do
  print expr
  case step expr of
    Right expr -> getChar >> expr >>= runStepByStep
    Left Atom -> pure ()
    Left err -> putStrLn "= Error =" >> print err

run :: Expr -> IO ()
run expr =
  case step expr of
    Right expr -> expr >>= run
    Left Atom -> print expr
    Left err -> putStrLn "= Error =" >> print err


test :: IO ()
test =
  runStepByStep $
    ExprApply (ExprLambda "a" $ ExprLambda "b" $ ExprVar "a") $ ExprValue $ ValInt 1

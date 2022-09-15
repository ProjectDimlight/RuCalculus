{-# LANGUAGE LambdaCase #-}
module Eval where
import AST
import MonadicParse
import HostFuncs
import Text.Show.Unicode
import Text.Parsec.Error

injectHostFunctions :: [(String, Type, IsLazy, Expr -> IO (Either String Expr))] -> Expr -> Expr
injectHostFunctions ls e = foldl (\e (name, t, l, f) -> apply' name (ExprHostFunc name t l f) e) e ls

data EvalError
  = Atom
  | UnboundedVariable String
  | CannotApply Expr Expr
  | HostFuncError String
  | UnresolvedInclude
  | IncludeError Text.Parsec.Error.ParseError
  deriving Show

removeHeadTail :: String -> String
removeHeadTail str = filter (/='】')  (filter (/='【') str)

mapLeft :: (t -> u) -> Either t b -> Either u b
mapLeft f (Left x) = Left $ f x
mapLeft _ (Right x) = Right x

apply' :: Variable -> Expr -> Expr -> Expr
apply' var val (ExprVar v)
  | v == var = val
  | otherwise = ExprVar v
apply' var val (ExprLambda v e)
  | var == v = ExprLambda v e
  | otherwise = ExprLambda v $ apply' var val e
apply' var val (ExprApply a b) = ExprApply (apply' var val a) (apply' var val b)
apply' _ _ (ExprValue v) = ExprValue v
apply' _ _ (ExprHostFunc name t isLazy f) = ExprHostFunc name t isLazy f
apply' _ _ (ExprInclude v) = ExprInclude v

step :: Expr -> IO (Either EvalError Expr)
step (ExprInclude var) = do
  lib <- parseRu (removeHeadTail var)
  case lib of
    Left err -> pure $ Left $ IncludeError err
    Right exp -> pure $ Right $ exp
step (ExprVar v) = pure $ Left $ UnboundedVariable v
step (ExprLambda s v) = pure $ Left Atom
step (ExprApply (ExprLambda v (ExprInclude fname)) arg) = do
  lib <- step (ExprInclude fname)
  case lib of 
    Right lib' -> pure $ Right $ apply' v arg (injectHostFunctions hostFuncs lib')
    Left e -> pure $ Left e
step (ExprApply (ExprLambda v lambda) arg) = do
  arg' <- step arg
  case arg' of 
    Left Atom -> pure $ Right $ apply' v arg lambda
    Right arg'' -> pure $ Right $ ExprApply (ExprLambda v lambda) arg''
    Left e -> pure $ Left e
step (ExprApply (ExprHostFunc name t isLazy f) arg) = do
  let execute a = mapLeft HostFuncError <$> f a
  if isLazy then execute arg
  else do
    step arg >>= \case
      Left Atom -> execute arg
      Right arg' -> pure $ Right $ ExprApply (ExprHostFunc name t isLazy f) arg'
      Left err -> pure $ Left err
step (ExprApply left right) = do
  left' <- step left
  case left' of
    Right expr -> pure $ Right $ ExprApply expr right
    Left Atom -> pure $ Left $ CannotApply left right
    Left err -> pure $ Left err
step (ExprValue _) = pure $ Left Atom
step (ExprHostFunc {}) = pure $ Left Atom

reduce :: Expr -> IO (Either EvalError Expr)
reduce x = step x >>= \case
  Left Atom -> pure $ Right x
  Left e -> pure $ Left e
  Right x' -> reduce x'

runStepByStep :: Expr -> IO ()
runStepByStep expr = do
  print expr
  step' <- step expr
  case step' of
    Right expr -> getChar >> runStepByStep expr 
    Left Atom -> pure ()
    Left err -> putStrLn "= Error =" >> uprint err

run :: Bool -> Expr -> IO ()
run showResult expr = reduce expr >>= \case
  Right (ExprValue ValUnit) -> pure ()
  Right expr -> if showResult then print expr else pure ()
  Left err -> putStrLn "= Error =" >> uprint err


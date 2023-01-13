
module Interp where
import AST
import MonadicParse

import Data.Map as M
import Control.Monad.Except
import Control.Exception
import qualified Text.Parsec as Text.Parsec.Error
import HostFuncs

data Atom
  = AtomValue Value
  | AtomClos InterpEnv Variable Expr
  | AtomHost InterpEnv String Type IsLazy (Expr -> IO (Either String Expr))

instance Show Atom where
  show (AtomValue val) = show val
  show (AtomClos _ _ _) = "<closure>"
  show (AtomHost name _ _ _ _) = show name

type InterpEnv = M.Map Variable Atom

data InterpError
  = UnboundedVariable Variable
  | CannotApply Expr Expr
  | HostFuncError String
  | UnresolvedInclude
  | IncludeError Text.Parsec.Error.ParseError
  | NotAnAtom Expr
  deriving (Show)

removeHeadTail :: String -> String
removeHeadTail = Prelude.filter (/='】') .
                 Prelude.filter (/='」') .
                 Prelude.filter (/='【') .
                 Prelude.filter (/='「')

interp :: InterpEnv -> Expr -> ExceptT InterpError IO Atom
interp env (ExprValue val) =
  return $ AtomValue val
interp env (ExprVar x) = 
  case M.lookup x env of
    Just atom -> return atom
    Nothing -> throwError (UnboundedVariable x)
interp env (ExprLambda x e) = 
  return $ AtomClos env x e
interp env (ExprApply e1 e2) = do
  e1' <- interp env e1
  case e1' of
    (AtomClos env' arg body) -> do
      e2' <- interp env e2
      interp (M.insert arg e2' env') body
    (AtomHost env' name typ isLazy f) -> do
      e2' <- if isLazy
        then return e2 
        else do
          e2' <- interp env e2
          case e2' of
            AtomValue val -> return $ ExprValue val
            AtomClos env var expr -> return $ ExprLambda var expr
            _ -> throwError (NotAnAtom e2)
      res <- lift $ f e2'
      case res of
        Left err -> throwError (HostFuncError err)
        Right exp -> interp env exp
    _ -> throwError (CannotApply e1 e2)
interp env (ExprHostFunc name t isLazy f) =
  return $ AtomHost env name t isLazy f
interp env (ExprInclude var expr) = do
  lib <- lift $ parseRu (removeHeadTail var) expr
  case lib of
    Left err -> throwError $ IncludeError err
    Right exp -> interp env $ injectHostFunctions hostFuncs exp

runInterp :: Expr -> IO (Either InterpError Atom)
runInterp expr = runExceptT $ interp M.empty expr

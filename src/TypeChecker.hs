module TypeChecker where
import AST

type Env = [(String, Type)]

data TypeError
  = TypeErrorUnboundedVariable Variable
  | TypeErrorNotFunc (Type, Expr)
  | TypeErrorApplyFailure (Type, Expr) (Type, Expr)
  deriving Show


(===) :: Type -> Type -> Bool
TypeVal v === TypeVal v' = v == v'
TypeFunc a b === TypeFunc c d = a === c && b === d
TypeFuncDelay _ _ === TypeFuncDelay _ _ = True
TypeFuncDelayWithResultType _ _ t1 === TypeFuncDelayWithResultType _ _ t2 = t1 === t2

TypeFuncDelay _ _ === TypeFuncDelayWithResultType {} = True
TypeFuncDelay _ _ === TypeFunc _ _ = True
TypeFuncDelayWithResultType _ _ t1 === TypeFunc _ t2 = t1 === t2

_ === _ = False


checkType' :: Env -> Expr -> Either TypeError Type
checkType' env (ExprVar v) =
  case lookup v env of
      Nothing -> Left $ TypeErrorUnboundedVariable v
      Just t  -> Right t
checkType' env (ExprLambda v e) =
  case checkType' env e of 
    Left (TypeErrorUnboundedVariable _) -> Right $ TypeFuncDelay v e
    Right t -> Right $ TypeFuncDelayWithResultType v e t
    Left x -> Left x
checkType' env (ExprApply func val) = do
  tArg <- checkType' env val
  tFunc <- checkType' env func
  case tFunc of
    TypeFuncDelay v body -> checkType' ((v, tArg) : env) body
    TypeFuncDelayWithResultType v body _ -> checkType' ((v, tArg) : env) body
    TypeFunc a b ->
      if a === tArg
      then Right b
      else Left $ TypeErrorApplyFailure (tFunc, func) (tArg, val)
    _ -> Left $ TypeErrorNotFunc (tFunc, func)

checkType' _ (ExprValue (ValInt _)) = Right $ TypeVal ValTypeInt
checkType' _ (ExprValue (ValNum _)) = Right $ TypeVal ValTypeNum
checkType' _ (ExprHostFunc _ t _) = Right t

checkType :: Expr -> Either TypeError Type
checkType = checkType' []

testTypeChecker :: IO ()
testTypeChecker =
  print $ checkType $
    ExprApply (ExprLambda "a" $ ExprLambda "b" $ ExprVar "a") $ ExprValue $ ValInt 1

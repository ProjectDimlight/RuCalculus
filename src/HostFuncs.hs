module HostFuncs where
import AST
import TypeChecker

type ParamsCount = Int
type CurriedHostFunc = Expr -> IO (Either String Expr)
type UncurriedHostFunc = [Expr] -> IO (Either String Expr)
type HostFuncQuad = (String, Type, IsLazy, CurriedHostFunc)

pack' :: IsLazy -> String -> Type -> ParamsCount -> UncurriedHostFunc -> [Expr] -> CurriedHostFunc
pack' _ name t 1 f args = \arg -> f (reverse $ arg : args)
pack' l name t n f args = \arg ->
  pure $ Right $ ExprHostFunc name t l $ pack' l name nextType (n - 1) f (arg : args)
  where nextType = case t of TypeFunc _ b -> b
                             TypeFuncDelay a b -> TypeFuncDelay a b
                             TypeFuncDelayWithResultType _ _ a -> a
                             _ -> error "Can not pack function with non-function type"


makeUntypedHostFuncType :: ParamsCount -> Type -> Type
makeUntypedHostFuncType 0 resultType = resultType
makeUntypedHostFuncType n resultType =
  TypeFuncDelayWithResultType "" (ExprVar "") $ makeUntypedHostFuncType (n - 1) resultType


packUntyped :: IsLazy -> String -> ParamsCount -> UncurriedHostFunc -> HostFuncQuad
packUntyped isLazy name params f
  | params <= 0 = error "packUntyped: params <= 0"
  | otherwise =
    (name, t, isLazy, pack' isLazy name t params f [])
    where
      dummyType = TypeFuncDelay "" $ ExprVar ""
      t = makeUntypedHostFuncType (params - 1) dummyType

packResultTyped :: IsLazy -> String -> ParamsCount -> Type -> UncurriedHostFunc -> HostFuncQuad
packResultTyped isLazy name params returnType f
  | params <= 0 = error "packResultTyped: params <= 0"
  | otherwise =
    (name, t, isLazy, pack' isLazy name t params f [])
    where t = makeUntypedHostFuncType params returnType

packTyped :: IsLazy -> String -> [Type] -> Type -> UncurriedHostFunc -> HostFuncQuad
packTyped isLazy name paramTypes returnType f
  | null paramTypes = error "packTyped: paramTypes is null."
  | otherwise =
    (name, typeOfFunc, isLazy, pack' isLazy name typeOfFunc (length paramTypes) f [])
    where typeOfFunc = foldr TypeFunc returnType paramTypes

hostFuncs :: [HostFuncQuad]
hostFuncs =
    [
        sum,
        sub,
        prod,
        div,
        mod,
        match,
        equal,
        unequal,
        print'
    ]
    where
    sum = packUntyped False "和" 2 $ \[a, b] ->
        pure $
        case (a, b) of
            (ExprValue (ValInt a'), ExprValue (ValInt b')) -> Right $ ExprValue $ ValInt $ a' + b'
            (ExprValue (ValNum a'), ExprValue (ValNum b')) -> Right $ ExprValue $ ValNum $ a' + b'
            _ -> Left "参数非数，且亦数类也"

    sub = packUntyped False "差" 2 $ \[a, b] ->
        pure $ case (a, b) of
        (ExprValue (ValInt a'), ExprValue (ValInt b')) -> Right $ ExprValue $ ValInt $ a' - b'
        (ExprValue (ValNum a'), ExprValue (ValNum b')) -> Right $ ExprValue $ ValNum $ a' - b'
        _ -> Left "参数非数也，且亦数类也"

    prod = packUntyped False "积" 2 $ \[a, b] ->
        pure $ case (a, b) of
        (ExprValue (ValInt a'), ExprValue (ValInt b')) -> Right $ ExprValue $ ValInt $ a' * b'
        (ExprValue (ValNum a'), ExprValue (ValNum b')) -> Right $ ExprValue $ ValNum $ a' * b'
        _ -> Left "参数非数也，且亦数类也"

    div = packUntyped False "商" 2 $ \[a, b] ->
        pure $ case (a, b) of
        (ExprValue (ValInt _), ExprValue (ValInt 0)) -> Left "为法不能零"
        (ExprValue (ValInt a'), ExprValue (ValInt b')) -> Right $ ExprValue $ ValInt $ a' `Prelude.div` b'
        (ExprValue (ValNum a'), ExprValue (ValNum b')) -> Right $ ExprValue $ ValNum $ a' / b'
        _ -> Left "参数非数也，且亦数类也"

    mod = packUntyped False "余" 2 $ \[a, b] ->
        pure $ case (a, b) of
        (ExprValue (ValInt _), ExprValue (ValInt 0)) -> Left "为法不能零"
        (ExprValue (ValInt a'), ExprValue (ValInt b')) -> Right $ ExprValue $ ValInt $ a' `Prelude.mod` b'
        (ExprValue (ValNum a'), ExprValue (ValNum b')) -> Left "不能实数求余"
        _ -> Left "参数非数也，且亦数类也"

    match = packUntyped False "择" 3 $ \[cond, a, b] ->
        pure $ case cond of
          (ExprValue (ValInt 0)) -> Right $ b
          (ExprValue (ValInt _)) -> Right $ a
          _ -> Left "条件非数也，且亦数类也"

    equal = packUntyped False "等" 2 $ \[a, b] ->
        pure $ case (a, b) of
        (ExprValue (ValInt a'), ExprValue (ValInt b')) -> Right $ ExprValue $ ValInt $ if a' == b' then 1 else 0
        (ExprValue (ValNum a'), ExprValue (ValNum b')) -> Right $ ExprValue $ ValInt $ if a' == b' then 1 else 0
        _ -> Left "参数非数也，且亦数类也"
    
    unequal = packUntyped False "异" 2 $ \[a, b] ->
        pure $ case (a, b) of
        (ExprValue (ValInt a'), ExprValue (ValInt b')) -> Right $ ExprValue $ ValInt $ if a' /= b' then 1 else 0
        (ExprValue (ValNum a'), ExprValue (ValNum b')) -> Right $ ExprValue $ ValInt $ if a' /= b' then 1 else 0
        _ -> Left "参数非数也，且亦数类也"

    print' = packResultTyped False "书" 1 (TypeVal ValTypeUnit) $ \[a] -> 
        print a >> pure (Right $ ExprValue ValUnit)

    
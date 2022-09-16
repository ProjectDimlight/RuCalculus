{-# LANGUAGE LambdaCase #-}
module HostFuncs where
import AST
import Data.Char
import TypeChecker
import Text.Show.Unicode

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
apply' _ _ (ExprInclude v e) = ExprInclude v e

injectHostFunctions :: [(String, Type, IsLazy, Expr -> IO (Either String Expr))] -> Expr -> Expr
injectHostFunctions ls e = foldl (\e (name, t, l, f) -> apply' name (ExprHostFunc name t l f) e) e ls

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
        true', 
        false',
        
        sum,
        sub,
        prod,
        div,
        mod,

        gt,
        lt,
        ge,
        le,

        identical,
        equal,
        unequal,
        
        print',
        printZi
    ]
    where

    true' = packUntyped True "真" 2 $ \[a, b] -> pure $ Right $ a
    false' = packUntyped True "伪" 2 $ \[a, b] -> pure $ Right $ b

    boolean cond = do
        case (if cond then true' else false') of
             (name, t, l, f) -> (ExprHostFunc name t l f)

    sum = packUntyped False "和" 2 $ \[a, b] ->
        pure $
        case (a, b) of
            (ExprValue (ValInt a'), ExprValue (ValInt b')) -> Right $ ExprValue $ ValInt $ a' + b'
            (ExprValue (ValNum a'), ExprValue (ValNum b')) -> Right $ ExprValue $ ValNum $ a' + b'
            (ExprValue (ValUnit), ExprValue (ValUnit)) -> Right $ ExprValue $ ValUnit
            _ -> Left "参数非数，且亦数类也"

    sub = packUntyped False "差" 2 $ \[a, b] ->
        pure $ case (a, b) of
        (ExprValue (ValInt a'), ExprValue (ValInt b')) -> Right $ ExprValue $ ValInt $ a' - b'
        (ExprValue (ValNum a'), ExprValue (ValNum b')) -> Right $ ExprValue $ ValNum $ a' - b'
        _ -> Left "参数非数也，且亦数类也"

    gt = packUntyped False "盈" 2 $ \[a, b] ->
        pure $ case (a, b) of
        (ExprValue (ValInt a'), ExprValue (ValInt b')) -> Right $ boolean $ a' > b'
        (ExprValue (ValNum a'), ExprValue (ValNum b')) -> Right $ boolean $ a' > b'
        _ -> Left "参数非数也，且亦数类也"
        
    lt = packUntyped False "亏" 2 $ \[a, b] ->
        pure $ case (a, b) of
        (ExprValue (ValInt a'), ExprValue (ValInt b')) -> Right $ boolean $ a' < b'
        (ExprValue (ValNum a'), ExprValue (ValNum b')) -> Right $ boolean $ a' < b'
        _ -> Left "参数非数也，且亦数类也"
        
    ge = packUntyped False "【或盈】" 2 $ \[a, b] ->
        pure $ case (a, b) of
        (ExprValue (ValInt a'), ExprValue (ValInt b')) -> Right $ boolean $ a' >= b'
        (ExprValue (ValNum a'), ExprValue (ValNum b')) -> Right $ boolean $ a' >= b'
        _ -> Left "参数非数也，且亦数类也"
        
    le = packUntyped False "【或亏】" 2 $ \[a, b] ->
        pure $ case (a, b) of
        (ExprValue (ValInt a'), ExprValue (ValInt b')) -> Right $ boolean $ a' <= b'
        (ExprValue (ValNum a'), ExprValue (ValNum b')) -> Right $ boolean $ a' <= b'
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

    identical = packUntyped False "同" 2 $ \[a, b] ->
        pure $ case (a, b) of
        (ExprValue a', ExprValue b') -> Right $ boolean $ a' == b'
        _ -> Right $ boolean $ False
        
    equal = packUntyped False "等" 2 $ \[a, b] ->
        pure $ case (a, b) of
        (ExprValue a', ExprValue b') -> Right $ boolean $ a' == b'
        _ -> Left ((ushow a) ++ "与" ++ (ushow b) ++ "：参数非值也")
    
    unequal = packUntyped False "异" 2 $ \[a, b] ->
        pure $ case (a, b) of
        (ExprValue (ValInt a'), ExprValue (ValInt b')) -> Right $ boolean $ a' /= b'
        (ExprValue (ValNum a'), ExprValue (ValNum b')) -> Right $ boolean $ a' /= b'
        _ -> Right $ boolean $ True

    print' = packResultTyped False "书" 1 (TypeVal ValTypeUnit) $ \[a] -> 
        putStr (show a) >> pure (Right $ ExprValue ValUnit)

    printZi = packResultTyped False "【活字印刷】" 1 (TypeVal ValTypeUnit) $ \[a] -> 
        case (a) of
        ExprValue (ValInt a') -> putStr (urecover [chr (fromIntegral a' :: Int)]) >> pure (Right $ ExprValue ValUnit)
        _ -> pure $ Left ((ushow a) ++ "：参数非整数也，寻其活字而不得")


    
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

    true' = packUntyped True "???" 2 $ \[a, b] -> pure $ Right $ a
    false' = packUntyped True "???" 2 $ \[a, b] -> pure $ Right $ b

    boolean cond = do
        case (if cond then true' else false') of
             (name, t, l, f) -> (ExprHostFunc name t l f)

    sum = packUntyped False "???" 2 $ \[a, b] ->
        pure $
        case (a, b) of
            (ExprValue (ValInt a'), ExprValue (ValInt b')) -> Right $ ExprValue $ ValInt $ a' + b'
            (ExprValue (ValNum a'), ExprValue (ValNum b')) -> Right $ ExprValue $ ValNum $ a' + b'
            (ExprValue (ValUnit), ExprValue (ValUnit)) -> Right $ ExprValue $ ValUnit
            _ -> Left "??????????????????????????????"

    sub = packUntyped False "???" 2 $ \[a, b] ->
        pure $ case (a, b) of
        (ExprValue (ValInt a'), ExprValue (ValInt b')) -> Right $ ExprValue $ ValInt $ a' - b'
        (ExprValue (ValNum a'), ExprValue (ValNum b')) -> Right $ ExprValue $ ValNum $ a' - b'
        _ -> Left "?????????????????????????????????"

    gt = packUntyped False "???" 2 $ \[a, b] ->
        pure $ case (a, b) of
        (ExprValue (ValInt a'), ExprValue (ValInt b')) -> Right $ boolean $ a' > b'
        (ExprValue (ValNum a'), ExprValue (ValNum b')) -> Right $ boolean $ a' > b'
        _ -> Left "?????????????????????????????????"
        
    lt = packUntyped False "???" 2 $ \[a, b] ->
        pure $ case (a, b) of
        (ExprValue (ValInt a'), ExprValue (ValInt b')) -> Right $ boolean $ a' < b'
        (ExprValue (ValNum a'), ExprValue (ValNum b')) -> Right $ boolean $ a' < b'
        _ -> Left "?????????????????????????????????"
        
    ge = packUntyped False "????????????" 2 $ \[a, b] ->
        pure $ case (a, b) of
        (ExprValue (ValInt a'), ExprValue (ValInt b')) -> Right $ boolean $ a' >= b'
        (ExprValue (ValNum a'), ExprValue (ValNum b')) -> Right $ boolean $ a' >= b'
        _ -> Left "?????????????????????????????????"
        
    le = packUntyped False "????????????" 2 $ \[a, b] ->
        pure $ case (a, b) of
        (ExprValue (ValInt a'), ExprValue (ValInt b')) -> Right $ boolean $ a' <= b'
        (ExprValue (ValNum a'), ExprValue (ValNum b')) -> Right $ boolean $ a' <= b'
        _ -> Left "?????????????????????????????????"

    prod = packUntyped False "???" 2 $ \[a, b] ->
        pure $ case (a, b) of
        (ExprValue (ValInt a'), ExprValue (ValInt b')) -> Right $ ExprValue $ ValInt $ a' * b'
        (ExprValue (ValNum a'), ExprValue (ValNum b')) -> Right $ ExprValue $ ValNum $ a' * b'
        _ -> Left "?????????????????????????????????"

    div = packUntyped False "???" 2 $ \[a, b] ->
        pure $ case (a, b) of
        (ExprValue (ValInt _), ExprValue (ValInt 0)) -> Left "???????????????"
        (ExprValue (ValInt a'), ExprValue (ValInt b')) -> Right $ ExprValue $ ValInt $ a' `Prelude.div` b'
        (ExprValue (ValNum a'), ExprValue (ValNum b')) -> Right $ ExprValue $ ValNum $ a' / b'
        _ -> Left "?????????????????????????????????"

    mod = packUntyped False "???" 2 $ \[a, b] ->
        pure $ case (a, b) of
        (ExprValue (ValInt _), ExprValue (ValInt 0)) -> Left "???????????????"
        (ExprValue (ValInt a'), ExprValue (ValInt b')) -> Right $ ExprValue $ ValInt $ a' `Prelude.mod` b'
        (ExprValue (ValNum a'), ExprValue (ValNum b')) -> Left "??????????????????"
        _ -> Left "?????????????????????????????????"

    identical = packUntyped False "???" 2 $ \[a, b] ->
        pure $ case (a, b) of
        (ExprValue a', ExprValue b') -> Right $ boolean $ a' == b'
        _ -> Right $ boolean $ False
        
    equal = packUntyped False "???" 2 $ \[a, b] ->
        pure $ case (a, b) of
        (ExprValue a', ExprValue b') -> Right $ boolean $ a' == b'
        _ -> Left ((ushow a) ++ "???" ++ (ushow b) ++ "??????????????????")
    
    unequal = packUntyped False "???" 2 $ \[a, b] ->
        pure $ case (a, b) of
        (ExprValue (ValInt a'), ExprValue (ValInt b')) -> Right $ boolean $ a' /= b'
        (ExprValue (ValNum a'), ExprValue (ValNum b')) -> Right $ boolean $ a' /= b'
        _ -> Right $ boolean $ True

    print' = packResultTyped False "???" 1 (TypeVal ValTypeUnit) $ \[a] -> 
        putStr (show a) >> pure (Right $ ExprValue ValUnit)

    printZi = packResultTyped False "??????????????????" 1 (TypeVal ValTypeUnit) $ \[a] -> 
        case (a) of
        ExprValue (ValInt a') -> putStr (urecover [chr (fromIntegral a' :: Int)]) >> pure (Right $ ExprValue ValUnit)
        _ -> pure $ Left ((ushow a) ++ "?????????????????????????????????????????????")


    
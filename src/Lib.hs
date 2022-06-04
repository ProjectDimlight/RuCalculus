module Lib where
import AST

ruSum :: Expr -> IO (Either String Expr)
ruSum (ExprApply (ExprApply name (ExprValue x)) (ExprValue y)) = pure $ Right (ExprValue (x + y))
ruSum _ = pure $ Left "Cannot apply sum here"
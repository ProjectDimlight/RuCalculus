module MonadicParse where
import AST
import Text.Parsec
import qualified Text.Parsec.Token as P
import Text.Parsec.Language (haskellDef)

ruChar :: Parsec String st Char
ruChar = noneOf "入得【】"

concatParser :: [Parsec String st String] -> (Parsec String st String)
concatParser [s] = s
concatParser (s:xs) = fmap (++) s <*> concatParser xs

variable :: Parsec String st AST.Variable
variable = ( concatParser [string "【", many (noneOf "】"), string "】"] )
    <|> count 1 (ruChar)
    <|> error "Illegal identifier"

lexer = P.makeTokenParser haskellDef
value :: Parsec String st Value
value = fmap ValInt (P.integer lexer)

exprLambda :: Parsec String st Expr
exprLambda = do _ <- string "入"
                var <- variable
                _ <- string "得"
                exp <- expr
                return $ ExprLambda var exp

exprApply :: Parsec String st Expr
exprApply = do exp2 <- expr'
               _ <- string "之"
               exp1 <- expr
               return $ ExprApply exp1 exp2 

exprApplyRev :: Parsec String st Expr
exprApplyRev = do exp1 <- exprLambda
                  _ <- string "取"
                  exp2 <- expr
                  _ <- string "者"
                  return $ ExprApply exp1 exp2

expr' :: Parsec String st Expr
expr' = try exprApplyRev
    <|> try exprLambda
    <|> fmap ExprValue (try value)
    <|> fmap ExprVar (try variable)

expr :: Parsec String st Expr
expr = (try exprApply) <|> (try expr')
          
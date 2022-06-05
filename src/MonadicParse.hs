module MonadicParse where
import AST
import Text.Parsec
import qualified Text.Parsec.Token as P
import Text.Parsec.Language (haskellDef)
import Text.Parsec.String (parseFromFile)

ruChar :: Parsec String st Char
ruChar = noneOf "入得之取者也以为并则即元【】"

ruWhiteSpace :: Parsec String st Char
ruWhiteSpace = oneOf " \t\n，：！？。"

ruSpaces :: Parsec String st ()
ruSpaces = optional (many ruWhiteSpace)

concatParser :: [Parsec String st String] -> (Parsec String st String)
concatParser [] = error "Empty list of parsers"
concatParser [s] = s
concatParser (s:xs) = fmap (++) s <*> concatParser xs

variable :: Parsec String st AST.Variable
variable = try ( concatParser [ruString "【", many (noneOf "】"), ruString "】"] )
    <|> count 1 (ruChar)

lexer = P.makeTokenParser haskellDef
value :: Parsec String st Value
value = fmap ValNum (try (P.float lexer))
    <|> fmap ValInt (try (P.integer lexer))
    <|> try (do _ <- ruString "元"
                return ValUnit)

ruString str = do _ <- ruSpaces
                  res <- string str
                  _ <- ruSpaces
                  return res

exprValue = fmap ExprValue value
exprVariable = fmap ExprVar variable

exprLambda :: Parsec String st Expr
exprLambda = do _ <- ruString "入"
                var <- variable
                _ <- ruString "得"
                exp <- expr
                return $ ExprLambda var exp

exprApplyBin :: Expr -> (Parsec String st Expr)
exprApplyBin exp2 = (do _ <- ruString "与"
                        t1 <- exprR
                        exp3 <- (try (exprApplyBin t1)) <|> (return t1)
                        _ <- ruString "之"
                        exp1 <- exprR
                        return (ExprApply (ExprApply exp1 exp2) exp3))

exprApplyUni :: Expr -> (Parsec String st Expr)
exprApplyUni exp2 = (do _ <- ruString "之"
                        exp1 <- expr
                        return (ExprApply exp1 exp2)) 

exprApply :: Parsec String st Expr
exprApply = do exp2 <- exprR
               rest exp2
            where rest exp2 = (do x <- (try (exprApplyBin exp2))
                                  rest x)
                          <|> (do x <- (try (exprApplyUni exp2))
                                  rest x)
                          <|> return exp2

exprApplyRev :: Parsec String st Expr
exprApplyRev = do exp1 <- try exprLambda
                      <|> try exprValue
                      <|> try exprVariable
                  _ <- ruString "取"
                  exp2 <- expr
                  _ <- ruString "者"
                  return $ ExprApply exp1 exp2

exprLet :: Parsec String st Expr
exprLet = do _ <- ruString "以"
             var <- variable
             _ <- ruString "为"
             exp1 <- exprApply
             other <- try (ruString "并") <|> try (ruString "则")
             exp2 <- expr
             return $ ExprApply (ExprLambda var exp2) exp1

exprR :: Parsec String st Expr
exprR = try exprApplyRev
    <|> try exprLambda
    <|> try exprValue
    <|> try exprVariable

expr :: Parsec String st Expr
expr = do _ <- ruSpaces
          res <- (try exprLet) <|> (try exprApply)
          _ <- ruSpaces
          return res


parseRu :: FilePath -> IO (Either ParseError Expr)
parseRu = parseFromFile expr

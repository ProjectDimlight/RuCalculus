{-# LANGUAGE LambdaCase #-}

module MonadicParse where
import AST
import Data.Char
import Text.Parsec
import qualified Text.Parsec.Token as P
import Text.Parsec.Language (haskellDef)
import Text.Parsec.String (parseFromFile)

ruChar :: Parsec String st Char
ruChar = noneOf "令时入之取者也以为并否则即元引【】"

ruWhiteSpace :: Parsec String st Char
ruWhiteSpace = oneOf " \t\n、，：！？得。"

ruSpaces :: Parsec String st ()
ruSpaces = optional (many ruWhiteSpace)

concatParser :: [Parsec String st String] -> (Parsec String st String)
concatParser [] = error "此列为空"
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

stringConstant' :: Parsec String st Expr
stringConstant' = try (do _ <- string "”"
                          return $ ExprValue ValUnit)
              <|> try (do c <- noneOf "”"
                          rest <- stringConstant'
                          return $ ExprApply (ExprApply (ExprVar "对") (ExprValue (ValInt (toInteger (ord c))))) rest)

stringConstant :: Parsec String st Expr
stringConstant = do _ <- string "“"
                    lst <- stringConstant'
                    return lst

exprValue = stringConstant <|> fmap ExprValue value
exprVariable = fmap ExprVar variable

exprLambda :: Parsec String st Expr
exprLambda = do _ <- ruString "入"
                var <- variable
                _ <- try(ruString "") 
                exp <- expr
                return $ ExprLambda var exp

exprApplyBin :: Expr -> (Parsec String st Expr)
exprApplyBin exp2 = (do _ <- ruString "与"
                        t1 <- exprR
                        exp3 <- (try (exprApplyBin t1)) <|> (return t1)
                        _ <- try (ruString "之") <|> (ruString "相")
                        exp1 <- exprR
                        return (ExprApply (ExprApply exp1 exp2) exp3))

exprApplyBin2 :: Expr -> (Parsec String st Expr)
exprApplyBin2 exp2 = (do exp1 <- exprR
                         _ <- try (ruString "于")
                         t1 <- exprR
                         exp3 <- (try (exprApplyBin t1)) <|> (return t1)
                         return (ExprApply (ExprApply exp1 exp2) exp3))

exprApplyUni :: Expr -> (Parsec String st Expr)
exprApplyUni exp2 = (do _ <- ruString "之"
                        exp1 <- exprR
                        return (ExprApply exp1 exp2))

exprApply :: Parsec String st Expr
exprApply = do exp2 <- exprR
               rest exp2
            where rest exp2 = (do x <- (try (exprApplyBin exp2))
                                  rest x)
                          <|> (do x <- (try (exprApplyBin2 exp2))
                                  rest x)
                          <|> (do x <- (try (exprApplyUni exp2))
                                  rest x)
                          <|> return exp2

exprApplyRev :: Parsec String st Expr
exprApplyRev = do exp1 <- try exprLambda
                      <|> try exprValue
                      <|> try exprVariable
                  rest exp1
               where rest exp1 = try (do _ <- ruString "取"
                                         _ <- variable
                                         _ <- ruString "为"
                                         exp2 <- expr
                                         _ <- ruString "者"
                                         rest $ ExprApply exp1 exp2)
                             <|> try (do _ <- ruString "取"
                                         exp2 <- expr
                                         _ <- ruString "者"
                                         rest $ ExprApply exp1 exp2)
                             <|> return exp1

--------------------------------------------------------

eta = ExprLambda "z" (ExprApply (ExprApply (ExprVar "y") (ExprVar "y")) (ExprVar "z"))
form = ExprLambda "y" (ExprApply (ExprVar "x") eta)
ycomb = ExprLambda "x" (ExprApply form form)

exprLet :: Parsec String st Expr
exprLet = do _ <- ruString "以"
             var <- variable
             _ <- ruString "为"
             exp1 <- (try exprMatch) <|> (try exprApply)
             exp2 <- try(do _ <- try (ruString "并") <|> try (ruString "则")
                            exp2 <- expr
                            return exp2)
                 <|> try exprLet
                 <|> try(do _ <- eof
                            return $ ExprVar "【引用者】")
             return $ ExprApply (ExprLambda var exp2) (ExprApply ycomb (ExprLambda var exp1))

exprMatch :: Parsec String st Expr
exprMatch = do _ <- ruString "令"
               exp1 <- expr
               _ <- ruString "时"
               _ <- optional $ ruString "取"
               exp2 <- expr
               exp3 <- exprMatch 
                   <|> try ( do _ <- ruString "否则"
                                _ <- optional $ ruString "取"
                                exp3 <- expr
                                return exp3)
                   <|> (do return (ExprValue ValUnit))
               return $ ExprApply (ExprApply exp1 exp2) exp3

--------------------------------------------------------

exprR :: Parsec String st Expr
exprR = try exprApplyRev
    <|> try exprLambda
    <|> try exprValue
    <|> try exprVariable

exprInclude :: Parsec String st Expr
exprInclude = do _ <- ruString "引"
                 fname <- variable
                 expX <- expr
                 return $ ExprApply (ExprLambda "【引用者】" (ExprInclude fname)) expX

expr :: Parsec String st Expr
expr = do _ <- ruSpaces
          res <- (try exprInclude) <|> (try exprLet) <|> (try exprMatch) <|> (try exprApply)
          _ <- ruSpaces
          return res

parseRu :: FilePath -> IO (Either ParseError Expr)
parseRu = parseFromFile expr

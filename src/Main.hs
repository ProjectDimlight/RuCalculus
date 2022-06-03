{-# LANGUAGE FlexibleContexts #-}

module Main where

import MonadicParse as MP
import Text.Parsec
import Text.Show.Unicode
import Debug.Trace
import Control.Monad.Identity (Identity, runIdentity)

myParseTest :: (Stream s Identity t, Show a)
          => Parsec s () a -> s -> IO ()
myParseTest p input
    = case parse p "" input of
        Left err -> do putStr "parse error at "
                       uprint err
        Right x  -> uprint x

myParserTrace :: (Show t, Stream s m t) => String -> ParsecT s u m ()
{-# INLINABLE myParserTrace #-}
myParserTrace s = pt <|> return ()
    where
        pt = try $ do
           x <- try $ many1 anyToken
           trace (s++": " ++ ushow x) $ try $ eof
           fail (ushow x)

main :: IO ()
main = do
  --code <- getLine
  uprint (parse MP.variable "" "【入语言】")
  uprint (parse MP.value "" "123甲")
  myParseTest (MP.expr >> parserTrace "label") "入甲得甲"
  myParseTest (MP.expr >> parserTrace "label") "入甲得甲取2者"
  myParseTest (MP.expr >> parserTrace "label") "2之入甲得甲"

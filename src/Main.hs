{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import MonadicParse as MP
import AST
import HostFuncs
import Text.Parsec
import Text.Show.Unicode
import qualified System.Environment
import Interp

data Option = Option { debug :: Bool, file :: Maybe String }

parseOption :: Option -> [String] -> Either String Option
parseOption o [] = Right o
parseOption Option { debug = True } ("-d":_) = Left "\'-d\'标记仅可使用一次"
parseOption o ("-d":xs) = parseOption o { debug = True } xs
parseOption Option { file = Just _} (_:_) = Left "仅可指定一个要执行的文件"
parseOption o (file:xs) = parseOption o { file = Just file } xs

help :: IO ()
help = putStr $ unlines
    [
        "入算术",
        "作者（排名分先后）：SOL、AntonPing、许兴逸",
        "",
        "行此术：",
        "    ./RuCalculus <src.入>",
        ""
    ]

runProgram :: Option -> IO ()
runProgram Option { debug = debug, file = Just file } =
  let expr = parseRu file (AST.ExprValue AST.ValUnit)
      runCode = runInterp . injectHostFunctions hostFuncs in
  expr >>= \case
    Left err -> print err
    Right x -> do
        res <- runCode x
        case res of
            Left err -> print err
            Right (AtomValue ValUnit) -> putStrLn ""
            Right atom -> print atom
        return ()
runProgram _ = help

main :: IO ()
main = System.Environment.getArgs >>= either putStrLn runProgram . parseOption Option { debug = False, file = Nothing }

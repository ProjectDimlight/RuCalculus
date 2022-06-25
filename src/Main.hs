{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import MonadicParse as MP
import Eval
import HostFuncs
import Text.Parsec
import Text.Show.Unicode
import qualified System.Environment

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
        "作者：SOL、许兴逸",
        "",
        "行此术：",
        "    ./RuCalculus [-d] <src.入>",
        "",
        "    -d 即逐步演算",
        ""
    ]

runProgram :: Option -> IO ()
runProgram Option { debug = debug, file = Just file } =
  let expr = parseRu file
      runnerBasic = if debug then runStepByStep else run True
      runCode = runnerBasic . injectHostFunctions hostFuncs in
  expr >>= \case
    Left err -> print err
    Right x -> runCode x
runProgram _ = help

main :: IO ()
main = System.Environment.getArgs >>= either putStrLn runProgram . parseOption Option { debug = False, file = Nothing }

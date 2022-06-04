{-# LANGUAGE FlexibleContexts #-}

module Main where

import MonadicParse as MP
import Text.Parsec
import Text.Show.Unicode

main :: IO ()
main = do
  --code <- getLine
  uprint ("----------------------------")
  uprint ("第1节 Token")
  uprint (parse MP.variable "" "【入语言】")
  uprint (parse MP.variable "" "【入语言】123")
  uprint (parse MP.value "" "123甲")
  uprint (parse MP.expr "" "甲")
  uprint (parse MP.expr "" "【入语言】")
  uprint ("----------------------------")
  uprint ("第2节 复合函数")
  uprint (parse MP.expr "" "入甲得甲")
  uprint (parse MP.expr "" "入甲得甲取2者")
  uprint (parse MP.expr "" "2之入甲得甲")
  uprint (parse MP.expr "" "甲与甲之积")
  uprint (parse MP.expr "" "入甲得甲与甲之积")
  uprint (parse MP.expr "" "入函得入甲得甲之函之函")
  uprint (parse MP.expr "" "3之入函得入甲得甲之函之函取入甲得甲与甲之积者")
  uprint ("----------------------------")
  uprint ("第3节 二元函数")
  uprint (parse MP.expr "" "甲与甲与甲之积之积")
  uprint (parse MP.expr "" "甲与甲之积与甲之积")
  uprint (parse MP.expr "" "3之入甲得甲与甲之积与甲与甲之积之积")
  uprint (parse MP.expr "" "3之入甲得甲与甲之积与甲与甲之积之积")
  uprint ("----------------------------")
  uprint ("第4节 let")
  uprint (parse MP.expr "" "以甲为100则3之入乙得甲与乙之和")
  uprint ("----------------------------")
  uprint ("第5节 空白字符")
  uprint (parse MP.expr "" "以 甲 为 100 ，则：\n3之 入乙得甲与乙之和？")

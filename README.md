# RuCalculus

"入语言"是一种用中文表示lambda演算的函数式编程语言，大概对标[wenyan-lang](https://github.com/wenyan-lang/wenyan)。  
该项目致力于让lambda演算基本符合中文（文言文）的语法结构，例如入语言中大量使用后缀表达式（如square x写作甲之平方）。 由于基于lambda演算，在很多情况下入语言的代码将会更加清晰而简练（例如[斐氏列线性.入](./samples/斐氏列线性.入)和[快排.入](./samples/快排.入)） 。


项目刚刚启动，功能和文档尚不完全，欢迎贡献任何力量。

## Quick Start

使用入语言解释器：
```
RuCalculus 源代码.入
RuCalculus 源代码.入 -d   # 逐步打印演算过程
```

使用stack启动入语言解释器：
```
stack run -- 源代码.入
```

在[samples](./samples/)目录下可以查看现有的范例程序。

## Highlight grammar features

- 自带丫（y组合子）的let
```
以甲为1        // 等价于 let 甲 = 1
```
- 单参数函数更符合中文语序：
```
甲之平方       // 等价于 square x
【序对】取首者 // 科里化语法，等价于 (序对 first)
```
- 双参数函数求值的多变语法：
```
甲与乙之和     // 等价于 ((和 甲) 乙)
甲与乙相等     // 等价于 ((等 甲) 乙)
甲亏于乙       // 等价于 ((亏 甲) 乙)，也就是甲小于乙
```
- 多参数函数科里化：
```
甲之盈者    // 等价于 lambda y. greater x y，即取甲“盈”过的数，也就是判断是否小于甲的函数
```

## Documents

- TODO

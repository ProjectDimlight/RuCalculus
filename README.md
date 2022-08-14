# 入墨答（Rumbda Calculus）

"入墨答"（又称Rumbda Calculus，RuCalculus，或者入语言）是一种用中文表示lambda演算的函数式编程语言。该项目致力于让lambda演算贴合中文（文言文），因而加入大量变种语法以适应各类语言习惯。 由于基于lambda演算，在很多情况下入语言的代码将会更加清晰而简练（例如[斐氏列线性.入](./samples/斐氏列线性.入)和[快排.入](./samples/快排.入)） 。

- 入墨答是lambda的谐音
- 入象征λ
- 墨，即笔墨，象征汉字
- 答，即答题，象征演算
- 故，入墨答即为“λ汉字演算”。


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

- 入
```
入甲得甲             // 等价于 lambda x. x， 如你所见，入是lambda的象形文字
```
- 自带丫（y组合子）的let
```
以甲为1              // 等价于 let 甲 = 1
```
- 单参数函数更符合中文语序：
```
甲之【平方】          // 等价于 square x，相当于 pipeline，左结合
【平方】取甲者        // 等价于 square x
【平方】取根为甲者    // 等价于 square x，但“根”在语法中并无实际含义，仅增加可读性
```
- 双参数函数求值的多变语法：
```
甲与乙之和           // 等价于 ((和 甲) 乙)
甲与乙相等           // 等价于 ((等 甲) 乙)
甲亏于乙             // 等价于 ((亏 甲) 乙)，也就是甲小于乙
```
- 多参数函数科里化：
```
甲之盈               // 等价于 lambda y. greater x y，即取甲“盈”过的数，也就是判断是否小于甲的函数
```

## Documents

- [语法手册](./doc/Manual.md)
- [教程1：入门](./doc/Tutorial1.md)

## Acknowledgements

- [wenyan-lang](https://github.com/wenyan-lang/wenyan)
- [yuyan](https://github.com/yuyan-lang/yuyan/)

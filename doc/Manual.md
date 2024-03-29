# Manual

## 语法特性

基本语法

| 关键词        | 解释        | 例            | Formal  | 备注 |
| ------------- | ---------- | ------------- | ----------- | ---- |
| 入..得        | lambda      | 入甲得甲      | lambda x. x |      |
| 取..者        | app         | 函取甲者      | f x         |      | 
| 取..为..者    | app         | 函取参为甲者   | f x         | “参”无实际意义，仅为语法通顺；并不要求参的名称与lambda所给参数名相同。 |
| 之            | 后序app     | 甲之函         | f x        | 可实现pipeline，如甲之【函1】之【函2】。 |
| ..与..之      | 二元函数app | 甲与乙之和    | + x y       | 可以科里化，相当于传入两个参数。 |
| 【】          | 多字标识符  | 【埃克斯】     | x           | 【甲】与甲并不是同一个标识符。 |

语法糖

| 关键词        | 解释        | 例            | Formal      | 备注 |
| ------------- | ---------- | ------------- | ----------- | ---- |
| “”            | 字符串常量  | “你好，入语言” |             | 与haskell类似，字符串是list的语法糖，而list是用pair实现的。详见“序对.入”和“列.入”。 |
| 以..为..则    | let         | 以甲为1则甲    | let x = 1 in x | 自带y组合子，可以递归。|
| 令..时取..否则 | if else    | 令甲等于1时取1否则取0 | if x=1 then 1 else 0 | 可以多个令嵌套。 |
| ..与..相      | 二元函数求值 | 甲与乙相等    | = x y       | 等价于..与..之。 |
| .. 于         | 二元函数求值 | 甲盈于乙      | > x y       | 等价于..与..之。 |
| 引            | include     | 引【./libru/甲.入】 |        | 目前仅支持以当前路径（pwd）为相对路径寻址，本质上相当于Y组合子。 |


## 基于haskell的库函数

这些函数将被自动包含。

| 名称          | 功能                              | lazy eval |
| ------------- | --------------------------------- | ------- |
| 真            | 二者取前者（丘奇布尔）             | 是       |
| 伪            | 二者取后者（丘奇布尔）             | 是       |
| 和            | 加法                              | 否       |
| 差            | 减法                              | 否       |
| 盈            | 大于                              | 否       |
| 亏            | 小于                              | 否       |
| 【或盈】      | 大于等于                          | 否       |
| 【或亏】      | 小于等于                          | 否       |
| 积            | 乘法                              | 否       |
| 商            | 除法                              | 否       |
| 余            | 除法求余数（haskell mod）         | 否       |
| 同            | 相等；若类型不同为false            | 否       |
| 等            | 相等；若类型不同则报错             | 否       |
| 异            | 不相等；若类型不同也为真           | 否       |
| 书            | 打印值                            | 否       |
| 【活字印刷】   | 打印值对应的unicode汉字           | 否       |

## 基于入的库函数

这些函数需要手动“引”。

### 序对.入

| 名称          | 参数                  | 功能                                   |
| ------------- | --------------------- | ------------------------------------- |
| 对            | 入甲，入乙，入【首次】 | 构造一个有序对；可根据【首次】选择前后项 |
| 前            | 入甲，入乙             | 二者取前者（丘奇布尔），对取前者        |
| 后            | 入甲，入乙             | 二者取后者（丘奇布尔），对取后者         |
| 首            | 入对                   | 序对取前者，对之首 （请结合“之”与“取”的含义理解）|
| 次            | 入对                   | 序对取后者，对之次                      |

### 列.入

| 名称          | 参数                  | 功能                                   |
| ------------- | --------------------- | ------------------------------------- |
| 衔            | 入【列1】，入【列2】   | 衔接两个列表                            |
| 筛            | 入【原列】，入【条件】 | 筛选列表中符合条件的元素                 |
| 【列印】      | 入列                  | 打印列表元素                            |
| 【串印】      | 入串                  | 打印字符串                              | 

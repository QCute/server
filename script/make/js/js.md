## js script 使用

#### 配置
```erl
js() ->
    [
        #{
            file => "",                               %% 文件
            sql => [                                  %% SQL列表
            ],
            extra => [],                              %% 可选额外附加的函数
        }
    ].
```

## SQL例子

#### 基本键值对

* 配置
```erl
#{
    select => {},
    from => table,
    by => key,
    as => get
}
```

* 代码
```js
return {
    "get": {
        "key": { "key": "key", "value": "value" }
    }
}
```

#### 指定列

* 配置
```erl
#{
    select => value,
    from => table,
    by => key,
    as => get
}
```

* 代码
```js
return {
    "get": {
        "key": "value"
    }
}
```

#### 指定多列

* 配置
```erl
#{
    select => {key, value},
    from => table,
    by => key,
    as => get
}
```

* 代码
```js
return {
    "get": {
        "key": { "key": "key", "value": "value" }
    }
}
```

#### 添加排除列

* 配置
```erl
#{
    select => {},
    except => key,
    from => table,
    by => key,
    as => get
}
```

* 代码
```js
return {
    "get": {
        "key": { "value": "value" }
    }
}
```

#### 指定多列数据为列表

* 配置
```erl
#{
    select => [key, value],
    from => table,
    by => key,
    as => get
}
```

或者

```erl
#{
    select => list(key, value),
    from => table,
    by => key,
    as => get
}
```

* 代码
```js
return {
    "get": {
        "key": [ "key", "value" ]
    }
}
```

#### 指定多列数据为元组

* 配置
```erl
#{
    select => {key, value},
    from => table,
    by => key,
    as => get
}
```

或者

```erl
#{
    select => tuple(key, value),
    from => table,
    by => key,
    as => get
}
```

* 代码
```js
return {
    "get": {
        "key": { "key": "key", "value": "value" }
    }
}
```

#### 指定多列数据为键值表

* 配置
```erl
#{
    select => map(key, value),
    from => table,
    by => key,
    as => get
}
```

* 代码
```js
return {
    "get": {
        "key": { "key": "key", "value": "value" }
    }
}
```

#### 指定多列数据为记录

* 配置
```erl
#{
    select => record(key, value),
    from => table,
    by => key,
    as => get
}
```

* 代码
```js
return {
    "get": {
        "key": { "key": "key", "value": "value" }
    }
}
```

#### 使用聚合窗口函数

* 配置
```erl
#{
    select => min(value),
    from => table,
    as => get
}
```

* 代码
```js
return {
    "get": {
        "key": "value"
    }
}
```

#### 使用[all]()聚合多个数据

* 配置
```erl
#{
    select => all(value),
    from => table,
    as => get
}
```

* 代码
```js
return {
    "get": {
        "key": [ "value", "other_value_over_key", "another_value_over_key" ]
    }
}
```

#### 使用其他查询方式

* 配置
```erl
#{
    select => value,
    from => table,
    by => #{
        key1 => '=',
        key2 => '<',
        key3 => #{
            '>' => param(),
            like => "%Key%"
        }
    },
    as => get
}
```

* 代码
```js
return {
    "get": {
        "key1": {
            "key2": {
                "key3": "value"
            }
        }
    }
}
```

#### 对数据进行过滤

* 配置
```erl
#{
    select => value,
    from => table,
    by => #{
        key => #{
            '=' => param(),
            like => "%Key%"
        }
    },
    as => get
}
```

* 代码
```js
return {
    "get": {
        "key": "value"
    }
}
```
#### 前置去重

> SELECT \`value\` FROM ( SELECT * FROM \`table\` GROUP BY \`id\` ) AS \`GROUP_TABLE\` WHERE \`key\` = ? AND \`key\` LIKE '%Key%'

* 配置
```erl
#{
    select => value,
    from => table,
    by => #{
        key => #{
            '=' => param(),
            like => "%Key%"
        }
    },
    unique_by => id,
    as => get
}
```

* 代码
```js
return {
    "get": {
        "key": "value"
    }
}
```

#### 前置排序

> SELECT \`value\` FROM ( SELECT * FROM \`table\` ORDER BY \`id\` ASC ) AS \`ORDER_TABLE\` WHERE \`key\` = ? AND \`key\` LIKE '%Key%'

* 配置
```erl
#{
    select => value,
    from => table,
    by => #{
        key => #{
            '=' => param(),
            like => "%Key%"
        }
    },
    sort_by => id,
    as => get
}
```

* 代码
```js
return {
    "get": {
        "key": "value"
    }
}
```

## 生成
```shell
maker js 文件名(不含扩展名)
```

## SQL释义:
| Key                           | Operation                     | Description
| ----------------------------- | ----------------------------- | -----------------------------
| select                        | SELECT                        | 查询
| except                        | EXCEPT Value                  | 排除字段
| all                           |                               | 归集数据
| from                          | SELECT TABLE NAME             | 查询时使用, 指定的表名
| by                            | WHERE                         | 查询条件
| group_by                      | GROUP BY                      | 组合条件
| unique_by                     | GROUP BY                      | 前置组合条件
| having                        | HAVING                        | 组合查询条件
| order_by                      | ORDER BY                      | 排序条件
| sort_by                       | ORDER BY                      | 前置排序条件
| as                            |                               | 函数名字

* [select]() =>

| Value                         | SQL                           | Description
| ----------------------------- | ----------------------------- | -----------------------------
| value                         | \`value\`                     | 单个
| []                            | *                             | 全部, 数组形式
| {}                            | *                             | 全部, 对象形式
| list()                        | *                             | 全部, 数组形式
| map()                         | *                             | 全部, 对象形式
| [v1, v2]                      | \`v1\`, \`v2\`                | 多个, 数组形式
| {v1, v2}                      | \`v1\`, \`v2\`                | 多个, 对象形式
| list(v1, v2)                  | \`v1\`, \`v2\`                | 多个, 数组形式
| map(v1, v2)                   | \`v1\`, \`v2\`                | 多个, 对象形式

* [except]() =>

| Value                         | SQL                           | Description
| ----------------------------- | ----------------------------- | -----------------------------
| value                         | \`value\`                     | 单个
| [v1, v2]                      | \`v1\`, \`v2\`                | 多个

* [by]() =>

| Value                         | SQL                           | Description
| ----------------------------- | ----------------------------- | -----------------------------
| key                           | \`key\` = ?                   | 单个
| [k1, k2]                      | \`k1\` = ? AND \`k2\` = ?     | 多个

* [by]() => #{ } - 使用其他比较方式

| Value                         | SQL                           | Description
| ----------------------------- | ----------------------------- | -----------------------------
| k1 => '='                     | \`k1\` = ?                    | 完全匹配
| k2 => '>'                     | \`k2\` > ?                    | 区间
| k2 => '>='                    | \`k2\` >= ?                   | 区间
| k2 => '<'                     | \`k2\` < ?                    | 区间
| k2 => '<='                    | \`k2\` <= ?                   | 区间
| k2 => #{in => [1, 2, 3]}      | \`k3\` IN (1, 2, 3)           | 查找多个
| k4 => #{like => "%size%"}     | \`k4\` LIKE "%size%"          | 模糊匹配
| k5 => #{not_in => [5, "6"]}   | \`k5\` NOT IN (5, '6')        | 使用常量

* [group_by]() =>

| Value                         | SQL                           | Description
| ----------------------------- | ----------------------------- | -----------------------------
| type                          | GROUP BY \`type\`             | 单个
| [type, group]                 | GROUP BY \`type\`, \`group\`  | 多个

* [unique_by]() =>

| Value                         | SQL                           | Description
| ----------------------------- | ----------------------------- | -----------------------------
| type                          | GROUP BY \`type\`             | 单个
| [type, group]                 | GROUP BY \`type\`, \`group\`  | 多个

* [having]() =>

| Value                         | SQL                           | Description
| ----------------------------- | ----------------------------- | -----------------------------
| key                           | \`key\` = ?                   | 单个
| [k1, k2]                      | \`k1\` = ? AND \`k2\` = ?     | 多个

* [having]() => #{ } - 使用其他比较方式

| Value                         | SQL                           | Description
| ----------------------------- | ----------------------------- | -----------------------------
| k1 => '='                     | \`k1\` = ?                    | 完全匹配
| k2 => '>'                     | \`k2\` > ?                    | 区间
| k2 => '>='                    | \`k2\` >= ?                   | 区间
| k2 => '<'                     | \`k2\` < ?                    | 区间
| k2 => '<='                    | \`k2\` <= ?                   | 区间
| k2 => #{in => [1, 2, 3]}      | \`k3\` IN (1, 2, 3)           | 查找多个
| k4 => #{like => "%size%"}     | \`k4\` LIKE "%size%"          | 模糊匹配
| k5 => #{not_in => [5, "6"]}   | \`k5\` NOT IN (5, '6')        | 使用常量

* [order_by]() =>

| Value                         | SQL                           | Description
| ----------------------------- | ----------------------------- | -----------------------------
| sort                          | ORDER BY \`sort\`             | 默认升序
| [k1, k2]                      | ORDER BY \`k1\`, \`k2\`       | 默认升序

* [order_by]() => #{} - 使用其他排序方式

| Value                         | SQL                           | Description
| ----------------------------- | ----------------------------- | -----------------------------
| k1 => asc                     | \`k1\` ASC                    | 指定升序
| k2 => desc                    | \`k2\` DESC                   | 指定降序

* [sort_by]() =>

| Value                         | SQL                           | Description
| ----------------------------- | ----------------------------- | -----------------------------
| sort                          | ORDER BY \`sort\`             | 默认升序
| [k1, k2]                      | ORDER BY \`k1\`, \`k2\`       | 默认升序

* [sort_by]() => #{} - 使用其他排序方式

| Value                         | SQL                           | Description
| ----------------------------- | ----------------------------- | -----------------------------
| k1 => asc                     | \`k1\` ASC                    | 指定升序
| k2 => desc                    | \`k2\` DESC                   | 指定降序

## 规则

* 可选项
    * [all]()
    * [by]()
    * [group_by]()
    * [having]()
    * [order_by]()
    * [limit]()
    * [offset]()
* [by]()/[having]()比较支持所有预设值, 但参数化值只支持如下
    * 支持
        * 等于(=)
        * 大于(>)
        * 小于(<)
        * 大于等于(>=)
        * 小于等于(<=)
    * 不支持
        * 不等于(<>)
        * 模糊匹配(LIKE/NOT LIK)
        * 在...里面(IN/NOT IN)
        * 存在(EXISTS/NOT EXISTS)
* [all]()
    * 键重复时, 归集多个数据

## 字段规则

* 字段(Value)
    * 字段使用逗号(,)分隔, 空为全部
* 数据类型
    * Value: 单个
    * \[Values...\]: 数组
    * \{Values...\}: 对象

* 其他
    * char 类型生成字符串数据
    * varchar 类型自动转成js数据
        * 字符串 -> 字符串
        * [] -> [value1, value2, value3, ...]
        * {} -> [value1, value2, value3]

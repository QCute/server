## erl script 使用

#### 配置

```erl
erl() ->
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
```erl
-spec get(Key :: binary()) -> {binary(), binary()} | undefined.
get(<<"key">>) ->
    {<<"key">>, <<"value">>};
get(_) ->
    undefined.
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
```erl
-spec get(Key :: binary()) -> binary() | undefined.
get(<<"key">>) ->
    <<"value">>;
get(_) ->
    undefined.
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
```erl
-spec get(Key :: binary()) -> {binary(), binary()} | undefined.
get(<<"key">>) ->
    {<<"key">>, <<"value">>};
get(_) ->
    undefined.
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
```erl
-spec get(Key :: binary()) -> {binary()} | undefined.
get(<<"key">>) ->
    {<<"value">>};
get(_) ->
    undefined.
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
```erl
-spec get(Key :: binary()) -> [term()] | undefined.
get(<<"key">>) ->
    [<<"key">>, <<"value">>];
get(_) ->
    undefined.
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
```erl
-spec get(Key :: binary()) -> {binary(), binary()} | undefined.
get(<<"key">>) ->
    {<<"key">>, <<"value">>};
get(_) ->
    undefined.
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
```erl
-spec get(Key :: binary()) -> #{} | undefined.
get(<<"key">>) ->
    #{key => <<"key">>, value => <<"value">>};
get(_) ->
    undefined.
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
```erl
-spec get(Key :: binary()) -> #record{} | undefined.
get(<<"key">>) ->
    #record{key = <<"key">>, value = <<"value">>};
get(_) ->
    undefined.
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
```erl
-spec get(Key :: binary()) -> binary().
get(<<"key">>) ->
    <<"value">>.
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
```erl
-spec get(Key :: binary()) -> binary().
get(<<"key">>) ->
    [<<"value">>, <<"other_value_over_key">>, <<"another_value_over_key">>].
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
```erl
-spec get(Key1 :: binary(), Key2 :: binary(), Key3 :: binary()) -> binary() | undefined.
get(<<"Key1">>, <<"Key2">>, <<"Key3">>) ->
    <<"value">>;
get(_) ->
    undefined.
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
```erl
-spec get(Key :: binary()) -> binary() | undefined.
get(<<"key">>) ->
    <<"value">>;
get(_) ->
    undefined.
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
```erl
-spec get(Key :: binary()) -> binary() | undefined.
get(<<"key">>) ->
    <<"value">>;
get(_) ->
    undefined.
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
```erl
-spec get(Key :: binary()) -> binary() | undefined.
get(<<"key">>) ->
    <<"value">>;
get(_) ->
    undefined.
```

#### 使用默认值

* 配置
```erl
#{
    select => value,
    from => table,
    by => key,
    default => <<>>,
    as => get
}
```

* 代码
```erl
-spec get(Key :: binary()) -> binary().
get(<<"key">>) ->
    <<"value">>;
get(_) ->
    <<>>.
```

## 生成  
```shell
maker erl 文件名(不含扩展名)  
```

## SQL释义  

| Key                           | Operation                     | Description                 
| ----------------------------- | ----------------------------- | -----------------------------
| select                        | SELECT                        | 查询
| except                        | EXCEPT FIELD                  | 排除字段
| all                           |                               | 归集数据
| from                          | SELECT TABLE NAME             | 查询时使用, 指定的表名
| by                            | WHERE                         | 查询条件
| group_by                      | GROUP BY                      | 组合条件
| unique_by                     | GROUP BY                      | 前置组合条件
| having                        | HAVING                        | 组合查询条件
| order_by                      | ORDER BY                      | 排序条件
| sort_by                       | ORDER BY                      | 前置排序条件
| default                       |                               | 默认值
| as                            |                               | 函数名字

* [select]() => 

| Value                         | SQL                           | Description                 
| ----------------------------- | ----------------------------- | -----------------------------
| value                         | \`value\`                     | 单个
| []                            | *                             | 全部, 列表形式
| {}                            | *                             | 全部, 元组形式
| list()                        | *                             | 全部, 列表形式
| tuple()                       | *                             | 全部, 元组形式
| record()                      | *                             | 全部, 记录形式
| map()                         | *                             | 全部, 键值表形式
| [v1, v2]                      | \`v1\`, \`v2\`                | 多个, 列表形式
| {v1, v2}                      | \`v1\`, \`v2\`                | 多个, 元组形式
| list(v1, v2)                  | \`v1\`, \`v2\`                | 多个, 列表形式
| tuple(v1, v2)                 | \`v1\`, \`v2\`                | 多个, 元组形式
| record(v1, v2)                | \`v1\`, \`v2\`                | 多个, 记录形式
| map(v1, v2)                   | \`v1\`, \`v2\`                | 多个, 键值表形式

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
| k5 => #{not_in => [5, "6"]}   | \`k5\` NOT IN (5, '6')        | 使用字面值

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
| k5 => #{not_in => [5, "6"]}   | \`k5\` NOT IN (5, '6')        | 使用字面值

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

* [default]() => 

| Value                         | SQL                           | Description                 
| ----------------------------- | ----------------------------- | -----------------------------
| value                         | value                         | 原生代码
| []                            | *                             | 空列表
| {}                            | *                             | 空元组
| list()                        | *                             | 空列表
| tuple()                       | *                             | 空元组
| record()                      | *                             | 空记录
| map()                         | *                             | 空键值表

## 规则  

* 可选项
    * [all]()
    * [by]()
    * [group_by]()
    * [having]()
    * [order_by]()
    * [limit]()
    * [offset]()
    * [default]()
* [by]()/[having]()比较支持所有预设值, 但参数化值只支持如下
    * 支持
        * 等于(=)
        * 大于(>)
        * 小于(<)
        * 大于等于(>=)
        * 小于等于(<=)
    * 不支持
        * 不等于(<>)
        * 模糊匹配(LIKE/NOT LIKE)
        * 在...里面(IN/NOT IN)
        * 存在(EXISTS/NOT EXISTS)
* [all]()
    * 键重复时, 归集多个数据

## 字段规则  

* 字段(field)  
    * 字段使用逗号(,)分隔, 空为全部  

* 生成数据类型  
    * field: 单个  
    * \[field, fields...\]: 列表  
    * \{field, fields...\}: 元组  
    * list(field, fields...): 列表  
    * tuple(field, fields...): 元组  
    * record(field, fields...): 记录  
    * map(field, fields...): 键值表  

* 默认值  
    * 无default则默认值为undefined  

* 其他  
    * char 类型生成二进制字符串数据  
    * varchar 类型自动转成term  
        * 字符串 -> 原子  
        * [] -> [value1, value2, value3, ...]  
        * {} -> {value1, value2, value3}  

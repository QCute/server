## record script 使用

#### 配置  

```erl
record() ->
    [].
```

#### 生成  
```shell
maker record 归类名  
```

## 归类规则: 生成以下表名的记录到 [xx.hrl]() 文件  

| Table Name                    | Record                        |
| ----------------------------- | ----------------------------- |
| xx_data                       | -record(xx_data, { ... })     |
| xx_yy_data                    | -record(xx_yy_data, { ... })  |
| xx_zz_data                    | -record(xx_zz_data, { ... })  |
| xx                            | -record(xx, { ... })          |
| xx_yy                         | -record(xx_yy, { ... })       |
| xx_yy_zz                      | -record(xx_yy_zz, { ... })    |
| xx_zz                         | -record(xx_zz, { ... })       |
| xx_zz_yy                      | -record(xx_zz_yy, { ... })    |
| xx_log                        |                               |


## 字段类型  

| Field                         | Default                       | Description                 
| ----------------------------- | ----------------------------- | -----------------------------
| AUTO_INCREMENT                | 0                             | 自增字段默认值为 - 0
| TINY/SMALL/INTEGER/BIG        | DEFAULT                       | 整数字段使用数据库默认值
| BOOLEAN                       | DEFAULT                       | 布尔值字段使用数据库默认值
| DECIMAL                       | DEFAULT                       | 十进制值字段使用数据库默认值
| FLAOT/DOUBLE                  | DEFAULT                       | 浮点数值字段使用数据库默认值
| CHAR                          | <<>>                          | 字符串值默认为空二进制 - <<>>
| VARCHAR                       | DEFAULT                       | 动态长度字符串值字段使用数据库默认值
| ENUM                          | DEFAULT                       | 枚举值字段使用数据库默认值
| SET                           | DEFAULT                       | 集合值字段使用数据库默认值

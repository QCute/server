# data script 使用

* 配置  

```erl
data() ->
    [
        #{  
            file => "",           %% 文件名(相对路径)
            include => [],        %% 包含的头文件
            meta => [             %% 元数据列表
                #{
                    name => "",   %% 函数名字
                    sql => "",    %% 伪SQL
                }
            ], 
            extra => [],          %% 可选额外附加的函数
        }
    ].
```

* 生成:  
    maker data 文件名(不含扩展名)  

* 伪SQL语法:  

```sql
SELECT ALL fields FROM table WHERE condition GROUP BY condition HAVING condition ORDER BY condition LIMIT number DEFAULT value
```

* 伪SQL规则:  
    - 其中 ALL/WHERE/GROUP BY/HAVING/ORDER BY/LIMIT/DEFAULT 为可选  
    - 当WHERE条件为空时, 则查询全表  
    - 当WHERE条件非唯一(主键重复), 设置ALL可把数据归集为一(键)对多(值)的数据映射集合  
    - WHERE只支持等于(=)/大于(>)/小于(<)/大于等于(>=)/小于等于(=<), 不支持不等于(!=)和模糊匹配(LIKE)  
    - WHERE使用大于(>)/小于(<)/大于等于(>=)/小于等于(=<)比较条件时, 生成erlang函数中的when guards  
    - 如需要数据去重, 可使用GROUP BY  

* 伪SQL字段规则:  
    - 字段(field):  
        - 字段使用逗号(,)分隔, *为全部,但不包含(client)指定字段  

    - 生成数据类型:  
        - fields: 原本数值  
        - \[fields\]: 列表  
        - {fields}: 元组  
        - \#{fields}: 映射表  
        - \#record{fields}: 记录  

    - 默认值:  
        - \#record{}: 当前表名记录  
        - []: 空列表  
        - {}: 空元组  
        - \#{}: 空映射表  
        - KEY/{KEY}/[KEY]: 参数(作为默认值)  
        - value/{value}/\[value\]: 给定值  
        - 无DEFAULT则默认值为空列表[]  

    - 其他:  
        varchar 类型自动转成term  
        char 类型生成二进制字符串数据  

# record script 使用  

* 配置:  
```erl
record() ->
    [
        #{
            file => "",    %% 文件名
            table => role  %% 表名
        }
    ].
```

* 生成:  
    maker record 文件名(不含扩展名)  

* 规则:  
    - tiny/small/integer/big 字段使用数据库默认值作为记录默认值  
    - 自增auto_increment字段默认值为0  
    - varchar 默认值为空列表 []  
    - char 默认值为空二进制 <<>>  

* 其他:  
    - 字段不包含(client)指定字段  

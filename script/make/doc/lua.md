# lua script 使用

* 配置  
    {文件名(相对路径), [{伪SQL, 表名},...]}  

* 生成:  
    maker lua 文件名(不含扩展名)  

* 规则:  
    SELECT ALL fields FROM table WHERE condition GROUP BY condition ORDER BY condition LIMIT number  
    WHERE/GROUP BY/ORDER BY/LIMIT 为可选  
    无WHERE条件则查询全表  
    如WHERE条件非唯一(主键重复), 设置ALL可把数据归集为一(键)对多(值)的数据映射集合  
    如需要数据去重, 可设置GROUP BY  
    * 字段(field):  
        字段, 使用逗号分隔, *为全部,但不包含(server)指定字段  
    * 字段类型:  
        {fields}: 键值表  
        \[fields\]: 顺序表  

* 其他:  
    varchar 类型自动转成表数据, erlang原子atom自动转成字符串, erlang列表[]自动转成lua表{}  
    char 类型生成二进制字符串数据  

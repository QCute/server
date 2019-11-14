# json script 使用

* 配置  
    {文件名(相对路径), [{伪SQL, 对象键名},...]}  

* 生成:  
    maker json 文件名(不含扩展名)  

* 规则:  
    SELECT fields FROM table WHERE condition GROUP BY condition ORDER BY condition LIMIT number  
    WHERE/GROUP BY/ORDER BY/LIMIT 为可选  
    无WHERE条件则查询全表  
    如WHERE条件非唯一(主键重复), 则需要对WHERE条件进行GROUP BY, 然后数据归集为一(键)对多(值)的数据映射集合  
    * 字段(field):  
        字段, 使用逗号分隔, *为全部,但不包含(server)指定字段  
    * 数据类型:  
        \[fields\]: 数组  
        {fields}: 对象  

* 其他:  
    varchar类型自动转成数组/对象, erlang元组{}自动转成json数组[]  
    使用注释default(<<>>), 生成字符串  
    char/text类型生成字符串数据  
# js script 使用

* 配置  
    {文件名(相对路径), [{伪SQL, 对象键名}, ...]}  

* 生成:  
    maker js 文件名(不含扩展名)  

* 规则:  
    SELECT ALL fields FROM table WHERE condition GROUP BY condition HAVING condition ORDER BY condition LIMIT number  
    WHERE/GROUP BY/HAVING/ORDER BY/LIMIT 为可选  
    无WHERE条件则查询全表  
    如WHERE条件非唯一(主键重复), 设置ALL可把数据归集为一(键)对多(值)的数据映射集合  
    WHERE只支持等于(=)条件, 不支持不等于(!=)/大于(>)/小于(<)/大于等于(>=)/小于等于(=<)和模糊匹配(LIKE)  
    如需要数据去重, 可使用GROUP BY  
    - 字段(field):  
        - 字段使用逗号(,)分隔, *为全部,但不包含(server)指定字段  
    - 数据类型:  
        - {fields}: 对象  
        - \[fields\]: 数组  
* 其他:  
    varchar 类型自动转成数组/对象, erlang原子atom自动转成字符串, erlang元组{}自动转成js数组[]  
    char 类型生成字符串数据  

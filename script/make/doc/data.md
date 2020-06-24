# data script 使用

* 配置  
    {文件名(相对路径), 包含的头文件, [{伪SQL, 函数名},...]}  

* 生成:  
    maker data 文件名(不含扩展名)  

* 规则:  
    SELECT fields FROM table WHERE condition GROUP BY condition ORDER BY condition LIMIT number DEFAULT value  
    WHERE/GROUP BY/ORDER BY/LIMIT/DEFAULT 为可选  
    无WHERE条件则查询全表  
    如WHERE条件非唯一(主键重复), 则需要对WHERE条件进行GROUP BY, 然后数据归集为一(键)对多(值)的数据映射集合,数据自动去重  
    * 字段(field):  
        字段, 使用逗号分隔, *为全部,但不包含(client)指定字段  
    * 生成数据类型:  
        fields: 原本数值
        \[fields\]: 列表  
        {fields}: 元组  
        \#{fields}: 映射表  
        \#record{fields}/: 记录  
    * 默认值:  
        #record{}: 当前表名空记录  
        []: 空列表  
        {}: 空元组  
        \#{}: 空映射表  
        KEY/{KEY}/[KEY]: 参数(作为默认值)  
        value/{value}/\[value\]: 给定值  
        无DEFAULT则默认值为空列表[]  

* 其他:  
    varchar 类型自动转成term  
    char 类型生成二进制字符串数据  

# record script 使用  

* 配置:  
    {文件名(相对路径), 表名}  
    {文件名(相对路径), 表名, 记录名}  

* 生成:  
    maker.bat/sh record 文件名(不含扩展名)  

* 规则:  
    tiny/small/integer/big 字段使用数据库默认值作为记录默认值  
    varchar 默认值为空列表 []  
    char 默认值为空二进制 <<>>  

* 其他:  
    使用注释 default(x) 定义记录默认值为x  

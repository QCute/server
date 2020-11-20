# log script 使用

* 配置  
    {文件名(相对路径), 类型, 表名}  
    {文件名(相对路径), clean/retain, 表名, day/week/month(过期时间/指定时间)}  

* 生成:  
    maker log 表名  

* 规则:  
    log为调用接口  
    save为保存数据sql  
    clean为清除数据sql  
    retain为转存数据sql  

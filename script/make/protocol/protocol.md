# protocol script 使用  

* 配置  
    使用protocol记录配置具体功能一系列协议  
    erl字段为erl文件路径  
    lua字段为lua文件路径  
    js字段为js文件路径  
    cs字段为cs文件路径  
    使用io记录配置单条协议  
    使用read/write字段配置读写协议内容  
    使用handler生成*_handler路由接口  
    协议生成erlang直接读写代码和lua/js协议描述元数据  
    具体配置内容查看include/serialize.hrl  

* 生成:  
    maker pt 脚本名(_script后缀与不含扩展名), 也就是脚本具体功能的名字  
    maker protocol 生成全部  

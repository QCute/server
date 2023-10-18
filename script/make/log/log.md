# log script 使用

* 配置  
```erl
log() ->
    [
        #{file => "src/module/log/log.erl", table => table_name},
        #{file => "src/module/log/log_sql_save.erl", table => table_name},
        #{file => "src/module/log/log_sql_clean.erl", table => table_name},
        #{file => "src/module/log/log_sql_retain.erl", table => table_name},
    ].
```

* 生成:  
    maker log  

* 规则:  
    log.erl为调用接口  
    log_sql_save为保存数据sql  
    log_sql_clean为清除数据sql  
    log_sql_retain为转存数据sql  

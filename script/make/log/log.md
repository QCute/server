# log script 使用

* 配置  
```erl
log() ->
    [ ... ].
```

## 生成  
```shell
maker log  
```

## 文件  
* log.erl 调用接口  
* log_save 保存数据接口  
* log_delete 删除数据接口  
* log_delete_return 转存数据接口  
* log_replace 覆写数据接口  

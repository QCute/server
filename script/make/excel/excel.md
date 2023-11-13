## Excel 配置工具使用

#### 导出单个表, 包含表依赖
```shell
# 生成文件 comment.xlsm, 文件名是数据库 table 的注释
maker.sh sheet table

# 生成文件 excel/comment.xlsm, 文件名是数据库 table 的注释
maker.sh sheet table excel/
```

#### 导出多个表, 包含表依赖
> `file`参数是 [Erl](/script/make/erl/erl_script.erl)/[Lua](/script/make/lua/lua_script.erl)/[Js](/script/make/js/js_script.erl) 文件中的`file`参数(不包含文件路径)  
> 导出 [Erl](/script/make/erl/erl_script.erl)/[Lua](/script/make/lua/lua_script.erl)/[Js](/script/make/js/js_script.erl) 文件的配置生成结构中的`from`参数上的表  
```shell
# 生成文件 comment.xlsm, 文件名是配置生成结构的 comment 参数
maker.sh book file

# 生成文件 excel/comment.xlsm, 文件名是配置生成结构的 comment 参数
maker.sh book file excel/
```

#### 导出单个表
```shell
# 使用文件 comment.xlsm 导入到 table, table 是文件名 comment 对应数据库的表
maker.sh table comment.xlsm

# 使用文件 excel/comment.xlsm 导入到 table, table 是文件名 comment 对应数据库的表
maker.sh table excel/comment.xlsm
```

#### 导入多个表
```shell
# 使用文件 comment.xlsm 导入到 table, table2 ...,
maker.sh collection comment.xlsm

# 使用文件 excel/comment.xlsm 导入到 table, table2 ...
maker.sh collection excel/comment.xlsm
```

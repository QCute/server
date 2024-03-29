# sql script 使用

* 配置  
```erl
sql() ->
    [
        #{
            file => "",      %% 文件
            table => table,  %% 表名
            include => [],   %% 包含的头文件
            mode => []       %% 模式, 可选
        }
    ].
```

* 生成:  
    maker sql 文件名(不含扩展名)  

* 规则:  
    - 查询(select)  
        - 默认使用主键查询  
        - 可使用注释 (select_???) 设置查询组, 默认查询所有字段(*)数据  
        - 在模式中设置{select, []}, 查询全表  
    - 联查(join):  
        - 使用注释join_on(\`table_name\`.\`field\`)设置联查主键  
        - 使用注释join(\`table_name\`.\`field\`)设置联查字段  
        - 设置{select, []}模式, 联查全表  
    - 删除:  
        - 默认使用主键删除, 默认删除整行数据  
        - 使用注释(delete_???)可设置删除组, 相同名字一组,可设置多组  
    - 更新:  
        - 默认使用主键更新, 不包含(flag)指定/VIRTUAL和自增字段  
        - 使用注释(update_???)可设置更新组, 相同名字一组,可设置多组  
    - 插入:  
        - 插入数据, 不包含(flag)指定/VIRTUAL和自增字段  
        - 插入更新数据, 不包含(flag)指定/VIRTUAL和自增字段  
    - 清空:  
        - 在模式中设置{truncate, []}, 生成清空表代码  

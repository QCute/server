# protocol script 使用  

## 配置  
```erl
protocol() ->
    #protocol{
        number = 100,            %% 协议号
        comment = ...,           %% 注释
        erl = ...,               %% erl文件路径
        html = ...,              %% html文件路径
        lua = ...,               %% lua文件路径
        js = ...,                %% js文件路径
        cs = ...,                %% cs文件路径
        io = [                   %% 协议读写配置
            #io{
                number = ...,    %% 协议号
                comment = ...,   %% 注释
                read = [         %% 读取配置
                    ...
                ],
                write = [        %% 写入配置
                    ...
                ]
            }
        ]
    }.
```

## 例子 

### 读取  

1. 基本类型
```erl
                read = [
                    #bool{name = boolean, comment = "the boolean type"},    %% 布尔值
                    #u8{name = u8, comment = "the u8 type"},                %% 8位无符号
                    #u16{name = u16, comment = "the u16 type"},             %% 16位无符号
                    #u32{name = u32, comment = "the u32 type"},             %% 32位无符号
                    #u64{name = u64, comment = "the u64 type"},             %% 64位无符号
                    #i8{name = i8, comment = "the i8 type"},                %% 8位有符号
                    #i16{name = i16, comment = "the i16 type"},             %% 16位有符号
                    #i32{name = i32, comment = "the i32 type"},             %% 32位有符号
                    #i64{name = i64, comment = "the i64 type"},             %% 64位有符号
                    #f32{name = f32, comment = "the f32 type"},             %% 32位浮点数
                    #f64{name = f64, comment = "the f64 type"},             %% 64位浮点数
                    #str{name = str, comment = "the str type"},             %% 列表类型字符串
                    #bst{name = bst, comment = "the bst type"},             %% 二进制类型字符串
                ],
```

2. 使用元组
```erl
                read = [
                    #tuple{
                        name = tuple, 
                        explain = #item{
                            item_no = #u64{comment = "物品编号"},
                            item_id = #u32{comment = "物品ID"},
                            type = #u8{comment = "类型"},
                            number = #u16{comment = "数量"}
                        }
                    },
                    #item{
                        item_no = #u64{comment = "物品编号"},
                        item_id = #u32{comment = "物品ID"},
                        type = #u8{comment = "类型"},
                        number = #u16{comment = "数量"}
                    }
                ]
```

3. 使用记录
```erl
                read = [
                    #item{
                        item_no = #u64{comment = "物品编号"},
                        item_id = #u32{comment = "物品ID"},
                        type = #u8{comment = "类型"},
                        number = #u16{comment = "数量"}
                    }
                ]
```

4. 使用列表
```erl
                read = [
                    #list{
                        name = list, 
                        comment = "道具列表", 
                        explain = #item{
                            item_no = #u64{comment = "物品编号"},
                            item_id = #u32{comment = "物品ID"},
                            type = #u8{comment = "类型"},
                            number = #u16{comment = "数量"}
                        }
                    }
                ]
```

5. 使用[ETS]()
```erl
                read = [
                    #ets{
                        name = list, 
                        comment = "道具列表", 
                        explain = #item{
                            item_no = #u64{comment = "物品编号"},
                            item_id = #u32{comment = "物品ID"},
                            type = #u8{comment = "类型"},
                            number = #u16{comment = "数量"}
                        }
                    }
                ]
```

### 写入  

1. 基本类型
```erl
                write = [
                    #bool{name = boolean, comment = "the boolean type"},    %% 布尔值
                    #u8{name = u8, comment = "the u8 type"},                %% 8位无符号
                    #u16{name = u16, comment = "the u16 type"},             %% 16位无符号
                    #u32{name = u32, comment = "the u32 type"},             %% 32位无符号
                    #u64{name = u64, comment = "the u64 type"},             %% 64位无符号
                    #i8{name = i8, comment = "the i8 type"},                %% 8位有符号
                    #i16{name = i16, comment = "the i16 type"},             %% 16位有符号
                    #i32{name = i32, comment = "the i32 type"},             %% 32位有符号
                    #i64{name = i64, comment = "the i64 type"},             %% 64位有符号
                    #f32{name = f32, comment = "the f32 type"},             %% 32位浮点数
                    #f64{name = f64, comment = "the f64 type"},             %% 64位浮点数
                    #str{name = str, comment = "the str type"},             %% 列表类型字符串
                    #bst{name = bst, comment = "the bst type"},             %% 二进制类型字符串
                    #rst{name = rst, comment = "the rst type"},             %% 原子类型字符串
                ],
```

2. 使用元组
```erl
                write = [
                    #tuple{
                        name = tuple, 
                        explain = #item{
                            item_no = #u64{comment = "物品编号"},
                            item_id = #u32{comment = "物品ID"},
                            type = #u8{comment = "类型"},
                            number = #u16{comment = "数量"}
                        }
                    }
                ]
```

3. 使用记录
```erl
                write = [
                    #item{
                        item_no = #u64{comment = "物品编号"},
                        item_id = #u32{comment = "物品ID"},
                        type = #u8{comment = "类型"},
                        number = #u16{comment = "数量"}
                    }
                ]
```

4. 使用列表
```erl
                write = [
                    #list{
                        name = list, 
                        comment = "道具列表", 
                        explain = #item{
                            item_no = #u64{comment = "物品编号"},
                            item_id = #u32{comment = "物品ID"},
                            type = #u8{comment = "类型"},
                            number = #u16{comment = "数量"}
                        }
                    }
                ]
```

5. 使用[ETS]()
```erl
                write = [
                    #ets{
                        name = list, 
                        comment = "道具列表", 
                        explain = #item{
                            item_no = #u64{comment = "物品编号"},
                            item_id = #u32{comment = "物品ID"},
                            type = #u8{comment = "类型"},
                            number = #u16{comment = "数量"}
                        }
                    }
                ]
```

## 生成:  
```sh
maker pt 脚本名(_script后缀与不含扩展名), 也就是脚本具体功能的名字  
maker protocol 生成全部  
```


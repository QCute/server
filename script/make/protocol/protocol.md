## protocol script 使用  

#### 配置  
```erl
protocol() ->
    #protocol{
        number = 100,                      %% 协议簇号
        comment = ...,                     %% 注释
        erl = ...,                         %% erl文件路径
        html = ...,                        %% html文件路径
        lua = ...,                         %% lua文件路径
        js = ...,                          %% js文件路径
        cs = ...,                          %% cs文件路径
        io = [                             %% 协议编码解码配置
            #io{
                number = ...,              %% 协议号
                comment = ...,             %% 注释
                decode = ...,              %% 接收解码配置
                encode = ...,              %% 发送编码配置
            }
        ]
    }.
```

## 用法用例

#### 解码  

1. 单个值
```erl
%% json: true/false
decode = bool()                            %% 布尔值
```

2. 基本类型(使用元组)
```erl
%% json: { "binary": new Uint8Array([1, 2, 3, 4, 5, 6]), "bool": false, ... }
decode = {
    binary = binary(6),                    %% 固定长度二进制
    bool = bool(),                         %% 布尔值
    u8 = u8(),                             %% 8位无符号整数
    u16 = u16(),                           %% 16位无符号整数
    u32 = u32(),                           %% 32位无符号整数
    u64 = u64(),                           %% 64位无符号整数
    i8 = i8(),                             %% 8位有符号整数
    i16 = i16(),                           %% 16位有符号整数
    i32 = i32(),                           %% 32位有符号整数
    i64 = i64(),                           %% 64位有符号整数
    f32 = f32(),                           %% 32位浮点数
    f64 = f64(),                           %% 64位浮点数
    str = str(),                           %% 列表类型字符串
    bst = bst(),                           %% 二进制类型字符串
}
```

3. 使用记录
```erl
%% json: { "item_no": 1, "item_id": 2, "type": 3, "number": 4 }
decode = #item{
    item_no = u64(),                       %% 物品编号
    item_id = u32(),                       %% 物品ID
    type = u8(),                           %% 类型
    number = u16(),                        %% 数量
}
```

4. 使用映射表
```erl
%% json: { "item_no": 1, "item_id": 2, "type": 3, "number": 4 }
decode = #{
    item_no => u64(),                      %% 物品编号
    item_id => u32(),                      %% 物品ID
    type => u8(),                          %% 类型
    number => u16(),                       %% 数量
}
```

5. 使用列表
```erl
%% json: [ { "item_no": 1, "item_id": 2, "type": 3, "number": 4 }, ... ]
decode = [
    #item{
        item_no = u64(),                   %% 物品编号
        item_id = u32(),                   %% 物品ID
        type = u8(),                       %% 类型
        number = u16(),                    %% 数量
    }
]
```

6. 使用列表, 指定键名字, 对端生成映射表
```erl
%% json: { "1": { "item_no": 1, "item_id": 2, "type": 3, "number": 4 }, ... }
decode = [
    item_no = #item{                       %% 键名为物品编号
        item_no = u64(),                   %% 物品编号
        item_id = u32(),                   %% 物品ID
        type = u8(),                       %% 类型
        number = u16(),                    %% 数量
    }
]
```


#### 编码  

1. 单个值
```erl
%% json: "wow"
encode = bst()                             %% 二进制类型字符串
```

2. 基本类型(使用元组)
```erl
%% json: { "binary": new Uint8Array([1, 2, 3, 4, 5, 6]), "bool": false, ... }
encode = {
    binary = binary(6),                    %% 固定长度二进制
    bool = bool(),                         %% 布尔值
    u8 = u8(),                             %% 8位无符号整数
    u16 = u16(),                           %% 16位无符号整数
    u32 = u32(),                           %% 32位无符号整数
    u64 = u64(),                           %% 64位无符号整数
    i8 = i8(),                             %% 8位有符号整数
    i16 = i16(),                           %% 16位有符号整数
    i32 = i32(),                           %% 32位有符号整数
    i64 = i64(),                           %% 64位有符号整数
    f32 = f32(),                           %% 32位浮点数
    f64 = f64(),                           %% 64位浮点数
    str = str(),                           %% 列表类型字符串
    bst = bst(),                           %% 二进制类型字符串
    ast = ast(),                           %% 原子类型字符串
}
```

3. 使用记录
```erl
%% json: { "item_no": 1, "item_id": 2, "type": 3, "number": 4 }
encode = #item{
    item_no = u64(),                       %% 物品编号
    item_id = u32(),                       %% 物品ID
    type = u8(),                           %% 类型
    number = u16(),                        %% 数量
}
```

4. 使用映射表
```erl
%% json: { "item_no": 1, "item_id": 2, "type": 3, "number": 4 }
encode = #{
    item_no => u64(),                      %% 物品编号
    item_id => u32(),                      %% 物品ID
    type => u8(),                          %% 类型
    number => u16(),                       %% 数量
}
```

5. 使用列表
```erl
%% json: [ { "item_no": 1, "item_id": 2, "type": 3, "number": 4 }, ... ]
encode = [
    #item{
        item_no = u64(),                   %% 物品编号
        item_id = u32(),                   %% 物品ID
        type = u8(),                       %% 类型
        number = u16(),                    %% 数量
    }
]
```

6. 使用列表, 指定键名字, 对端生成映射表
```erl
%% json: { "1": { "item_no": 1, "item_id": 2, "type": 3, "number": 4 }, ... }
encode = [
    item_no = #item{                       %% 键名为物品编号
        item_no = u64(),                   %% 物品编号
        item_id = u32(),                   %% 物品ID
        type = u8(),                       %% 类型
        number = u16(),                    %% 数量
    }
]
```

7. 使用[ETS](), 对端生成列表
```erl
%% json: [ { "item_no": 1, "item_id": 2, "type": 3, "number": 4 }, ... ]
encode = [
    [] = #item{                            %% 键名为空
        item_no = u64(),                   %% 物品编号
        item_id = u32(),                   %% 物品ID
        type = u8(),                       %% 类型
        number = u16(),                    %% 数量
    }
]
```

8. 使用[ETS](), 指定键名字, 对端生成映射表
```erl
%% json: { "1": { "item_no": 1, "item_id": 2, "type": 3, "number": 4 }, ... }
encode = [
    [item_no] = #item{                     %% 键名为物品编号
        item_no = u64(),                   %% 物品编号
        item_id = u32(),                   %% 物品ID
        type = u8(),                       %% 类型
        number = u16(),                    %% 数量
    }
]
```

9. 使用`原子`自动转换成多语言(i18n)字符串, 翻译使用`text_data`所提供的数据
```
encode = ast()                             %% 结果
```

10. 使用`零字节`占位符忽略元组中不需要写入的元素
```
%% json: { "1": { "item_no": 1, "item_id": 2, "type": 3, }, ... }
encode = [
    {                                      %% 键名为物品编号
        item_no = u64(),                   %% 物品编号
        item_id = u32(),                   %% 物品ID
        type = u8(),                       %% 类型
        number = zero(),                   %% 数量 (忽略)
    }
]
```

## 生成:  
```sh
maker pt 脚本名(_script后缀与不含扩展名), 也就是脚本具体功能的名字  
maker protocol 生成全部  
```

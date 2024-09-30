# protocol script 使用  

## 配置  
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

## 例子 

### 解码  

1. 单个值
```erl
decode = bool()                            %% 布尔值
```

2. 基本类型(使用元组)
```erl
decode = {
    bin = binary(6),                       %% 固定长度二进制
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
decode = #item{
    item_no = u64(),                       %% 物品编号
    item_id = u32(),                       %% 物品ID
    type = u8(),                           %% 类型
    number = u16(),                        %% 数量
}
```

4. 使用映射表
```erl
decode = #{
    item_no => u64(),                      %% 物品编号
    item_id => u32(),                      %% 物品ID
    type => u8(),                          %% 类型
    number => u16(),                       %% 数量
}
```

5. 使用列表
```erl
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
decode = [
    item_no = #item{                       %% 键名为物品编号
        item_no = u64(),                   %% 物品编号
        item_id = u32(),                   %% 物品ID
        type = u8(),                       %% 类型
        number = u16(),                    %% 数量
    }
]
```


### 编码  

1. 单个值
```erl
encode = rst()                             %% 原子类型字符串
```

2. 基本类型(使用元组)
```erl
encode = {
    bin = binary(6),                       %% 固定长度二进制
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
    rst = rst(),                           %% 原子类型字符串
}
```

3. 使用记录
```erl
encode = #item{
    item_no = u64(),                       %% 物品编号
    item_id = u32(),                       %% 物品ID
    type = u8(),                           %% 类型
    number = u16(),                        %% 数量
}
```

4. 使用映射表
```erl
encode = #{
    item_no => u64(),                      %% 物品编号
    item_id => u32(),                      %% 物品ID
    type => u8(),                          %% 类型
    number => u16(),                       %% 数量
}
```

5. 使用列表
```erl
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
encode = [
    [item_no] = #item{                     %% 键名为物品编号
        item_no = u64(),                   %% 物品编号
        item_id = u32(),                   %% 物品ID
        type = u8(),                       %% 类型
        number = u16(),                    %% 数量
    }
]
```

## 生成:  
```sh
maker pt 脚本名(_script后缀与不含扩展名), 也就是脚本具体功能的名字  
maker protocol 生成全部  
```


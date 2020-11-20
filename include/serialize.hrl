%%%-------------------------------------------------------------------
%%% @doc
%%% serialize define
%%% @end
%%%-------------------------------------------------------------------

%% 协议配置 
-record(protocol, {
    number = 0,                                       %% 系列协议号
    comment = [],                                     %% 描述 
    includes = [],                                    %% 包含的头文件 
    io = [],                                          %% 读写配置
    erl = [],                                         %% erl文件路径
    js = [],                                          %% js文件路径
    lua = [],                                         %% lua文件路径
    handler = []                                      %% 处理协议文件
}).

%% 读写配置 
-record(io, {
    protocol = 0,                                     %% 协议号
    comment = [],                                     %% 描述
    read,                                             %% 读配置
    write,                                            %% 写配置
    handler                                           %% 处理协议配置
}).

%% 协议处理函数配置
-record(handler, {
    module,                                           %% 模块
    function,                                         %% 函数
    arg = user,                                       %% 进程状态参数名, 不使用设为空[]
    protocol = false,                                 %% 包含协议号
    alias = true                                      %% 生成协议别名宏定义(protocol.hrl), false不生成, true为函数名, 或者指定字符串
}).

%% 组合定义
-record(tuple,    {name = [], default = [], comment = [], explain = []}). %% 元组
-record(record,   {name = [], default = [], comment = [], explain = []}). %% 记录
-record(list,     {name = [], default = [], comment = [], explain = []}). %% 列表
-record(ets,      {name = [], default = [], comment = [], explain = []}). %% ETS

%% 单元定义
-record(binary,   {name = [], default = [], comment = [], explain = []}). %% 固定长度二进制
-record(rst,      {name = [], default = [], comment = [], explain = []}). %% 结果字符串(原子)
-record(bst,      {name = [], default = [], comment = [], explain = []}). %% 字符串(二进制)
-record(str,      {name = [], default = [], comment = [], explain = []}). %% 字符串(列表)
-record(bool,     {name = [], default = [], comment = [], explain = []}). %% 8   位(1/0)布尔值
-record(u128,     {name = [], default = [], comment = [], explain = []}). %% 128 位无符号整数
-record(u64,      {name = [], default = [], comment = [], explain = []}). %% 64  位无符号整数
-record(u32,      {name = [], default = [], comment = [], explain = []}). %% 32  位无符号整数
-record(u16,      {name = [], default = [], comment = [], explain = []}). %% 16  位无符号整数
-record(u8,       {name = [], default = [], comment = [], explain = []}). %% 8   位无符号整数
-record(i128,     {name = [], default = [], comment = [], explain = []}). %% 128 位有符号整数
-record(i64,      {name = [], default = [], comment = [], explain = []}). %% 64  位有符号整数
-record(i32,      {name = [], default = [], comment = [], explain = []}). %% 32  位有符号整数
-record(i16,      {name = [], default = [], comment = [], explain = []}). %% 16  位有符号整数
-record(i8,       {name = [], default = [], comment = [], explain = []}). %% 8   位无符号整数
-record(zero,     {}).                                                    %% 0   零字节占位符

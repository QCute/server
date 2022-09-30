%%%-------------------------------------------------------------------
%%% @doc
%%% serialize define
%%% @end
%%%-------------------------------------------------------------------

%% 协议配置
-record(protocol, {
    number = 0,                                       %% 系列协议号
    comment = [],                                     %% 描述
    handler = [],                                     %% handler文件路径
    includes = [],                                    %% 包含的头文件
    io = [],                                          %% 读写配置
    erl = [],                                         %% erl文件路径
    html = [],                                        %% html文件路径
    lua = [],                                         %% lua文件路径
    js = [],                                          %% js文件路径
    cs = []                                           %% cs文件路径
}).

%% 读写配置
-record(io, {
    protocol = 0,                                     %% 协议号
    interval = 0,                                     %% 协议时间间隔(毫秒)
    comment = [],                                     %% 描述
    handler,                                          %% 处理协议配置
    read,                                             %% 读配置
    write                                             %% 写配置
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
-record(tuple,    {name = [], default = [], comment = [], explain = []}). %% 元组, 使用explain描述元组具体信息
-record(maps,     {name = [], default = [], comment = [], explain = []}). %% 映射, 使用explain描述映射具体信息
-record(list,     {name = [], default = [], comment = [], explain = [], key}). %% 列表, 使用explain描述列表具体信息
-record(ets,      {name = [], default = [], comment = [], explain = [], key}). %% ETS, 使用explain描述ETS具体信息

%% 单元定义
-record(binary,   {name = [], default = [], comment = [], explain = []}). %% 固定长度二进制, 使用explain设置字节长度
-record(bool,     {name = [], default = [], comment = [], explain = []}). %% 8   位(1/0)与布尔值(true/false)读写互转
-record(u8,       {name = [], default = [], comment = [], explain = []}). %% 8   位无符号整数
-record(u16,      {name = [], default = [], comment = [], explain = []}). %% 16  位无符号整数
-record(u32,      {name = [], default = [], comment = [], explain = []}). %% 32  位无符号整数
-record(u64,      {name = [], default = [], comment = [], explain = []}). %% 64  位无符号整数
-record(u128,     {name = [], default = [], comment = [], explain = []}). %% 128 位无符号整数
-record(i8,       {name = [], default = [], comment = [], explain = []}). %% 8   位无符号整数
-record(i16,      {name = [], default = [], comment = [], explain = []}). %% 16  位有符号整数
-record(i32,      {name = [], default = [], comment = [], explain = []}). %% 32  位有符号整数
-record(i64,      {name = [], default = [], comment = [], explain = []}). %% 64  位有符号整数
-record(i128,     {name = [], default = [], comment = [], explain = []}). %% 128 位有符号整数
-record(f32,      {name = [], default = [], comment = [], explain = []}). %% 32  位有有符号浮点数
-record(f64,      {name = [], default = [], comment = [], explain = []}). %% 64  位有有符号浮点数
-record(rst,      {name = [], default = [], comment = [], explain = []}). %% 结果字符串(原子)(仅支持写)
-record(bst,      {name = [], default = [], comment = [], explain = []}). %% 字符串(二进制形式)
-record(str,      {name = [], default = [], comment = [], explain = []}). %% 字符串(列表形式)
-record(zero,     {name = [], default = [], comment = [], explain = []}). %% 0   零字节占位符

%% 对于读取时:
%%     如列表包含字符串, 使用binary固定长度二进制代替
%%
%% 对于写入时:
%%     可使用rst自动转换成多语言(i18n)字符串
%%     可使用零字节占位符忽略元组中不需要写入的元素

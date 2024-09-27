-compile(nowarn_unused_function).

%% 协议配置
-record(protocol, {
    number = 0,                                       %% 系列协议号
    comment = [],                                     %% 描述
    erl = [],                                         %% erl文件路径
    lua = [],                                         %% lua文件路径
    js = [],                                          %% js文件路径
    cs = [],                                          %% cs文件路径
    html = [],                                        %% html文件路径
    io = []                                           %% 读写配置
}).

%% 编码解码配置
-record(io, {
    number = 0,                                       %% 协议号
    comment = [],                                     %% 描述
    interval = 0,                                     %% 协议时间间隔(毫秒)
    handler,                                          %% 处理协议配置
    decode,                                           %% 接收解码配置
    encode                                            %% 发送编码配置
}).

%% 协议处理函数配置
-record(handler, {
    module,                                           %% 模块
    function,                                         %% 函数
    state = user,                                     %% 进程状态参数名
    alias = false,                                    %% true:使用模块名, false:使用函数名, 其他:使用指定值
    protocol = false,                                 %% 包含协议号
    response = buffer,                                %% 响应发送方式send:直接发送, buffer:缓存发送
    imp = user                                        %% sender名字
}).


%% 组合定义
-record(maps,     {name = [], comment = [], explain = []}). %% 映射, 使用explain描述映射具体信息
-record(list,     {name = [], comment = [], explain = [], key}). %% 列表, 使用explain描述列表具体信息
-record(ets,      {name = [], comment = [], explain = [], key}). %% ETS, 使用explain描述ETS具体信息
-record(record,   {name = [], comment = [], explain = []}). %% 记录, 使用explain描述元组具体信息
-record(tuple,    {name = [], comment = [], explain = []}). %% 元组, 使用explain描述元组具体信息

%% 单元定义
-record(binary,   {name = [], comment = [], explain = []}). %% 固定长度二进制, 使用explain设置字节长度
-record(bool,     {name = [], comment = [], explain = []}). %% 8   位(1/0)与布尔值(true/false)读写互转
-record(u8,       {name = [], comment = [], explain = []}). %% 8   位无符号整数
-record(u16,      {name = [], comment = [], explain = []}). %% 16  位无符号整数
-record(u32,      {name = [], comment = [], explain = []}). %% 32  位无符号整数
-record(u64,      {name = [], comment = [], explain = []}). %% 64  位无符号整数
-record(u128,     {name = [], comment = [], explain = []}). %% 128 位无符号整数
-record(i8,       {name = [], comment = [], explain = []}). %% 8   位无符号整数
-record(i16,      {name = [], comment = [], explain = []}). %% 16  位有符号整数
-record(i32,      {name = [], comment = [], explain = []}). %% 32  位有符号整数
-record(i64,      {name = [], comment = [], explain = []}). %% 64  位有符号整数
-record(i128,     {name = [], comment = [], explain = []}). %% 128 位有符号整数
-record(f32,      {name = [], comment = [], explain = []}). %% 32  位有有符号浮点数
-record(f64,      {name = [], comment = [], explain = []}). %% 64  位有有符号浮点数
-record(rst,      {name = [], comment = [], explain = []}). %% 结果字符串(原子)(仅支持写)
-record(bst,      {name = [], comment = [], explain = []}). %% 字符串(二进制形式)
-record(str,      {name = [], comment = [], explain = []}). %% 字符串(列表形式)
-record(zero,     {name = [], comment = [], explain = []}). %% 0   零字节占位符



zero() -> ok.
bin(Size) -> Size.
bool() -> ok.
u8() -> ok.
u16() -> ok.
u32() -> ok.
u64() -> ok.
i8() -> ok.
i16() -> ok.
i32() -> ok.
i64() -> ok.
f32() -> ok.
f64() -> ok.
str() -> ok.
bst() -> ok.


%% take encode row from epp:parse_file
%% take comment from erl_scan:string

%% erl record/maps encode/decode use single function prevent name conflict
%% js/cs use sub scope {} prevent name conflict

%% 对于读取时:
%%     如列表包含字符串, 使用binary固定长度二进制代替
%%
%% 对于写入时:
%%     可使用rst自动转换成多语言(i18n)字符串
%%     可使用零字节占位符忽略元组中不需要写入的元素

-define(IS_UNSIGNED(Unit), is_record(Unit, u8) orelse is_record(Unit, u16) orelse is_record(Unit, u32) orelse is_record(Unit, u64)).
-define(IS_SIGNED(Unit), is_record(Unit, i8) orelse is_record(Unit, i16) orelse is_record(Unit, i32) orelse is_record(Unit, i64)).
-define(IS_FLOAT(Unit), is_record(Unit, f32) orelse is_record(Unit, f64)).
-define(IS_STRING(Unit), is_record(Unit, str) orelse is_record(Unit, bst)).
-define(IS_SEQ(Unit), is_record(Unit, list) orelse is_record(Unit, tuple) orelse is_record(Unit, maps) orelse is_record(Unit, ets)).
-define(IS_UNIT(Unit), IS_UNSIGNED(Unit) orelse IS_SIGNED(Unit) orelse IS_FLOAT(Unit) orelse IS_STRING(Unit) orelse IS_SEQ(Unit)).

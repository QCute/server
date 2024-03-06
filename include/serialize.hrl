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
    response = buffer,                                %% 响应发送方式send:直接发送, buffer:缓存队列发送
    imp = user                                        %% sender名字
}).


%% 组合定义
% -record(maps,     {name = [], comment = [], explain = []}). %% 映射, 使用explain描述映射具体信息
% -record(list,     {name = [], comment = [], explain = [], key}). %% 列表, 使用explain描述列表具体信息
% -record(ets,      {name = [], comment = [], explain = [], key}). %% ETS, 使用explain描述ETS具体信息
% -record(record,   {name = [], comment = [], explain = []}). %% 记录, 使用explain描述元组具体信息
% -record(tuple,    {name = [], comment = [], explain = []}). %% 元组, 使用explain描述元组具体信息

% %% 单元定义
% -record(binary,   {name = [], comment = [], explain = []}). %% 固定长度二进制, 使用explain设置字节长度
% -record(bool,     {name = [], comment = [], explain = []}). %% 8   位(1/0)与布尔值(true/false)读写互转
% -record(u8,       {name = [], comment = [], explain = []}). %% 8   位无符号整数
% -record(u16,      {name = [], comment = [], explain = []}). %% 16  位无符号整数
% -record(u32,      {name = [], comment = [], explain = []}). %% 32  位无符号整数
% -record(u64,      {name = [], comment = [], explain = []}). %% 64  位无符号整数
% -record(u128,     {name = [], comment = [], explain = []}). %% 128 位无符号整数
% -record(i8,       {name = [], comment = [], explain = []}). %% 8   位无符号整数
% -record(i16,      {name = [], comment = [], explain = []}). %% 16  位有符号整数
% -record(i32,      {name = [], comment = [], explain = []}). %% 32  位有符号整数
% -record(i64,      {name = [], comment = [], explain = []}). %% 64  位有符号整数
% -record(i128,     {name = [], comment = [], explain = []}). %% 128 位有符号整数
% -record(f32,      {name = [], comment = [], explain = []}). %% 32  位有有符号浮点数
% -record(f64,      {name = [], comment = [], explain = []}). %% 64  位有有符号浮点数
% -record(rst,      {name = [], comment = [], explain = []}). %% 结果字符串(原子)(仅支持写)
% -record(bst,      {name = [], comment = [], explain = []}). %% 字符串(二进制形式)
% -record(str,      {name = [], comment = [], explain = []}). %% 字符串(列表形式)
% -record(zero,     {name = [], comment = [], explain = []}). %% 0   零字节占位符


%% ast metadata
-record(meta, {name = "", type, explain, comment = "", key}).
%% file
-record(file, {import = [], export = [], function = [], extra = []}).
%% language code set
-record(set, {code = #file{}, meta = #file{}, handler = #file{}}).
%% protocol data
-record(data, {protocol = 0, erl = #set{}, lua = #set{}, js = #set{}, cs = #set{}, html = #set{}}).

%% protocol meta macro
-define(IS_UNSIGNED(Meta), Meta == u8 orelse Meta == u16 orelse Meta == u32 orelse Meta == u64).
-define(IS_SIGNED(Meta), Meta == i8 orelse Meta == i16 orelse Meta == i32 orelse Meta == i64).
-define(IS_FLOAT(Meta), Meta == f32 orelse Meta == f64).
-define(IS_STRING(Meta), Meta == str orelse Meta == bst orelse Meta == rst).
-define(IS_OBJECT(Meta), (is_tuple(Meta) andalso tuple_size(Meta) > 0 andalso is_atom(element(1, Meta) andalso is_record(Meta, element(1, Meta)))) orelse is_map(Meta)).
-define(IS_LIST(Meta), is_list(Meta)).
-define(IS_UNIT(Meta), Meta == zero orelse Meta == bin orelse Meta == bool orelse ?IS_UNSIGNED(Meta) orelse ?IS_SIGNED(Meta) orelse ?IS_FLOAT(Meta) orelse ?IS_STRING(Meta)).
-define(IS_META(Meta), ?IS_UNIT(Meta) orelse ?IS_OBJECT(Meta) orelse ?IS_LIST(Meta)).

%% protocol meta unit
zero() -> zero.
binary(Size) -> Size.
bool() -> bool.
u8() -> u8.
u16() -> u16.
u32() -> u32.
u64() -> u64.
i8() -> i8.
i16() -> i16.
i32() -> i32.
i64() -> i64.
f32() -> f32.
f64() -> f64.
str() -> str.
bst() -> bst.
rst() -> rst.

%% protocol meta
zero(Name, Data, Comment) -> #meta{name = Name, type = ?FUNCTION_NAME, explain = Data, comment = Comment}.
binary(Name, Data, Comment) -> #meta{name = Name, type = ?FUNCTION_NAME, explain = Data, comment = Comment}.
bool(Name, Data, Comment) -> #meta{name = Name, type = ?FUNCTION_NAME, explain = Data, comment = Comment}.
u8(Name, Data, Comment) -> #meta{name = Name, type = ?FUNCTION_NAME, explain = Data, comment = Comment}.
u16(Name, Data, Comment) -> #meta{name = Name, type = ?FUNCTION_NAME, explain = Data, comment = Comment}.
u32(Name, Data, Comment) -> #meta{name = Name, type = ?FUNCTION_NAME, explain = Data, comment = Comment}.
u64(Name, Data, Comment) -> #meta{name = Name, type = ?FUNCTION_NAME, explain = Data, comment = Comment}.
i8(Name, Data, Comment) -> #meta{name = Name, type = ?FUNCTION_NAME, explain = Data, comment = Comment}.
i16(Name, Data, Comment) -> #meta{name = Name, type = ?FUNCTION_NAME, explain = Data, comment = Comment}.
i32(Name, Data, Comment) -> #meta{name = Name, type = ?FUNCTION_NAME, explain = Data, comment = Comment}.
i64(Name, Data, Comment) -> #meta{name = Name, type = ?FUNCTION_NAME, explain = Data, comment = Comment}.
f32(Name, Data, Comment) -> #meta{name = Name, type = ?FUNCTION_NAME, explain = Data, comment = Comment}.
f64(Name, Data, Comment) -> #meta{name = Name, type = ?FUNCTION_NAME, explain = Data, comment = Comment}.
str(Name, Data, Comment) -> #meta{name = Name, type = ?FUNCTION_NAME, explain = Data, comment = Comment}.
bst(Name, Data, Comment) -> #meta{name = Name, type = ?FUNCTION_NAME, explain = Data, comment = Comment}.
rst(Name, Data, Comment) -> #meta{name = Name, type = ?FUNCTION_NAME, explain = Data, comment = Comment}.

%% { name = ... }
'$tuple$'(Name, Data, Comment) -> #meta{name = Name, type = list_to_atom(string:trim(atom_to_list(?FUNCTION_NAME), both, "$")), explain = Data, comment = Comment}.
%% #record{ name = ... }
'$record$'(Name, Data, Comment) -> #meta{name = Name, type = list_to_atom(string:trim(atom_to_list(?FUNCTION_NAME), both, "$")), explain = Data, comment = Comment}.
%% #{ name => ... }
'$maps$'(Name, Data, Comment) -> #meta{name = Name, type = list_to_atom(string:trim(atom_to_list(?FUNCTION_NAME), both, "$")), explain = Data, comment = Comment}.
%% [ ... ] / [ name = ... ]
%% key:
%%     undefined => list
%%     atom => key value pair
'$list$'(Name, Key, Data, Comment) -> #meta{name = Name, type = list_to_atom(string:trim(atom_to_list(?FUNCTION_NAME), both, "$")), explain = Data, comment = Comment, key = Key}.
%% ets:[ ... ] / ets:[ [name] = ... ]
%% key:
%%     [] => ets
%%     [atom] => ets key value pair
'$ets$'(Name, Key, Data, Comment) -> #meta{name = Name, type = list_to_atom(string:trim(atom_to_list(?FUNCTION_NAME), both, "$")), explain = Data, comment = Comment, key = Key}.

%% @todo
%% erl record/maps encode/decode use single function prevent name conflict
%% js/cs use sub scope {} prevent name conflict
%% @todo
%% lua/js/cs meta read/write
%% @todo
%% html ui beautify


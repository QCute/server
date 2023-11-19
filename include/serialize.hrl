-ifndef(SERIALIZE_HRL).
-define(SERIALIZE_HRL, 'SERIALIZE_HRL').
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

%% ast metadata
-record(meta, {name = "", from = "", path = [], prefix = [], suffix = [], get = [], tag, type, explain, comment = "", key}).
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
-define(IS_STRING(Meta), Meta == str orelse Meta == bst orelse Meta == ast).
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
ast() -> ast.

%% protocol meta
zero(Name, From, Path, Prefix, Suffix, Get, Data, Comment) -> #meta{name = Name, from = From, path = Path, prefix = Prefix, suffix = Suffix, get = Get, type = ?FUNCTION_NAME, explain = Data, comment = Comment}.
binary(Name, From, Path, Prefix, Suffix, Get, Data, Comment) -> #meta{name = Name, from = From, path = Path, prefix = Prefix, suffix = Suffix, get = Get, type = ?FUNCTION_NAME, explain = Data, comment = Comment}.
bool(Name, From, Path, Prefix, Suffix, Get, Data, Comment) -> #meta{name = Name, from = From, path = Path, prefix = Prefix, suffix = Suffix, get = Get, type = ?FUNCTION_NAME, explain = Data, comment = Comment}.
u8(Name, From, Path, Prefix, Suffix, Get, Data, Comment) -> #meta{name = Name, from = From, path = Path, prefix = Prefix, suffix = Suffix, get = Get, type = ?FUNCTION_NAME, explain = Data, comment = Comment}.
u16(Name, From, Path, Prefix, Suffix, Get, Data, Comment) -> #meta{name = Name, from = From, path = Path, prefix = Prefix, suffix = Suffix, get = Get, type = ?FUNCTION_NAME, explain = Data, comment = Comment}.
u32(Name, From, Path, Prefix, Suffix, Get, Data, Comment) -> #meta{name = Name, from = From, path = Path, prefix = Prefix, suffix = Suffix, get = Get, type = ?FUNCTION_NAME, explain = Data, comment = Comment}.
u64(Name, From, Path, Prefix, Suffix, Get, Data, Comment) -> #meta{name = Name, from = From, path = Path, prefix = Prefix, suffix = Suffix, get = Get, type = ?FUNCTION_NAME, explain = Data, comment = Comment}.
i8(Name, From, Path, Prefix, Suffix, Get, Data, Comment) -> #meta{name = Name, from = From, path = Path, prefix = Prefix, suffix = Suffix, get = Get, type = ?FUNCTION_NAME, explain = Data, comment = Comment}.
i16(Name, From, Path, Prefix, Suffix, Get, Data, Comment) -> #meta{name = Name, from = From, path = Path, prefix = Prefix, suffix = Suffix, get = Get, type = ?FUNCTION_NAME, explain = Data, comment = Comment}.
i32(Name, From, Path, Prefix, Suffix, Get, Data, Comment) -> #meta{name = Name, from = From, path = Path, prefix = Prefix, suffix = Suffix, get = Get, type = ?FUNCTION_NAME, explain = Data, comment = Comment}.
i64(Name, From, Path, Prefix, Suffix, Get, Data, Comment) -> #meta{name = Name, from = From, path = Path, prefix = Prefix, suffix = Suffix, get = Get, type = ?FUNCTION_NAME, explain = Data, comment = Comment}.
f32(Name, From, Path, Prefix, Suffix, Get, Data, Comment) -> #meta{name = Name, from = From, path = Path, prefix = Prefix, suffix = Suffix, get = Get, type = ?FUNCTION_NAME, explain = Data, comment = Comment}.
f64(Name, From, Path, Prefix, Suffix, Get, Data, Comment) -> #meta{name = Name, from = From, path = Path, prefix = Prefix, suffix = Suffix, get = Get, type = ?FUNCTION_NAME, explain = Data, comment = Comment}.
str(Name, From, Path, Prefix, Suffix, Get, Data, Comment) -> #meta{name = Name, from = From, path = Path, prefix = Prefix, suffix = Suffix, get = Get, type = ?FUNCTION_NAME, explain = Data, comment = Comment}.
bst(Name, From, Path, Prefix, Suffix, Get, Data, Comment) -> #meta{name = Name, from = From, path = Path, prefix = Prefix, suffix = Suffix, get = Get, type = ?FUNCTION_NAME, explain = Data, comment = Comment}.
ast(Name, From, Path, Prefix, Suffix, Get, Data, Comment) -> #meta{name = Name, from = From, path = Path, prefix = Prefix, suffix = Suffix, get = Get, type = ?FUNCTION_NAME, explain = Data, comment = Comment}.

%% { name = ... }
'$tuple$'(Name, From, Path, Prefix, Suffix, Get, Data, Comment) -> #meta{name = Name, from = From, path = Path, prefix = Prefix, suffix = Suffix, get = Get, type = list_to_atom(string:trim(atom_to_list(?FUNCTION_NAME), both, "$")), explain = Data, comment = Comment}.
%% #record{ name = ... }
'$record$'(Name, From, Path, Prefix, Suffix, Get, Tag, Data, Comment) -> #meta{name = Name, from = From, path = Path, tag = Tag, prefix = Prefix, suffix = Suffix, get = Get, type = list_to_atom(string:trim(atom_to_list(?FUNCTION_NAME), both, "$")), explain = Data, comment = Comment}.
%% #{ name => ... }
'$maps$'(Name, From, Path, Prefix, Suffix, Get, Data, Comment) -> #meta{name = Name, from = From, path = Path, prefix = Prefix, suffix = Suffix, get = Get, type = list_to_atom(string:trim(atom_to_list(?FUNCTION_NAME), both, "$")), explain = Data, comment = Comment}.
%% [ ... ] / [ name = ... ]
%% key:
%%     undefined => list
%%     atom => key value pair
'$list$'(Name, From, Path, Prefix, Suffix, Get, Key, Data, Comment) -> #meta{name = Name, from = From, path = Path, prefix = Prefix, suffix = Suffix, get = Get, type = list_to_atom(string:trim(atom_to_list(?FUNCTION_NAME), both, "$")), explain = Data, comment = Comment, key = Key}.
%% ets:[ ... ] / ets:[ [name] = ... ]
%% key:
%%     [] => ets
%%     [atom] => ets key value pair
'$ets$'(Name, From, Path, Prefix, Suffix, Get, Key, Data, Comment) -> #meta{name = Name, from = From, path = Path, prefix = Prefix, suffix = Suffix, get = Get, type = list_to_atom(string:trim(atom_to_list(?FUNCTION_NAME), both, "$")), explain = Data, comment = Comment, key = Key}.

-define(ANONYMOUS_DATA_NAME, data).

-endif.
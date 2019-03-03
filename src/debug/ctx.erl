%%%-------------------------------------------------------------------
%%% @doc
%%% module ctx (context)
%%% @end
%%%-------------------------------------------------------------------
-module(ctx).
-compile(nowarn_export_all).
-compile(nowarn_deprecated_function).
-compile(export_all).
-include("../../include/player.hrl").
%% API
%% @doc for e script
main(_) ->
    code:add_path("beam"),
    code:add_path("../beam"),
    code:add_path("../../beam"),
    code:add_path("../../../beam"),
    %io:format("~p~n", [word:sensitive("官方")]),

    io:format("~ts~n", [ts()]),

    ok.


c() ->
    {ok, S} = gen_tcp:connect("127.0.0.1", 10000, []),
    %% data length(16) protocol(16) data part
    gen_tcp:send(S, <<9:16, 10001:16, 1:16, 1:16, 49:8>>),
    put(s, S),
    S.

close() ->
    gen_tcp:close(get(s)).


%% test
rt() ->
    List = [{1, 3, 0}, {2, 100, 0}, {3, 1000, 0}],
    NewUser = item:add(item:load(#user{id = 1}), List),
    io:format("~p~n", [NewUser]),
    ok.


%% test
t() ->
    S = sorter:new(ssr, local, replace, infinity, 1, 2, 3, undefined, []),
    N = sorter:update({1,2,3}, S),
    M = sorter:update({2,3,4}, N),
    L = sorter:update({3,4,5}, M),
    timer:sleep(2000),
    io:format("~p~n", [sorter:update({3,6,9}, L)]),
    ok.

%% test
tt() ->
    S = sorter:new(ssr, share, replace, infinity, 1, 2, 3, undefined, []),
    sorter:update({1,2,3}, S),
    timer:sleep(2000),
    io:format("~p~n", [ets:tab2list(ssr)]),
    ok.

%% test
ttt() ->
    S = sorter:new(wow, global, replace, infinity, 1, 2, 3, undefined, []),
    sorter:update({1,2,3}, S),
    timer:sleep(2000),
    io:format("~p~n", [ets:tab2list(wow)]),
    ok.


tts() ->
    ok.


%% not tail recursive function
append([H|T], Tail) ->
    [H|append(T, Tail)];
append([], Tail) ->
    Tail.


%% @doc file encoding test
tss() ->
    "一".
ts() ->
    case "一" of
        [14989440] ->
            utf8;
        [228, 184, 128] ->
            utf8;
        [19968] ->
            unicode;
        [78, 0] ->
            unicode;
        [53947] ->
            gbk;
        [210, 187] ->
            gbk
    end.

%% 一
%% <<228,184,128>>  .utf8      228*256*256 + 184*256 + 128   [14989440]
%% <<78,0>>         .unicode   78*256 + 0                    [19968]
%% <<210,187>>      .gbk       210*256+187                   [53947]


map_reduce(F, L) ->
    Parent = self(),
    [spawn(fun() -> catch Parent ! F(I) end) || I <- L],
    [receive R -> R end || _ <- L].


script_path() ->
    Name = lists:reverse(escript:script_name()),
    lists:reverse(trim_path(Name, [])) ++ "../../../".
trim_path([], List) ->
    List;
trim_path([$\\ | _] = List, _) ->
    List;
trim_path([$/ | _] = List, _) ->
    List;
trim_path([H | T], List) ->
    trim_path(T, [H | List]).



%%%===================================================================
%%% code assist
%%%===================================================================
list(Table) ->
    list('game', Table).
list(DataBase, Table) ->
    FieldsSql = io_lib:format(<<"SELECT `COLUMN_NAME`, `COLUMN_DEFAULT`, `DATA_TYPE`, `COLUMN_COMMENT`, `ORDINAL_POSITION`, `COLUMN_KEY`, `EXTRA` FROM information_schema.`COLUMNS` WHERE `TABLE_SCHEMA` = '~s' AND `TABLE_NAME` = '~s'">>, [DataBase, Table]),
    Fields = sql:select(FieldsSql),
    string:join([binary_to_list(Name) || [Name, _, _, _, _, _, _] <- Fields], ", ").

%% @doc fields to hump name
hump(Table) ->
    hump('game', Table).
hump(DataBase, Table) ->
    FieldsSql = io_lib:format(<<"SELECT `COLUMN_NAME`, `COLUMN_DEFAULT`, `DATA_TYPE`, `COLUMN_COMMENT`, `ORDINAL_POSITION`, `COLUMN_KEY`, `EXTRA` FROM information_schema.`COLUMNS` WHERE `TABLE_SCHEMA` = '~s' AND TABLE_NAME = '~s' ORDER BY ORDINAL_POSITION;">>, [DataBase, Table]),
    Fields = sql:select(FieldsSql),
    F = fun(Name) -> lists:concat([[case 96 < H andalso H < 123 of true -> H - 32; _ -> H end | T] || [H | T] <- string:tokens(Name, "_")]) end,
    string:join([F(binary_to_list(Name)) || [Name, _, _, _, _, _, _] <- Fields], ", ").

%% @doc code construct
make(Table) ->
    make('game', Table).
make(DataBase, Table) ->
    FieldsSql = io_lib:format(<<"SELECT `COLUMN_NAME`, `COLUMN_DEFAULT`, `DATA_TYPE`, `COLUMN_COMMENT`, `ORDINAL_POSITION`, `COLUMN_KEY`, `EXTRA` FROM information_schema.`COLUMNS` WHERE `TABLE_SCHEMA` = '~s' AND TABLE_NAME = '~s' ORDER BY ORDINAL_POSITION;">>, [DataBase, Table]),
    Fields = sql:select(FieldsSql),
    F = fun(Name) -> lists:concat([[case 96 < H andalso H < 123 of true -> H - 32; _ -> H end | T] || [H | T] <- string:tokens(Name, "_")]) end,
    Args = string:join([F(binary_to_list(Name)) || [Name, _, _, _, _, _, _] <- Fields], ", "),
    Fill = string:join([lists:concat(["        ", binary_to_list(Name), " = ", F(binary_to_list(Name))]) || [Name, _, _, _, _, _, _] <- Fields], ",\n"),
    Code = lists:concat(["make_", Table, "(", Args, ") ->\n    #", Table, "{\n", Fill, "\n    }."]),
    io:format("~s~n", [Code]).

%%%===================================================================
%%% regexp
%%%===================================================================
%% match record(multi line)
%% 跨行匹配左边不接非空白字符，名字开头，后接以.结尾或者后面是注释%的记录
%% (?s)(?<!\\S)(-record\\(~s\\s*,.+?)(?=\\.$|\\%)\\.

%% function(multi line)
%% 跨行匹配左边不接非空白字符，名字开头，后接以.结尾或者后面是注释%的函数
%% (?s)(?<!\\S)(~s.+?)(?=\\.$|\\%)\\.

%% define(single line)
%% 跨行匹配左边不接非空白字符，名字开头，后接以.结尾或者后面是注释%的定义
%% (?<!\\S)(-define\\s*\\(~s.+?)(?=\\.$|\\%)\\.

%% all include(single line)
%% 跨行匹配左边不接非空白字符，名字开头，后接以.结尾或者后面是注释%的依赖
%% (?<!\\S)(-include\\s*\\(~s\\s*\\.+?)(?=\\.$|\\%)\\.
%% 匹配所有include
%% (?<!\\S)(-include.+?)(?=\\.$|\\%)\\.

%% 匹配record
%% (?m)(?s)^-record\\(~s\s*,\s*\\{.+?^((?!%).)*?\\}\s*\\)\\.(?=$|\s|%)
%% 匹配函数
%% (?m)(?s)^~s\(.*?\)\s*->.+?^((?!%).)*?\.(?=$|\s|%)



%%%===================================================================
%%% back plant
%%%===================================================================
%% user data
%% log data
%% config data
%% statistic(active/charge/use new or lost)
%% user manager(mail/forbid/login/chat)
%% tool(config data hot load)
%% admin(user/privileges)

%%%===================================================================
%%% architecture plant
%%%===================================================================
%%IO(ok)
%%数据(ok)
%%协议
%%集群(ok)
%%通用工具(ok)
%%错误日志(ok)
%%构造器(敏感词/表到记录/表到sql/表到日志/表到数据/表到excel/协议)(ok)
%%
%%日志(模块数据)(ok)
%%背包(物品)
%%帮派(guild_handle,guild_server,guild)
%%任务(quest_handle,quest_check,quest)
%%好友()
%%聊天
%%商店
%%邮件
%%排行
%%公告
%%支付
%%敏感词(ok)

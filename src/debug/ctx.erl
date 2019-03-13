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

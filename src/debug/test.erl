%%%-------------------------------------------------------------------
%%% @doc
%%% module test
%%% @end
%%%-------------------------------------------------------------------
-module(test).
-compile(nowarn_export_all).
-compile(nowarn_deprecated_function).
-compile(export_all).
-include("../../include/assets.hrl").
-include("../../include/common.hrl").
-include("../../include/ets.hrl").
-include("../../include/event.hrl").
-include("../../include/extra.hrl").
-include("../../include/fashion.hrl").
-include("../../include/guild.hrl").
-include("../../include/item.hrl").
-include("../../include/key.hrl").
-include("../../include/mail.hrl").
-include("../../include/notice.hrl").
-include("../../include/player.hrl").
-include("../../include/protocol.hrl").
-include("../../include/quest.hrl").
-include("../../include/rank.hrl").
-include("../../include/record.hrl").
-include("../../include/serialize.hrl").
-include("../../include/socket.hrl").
-include("../../include/sorter.hrl").
-include("../../include/table.hrl").
-include("../../include/vip.hrl").


gr() ->
    [X || X <- erlang:registered(), string:str(lists:concat([X]), "receiver") =/= 0].

gs(A) ->sys:get_state(erlang:whereis(A)).

%% API
main(Args) ->
    io:format("~p~n", [Args]).

g() ->
    [
        sql:insert(io_lib:format(
            "insert into `rank` (`type`, `key`, `value`, `time`, `rank`, `name`, `other`) VALUES ('1', '~w', '1', '~w', '~w', '', '')",
            [X, X, X]))
        ||
    X <- lists:seq(1000, 10000)].



test() ->
    Begin = os:timestamp(),
    %% first
    L = tl(),
    Middle = os:timestamp(),
    %% second
    End = os:timestamp(),
    First = timer:now_diff(Middle, Begin) div 1000,
    Second = timer:now_diff(End, Middle) div 1000,
    io:format("First:~p   Second:~p ~p~n", [First, Second, length(L)]),
    ok.

tl() ->
    Data = rank_server:rank(1),
    rank_sql:update_into([X#rank{flag = update} || X <- Data]).


c() ->
    {ok, S} = gen_tcp:connect("127.0.0.1", 10000, []),
    %% data length(16) protocol(16) data part
    gen_tcp:send(S, <<9:16, 10001:16, 1:16, 1:16, 49:8>>),
    gen_tcp:close(S),
    put(s, S),
    S.

close() ->
    gen_tcp:close(get(s)).


rl() ->
    U = item:load(#user{id = 1}),
    io:format("~p~n", [U]).

%% test
rt() ->
    List = [{1, 3, 0},{2, 5, 0}, {3, 25, 0}],
    NewUser = item:add(item:load(#user{id = 1, player = #player{bag_size = 10, item_size = 10}}), List),
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
%%构造器(敏感词/表到记录/表到sql/表到日志/表到数据/表到lua/表到excel/协议)(ok)
%%
%%日志(模块数据)(ok)
%%背包(item, bag, store)(ok)
%%帮派(guild_handle,guild_server,guild)
%%任务(quest_handle,quest_check,quest)(ok)
%%好友
%%商店
%%聊天(ok)
%%邮件(ok)
%%公告(ok)
%%排行(ok)
%%敏感词(ok)
%%兑换码(ok)
%%活动
%%支付

%% 属性

%% 技能
%% buff
%% 地图
%% 战场
%% 副本

%%%===================================================================
%%% important
%%%===================================================================
%% 战斗
%% 攻击者使用技能对作用半径内防守者(一个或多个)发起攻击
%% 如果命中
%% 计算伤害(基本属性伤害),计算被动技能
%% 更新对象
%% 过滤作用距离外对象,计算被动技能,
%%%===================================================================

%%%===================================================================
%%% important
%%%===================================================================
%% 怪物AI

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
-include("../../include/user.hrl").
-include("../../include/vip.hrl").


t(T) -> catch ets:tab2list(T).

s(A) ->sys:get_state(erlang:whereis(A)).

%% API
main(Args) ->
    io:format("~p~n", [Args]).



t() ->
    U = player_login:login(#user{id = 1}),
    P = player_route:write(?CMD_PLAYER, [U#user.player]),
    PA = player_route:write(?CMD_PLAYER_ASSETS, [U#user.assets]),
    ITEM = player_route:write(?CMD_ITEM, [U#user.item]),
    MAIL = player_route:write(?CMD_MAIL, [U#user.mail]),
    QUEST = player_route:write(?CMD_QUEST, [U#user.quest]),
    SHOP = player_route:write(?CMD_SHOP, [U#user.shop]),

    CHAT = player_route:write(?CMD_CHAT_WORLD, [1, <<"1">>, <<"1">>]),
    RANK = player_route:write(?CMD_RANK, [rank_server:rank(1)]),

    io:format("~p~n", [U]),
    [io:format("~p~n", [element(1, X)]) || X <- [P, PA, ITEM, MAIL, QUEST, SHOP, CHAT, RANK]],
    ok.

r() ->
    [X || X <- erlang:registered(), string:str(atom_to_list(X), "receiver") =/= 0].


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
%%商店(ok)
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

%%%-------------------------------------------------------------------
%%% @doc
%%% module test
%%% @end
%%%-------------------------------------------------------------------
-module(test).
-compile(nowarn_export_all).
-compile(nowarn_deprecated_function).
-compile(export_all).
-include("../../include/account.hrl").
-include("../../include/asset.hrl").
-include("../../include/attribute.hrl").
-include("../../include/common.hrl").
-include("../../include/ets.hrl").
-include("../../include/event.hrl").
-include("../../include/extra.hrl").
-include("../../include/fashion.hrl").
-include("../../include/friend.hrl").
-include("../../include/guild.hrl").
-include("../../include/item.hrl").
-include("../../include/key.hrl").
-include("../../include/mail.hrl").
-include("../../include/map.hrl").
-include("../../include/monster.hrl").
-include("../../include/notice.hrl").
-include("../../include/online.hrl").
-include("../../include/protocol.hrl").
-include("../../include/quest.hrl").
-include("../../include/rank.hrl").
-include("../../include/record.hrl").
-include("../../include/role.hrl").
-include("../../include/serialize.hrl").
-include("../../include/shop.hrl").
-include("../../include/socket.hrl").
-include("../../include/sorter.hrl").
-include("../../include/table.hrl").
-include("../../include/user.hrl").
-include("../../include/vip.hrl").

t(T) -> catch ets:tab2list(T).

s(A) ->sys:get_state(erlang:whereis(A)).

ms() -> s(map_100000).

%% API
main(Args) ->
    io:format("~p~n", [Args]).

%% channel/server/servo
%%
%% 1000100000000000

t() ->
    U = user_loader:load(#user{role_id = 1}),
    R = user_router:write(?PROTOCOL_ROLE, [U#user.role]),
    ASSETS = user_router:write(?PROTOCOL_ASSET, [U#user.asset]),
    ITEM = user_router:write(?PROTOCOL_ITEM, [U#user.item]),
    MAIL = user_router:write(?PROTOCOL_MAIL, [U#user.mail]),
    QUEST = user_router:write(?PROTOCOL_QUEST, [U#user.quest]),
    SHOP = user_router:write(?PROTOCOL_SHOP, [U#user.shop]),
    FRIEND = user_router:write(?PROTOCOL_FRIEND, [U#user.friend]),
    CHAT = user_router:write(?PROTOCOL_CHAT_WORLD, [1, <<"1">>, <<"1">>]),
    RANK = user_router:write(?PROTOCOL_RANK, [rank_server:rank(1)]),

    io:format("~p~n", [U]),
    [io:format("~p~n", [element(1, X)]) || X <- [R, ASSETS, ITEM, MAIL, QUEST, SHOP, FRIEND, CHAT, RANK]],
    ok.

r() ->
    [X || X <- erlang:registered(), string:str(atom_to_list(X), "receiver") =/= 0].


hp(Old, New) ->
    HPLevel = (Old div 10),
    case (HPLevel) =/= New div 10 of
        true ->
            hp(HPLevel);
        false ->
            ok
    end.

hp(1) ->
    1;
hp(2) ->
    1;
hp(3) ->
    2;
hp(4) ->
    3;
hp(5) ->
    4;
hp(6) ->
    5;
hp(7) ->
    6;
hp(8) ->
    7;
hp(9) ->
    8;
hp(10) ->
    9;
hp(_) ->
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
%% (?m)(?s)^-record\\(~s\\s*,\\s*\\{.+?^((?!%).)*?\\}\s*\\)\\.(?=$|\s|%)
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
%% 攻击者使用技能对作用半径内敌人(一个或多个)发起攻击
%% 如果命中
%% 计算伤害(基本属性伤害),计算被动技能
%% 计算技能Buff
%% 更新对象

%%%===================================================================

%%%===================================================================
%%% important
%%%===================================================================
%% 怪物AI (通过类型和目标对象组合得来)
%% 类型           |   目标对象
%% 固定(fix)      |   敌人(enemy)
%% 移动(move)     |   玩家(fighter)
%% 主动(active)   |   怪物(monster)
%% 被动(passive)  |   指定类型怪物(monster, id)

%%
%% type   : fix move active passive
%% act_script : enemy fighter monster {monster, id} location
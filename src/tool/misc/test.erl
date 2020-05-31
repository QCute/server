%%%-------------------------------------------------------------------
%%% @doc
%%% module test
%%% @end
%%%-------------------------------------------------------------------
-module(test).
-compile(nowarn_export_all).
-compile(export_all).
-include("../../../include/activity.hrl").
-include("../../../include/asset.hrl").
-include("../../../include/attribute.hrl").
-include("../../../include/auction.hrl").
-include("../../../include/boss.hrl").
-include("../../../include/buff.hrl").
-include("../../../include/common.hrl").
-include("../../../include/count.hrl").
-include("../../../include/dungeon.hrl").
-include("../../../include/effect.hrl").
-include("../../../include/event.hrl").
-include("../../../include/friend.hrl").
-include("../../../include/guild.hrl").
-include("../../../include/item.hrl").
-include("../../../include/key.hrl").
-include("../../../include/lucky_money.hrl").
-include("../../../include/mail.hrl").
-include("../../../include/map.hrl").
-include("../../../include/monster.hrl").
-include("../../../include/notice.hrl").
-include("../../../include/online.hrl").
-include("../../../include/protocol.hrl").
-include("../../../include/quest.hrl").
-include("../../../include/rank.hrl").
-include("../../../include/recharge.hrl").
-include("../../../include/role.hrl").
-include("../../../include/serialize.hrl").
-include("../../../include/shop.hrl").
-include("../../../include/skill.hrl").
-include("../../../include/socket.hrl").
-include("../../../include/sorter.hrl").
-include("../../../include/title.hrl").
-include("../../../include/user.hrl").
-include("../../../include/vip.hrl").


%%%===================================================================
%%% API functions
%%%===================================================================
%% @doc escript entry
main(Env) ->
    catch code:add_path(filename:dirname(escript:script_name()) ++ "/../../../beam/"),
    io:format("Env: ~p~n", [Env]).

%% process state
s(A) -> sys:get_state(erlang:whereis(A)).

t(T) -> ets:tab2list(T).

%% list processes
ls() ->
    [io:format("~w~s~w~n", [X, lists:duplicate(32 - length(pid_to_list(X)), " "), erlang:process_info(X, registered_name)]) || X <- lists:sort(erlang:processes())],
    ok.

lsp() ->
    [io:format("~w~s~w~n", [X, lists:duplicate(32 - length(atom_to_list(X)), " "), erlang:whereis(X)]) || X <- lists:sort(erlang:registered())],
    ok.

%% make truncate table sentence
%% SELECT CONCAT('TRUNCATE TABLE `', `TABLE_NAME`, '`;') FROM information_schema.`TABLES` WHERE `TABLE_SCHEMA` IN ('~s')
%% SELECT CONCAT('TRUNCATE TABLE `', `TABLE_NAME`, '`;') FROM information_schema.`TABLES` WHERE `TABLE_SCHEMA` IN ('~s') AND `TABLE_NAME` NOT LIKE '%_data'
%%
%% local.sql
%% mysqldump --user=root --password=root local > script/sql/local.sql
%% open.sql
%% mysqldump --user=root --password=root --no-data --compact --add-drop-table main | sed 's/\bAUTO_INCREMENT=[0-9]*\s*//g' > script/sql/open.sql
%% [sql:select(<<"SELECT * FROM `", Table/binary, "`">>) || [Table] <- sql:select("SHOW TABLES")]
%%
%% <<9,0,9,39,17,0,1,0,1,49>>,

%%%===================================================================
%%% User data test
%%%===================================================================
u() ->
    %% load
    LoadedUser = user_loop:load(#user{role_id = 1, pid = self(), sender_pid = self(), receiver_pid = self()}),
    %% reset and clean
    USER = user_loop:loop(LoadedUser, 1, role:online_time(LoadedUser), time:ts()),
    %% list type
    {ok, Role} = user_router:write(?PROTOCOL_ROLE, USER#user.role),
    {ok, Asset} = user_router:write(?PROTOCOL_ASSET, USER#user.asset),
    {ok, Item} = user_router:write(?PROTOCOL_ITEM, USER#user.item),
    {ok, Bag} = user_router:write(?PROTOCOL_ITEM, USER#user.bag),
    {ok, Body} = user_router:write(?PROTOCOL_ITEM, USER#user.body),
    {ok, Store} = user_router:write(?PROTOCOL_ITEM, USER#user.store),
    {ok, Mail} = user_router:write(?PROTOCOL_MAIL, USER#user.mail),
    {ok, Quest} = user_router:write(?PROTOCOL_QUEST, USER#user.quest),
    {ok, Shop} = user_router:write(?PROTOCOL_SHOP, USER#user.shop),
    {ok, Friend} = user_router:write(?PROTOCOL_FRIEND, USER#user.friend),
    {ok, Buff} = user_router:write(?PROTOCOL_BUFF, USER#user.buff),
    {ok, Skill} = user_router:write(?PROTOCOL_SKILL, USER#user.skill),
    {ok, Title} = user_router:write(?PROTOCOL_SKILL, USER#user.title),
    {ok, Dungeon} = user_router:write(?PROTOCOL_DUNGEON, USER#user.dungeon),
    %% no storage type
    {ok, Chat} = user_router:write(?PROTOCOL_CHAT_WORLD, [ok, 1, <<"1">>, <<"1">>]),
    %% ets share list type
    {ok, Rank} = user_router:write(?PROTOCOL_RANK + 1, element(2, rank_server:query(?PROTOCOL_RANK + 1))),
    %% ets type
    {ok, LuckyMoney} = user_router:write(?PROTOCOL_LUCKY_MONEY_LIST, element(2, lucky_money_server:query())),
    {ok, Auction} = user_router:write(?PROTOCOL_AUCTION_LIST, element(2, auction_server:query())),
    {ok, GuildList} = user_router:write(?PROTOCOL_GUILD_LIST, element(2, guild_server:query_guild())),
    {ok, RoleList} = user_router:write(?PROTOCOL_GUILD_ROLE_LIST, element(2, guild_server:query_role(USER))),
    {ok, ApplyList} = user_router:write(?PROTOCOL_GUILD_APPLY_LIST, element(2, guild_server:query_apply(USER))),
    {ok, SelfGuildList} = user_router:write(?PROTOCOL_GUILD_SELF_GUILD, element(2, guild_server:query_self_guild(USER))),
    {ok, SelfRoleList} = user_router:write(?PROTOCOL_GUILD_SELF_ROLE, element(2, guild_server:query_self_role(USER))),
    {ok, SelfApplyList} = user_router:write(?PROTOCOL_GUILD_SELF_APPLY, element(2, guild_server:query_self_apply(USER#user{role_id = 3}))),
    %% output
    io:format("~p~n", [[Role, Asset, Item, Bag, Body, Store, Mail, Quest, Shop, Friend, Buff, Skill, Title, Dungeon, Chat, Rank, LuckyMoney, Auction, GuildList, RoleList, ApplyList, SelfGuildList, SelfRoleList, SelfApplyList]]),
    %% return
    USER.

%%%===================================================================
%%% User Socket Event Test
%%%===================================================================
send(Name, Protocol, Data) when is_list(Name) orelse is_binary(Name) ->
    Binary = list_to_binary(encoding:to_list(Name)),
    [[Id]] = sql:select(io_lib:format("SELECT `role_id` FROM `role` WHERE role_name = '~s' OR `account` = '~s'", [Binary, Binary])),
    send(Id, Protocol, Data);
send(Id, Protocol, Data) ->
    case user_server:pid(Id) of
        Pid when is_pid(Pid) ->
            user_server:socket_event(Pid, Protocol, Data);
        Other ->
            Other
    end.

%%%===================================================================
%%% map test
%%%===================================================================






%%%===================================================================
%%% console debug assist
%%%===================================================================
%% @doc clear console
c() ->
    cmd(clear).

%% @doc make and load all
make() ->
    file:set_cwd("script"),
    make:all(),
    file:set_cwd("../"),
    ok.

%% @doc recompile and reload module
cc() ->
    cc(?MODULE, [debug_info, {d, 'DEBUG', true}]).
cc(Module) ->
    cc(Module, [debug_info, {d, 'DEBUG', true}]).
cc(Module, Option) ->
    %% in config dir by default
    cc(Module, "src/", "include/", "beam/", Option).
cc(Module, SrcPath, IncludePath, BeamPath, Option) ->
    %% locate file
    case cmd(find, [SrcPath, lists:concat([Module, ".erl"])]) of
        [] ->
            {error, nofile};
        [Result | _] ->
            %% recompile and reload it
            c:c(Result, [{i, IncludePath}, {outdir, BeamPath} | Option])
    end.

%% @doc hot reload all module
r() ->
    %% in config dir by default
    r("beam").
r(BeamPath) ->
    {ok, LineList} = file:list_dir_all(BeamPath),
    [c:l(type:to_atom(filename:rootname(Line))) || Line <- LineList, string:str(Line, ".beam") =/= 0],
    ok.

%% @doc shell command
cmd(Type) ->
    cmd(Type, []).
cmd(Type, Args) ->
    cmd(Type, Args, os:type()).
cmd(clear, _, {win32, _}) ->
    spawn(fun() -> os:cmd("powershell clear") end);
cmd(clear, _, {unix, _}) ->
    spawn(fun() -> io:format("\e[H\e[J") end);
cmd(list, [Path], {win32, _}) ->
    string:tokens(os:cmd(lists:concat(["dir /b ", Path])), cmd(line));
cmd(list, [Path], {unix, _}) ->
    string:tokens(os:cmd(lists:concat(["ls ", Path])), cmd(line));
cmd(remove, _, {win32, _}) ->
    "del ";
cmd(remove, _, {unix, _}) ->
    "rm ";
cmd(line, _, {win32, _}) ->
    "\r\n";
cmd(line, _, {unix, _}) ->
    "\n";
cmd(path, [], {win32, _}) ->
    $\\;
cmd(path, [], {unix, _}) ->
    $/;
cmd(path, [Path], {win32, _}) ->
    lists:foldr(fun($/, A) -> [$\\ | A];(C, A) -> [C | A] end, [], Path);
cmd(path, [Path], {unix, _}) ->
    lists:foldr(fun($\\, A) -> [$/ | A];(C, A) -> [C | A] end, [], Path);
cmd(find, [Path, Target], {win32, _}) ->
    string:tokens(os:cmd(lists:concat(["where /R ", cmd(path, [Path]), " ", Target, " 2>nul"])), cmd(line));
cmd(find, [Path, Target], {unix, _}) ->
    string:tokens(os:cmd(lists:concat(["find ", cmd(path, [Path]), " -name ", Target, " 2>/dev/null"])), cmd(line)).

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
%%% administrator plant
%%%===================================================================
%% user/log/configure data view(ok)
%% statistic(active/charge/user new or lost)
%% user manager(mail/forbid/login/chat)
%% tool(configure data hot load)
%% admin(user/privileges)
%% open/merge server
%% 

%%%===================================================================
%%% architecture plant
%%%===================================================================
%% 开发/部署脚本(ok)
%% 基础网络tcp/http/ws/wss(ok)
%% 配置数据(ok)
%% 通信协议(ok)
%% 集群工具(ok)
%% 通用工具(ok)
%% 错误日志(ok)
%% 构造器(敏感词/表到记录/表到sql/表到日志/表到配置/表到lua/表到js/表到excel/协议)(ok)
%% 
%% 日志(模块数据)(ok)
%% 账户(ok)
%% 角色(ok)
%% 资产(ok)
%% 背包(item, bag, body, store)(ok)
%% 帮派(ok)
%% 任务(ok)
%% 好友(ok)
%% 商店(ok)
%% 聊天(ok)
%% 邮件(ok)
%% 公告(ok)
%% 排行榜(ok)
%% 敏感词(ok)
%% 统计(ok)
%% 兑换码(ok)
%% 活动(ok)
%% 公告(ok)
%% 管理员(ok)
%% 充值(ok)
%% 机器人

%% 战场(ok)
%% 副本(ok)

%% 属性(ok)
%% 技能(ok)
%% buff(ok)
%% 效果(ok)
%% 地图(ok)
%% 怪物AI(ok)



%%%===================================================================
%%% important
%%%===================================================================
%% 战斗
%% 攻击者使用技能对作用半径内敌人(一个或多个)发起攻击
%% 如果命中
%% 计算伤害(基本属性伤害),计算被动技能
%% 计算技能Buff
%% 更新对象

%%% 玩家/怪物/NPC/掉落
%%% 属性/技能/Buff
%%%
%%%
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
%%chose_object(State, Attacker, Target, self) -> Attacker;
%%chose_object(State, Attacker, Target, rival) -> Target.
%%
%%chose_attribute(State, Object, Hurt, attribute) -> Object#fighter.attribute;
%%chose_attribute(State, Object, Hurt, buff) -> Object#fighter.buffs;
%%chose_attribute(State, Object, Hurt, hurt) -> Hurt;
%%chose_attribute(State, Object, Hurt, skill) -> Object#fighter.skills.
%%
%%chose_field(power) -> ok.
%%
%%calculate_value() -> ok.
%%
%%chose_operation(add) -> ok;
%%chose_operation(clear) -> ok;
%%chose_operation(reduce) -> ok;
%%chose_operation(set) -> ok.

%% type   : fix move active passive
%% act_script : enemy fighter monster {monster, id} location

%% @todo work plan
%% effect auto/manual
%% monster ai
%% robot
%% module test unit
%% asset add/check/cost/ generate
%% map/battle/tool arrangement
%% excel refer

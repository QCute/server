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
-include("../../../include/buff.hrl").
-include("../../../include/common.hrl").
-include("../../../include/count.hrl").
-include("../../../include/event.hrl").
-include("../../../include/friend.hrl").
-include("../../../include/guild.hrl").
-include("../../../include/item.hrl").
-include("../../../include/key.hrl").
-include("../../../include/mail.hrl").
-include("../../../include/map.hrl").
-include("../../../include/monster.hrl").
-include("../../../include/notice.hrl").
-include("../../../include/online.hrl").
-include("../../../include/protocol.hrl").
-include("../../../include/quest.hrl").
-include("../../../include/rank.hrl").
-include("../../../include/role.hrl").
-include("../../../include/serialize.hrl").
-include("../../../include/shop.hrl").
-include("../../../include/skill.hrl").
-include("../../../include/socket.hrl").
-include("../../../include/sorter.hrl").
-include("../../../include/user.hrl").
-include("../../../include/vip.hrl").

%%%===================================================================
%%% API
%%%===================================================================
%% @doc escript entry
main(Args) ->
    io:format("~p~n", [Args]).

s(A) ->sys:get_state(erlang:whereis(A)).

%%%===================================================================
%%% map test
%%%===================================================================
t(T) -> catch ets:tab2list(T).


ms() -> s(map_100000).


%%%===================================================================
%%% console test
%%%===================================================================
ct() ->
    console:print(?MODULE, ?LINE, "~s~n", [<<"print">>]),
    console:debug(?MODULE, ?LINE, "~p~n", [<<"debug">>]),
    console:info(?MODULE, ?LINE, "~p~n", [info]),
    console:warming(?MODULE, ?LINE, "~p~n", [warming]),
    console:error(?MODULE, ?LINE, "~p~n", [error]).

%%%===================================================================
%%% User data test
%%%===================================================================
t() ->
    USER = user_loader:load(#user{role_id = 1}),
    ROLE = user_router:write(?PROTOCOL_ROLE, [USER#user.role]),
    ASSET = user_router:write(?PROTOCOL_ASSET, [USER#user.asset]),
    ITEM = user_router:write(?PROTOCOL_ITEM, [USER#user.item]),
    BAG = user_router:write(?PROTOCOL_ITEM, [USER#user.bag]),
    STORE = user_router:write(?PROTOCOL_ITEM, [USER#user.store]),
    MAIL = user_router:write(?PROTOCOL_MAIL, [USER#user.mail]),
    QUEST = user_router:write(?PROTOCOL_QUEST, [USER#user.quest]),
    SHOP = user_router:write(?PROTOCOL_SHOP, [USER#user.shop]),
    FRIEND = user_router:write(?PROTOCOL_FRIEND, [USER#user.friend]),
    BUFF = user_router:write(?PROTOCOL_BUFF, [USER#user.buff]),
    SKILL = user_router:write(?PROTOCOL_SKILL, [USER#user.skill]),
    CHAT = user_router:write(?PROTOCOL_CHAT_WORLD, [1, <<"1">>, <<"1">>]),
    RANK = user_router:write(?PROTOCOL_RANK, [rank_server:rank(1)]),

    io:format("~p~n", [USER]),
    [io:format("~p~n", [element(1, X)]) || X <- [ROLE, ASSET, ITEM, BAG, STORE, MAIL, QUEST, SHOP, FRIEND, CHAT, RANK, BUFF, SKILL]],
    USER.

vr() ->
    [X || X <- erlang:registered(), string:str(atom_to_list(X), "receiver") =/= 0].



%%%===================================================================
%%% protocol test
%%%===================================================================

x() ->
%%    F = fun(Binary) ->
%%        {Id, Binary1} = protocol:read_unsigned(Binary, 32),
%%        {Nick, Binary2} = protocol:read_string(Binary1),
%%        {Name, Remain} = protocol:read_bit_string(Binary2),
%%        %% protocol:read_bit_string(protocol:read_string(protocol:read_unsigned(Binary, 32))),
%%        {{Id, Nick, Name}, Remain}
%%    end,
    Binary = <<1:32, 1:16, "1", 1:16, "1", 2:32, 1:16, "2", 1:16, "2">>,
    protocol:read_list(fun(BinaryData) -> protocol:revise(protocol:read(string, protocol:read(string, protocol:read(32, BinaryData)))) end, Binary).

%%%===================================================================
%%% parser
%%%===================================================================
do() ->
    F = fun({A, B, C, _})  -> [A, B, C] end,
    L = [
        {1,2,3,x},
        {4,5,6,x},
        {7,8,9,x}
    ],
    parser:collect_into(L, F, {<<"insert into `test` (`a`, `b`, `c`) values ">>, <<"(~w, ~w, ~w)">>, <<" on duplicate key update `type` = VALUES(`type`), `type` = VALUES(`type`), `type` = VALUES(`type`)">>}, 4).

doo() ->
    catch ets:new(test, [named_table, {keypos, 1}]),

    F = fun({A, B, C, _})  -> [A, B, C] end,
    L = [
        {1,2,3,x},
        {4,5,6,x},
        {7,8,9,x}
    ],
    ets:insert(test, L),
    parser:collect_into(test, F, {<<"insert into `test` (`a`, `b`, `c`) values ">>, <<"(~w, ~w, ~w)">>, <<" on duplicate key update `type` = VALUES(`type`), `type` = VALUES(`type`), `type` = VALUES(`type`)">>}, 4).

%%%===================================================================
%%% sorter test
%%%===================================================================
tx() ->
    SortList = [
        #rank{type = 1, key = 1, value = 1, rank = 2},
        #rank{type = 1, key = 1, value = 1, rank = 3},
        #rank{type = 1, key = 1, value = 1, rank = 1},
        #rank{type = 1, key = 1, value = 1, rank = 4},
        #rank{type = 1, key = 1, value = 1, rank = 5}
    ],
    Sorter = sorter:new(wow, share, replace, 100, #rank.key, #rank.value, #rank.time, #rank.rank, SortList),
    sorter:update(#rank{type = 1, rank = 0}, Sorter),
    sorter:data(Sorter).

%%%===================================================================
%%% randomness test
%%%===================================================================
test_randomness() ->
    F = fun(_) -> test_randomness_loop(lists:duplicate(1000, 0), dict:new()) end,
    All = map_reduce(F, lists:seq(1, 1000)),
    String = lists:flatten(["[" ++ string:join([io_lib:format("{~p:~p}", [X, N]) || {X, N} <- List], ", ") ++ "]\n" || List <- All]),
    file:write_file("sample.json", String).

test_randomness_loop([], Dict) ->
    lists:sort(dict:to_list(Dict));
test_randomness_loop([_ | T], Dict) ->
    X = randomness:rand(),
    test_randomness_loop(T, dict:update_counter(X, 1, Dict)).

ac(X) ->
    activity:next_state(#activity{show_time = 10, start_time = 10, over_time = 30, award_time = 30, hide_time = 30, clean_time = 30}, X).

%%%===================================================================
%%% other test
%%%===================================================================

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

-record(priority_queue, {size, key, left, right, queue}).

new(Key) ->
    #priority_queue{key = Key, size = 0, left = [], right = [], queue = []}.

query(Item, Queue = #priority_queue{key = Key, queue = Queue}) ->
    NewQueue = push_loop(Queue, Key, Item, []),
    Queue#priority_queue{queue = NewQueue}.

push_loop([], _, Item, List) ->
    lists:reverse([Item | List]);
push_loop([H | T], Key, Item, List) when erlang:element(Key, Item) >= erlang:element(Key, H)->
    lists:reverse([H | List], T);
push_loop([H | T], Key, Item, List) ->
    push_loop(T, Key, Item, [H | List]).

priority_queue() ->
    Q = new(2),
    Q1 = query(Q, {1, 1}),
    Q1 = query(Q, {2, 2}),
    ok.


%% init:stop(),
%% application:stop(),
%% trap_exit |   exit signal |    action
%% ----------|---------------|----------------------------------------------------------
%% true      |   kill        |    Die: Broadcast the exit signal killed to the link set.
%% true      |   X           |    Add {'EXIT', Pid, X} to the mailbox.
%% false     |   normal      |    Continue: Do-nothing signal vanishes
%% false     |   kill        |    Die: Broadcast the exit signal killed to the link set
%% false     |   X           |    Die: Broadcast the exit signal X to the link set
%% -------------------------------------------------------------------------------------
%%%===================================================================
%%% misc code
%%%===================================================================
map_reduce(F, L) ->
    Parent = self(),
    [spawn(fun() -> erlang:send(Parent, catch F(I)) end) || I <- L],
    [receive R -> R end || _ <- L].

%% not tail recursive function
append([H|T], Tail) ->
    [H|append(T, Tail)];
append([], Tail) ->
    Tail.

%% @doc remote reload module
reload(Module) ->
    [IP | _] = [Address || {_, Opts} <- element(2, inet:getifaddrs()), {addr, Address} <- Opts, size(Address) == 4 andalso Address =/= {127, 0, 0, 1}],
    LocalIP = string:join([integer_to_list(F) || F <- tuple_to_list(IP)], "."),
    {ok, NameList} = erl_epmd:names(),
    [Self | _] = string:tokens(atom_to_list(node()), "@"),
    [reload(Module, list_to_atom(Name ++ "@" ++ LocalIP)) || {Name, _} <- NameList, Name =/= Self].
reload(Module, Node) ->
    reload(Module, Node, []).
reload(Module, Node, data) ->
    Extra = file:read_file(lists:concat(["ebin/", filename:rootname(Module, ".beam"), ".beam"])),
    reload(Module, Node, Extra);
reload(Module, Node, Extra) ->
    case net_adm:ping(Node) of
        pong ->
            LocalVsn = checksum(Module),
            case rpc:call(Node, ?MODULE, hot_load, [Module, Extra]) of
                {Node, {Flag, Atom}, RemoteVsn} ->
                    io:format("response from :~p result:~p ~p checksum:~p ~n", [Node, Flag, Atom, LocalVsn == RemoteVsn]);
                Error ->
                    io:format("reload error on node: ~p error:~p~n", [Node, Error])
            end;
        pang ->
            io:format("cannot connect node: ~p, plaease check your cookie and connect privilege~n", [Node])
    end.

%% @doc reload remote callback
hot_load(Module, {ok, Data}) ->
    BeamName = filename:rootname(Module, ".beam"),
    file:write_file("ebin/" ++ BeamName ++ ".beam", Data),
    hot_load(Module, []);
hot_load(Module, []) ->
    Result = c:l(Module),
    Vsn = checksum(Module),
    {node(), Result, Vsn};
hot_load(_, Extra) ->
    {node(), Extra, nomatch}.

%% @doc beam checksum
checksum(Module) ->
    case catch Module:module_info(attributes) of
        {'EXIT', _} ->
            [];
        Attributes ->
            {vsn, Vsn} = lists:keyfind(vsn, 1, Attributes),
            Vsn
    end.

%% insert table initialized data
initialize_table(Id, Database, Table) ->
    Sql = io_lib:format("
    SELECT
        GROUP_CONCAT('`', information_schema.`COLUMNS`.`COLUMN_NAME`, '`'),
        GROUP_CONCAT('''', information_schema.`COLUMNS`.`COLUMN_DEFAULT`, '''')
    FROM
        information_schema.`COLUMNS`
    WHERE
        information_schema.`COLUMNS`.`TABLE_SCHEMA` = '~s'
        AND
        information_schema.`COLUMNS`.`TABLE_NAME` = '~s'", [Database, Table]),
    %% collect all fields and default value
    [[Fields, Default]] = sql:select(Sql),
    %% strong match insert id equals given id
    Id = sql:insert(io_lib:format("INSERT INTO `~s` (~s) VALUES ('~w', ~s)", [Table, Fields, Id, Default])).

%%%===================================================================
%%% general server
%%%===================================================================
start_link() ->
    start_link([]).
start_link(Args) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Args, []).
init(_) ->
    {ok, []}.
handle_call(_Request, _From, State) ->
    {reply, ok, State}.
handle_cast(_Request, State) ->
    {noreply, State}.
handle_info(_Request, State) ->
    {noreply, State}.
terminate(_Reason, State) ->
    {ok, State}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
%%%===================================================================
%%% characters test tool
%%%===================================================================
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

ts(String) ->
    case os:type() of
        {win32, nt} ->
            io:format("\"~ts\"~n", [encoding:to_list(String)]);
        {unix, linux} ->
            io:format("\"~ts\"~n", [encoding:to_list_int(String)])
    end.
%% 一
%% <<228,184,128>>  .utf8      228*256*256 + 184*256 + 128   [14989440]
%% <<78,0>>         .unicode   78*256 + 0                    [19968]
%% <<210,187>>      .gbk       210*256+187                   [53947]

%%%===================================================================
%%% performance tool
%%%===================================================================
more_test() ->
    L = lists:seq(1, 1000),
    [test() || _ <- L],
    ok.
test() ->
    O = 1000000,
    L = lists:seq(1, O),
    Begin = os:timestamp(),
    %% first
    Middle = os:timestamp(),
    %% second
    End = os:timestamp(),
    First = timer:now_diff(Middle, Begin) div 1000,
    Second = timer:now_diff(End, Middle) div 1000,
    io:format("First:~p   Second:~p~n", [First, Second]),
    L.

%%%===================================================================
%%% ip tool
%%%===================================================================
ip(4) ->
    [IP | _] = [Address || {_, Opts} <- element(2, inet:getifaddrs()), {addr, Address} <- Opts, size(Address) == 4 andalso Address =/= {127, 0, 0, 1}],
    string:join([integer_to_list(I) || I <- tuple_to_list(IP)], ".");
ip(6) ->
    [IP | _] = [Address || {_, Opts} <- element(2, inet:getifaddrs()), {addr, Address} <- Opts, size(Address) == 8 andalso Address =/= {0, 0, 0, 0, 0, 0, 0, 1}],
    string:join([integer_to_list(I, 16) || I <- tuple_to_list(IP)], ":").

%% local ipv4 address
ipv4() ->
    [IP | _] = [Address || {_, Opts} <- element(2, inet:getifaddrs()), {addr, Address} <- Opts, size(Address) == 4 andalso Address =/= {127, 0, 0, 1}],
    string:join([integer_to_list(I) || I <- tuple_to_list(IP)], ".").

%% local ipv6 address
ipv6() ->
    [IP | _] = [Address || {_, Opts} <- element(2, inet:getifaddrs()), {addr, Address} <- Opts, size(Address) == 8 andalso Address =/= {0, 0, 0, 0, 0, 0, 0, 1}],
    string:join([integer_to_list(I, 16) || I <- tuple_to_list(IP)], ":").

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
    cc(?MODULE, [debug_info, {d, 'DEBUG'}]).
cc(Module) ->
    cc(Module, [debug_info, {d, 'DEBUG'}]).
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
    [c:l(list_to_atom(filename:rootname(Line))) || Line <- LineList, string:str(Line, ".beam") =/= 0],
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
%% error code generate from protocol script
%% asset add/check/cost/ generate
%% excel maker/refer(`event_data`.`event`, `event_data`.`description`)
%% map/battle/tool arrangement
%%
%%
%% full robot
%% split robot
%%
%% role_state_misc
%% role_state_time
%% role_state_number
%% role_state_size
%%
%% single machine/one platform/one admin
%%
%% learn digest
%% git/mysql
%%
%%
%%
%%
%% 0000000000000000
%% ChannelId * 1000000000000000000 + ServerId * 1000000000000001.
%%
%% BaseChannelId    => 01000000000000000000
%% BaseServerId     => 00001000000000000000
%% DistinctServerId => 00000001000000000000
%% 01000000000000000000 * ChannelId
%% 00001000000000000000 * ServerId
%% 00000001000000000000 * ServerId
%% 00000000000000000000
%% 01001001000000000000  1      =>  01001001000000000000  1
%% 01002002000000000000  2      =>  01001002000000000000  1
%% 01099099000000000000  99     =>  01100099000000000000  100
%% 01100100000000000000  100    =>  01100100000000000000  100
%% ServerId = (Id rem BaseChannelId) div BaseServerId
%%
%%
%% select min(id) from log;
%% select max(id) from log;
%% page:
%% first: select * from log where id between (min) and (max) limit 100;
%% next:  select * from log where id between (page max) and (max) limit 100;

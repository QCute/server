%%%------------------------------------------------------------------
%%% @doc
%%% module test
%%% @end
%%%------------------------------------------------------------------
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
%% process flag and exit operation
%% ---------------------------------------------------------------------------
%% trap_exit | signal | action
%% ----------|--------|-------------------------------------------------------
%% true      | kill   | Die: Broadcast the exit signal killed to the link set.
%% true      | X      | Add {'EXIT', Pid, X} to the mailbox.
%% false     | normal | Continue: Do-nothing signal vanishes
%% false     | kill   | Die: Broadcast the exit signal killed to the link set
%% false     | X      | Die: Broadcast the exit signal X to the link set
%% ---------------------------------------------------------------------------

%%%==================================================================
%%% API functions
%%%==================================================================
%% @doc escript entry
main(_) ->
    catch code:add_path(filename:dirname(escript:script_name()) ++ "/../../../beam/"),
    io:format("").

%% process state
s(A) ->sys:get_state(erlang:whereis(A)).

%% list processes
ls() ->
    [io:format("~w~s~w~n", [X, lists:duplicate(32 - length(atom_to_list(X)), " "), erlang:whereis(X)]) || X <- lists:sort(erlang:registered())],
    ok.

lsp() ->
    [io:format("~w~s~w~n", [X, lists:duplicate(32 - length(atom_to_list(X)), " "), erlang:whereis(X)]) || X <- lists:sort(erlang:registered())],
    ok.

%% make truncate table sentence
%% SELECT CONCAT('TRUNCATE TABLE `', `TABLE_NAME`, '`;') FROM information_schema.`TABLES` WHERE `TABLE_SCHEMA` IN ('~s')
%%

%%%==================================================================
%%% User Socket Event Test
%%%==================================================================
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
%%%==================================================================
%%% User data test
%%%==================================================================
t() ->
    %% load
    LoadedUser = user_loop:load(#user{role_id = 1, pid = self(), sender_pid = self(), receiver_pid = self()}),
    %% reset and clean
    USER = user_loop:loop(LoadedUser, role:online_time(LoadedUser), time:ts()),
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
    {ok, Rank} = user_router:write(?PROTOCOL_RANK, element(2, rank_server:query(1))),
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

%%%==================================================================
%%% map test
%%%==================================================================


%%%==================================================================
%%% console test
%%%==================================================================
ct() ->
    console:print(?MODULE, ?LINE, "~s~n", [<<"print">>]),
    console:debug(?MODULE, ?LINE, "~p~n", [<<"debug">>]),
    console:info(?MODULE, ?LINE, "~p~n", [info]),
    console:warming(?MODULE, ?LINE, "~p~n", [warming]),
    console:error(?MODULE, ?LINE, "~p~n", [error]).

%%%==================================================================
%%% protocol test
%%%==================================================================

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

%%%==================================================================
%%% parser test
%%%==================================================================
ds() ->
    State = [{a, 0}, {b, 0}, {c, 0}, {d, x}],
    %% batch save only at server close
    Format = {<<"INSERT INTO `increment` (`name`, `value`) VALUES ">>, <<"('~s', '~w')">>, <<" ON DUPLICATE KEY UPDATE `value` = VALUES(`value`)">>},
    %% rename table, avoid other process update sequence after save value
    %% F = fun({Name, _}) -> NewName = type:to_atom(erlang:make_ref()), ets:rename(Name, NewName), Value = ets:lookup_element(NewName, sequence, 2), ets:delete(NewName), {Name, Value} end,
    {Sql, _} = parser:collect_into(State, fun erlang:tuple_to_list/1, Format, 2),
    Sql.

do() ->
    F = fun({A, B, C, _})  -> [A, B, C] end,
    L = [
        {1,2,3,x},
        {4,5,6,x},
        {7,8,9,x},
        {10,11,12,x}
    ],
    parser:collect_into(L, F, {<<"insert into `test` (`a`, `b`, `c`) values ">>, <<"(~w, ~w, ~w)">>, <<" on duplicate key update `type` = VALUES(`type`), `type` = VALUES(`type`), `type` = VALUES(`type`)">>}, 4).

doo() ->
    catch ets:delete(test),
    catch ets:new(test, [named_table,ordered_set, {keypos, 1}]),

    F = fun({A, B, C, _})  -> [A, B, C] end,
    L = [
        {1,2,3,0},
        {4,5,6,0},
        {7,8,9,x},
        {10,11,12,x}
    ],
    ets:insert(test, L),
    {Sql, _} = parser:collect_into(test, F, {<<"insert into `test` (`a`, `b`, `c`) values ">>, <<"(~w, ~w, ~w)">>, <<" on duplicate key update `type` = VALUES(`type`), `type` = VALUES(`type`), `type` = VALUES(`type`)">>}, 4),
    Sql.

%%%==================================================================
%%% sorter test
%%%==================================================================
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

%%%==================================================================
%%% randomness test
%%%==================================================================
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
    activity:continue(#activity{show_time = 10, start_time = 10, over_time = 30, award_time = 30, stop_time = 30}, X).


%% lucky money test
%% total_gold < total_number * Radix
random_lucky_money_test(LuckyMoney = #lucky_money{total_gold = TotalGold, total_number = TotalNumber, remain_gold = RemainGold, receive_number = ReceiveNumber}) when TotalGold >= TotalNumber ->
    case random_lucky_money(LuckyMoney) of
        0 ->
            ok;
        N ->
            io:format("~w~n", [N]),
            random_lucky_money_test(LuckyMoney#lucky_money{remain_gold = RemainGold - N, receive_number = ReceiveNumber + 1})
    end;
random_lucky_money_test(_) ->
    oops.

%% the last one
random_lucky_money(#lucky_money{total_number = TotalNumber, receive_number = ReceiveNumber, remain_gold = RemainGold}) when ReceiveNumber + 1 == TotalNumber ->
    RemainGold;
%% random range between radix and (remain gold - (remain number - 1) * radix)
random_lucky_money(#lucky_money{remain_gold = RemainGold, total_number = TotalNumber, receive_number = ReceiveNumber}) when ReceiveNumber < TotalNumber ->
    Radix = 1,
    randomness:rand(Radix, (RemainGold - ((TotalNumber - ReceiveNumber - 1) * Radix)));
random_lucky_money(_) ->
    0.

%%%==================================================================
%%% other test
%%%==================================================================

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




%%%==================================================================
%%% misc code
%%%==================================================================
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
    [IP | _] = [Address || {_, Opts} <- element(2, inet:getifaddrs()), {addr, Address} <- Opts, tuple_size(Address) == 4 andalso Address =/= {127, 0, 0, 1}],
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



%% @doc load module for all node, shell execute compatible
-spec load(atom() | [atom()]) -> ok.
load(Modules) ->
    load(proplists:get_value('BEAM_LOADER_NODES', init:get_arguments(), []), Modules).

%% @doc load module (local call)
-spec load(atom() | [atom()], atom() | [atom()]) -> ok.
load(Nodes, Modules) ->
    execute_load(Nodes, Modules, soft_purge).

%% @doc force load module for all node, shell execute compatible
-spec force_load(atom() | [atom()]) -> ok.
force_load(Modules) ->
    force_load(proplists:get_value('BEAM_LOADER_NODES', init:get_arguments(), []), Modules).

%% @doc force load module (local call)
-spec force_load(atom() | [atom()], atom() | [atom()]) -> ok.
force_load(Nodes, Modules) ->
    execute_load(Nodes, Modules, purge).

%% @doc load module (local call)
-spec execute_load(atom() | [atom()], atom() | [atom()], atom()) -> ok.
execute_load(Node, Modules, Mode) when is_atom(Node) ->
    execute_load([Node], Modules, Mode);
execute_load(Nodes, Module, Mode) when is_atom(Module) ->
    execute_load(Nodes, [Module], Mode);
execute_load(Node, Module, Mode) when is_atom(Node) andalso is_atom(Module) ->
    execute_load([Node], [Module], Mode);
execute_load(Nodes, Modules, Mode) ->
    execute_load_loop(Nodes, [{type:to_atom(Module), checksum(type:to_atom(Module))} || Module <- Modules], Mode).

execute_load_loop([], _, _) ->
    ok;
execute_load_loop([Node | T], Modules, Mode) ->
    case rpc:call(type:to_atom(Node), ?MODULE, load_callback, [Modules, Mode]) of
        {ok, Result} ->
            handle_result(Result),
            execute_load_loop(T, Modules, Mode);
        _ ->
            io:format(standard_error, "cannot connect to node:~p~n", [Node]),
            execute_load_loop(T, Modules, Mode)
    end.

%% handle remote result
handle_result([]) ->
    io:format("~n~n");
handle_result([{Node, Module, _, {error, Error}, _} | T]) ->
    NodePadding = lists:duplicate(32 - length(lists:concat([Node])), " "),
    ModulePadding = lists:duplicate(24 - length(lists:concat([Module])), " "),
    io:format("node:~p~s module:~p~s result:~p~n", [Node, NodePadding, Module, ModulePadding, Error]),
    handle_result(T);
handle_result([{Node, Module, _, {_, _}, true} | T]) ->
    NodePadding = lists:duplicate(32 - length(lists:concat([Node])), " "),
    ModulePadding = lists:duplicate(24 - length(lists:concat([Module])), " "),
    io:format("node:~p~s module:~p~s result:~p~n", [Node, NodePadding, Module, ModulePadding, true]),
    handle_result(T).

%% @doc soft/purge and load module (remote call)
-spec load_callback([atom()], atom()) -> ok.
load_callback(Modules, Mode) ->
    load_callback_loop(Modules, Mode, []).

load_callback_loop([], _, Result) ->
    {ok, lists:reverse(Result)};
load_callback_loop([{Module, Vsn} | T], Mode, Result) ->
    case code:is_loaded(Module) of
        false ->
            load_callback_loop(T, Mode, [{node(), Module, true, {error, unloaded}, false} | Result]);
        _ ->
            Purge = code:Mode(Module),
            Load = code:load_file(Module),
            Checksum = checksum(Module),
            load_callback_loop(T, Mode, [{node(), Module, Purge, Load, Checksum == Vsn} | Result])
    end.


%% @doc transform list data to record
transform(Table, CallBack) ->
    %% table name same as record name
    Sql = lists:concat(["SELECT * FROM `", Table, "`"]),
    transform(Sql, Table, Table, CallBack).
transform(Sql, Table, CallBack) ->
    %% table name same as record name
    transform(Sql, Table, Table, CallBack).
transform(Sql, Table, Record, CallBack) ->
    Data = sql:select(Sql),
    %% load data delete first
    catch ets:delete_all_objects(Table),
    %% use callback transform data
    List = lists:foldl(fun(E, Acc) -> catch CallBack(list_to_tuple([Record | E]), Acc) end, [], Data),
    %% save to ets
    ets:insert(Table, List).

%%%==================================================================
%%% general server
%%%==================================================================
start_link() ->
    start_link([]).
start_link(Args) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Args, []).
init(_) ->
    %% erlang:process_flag(trap_exit, true),
    {ok, []}.
handle_call(_Request, _From, State) ->
    {reply, ok, State}.
handle_cast(_Request, State) ->
    {noreply, State}.
handle_info(normal, State) ->
    {stop, normal, State};
handle_info(shutdown, State) ->
    {stop, shutdown, State};
handle_info({shutdown, Reason}, State) ->
    {stop, {shutdown, Reason}, State};
handle_info(_Request, State) ->
    io:format("handle_info:~p~n", [_Request]),
    {noreply, State}.
terminate(_Reason, State) ->
    io:format("terminate:~p~n", [_Reason]),
    {ok, State}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
%%%==================================================================
%%% characters test tool
%%%==================================================================
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
%% にほんご
jp() ->
    "にほんご".
jps() ->
    <<"にほんご"/utf8>>.

%%%==================================================================
%%% performance tool
%%%==================================================================
%% test on OTP 21.3
%%
%% function(Arg)
%% module:function(arg)
%%
%% apply(Module, Function, Args)
%% Module:Function(Arg)
%% apply(fun Function/arity, Args)
%%
%% 2.x times slow
%% fun() -> ok end
%% fun Function/arity(Arg)
%%
%% performance
%% local > remote >  apply(f,a) ~= apply(m,f,a) ~= execute(m,f,a) >> execute(f,a)(lambda) > execute(f,a)
%%

test_apply() ->
    L = lists:seq(1, 10000000),
    LocalTime = os:timestamp(),
    [jp() || _ <- L],
    RemoteTime = os:timestamp(),
    [?MODULE:jp() || _ <- L],
    ApplyTime = os:timestamp(),
    [apply(?MODULE, jp, []) || _ <- L],
    ExecuteTime = os:timestamp(),
    Module = ?MODULE,
    Function = jp,
    [Module:Function() || _ <- L],
    ApplyFunctionTime = os:timestamp(),
    [apply(fun jp/0, []) || _ <- L],
    ExecuteFunctionTime = os:timestamp(),
    F = fun jp/0,
    [F() || _ <- L],
    LambdaFunctionTime = os:timestamp(),
    FF = fun() -> "にほんご" end,
    [FF() || _ <- L],
    EndTime = os:timestamp(),
    io:format("~p ~p ~p ~p ~p ~p ~p~n", [timer:now_diff(RemoteTime, LocalTime), timer:now_diff(ApplyTime, RemoteTime), timer:now_diff(ExecuteTime, ApplyTime), timer:now_diff(ApplyFunctionTime, ExecuteTime), timer:now_diff(ExecuteFunctionTime, ApplyFunctionTime), timer:now_diff(LambdaFunctionTime, ExecuteFunctionTime), timer:now_diff(EndTime, LambdaFunctionTime)]),
    ok.

more_test() ->
    L = lists:seq(1, 1000),
    [test() || _ <- L],
    ok.
test() ->
    O = 10000000,
    L = lists:seq(1, O),
    Begin = os:timestamp(),
    %% first
    F = fun jp/0,
    [F() || _ <- L],
    %% [?MODULE:jp() || _ <- L],
    %% middle
    Middle = os:timestamp(),
    %% middle
    [apply(fun jp/0, []) || _ <- L],
    %% [apply(?MODULE, jp, []) || _ <- L],
    %% [?MODULE:jp() || _ <- L],
    %% second
    End = os:timestamp(),
    %% diff
    First = timer:now_diff(Middle, Begin) div 1000,
    Second = timer:now_diff(End, Middle) div 1000,
    io:format("First:~p   Second:~p~n", [First, Second]),
    ok.

qs(Pid, Sql) ->
    erlang:send(Pid, {query, self(), Sql}),
    receive
        {Pid, Result} ->
            Result
    end.

qd() ->
    receive
        {query, From, Sql} ->
            Self = erlang:self(),
            erlang:send(From, {Self, Sql}),
            qd()
    end.

%%%==================================================================
%%% ip tool
%%%==================================================================
ip(4) ->
    [IP | _] = [Address || {_, Opts} <- element(2, inet:getifaddrs()), {addr, Address} <- Opts, tuple_size(Address) == 4 andalso Address =/= {127, 0, 0, 1}],
    string:join([integer_to_list(I) || I <- tuple_to_list(IP)], ".");
ip(6) ->
    [IP | _] = [Address || {_, Opts} <- element(2, inet:getifaddrs()), {addr, Address} <- Opts, tuple_size(Address) == 8 andalso Address =/= {0, 0, 0, 0, 0, 0, 0, 1}],
    string:join([integer_to_list(I, 16) || I <- tuple_to_list(IP)], ":").

%% local ipv4 address
ipv4() ->
    [IP | _] = [Address || {_, Opts} <- element(2, inet:getifaddrs()), {addr, Address} <- Opts, tuple_size(Address) == 4 andalso Address =/= {127, 0, 0, 1}],
    string:join([integer_to_list(I) || I <- tuple_to_list(IP)], ".").

%% local ipv6 address
ipv6() ->
    [IP | _] = [Address || {_, Opts} <- element(2, inet:getifaddrs()), {addr, Address} <- Opts, tuple_size(Address) == 8 andalso Address =/= {0, 0, 0, 0, 0, 0, 0, 1}],
    string:join([integer_to_list(I, 16) || I <- tuple_to_list(IP)], ":").

%%%==================================================================
%%% code assist
%%%==================================================================
list(Table) ->
    list(main, Table).
list(DataBase, Table) ->
    FieldsSql = io_lib:format(<<"SELECT `COLUMN_NAME`, `COLUMN_DEFAULT`, `DATA_TYPE`, `COLUMN_COMMENT`, `ORDINAL_POSITION`, `COLUMN_KEY`, `EXTRA` FROM information_schema.`COLUMNS` WHERE `TABLE_SCHEMA` = '~s' AND `TABLE_NAME` = '~s'">>, [DataBase, Table]),
    Fields = sql:select(FieldsSql),
    string:join([binary_to_list(Name) || [Name, _, _, _, _, _, _] <- Fields], ", ").

%% @doc fields to hump name
hump(Table) ->
    hump(main, Table).
hump(DataBase, Table) ->
    FieldsSql = io_lib:format(<<"SELECT `COLUMN_NAME`, `COLUMN_DEFAULT`, `DATA_TYPE`, `COLUMN_COMMENT`, `ORDINAL_POSITION`, `COLUMN_KEY`, `EXTRA` FROM information_schema.`COLUMNS` WHERE `TABLE_SCHEMA` = '~s' AND TABLE_NAME = '~s' ORDER BY ORDINAL_POSITION;">>, [DataBase, Table]),
    Fields = sql:select(FieldsSql),
    F = fun(Name) -> lists:concat([[case 96 < H andalso H < 123 of true -> H - 32; _ -> H end | T] || [H | T] <- string:tokens(Name, "_")]) end,
    string:join([F(binary_to_list(Name)) || [Name, _, _, _, _, _, _] <- Fields], ", ").

%% @doc code construct
make(Table) ->
    make(main, Table).
make(DataBase, Table) ->
    FieldsSql = io_lib:format(<<"SELECT `COLUMN_NAME`, `COLUMN_DEFAULT`, `DATA_TYPE`, `COLUMN_COMMENT`, `ORDINAL_POSITION`, `COLUMN_KEY`, `EXTRA` FROM information_schema.`COLUMNS` WHERE `TABLE_SCHEMA` = '~s' AND TABLE_NAME = '~s' ORDER BY ORDINAL_POSITION;">>, [DataBase, Table]),
    Fields = sql:select(FieldsSql),
    F = fun(Name) -> lists:concat([[case 96 < H andalso H < 123 of true -> H - 32; _ -> H end | T] || [H | T] <- string:tokens(Name, "_")]) end,
    Args = string:join([F(binary_to_list(Name)) || [Name, _, _, _, _, _, _] <- Fields], ", "),
    Fill = string:join([lists:concat(["        ", binary_to_list(Name), " = ", F(binary_to_list(Name))]) || [Name, _, _, _, _, _, _] <- Fields], ",\n"),
    Code = lists:concat(["make_", Table, "(", Args, ") ->\n    #", Table, "{\n", Fill, "\n    }."]),
    io:format("~s~n", [Code]).
%%%==================================================================
%%% console debug assist
%%%==================================================================

%% @doc find module file from source path
-spec locate(Path :: string(), File :: file:filename()) -> [file:filename()].
locate(Path, File) ->
    {ok, FileList} = file:list_dir_all(Path),
    locate_loop(FileList, Path, File, []).

%% depth first search
locate_loop([], _, _, List) ->
    List;
locate_loop([Name | T], Path, File, List) ->
    SubFile = Path ++ "/" ++ Name,
    case filelib:is_dir(SubFile) of
        true ->
            %% sub dir recursion
            Result = locate(SubFile, File),
            locate_loop(T, Path, File, List ++ Result);
        false when Name =:= File ->
            locate_loop(T, Path, File, [SubFile | List]);
        _ ->
            locate_loop(T, Path, File, List)
    end.

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

%%%==================================================================
%%% regexp
%%%==================================================================
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



%%%==================================================================
%%% administrator plant
%%%==================================================================
%% user/log/configure data view(ok)
%% statistic(active/charge/user new or lost)
%% user manager(mail/forbid/login/chat)
%% tool(configure data hot load)
%% admin(user/privileges)
%% open/merge server
%% 

%%%==================================================================
%%% architecture plant
%%%==================================================================
%% 开发/部署脚本(ok)

%% 基础网络tcp/http(ok)
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
%% 支付
%% 机器人

%% 战场(ok)
%% 副本(ok)

%% 属性(ok)
%% 技能(ok)
%% buff(ok)
%% 效果(ok)
%% 地图(ok)
%% 怪物AI(ok)



%%%==================================================================
%%% important
%%%==================================================================
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
%%%==================================================================
%%% important
%%%==================================================================
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
%% effect auto/manual
%% monster ai
%% robot
%% module test unit
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

%%%-------------------------------------------------------------------
%%% @doc
%%% test code
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
-include("../../../include/net.hrl").
-include("../../../include/notice.hrl").
-include("../../../include/online.hrl").
-include("../../../include/protocol.hrl").
-include("../../../include/quest.hrl").
-include("../../../include/rank.hrl").
-include("../../../include/recharge.hrl").
-include("../../../include/role.hrl").
-include("../../../include/serialize.hrl").
-include("../../../include/shop.hrl").
-include("../../../include/sign.hrl").
-include("../../../include/skill.hrl").
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
s(A) ->
    sys:get_state(erlang:whereis(A)).

t(T) ->
    ets:tab2list(T).

%% list processes
ls() ->
    [io:format("~w~s~w~n", [Pid, lists:duplicate(32 - length(pid_to_list(Pid)), " "), tool:default(erlang:process_info(Pid, registered_name), proplists:get_value('$initial_call', element(2, erlang:process_info(Pid, dictionary)), erlang:process_info(Pid, initial_call)))]) || Pid <- lists:sort(erlang:processes())],
    ok.

lsp() ->
    [io:format("~w~s~w~n", [X, lists:duplicate(32 - length(atom_to_list(X)), " "), erlang:whereis(X)]) || X <- lists:sort(erlang:registered())],
    ok.

format_pid(Pid) ->
    lists:concat(["#Pid", re:replace(erlang:pid_to_list(Pid), "(?<=<)\\d+", erlang:atom_to_list(node(Pid)), [{return,list}])]).

%% make truncate table sentence
%% SELECT CONCAT('TRUNCATE TABLE `', `TABLE_SCHEMA`, '`.`', `TABLE_NAME`, '`;') FROM information_schema.`TABLES` WHERE `TABLE_SCHEMA` IN (DATABASE())
%% SELECT CONCAT('TRUNCATE TABLE `', `TABLE_SCHEMA`, '`.`', `TABLE_NAME`, '`;') FROM information_schema.`TABLES` WHERE `TABLE_SCHEMA` IN (DATABASE()) AND `TABLE_NAME` NOT LIKE '%_data'
%%
%%
%% local.sql
%% mysqldump --host=127.0.0.1 --user=root --password=root local > script/sql/local.sql
%%
%% open.sql
%% mysqldump --host=127.0.0.1 --user=root --password=root --no-data --compact --add-drop-table local | sed 's/\bAUTO_INCREMENT=[0-9]*\s*//g' > script/sql/open.sql
%%
%% sql test
%% [db:select(<<"SELECT * FROM `", Table/binary, "`">>) || [Table] <- db:select("SHOW TABLES")]
%%
%%

%% @doc query without primary key table
%% SELECT `TABLE_NAME`, COUNT(IF(`COLUMN_KEY` = 'PRI', `COLUMN_KEY`, NULL)) AS `KEY_NUMBER` FROM information_schema.`COLUMNS` WHERE `TABLE_SCHEMA` = DATABASE () GROUP BY `TABLE_NAME` HAVING `KEY_NUMBER` = 0
%% @doc query auto_increment not bigint
%% SELECT `TABLE_NAME`, `COLUMN_NAME` FROM information_schema.`COLUMNS` WHERE `TABLE_SCHEMA` = DATABASE () AND `EXTRA` = 'auto_increment' AND `DATA_TYPE` != 'bigint'
%% @doc query non bigint auto increment ref field
%% SELECT `TABLE_NAME`, `COLUMN_NAME`, `DATA_TYPE` FROM information_schema.`COLUMNS` WHERE `TABLE_SCHEMA` IN ( 'local' ) AND `TABLE_NAME` NOT LIKE '%_data' AND `COLUMN_NAME` IN (SELECT `COLUMN_NAME` AS `NAME` FROM information_schema.`COLUMNS` WHERE `TABLE_SCHEMA` = DATABASE() AND `TABLE_NAME` NOT LIKE '%_data' AND `EXTRA` = 'AUTO_INCREMENT' GROUP BY `TABLE_NAME`) AND `DATA_TYPE` != 'bigint' AND `IS_GENERATED` = 'NEVER' ORDER BY `TABLE_NAME`, `ORDINAL_POSITION`
%% @doc query non compressed log table
%% SELECT `TABLE_NAME`, `TABLE_COMMENT`, `ROW_FORMAT` FROM information_schema.`TABLES` WHERE `TABLE_SCHEMA` = DATABASE() AND `TABLE_NAME` LIKE '%_log' AND `ROW_FORMAT` != 'Compressed'
%% @doc query non log table with compressed
%% SELECT `TABLE_NAME`, `TABLE_COMMENT`, `ROW_FORMAT` FROM information_schema.`TABLES` WHERE `TABLE_SCHEMA` = DATABASE() AND `ROW_FORMAT` = 'Compressed' AND `TABLE_NAME` NOT LIKE '%_log'
%% @doc duplicate comment table
%% SELECT `TABLE_NAME`, `TABLE_COMMENT` FROM information_schema.`TABLES` WHERE `TABLE_SCHEMA` = DATABASE() AND `TABLE_COMMENT` IN ( SELECT `TABLE_COMMENT` FROM information_schema.`TABLES` WHERE `TABLE_SCHEMA` = DATABASE() GROUP BY `TABLE_COMMENT` HAVING COUNT(*) > 1 ) ORDER BY `TABLE_COMMENT`
%% @doc query non virtual field default
%% SELECT `TABLE_NAME`, `COLUMN_NAME` FROM information_schema.`COLUMNS` WHERE `TABLE_SCHEMA` = DATABASE () AND `COLUMN_DEFAULT` != 'NULL' AND `IS_GENERATED` != 'NEVER'
%%

%% find script/ -name "*.erl" ! -name "*_protocol.erl" ! -name "*_sql.erl" ! -name "*_handler.erl" ! -name "*_data.erl" | xargs wc -l
%% find src/ -name "*.erl" ! -name "*_protocol.erl" ! -name "*_sql.erl" ! -name "*_handler.erl" ! -name "*_data.erl" | xargs wc -l
%% find src/ -name "*.erl" | xargs wc -l
%%%===================================================================
%%% robot test
%%%===================================================================
-record(state, {active = [], down = [], progress = [], timer}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

active() ->
    gen_server:call(?MODULE, active).

down() ->
    gen_server:call(?MODULE, down).

progress() ->
    gen_server:call(?MODULE, progress).

timer() ->
    gen_server:call(?MODULE, timer).

init(_) ->
    erlang:process_flag(trap_exit, true),
    List = [{type:to_list(X), undefined} || X <- lists:seq(1, 100)],
    Timer = erlang:send_after(1000 * randomness:rand(1, 10), self(), {loop, active, listing:random(List)}),
    {ok, #state{active = [], down = List, progress = [], timer = Timer}}.

handle_call(active, _From, State = #state{active = Active}) ->
    {reply, Active, State};

handle_call(down, _From, State = #state{down = Down}) ->
    {reply, Down, State};

handle_call(progress, _From, State = #state{progress = Progress}) ->
    {reply, Progress, State};

handle_call(timer, _From, State = #state{timer = Timer}) ->
    {reply, catch erlang:read_timer(Timer), State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info({loop, active, {N, _}}, State = #state{active = Active, down = Down, progress = Progress}) ->
    try
        case robot:start_link(N) of {ok, Pid} -> Pid = Pid; {error, {already_started, Pid}} -> Pid = Pid end,
        NewActive =  [{N, Pid} | Active],
        NewDown = lists:keydelete(N, 1, Down),
        case listing:random(lists:duplicate(length(NewActive), down) ++ lists:duplicate(length(NewDown), active)) of
            active when NewDown =/= [] ->
                Timer = erlang:send_after(1000 * randomness:rand(1, 10), self(), {loop, active, listing:random(NewDown)});
            active ->
                Timer = erlang:send_after(1000 * randomness:rand(1, 10), self(), {loop, down, listing:random(Active)});
            down when Active =/= [] ->
                Timer = erlang:send_after(1000 * randomness:rand(1, 10), self(), {loop, down, listing:random(Active)});
            down ->
                Timer = erlang:send_after(1000, self(), stop)
        end,
        {noreply, State#state{active = NewActive, down = NewDown, progress = [{active, N} | Progress], timer = Timer}}
    catch ?EXCEPTION(_Class, Reason, Stacktrace) ->
        ?STACKTRACE(Reason, ?GET_STACKTRACE(Stacktrace)),
        {stop, normal, State}
    end;

handle_info({loop, down, {N, Pid}}, State = #state{active = Active, down = Down, progress = Progress}) ->
    try
        gen_server:stop(Pid),
        NewActive = lists:keydelete(N, 1, Active),
        NewDown = [{N, undefined} | Down],
        case listing:random(lists:duplicate(length(NewActive), down) ++ lists:duplicate(length(NewDown), active)) of
            active when Down =/= [] ->
                Timer = erlang:send_after(1000 * randomness:rand(1, 10), self(), {loop, active, listing:random(Down)});
            active ->
                Timer = erlang:send_after(1000 * randomness:rand(1, 10), self(), {loop, down, listing:random(NewActive)});
            down when NewActive =/= [] ->
                Timer = erlang:send_after(1000 * randomness:rand(1, 10), self(), {loop, down, listing:random(NewActive)});
            down ->
                Timer = erlang:send_after(1000, self(), stop)
        end,
        {noreply, State#state{active = NewActive, down = NewDown, progress = [{down, N} | Progress], timer = Timer}}
    catch ?EXCEPTION(_Class, Reason, Stacktrace) ->
        ?STACKTRACE(Reason, ?GET_STACKTRACE(Stacktrace)),
        {stop, normal, State}
    end;

handle_info(stop, State) ->
    {stop, normal, State};

handle_info({'EXIT', _, _}, State) ->
    {noreply, State};

handle_info(_Request, State) ->
    io:format("~p~n", [_Request]),
    {noreply, State}.

terminate(_Reason, State) ->
    {ok, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%%===================================================================
%%% User data test
%%%===================================================================
u() ->
    %% load
    LoadedUser = user_loop:load(#user{role_id = 1, sender_pid = self()}),
    %% reset and clean
    USER = user_loop:loop(LoadedUser, 1, 0, time:now()),
    %% list type
    {ok, Role} = user_router:write(?PROTOCOL_ROLE_QUERY, USER#user.role),
    {ok, Asset} = user_router:write(?PROTOCOL_ROLE_ASSET_QUERY, USER#user.asset),
    {ok, Item} = user_router:write(?PROTOCOL_ITEM_QUERY_ITEM, USER#user.item),
    {ok, Bag} = user_router:write(?PROTOCOL_ITEM_QUERY_BAG, USER#user.bag),
    {ok, Body} = user_router:write(?PROTOCOL_ITEM_QUERY_ITEM, USER#user.body),
    {ok, Store} = user_router:write(?PROTOCOL_ITEM_QUERY_STORE, USER#user.store),
    {ok, Mail} = user_router:write(?PROTOCOL_MAIL_QUERY, USER#user.mail),
    {ok, Quest} = user_router:write(?PROTOCOL_QUEST_QUERY, USER#user.quest),
    {ok, Shop} = user_router:write(?PROTOCOL_SHOP_QUERY, USER#user.shop),
    {ok, Friend} = user_router:write(?PROTOCOL_FRIEND_QUERY, USER#user.friend),
    {ok, Buff} = user_router:write(?PROTOCOL_BUFF_QUERY, USER#user.buff),
    {ok, Skill} = user_router:write(?PROTOCOL_SKILL_QUERY, USER#user.skill),
    {ok, Title} = user_router:write(?PROTOCOL_TITLE_QUERY, USER#user.title),
    {ok, Dungeon} = user_router:write(?PROTOCOL_DUNGEON_QUERY, USER#user.dungeon),
    %% no storage type
    {ok, Chat} = user_router:write(?PROTOCOL_CHAT_WORLD, [ok, 1, <<"1">>, <<"1">>]),
    %% ets share list type
    {ok, Rank} = user_router:write(19000 + 1, element(2, rank_server:query(19000 + 1))),
    %% {ok, Rank} = user_router:write(19100 + 1, element(2, rank_server:query(19200 + 1))),
    %% {ok, Rank} = user_router:write(19200 + 1, element(2, rank_server:query(19200 + 1))),
    %% ets type
    {ok, LuckyMoney} = user_router:write(?PROTOCOL_WELFARE_QUERY_LUCKY_MONEY, element(2, lucky_money_server:query())),
    {ok, Auction} = user_router:write(?PROTOCOL_AUCTION_QUERY, element(2, auction_server:query())),
    {ok, GuildList} = user_router:write(?PROTOCOL_GUILD_QUERY_GUILD, element(2, guild_server:query_guild())),
    {ok, RoleList} = user_router:write(?PROTOCOL_GUILD_QUERY_ROLE, element(2, guild_server:query_role(USER))),
    {ok, ApplyList} = user_router:write(?PROTOCOL_GUILD_QUERY_APPLY, element(2, guild_server:query_apply(USER))),
    {ok, SelfGuildList} = user_router:write(?PROTOCOL_GUILD_QUERY_SELF_GUILD, element(2, guild_server:query_self_guild(USER))),
    {ok, SelfRoleList} = user_router:write(?PROTOCOL_GUILD_QUERY_SELF_ROLE, element(2, guild_server:query_self_role(USER))),
    {ok, SelfApplyList} = user_router:write(?PROTOCOL_GUILD_QUERY_SELF_APPLY, element(2, guild_server:query_self_apply(USER#user{role_id = 3}))),
    %% output
    io:format("~p~n", [[Role, Asset, Item, Bag, Body, Store, Mail, Quest, Shop, Friend, Buff, Skill, Title, Dungeon, Chat, Rank, LuckyMoney, Auction, GuildList, RoleList, ApplyList, SelfGuildList, SelfRoleList, SelfApplyList]]),
    %% return
    USER.

%%%===================================================================
%%% User Socket Event Test
%%%===================================================================
id(Name) when is_list(Name) orelse is_binary(Name) ->
    Binary = list_to_binary(encoding:to_list(Name)),
    db:select_one(io_lib:format("SELECT `role_id` FROM `role` WHERE role_name = '~s' OR `account` = '~s'", [Binary, Binary]));
id(Id) ->
    Id.

pid(Id) ->
    user_server:pid(id(Id)).

send(Id, Protocol, Data) ->
    case pid(Id) of
        Pid when is_pid(Pid) ->
            user_server:socket_event(Pid, Protocol, Data);
        Other ->
            Other
    end.

%%%===================================================================
%%% map test
%%%===================================================================




%%%===================================================================
%%% parser test
%%%===================================================================
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
%%% randomness test
%%%===================================================================
test_randomness() ->
    F = fun(_) -> test_randomness_loop(lists:duplicate(1000, 0), dict:new()) end,
    All = misc:map_reduce(F, lists:seq(1, 1000)),
    String = lists:flatten(["[" ++ string:join([io_lib:format("{~p:~p}", [X, N]) || {X, N} <- List], ", ") ++ "]\n" || List <- All]),
    file:write_file("sample.json", String).

test_randomness_loop([], Dict) ->
    lists:sort(dict:to_list(Dict));
test_randomness_loop([_ | T], Dict) ->
    X = randomness:rand(),
    test_randomness_loop(T, dict:update_counter(X, 1, Dict)).

ac(X) ->
    activity:continue(#activity{show_time = 10, start_time = 10, over_time = 30, award_time = 30, stop_time = 30}, X).


%%%===================================================================
%%% sorter test
%%%===================================================================
tx() ->
    SortList = [
        #rank{type = 1, key = 1, value = 1, order = 2},
        #rank{type = 1, key = 1, value = 1, order = 3},
        #rank{type = 1, key = 1, value = 1, order = 1},
        #rank{type = 1, key = 1, value = 1, order = 4},
        #rank{type = 1, key = 1, value = 1, order = 5}
    ],
    Sorter = sorter:new(wow, share, replace, 100, #rank.key, #rank.value, #rank.time, #rank.order, SortList),
    sorter:update(#rank{type = 1, order = 0}, Sorter),
    sorter:data(Sorter).

%%%-------------------------------------------------------------------
%%! +pc unicode
%%% @doc
%%% test code
%%% @end
%%%-------------------------------------------------------------------
-module(test).
-mode(compile).
-compile(nowarn_export_all).
-compile(export_all).
-include("../../../include/achievement.hrl").
-include("../../../include/activity.hrl").
-include("../../../include/asset.hrl").
-include("../../../include/attribute.hrl").
-include("../../../include/auction.hrl").
-include("../../../include/boss.hrl").
-include("../../../include/bubble.hrl").
-include("../../../include/buff.hrl").
-include("../../../include/chat.hrl").
-include("../../../include/common.hrl").
-include("../../../include/count.hrl").
-include("../../../include/daily.hrl").
-include("../../../include/dungeon.hrl").
-include("../../../include/effect.hrl").
-include("../../../include/event.hrl").
-include("../../../include/fashion.hrl").
-include("../../../include/friend.hrl").
-include("../../../include/guild.hrl").
-include("../../../include/item.hrl").
-include("../../../include/journal.hrl").
-include("../../../include/key.hrl").
-include("../../../include/lucky_money.hrl").
-include("../../../include/mail.hrl").
-include("../../../include/map.hrl").
-include("../../../include/monster.hrl").
-include("../../../include/net.hrl").
-include("../../../include/notice.hrl").
-include("../../../include/online.hrl").
-include("../../../include/protocol.hrl").
-include("../../../include/rank.hrl").
-include("../../../include/charge.hrl").
-include("../../../include/role.hrl").
-include("../../../include/serialize.hrl").
-include("../../../include/shop.hrl").
-include("../../../include/sign.hrl").
-include("../../../include/skill.hrl").
-include("../../../include/sorter.hrl").
-include("../../../include/task.hrl").
-include("../../../include/time.hrl").
-include("../../../include/title.hrl").
-include("../../../include/user.hrl").
-include("../../../include/vip.hrl").
-include_lib("xmerl/include/xmerl.hrl").
%% ms
-include_lib("stdlib/include/ms_transform.hrl").
%%%===================================================================
%%% API functions
%%%===================================================================
%% @doc escript entry
main(Env) ->
    catch code:add_path(filename:dirname(escript:script_name()) ++ "/../../../beam/"),
    io:setopts([{encoding, unicode}]),

    timer:sleep(300),
    io:format("Env: ~p~n", [Env]).



%% process state
s(A) ->
    sys:get_state(erlang:whereis(A)).

t(T) ->
    ets:tab2list(T).

%% process registered name
rn(Pid) ->
    lists:concat([element(2, hd(erlang:process_info(Pid, [registered_name])))]).

%% process initial call
ic(Pid) ->
    {M, F, A} = proplists:get_value('$initial_call', element(2, erlang:process_info(Pid, dictionary)), element(2, erlang:process_info(Pid, initial_call))),
    lists:concat([M, ":", F, "/", A]).

%% statistics memory
sm() ->
    io:format("~-48s~s~n", ["Type", "Memory"]),
    io:format("================================================================~n"),
    [io:format("~-48s~s~n", [Name, h(Memory)]) || {Name, Memory} <- lists:reverse(lists:keysort(2, erlang:memory()))],
    %% services
    ServiceList = [{Name, element(2, hd(erlang:process_info(Pid, [memory])))}|| {Name, Pid, _, _} <- supervisor:which_children(service_supervisor)],
    ServiceTotal = lists:sum([element(2, P) || P <- ServiceList]),
    RoleList = [{Name, element(2, hd(erlang:process_info(erlang:whereis(Name), [memory])))} || Name <- erlang:registered(), string:str(erlang:atom_to_list(Name), "role") =/= 0],
    RoleTotal = lists:sum([element(2, P) || P <- RoleList]),
    io:format("~n"),
    io:format("================================================================~n"),
    io:format("~-48s~s~n", ["services", h(ServiceTotal)]),
    io:format("~-48s~s~n", ["roles", h(RoleTotal)]),
    ok.

%% statistics services memory
ssm() ->
    List = [{Name, element(2, hd(erlang:process_info(Pid, [memory])))}|| {Name, Pid, _, _} <- supervisor:which_children(service_supervisor)],
    Total = lists:sum([element(2, P) || P <- List]),
    io:format("~-48s~s(Total: ~s)~n", ["Name", "Memory", h(Total)]),
    io:format("================================================================~n"),
    [io:format("~-48s~-48s~n", [Name, h(Memory)]) || {Name, Memory} <- lists:reverse(lists:keysort(2, List))],
    ok.

ssm(N) ->
    List = [{Name, element(2, hd(erlang:process_info(Pid, [memory])))}|| {Name, Pid, _, _} <- supervisor:which_children(service_supervisor)],
    Total = lists:sum([element(2, P) || P <- List]),
    io:format("~-48s~s(Total: ~s)~n", ["Name", "Memory", h(Total)]),
    io:format("================================================================~n"),
    [io:format("~-48s~-48s~n", [Name, h(Memory)]) || {Name, Memory} <- lists:sublist(lists:reverse(lists:keysort(2, List)), N)],
    ok.

%% statistics role memory
ssr() ->
    List = [{Name, element(2, hd(erlang:process_info(erlang:whereis(Name), [memory])))} || Name <- erlang:registered(), string:str(erlang:atom_to_list(Name), "role") =/= 0],
    Total = lists:sum([element(2, P) || P <- List]),
    io:format("~-48s~s(Total: ~s)~n", ["Name", "Memory", h(Total)]),
    io:format("================================================================~n"),
    [io:format("~-48s~-48s~n", [Name, h(Memory)]) || {Name, Memory} <- lists:reverse(lists:keysort(2, List))],
    ok.

ssr(N) ->
    List = [{Name, element(2, hd(erlang:process_info(erlang:whereis(Name), [memory])))} || Name <- erlang:registered(), string:str(erlang:atom_to_list(Name), "role") =/= 0],
    Total = lists:sum([element(2, P) || P <- List]),
    io:format("~-48s~s(Total: ~s)~n", ["Name", "Memory", h(Total)]),
    io:format("================================================================~n"),
    [io:format("~-48s~-48s~n", [Name, h(Memory)]) || {Name, Memory} <- lists:sublist(lists:reverse(lists:keysort(2, List)), N)],
    ok.

mm(Pid) ->
    h(element(2, erlang:process_info(Pid, memory))).

h(Size) when Size < 1024 ->
    io_lib:format("~w B", [Size]);
h(Size) when Size < (1024 * 1024) ->
    io_lib:format("~.2f KiB", [Size / 1024]);
h(Size) when Size < (1024 * 1024 * 1024) ->
    io_lib:format("~.2f MiB", [Size / (1024 * 1024)]).

%% list processes
ls() ->
    Total = lists:sum([element(2, erlang:process_info(Pid, memory)) || Pid <- erlang:processes()]),
    io:format("~-48s~-48s~-64s~s(Total:~s)~n", ["Pid", "Name", "Function", "Memory", h(Total)]),
    io:format("================================================================================~n"),
    [io:format("~-48s~-48s~-64s~s~n", [format_pid(Pid), rn(Pid), ic(Pid), mm(Pid)]) || Pid <- lists:sort(erlang:processes())],
    ok.

format_pid(Pid) ->
    io_lib:format("(~s)~w", [node(Pid), Pid]).

%% trace user protocol
tp() ->
    tp(0).
tp(Protocol) ->
    tp(Protocol, 0).
tp(Protocol, Id) ->
    %% stop previous
    %% dbg:stop_clear(),
    %% must stop tracer after use it
    Pid = proplists:get_value(Id, [{0, all}, {Id, user_server:pid(Id)}]),
    dbg:tracer(process, {fun trace_handler/2, {Protocol, Id}}),
    dbg:p(Pid, [m]).

%% all user all protocol
trace_handler({trace, Self, 'receive', {'$gen_cast', {socket_event, Protocol, Data}}}, {0, 0} = Parameter) ->
    journal:format("Receive User:~0p Protocol:~0p Data:~0p~n", [rn(Self), Protocol, Data]),
    Parameter;

%% all user spec protocol
trace_handler({trace, Self, 'receive', {'$gen_cast', {socket_event, Protocol, Data}}}, {Protocol, 0} = Parameter) ->
    journal:format("Receive User:~0p Protocol:~0p Data:~0p~n", [rn(Self), Protocol, Data]),
    Parameter;

%% spec use all protocol
trace_handler({trace, Self, 'receive', {'$gen_cast', {socket_event, Protocol, Data}}}, {0, Id} = Parameter) ->
    user_server:pid(Id) == Self andalso journal:format("Receive User:~0p Protocol:~0p Data:~0p~n", [rn(Self), Protocol, Data]),
    Parameter;

%% spec user spec protocol
trace_handler({trace, Self, 'receive', {'$gen_cast', {socket_event, Protocol, Data}}}, {Protocol, Id} = Parameter) ->
    user_server:pid(Id) == Self andalso journal:format("Receive User:~0p Protocol:~0p Data:~0p~n", [rn(Self), Protocol, Data]),
    Parameter;

%% spec use all protocol
trace_handler({trace, Self, 'send', {'$gen_cast', {send, Data}}, _}, {0, Id} = Parameter) ->
    user_server:pid(Id) == Self andalso journal:format("Send User:~0p Protocol:~0p Data:~0p~n", [rn(Self), 0, Data]),
    Parameter;

%% spec user spec protocol
trace_handler({trace, Self, 'send', {'$gen_cast', {send, Data}}, _}, {Protocol, Id} = Parameter) ->
    user_server:pid(Id) == Self andalso journal:format("Send User:~0p Protocol:~0p Data:~0p~n", [rn(Self), Protocol, Data]),
    Parameter;

trace_handler(_, Parameter) ->
    Parameter.


%% Time = 9223372036000000000 - trunc((erlang:monotonic_time() - erlang:system_info(start_time)) / 1000 / 1000),
%% erlang:send_after(9223372036000, self(), ok),
%% Time = trunc((erlang:monotonic_time() - erlang:system_info(start_time)) / 1000 / 1000),
%% erlang:send_after(9223372036000 - Time - 1000 - 1, self(), ok),
%% {9223372036000 - Time - 1000 - 1, Time}.
%% 9223372036000
%% 8796093022208
%% erlang:send_after(9223372036000 - trunc((erlang:monotonic_time() - erlang:system_info(start_time)) / 1000 / 1000) - 1, self(), ok).
na() ->
    Atomics = atomics:new(1, [{signed, false}]),
    atomics:put(Atomics, 1, 0),
    persistent_term:put(?MODULE, Atomics),
    Atomics.

tss() ->
    Time = time:millisecond() - 1577808000000,
    <<Id:64>> = <<1:16, Time:40, 0:8>>,
    Id.

%% local.sql
%% mysqldump --host=127.0.0.1 --user=root --password=root local > script/sql/local.sql
%%
%% open.sql
%% mysqldump --host=127.0.0.1 --user=root --password=root --no-data --compact --add-drop-table local | sed 's/\bAUTO_INCREMENT=[0-9]*\s*//g' > script/sql/open.sql
%%



%% find script/ -name "*.erl" ! -name "*_protocol.erl" ! -name "*_sql.erl" ! -name "*_handler.erl" ! -name "*_data.erl" | xargs wc -l
%% find src/ -name "*.erl" ! -name "*_protocol.erl" ! -name "*_sql.erl" ! -name "*_handler.erl" ! -name "*_data.erl" | xargs wc -l
%% find src/ -name "*.erl" | xargs wc -l
%%%===================================================================
%%% robot test
%%%===================================================================
-define(ROBOT_NUMBER, 10000).
-record(state, {active = [], down = [], progress = [], timer}).

trb() ->
    process:start(?MODULE).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [robot], []).

start_user() ->
    gen_server:start({local, ?MODULE}, ?MODULE, [user], []).

resize(Size) ->
    gen_server:call(?MODULE, {resize, Size}).

active() ->
    gen_server:call(?MODULE, active).

down() ->
    gen_server:call(?MODULE, down).

progress() ->
    gen_server:call(?MODULE, progress).

timer() ->
    gen_server:call(?MODULE, timer).

init([robot]) ->
    erlang:process_flag(trap_exit, true),
    List = [{type:to_list(X), undefined} || X <- lists:seq(1, ?ROBOT_NUMBER)],
    Timer = erlang:send_after(?ROBOT_NUMBER * randomness:rand(1, 10), self(), {loop, active, listing:random(List)}),
    {ok, #state{active = [], down = List, progress = [], timer = Timer}};
init([user]) ->
    {ok, uf(1), ?SECOND_MILLISECONDS}.

handle_call({resize, New}, _From, State = #state{active = Active, down = Down}) ->
    Old = length(Active) + length(Down),
    case New > Old of
        true ->
            List = [{type:to_list(X), undefined} || X <- lists:seq(Old, New)],
            {reply, New - Old, State#state{down = Down ++ List}};
        false ->
            NewActive = [{Id, Pid} || {Id, Pid} <- Active, (Id > New andalso gen_server:stop(Pid) =/= ok) orelse Id =< New],
            NewDown = [{Id, Pid} || {Id, Pid} <- Down, Id =< New],
            {reply, New - Old, State#state{active = NewActive, down = NewDown}}
    end;

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
                Timer = erlang:send_after(?ROBOT_NUMBER * randomness:rand(1, 10), self(), {loop, active, listing:random(NewDown)});
            active ->
                Timer = erlang:send_after(?ROBOT_NUMBER * randomness:rand(1, 10), self(), {loop, down, listing:random(Active)});
            down when Active =/= [] ->
                Timer = erlang:send_after(?ROBOT_NUMBER * randomness:rand(1, 10), self(), {loop, down, listing:random(Active)});
            down ->
                Timer = erlang:send_after(?ROBOT_NUMBER, self(), stop)
        end,
        {noreply, State#state{active = NewActive, down = NewDown, progress = [{active, N} | Progress], timer = Timer}}
    catch ?EXCEPTION(Class, Reason, Stacktrace) ->
        ?STACKTRACE(Class, Reason, ?GET_STACKTRACE(Stacktrace)),
        {stop, normal, State}
    end;

handle_info({loop, down, {N, Pid}}, State = #state{active = Active, down = Down, progress = Progress}) ->
    try
        erlang:is_process_alive(Pid) andalso gen_server:stop(Pid),
        NewActive = lists:keydelete(N, 1, Active),
        NewDown = [{N, undefined} | Down],
        case listing:random(lists:duplicate(length(NewActive), down) ++ lists:duplicate(length(NewDown), active)) of
            active when Down =/= [] ->
                Timer = erlang:send_after(?ROBOT_NUMBER * randomness:rand(1, 10), self(), {loop, active, listing:random(Down)});
            active ->
                Timer = erlang:send_after(?ROBOT_NUMBER * randomness:rand(1, 10), self(), {loop, down, listing:random(NewActive)});
            down when NewActive =/= [] ->
                Timer = erlang:send_after(?ROBOT_NUMBER * randomness:rand(1, 10), self(), {loop, down, listing:random(NewActive)});
            down ->
                Timer = erlang:send_after(?ROBOT_NUMBER, self(), stop)
        end,
        {noreply, State#state{active = NewActive, down = NewDown, progress = [{down, N} | Progress], timer = Timer}}
    catch ?EXCEPTION(Class, Reason, Stacktrace) ->
        ?STACKTRACE(Class, Reason, ?GET_STACKTRACE(Stacktrace)),
        {stop, normal, State}
    end;

handle_info(stop, State) ->
    {stop, normal, State};

handle_info({'EXIT', Pid, _}, State = #state{active = Active}) ->
    NewActive = lists:keydelete(Pid, 2, Active),
    {noreply, State#state{active = NewActive}};

handle_info(timeout, State) ->
    NewState = user_event:trigger(State, #event{name = save}),
    {noreply, NewState, ?SECOND_MILLISECONDS};

handle_info(Request, State) ->
    ?PRINT("Request:~p~n", [Request]),
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
    LoadedUser = user_event:trigger(#user{role_id = 1, sender_pid = self()}, #event{name = load}),
    %% reset and clean
    USER = user_loop:loop(LoadedUser, 1, 0, time:now()),
    %% list type
    {ok, Role} = user_router:encode(?PROTOCOL_ROLE_QUERY, USER#user.role),
    {ok, Asset} = user_router:encode(?PROTOCOL_ROLE_ASSET_QUERY, USER#user.asset),
    {ok, Item} = user_router:encode(?PROTOCOL_ITEM_QUERY_ITEM, USER#user.item),
    {ok, Bag} = user_router:encode(?PROTOCOL_ITEM_QUERY_BAG, USER#user.bag),
    {ok, Body} = user_router:encode(?PROTOCOL_ITEM_QUERY_ITEM, USER#user.body),
    {ok, Store} = user_router:encode(?PROTOCOL_ITEM_QUERY_STORE, USER#user.store),
    {ok, Mail} = user_router:encode(?PROTOCOL_MAIL_QUERY, USER#user.mail),
    {ok, Task} = user_router:encode(?PROTOCOL_TASK_QUERY, USER#user.task),
    {ok, Shop} = user_router:encode(?PROTOCOL_SHOP_QUERY, USER#user.shop),
    {ok, Friend} = user_router:encode(?PROTOCOL_FRIEND_QUERY, USER#user.friend),
    {ok, Buff} = user_router:encode(?PROTOCOL_BUFF_QUERY, USER#user.buff),
    {ok, Skill} = user_router:encode(?PROTOCOL_SKILL_QUERY, USER#user.skill),
    {ok, Title} = user_router:encode(?PROTOCOL_TITLE_QUERY, USER#user.title),
    {ok, Dungeon} = user_router:encode(?PROTOCOL_DUNGEON_QUERY, USER#user.dungeon),
    %% no storage type
    {ok, Chat} = user_router:encode(?PROTOCOL_CHAT_WORLD, [ok, 1, <<"1">>, <<"1">>]),
    %% ets share list type
    {ok, Rank} = user_router:encode(19000 + 1, element(2, rank_server:query(USER, 19000 + 1))),
    %% {ok, Rank} = user_router:encode(19100 + 1, element(2, rank_server:query(19200 + 1))),
    %% {ok, Rank} = user_router:encode(19200 + 1, element(2, rank_server:query(19200 + 1))),
    %% ets type
    {ok, LuckyMoney} = user_router:encode(?PROTOCOL_WELFARE_QUERY_LUCKY_MONEY, element(2, lucky_money_server:query(USER, ets:first(lucky_money_server)))),
    {ok, Auction} = user_router:encode(?PROTOCOL_AUCTION_QUERY, element(2, auction_server:query(USER))),
    {ok, GuildList} = user_router:encode(?PROTOCOL_GUILD_QUERY_GUILD, element(2, guild_server:query_guild(USER))),
    {ok, RoleList} = user_router:encode(?PROTOCOL_GUILD_QUERY_ROLE, element(2, guild_server:query_role(USER))),
    {ok, ApplyList} = user_router:encode(?PROTOCOL_GUILD_QUERY_APPLY, element(2, guild_server:query_apply(USER))),
    {ok, SelfGuildList} = user_router:encode(?PROTOCOL_GUILD_QUERY_SELF_GUILD, element(2, guild_server:query_self_guild(USER))),
    {ok, SelfRoleList} = user_router:encode(?PROTOCOL_GUILD_QUERY_SELF_ROLE, element(2, guild_server:query_self_role(USER))),
    {ok, SelfApplyList} = user_router:encode(?PROTOCOL_GUILD_QUERY_SELF_APPLY, element(2, guild_server:query_self_apply(USER#user{role_id = 3}))),
    %% output
    io:format("~p~n", [[Role, Asset, Item, Bag, Body, Store, Mail, Task, Shop, Friend, Buff, Skill, Title, Dungeon, Chat, Rank, LuckyMoney, Auction, GuildList, RoleList, ApplyList, SelfGuildList, SelfRoleList, SelfApplyList]]),
    %% return
    USER.

uf(Id) ->
    db:insert("INSERT IGNORE INTO `role` (`role_id`, `role_name`) VALUES (?, ?)", [Id, integer_to_binary(Id)]),
    Number = 100,
    User = #user{
        role = #role{role_id = Id, role_name = integer_to_binary(Id)},
        asset = #asset{role_id = Id},
        vip = #vip{role_id = Id},
        count = [#count{role_id = Id, type = I} || I <- lists:seq(1, Number)],
        item = [#item{role_id = Id, item_no = I} || I <- lists:seq(1, Number)],
        bag = [#item{role_id = Id, item_no = I} || I <- lists:seq(1, Number)],
        body = [#item{role_id = Id, item_no = I} || I <- lists:seq(1, Number)],
        store = [#item{role_id = Id, item_no = I} || I <- lists:seq(1, Number)],
        task = [#task{role_id = Id, type = I} || I <- lists:seq(1, Number)],
        achievement = [#achievement{role_id = Id, type = I} || I <- lists:seq(1, Number)],
        shop = [#shop{role_id = Id, shop_id = I} || I <- lists:seq(1, Number)],
        mail = [#mail{role_id = Id, mail_id = I} || I <- lists:seq(1, Number)],
        friend = [#friend{role_id = Id, friend_role_id = I} || I <- lists:seq(1, Number)],
        buff = [#buff{role_id = Id, buff_id = I} || I <- lists:seq(1, Number)],
        skill = [#skill{role_id = Id, skill_id =  I} || I <- lists:seq(1, Number)],
        fashion = [#fashion{role_id = Id, fashion_id = I} || I <- lists:seq(1, Number)],
        title = [#title{role_id = Id, title_id = I} || I <- lists:seq(1, Number)],
        bubble = [#bubble{role_id = Id, bubble_id = I} || I <- lists:seq(1, Number)],
        dungeon = [#dungeon{role_id = Id, dungeon_id = I} || I <- lists:seq(1, Number)],
        daily = [#daily{role_id = Id, daily_id = I} || I <- lists:seq(1, Number)],
        daily_active = #daily_active{role_id = Id},
        sign = #sign{role_id = Id},
        guild = #guild_role{},
        role_id = 0,
        role_name = <<>>,
        sender_pid = undefined,
        loop_timer = undefined,
        node = local,
        total_attribute = #attribute{},
        attributes = [],
        effect = [],
        trigger = []
    },
    user_event:trigger(User, #event{name = save}).

%%%===================================================================
%%% User Socket Event Test
%%%===================================================================
id(Name) when is_list(Name) orelse is_binary(Name) ->
    db:select_one(io_lib:format("SELECT `role_id` FROM `role` WHERE role_name = '~s' OR `account` = '~s'", [Name, Name]));
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
%%% protocol test
%%%===================================================================
tpp() ->
    catch ets:delete(test),
    catch ets:new(test, [named_table, set]),
    List = [{X, X} || X <- lists:seq(1, 1000)],
    ets:insert(test, List),
    Begin = os:timestamp(),
    ets:select(test, ets:fun2ms(fun({XX, _}) -> XX end)),
    Middle = os:timestamp(),
    protocol:write_ets(fun([{X, Y}]) -> <<X:16, Y:16>> end, test),
    End = os:timestamp(),
    protocol:write_list(fun({X, Y}) -> <<X:16, Y:16>> end, List),
    Final = os:timestamp(),
    io:format("S: ~p W:~p L:~p~n", [timer:now_diff(Middle, Begin), timer:now_diff(End, Middle), timer:now_diff(Final, End)]).


%%%===================================================================
%%% parser test
%%%===================================================================
tpf() ->
    io:format("~~p []: "),erlang:display(db:format("?", [[]])),
    io:format("~~s []: "),erlang:display(db:format("?", [[]])),
    io:format("~~w []: "),erlang:display(db:format("?", [[]])),

    io:format("~n~n"),

    io:format("~~p [97,98,99]: "),erlang:display(db:format("?", [[97,98,99]])),
    io:format("~~s [97,98,99]: "),erlang:display(db:format("?", [[97,98,99]])),
    io:format("~~w [97,98,99]: "),erlang:display(db:format("?", [[97,98,99]])),

    io:format("~n~n"),

    io:format("~~p <<>>: "),erlang:display(db:format("?", [<<>>])),
    io:format("~~s <<>>: "),erlang:display(db:format("?", [<<>>])),
    io:format("~~w <<>>: "),erlang:display(db:format("?", [<<>>])),

    io:format("~n~n"),

    io:format("~~p <<97,98,99>>: "),erlang:display(db:format("?", [<<97,98,99>>])),
    io:format("~~s <<97,98,99>>: "),erlang:display(db:format("?", [<<97,98,99>>])),
    io:format("~~w <<97,98,99>>: "),erlang:display(db:format("?", [<<97,98,99>>])),

    ok.


test_collect_list() ->
    L = [{X, randomness:rand(1,100), randomness:rand(1,100), 1} || X <- lists:seq(1, 1000)],
    F = fun() -> db:collect(<<"insert into `test` (`a`, `b`, `c`) values ">>, <<"(?, ?, ?)">>, <<" on duplicate key update `type` = VALUES(`type`), `type` = VALUES(`type`), `type` = VALUES(`type`)">>, L, 0) end,
    timer:tc(F).

test_collect_ets() ->
    catch ets:delete(test),
    catch ets:new(test, [named_table, ordered_set, {keypos, 1}]),
    L = [{X, randomness:rand(1,100), randomness:rand(1,100), 1} || X <- lists:seq(1, 1000)],
    ets:insert(test, L),
    F = fun() -> db:collect(<<"insert into `test` (`a`, `b`, `c`) values ">>, <<"(?, ?, ?)">>, <<" on duplicate key update `type` = VALUES(`type`), `type` = VALUES(`type`), `type` = VALUES(`type`)">>, test, 0) end,
    timer:tc(F).

test_collect_into_list() ->
    L = [{X, randomness:rand(1,100), randomness:rand(1,100), 1} || X <- lists:seq(1, 1000)],
    F = fun() -> db:collect(<<"insert into `test` (`a`, `b`, `c`) values ">>, <<"(?, ?, ?)">>, <<" on duplicate key update `type` = VALUES(`type`), `type` = VALUES(`type`), `type` = VALUES(`type`)">>, L, 4) end,
    timer:tc(F).

test_collect_into_ets() ->
    catch ets:delete(test),
    catch ets:new(test, [named_table, ordered_set, {keypos, 1}]),
    L = [{X, randomness:rand(1,100), randomness:rand(1,100), 1} || X <- lists:seq(1, 1000)],
    ets:insert(test, L),
    F = fun() -> db:collect(<<"insert into `test` (`a`, `b`, `c`) values ">>, <<"(?, ?, ?)">>, <<" on duplicate key update `type` = VALUES(`type`), `type` = VALUES(`type`), `type` = VALUES(`type`)">>, test, 4) end,
    timer:tc(F).

%%%===================================================================
%%% console test
%%%===================================================================
ct() ->
    journal:print(?MODULE, ?LINE, "~s~n", [<<"print">>]),
    journal:debug(?MODULE, ?LINE, "~p~n", [<<"debug">>]),
    journal:info(?MODULE, ?LINE, "~p~n", [info]),
    journal:warning(?MODULE, ?LINE, "~p~n", [warming]),
    journal:error(?MODULE, ?LINE, "~p~n", [error]).

%%%===================================================================
%%% randomness test
%%%===================================================================
test_randomness() ->
    F = fun(_) -> test_randomness_loop(lists:seq(1, 1000), dict:new()) end,
    All = lists:append(misc:map_reduce(F, lists:seq(1, 1000))),
    List = lists:sort(dict:to_list(lists:foldr(fun({K, V}, Dict) -> dict:update_counter(K, V, Dict) end, dict:new(), All))),
    String = "[\n" ++ [string:join([io_lib:format("[~p,~p]", [N, X]) || {X, N} <- List], ",\n")] ++ "\n]",
    %% ChartCube.AliPay.com
    file:write_file("sample.json", String).

test_randomness_loop([], Dict) ->
    lists:sort(dict:to_list(Dict));
test_randomness_loop([_ | T], Dict) ->
    X = randomness:rand(),
    test_randomness_loop(T, dict:update_counter(X, 1, Dict)).

ac(X) ->
    activity:continue(#activity{show_time = 10, start_time = 20, over_time = 30, award_time = 40, stop_time = 50}, X).

acs(X) ->
    activity:continue(#activity{show_time = 10, start_time = 10, over_time = 30, award_time = 30, stop_time = 50}, X).

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


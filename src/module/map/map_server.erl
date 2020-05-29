%%%-------------------------------------------------------------------
%%% @doc
%%% module map server
%%% @end
%%%-------------------------------------------------------------------
-module(map_server).
-behaviour(gen_server).
%% API
-export([start_city/0, start/1, start/2, start_link/2, stop/1, stop/2]).
-export([city_id/0, city_map_no/0, city_pid/0, city/0]).
-export([map_id/1, map_no/2, map_no/1, name/1, pid/1]).
-export([query/1, enter/1, enter/2, leave/1, move/3]).
-export([attack/3]).
-export([apply_call/3, apply_call/4, apply_cast/3, apply_cast/4, apply_delay_cast/4, apply_delay_cast/5]).
-export([pure_call/3, pure_call/4, pure_cast/3, pure_cast/4, pure_delay_cast/4, pure_delay_cast/5]).
-export([call/2, cast/2, info/2]).
-export([field/2, field/3, field/4]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
%% includes
-include("common.hrl").
-include("protocol.hrl").
-include("event.hrl").
-include("user.hrl").
-include("role.hrl").
-include("map.hrl").
%%%===================================================================
%%% API functions
%%%===================================================================
%% @doc start main city
-spec start_city() -> {ok, pid()} | {error, term()}.
start_city() ->
    process:start(?MODULE, [city_id(), city_map_no()]).

%% @doc server start
-spec start(non_neg_integer()) -> #map{}.
start(MapId) ->
    MapNo = map_no(MapId, increment_server:next(map)),
    start(MapId, MapNo).

%% @doc server start
-spec start(non_neg_integer(), non_neg_integer()) -> #map{}.
start(MapId, MapNo) ->
    {ok, Pid} = start_link(MapId, MapNo),
    #map{map_no = MapNo, map_id = MapId, pid = Pid}.

%% @doc server start
-spec start_link(non_neg_integer(), non_neg_integer()) -> {ok, pid()} | {error, term()}.
start_link(MapId, MapNo) ->
    gen_server:start_link({local, name(MapNo)}, ?MODULE, [MapId, MapNo], []).

%% @doc server stop
-spec stop(pid() | non_neg_integer() | atom()) -> ok.
stop(Id) ->
    erlang:send(pid(Id), stop),
    ok.

%% @doc server stop
-spec stop(pid() | non_neg_integer() | atom(), Time :: non_neg_integer()) -> ok.
stop(Id, Time) ->
    erlang:send_after(Time, pid(Id), stop),
    ok.

%% @doc main city map id
-spec city_id() -> non_neg_integer().
city_id() ->
    100000.

%% @doc main city map no
-spec city_map_no() -> non_neg_integer().
city_map_no() ->
    map_no(city_id(), 0).

%% @doc main city map pid
-spec city_pid() -> pid().
city_pid() ->
    pid(name(city_map_no())).

%% @doc main city map
-spec city() -> #map{}.
city() ->
    #map{map_no = city_map_no(), map_id = city_id(), pid = city_pid(), type = city}.

%% @doc map no
-spec map_id(non_neg_integer()) -> non_neg_integer().
map_id(MapNo) ->
    (MapNo div 10000000000).

%% @doc map no
-spec map_no(non_neg_integer(), non_neg_integer()) -> non_neg_integer().
map_no(MapId, Id) ->
    (MapId * 10000000000 + Id).

%% @doc map no
-spec map_no(atom()) -> non_neg_integer().
map_no(Name) ->
    type:to_integer(hd(tl(string:tokens(type:to_list(Name), "_")))).

%% @doc map server name
-spec name(non_neg_integer() | pid()) -> atom().
name(MapNo) when is_integer(MapNo) ->
    type:to_atom(lists:concat(["map_", MapNo]));
name(Pid) when is_pid(Pid) ->
    erlang:element(2, erlang:process_info(Pid, registered_name)).

%% @doc map pid
-spec pid(pid() | non_neg_integer() | atom()) -> pid() | undefined.
pid(Pid) when is_pid(Pid) ->
    Pid;
pid(MapNo) when is_integer(MapNo) ->
    pid(name(MapNo));
pid(Name) when is_atom(Name) ->
    process:pid(Name).

%% @doc query
-spec query(User :: #user{}) -> ok().
query(#user{sender_pid = SenderPid, role = #role{map = #map{pid = Pid}}}) ->
    cast(Pid, {query, SenderPid}).

%% @doc enter map
-spec enter(#user{}) -> #user{}.
enter(User = #user{role = #role{map = Map = #map{map_no = MapNo, map_id = MapId}}}) ->
    Pid = pid(MapNo),
    #map_data{reconnect = Reconnect} = map_data:get(MapId),
    case erlang:is_pid(Pid) of
        true when Reconnect == true ->
            enter(User, Map#map{pid = Pid});
        _ ->
            enter(User, city())
    end;
enter(User) ->
    enter(User, city()).

%% @doc enter map
-spec enter(#user{}, non_neg_integer() | pid() | #map{}) -> #user{}.
enter(User, MapNo) when is_integer(MapNo) ->
    MapId = map_id(MapNo),
    Pid = pid(MapNo),
    Map = #map{map_no = MapNo, map_id = MapId, pid = Pid},
    enter(User, Map);
enter(User, Pid) when is_pid(Pid) ->
    MapNo = map_no(name(Pid)),
    MapId = map_id(MapNo),
    Map = #map{map_no = MapNo, map_id = MapId, pid = Pid},
    enter(User, Map);
enter(User, Map = #map{map_id = MapId, x = 0, y = 0}) ->
    {X, Y} = listing:random((map_data:get(MapId))#map_data.enter_points, {randomness:rand(1, 100), randomness:rand(1, 100)}),
    enter(User, Map#map{x = X, y = Y});
enter(User = #user{role = Role}, Map = #map{pid = Pid}) ->
    NewUser = leave(User),
    FinalUser = NewUser#user{role = Role#role{map = Map}},
    Fighter = user_convert:to(FinalUser, map),
    cast(Pid, {enter, Fighter}),
    FinalUser.

%% @doc leave map
-spec leave(#user{}) -> #user{}.
leave(User = #user{role_id = RoleId, role = Role = #role{map = #map{pid = Pid}}}) ->
    cast(Pid, {leave, RoleId}),
    User#user{role = Role#role{map = []}};
leave(User = #user{role = Role}) ->
    User#user{role = Role#role{map = []}}.

%% @doc move
-spec move(User :: #user{}, X :: non_neg_integer(), Y :: non_neg_integer()) -> ok.
move(#user{role_id = RoleId, role = #role{map = #map{pid = Pid}}}, X, Y) ->
    cast(Pid, {move, RoleId, X, Y}).

%% @doc attack
-spec attack(User :: #user{}, SkillId :: non_neg_integer(), TargetList :: list()) -> ok.
attack(#user{role_id = RoleId, role = #role{map = #map{pid = Pid}}}, SkillId, TargetList) ->
    cast(Pid, {attack, RoleId, SkillId, TargetList}).

%% @doc alert !!!
-spec apply_call(pid() | non_neg_integer(), Function :: atom() | function(), Args :: [term()]) -> term().
apply_call(Id, Function, Args) ->
    gen_server:call(pid(Id), {'APPLY_CALL', Function, Args}).

-spec apply_call(pid() | non_neg_integer(), Module :: atom(), Function :: atom() | function(), Args :: [term()]) -> term().
apply_call(Id, Module, Function, Args) ->
    gen_server:call(pid(Id), {'APPLY_CALL', Module, Function, Args}).

%% @doc alert !!!
-spec pure_call(pid() | non_neg_integer(), Function :: atom() | function(), Args :: [term()]) -> term().
pure_call(Id, Function, Args) ->
    gen_server:call(pid(Id), {'PURE_CALL', Function, Args}).

-spec pure_call(pid() | non_neg_integer(), Module :: atom(), Function :: atom() | function(), Args :: [term()]) -> term().
pure_call(Id, Module, Function, Args) ->
    gen_server:call(pid(Id), {'PURE_CALL', Module, Function, Args}).

%% @doc main async cast
-spec apply_cast(pid() | non_neg_integer(), Function :: atom() | function(), Args :: [term()]) -> term().
apply_cast(Id, Function, Args) ->
    gen_server:cast(pid(Id), {'APPLY_CAST', Function, Args}).

-spec apply_cast(pid() | non_neg_integer(), Module :: atom(), Function :: atom() | function(), Args :: [term()]) -> term().
apply_cast(Id, Module, Function, Args) ->
    gen_server:cast(pid(Id), {'APPLY_CAST', Module, Function, Args}).

%% @doc main async cast
-spec pure_cast(pid() | non_neg_integer(), Function :: atom() | function(), Args :: [term()]) -> term().
pure_cast(Id, Function, Args) ->
    gen_server:cast(pid(Id), {'PURE_CAST', Function, Args}).

-spec pure_cast(pid() | non_neg_integer(), Module :: atom(), Function :: atom() | function(), Args :: [term()]) -> term().
pure_cast(Id, Module, Function, Args) ->
    gen_server:cast(pid(Id), {'PURE_CAST', Module, Function, Args}).

%% @doc main async cast
-spec apply_delay_cast(pid() | non_neg_integer(), Function :: atom() | function(), Args :: [term()], Time :: non_neg_integer()) -> reference().
apply_delay_cast(Id, Function, Args, Time) ->
    erlang:send_after(Time, pid(Id), {'$gen_cast', {'APPLY_CAST', Function, Args}}).

-spec apply_delay_cast(pid() | non_neg_integer(), Module :: atom(), Function :: atom() | function(), Args :: [term()], Time :: non_neg_integer()) -> reference().
apply_delay_cast(Id, Module, Function, Args, Time) ->
    erlang:send_after(Time, pid(Id), {'$gen_cast', {'APPLY_CAST', Module, Function, Args}}).

%% @doc main async cast
-spec pure_delay_cast(pid() | non_neg_integer(), Function :: atom() | function(), Args :: [term()], Time :: non_neg_integer()) -> reference().
pure_delay_cast(Id, Function, Args, Time) ->
    erlang:send_after(Time, pid(Id), {'$gen_cast', {'PURE_CAST', Function, Args}}).

-spec pure_delay_cast(pid() | non_neg_integer(), Module :: atom(), Function :: atom() | function(), Args :: [term()], Time :: non_neg_integer()) -> reference().
pure_delay_cast(Id, Module, Function, Args, Time) ->
    erlang:send_after(Time, pid(Id), {'$gen_cast', {'PURE_CAST', Module, Function, Args}}).

%% @doc call
-spec call(pid() | non_neg_integer(), Request :: term()) -> term().
call(Id, Request) ->
    gen_server:call(pid(Id), Request).

%% @doc cast
-spec cast(pid() | non_neg_integer(), Request :: term()) -> ok.
cast(Id, Request) ->
    gen_server:cast(pid(Id), Request).

%% @doc info
-spec info(pid() | non_neg_integer(), Request :: term()) -> term().
info(Id, Request) ->
    erlang:send(pid(Id), Request).

%% @doc lookup record field
-spec field(pid() | non_neg_integer(), Field :: atom()) -> term().
field(Id, Field) ->
    apply_call(Id, fun(State) -> beam:field(State, Field) end, []).

%% @doc lookup record field
-spec field(pid() | non_neg_integer(), Field :: atom(), Key :: term()) -> term().
field(Id, Field, Key) ->
    field(Id, Field, Key, 2).

%% @doc lookup record field
-spec field(pid() | non_neg_integer(), Field :: atom(), Key :: term(), N :: pos_integer()) -> term().
field(Id, Field, Key, N) ->
    apply_call(Id, fun(State) -> lists:keyfind(Key, N, beam:field(State, Field)) end, []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
%% @doc init
-spec init(Args :: term()) -> {ok, State :: #map_state{}}.
init([MapId, MapNo]) ->
    erlang:process_flag(trap_exit, true),
    erlang:send_after(1000, self(), loop),
    %% crash it if the map data not found
    #map_data{monsters = Monsters, type = Type, rank_mode = RankMode} = map_data:get(MapId),
    State = #map_state{map_no = MapNo, map_id = MapId, type = Type, pid = self()},
    %% start rank
    Sorter = battle_rank:new(State, RankMode),
    %% create map monster
    Fighters = monster:create(Monsters),
    {ok, State#map_state{fighters = Fighters, sorter = Sorter}}.

%% @doc handle_call
-spec handle_call(Request :: term(), From :: {pid(), Tag :: term()}, State :: #map_state{}) -> {reply, Reply :: term(), NewState :: #map_state{}}.
handle_call(Request, From, State) ->
    try
        do_call(Request, From, State)
    catch ?EXCEPTION(_Class, Reason, Stacktrace) ->
        ?STACKTRACE(Reason, ?GET_STACKTRACE(Stacktrace)),
        {reply, ok, State}
    end.

%% @doc handle_cast
-spec handle_cast(Request :: term(), State :: #map_state{}) -> {noreply, NewState :: #map_state{}}.
handle_cast(Request, State) ->
    try
        do_cast(Request, State)
    catch ?EXCEPTION(_Class, Reason, Stacktrace) ->
        ?STACKTRACE(Reason, ?GET_STACKTRACE(Stacktrace)),
        {noreply, State}
    end.

%% @doc handle_info
-spec handle_info(Request :: term(), State :: #map_state{}) -> {noreply, NewState :: #map_state{}}.
handle_info(Info, State) ->
    try
        do_info(Info, State)
    catch ?EXCEPTION(_Class, Reason, Stacktrace) ->
        ?STACKTRACE(Reason, ?GET_STACKTRACE(Stacktrace)),
        {noreply, State}
    end.

%% @doc terminate
-spec terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()), State :: #map_state{}) -> {ok, NewState :: #map_state{}}.
terminate(_Reason, State) ->
    {ok, State}.

%% @doc code_change
-spec code_change(OldVsn :: (term() | {down, term()}), State :: #map_state{}, Extra :: term()) -> {ok, NewState :: #map_state{}}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

do_call({'APPLY_CALL', Function, Args}, _From, State) ->
    case erlang:apply(Function, [State | Args]) of
        {ok, Reply, NewState = #map_state{}} ->
            {reply, Reply, NewState};
        {ok, NewState = #map_state{}} ->
            {reply, ok, NewState};
        Reply ->
            {reply, Reply, State}
    end;
do_call({'PURE_CALL', Function, Args}, _From, State) ->
    case erlang:apply(Function, Args) of
        {ok, Reply, NewState = #map_state{}} ->
            {reply, Reply, NewState};
        {ok, NewState = #map_state{}} ->
            {reply, ok, NewState};
        Reply ->
            {reply, Reply, State}
    end;
do_call({'APPLY_CALL', Module, Function, Args}, _From, State) ->
    case erlang:apply(Module, Function, [State | Args]) of
        {ok, Reply, NewState = #map_state{}} ->
            {reply, Reply, NewState};
        {ok, NewState = #map_state{}} ->
            {reply, ok, NewState};
        Reply ->
            {reply, Reply, State}
    end;
do_call({'PURE_CALL', Module, Function, Args}, _From, State) ->
    case erlang:apply(Module, Function, Args) of
        {ok, Reply, NewState = #map_state{}} ->
            {reply, Reply, NewState};
        {ok, NewState = #map_state{}} ->
            {reply, ok, NewState};
        Reply ->
            {reply, Reply, State}
    end;
do_call(_Request, _From, State) ->
    {reply, ok, State}.

do_cast({'APPLY_CAST', Function, Args}, State) ->
    case erlang:apply(Function, [State | Args]) of
        {ok, NewState = #map_state{}} ->
            {noreply, NewState};
        _ ->
            {noreply, State}
    end;
do_cast({'PURE_CAST', Function, Args}, State) ->
    case erlang:apply(Function, Args) of
        {ok, NewState = #map_state{}} ->
            {noreply, NewState};
        _ ->
            {noreply, State}
    end;
do_cast({'APPLY_CAST', Module, Function, Args}, State) ->
    case erlang:apply(Module, Function, [State | Args]) of
        {ok, NewState = #map_state{}} ->
            {noreply, NewState};
        _ ->
            {noreply, State}
    end;
do_cast({'PURE_CAST', Module, Function, Args}, State) ->
    case erlang:apply(Module, Function, Args) of
        {ok, NewState = #map_state{}} ->
            {noreply, NewState};
        _ ->
            {noreply, State}
    end;
do_cast({query, SenderPid}, State = #map_state{fighters = Fighters}) ->
    user_sender:send(SenderPid, ?PROTOCOL_MAP_FIGHTER, Fighters),
    {noreply, State};
do_cast({enter, Fighter = #fighter{id = Id}}, State = #map_state{fighters = Fighters}) ->
    NewFighters = lists:keystore(Id, #fighter.id, Fighters, Fighter),
    %% notify update
    map:enter(State, Fighter),
    NewState = battle_event:handle(State, #battle_event{name = event_role_enter, object = Fighter}),
    {noreply, NewState#map_state{fighters = NewFighters}};
do_cast({leave, Id}, State = #map_state{fighters = Fighters}) ->
    case lists:keytake(Id, #fighter.id, Fighters) of
        {value, Fighter, NewFighters} ->
            %% notify update
            map:leave(State, Fighter),
            NewState = battle_event:handle(State, #battle_event{name = event_role_leave, object = Fighter}),
            {noreply, NewState#map_state{fighters = NewFighters}};
        _ ->
            {noreply, State}
    end;
do_cast({move, RoleId, NewX, NewY}, State = #map_state{fighters = Fighters}) ->
    case lists:keyfind(RoleId, #fighter.id, Fighters) of
        Fighter = #fighter{x = OldX, y = OldY} ->
            NewFighter = Fighter#fighter{x = NewX, y = NewY},
            NewFighters = lists:keystore(RoleId, #fighter.id, Fighters, NewFighter),
            %% notify update
            map:move(State, NewFighter, OldX, OldY, NewX, NewY),
            {noreply, State#map_state{fighters = NewFighters}};
        _ ->
            {noreply, State}
    end;
do_cast({path, Id, Path}, State = #map_state{fighters = Fighters}) ->
    case lists:keyfind(Id, #fighter.id, Fighters) of
        Monster = #fighter{} ->
            NewFighters = lists:keystore(Id, #fighter.id, Fighters, Monster#fighter{path = Path}),
            {noreply, State#map_state{fighters = NewFighters}};
        _ ->
            {noreply, State}
    end;

do_cast({attack, AttackerId, SkillId, TargetList}, State) ->
    case battle_role:attack(State, AttackerId, SkillId, TargetList) of
        {ok, NewState = #map_state{}} ->
            {noreply, NewState};
        _ ->
            {noreply, State}
    end;
do_cast(_Request, State) ->
    {noreply, State}.

do_info(loop, State = #map_state{tick = Tick}) ->
    erlang:send_after(125, self(), loop),
    NewState = monster_act:loop(State),
    {noreply, NewState#map_state{tick = Tick + 1}};
do_info(stop, State) ->
    {stop, normal, State};
do_info(_Info, State) ->
    {noreply, State}.

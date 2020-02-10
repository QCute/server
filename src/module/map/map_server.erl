%%%------------------------------------------------------------------
%%% @doc
%%% module map server
%%% @end
%%%------------------------------------------------------------------
-module(map_server).
-behaviour(gen_server).
%% API
-export([start_city/0, start/1, start/2, start_link/2, stop/1, stop/2]).
-export([city_id/0, city_unique_id/0, city_pid/0]).
-export([map_id/1, unique_id/2, unique_id/1, name/1, pid/1]).
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
-include("user.hrl").
-include("role.hrl").
-include("map.hrl").
%%%==================================================================
%%% API functions
%%%==================================================================
%% @doc start main city
-spec start_city() -> {ok, pid()} | {error, term()}.
start_city() ->
    process:start(?MODULE, [city_id(), city_unique_id()]).

%% @doc server start
-spec start(non_neg_integer()) -> #map{}.
start(MapId) ->
    UniqueId = unique_id(MapId, increment_server:next(map)),
    start(MapId, UniqueId).

%% @doc server start
-spec start(non_neg_integer(), non_neg_integer()) -> #map{}.
start(MapId, UniqueId) ->
    {ok, Pid} = start_link(MapId, UniqueId),
    #map{unique_id = UniqueId, map_id = MapId, pid = Pid}.

%% @doc server start
-spec start_link(non_neg_integer(), non_neg_integer()) -> {ok, pid()} | {error, term()}.
start_link(MapId, UniqueId) ->
    gen_server:start_link({local, name(UniqueId)}, ?MODULE, [MapId, UniqueId], []).

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

%% @doc main city map unique id
-spec city_unique_id() -> non_neg_integer().
city_unique_id() ->
    unique_id(city_id(), 0).

%% @doc main city map pid
-spec city_pid() -> pid().
city_pid() ->
    pid(name(city_unique_id())).

%% @doc map unique id
-spec map_id(non_neg_integer()) -> non_neg_integer().
map_id(UniqueId) ->
    (UniqueId div 10000000000).

%% @doc map unique id
-spec unique_id(non_neg_integer(), non_neg_integer()) -> non_neg_integer().
unique_id(MapId, Id) ->
    (MapId * 10000000000 + Id).

%% @doc map unique id
-spec unique_id(atom()) -> non_neg_integer().
unique_id(Name) ->
    type:to_integer(hd(tl(string:tokens(type:to_list(Name), "_")))).

%% @doc map unique name
-spec name(non_neg_integer() | pid()) -> atom().
name(UniqueId) when is_integer(UniqueId) ->
    type:to_atom(lists:concat(["map_", UniqueId]));
name(Pid) when is_pid(Pid) ->
    erlang:element(2, erlang:process_info(Pid, registered_name)).

%% @doc map pid
-spec pid(pid() | non_neg_integer() | atom()) -> Pid :: pid() | undefined.
pid(Pid) when is_pid(Pid) ->
    Pid;
pid(UniqueId) when is_integer(UniqueId) ->
    pid(name(UniqueId));
pid(Name) when is_atom(Name) ->
    process:pid(Name).

%% @doc query
-spec query(User :: #user{}) -> ok().
query(#user{sender_pid = SenderPid, role = #role{map = #map{pid = Pid}}}) ->
    cast(Pid, {scene, SenderPid}).

%% @doc enter map
-spec enter(#user{}) -> #user{}.
enter(User = #user{role = #role{map = []}}) ->
    enter(User, #map{unique_id = city_unique_id(), map_id = city_id(), pid = city_pid()});
enter(User = #user{role = #role{map = Map = #map{unique_id = UniqueId, map_id = MapId}}}) ->
    Pid = pid(UniqueId),
    #map_data{reconnect = Reconnect} = map_data:get(MapId),
    case erlang:is_pid(Pid) of
        true when Reconnect ->
            enter(User, Map#map{pid = Pid});
        _ ->
            enter(User, #map{unique_id = city_unique_id(), map_id = city_id(), pid = city_pid()})
    end.

%% @doc enter map
-spec enter(#user{}, non_neg_integer() | pid() | #map{}) -> #user{}.
enter(User, UniqueId) when is_integer(UniqueId) ->
    MapId = map_id(UniqueId),
    Pid = pid(UniqueId),
    Map = #map{unique_id = UniqueId, map_id = MapId, pid = Pid},
    enter(User, Map);
enter(User, Pid) when is_pid(Pid) ->
    UniqueId = unique_id(name(Pid)),
    MapId = map_id(UniqueId),
    Map = #map{unique_id = UniqueId, map_id = MapId, pid = Pid},
    enter(User, Map);
enter(User, Map = #map{map_id = MapId, x = 0, y = 0}) ->
    {X, Y} = listing:random((map_data:get(MapId))#map_data.enter_points),
    enter(User, Map#map{x = X, y = Y});
enter(User = #user{role = Role}, Map = #map{pid = Pid}) ->
    NewUser = leave(User),
    FinalUser = NewUser#user{role = Role#role{map = Map}},
    Fighter = #fighter{} = user_convert:to(FinalUser, map),
    cast(Pid, {enter, Fighter}),
    FinalUser.

%% @doc leave map
-spec leave(#user{}) -> #user{}.
leave(User = #user{role_id = RoleId, role = Role = #role{map = #map{pid = Pid}}}) ->
    cast(Pid, {leave, RoleId}),
    User#user{role = Role#role{map = #map{}}};
leave(User = #user{role = Role}) ->
    User#user{role = Role#role{map = #map{}}}.

%% @doc move
-spec move(User :: #user{}, X :: non_neg_integer(), Y :: non_neg_integer()) -> ok.
move(#user{role_id = RoleId, role = #role{map = #map{pid = Pid}}}, X, Y) ->
    cast(Pid, {move, RoleId, X, Y}).

%% @doc attack
-spec attack(User :: #user{}, SkillId :: non_neg_integer(), TargetList :: list()) -> ok.
attack(#user{role_id = RoleId, role = #role{map = #map{pid = Pid}}}, SkillId, TargetList) ->
    cast(Pid, {attack, RoleId, SkillId, TargetList}).

%% @doc alert !!! call it debug only
-spec apply_call(pid() | non_neg_integer(), Function :: atom() | function(), Args :: []) -> term().
apply_call(Id, Function, Args) ->
    gen_server:call(pid(Id), {'APPLY_CALL', Function, Args}).

-spec apply_call(pid() | non_neg_integer(), Module :: atom(), Function :: atom() | function(), Args :: []) -> term().
apply_call(Id, Module, Function, Args) ->
    gen_server:call(pid(Id), {'APPLY_CALL', Module, Function, Args}).

%% @doc alert !!! call it debug only
-spec pure_call(pid() | non_neg_integer(), Function :: atom() | function(), Args :: []) -> term().
pure_call(Id, Function, Args) ->
    gen_server:call(pid(Id), {'PURE_CALL', Function, Args}).

-spec pure_call(pid() | non_neg_integer(), Module :: atom(), Function :: atom() | function(), Args :: []) -> term().
pure_call(Id, Module, Function, Args) ->
    gen_server:call(pid(Id), {'PURE_CALL', Module, Function, Args}).

%% @doc main async cast
-spec apply_cast(pid() | non_neg_integer(), Function :: atom() | function(), Args :: []) -> term().
apply_cast(Id, Function, Args) ->
    gen_server:cast(pid(Id), {'APPLY_CAST', Function, Args}).

-spec apply_cast(pid() | non_neg_integer(), Module :: atom(), Function :: atom() | function(), Args :: []) -> term().
apply_cast(Id, Module, Function, Args) ->
    gen_server:cast(pid(Id), {'APPLY_CAST', Module, Function, Args}).

%% @doc main async cast
-spec pure_cast(pid() | non_neg_integer(), Function :: atom() | function(), Args :: []) -> term().
pure_cast(Id, Function, Args) ->
    gen_server:cast(pid(Id), {'PURE_CAST', Function, Args}).

-spec pure_cast(pid() | non_neg_integer(), Module :: atom(), Function :: atom() | function(), Args :: []) -> term().
pure_cast(Id, Module, Function, Args) ->
    gen_server:cast(pid(Id), {'PURE_CAST', Module, Function, Args}).

%% @doc main async cast
-spec apply_delay_cast(pid() | non_neg_integer(), Function :: atom() | function(), Args :: [], Time :: non_neg_integer()) -> term().
apply_delay_cast(Id, Function, Args, Time) ->
    catch erlang:send_after(Time, pid(Id), {'$gen_cast', {'APPLY_CAST', Function, Args}}),
    ok.

-spec apply_delay_cast(pid() | non_neg_integer(), Module :: atom(), Function :: atom() | function(), Args :: [], Time :: non_neg_integer()) -> term().
apply_delay_cast(Id, Module, Function, Args, Time) ->
    catch erlang:send_after(Time, pid(Id), {'$gen_cast', {'APPLY_CAST', Module, Function, Args}}),
    ok.

%% @doc main async cast
-spec pure_delay_cast(pid() | non_neg_integer(), Function :: atom() | function(), Args :: [], Time :: non_neg_integer()) -> term().
pure_delay_cast(Id, Function, Args, Time) ->
    catch erlang:send_after(Time, pid(Id), {'$gen_cast', {'PURE_CAST', Function, Args}}),
    ok.

-spec pure_delay_cast(pid() | non_neg_integer(), Module :: atom(), Function :: atom() | function(), Args :: [], Time :: non_neg_integer()) -> term().
pure_delay_cast(Id, Module, Function, Args, Time) ->
    catch erlang:send_after(Time, pid(Id), {'$gen_cast', {'PURE_CAST', Module, Function, Args}}),
    ok.

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
    apply_call(Id, fun(User) -> beam:field(User, Field) end, []).

%% @doc lookup record field
-spec field(pid() | non_neg_integer(), Field :: atom(), Key :: term()) -> term().
field(Id, Field, Key) ->
    field(Id, Field, Key, 2).

%% @doc lookup record field
-spec field(pid() | non_neg_integer(), Field :: atom(), Key :: term(), N :: pos_integer()) -> term().
field(Id, Field, Key, N) ->
    apply_call(Id, fun(State) -> lists:keyfind(Key, N, beam:field(State, Field)) end, []).

%%%==================================================================
%%% gen_server callbacks
%%%==================================================================
init([MapId, UniqueId]) ->
    erlang:process_flag(trap_exit, true),
    erlang:send_after(1000, self(), loop),
    %% crash it if map data not found
    #map_data{monsters = Monsters, type = Type, rank_mode = RankMode} = map_data:get(MapId),
    State = #map_state{unique_id = UniqueId, map_id = MapId, type = Type, pid = self()},
    %% start rank
    Sorter = battle_rank:new(State, RankMode),
    %% create map monster
    Fighters = monster:create(Monsters),
    {ok, State#map_state{fighters = Fighters, sorter = Sorter}}.

handle_call(Request, From, State) ->
    try
        do_call(Request, From, State)
    catch ?EXCEPTION(_Class, Reason, Stacktrace) ->
        ?STACKTRACE(Reason, ?GET_STACKTRACE(Stacktrace)),
        {reply, ok, State}
    end.

handle_cast(Request, State) ->
    try
        do_cast(Request, State)
    catch ?EXCEPTION(_Class, Reason, Stacktrace) ->
        ?STACKTRACE(Reason, ?GET_STACKTRACE(Stacktrace)),
        {noreply, State}
    end.

handle_info(Info, State) ->
    try
        do_info(Info, State)
    catch ?EXCEPTION(_Class, Reason, Stacktrace) ->
        ?STACKTRACE(Reason, ?GET_STACKTRACE(Stacktrace)),
        {noreply, State}
    end.

terminate(_Reason, _State) ->
    try
        ok
    catch ?EXCEPTION(_Class, Reason, Stacktrace) ->
        ?STACKTRACE(Reason, ?GET_STACKTRACE(Stacktrace)),
        ok
    end.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%==================================================================
%%% Internal functions
%%%==================================================================

do_call({'APPLY_CALL', Function, Args}, _From, User) ->
    %% alert !!! call it debug only
    case erlang:apply(Function, [User | Args]) of
        {ok, Reply, NewUser = #user{}} ->
            {reply, Reply, NewUser};
        {ok, NewUser = #user{}} ->
            {reply, ok, NewUser};
        Reply ->
            {reply, Reply, User}
    end;
do_call({'PURE_CALL', Function, Args}, _From, User) ->
    %% alert !!! call it debug only
    case erlang:apply(Function, Args) of
        {ok, Reply, NewUser = #user{}} ->
            {reply, Reply, NewUser};
        {ok, NewUser = #user{}} ->
            {reply, ok, NewUser};
        Reply ->
            {reply, Reply, User}
    end;
do_call({'APPLY_CALL', Module, Function, Args}, _From, User) ->
    %% alert !!! call it debug only
    case erlang:apply(Module, Function, [User | Args]) of
        {ok, Reply, NewUser = #user{}} ->
            {reply, Reply, NewUser};
        {ok, NewUser = #user{}} ->
            {reply, ok, NewUser};
        Reply ->
            {reply, Reply, User}
    end;
do_call({'PURE_CALL', Module, Function, Args}, _From, User) ->
    %% alert !!! call it debug only
    case erlang:apply(Module, Function, Args) of
        {ok, Reply, NewUser = #user{}} ->
            {reply, Reply, NewUser};
        {ok, NewUser = #user{}} ->
            {reply, ok, NewUser};
        Reply ->
            {reply, Reply, User}
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
do_cast({scene, SenderPid}, State = #map_state{fighters = Fighters}) ->
    user_sender:send(SenderPid, ?PROTOCOL_MAP_FIGHTER, Fighters),
    {noreply, State};
do_cast({enter, Fighter = #fighter{id = Id}}, State = #map_state{fighters = Fighters}) ->
    NewFighters = lists:keystore(Id, #fighter.id, Fighters, Fighter),
    %% notify update
    map:enter(State, Fighter),
    {noreply, State#map_state{fighters = NewFighters}};
do_cast({leave, Id}, State = #map_state{fighters = Fighters}) ->
    {value, Fighter, NewFighters} = lists:keytake(Id, #fighter.id, Fighters),
    %% notify update
    map:leave(State, Fighter),
    {noreply, State#map_state{fighters = NewFighters}};
do_cast({create_monster, MonsterId}, State = #map_state{fighters = Fighters}) ->
    [Monster] = monster:create([MonsterId]),
    %% notify update
    map:enter(State, Monster),
    {noreply, State#map_state{fighters = [Monster | Fighters]}};
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

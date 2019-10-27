%%%-------------------------------------------------------------------
%%% @doc
%%% module map server
%%% @end
%%%-------------------------------------------------------------------
-module(map_server).
-behaviour(gen_server).
%% API
-export([start/0, start/1, start/2]).
-export([city_id/0, city_unique_id/0, map_id/1, unique_id/2]).
-export([name/1, pid/1]).
-export([query/1, move/3, enter/1, update_fighter/1]).
-export([apply_call/3, apply_call/4, apply_cast/3, apply_cast/4]).
-export([pure_call/3, pure_call/4, pure_cast/3, pure_cast/4]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
%% includes
-include("common.hrl").
-include("user.hrl").
-include("map.hrl").
-include("protocol.hrl").
%%%===================================================================
%%% API
%%%===================================================================
-spec start() -> {ok, pid()} | {error, term()}.
start() ->
    City = city_id(),
    CityUniqueId = city_unique_id(),
    Name = name(CityUniqueId),
    gen_server:start_link({local, Name}, ?MODULE, [City, CityUniqueId], []).

%% @doc server start
-spec start(non_neg_integer()) -> {ok, pid()} | {error, term()}.
start(MapId) ->
    UniqueId = unique_id(MapId, increment:next(map)),
    Name = name(UniqueId),
    gen_server:start_link({local, Name}, ?MODULE, [MapId, UniqueId], []).

%% @doc server start
-spec start(non_neg_integer(), non_neg_integer()) -> {ok, pid()} | {error, term()}.
start(MapId, Id) ->
    UniqueId = unique_id(MapId, Id),
    Name = name(UniqueId),
    gen_server:start_link({local, Name}, ?MODULE, [MapId, UniqueId], []).

%% @doc main city map id
-spec city_id() -> non_neg_integer().
city_id() ->
    100000.

%% @doc main city map unique id
-spec city_unique_id() -> non_neg_integer().
city_unique_id() ->
    unique_id(city_id(), 0).

%% @doc map unique id
-spec map_id(non_neg_integer()) -> non_neg_integer().
map_id(UniqueId) ->
    (UniqueId div 10000000000).

%% @doc map unique id
-spec unique_id(non_neg_integer(), non_neg_integer()) -> non_neg_integer().
unique_id(MapId, UniqueId) ->
    (MapId * 10000000000 + UniqueId).

%% @doc map unique name
-spec name(non_neg_integer()) -> atom().
name(UniqueId) ->
    type:to_atom(lists:concat(["map_", UniqueId])).

%% @doc map pid
-spec pid(pid() | non_neg_integer() | atom()) -> Pid :: pid() | undefined.
pid(Pid) when is_pid(Pid) ->
    Pid;
pid(UniqueId) when is_integer(UniqueId) ->
    process:pid(name(UniqueId));
pid(Name) when is_atom(Name) ->
    process:pid(Name).

%% @doc query
-spec query(User :: #user{}) -> ok().
query(#user{map = #map{map_id = MapId, x = X, y = Y}}) ->
    {ok, [MapId, X, Y]}.

%% @doc move
-spec move(User :: #user{}, X :: non_neg_integer(), Y :: non_neg_integer()) -> ok.
move(#user{role_id = RoleId, map = #map{pid = Pid}}, X, Y) ->
    gen_server:cast(Pid, {move, RoleId, X, Y}),
    ok.

%% @doc enter map
-spec enter(#user{}) -> #map{}.
enter(#user{map = Map = #map{}}) ->
    Map;
enter(_) ->
    City = city_id(),
    {X, Y} = listing:random((map_data:get(City))#map_data.enter_point, {0, 0}),
    #map{x = X, y = Y, map_id = City, pid = pid(city_unique_id())}.

%% @doc update fighter
-spec update_fighter(#user{}) -> #map{}.
update_fighter(User) ->
    Map = #map{pid = Pid} = enter(User),
    NewUser = User#user{map = Map},
    MapObject = user_convert:to(NewUser, map),
    gen_server:cast(Pid, {update_fighter, MapObject}),
    NewUser.

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

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([MapId, UniqueId]) ->
    erlang:process_flag(trap_exit, true),
    erlang:send_after(1000, self(), loop),
    %% crash it if map data not found
    #map_data{monsters = Monsters, type = Type} = map_data:get(MapId),
    Length = length(Monsters),
    List =  monster:create(Monsters, lists:seq(1, Length), []),
    {ok, #map_state{id = UniqueId, unique = Length + 1, type = Type, monsters = List}}.

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

%%%===================================================================
%%% Internal functions
%%%===================================================================

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
do_cast({update_fighter, Fighter = #fighter{id = Id}}, State = #map_state{roles = Fighters}) ->
    NewList = lists:keystore(Id, #fighter.id, Fighters, Fighter),
    %% broadcast update
    {noreply, State#map_state{roles = NewList}};
do_cast({create_monster, MonsterId}, State = #map_state{monsters = Monsters, unique = Increase}) ->
    [Monster = #fighter{id = Id}] =  monster:create([MonsterId], [Increase + 1], []),
    NewList = lists:keystore(Id, #fighter.id, Monsters, Monster),
    %% broadcast update
    {ok, Data} = user_router:write(?PROTOCOL_MAP_MONSTER, [Monster#fighter.x, Monster#fighter.y]),
    map:broadcast(State, Data),
    {noreply, State#map_state{monsters = NewList, unique = Increase + 1}};
do_cast({move, Id, X, Y}, State = #map_state{roles = Fighters}) ->
    case lists:keyfind(Id, #fighter.id, Fighters) of
        Fighter = #fighter{} ->
            New = Fighter#fighter{x = X, y = Y},
            NewList = lists:keystore(Id, #fighter.id, Fighters, New),
            {noreply, State#map_state{roles = NewList}};
        _ ->
            {noreply, State}
    end;
do_cast({path, Id, Path}, State = #map_state{monsters = Monsters}) ->
    case lists:keyfind(Id, #fighter.id, Monsters) of
        Monster = #fighter{} ->
            New = Monster#fighter{path = Path},
            NewList = lists:keystore(Id, #fighter.id, Monsters, New),
            {noreply, State#map_state{monsters = NewList}};
        _ ->
            {noreply, State}
    end;
do_cast({scene, Id, PidSender}, State) ->
    case lists:keyfind(Id, #fighter.id, State#map_state.roles) of
        #fighter{} ->
            SliceFighters = monster_agent:get_slice_roles(State, 10, 0),
            SliceMonsters = monster_agent:get_slice_monsters(State, 10, 0),
            {ok, FightersData} = user_router:write(?PROTOCOL_MAP_FIGHTER, [SliceFighters]),
            {ok, MonstersData} = user_router:write(?PROTOCOL_MAP_MONSTER, [SliceMonsters]),
            user_sender:send(PidSender, <<FightersData/binary, MonstersData/binary>>),
            {noreply, State};
        _ ->
            {noreply, State}
    end;
do_cast({attack, AttackerId, SkillId, DefenderIdList}, State) ->
    case battle_role:attack(State, AttackerId, SkillId, DefenderIdList) of
        {ok, NewState = #map_state{}} ->
            {noreply, NewState};
        _ ->
            skip
    end;
do_cast(_Request, State) ->
    {noreply, State}.


do_info(loop, State = #map_state{tick = Tick}) ->
    erlang:send_after(125, self(), loop),
    NewState = monster_act:loop(State),
    {noreply, NewState#map_state{tick = Tick + 1}};
do_info(_Info, State) ->
    {noreply, State}.




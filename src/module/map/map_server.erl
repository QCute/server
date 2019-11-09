%%%------------------------------------------------------------------
%%% @doc
%%% module map server
%%% @end
%%%------------------------------------------------------------------
-module(map_server).
-behaviour(gen_server).
%% API
-export([start/0, start/1, start/2]).
-export([city_id/0, city_unique_id/0, city_pid/0]).
-export([map_id/1, unique_id/2, unique_id/1, name/1, pid/1]).
-export([query/1]).
-export([enter/1, enter/2, enter/4, leave/1, move/3]).
-export([apply_call/3, apply_call/4, apply_cast/3, apply_cast/4]).
-export([pure_call/3, pure_call/4, pure_cast/3, pure_cast/4]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
%% includes
-include("common.hrl").
-include("user.hrl").
-include("role.hrl").
-include("map.hrl").
-include("protocol.hrl").
%%%==================================================================
%%% API functions
%%%==================================================================
%% @doc start main city
-spec start() -> {ok, pid()} | {error, term()}.
start() ->
    start(city_id(), 0).

%% @doc server start
-spec start(non_neg_integer()) -> {ok, non_neg_integer(), pid()}.
start(MapId) ->
    UniqueId = unique_id(MapId, increment:next(map)),
    Name = name(UniqueId),
    {ok, Pid} = gen_server:start_link({local, Name}, ?MODULE, [MapId, UniqueId], []),
    {ok, UniqueId, Pid}.

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

%% @doc main city map pid
-spec city_pid() -> pid().
city_pid() ->
    process:pid(name(city_unique_id())).

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
    gen_server:cast(Pid, {scene, SenderPid}).

%% @doc enter map
-spec enter(#user{}) -> #user{}.
enter(User = #user{role = #role{map = undefined}}) ->
    enter(User, city_unique_id(), city_id(), city_pid());
enter(User = #user{role = #role{map = []}}) ->
    enter(User, city_unique_id(), city_id(), city_pid());
enter(User = #user{role = #role{map = #map{unique_id = 0, map_id = 0}}}) ->
    enter(User, city_unique_id(), city_id(), city_pid());
enter(User = #user{role = #role{map = #map{unique_id = UniqueId, map_id = MapId}}}) ->
    enter(User, UniqueId, MapId, pid(UniqueId)).

%% @doc enter map
-spec enter(#user{}, non_neg_integer()) -> #user{}.
enter(User, UniqueId) when is_integer(UniqueId) ->
    enter(User, UniqueId, map_id(UniqueId), pid(UniqueId));
enter(User, Pid) when is_pid(Pid) ->
    UniqueId = unique_id(name(Pid)),
    enter(User, UniqueId, map_id(UniqueId), Pid).

%% @doc enter map
-spec enter(#user{}, non_neg_integer(), non_neg_integer(), pid()) -> #user{}.
enter(User, UniqueId, MapId, Pid) ->
    NewUser = leave(User),
    Fighter = user_convert:to(NewUser, map),
    {X, Y} = listing:random((map_data:get(MapId))#map_data.enter_points, {0, 0}),
    gen_server:cast(Pid, {enter, Fighter#fighter{x = X, y = Y}}),
    NewUser#user{role = #role{map = #map{unique_id = UniqueId, map_id = MapId, pid = Pid, x = X, y = Y}}}.

%% @doc leave map
-spec leave(#user{}) -> #user{}.
leave(User = #user{role_id = RoleId, role = #role{map = #map{pid = Pid}}}) ->
    gen_server:cast(Pid, {leave, RoleId}),
    User#user{role = #role{map = #map{}}};
leave(User = #user{role = Role}) ->
    User#user{role = Role#role{map = #map{}}}.

%% @doc move
-spec move(User :: #user{}, X :: non_neg_integer(), Y :: non_neg_integer()) -> ok.
move(#user{role_id = RoleId, role = #role{map = #map{pid = Pid}}}, X, Y) ->
    gen_server:cast(Pid, {move, RoleId, X, Y}).

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

%%%==================================================================
%%% gen_server callbacks
%%%==================================================================
init([MapId, UniqueId]) ->
    erlang:process_flag(trap_exit, true),
    erlang:send_after(1000, self(), loop),
    %% crash it if map data not found
    #map_data{monsters = Monsters, type = Type} = map_data:get(MapId),
    {ok, #map_state{id = UniqueId, type = Type, fighters = monster:create(Monsters)}}.

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
do_cast({enter, Fighter = #fighter{id = Id}}, State = #map_state{fighters = Fighters}) ->
    NewFighters = lists:keystore(Id, #fighter.id, Fighters, Fighter),
    %% broadcast update
    {noreply, State#map_state{fighters = NewFighters}};
do_cast({create_monster, MonsterId}, State = #map_state{fighters = Fighters}) ->
    [Monster] =  monster:create([MonsterId]),
    %% broadcast update
    {ok, Data} = user_router:write(?PROTOCOL_MAP_MONSTER, [Monster#fighter.x, Monster#fighter.y]),
    map:broadcast(State, Data),
    {noreply, State#map_state{fighters = [Monster | Fighters]}};
do_cast({move, RoleId, X, Y}, State = #map_state{fighters = Fighters}) ->
    case lists:keyfind(RoleId, #fighter.id, Fighters) of
        Fighter = #fighter{} ->
            NewFighters = lists:keystore(RoleId, #fighter.id, Fighters, Fighter#fighter{x = X, y = Y}),
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
do_cast({scene, SenderPid}, State = #map_state{fighters = Fighters}) ->
    user_sender:send(SenderPid, ?PROTOCOL_MAP, [Fighters]),
    {noreply, State};
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




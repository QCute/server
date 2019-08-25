%%%-------------------------------------------------------------------
%%% @doc
%%% module map server
%%% @end
%%%-------------------------------------------------------------------
-module(map_server).
-behaviour(gen_server).
%% API
-export([start/0, start/2]).
-export([name/1, map/1, move/3, unique_id/2]).
-export([update_fighter/1]).
-export([create_monster/2]).
-export([apply_call/2, apply_call/3, apply_call/4, apply_cast/2, apply_cast/3, apply_cast/4]).
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
start() ->
    Name = name(100000),
    %% map id same as unique id
    gen_server:start(?MODULE, [100000, 100000, Name], []).

%% @doc server start
start(MapId, Id) ->
    UniqueId = unique_id(MapId, Id),
    Name = name(UniqueId),
    gen_server:start(?MODULE, [MapId, UniqueId, Name], []).

%% @doc map unique name
name(UniqueId) ->
    type:to_atom(lists:concat(["map_", UniqueId])).

%% @doc map unique id
unique_id(MapId, Id) ->
    MapId * 100000 + Id.

%% @doc update fighter
update_fighter(User) ->
    Map = #map{pid = Pid, x = X, y = Y} = map(User),
    {ok, Data} = user_router:write(?PROTOCOL_MAP_FIGHTER, [X, Y]),
    user_sender:send(User, Data),
    NewUser = User#user{map = Map},
    MapObject = user_convert:to(NewUser, map),
    gen_server:cast(Pid, {update_fighter, MapObject}),
    NewUser.

%% @doc move
move(#user{role_id = RoleId, map = #map{pid = Pid}}, X, Y) ->
    gen_server:cast(Pid, {move, RoleId, X, Y}),
    ok.

%% @doc map
map(#user{map = Map = #map{}}) ->
    Map;
map(_) ->
    {X, Y} = enter_point(100000),
    #map{x = X, y = Y, map_id = 100000, pid = process:pid(name(100000))}.

enter_point(MapId) ->
    case map_data:get(MapId) of
        #map_data{enter_point = Points} ->
            listing:random(Points, {0,0});
        _ ->
            {0,0}
    end.

%% @doc create monster
create_monster(UniqueId, MonsterId) ->
    gen_server:cast(name(UniqueId), {create_monster, MonsterId}).

%% @doc alert !!! call it debug only
apply_call(Pid, Function) ->
    gen_server:cast(Pid, {'APPLY_CALL', Function}).
apply_call(Pid, Function, Args) ->
    gen_server:cast(Pid, {'APPLY_CALL', Function, Args}).
apply_call(Pid, Module, Function, Args) ->
    gen_server:cast(Pid, {'APPLY_CALL', Module, Function, Args}).

%% @doc main async cast
apply_cast(Pid, Function) ->
    gen_server:cast(Pid, {'APPLY_CAST', Function}).
apply_cast(Pid, Function, Args) ->
    gen_server:cast(Pid, {'APPLY_CAST', Function, Args}).
apply_cast(Pid, Module, Function, Args) ->
    gen_server:cast(Pid, {'APPLY_CAST', Module, Function, Args}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([MapId, UniqueId, Name]) ->
    erlang:register(Name, self()),
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

%%-------------------------------------------------------------------
%% main sync role process call back
%%-------------------------------------------------------------------
do_call(_Request, _From, State) ->
    {reply, ok, State}.

%%-------------------------------------------------------------------
%% main async role process call back
%%-------------------------------------------------------------------
do_cast({update_fighter, Fighter = #fighter{id = Id}}, State = #map_state{fighters = Fighters}) ->
    NewList = lists:keystore(Id, #fighter.id, Fighters, Fighter),
    %% broadcast update
    {noreply, State#map_state{fighters = NewList}};
do_cast({create_monster, MonsterId}, State = #map_state{monsters = Monsters, unique = Increase}) ->
    [Monster = #monster{id = Id}] =  monster:create([MonsterId], [Increase + 1], []),
    NewList = lists:keystore(Id, #monster.id, Monsters, Monster),
    %% broadcast update
    {ok, Data} = user_router:write(?PROTOCOL_MAP_MONSTER, [Monster#monster.x, Monster#monster.y]),
    map:broadcast(State, Data),
    {noreply, State#map_state{monsters = NewList, unique = Increase + 1}};
do_cast({move, Id, X, Y}, State = #map_state{fighters = Fighters}) ->
    case lists:keyfind(Id, #monster.id, Fighters) of
        Fighter = #fighter{} ->
            New = Fighter#fighter{x = X, y = Y},
            NewList = lists:keystore(Id, #fighter.id, Fighters, New),
            {noreply, State#map_state{fighters = NewList}};
        _ ->
            {noreply, State}
    end;
do_cast({path, Id, Path}, State = #map_state{monsters = Monsters}) ->
    case lists:keyfind(Id, #monster.id, Monsters) of
        Monster = #monster{} ->
            New = Monster#monster{path = Path},
            NewList = lists:keystore(Id, #monster.id, Monsters, New),
            {noreply, State#map_state{monsters = NewList}};
        _ ->
            {noreply, State}
    end;
do_cast({scene, Id, PidSender}, State) ->
    case lists:keyfind(Id, #monster.id, State#map_state.fighters) of
        #fighter{} ->
            SliceFighters = monster_agent:get_slice_fighters(State, 10, 0),
            SliceMonsters = monster_agent:get_slice_monsters(State, 10, 0),
            {ok, FightersData} = user_router:write(?PROTOCOL_MAP_FIGHTER, [SliceFighters]),
            {ok, MonstersData} = user_router:write(?PROTOCOL_MAP_MONSTER, [SliceMonsters]),
            user_sender:send(PidSender, <<FightersData/binary, MonstersData/binary>>),
            {noreply, State};
        _ ->
            {noreply, State}
    end;
do_cast(_Request, State) ->
    {noreply, State}.


%%-------------------------------------------------------------------
%% self message call back
%%-------------------------------------------------------------------
do_info(loop, State) ->
    erlang:send_after(125, self(), loop),
    NewState = monster_act:loop(State),
    {noreply, NewState};
do_info(_Info, State) ->
    {noreply, State}.


%%%===================================================================
%%% Internal functions
%%%===================================================================

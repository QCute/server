%%%-------------------------------------------------------------------
%%% @doc
%%% boss server
%%% @end
%%%-------------------------------------------------------------------
-module(boss_server).
-behaviour(gen_server).
%% API
-export([start/0, start_link/0]).
-export([query/0]).
-export([battle/2]).
-export([update_hp/2]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
%% Includes
-include("common.hrl").
-include("event.hrl").
-include("user.hrl").
-include("map.hrl").
-include("monster.hrl").
-include("boss.hrl").
%%%===================================================================
%%% API functions
%%%===================================================================
%% @doc start
-spec start() -> {ok, pid()} | {error, term()}.
start() ->
    process:start(?MODULE).

%% @doc server start
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc query
-spec query() -> ok().
query() ->
    {ok, ?MODULE}.

%% @doc battle
-spec battle(User :: #user{}, MonsterId :: non_neg_integer()) -> ok() | error().
battle(User, MonsterId) ->
    case ets:lookup(?MODULE, MonsterId) of
        [#boss{map_no = MapNo, map_id = MapId, map_pid = MapPid}] ->
            enter(User, MonsterId, MapNo, MapId, MapPid);
        _ ->
            {error, no_such_boss}
    end.

enter(User, MonsterId, MapNo, MapId, MapPid) ->
    case process:alive(MapPid) of
        true ->
            NewUser = user_event:handle(User, #event{name = battle_boss, target = MonsterId}),
            {ok, ok, map_server:enter(NewUser, #map{map_no = MapNo, map_id = MapId, pid = MapPid})};
        false ->
            {error, boss_dead}
    end.

%% @doc update hp
-spec update_hp(MonsterId :: non_neg_integer(), Hp :: non_neg_integer()) -> ok.
update_hp(MonsterId, Hp) ->
    gen_server:cast(?MODULE, {hp, MonsterId, Hp}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
%% @doc init
-spec init(Args :: term()) -> {ok, State :: []}.
init([]) ->
    process_flag(trap_exit, true),
    ets:new(?MODULE, [named_table, set, {keypos, #boss.monster_id}, {read_concurrency, true}]),
    [relive(MonsterId) || MonsterId <- monster_data:type(1)],
    {ok, []}.

%% @doc handle_call
-spec handle_call(Request :: term(), From :: {pid(), Tag :: term()}, State :: []) -> {reply, Reply :: term(), NewState :: []}.
handle_call(Request, From, State) ->
    try
        do_call(Request, From, State)
    catch ?EXCEPTION(_Class, Reason, Stacktrace) ->
        ?STACKTRACE(Reason, ?GET_STACKTRACE(Stacktrace)),
        {reply, ok, State}
    end.

%% @doc handle_cast
-spec handle_cast(Request :: term(), State :: []) -> {noreply, NewState :: []}.
handle_cast(Request, State) ->
    try
        do_cast(Request, State)
    catch ?EXCEPTION(_Class, Reason, Stacktrace) ->
        ?STACKTRACE(Reason, ?GET_STACKTRACE(Stacktrace)),
        {noreply, State}
    end.

%% @doc handle_info
-spec handle_info(Request :: term(), State :: []) -> {noreply, NewState :: []}.
handle_info(Info, State) ->
    try
        do_info(Info, State)
    catch ?EXCEPTION(_Class, Reason, Stacktrace) ->
        ?STACKTRACE(Reason, ?GET_STACKTRACE(Stacktrace)),
        {noreply, State}
    end.

%% @doc terminate
-spec terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()), State :: []) -> {ok, NewState :: []}.
terminate(_Reason, State) ->
    {ok, State}.

%% @doc code_change
-spec code_change(OldVsn :: (term() | {down, term()}), State :: [], Extra :: term()) -> {ok, NewState :: []}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
do_call(_Request, _From, State) ->
    {reply, ok, State}.

do_cast({hp, MonsterId, Hp}, State) ->
    case ets:lookup(?MODULE, MonsterId) of
        [Boss = #boss{}] when Hp =< 0 ->
            ReliveTime = (monster_data:get(MonsterId))#monster_data.relive_time,
            WaitTime = ReliveTime + time:now(),
            Timer = erlang:send_after(?MILLISECONDS(WaitTime), self(), {relive, MonsterId}),
            NewBoss = Boss#boss{hp = 0, map_no = 0, map_pid = undefined, relive_time = ReliveTime, timer = Timer},
            ets:insert(?MODULE, NewBoss);
        [Boss = #boss{}] ->
            NewBoss = Boss#boss{hp = Hp},
            ets:insert(?MODULE, NewBoss);
        _ ->
            skip
    end,
    {noreply, State};
do_cast(_Request, State) ->
    {noreply, State}.

do_info({relive, MonsterId}, State) ->
    relive(MonsterId),
    {noreply, State};
do_info(_Info, State) ->
    {noreply, State}.

%% monster relive
relive(MonsterId) ->
    #monster_data{map_id = MapId, hp = Hp} = monster_data:get(MonsterId),
    #map{map_no = MapNo, pid = MapPid} = map_server:start(MapId),
    map_server:apply_cast(MapPid, boss_map, start, []),
    Boss = #boss{monster_id = MonsterId, hp = Hp, map_no = MapNo, map_id = MapId, map_pid = MapPid, relive_time = 0},
    ets:insert(?MODULE, Boss),
    ok.

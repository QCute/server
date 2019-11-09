%%%------------------------------------------------------------------
%%% @doc
%%% module boss server
%%% @end
%%%------------------------------------------------------------------
-module(boss_server).
-behaviour(gen_server).
%% API
-export([start/0, start_link/0]).
-export([query/0]).
-export([enter/2]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
%% Includes
-include("common.hrl").
-include("user.hrl").
-include("monster.hrl").
-include("boss.hrl").
%% Macros
-define(BOSS, boss).
%%%==================================================================
%%% API functions
%%%==================================================================
%% @doc start
-spec start() -> {ok, Pid :: pid()} | {error, term()}.
start() ->
    process:start(?MODULE).

%% @doc server start
-spec start_link() -> {ok, Pid :: pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc query
-spec query() -> ok().
query() ->
    {ok, [?BOSS]}.

%% @doc enter
-spec enter(User :: #user{}, MonsterId :: non_neg_integer()) -> ok() | error().
enter(User, MonsterId) ->
    case ets:lookup(?BOSS, MonsterId) of
        [#boss{map_unique_id = MapUniqueId, map_id = MapId, map_pid = MapPid}] ->
            {ok, [1], map_server:enter(User, MapUniqueId, MapId, MapPid)};
        _ ->
            {error, [2]}
    end.
%%%==================================================================
%%% gen_server callbacks
%%%==================================================================
init(_) ->
    ets:new(?BOSS, [named_table, set, {keypos, #boss.monster_id}, {read_concurrency, true}]),
    [relive(MonsterId) || MonsterId <- monster_data:type(boss)],
    {ok, []}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({hp, MonsterId, Hp}, State) ->
    case ets:lookup(?BOSS, MonsterId) of
        [Boss = #boss{}] ->
            case Hp =< 0 of
                true ->
                    WaitTime = (monster_data:get(MonsterId))#monster_data.relive_time + time:ts(),
                    Timer = erlang:send_after(WaitTime, self(), {relive, MonsterId}),
                    NewBoss = Boss#boss{hp = 0, timer = Timer};
                _ ->
                    NewBoss = Boss#boss{hp = Hp}
            end,
            ets:insert(?BOSS, NewBoss);
        _ ->
            skip
    end,
    {noreply, State};
handle_cast(_Request, State) ->
    {noreply, State}.

handle_info({relive, MonsterId}, State) ->
    relive(MonsterId),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%==================================================================
%%% Internal functions
%%%==================================================================
%% monster relive
relive(MonsterId) ->
    #monster_data{map_id = MapId, hp = Hp} = monster_data:get(MonsterId),
    increment:next(map),
    {ok, MapUniqueId, Pid} = map_server:start(MapId),
    Boss = #boss{monster_id = MonsterId, hp = Hp, map_unique_id = MapUniqueId, map_id = MapId, map_pid = Pid, relive_time = 0},
    ets:insert(?BOSS, Boss),
    ok.

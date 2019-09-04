%%%-------------------------------------------------------------------
%%% @doc
%%% module rank server
%%% @end
%%%-------------------------------------------------------------------
-module(rank_server).
-behaviour(gen_server).
%% API
-export([update/2, name/1, rank/1]).
-export([query/1]).
-export([start_all/1, start/2, start_link/2]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
%% Includes
-include("common.hrl").
-include("user.hrl").
-include("rank.hrl").
-record(state, {sorter, name, cache = [], node, tick = 1}).
%%%===================================================================
%%% API
%%%===================================================================
%% @doc update rank
-spec update(Type :: non_neg_integer(), Data :: #rank{} | [#rank{}]) -> ok.
update(Type, Data) ->
    process:cast(name(Type), {'update', Data}).

%% @doc rank server name
-spec name(Type :: non_neg_integer()) -> atom().
name(Type) ->
    type:to_atom(lists:concat([?MODULE, "_", Type])).

%% @doc rank data
-spec rank(Type :: non_neg_integer()) -> [#rank{}].
rank(Type) ->
    Name = name(Type),
    sorter:data(Name).

%% @doc query
-spec query(Type :: non_neg_integer()) -> ok().
query(Type) ->
    {ok, [rank_server:rank(Type)]}.

%% @doc start all
-spec start_all(Node :: atom()) -> ok.
start_all(Node) ->
    %% start all rank server, one type per server
    [{ok, _} = start(Type, [Node, Type]) || Type <- [1, 2, 3]],
    ok.

%% @doc start one
-spec start(Name :: atom(), Args :: [term()]) -> {ok, Pid :: pid()} | {error, term()}.
start(Name, Args) ->
    FullName = name(Name),
    process:start(FullName, ?MODULE, [FullName, Args]).

%% @doc server start
-spec start_link(Name :: atom(), Args :: [term()]) -> {ok, Pid :: pid()} | {error, term()}.
start_link(Name, Args) ->
    gen_server:start_link({local, Name}, ?MODULE, Args, []).
%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([local, Type]) ->
    %% construct name with type
    Name = name(Type),
    %% load from database
    Data = rank_sql:select(Type),
    %% transform rank record
    RankList = parser:convert(Data, rank, fun(I = #rank{other = Other}) -> I#rank{other = parser:to_term(Other)} end),
    %% make sorter with origin data, data select from database will sort with key(rank field)
    Sorter = sorter:new(Name, share, replace, 100, #rank.key, #rank.value, #rank.time, #rank.rank, RankList),
    %% first loop after 1 minutes
    erlang:send_after(30 * 1000, self(), 'first_sync'),
    erlang:send_after(?MINUTE_SECONDS * 1000, self(), loop),
    {ok, #state{sorter = Sorter, name = Name, node = local}};
init([center, Type]) ->
    %% construct name with type
    Name = name(Type),
    %% center node only show rank data, not save data
    Sorter = sorter:new(Name, share, replace, 100, #rank.key, #rank.value, #rank.time, #rank.rank, []),
    {ok, #state{sorter = Sorter, name = Name, node = center}};
init([world, Type]) ->
    %% construct name with type
    Name = name(Type),
    %% world node only show rank data, not save data
    Sorter = sorter:new(Name, share, replace, 100, #rank.key, #rank.value, #rank.time, #rank.rank, []),
    {ok, #state{sorter = Sorter, name = Name, node = world}};
init(_) ->
    {ok, #state{}}.

handle_call(_Info, _From, State) ->
    {reply, ok, State}.

handle_cast({'update', Data = [_ | _]}, State = #state{cache = Cache, node = local}) ->
    %% update online role info cache
    New = lists:ukeymerge(#rank.key, Data, Cache),
    {noreply, State#state{cache = New}};
handle_cast({'update', Data = #rank{key = Key}}, State = #state{cache = Cache, node = local}) ->
    %% update online role info cache
    New = lists:keystore(Key, #rank.key, Cache, Data),
    {noreply, State#state{cache = New}};
handle_cast({'update', Data}, State = #state{sorter = Sorter, name = Name, node = center}) ->
    %% update first
    sorter:update(Data, Sorter),
    %% get rank list data
    RankList = sorter:data(Sorter),
    %% sync to world
    process:cast(world, Name, {'update', RankList}),
    {noreply, State};
handle_cast({'update', Data}, State = #state{sorter = Sorter, node = world}) ->
    %% update directly
    sorter:update(Data, Sorter),
    {noreply, State};
handle_cast(_Info, State) ->
    {noreply, State}.

handle_info('stop', State) ->
    {stop, normal, State};
handle_info('first_sync', State = #state{sorter = Sorter, name = Name}) ->
    Data = sorter:data(Sorter),
    %% first sync
    case node_server:is_connected(center) of
        true ->
            process:cast(center, Name, {'update', Data});
        _ ->
            erlang:send_after(30 * 1000, self(), 'first_sync')
    end,
    {noreply, State};
handle_info(loop, State = #state{cache = []}) ->
    {noreply, State};
handle_info(loop, State = #state{sorter = Sorter, name = Name, cache = Cache, node = local, tick = Tick}) ->
    erlang:send_after(?MINUTE_SECONDS * 1000, self(), loop),
    %% update cache
    sorter:update(Cache, Sorter),
    %% get rank list data
    Data = sorter:data(Sorter),
    %% sync to database, 3 minutes
    case Tick rem 3 of
        0 ->
            rank_sql:update_into(Data);
        _ ->
            skip
    end,
    %% sync to center
    process:cast(center, Name, {'update', Data}),
    {noreply, State#state{cache = [], tick = Tick + 1}};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State = #state{sorter = Sorter, node = local}) ->
    %% update data when server stop
    Data = sorter:data(Sorter),
    rank_sql:update_into(Data),
    {ok, State};
terminate(_Reason, State) ->
    {ok, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
%% ====================================================================
%% Internal functions
%% ====================================================================

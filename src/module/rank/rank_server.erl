%%%-------------------------------------------------------------------
%%% @doc
%%% module rank server
%%% @end
%%%-------------------------------------------------------------------
-module(rank_server).
-behaviour(gen_server).
%% export API function
-export([update/2]).
-export([start/0, start/2]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-include("common.hrl").
-include("rank.hrl").
-record(state, {sorter, node, tick}).
%%%===================================================================
%%% API
%%%===================================================================
%% @doc update rank
update(Type, Data) ->
    process:cast(name(Type), {'update', Data}).

%% @doc rank server name
name(Type) ->
    type:to_atom(lists:concat([?MODULE, "_", Type])).

%% @doc start
start() ->
    process:start(?MODULE).
start(Name, Args) ->
    FullName = type:to_atom(lists:concat([?MODULE, "_", Name])),
    ChildSpec = {FullName, {?MODULE, start_link, [FullName, Args]}, permanent, 10000, worker, [FullName]},
    process:start(ChildSpec).
%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([local, Type]) ->
    %% make new sorter
    Name = name(Type),
    Data = rank_sql:select(Type),
    RankList = data_tool:load(Data, rank, fun(I = #rank{other = Other}) -> I#rank{other = data_tool:string_to_term(Other)} end),
    SortList = lists:sort(fun compare/2, RankList),
    Sorter = sorter:new(Name, share, replace, 100, #rank.key, #rank.value, #rank.time, #rank.rank, SortList),
    {ok, #state{sorter = Sorter, node = local}};
init([center, Type]) ->
    Name = name(Type),
    Sorter = sorter:new(Name, share, replace, 100, #rank.key, #rank.value, #rank.time, #rank.rank, []),
    {ok, #state{sorter = Sorter, node = center}};
init(_) ->
    {ok, #state{}}.

handle_call(_Info, _From, State)->
    {reply, ok, State}.

handle_cast({'update', Data}, State = #state{sorter = Sorter}) ->
    %% update online player info cache
    ?STACK_TRACE(sorter:update(Data, Sorter)),
    {noreply, State};
handle_cast(_Info, State) ->
    {noreply, State}.

handle_info('stop', State) ->
    {stop, normel, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    {ok, State}.

code_change(_OldVsn, State, _Extra)->
    {ok, State}.
%% ====================================================================
%% Internal functions
%% ====================================================================
%% data compare
compare(#rank{value = X}, #rank{value = Y}) when X > Y ->
    true;
compare(#rank{value = X, time = TimeX}, #rank{value = Y, time = TimeY}) when X == Y andalso TimeX < TimeY ->
    true;
compare(#rank{value = X, time = TimeX, key = KeyX}, #rank{value = Y, time = TimeY, key = KeyY}) when X == Y andalso TimeX == TimeY andalso KeyX < KeyY ->
    true;
compare(_, _) ->
    false.
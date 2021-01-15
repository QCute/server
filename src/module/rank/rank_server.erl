%%%-------------------------------------------------------------------
%%% @doc
%%% rank server
%%% @end
%%%-------------------------------------------------------------------
-module(rank_server).
-behaviour(gen_server).
%% API
-export([update/2, name/1, rank/1]).
-export([query/1, query_center/2, query_world/2]).
-export([new/1, new/2, drop/1]).
-export([start/1, start/2, start_link/2]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
%% Includes
-include("common.hrl").
-include("protocol.hrl").
-include("user.hrl").
-include("rank.hrl").
%% Macros
-define(PROTOCOL_RANK,                                19000).  %% 本服排行榜
-define(PROTOCOL_RANK_CENTER,                         19100).  %% 中心服排行榜
-define(PROTOCOL_RANK_WORLD,                          19200).  %% 世界服排行榜
%% Records
-record(state, {sorter, type, name, cache = [], node, tick = 1}).
%%%===================================================================
%%% API functions
%%%===================================================================
%% @doc update rank
-spec update(Type :: non_neg_integer(), Data :: #rank{} | [#rank{}]) -> ok.
update(Type, Data) ->
    gen_server:cast(name(Type), {update, Data}).

%% @doc rank server name
-spec name(Type :: non_neg_integer()) -> atom().
name(Type) ->
    binary_to_atom(<<"rank_server_", (integer_to_binary(Type))/binary>>, utf8).

%% @doc rank data
-spec rank(Type :: non_neg_integer()) -> [#rank{}].
rank(Type) ->
    Name = name(Type),
    sorter:data(Name).

%% @doc query
-spec query(Protocol :: non_neg_integer()) -> ok().
query(Protocol) ->
    {ok, rank(Protocol - ?PROTOCOL_RANK)}.

%% @doc query center
-spec query_center(User :: #user{}, Protocol :: non_neg_integer()) -> ok.
query_center(#user{sender_pid = SenderPid}, Protocol) ->
    node:up_cast_center(name(Protocol - ?PROTOCOL_RANK_CENTER), {'APPLY_CAST', fun() -> user_sender:send(SenderPid, Protocol, rank(Protocol - ?PROTOCOL_RANK_CENTER)) end, []}),
    ok.

%% @doc query world
-spec query_world(User :: #user{}, Protocol :: non_neg_integer()) -> ok.
query_world(#user{sender_pid = SenderPid}, Protocol) ->
    node:up_cast_world(name(Protocol - ?PROTOCOL_RANK_WORLD), {'APPLY_CAST', fun() -> user_sender:send(SenderPid, Protocol, rank(Protocol - ?PROTOCOL_RANK_WORLD)) end, []}),
    ok.

%% @doc new rank
-spec new(Type :: non_neg_integer()) -> {ok, pid()} | {error, term()}.
new(Type) ->
    new(Type, infinity).

%% @doc new rank
-spec new(Type :: non_neg_integer(), Limit :: non_neg_integer() | infinity) -> {ok, pid()} | {error, term()}.
new(Type, Limit) ->
    Name = name(Type),
    start_link(Name, [config:node_type(), Type, Limit]).

%% @doc drop rank
-spec drop(Type :: non_neg_integer()) -> ok.
drop(Type) ->
    gen_server:cast(name(Type), drop).

%% @doc start all
-spec start(Node :: atom()) -> ok.
start(Node) ->
    %% start all rank server, one type per server
    [{ok, _} = start(Type, [Node, Type, Limit]) || {Type, Limit} <- ?RANK_TYPE_LIST],
    ok.

%% @doc start one
-spec start(Type :: term(), Args :: [term()]) -> {ok, pid()} | {error, term()}.
start(Type, Args) ->
    Name = name(Type),
    process:start(Name, ?MODULE, [Name, Args]).

%% @doc server start
-spec start_link(Name :: atom(), Args :: [term()]) -> {ok, pid()} | {error, term()}.
start_link(Name, Args) ->
    gen_server:start_link({local, Name}, ?MODULE, Args, []).
%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
%% @doc init
-spec init(Args :: term()) -> {ok, State :: #state{}}.
init([Node = local, Type, Limit]) ->
    process_flag(trap_exit, true),
    %% construct name with type
    Name = name(Type),
    %% trim redundant data
    db:delete(parser:format(<<"DELETE FROM `rank` WHERE `type` = ~w AND `order` > ~w">>, [Type, Limit])),
    %% load from database
    RankList = rank_sql:select_by_type(Type),
    %% make sorter with origin data, data select from the database will sort with key(rank field)
    Sorter = sorter:new(Name, share, replace, Limit, #rank.key, #rank.value, #rank.time, #rank.order, RankList),
    %% start update loop time
    erlang:send_after(?SECOND_MILLISECONDS(?MINUTE_SECONDS + (Type * ?MINUTE_SECONDS div length(?RANK_TYPE_LIST))), self(), loop),
    {ok, #state{sorter = Sorter, type = Type, name = Name, node = Node}};
init([Node = center, Type, Limit]) ->
    process_flag(trap_exit, true),
    %% construct name with type
    Name = name(Type),
    %% center node only show rank data, not save data
    Sorter = sorter:new(Name, share, replace, Limit, #rank.key, #rank.value, #rank.time, #rank.order, []),
    {ok, #state{sorter = Sorter, type = Type, name = Name, node = Node}};
init([Node = world, Type, Limit]) ->
    process_flag(trap_exit, true),
    %% construct name with type
    Name = name(Type),
    %% world node only show rank data, not save data
    Sorter = sorter:new(Name, share, replace, Limit, #rank.key, #rank.value, #rank.time, #rank.order, []),
    {ok, #state{sorter = Sorter, type = Type, name = Name, node = Node}}.

%% @doc handle_call
-spec handle_call(Request :: term(), From :: {pid(), Tag :: term()}, State :: #state{}) -> {reply, Reply :: term(), NewState :: #state{}}.
handle_call(Request, From, State) ->
    try
        do_call(Request, From, State)
    catch ?EXCEPTION(_Class, Reason, Stacktrace) ->
        ?STACKTRACE(Reason, ?GET_STACKTRACE(Stacktrace)),
        {reply, ok, State}
    end.

%% @doc handle_cast
-spec handle_cast(Request :: term(), State :: #state{}) -> {noreply, NewState :: #state{}} | {stop, term(), NewState :: #state{}}.
handle_cast(Request, State) ->
    try
        do_cast(Request, State)
    catch ?EXCEPTION(_Class, Reason, Stacktrace) ->
        ?STACKTRACE(Reason, ?GET_STACKTRACE(Stacktrace)),
        {noreply, State}
    end.

%% @doc handle_info
-spec handle_info(Request :: term(), State :: #state{}) -> {noreply, NewState :: #state{}}.
handle_info(Info, State) ->
    try
        do_info(Info, State)
    catch ?EXCEPTION(_Class, Reason, Stacktrace) ->
        ?STACKTRACE(Reason, ?GET_STACKTRACE(Stacktrace)),
        {noreply, State}
    end.

%% @doc terminate
-spec terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()), State :: #state{}) -> {ok, NewState :: #state{}}.
terminate({shutdown, drop}, State) ->
    {ok, State};
terminate(_Reason, State = #state{sorter = Sorter, cache = Cache, node = local}) ->
    try
        %% update data when server stop
        sorter:update(Cache, Sorter),
        List = sorter:data(Sorter),
        rank_sql:insert_update(List)
    catch ?EXCEPTION(_Class, Reason, Stacktrace) ->
        ?STACKTRACE(Reason, ?GET_STACKTRACE(Stacktrace))
    end,
    {ok, State};
terminate(_Reason, State) ->
    {ok, State}.

%% @doc code_change
-spec code_change(OldVsn :: (term() | {down, term()}), State :: #state{}, Extra :: term()) -> {ok, NewState :: #state{}}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

do_call(_Info, _From, State) ->
    {reply, ok, State}.

do_cast({'APPLY_CAST', Module, Function, Args},State) ->
    erlang:apply(Module, Function, Args),
    {noreply, State};
do_cast({'APPLY_CAST', Function, Args},State) ->
    erlang:apply(Function, Args),
    {noreply, State};
do_cast({update, Data = #rank{key = Key}}, State = #state{cache = Cache, node = local}) ->
    %% update online role info cache
    New = lists:keystore(Key, #rank.key, Cache, Data),
    {noreply, State#state{cache = New}};
do_cast({update, Data}, State = #state{cache = Cache, node = local}) when is_list(Data) ->
    %% update online role info cache
    New = lists:ukeymerge(#rank.key, Data, Cache),
    {noreply, State#state{cache = New}};
do_cast({update, Data}, State = #state{sorter = Sorter, name = Name, node = center}) ->
    %% update first
    sorter:update(Data, Sorter),
    %% get rank list data
    RankList = sorter:data(Sorter),
    %% sync to world
    node:up_cast_world(Name, {update, RankList}),
    {noreply, State};
do_cast({update, Data}, State = #state{sorter = Sorter, node = world}) ->
    %% update directly
    sorter:update(Data, Sorter),
    {noreply, State};
do_cast(drop, State = #state{sorter = Sorter, type = Type}) ->
    %% drop data
    sorter:drop(Sorter),
    %% delete directly
    rank_sql:delete_by_type(Type),
    %% shutdown it
    {stop, {shutdown, drop}, State};
do_cast(_Info, State) ->
    {noreply, State}.

do_info(loop, State = #state{sorter = Sorter, name = Name, cache = Cache, node = local, tick = Tick}) ->
    erlang:send_after(?MINUTE_MILLISECONDS, self(), loop),
    %% update cache
    sorter:update(Cache, Sorter),
    %% get rank list data
    Data = sorter:data(Sorter),
    %% sync to database, 3 minutes
    _ = Tick rem 3 == 0 andalso rank_sql:insert_update(Data) =/= [],
    %% sync to center
    node:up_cast_center(Name, {update, Data}),
    %% process:cast(center, Name, {update, Data}),
    {noreply, State#state{cache = [], tick = Tick + 1}};
do_info(_Info, State) ->
    {noreply, State}.

%%%-------------------------------------------------------------------
%%% @doc
%%% module node server
%%% @end
%%%-------------------------------------------------------------------
-module(node_server).
-behaviour(gen_server).
%% export API function
-export([connect/1, is_connected/1]).
-export([call/4, cast/4]).
-export([start/1, start_link/1]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-include("common.hrl").
-record(node, {type, node, status}).
-record(state, {node, center, big_world, list = []}).
%%%===================================================================
%%% API
%%%===================================================================
%% @doc connect active
-spec connect(Node :: atom()) -> ok.
connect(Node) ->
    erlang:send(process:pid(?MODULE), Node),
    ok.

%% @doc is_connected
-spec is_connected(Node :: atom()) -> boolean().
is_connected(Node) ->
    case ets:lookup(?MODULE, Node) of
        [#node{status = 1}] ->
            true;
        _ ->
            false
    end.

%% @doc call
-spec call(Node :: atom(), Module :: atom(), Function :: atom(), Args :: [term()]) -> term() | undefined.
call(Node, Module, Function, Args) ->
    case ets:lookup(?MODULE, Node) of
        [#node{node = NodeName, status = 1}] ->
            rpc:call(NodeName, Module, Function, Args);
        _ ->
            undefined
    end.

%% @doc cast
-spec cast(Node :: atom(), Module :: atom(), Function :: atom(), Args :: [term()]) -> ok | undefined.
cast(Node, Module, Function, Args) ->
    case ets:lookup(?MODULE, Node) of
        [#node{node = NodeName, status = 1}] ->
            rpc:cast(NodeName, Module, Function, Args);
        _ ->
            undefined
    end.

%% @doc start
start(Type) ->
    process:start(?MODULE, [Type]).

%% @doc server start
start_link(Args) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Args, []).
%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init(local) ->
    [_Name, IP | _] = string:tokens(atom_to_list(node()), "@"),
    DebugNode = list_to_atom(lists:concat([debug, "@", IP])),
    net_kernel:allow([node(), DebugNode]),
    ets:new(?MODULE, [named_table, {keypos, #node.type}, {read_concurrency, true}, set]),
    erlang:send_after(10 * 1000, self(), 'connect_center'),
    erlang:send_after(10 * 1000, self(), 'connect_big_world'),
    {ok, #state{node = local}};
init(center) ->
    ets:new(?MODULE, [named_table, {keypos, #node.type}, {read_concurrency, true}, set]),
    erlang:send_after(10 * 1000, self(), 'connect_big_world'),
    {ok, #state{node = center}};
init(Type) ->
    {ok, #state{node = Type}}.

handle_call(_Info, _From, State) ->
    {reply, ok, State}.

handle_cast({add, Node}, State = #state{node = center, list = List}) ->
    New = lists:usort([Node | List]),
    {noreply, State#state{list = New}};
handle_cast({add, Node}, State = #state{node = big_world, list = List}) ->
    New = lists:usort([Node | List]),
    {noreply, State#state{list = New}};
handle_cast(_Info, State) ->
    {noreply, State}.

handle_info('connect_center', State = #state{node = local, center = undefined}) ->
    [Name, IP | _] = string:tokens(atom_to_list(node()), "@"),
    CenterName = data_node:get(list_to_atom(Name)),
    CenterNode = list_to_atom(lists:concat([CenterName, "@", IP])),
    net_kernel:allow([CenterNode]),
    Node = connect(center, CenterNode, 'connect_center'),
    {noreply, State#state{center = Node}};
handle_info('connect_big_world', State = #state{node = local, big_world = undefined}) ->
    [_Name, IP | _] = string:tokens(atom_to_list(node()), "@"),
    BigWorldNode = list_to_atom(lists:concat([big_world, "@", IP])),
    net_kernel:allow([BigWorldNode]),
    Node = connect(big_world, BigWorldNode, 'connect_big_world'),
    {noreply, State#state{big_world = Node}};
handle_info('connect_big_world', State = #state{node = center, big_world = undefined}) ->
    [_Name, IP | _] = string:tokens(atom_to_list(node()), "@"),
    BigWorldNode = list_to_atom(lists:concat([big_world, "@", IP])),
    Node = connect(big_world, BigWorldNode, 'connect_big_world'),
    {noreply, State#state{big_world = Node}};
handle_info('stop', State) ->
    {stop, normal, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    {ok, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
%% ====================================================================
%% Internal functions
%% ====================================================================
connect(Type, Node, Msg) ->
    case net_adm:ping(Node) of
        pong ->
            %% connect success
            rpc:cast(Node, gen_server, cast, [?MODULE, {add, node()}]),
            ets:insert(?MODULE, #node{type = Type, node = Node, status = 1}),
            Node;
        pang ->
            %% try connect
            erlang:send_after(?MINUTE_SECONDS * 1000, self(), Msg),
            undefined
    end.
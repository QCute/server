%%%-------------------------------------------------------------------
%%% @doc
%%% module node server
%%% @end
%%%-------------------------------------------------------------------
-module(node_server).
-behaviour(gen_server).
%% API
-export([connect/1, is_connected/1]).
-export([call_center/4, cast_center/4]).
-export([call_local/4, cast_local/4]).
-export([start/1, start_link/1]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
%% Includes
-include("common.hrl").
-record(node, {type, name, server_id, status}).
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
-spec call_center(Type :: atom(), Module :: atom(), Function :: atom(), Args :: [term()]) -> term() | undefined.
call_center(Type, Module, Function, Args) ->
    case ets:lookup(?MODULE, Type) of
        [#node{name = NodeName, status = 1}] ->
            rpc:call(NodeName, Module, Function, Args);
        _ ->
            undefined
    end.

%% @doc cast
-spec cast_center(Type :: atom(), Module :: atom(), Function :: atom(), Args :: [term()]) -> ok | undefined.
cast_center(Type, Module, Function, Args) ->
    case ets:lookup(?MODULE, Type) of
        [#node{name = NodeName, status = 1}] ->
            rpc:cast(NodeName, Module, Function, Args);
        _ ->
            undefined
    end.

%% @doc call
-spec call_local(ServerId :: non_neg_integer(), Module :: atom(), Function :: atom(), Args :: [term()]) -> term() | undefined.
call_local(ServerId, Module, Function, Args) ->
    case ets:lookup(?MODULE, {local, ServerId}) of
        [#node{name = NodeName, status = 1}] ->
            rpc:call(NodeName, Module, Function, Args);
        _ ->
            undefined
    end.

%% @doc cast
-spec cast_local(ServerId :: non_neg_integer(), Module :: atom(), Function :: atom(), Args :: [term()]) -> ok | undefined.
cast_local(ServerId, Module, Function, Args) ->
    case ets:lookup(?MODULE, {local, ServerId}) of
        [#node{name = NodeName, status = 1}] ->
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
    ets:new(?MODULE, [named_table, {keypos, #node.type}, {read_concurrency, true}, set]),
    erlang:send_after(10 * 1000, self(), 'connect_center'),
    erlang:send_after(10 * 1000, self(), 'connect_big_world'),
    {ok, #state{node = local}};
init(center) ->
    ets:new(?MODULE, [named_table, {keypos, #node.type}, {read_concurrency, true}, set]),
    erlang:send_after(10 * 1000, self(), 'connect_big_world'),
    {ok, #state{node = center}};
init(Type) ->
    ets:new(?MODULE, [named_table, {keypos, #node.type}, {read_concurrency, true}, set]),
    {ok, #state{node = Type}}.

handle_call(_Info, _From, State) ->
    {reply, ok, State}.

handle_cast({add, Type, ServerId, Node}, State = #state{node = center, list = List}) ->
    New = lists:usort([Node | List]),
    ets:insert(?MODULE, #node{type = {Type, ServerId}, server_id = ServerId, name = Node, status = 1}),
    {noreply, State#state{list = New}};
handle_cast({add, Type, ServerId, Node}, State = #state{node = big_world, list = List}) ->
    New = lists:usort([Node | List]),
    ets:insert(?MODULE, #node{type = {Type, ServerId}, server_id = ServerId, name = Node, status = 1}),
    {noreply, State#state{list = New}};
handle_cast({apply, Module, Function, Args},State) ->
    catch erlang:apply(Module, Function, Args),
    {noreply, State};
handle_cast({apply, Function, Args},State) ->
    catch erlang:apply(Function, Args),
    {noreply, State};
handle_cast(_Info, State) ->
    {noreply, State}.

handle_info('connect_center', State = #state{node = local, center = undefined}) ->
    [Name, IP | _] = string:tokens(atom_to_list(node()), "@"),
    CenterName = data_node:get(list_to_atom(Name)),
    CenterNode = list_to_atom(lists:concat([CenterName, "@", ip(Name, IP)])),
    Node = connect(local, center, CenterNode, 'connect_center'),
    {noreply, State#state{center = Node}};
handle_info('connect_big_world', State = #state{node = Type, big_world = undefined}) ->
    [_Name, IP | _] = string:tokens(atom_to_list(node()), "@"),
    BigWorldNode = list_to_atom(lists:concat([big_world, "@", IP])),
    Node = connect(Type, big_world, BigWorldNode, 'connect_big_world'),
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
connect(LocalType, CenterType, Node, Msg) ->
    case net_adm:ping(Node) of
        pong ->
            %% connect success
            %% @ todo replace server id
            {ok, ServerId} = application:get_env(server_id),
            rpc:cast(Node, gen_server, cast, [?MODULE, {add, LocalType, ServerId, node()}]),
            ets:insert(?MODULE, #node{type = CenterType, name = Node, status = 1}),
            Node;
        pang ->
            %% try connect
            erlang:send_after(?MINUTE_SECONDS * 1000, self(), Msg),
            undefined
    end.

%% chose local ip when ip not set
ip(Node, LocalIP) ->
    case data_node:ip(Node) of
        [] ->
            LocalIP;
        IP ->
            IP
    end.
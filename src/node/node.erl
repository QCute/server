%%%-------------------------------------------------------------------
%%% @doc
%%% module node server
%%% @end
%%%-------------------------------------------------------------------
-module(node).
-behaviour(gen_server).
%% API
-export([type_to_integer/1, type_to_atom/1]).
-export([connect/1, is_connected/1]).
-export([call_world/3, cast_world/3]).
-export([call_center/3, cast_center/3]).
-export([call_center/4, cast_center/4]).
-export([call_local/4, cast_local/4]).
-export([start/1, start_link/1]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
%% Includes
-include("common.hrl").
%% Records
%% node
-record(node, {id, name, type, server_id = 0, status = 0}).
%% server state
-record(state, {node_type, center, world}).
%%%===================================================================
%%% API
%%%===================================================================
%% @doc atom node type to integer node type
-spec type_to_integer(local | center | world) -> 1 | 2 | 4.
type_to_integer(local) ->
    1;
type_to_integer(center) ->
    2;
type_to_integer(world) ->
    4.

%% @doc integer node type to atom node type
-spec type_to_atom(1 | 2 | 4) -> local | center | world.
type_to_atom(1) ->
    local;
type_to_atom(2) ->
    center;
type_to_atom(4) ->
    world.

%% @doc connect active
-spec connect(Node :: atom()) -> ok.
connect(Node) ->
    process:info(?MODULE, {connect, Node}),
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

%% @doc call world for local and center use
-spec call_world(Module :: atom(), Function :: atom(), Args :: [term()]) -> term() | undefined.
call_world(Module, Function, Args) ->
    case ets:lookup(?MODULE, world) of
        [#node{name = NodeName, status = 1}] ->
            rpc:call(NodeName, Module, Function, Args);
        _ ->
            undefined
    end.

%% @doc cast world for local and center use
-spec cast_world(Module :: atom(), Function :: atom(), Args :: [term()]) -> ok | undefined.
cast_world(Module, Function, Args) ->
    case ets:lookup(?MODULE, world) of
        [#node{name = NodeName, status = 1}] ->
            rpc:cast(NodeName, Module, Function, Args);
        _ ->
            undefined
    end.

%% @doc call center for local use
-spec call_center(Module :: atom(), Function :: atom(), Args :: [term()]) -> term() | undefined.
call_center(Module, Function, Args) ->
    case ets:lookup(?MODULE, center) of
        [#node{name = NodeName, status = 1}] ->
            rpc:call(NodeName, Module, Function, Args);
        _ ->
            undefined
    end.

%% @doc cast center for local use
-spec cast_center(Module :: atom(), Function :: atom(), Args :: [term()]) -> ok | undefined.
cast_center(Module, Function, Args) ->
    case ets:lookup(?MODULE, center) of
        [#node{name = NodeName, status = 1}] ->
            rpc:cast(NodeName, Module, Function, Args);
        _ ->
            undefined
    end.

%% @doc call center for world use
-spec call_center(ServerId :: non_neg_integer(), Module :: atom(), Function :: atom(), Args :: [term()]) -> term() | undefined.
call_center(ServerId, Module, Function, Args) ->
    case ets:lookup(?MODULE, ServerId) of
        [#node{name = NodeName, status = 1}] ->
            rpc:call(NodeName, Module, Function, Args);
        _ ->
            undefined
    end.

%% @doc cast center for world use
-spec cast_center(ServerId :: non_neg_integer(), Module :: atom(), Function :: atom(), Args :: [term()]) -> ok | undefined.
cast_center(ServerId, Module, Function, Args) ->
    case ets:lookup(?MODULE, ServerId) of
        [#node{name = NodeName, status = 1}] ->
            rpc:cast(NodeName, Module, Function, Args);
        _ ->
            undefined
    end.

%% @doc call local for center and world use
-spec call_local(ServerId :: non_neg_integer(), Module :: atom(), Function :: atom(), Args :: [term()]) -> term() | undefined.
call_local(ServerId, Module, Function, Args) ->
    case ets:lookup(?MODULE, ServerId) of
        [#node{name = NodeName, status = 1}] ->
            rpc:call(NodeName, Module, Function, Args);
        _ ->
            undefined
    end.

%% @doc cast local for center and world use
-spec cast_local(ServerId :: non_neg_integer(), Module :: atom(), Function :: atom(), Args :: [term()]) -> ok | undefined.
cast_local(ServerId, Module, Function, Args) ->
    case ets:lookup(?MODULE, ServerId) of
        [#node{name = NodeName, status = 1}] ->
            rpc:cast(NodeName, Module, Function, Args);
        _ ->
            undefined
    end.

%% @doc start
-spec start(Type :: atom()) -> {ok, Pid :: pid()} | {error, term()}.
start(Type) ->
    process:start(?MODULE, [Type]).

%% @doc server start
-spec start_link(Args :: [term()]) -> {ok, Pid :: pid()} | {error, term()}.
start_link(Args) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Args, []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init(NodeType = local) ->
    %% one center/world connected in theory
    %% so, local node use node type as key
    ets:new(?MODULE, [named_table, {keypos, #node.id}, {read_concurrency, true}, set]),
    erlang:send_after(10 * 1000, self(), {connect, center}),
    erlang:send_after(10 * 1000, self(), {connect, world}),
    {ok, #state{node_type = NodeType}};
init(NodeType = center) ->
    %% center will connect multi local node
    %% so, center node use server id as key
    ets:new(?MODULE, [named_table, {keypos, #node.id}, {read_concurrency, true}, set]),
    erlang:send_after(10 * 1000, self(), {connect, world}),
    {ok, #state{node_type = NodeType}};
init(NodeType = world) ->
    %% world will connect multi local/center node
    %% so, world node use server id as key
    ets:new(?MODULE, [named_table, {keypos, #node.id}, {read_concurrency, true}, set]),
    {ok, #state{node_type = NodeType}}.

handle_call(_Info, _From, State) ->
    {reply, ok, State}.


handle_cast({reply, Type = center, ServerId, Node}, State = #state{node_type = local}) ->
    %% center node type as id
    ets:insert(?MODULE, #node{id = Type, type = Type, server_id = ServerId, name = Node, status = 1}),
    {noreply, State#state{center = Node}};
handle_cast({reply, Type = world, ServerId, Node}, State = #state{node_type = local}) ->
    %% world node type as id
    ets:insert(?MODULE, #node{id = Type, type = Type, server_id = ServerId, name = Node, status = 1}),
    {noreply, State#state{world = Node}};
handle_cast({reply, Type = world, ServerId, Node}, State = #state{node_type = center}) ->
    %% world node type as id
    ets:insert(?MODULE, #node{id = Type, type = Type, server_id = ServerId, name = Node, status = 1}),
    {noreply, State#state{world = Node}};
handle_cast({connect, Type = local, ServerId, Node, Pid}, State = #state{node_type = NodeType = center}) ->
    %% local node server id as id
    ets:insert(?MODULE, #node{id = ServerId, type = Type, server_id = ServerId, name = Node, status = 1}),
    {ok, SelfServerId} = application:get_env(server_id),
    gen_server:cast(Pid, {reply, NodeType, SelfServerId, node()}),
    {noreply, State};
handle_cast({connect, Type, ServerId, Node, Pid}, State = #state{node_type = NodeType = world}) ->
    %% local/center node server id as id
    ets:insert(?MODULE, #node{id = ServerId, type = Type, server_id = ServerId, name = Node, status = 1}),
    {ok, SelfServerId} = application:get_env(server_id),
    gen_server:cast(Pid, {reply, NodeType, SelfServerId, node()}),
    {noreply, State};

%% apply cast
handle_cast({apply_cast, Module, Function, Args},State) ->
    catch erlang:apply(Module, Function, Args),
    {noreply, State};
handle_cast({apply_cast, Function, Args},State) ->
    catch erlang:apply(Function, Args),
    {noreply, State};

handle_cast(_Info, State) ->
    {noreply, State}.

handle_info({connect, Type = center}, State = #state{node_type = NodeType}) ->
    [Name, IP | _] = string:tokens(atom_to_list(node()), "@"),
    CenterNode = node_data:center_node(type:to_atom(Name)),
    CenterIP = tool:default(node_data:center_ip(type:to_atom(Name)), IP),
    Node = type:to_atom(lists:concat([CenterNode, "@", CenterIP])),
    connect_node(NodeType, Type, Node),
    {noreply, State};
handle_info({connect, Type = world}, State = #state{node_type = NodeType}) ->
    [_, IP | _] = string:tokens(atom_to_list(node()), "@"),
    CenterNode = hd(tool:default(node_data:server_node(Type), [""])),
    CenterIP = tool:default(hd(tool:default(node_data:server_ip(Type), [IP])), IP),
    Node = type:to_atom(lists:concat([CenterNode, "@", CenterIP])),
    connect_node(NodeType, Type, Node),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    {ok, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
%% ====================================================================
%% Internal functions
%% ====================================================================
connect_node(SelfNodeType, ConnectNodeType, Node) ->
    case net_adm:ping(Node) of
        pong ->
            %% connect success
            {ok, SelfServerId} = application:get_env(server_id),
            rpc:cast(Node, gen_server, cast, [?MODULE, {connect, SelfNodeType, SelfServerId, node(), self()}]);
        pang ->
            %% try connect one minutes ago
            erlang:send_after(?MINUTE_SECONDS * 1000, self(), {connect, ConnectNodeType})
    end.

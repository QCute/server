%%%-------------------------------------------------------------------
%%% @doc
%%% module node cluster manage server
%%% @end
%%%-------------------------------------------------------------------
-module(node).
-behaviour(gen_server).
%% API
-export([type_to_integer/1, type_to_atom/1]).
-export([connect/1, is_connected/1]).
%% local or center use
-export([up_call_world/2, up_call_world/3, up_cast_world/2, up_cast_world/3]).
%% local use
-export([up_call_center/2, up_call_center/3, up_cast_center/2, up_cast_center/3]).
%% world use
-export([down_call_center/3, down_call_center/4, down_cast_center/3, down_cast_center/4]).
%% center or world use
-export([down_call_local/3, down_call_local/4, down_cast_local/3, down_cast_local/4]).
%% start
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
%%% API functions
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

%% @doc connect
-spec connect(Node :: atom()) -> term().
connect(Node) ->
    erlang:send(?MODULE, {connect, Node}).

%% @doc is connected
-spec is_connected(Node :: atom()) -> boolean().
is_connected(Node) ->
    case ets:lookup(?MODULE, Node) of
        [#node{status = 1}] ->
            true;
        _ ->
            false
    end.

%% @doc call world for local and center use
-spec up_call_world(Name :: pid() | atom(), Request :: term()) -> term() | undefined.
up_call_world(Name, Request) ->
    case ets:lookup(?MODULE, world) of
        [#node{name = NodeName, status = 1}] ->
            gen_server:call({Name, NodeName}, Request, ?CALL_TIMEOUT);
        _ ->
            undefined
    end.

%% @doc call world for local and center use
-spec up_call_world(Module :: atom(), Function :: atom(), Args :: [term()]) -> term() | undefined.
up_call_world(Module, Function, Args) ->
    case ets:lookup(?MODULE, world) of
        [#node{name = NodeName, status = 1}] ->
            rpc:call(NodeName, Module, Function, Args);
        _ ->
            undefined
    end.

%% @doc cast world for local and center use
-spec up_cast_world(Name :: pid() | atom(), Request :: term()) -> ok | undefined.
up_cast_world(Name, Request) ->
    case ets:lookup(?MODULE, world) of
        [#node{name = NodeName, status = 1}] ->
            gen_server:cast({Name, NodeName}, Request);
        _ ->
            undefined
    end.

%% @doc cast world for local and center use
-spec up_cast_world(Module :: atom(), Function :: atom(), Args :: [term()]) -> ok | undefined.
up_cast_world(Module, Function, Args) ->
    case ets:lookup(?MODULE, world) of
        [#node{name = NodeName, status = 1}] ->
            rpc:cast(NodeName, Module, Function, Args);
        _ ->
            undefined
    end.

%% @doc call center for local use
-spec up_call_center(Name :: pid() | atom(), Request :: term()) -> term() | undefined.
up_call_center(Name, Request) ->
    case ets:lookup(?MODULE, center) of
        [#node{name = NodeName, status = 1}] ->
            gen_server:call({Name, NodeName}, Request, ?CALL_TIMEOUT);
        _ ->
            undefined
    end.

%% @doc call center for local use
-spec up_call_center(Module :: atom(), Function :: atom(), Args :: [term()]) -> term() | undefined.
up_call_center(Module, Function, Args) ->
    case ets:lookup(?MODULE, center) of
        [#node{name = NodeName, status = 1}] ->
            rpc:call(NodeName, Module, Function, Args, ?CALL_TIMEOUT);
        _ ->
            undefined
    end.

%% @doc cast center for local use
-spec up_cast_center(Name :: pid() | atom(), Request :: term()) -> ok | undefined.
up_cast_center(Name, Request) ->
    case ets:lookup(?MODULE, center) of
        [#node{name = NodeName, status = 1}] ->
            gen_server:cast({Name, NodeName}, Request);
        _ ->
            undefined
    end.

%% @doc cast center for local use
-spec up_cast_center(Module :: atom(), Function :: atom(), Args :: [term()]) -> ok | undefined.
up_cast_center(Module, Function, Args) ->
    case ets:lookup(?MODULE, center) of
        [#node{name = NodeName, status = 1}] ->
            rpc:cast(NodeName, Module, Function, Args);
        _ ->
            undefined
    end.

%% @doc call center for world use
-spec down_call_center(ServerId :: non_neg_integer(), Name :: pid() | atom(), Request :: term()) -> term() | undefined.
down_call_center(ServerId, Name, Request) ->
    case ets:lookup(?MODULE, ServerId) of
        [#node{name = NodeName, status = 1}] ->
            gen_server:call({Name, NodeName}, Request, ?CALL_TIMEOUT);
        _ ->
            undefined
    end.

%% @doc call center for world use
-spec down_call_center(ServerId :: non_neg_integer(), Module :: atom(), Function :: atom(), Args :: [term()]) -> term() | undefined.
down_call_center(ServerId, Module, Function, Args) ->
    case ets:lookup(?MODULE, ServerId) of
        [#node{name = NodeName, status = 1}] ->
            rpc:call(NodeName, Module, Function, Args, ?CALL_TIMEOUT);
        _ ->
            undefined
    end.

%% @doc cast center for world use
-spec down_cast_center(ServerId :: non_neg_integer(), Name :: pid() | atom(), Request :: term()) -> ok | undefined.
down_cast_center(ServerId, Name, Request) ->
    case ets:lookup(?MODULE, ServerId) of
        [#node{name = NodeName, status = 1}] ->
            gen_server:cast({Name, NodeName}, Request);
        _ ->
            undefined
    end.

%% @doc cast center for world use
-spec down_cast_center(ServerId :: non_neg_integer(), Module :: atom(), Function :: atom(), Args :: [term()]) -> ok | undefined.
down_cast_center(ServerId, Module, Function, Args) ->
    case ets:lookup(?MODULE, ServerId) of
        [#node{name = NodeName, status = 1}] ->
            rpc:cast(NodeName, Module, Function, Args);
        _ ->
            undefined
    end.

%% @doc call local for center and world use
-spec down_call_local(ServerId :: non_neg_integer(), Name :: pid() | atom(), Request :: term()) -> term() | undefined.
down_call_local(ServerId, Name, Request) ->
    case ets:lookup(?MODULE, ServerId) of
        [#node{name = NodeName, status = 1}] ->
            gen_server:call({Name, NodeName}, Request, ?CALL_TIMEOUT);
        _ ->
            undefined
    end.

%% @doc call local for center and world use
-spec down_call_local(ServerId :: non_neg_integer(), Module :: atom(), Function :: atom(), Args :: [term()]) -> term() | undefined.
down_call_local(ServerId, Module, Function, Args) ->
    case ets:lookup(?MODULE, ServerId) of
        [#node{name = NodeName, status = 1}] ->
            rpc:call(NodeName, Module, Function, Args, ?CALL_TIMEOUT);
        _ ->
            undefined
    end.

%% @doc cast local for center and world use
-spec down_cast_local(ServerId :: non_neg_integer(), Name :: pid() | atom(), Request :: term()) -> ok | undefined.
down_cast_local(ServerId, Name, Request) ->
    case ets:lookup(?MODULE, ServerId) of
        [#node{name = NodeName, status = 1}] ->
            gen_server:cast({Name, NodeName}, Request);
        _ ->
            undefined
    end.

%% @doc cast local for center and world use
-spec down_cast_local(ServerId :: non_neg_integer(), Module :: atom(), Function :: atom(), Args :: [term()]) -> ok | undefined.
down_cast_local(ServerId, Module, Function, Args) ->
    case ets:lookup(?MODULE, ServerId) of
        [#node{name = NodeName, status = 1}] ->
            rpc:cast(NodeName, Module, Function, Args);
        _ ->
            undefined
    end.

%% @doc start
-spec start(Type :: atom()) -> {ok, pid()} | {error, term()}.
start(Type) ->
    process:start(?MODULE, [Type]).

%% @doc server start
-spec start_link(Args :: [term()]) -> {ok, pid()} | {error, term()}.
start_link(Args) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Args, []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
%% @doc init
-spec init(Args :: term()) -> {ok, State :: #state{}}.
init(NodeType = local) ->
    process_flag(trap_exit, true),
    %% one center/world connected in theory
    %% so, local node use node type as key
    ets:new(?MODULE, [named_table, {keypos, #node.id}, {read_concurrency, true}, set]),
    erlang:send_after(?MILLISECONDS(10), self(), {connect, center}),
    erlang:send_after(?MILLISECONDS(10), self(), {connect, world}),
    {ok, #state{node_type = NodeType}};
init(NodeType = center) ->
    process_flag(trap_exit, true),
    %% center will connect multi local node
    %% so, center node use server id as key
    ets:new(?MODULE, [named_table, {keypos, #node.id}, {read_concurrency, true}, set]),
    erlang:send_after(?MILLISECONDS(10), self(), {connect, world}),
    {ok, #state{node_type = NodeType}};
init(NodeType = world) ->
    process_flag(trap_exit, true),
    %% world will connect multi local/center node
    %% so, world node use server id as key
    ets:new(?MODULE, [named_table, {keypos, #node.id}, {read_concurrency, true}, set]),
    {ok, #state{node_type = NodeType}}.

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
-spec handle_cast(Request :: term(), State :: #state{}) -> {noreply, NewState :: #state{}}.
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

%% apply cast
do_cast({'APPLY_CAST', Module, Function, Args},State) ->
    erlang:apply(Module, Function, Args),
    {noreply, State};
do_cast({'APPLY_CAST', Function, Args},State) ->
    erlang:apply(Function, Args),
    {noreply, State};
do_cast({reply, Type = center, ServerId, Node}, State = #state{node_type = local}) ->
    %% center node type as id
    ets:insert(?MODULE, #node{id = Type, type = Type, server_id = ServerId, name = Node, status = 1}),
    {noreply, State#state{center = Node}};
do_cast({reply, Type = world, ServerId, Node}, State = #state{node_type = local}) ->
    %% world node type as id
    ets:insert(?MODULE, #node{id = Type, type = Type, server_id = ServerId, name = Node, status = 1}),
    {noreply, State#state{world = Node}};
do_cast({reply, Type = world, ServerId, Node}, State = #state{node_type = center}) ->
    %% world node type as id
    ets:insert(?MODULE, #node{id = Type, type = Type, server_id = ServerId, name = Node, status = 1}),
    {noreply, State#state{world = Node}};
do_cast({connect, Type = local, ServerId, Node, Pid}, State = #state{node_type = NodeType = center}) ->
    %% local node server id as id
    ets:insert(?MODULE, #node{id = ServerId, type = Type, server_id = ServerId, name = Node, status = 1}),
    {ok, SelfServerId} = application:get_env(server_id),
    gen_server:cast(Pid, {reply, NodeType, SelfServerId, node()}),
    {noreply, State};
do_cast({connect, Type, ServerId, Node, Pid}, State = #state{node_type = NodeType = world}) ->
    %% local/center node server id as id
    ets:insert(?MODULE, #node{id = ServerId, type = Type, server_id = ServerId, name = Node, status = 1}),
    {ok, SelfServerId} = application:get_env(server_id),
    gen_server:cast(Pid, {reply, NodeType, SelfServerId, node()}),
    {noreply, State};
do_cast(_Info, State) ->
    {noreply, State}.

do_info({connect, Type = center}, State = #state{node_type = NodeType}) ->
    [Name, IP | _] = string:tokens(atom_to_list(node()), "@"),
    CenterNode = node_data:center_node(list_to_atom(Name)),
    CenterIP = tool:default(node_data:center_ip(list_to_atom(Name)), IP),
    Node = list_to_atom(lists:concat([CenterNode, "@", CenterIP])),
    connect_node(NodeType, Type, Node),
    {noreply, State};
do_info({connect, Type = world}, State = #state{node_type = NodeType}) ->
    [_, IP | _] = string:tokens(atom_to_list(node()), "@"),
    CenterNode = hd(tool:default(node_data:server_node(Type), [""])),
    CenterIP = tool:default(hd(tool:default(node_data:server_ip(Type), [IP])), IP),
    Node = list_to_atom(lists:concat([CenterNode, "@", CenterIP])),
    connect_node(NodeType, Type, Node),
    {noreply, State};
do_info(_Info, State) ->
    {noreply, State}.

%% connect to node
connect_node(SelfNodeType, ConnectNodeType, Node) ->
    case net_adm:ping(Node) of
        pong ->
            %% connect success
            {ok, SelfServerId} = application:get_env(server_id),
            rpc:cast(Node, gen_server, cast, [?MODULE, {connect, SelfNodeType, SelfServerId, node(), self()}]);
        pang ->
            %% try connect one minutes ago
            erlang:send_after(?MINUTE_MILLISECONDS(1), self(), {connect, ConnectNodeType})
    end.

%%%-------------------------------------------------------------------
%%% @doc
%%% module server proxy
%%% @end
%%%-------------------------------------------------------------------
-module(proxy).
%% API
-export([start_link/2]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
%% Includes
-include("../../../include/common.hrl").
%%%===================================================================
%%% API functions
%%%===================================================================
%% @doc start link
-spec start_link(SocketType :: gen_tcp | ssl, Host :: string()) -> {ok, pid()} | {error, term()}.
start_link(SocketType, Host) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [SocketType, Host], []).
%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([gen_tcp, Host]) ->
    %% port
    Port = config:net_gen_tcp_port() + config:server_id(),
    %% server socket
    {ok, InnerSocket} = gen_tcp:connect({127, 0, 0, 1}, Port, []),
    %% proxy socket
    {ok, IP} = inet:getaddr(Host, inet),
    %% port map
    {ok, OuterSocket} = gen_tcp:connect(IP, Port + 10000, []),
    gen_tcp:send(OuterSocket, <<>>),
    {ok, {InnerSocket, OuterSocket}};
init([ssl, Host]) ->
    %% port
    Port = config:net_ssl_port() + config:server_id(),
    %% server socket
    {ok, InnerSocket} = ssl:connect({127, 0, 0, 1}, Port, []),
    %% proxy socket
    {ok, IP} = inet:getaddr(Host, inet),
    %% port map
    {ok, OuterSocket} = ssl:connect(IP, Port + 10000, []),
    ssl:send(OuterSocket, <<>>),
    {ok, {InnerSocket, OuterSocket}}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info({tcp, InnerSocket, Data}, State = {InnerSocket, OuterSocket}) ->
    try
        gen_tcp:send(OuterSocket, Data)
    catch ?EXCEPTION(_Class, Reason, Stacktrace) ->
        ?STACKTRACE(Reason, ?GET_STACKTRACE(Stacktrace))
    end,
    {noreply, State};
handle_info({tcp, OuterSocket, Data}, State = {InnerSocket, OuterSocket}) ->
    try
        gen_tcp:send(InnerSocket, Data)
    catch ?EXCEPTION(_Class, Reason, Stacktrace) ->
        ?STACKTRACE(Reason, ?GET_STACKTRACE(Stacktrace))
    end,
    {noreply, State};
handle_info({ssl, InnerSocket, Data}, State = {InnerSocket, OuterSocket}) ->
    try
        ssl:send(OuterSocket, Data)
    catch ?EXCEPTION(_Class, Reason, Stacktrace) ->
        ?STACKTRACE(Reason, ?GET_STACKTRACE(Stacktrace))
    end,
    {noreply, State};
handle_info({ssl, OuterSocket, Data}, State = {InnerSocket, OuterSocket}) ->
    try
        ssl:send(InnerSocket, Data)
    catch ?EXCEPTION(_Class, Reason, Stacktrace) ->
        ?STACKTRACE(Reason, ?GET_STACKTRACE(Stacktrace))
    end,
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    {ok, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%%-------------------------------------------------------------------
%%% @doc
%%% module process
%%% @end
%%%-------------------------------------------------------------------
-module(process).
-export([alive/1]).
-export([start/1, start/2, pid/1, pid/2]).
-export([call/2, call/3, cast/2, cast/3, info/2, info/3]).
-export([player_name/1, sender_name/1]).
-export([player/1, sender/1]).
%%%===================================================================
%%% API
%%%===================================================================
%% @doc process is alive
-spec alive(Pid :: pid()) -> true | false | term().
alive(Pid) when is_pid(Pid) andalso node(Pid) =:= node() ->
    erlang:is_process_alive(Pid);
alive(Pid) when is_pid(Pid) ->
    case rpc:call(node(Pid), erlang, is_process_alive, [Pid]) of
        {badrpc, _Reason}  ->
            false;
        Result ->
            Result
    end.

%% @doc server start
-spec start(Name :: atom()) -> {ok, Pid :: pid()} | {error, term()}.
start(Name) ->
    server_supervisor:start_child(Name).
-spec start(Name :: atom(), Args :: term()) -> {ok, Pid :: pid()} | {error, term()}.
start(Name, Args) ->
    server_supervisor:start_child(Name, Args).

%% @doc process pid
-spec pid(Name :: atom() | {local, atom()} | {global, atom()}) -> Pid :: pid() | undefined.
pid(Name) ->
    case where(Name) of
        Pid when is_pid(Pid) ->
            Pid;
        _ ->
            case server_supervisor:start_child(Name) of
                {ok, Pid} ->
                    Pid;
                _ ->
                    undefined
            end
    end.

%% @doc process pid
-spec pid(Node :: local | center | big_world, Name :: atom()) -> Pid :: pid() | term().
pid(center, Name) ->
    center:call(?MODULE, pid, [Name]);
pid(big_world, Name) ->
    big_world:call(?MODULE, pid, [Name]);
pid(local, Name) ->
    pid(Name).

%% @doc call
-spec call(Name :: atom(), Request :: term()) -> Result :: term().
call(Name, Request) ->
    call(local, Name, Request).
-spec call(Node :: atom(), Name :: atom(), Request :: term()) -> Result :: term().
call(Node, Name, Request) ->
    gen_server:call(pid(Node, Name), Request).

%% @doc cast
-spec cast(Name :: atom(), Request :: term()) -> ok.
cast(Name, Request) ->
    cast(local, Name, Request).
-spec cast(Node :: atom(), Name :: atom(), Request :: term()) -> ok.
cast(Node, Name, Request) ->
    gen_server:cast(pid(Node, Name), Request).

%% @doc info
-spec info(Name :: atom(), Request :: term()) -> Request :: term().
info(Name, Request) ->
    info(local, Name, Request).
-spec info(Node :: atom(), Name :: atom(), Request :: term()) -> Request :: term().
info(Node, Name, Request) ->
    erlang:send(pid(Node, Name), Request).

%% @doc where
-spec where(Name :: term()) -> Pid :: pid() | undefined.
where({local, Name}) ->
    erlang:whereis(Name);
where({global, Name}) ->
    global:whereis_name(Name);
where(Name) ->
    erlang:whereis(Name).

%% @doc 玩家进程名
-spec player_name(PlayerId :: non_neg_integer()) -> atom().
player_name(PlayerId) ->
    type:to_atom(lists:concat([player_, PlayerId])).

%% @doc 玩家写消息进程名
-spec sender_name(PlayerId :: non_neg_integer()) -> atom().
sender_name(PlayerId) ->
    type:to_atom(lists:concat([player_sender_, PlayerId])).

%% @doc 获取玩家进程Pid
-spec player(PlayerId :: non_neg_integer()) -> Pid :: pid() | term().
player(PlayerId) ->
    erlang:whereis({local, player_name(PlayerId)}).

%% @doc 获取玩家写消息进程Pid
-spec sender(PlayerId :: non_neg_integer()) -> Pid :: pid() | term().
sender(PlayerId) ->
    erlang:whereis({local, sender_name(PlayerId)}).





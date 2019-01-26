%%%-------------------------------------------------------------------
%%% @doc
%%% module process
%%% @end
%%%-------------------------------------------------------------------
-module(process).
-export([start/1, start/2, pid/1, pid/2]).
-export([call/2, call/3, cast/2, cast/3, info/2, info/3]).
-export([player_name/1, sender_name/1]).
-export([player/1, sender/1]).
-export([alive/1]).
%%%===================================================================
%%% API
%%%===================================================================
%% @doc server start
start(Name) ->
    server_supervisor:start_child(Name).
start(Name, Args) ->
    server_supervisor:start_child(Name, Args).

%% @doc process pid
pid(Name) ->
    case where(Name) of
        Pid when is_pid(Pid) ->
            Pid;
        _ ->
            server_supervisor:start_child(Name)
    end.

%% @doc process pid
pid(center, Name) ->
    center:call(?MODULE, pid, [Name]);
pid(big_world, Name) ->
    big_world:call(?MODULE, pid, [Name]);
pid(local, Name) ->
    pid(Name).

%% @doc call
call(Name, Request) ->
    call(local, Name, Request).
call(Node, Name, Request) ->
    gen_server:call(pid(Node, Name), Request).

%% @doc cast
cast(Name, Request) ->
    cast(local, Name, Request).
cast(Node, Name, Request) ->
    gen_server:cast(pid(Node, Name), Request).

%% @doc info
info(Name, Request) ->
    info(local, Name, Request).
info(Node, Name, Request) ->
    erlang:send(pid(Node, Name), Request).


%% @doc where
where({local, Name}) ->
    erlang:whereis(Name);
where({global, Name}) ->
    global:whereis_name(Name);
where(Name) ->
    erlang:whereis(Name).

%% @doc 进程存活
alive(Pid) when is_pid(Pid) ->
    case node(Pid) =:= node() of
        true ->
            erlang:is_process_alive(Pid);
        false ->
            case rpc:call(node(Pid), erlang, is_process_alive, [Pid]) of
                {badrpc, _Reason}  ->
                    false;
                Result ->
                    Result
            end
    end.

%% @doc 玩家进程名
player_name(PlayerId) ->
    type:to_atom(lists:concat([player_, PlayerId])).

%% @doc 玩家写消息进程名
sender_name(PlayerId) ->
    type:to_atom(lists:concat([player_sender_, PlayerId])).

%% @doc 获取玩家进程pid
player(Id) ->
    erlang:whereis({local, player_name(Id)}).

%% @doc 获取玩家写消息进程pid
sender(Id) ->
    erlang:whereis({local, sender_name(Id)}).





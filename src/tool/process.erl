%%%-------------------------------------------------------------------
%%% @doc
%%% module process
%%% @end
%%%-------------------------------------------------------------------
-module(process).
-export([player_name/1, sender_name/1]).
-export([player/1, sender/1]).
-export([alive/1]).
%%%===================================================================
%%% API
%%%===================================================================
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





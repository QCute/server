%%%-------------------------------------------------------------------
%%% @doc
%%% module process
%%% @end
%%%-------------------------------------------------------------------
-module(process).
%% API
-export([start/1, start/2, start/3, pid/1, pid/2, alive/1]).
-export([call/2, call/3, cast/2, cast/3, info/2, info/3]).
-export([role_name/1, sender_name/1]).
-export([role_pid/1, sender_pid/1]).
%%%===================================================================
%%% API
%%%===================================================================
%% @doc server start
-spec start(Name :: atom()) -> {ok, Pid :: pid()} | {error, term()}.
start(Name) ->
    start(Name, []).
-spec start(Name :: atom(), Args :: [term()]) -> {ok, Pid :: pid()} | {error, term()}.
start(Name, Args) ->
    start(Name, Name, Args).
-spec start(Name :: atom(), Module :: module(), Args :: [term()]) -> {ok, Pid :: pid()} | {error, term()}.
start(Name, Module, Args) ->
    %% kill(force termination) worker server after 60 seconds
    ChildSpec = {Name, {Module, start_link, Args}, permanent, 60000, worker, [Name]},
    service_supervisor:start_child(ChildSpec).

%% @doc process pid
-spec pid(Node :: local | center | world, Name :: atom()) -> Pid :: pid() | term().
pid(local, Name) ->
    pid(Name);
pid(Node, Name) ->
    node_server:call_center(Node, ?MODULE, pid, [Name]).

%% @doc process pid
-spec pid(Name :: atom() | {local, atom()} | {global, atom()}) -> Pid :: pid() | undefined.
pid(Name) ->
    case where(Name) of
        Pid when is_pid(Pid) ->
            Pid;
        _ ->
            case start(Name) of
                {ok, Pid} ->
                    Pid;
                _ ->
                    undefined
            end
    end.

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

%% @doc 角色进程名
-spec role_name(RoleId :: non_neg_integer()) -> atom().
role_name(RoleId) ->
    type:to_atom(lists:concat([role_server_, RoleId])).

%% @doc 角色写消息进程名
-spec sender_name(RoleId :: non_neg_integer()) -> atom().
sender_name(RoleId) ->
    type:to_atom(lists:concat([role_sender_, RoleId])).

%% @doc 获取角色进程Pid
-spec role_pid(non_neg_integer() | pid()) -> Pid :: pid() | undefined.
role_pid(Pid) when is_pid(Pid) ->
    Pid;
role_pid(RoleId) when is_integer(RoleId) ->
    where(role_name(RoleId)).

%% @doc 获取角色写消息进程Pid
-spec sender_pid(non_neg_integer() | pid()) -> Pid :: pid() | undefined.
sender_pid(Pid) when is_pid(Pid) ->
    Pid;
sender_pid(RoleId) when is_integer(RoleId) ->
    where(sender_name(RoleId)).

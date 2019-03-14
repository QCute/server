%%%-------------------------------------------------------------------
%%% @doc
%%% module load
%%% @end
%%%-------------------------------------------------------------------
-module(load).
%% export function
-export([load/1, load/2, reload/3, checksum/1]).
%%%===================================================================
%%% API
%%%===================================================================
%% @doc load module for all node, shell execute compatible
-spec load(atom() | [atom()]) -> ok.
load(Modules) ->
    data_node:all(),
    load(Modules).

%% @doc load module (local call)
-spec load(atom() | [atom()], atom() | [atom()]) -> ok.
load(Node, Modules) when is_atom(Node) ->
    load([Node], Modules);
load(Nodes, Module) when is_atom(Module) ->
    load(Nodes, [Module]);
load(Node, Module) when is_atom(Node) andalso is_atom(Module) ->
    load([Node], [Module]);
load(Nodes, Modules) ->
    Ref = make_ref(),
    ChecksumModules = [{Module, checksum(Module)} || Module <- Modules],
    List = [{Node, net_adm:ping(Node) == pong andalso rpc:cast(Node, ?MODULE, reload, [self(), Ref, ChecksumModules])} || Node <- Nodes],
    [receive {Ref, Result} -> handle_result(Result) after 10 * 1000 -> io:format("receive timeout from ~p~n", [Node]) end || {Node, true} <- List],
    [io:format("cannot connect to node:~p~n", [Node]) || {Node, false} <- List],
    ok.

%% handle remote result
handle_result([]) ->
    io:format("~n~n");
handle_result([{Node, Module, true, {module, Module}, true} | T]) ->
    NodePadding = lists:duplicate(24 - length(lists:concat([Node])), " "),
    ModulePadding = lists:duplicate(16 - length(lists:concat([Module])), " "),
    io:format("node:~p~s  module:~p~s  result:~p~n", [Node, NodePadding, Module, ModulePadding, true]),
    handle_result(T);
handle_result([{Node, Module, _, _, Result} | T]) ->
    NodePadding = lists:duplicate(24 - length(lists:concat([Node])), " "),
    ModulePadding = lists:duplicate(16 - length(lists:concat([Module])), " "),
    io:format("node:~p~s   module:~p~s   result:~p~n", [Node, NodePadding, Module, ModulePadding, Result]),
    handle_result(T).

%% @doc soft purge and load module (remote call)
-spec reload(pid(), ref(), [atom()]) -> ok.
reload(Pid, Ref, Modules) ->
    Result = do_reload(Modules, []),
    erlang:send(Pid, {Ref, Result}),
    ok.

do_reload([], Result) ->
    Result;
do_reload([{Module, Vsn} | T], Result) ->
    Purge = code:soft_purge(Module),
    Load = code:load_file(Module),
    Checksum = checksum(Module),
    do_reload(T, [{node(), Module, Purge, Load, Checksum == Vsn} | Result]).

%% @doc beam checksum
-spec checksum(atom()) -> list().
checksum(Module) ->
    case catch Module:module_info(attributes) of
        {'EXIT', _} ->
            [];
        Attributes ->
            proplists:get_value(vsn, Attributes, [])
    end.

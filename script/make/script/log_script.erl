%%%-------------------------------------------------------------------
%%% @doc
%%% module log script
%%% @end
%%%-------------------------------------------------------------------
-module(log_script).
-export([main/1]).
%%%===================================================================
%%% API
%%%===================================================================
main([Key]) ->
    code:add_path(filename:dirname(escript:script_name()) ++ "/../../../beam/"),
    List = [X || X <- log(), atom_to_list(element(3, X)) == Key],
    console:stacktrace(catch maker:start(fun log_maker:parse/2, List)),
    ok;
main(_) ->
    io:format("invail argument~n").

%%%===================================================================
%%% log data
%%%===================================================================
log() ->
    [
        {"src/module/log/log.erl", log, role_log},
        {"src/module/log/log_sql.erl", sql, role_log}
    ].

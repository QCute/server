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
    List = [X || X <- log(), filename:basename(element(1, X), ".erl") == Key],
    console:stacktrace(catch maker:start(fun log_maker:parse/2, List)),
    ok;
main(_) ->
    io:format("invail argument~n").

%%%===================================================================
%%% log data
%%%===================================================================
log() ->
    [
        {"src/module/log/log.erl", log, log_player},
        {"src/module/log/log_sql.erl", sql, log_player}
    ].
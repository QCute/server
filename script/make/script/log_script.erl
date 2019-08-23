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
    console:stacktrace(catch log_maker:start(List));
main(_) ->
    io:format("invail argument~n").

%%%===================================================================
%%% log data
%%%===================================================================
log() ->
    [
        {"src/module/log/log.erl", log, role_log},
        {"src/module/log/log_sql.erl", sql, role_log},
        {"src/module/log/log.erl", log, item_produce_log},
        {"src/module/log/log_sql.erl", sql, item_produce_log},
        {"src/module/log/log.erl", log, item_consume_log},
        {"src/module/log/log_sql.erl", sql, item_consume_log},
        {"src/module/log/log.erl", log, shop_log},
        {"src/module/log/log_sql.erl", sql, shop_log},
        {"src/module/log/log.erl", log, quest_log},
        {"src/module/log/log_sql.erl", sql, quest_log},
        {"src/module/log/log.erl", log, auction_log},
        {"src/module/log/log_sql.erl", sql, auction_log}
    ].

%%%------------------------------------------------------------------
%%% @doc
%%% module log script
%%% @end
%%%------------------------------------------------------------------
-module(log_script).
-export([main/1]).
%%%==================================================================
%%% API functions
%%%==================================================================
main([Key]) ->
    code:add_path(filename:dirname(escript:script_name()) ++ "/../../../beam/"),
    Log = [X || X <- log(), atom_to_list(element(3, X)) == Key orelse atom_to_list(element(3, X)) == Key ++ "_log"],
    Name = string:join(string:replace(Key, "_log", "", trailing), "") ++ "_log",
    List = tool:default(Log, [{"src/module/log/log.erl", log, Name}, {"src/module/log/log_sql.erl", sql, Name}, {"src/module/log/log_sql_clean.erl", clean, Name}]),
    io:format("~p~n", [catch log_maker:start(List)]);
main(_) ->
    io:format("invalid argument~n").

%%%==================================================================
%%% log data
%%%==================================================================
log() ->
    [
        {"src/module/log/log.erl", log, online_log},
        {"src/module/log/log_sql.erl", sql, online_log},
        {"src/module/log/log.erl", log, role_log},
        {"src/module/log/log_sql.erl", sql, role_log},
        {"src/module/log/log_sql_clean.erl", clean, role_log},
        {"src/module/log/log.erl", log, item_produce_log},
        {"src/module/log/log_sql.erl", sql, item_produce_log},
        {"src/module/log/log_sql_clean.erl", clean, item_produce_log},
        {"src/module/log/log.erl", log, item_consume_log},
        {"src/module/log/log_sql.erl", sql, item_consume_log},
        {"src/module/log/log_sql_clean.erl", clean, item_consume_log},
        {"src/module/log/log.erl", log, shop_log},
        {"src/module/log/log_sql.erl", sql, shop_log},
        {"src/module/log/log_sql_clean.erl", clean, shop_log},
        {"src/module/log/log.erl", log, quest_log},
        {"src/module/log/log_sql.erl", sql, quest_log},
        {"src/module/log/log_sql_clean.erl", clean, quest_log},
        {"src/module/log/log.erl", log, auction_log},
        {"src/module/log/log_sql.erl", sql, auction_log},
        {"src/module/log/log_sql_clean.erl", clean, auction_log}
    ].

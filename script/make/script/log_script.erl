%%%-------------------------------------------------------------------
%%% @doc
%%% module log script
%%% @end
%%%-------------------------------------------------------------------
-module(log_script).
-export([main/1]).
%%%===================================================================
%%% API functions
%%%===================================================================
main(Keys) ->
    code:add_path(filename:dirname(escript:script_name()) ++ "/../../../beam/"),
    Log = [X || X <- log(), lists:member(atom_to_list(element(3, X)), Keys) orelse lists:member(atom_to_list(element(3, X)) ++ "_log", Keys)],
    List = tool:default(Log, [begin Name = string:join(string:replace(Key, "_log", "", trailing), "") ++ "_log", {"src/module/log/log.erl", log, Name}, {"src/module/log/log_sql.erl", sql, Name}, {"src/module/log/log_sql_clean.erl", clean, Name} end || Key <- Keys]),
    io:format("~p~n", [catch log_maker:start(List)]);
main(_) ->
    io:format("invalid argument~n").

%%%===================================================================
%%% log data
%%%===================================================================
log() ->
    [
        {"src/module/log/log.erl", log, online_log},
        {"src/module/log/log_sql.erl", sql, online_log},
        {"src/module/log/log_sql_clean.erl", clean, online_log},
        {"src/module/log/log.erl", log, total_login_log},
        {"src/module/log/log_sql.erl", sql, total_login_log},
        {"src/module/log/log_sql_clean.erl", clean, total_login_log},
        {"src/module/log/log.erl", log, login_log},
        {"src/module/log/log_sql.erl", sql, login_log},
        {"src/module/log/log_sql_clean.erl", clean, login_log},
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
        {"src/module/log/log.erl", log, title_log},
        {"src/module/log/log_sql.erl", sql, title_log},
        {"src/module/log/log_sql_clean.erl", clean, title_log},
        {"src/module/log/log.erl", log, auction_log},
        {"src/module/log/log_sql.erl", sql, auction_log},
        {"src/module/log/log_sql_clean.erl", clean, auction_log}
    ].

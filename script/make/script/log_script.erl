%%%-------------------------------------------------------------------
%%% @doc
%%% log script for log maker
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
    List = tool:default(Log, lists:flatten([begin Name = list_to_atom(string:join(string:replace(Key, "_log", "", trailing), "") ++ "_log"), [{"src/module/log/log.erl", log, Name}, {"src/module/log/log_sql_save.erl", save, Name}, {"src/module/log/log_sql_clean.erl", clean, Name}, {"src/module/log/log_sql_retain.erl", retain, Name}] end || Key <- Keys])),
    io:format("~p~n", [catch log_maker:start(List)]);
main(_) ->
    io:format("invalid argument~n").

%%%===================================================================
%%% log data
%%%===================================================================
log() ->
    [
        {"src/module/log/log.erl", log, online_log},
        {"src/module/log/log_sql_save.erl", save, online_log},
        {"src/module/log/log_sql_clean.erl", clean, online_log},
        {"src/module/log/log_sql_retain.erl", retain, online_log},

        {"src/module/log/log.erl", log, total_login_log},
        {"src/module/log/log_sql_save.erl", save, total_login_log},
        {"src/module/log/log_sql_clean.erl", clean, total_login_log},
        {"src/module/log/log_sql_retain.erl", retain, total_login_log},

        {"src/module/log/log.erl", log, login_log},
        {"src/module/log/log_sql_save.erl", save, login_log},
        {"src/module/log/log_sql_clean.erl", clean, login_log},
        {"src/module/log/log_sql_retain.erl", retain, login_log},

        {"src/module/log/log.erl", log, role_log},
        {"src/module/log/log_sql_save.erl", save, role_log},
        {"src/module/log/log_sql_clean.erl", clean, role_log},
        {"src/module/log/log_sql_retain.erl", retain, role_log},

        {"src/module/log/log.erl", log, item_produce_log},
        {"src/module/log/log_sql_save.erl", save, item_produce_log},
        {"src/module/log/log_sql_clean.erl", clean, item_produce_log},
        {"src/module/log/log_sql_retain.erl", retain, item_produce_log},

        {"src/module/log/log.erl", log, item_consume_log},
        {"src/module/log/log_sql_save.erl", save, item_consume_log},
        {"src/module/log/log_sql_clean.erl", clean, item_consume_log},
        {"src/module/log/log_sql_retain.erl", retain, item_consume_log},

        {"src/module/log/log.erl", log, shop_log},
        {"src/module/log/log_sql_save.erl", save, shop_log},
        {"src/module/log/log_sql_clean.erl", clean, shop_log},
        {"src/module/log/log_sql_retain.erl", retain, shop_log},

        {"src/module/log/log.erl", log, quest_log},
        {"src/module/log/log_sql_save.erl", save, quest_log},
        {"src/module/log/log_sql_clean.erl", clean, quest_log},
        {"src/module/log/log_sql_retain.erl", retain, quest_log},

        {"src/module/log/log.erl", log, title_log},
        {"src/module/log/log_sql_save.erl", save, title_log},
        {"src/module/log/log_sql_clean.erl", clean, title_log},
        {"src/module/log/log_sql_retain.erl", retain, title_log},

        {"src/module/log/log.erl", log, auction_log},
        {"src/module/log/log_sql_save.erl", save, auction_log},
        {"src/module/log/log_sql_clean.erl", clean, auction_log},
        {"src/module/log/log_sql_retain.erl", retain, auction_log}
    ].

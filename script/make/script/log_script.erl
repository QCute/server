%%%-------------------------------------------------------------------
%%! +pc unicode
%%% @doc
%%% log script for log maker
%%% @end
%%%-------------------------------------------------------------------
-module(log_script).
-export([main/1]).
-include("../../../include/journal.hrl").
%%%===================================================================
%%% API functions
%%%===================================================================
main(Keys) ->
    io:setopts([{encoding, unicode}]),
    io:setopts(standard_error, [{encoding, unicode}]),
    code:add_path(filename:dirname(escript:script_name()) ++ "/../../../beam/"),
    Log = [X || X <- log(), lists:member(atom_to_list(element(3, X)), Keys) orelse lists:member(atom_to_list(element(3, X)) ++ "_log", Keys)],
    Default = lists:flatten([begin Name = list_to_atom(string:join(string:replace(Key, "_log", "", trailing), "") ++ "_log"), [{"src/module/log/log.erl", log, Name}, {"src/module/log/log_sql_save.erl", save, Name}, {"src/module/log/log_sql_clean.erl", clean, Name}, {"src/module/log/log_sql_retain.erl", retain, Name}] end || Key <- Keys]),
    List = proplists:get_value(Log, [{[], Default}], Log),
    try
        io:format("~tp~n", [log_maker:start(List)])
    catch ?EXCEPTION(Class, Reason, Stacktrace) ->
        ?HALT(Class, Reason, Stacktrace)
    end.

%%%===================================================================
%%% log data
%%%===================================================================
log() ->
    [
        {"src/module/log/log.erl", log, online_log},
        {"src/module/log/log_sql_save.erl", save, online_log},
        {"src/module/log/log_sql_clean.erl", clean, online_log},
        {"src/module/log/log_sql_retain.erl", retain, online_log},

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

        {"src/module/log/log.erl", log, task_log},
        {"src/module/log/log_sql_save.erl", save, task_log},
        {"src/module/log/log_sql_clean.erl", clean, task_log},
        {"src/module/log/log_sql_retain.erl", retain, task_log},

        {"src/module/log/log.erl", log, achievement_log},
        {"src/module/log/log_sql_save.erl", save, achievement_log},
        {"src/module/log/log_sql_clean.erl", clean, achievement_log},
        {"src/module/log/log_sql_retain.erl", retain, achievement_log},

        {"src/module/log/log.erl", log, fashion_log},
        {"src/module/log/log_sql_save.erl", save, fashion_log},
        {"src/module/log/log_sql_clean.erl", clean, fashion_log},
        {"src/module/log/log_sql_retain.erl", retain, fashion_log},

        {"src/module/log/log.erl", log, title_log},
        {"src/module/log/log_sql_save.erl", save, title_log},
        {"src/module/log/log_sql_clean.erl", clean, title_log},
        {"src/module/log/log_sql_retain.erl", retain, title_log},

        {"src/module/log/log.erl", log, bubble_log},
        {"src/module/log/log_sql_save.erl", save, bubble_log},
        {"src/module/log/log_sql_clean.erl", clean, bubble_log},
        {"src/module/log/log_sql_retain.erl", retain, bubble_log},

        {"src/module/log/log.erl", log, auction_log},
        {"src/module/log/log_sql_save.erl", save, auction_log},
        {"src/module/log/log_sql_clean.erl", clean, auction_log},
        {"src/module/log/log_sql_retain.erl", retain, auction_log}
    ].

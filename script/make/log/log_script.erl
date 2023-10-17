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
    Log = [X || X <- log(), lists:member(atom_to_list(maps:get(table, X)), Keys)],
    try
        io:format("~tp~n", [log_maker:start(Log)])
    catch ?EXCEPTION(Class, Reason, Stacktrace) ->
        ?HALT(Class, Reason, Stacktrace)
    end.

%%%===================================================================
%%% log data
%%%===================================================================
log() ->
    [
        #{file => "src/module/log/log.erl", type => log, table => online_log},
        #{file => "src/module/log/log_sql_save.erl", type => save, table => online_log},
        #{file => "src/module/log/log_sql_clean.erl", type => clean, table => online_log},
        #{file => "src/module/log/log_sql_retain.erl", type => retain, table => online_log},
        
        #{file => "src/module/log/log.erl", type => log, table => total_online_log},
        #{file => "src/module/log/log_sql_save.erl", type => save, table => total_online_log},
        #{file => "src/module/log/log_sql_clean.erl", type => clean, table => total_online_log},
        #{file => "src/module/log/log_sql_retain.erl", type => retain, table => total_online_log},
        
        #{file => "src/module/log/log.erl", type => log, table => login_log},
        #{file => "src/module/log/log_sql_save.erl", type => save, table => login_log},
        #{file => "src/module/log/log_sql_clean.erl", type => clean, table => login_log},
        #{file => "src/module/log/log_sql_retain.erl", type => retain, table => login_log},
        
        #{file => "src/module/log/log.erl", type => log, table => total_login_log},
        #{file => "src/module/log/log_sql_save.erl", type => save, table => total_login_log},
        #{file => "src/module/log/log_sql_clean.erl", type => clean, table => total_login_log},
        #{file => "src/module/log/log_sql_retain.erl", type => retain, table => total_login_log},
        
        #{file => "src/module/log/log.erl", type => log, table => role_log},
        #{file => "src/module/log/log_sql_save.erl", type => save, table => role_log},
        #{file => "src/module/log/log_sql_clean.erl", type => clean, table => role_log},
        #{file => "src/module/log/log_sql_retain.erl", type => retain, table => role_log},
        
        #{file => "src/module/log/log.erl", type => log, table => asset_produce_log},
        #{file => "src/module/log/log_sql_save.erl", type => save, table => asset_produce_log},
        #{file => "src/module/log/log_sql_clean.erl", type => clean, table => asset_produce_log},
        #{file => "src/module/log/log_sql_retain.erl", type => retain, table => asset_produce_log},
        
        #{file => "src/module/log/log.erl", type => log, table => asset_consume_log},
        #{file => "src/module/log/log_sql_save.erl", type => save, table => asset_consume_log},
        #{file => "src/module/log/log_sql_clean.erl", type => clean, table => asset_consume_log},
        #{file => "src/module/log/log_sql_retain.erl", type => retain, table => asset_consume_log},
        
        #{file => "src/module/log/log.erl", type => log, table => item_produce_log},
        #{file => "src/module/log/log_sql_save.erl", type => save, table => item_produce_log},
        #{file => "src/module/log/log_sql_clean.erl", type => clean, table => item_produce_log},
        #{file => "src/module/log/log_sql_retain.erl", type => retain, table => item_produce_log},
        
        #{file => "src/module/log/log.erl", type => log, table => item_consume_log},
        #{file => "src/module/log/log_sql_save.erl", type => save, table => item_consume_log},
        #{file => "src/module/log/log_sql_clean.erl", type => clean, table => item_consume_log},
        #{file => "src/module/log/log_sql_retain.erl", type => retain, table => item_consume_log},
        
        #{file => "src/module/log/log.erl", type => log, table => shop_log},
        #{file => "src/module/log/log_sql_save.erl", type => save, table => shop_log},
        #{file => "src/module/log/log_sql_clean.erl", type => clean, table => shop_log},
        #{file => "src/module/log/log_sql_retain.erl", type => retain, table => shop_log},
        
        #{file => "src/module/log/log.erl", type => log, table => task_log},
        #{file => "src/module/log/log_sql_save.erl", type => save, table => task_log},
        #{file => "src/module/log/log_sql_clean.erl", type => clean, table => task_log},
        #{file => "src/module/log/log_sql_retain.erl", type => retain, table => task_log},
        
        #{file => "src/module/log/log.erl", type => log, table => achievement_log},
        #{file => "src/module/log/log_sql_save.erl", type => save, table => achievement_log},
        #{file => "src/module/log/log_sql_clean.erl", type => clean, table => achievement_log},
        #{file => "src/module/log/log_sql_retain.erl", type => retain, table => achievement_log},
        
        #{file => "src/module/log/log.erl", type => log, table => fashion_log},
        #{file => "src/module/log/log_sql_save.erl", type => save, table => fashion_log},
        #{file => "src/module/log/log_sql_clean.erl", type => clean, table => fashion_log},
        #{file => "src/module/log/log_sql_retain.erl", type => retain, table => fashion_log},
        
        #{file => "src/module/log/log.erl", type => log, table => title_log},
        #{file => "src/module/log/log_sql_save.erl", type => save, table => title_log},
        #{file => "src/module/log/log_sql_clean.erl", type => clean, table => title_log},
        #{file => "src/module/log/log_sql_retain.erl", type => retain, table => title_log},
        
        #{file => "src/module/log/log.erl", type => log, table => bubble_log},
        #{file => "src/module/log/log_sql_save.erl", type => save, table => bubble_log},
        #{file => "src/module/log/log_sql_clean.erl", type => clean, table => bubble_log},
        #{file => "src/module/log/log_sql_retain.erl", type => retain, table => bubble_log},
        
        #{file => "src/module/log/log.erl", type => log, table => auction_log},
        #{file => "src/module/log/log_sql_save.erl", type => save, table => auction_log},
        #{file => "src/module/log/log_sql_clean.erl", type => clean, table => auction_log},
        #{file => "src/module/log/log_sql_retain.erl", type => retain, table => auction_log}
    ].

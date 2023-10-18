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
main(_) ->
    io:setopts([{encoding, unicode}]),
    io:setopts(standard_error, [{encoding, unicode}]),
    code:add_path(filename:dirname(escript:script_name()) ++ "/../../../beam/"),
    Log = [#{file => F, meta => [X || X = #{file := File} <- log(), File == F]} || F <- sets:to_list(sets:from_list([maps:get(file, X) || X <- log()]))],
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
        #{file => "src/module/log/log.erl", table => online_log},
        #{file => "src/module/log/log_sql_save.erl", table => online_log},
        #{file => "src/module/log/log_sql_clean.erl", table => online_log},
        #{file => "src/module/log/log_sql_retain.erl", table => online_log},
        
        #{file => "src/module/log/log.erl", table => total_online_log},
        #{file => "src/module/log/log_sql_save.erl", table => total_online_log},
        #{file => "src/module/log/log_sql_clean.erl", table => total_online_log},
        #{file => "src/module/log/log_sql_retain.erl", table => total_online_log},
        
        #{file => "src/module/log/log.erl", table => login_log},
        #{file => "src/module/log/log_sql_save.erl", table => login_log},
        #{file => "src/module/log/log_sql_clean.erl", table => login_log},
        #{file => "src/module/log/log_sql_retain.erl", table => login_log},
        
        #{file => "src/module/log/log.erl", table => total_login_log},
        #{file => "src/module/log/log_sql_save.erl", table => total_login_log},
        #{file => "src/module/log/log_sql_clean.erl", table => total_login_log},
        #{file => "src/module/log/log_sql_retain.erl", table => total_login_log},
        
        #{file => "src/module/log/log.erl", table => role_log},
        #{file => "src/module/log/log_sql_save.erl", table => role_log},
        #{file => "src/module/log/log_sql_clean.erl", table => role_log},
        #{file => "src/module/log/log_sql_retain.erl", table => role_log},
        
        #{file => "src/module/log/log.erl", table => asset_produce_log},
        #{file => "src/module/log/log_sql_save.erl", table => asset_produce_log},
        #{file => "src/module/log/log_sql_clean.erl", table => asset_produce_log},
        #{file => "src/module/log/log_sql_retain.erl", table => asset_produce_log},
        
        #{file => "src/module/log/log.erl", table => asset_consume_log},
        #{file => "src/module/log/log_sql_save.erl", table => asset_consume_log},
        #{file => "src/module/log/log_sql_clean.erl", table => asset_consume_log},
        #{file => "src/module/log/log_sql_retain.erl", table => asset_consume_log},
        
        #{file => "src/module/log/log.erl", table => item_produce_log},
        #{file => "src/module/log/log_sql_save.erl", table => item_produce_log},
        #{file => "src/module/log/log_sql_clean.erl", table => item_produce_log},
        #{file => "src/module/log/log_sql_retain.erl", table => item_produce_log},
        
        #{file => "src/module/log/log.erl", table => item_consume_log},
        #{file => "src/module/log/log_sql_save.erl", table => item_consume_log},
        #{file => "src/module/log/log_sql_clean.erl", table => item_consume_log},
        #{file => "src/module/log/log_sql_retain.erl", table => item_consume_log},
        
        #{file => "src/module/log/log.erl", table => shop_log},
        #{file => "src/module/log/log_sql_save.erl", table => shop_log},
        #{file => "src/module/log/log_sql_clean.erl", table => shop_log},
        #{file => "src/module/log/log_sql_retain.erl", table => shop_log},
        
        #{file => "src/module/log/log.erl", table => task_log},
        #{file => "src/module/log/log_sql_save.erl", table => task_log},
        #{file => "src/module/log/log_sql_clean.erl", table => task_log},
        #{file => "src/module/log/log_sql_retain.erl", table => task_log},
        
        #{file => "src/module/log/log.erl", table => achievement_log},
        #{file => "src/module/log/log_sql_save.erl", table => achievement_log},
        #{file => "src/module/log/log_sql_clean.erl", table => achievement_log},
        #{file => "src/module/log/log_sql_retain.erl", table => achievement_log},
        
        #{file => "src/module/log/log.erl", table => fashion_log},
        #{file => "src/module/log/log_sql_save.erl", table => fashion_log},
        #{file => "src/module/log/log_sql_clean.erl", table => fashion_log},
        #{file => "src/module/log/log_sql_retain.erl", table => fashion_log},
        
        #{file => "src/module/log/log.erl", table => title_log},
        #{file => "src/module/log/log_sql_save.erl", table => title_log},
        #{file => "src/module/log/log_sql_clean.erl", table => title_log},
        #{file => "src/module/log/log_sql_retain.erl", table => title_log},
        
        #{file => "src/module/log/log.erl", table => bubble_log},
        #{file => "src/module/log/log_sql_save.erl", table => bubble_log},
        #{file => "src/module/log/log_sql_clean.erl", table => bubble_log},
        #{file => "src/module/log/log_sql_retain.erl", table => bubble_log},
        
        #{file => "src/module/log/log.erl", table => auction_log},
        #{file => "src/module/log/log_sql_save.erl", table => auction_log},
        #{file => "src/module/log/log_sql_clean.erl", table => auction_log},
        #{file => "src/module/log/log_sql_retain.erl", table => auction_log}
    ].

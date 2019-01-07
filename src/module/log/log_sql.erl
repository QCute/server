%%%-------------------------------------------------------------------
%%% @doc
%%% module log
%%% @end
%%%-------------------------------------------------------------------
-module(log_sql).
-compile(nowarn_export_all).
-compile(export_all).
%%%===================================================================
%%% API
%%%===================================================================
sql(log_player) ->
    {"INSERT INTO `log_player` (`user_id`, `exp`, `time`) VALUES ", "('~w', '~w', '~w')"};
sql(log_rank) ->
    {"INSERT INTO `log_rank` (`rank_type`, `role_id`, `role_name`, `pos`, `count_num`, `count_time`, `time`) VALUES ", "('~w', '~w', '~w', '~w', '~w', '~w', '~w')"};
sql(_) ->
    ok.


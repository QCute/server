%%%-------------------------------------------------------------------
%%% @doc
%%% module log sql
%%% @end
%%%-------------------------------------------------------------------
-module(log_sql).
-compile(nowarn_export_all).
-compile(export_all).
%%%===================================================================
%%% API
%%%===================================================================
sql(role_log) ->
    {"INSERT INTO `role_log` (`role_id`, `exp`, `time`, `daily_time`) VALUES ", "('~w', '~w', '~w', '~w')"};
sql(item_log) ->
    {"INSERT INTO `item_log` (`role_id`, `data_id`, `item_id`, `operation`, `source`, `time`, `daily_time`) VALUES ", "('~w', '~w', '~w', '~w', '~w', '~w', '~w')"};
sql(shop_log) ->
    {"INSERT INTO `shop_log` (`role_id`, `shop_id`, `amount`, `time`, `daily_time`) VALUES ", "('~w', '~w', '~w', '~w', '~w')"};
sql(quest_log) ->
    {"INSERT INTO `quest_log` (`role_id`, `quest_id`, `time`, `daily_time`) VALUES ", "('~w', '~w', '~w', '~w')"};
sql(_) ->
    ok.


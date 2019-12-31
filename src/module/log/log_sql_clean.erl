%%%------------------------------------------------------------------
%%% @doc
%%% module log clean sql
%%% @end
%%%------------------------------------------------------------------
-module(log_sql_clean).
-compile(nowarn_export_all).
-compile(export_all).
%%%==================================================================
%%% API functions
%%%==================================================================
sql() ->
    [
        {<<"DELETE FROM `role_log` WHERE `daily_time` <= ~w">>, 2592000},
        {<<"DELETE FROM `item_produce_log` WHERE `daily_time` <= ~w">>, 2592000},
        {<<"DELETE FROM `item_consume_log` WHERE `daily_time` <= ~w">>, 2592000},
        {<<"DELETE FROM `shop_log` WHERE `daily_time` <= ~w">>, 2592000},
        {<<"DELETE FROM `quest_log` WHERE `daily_time` <= ~w">>, 2592000},
        {<<"DELETE FROM `auction_log` WHERE `daily_time` <= ~w">>, 2592000}
    ].

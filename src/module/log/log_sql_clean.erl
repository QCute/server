%%%-------------------------------------------------------------------
%%% @doc
%%% log sql clean
%%% @end
%%%-------------------------------------------------------------------
-module(log_sql_clean).
-compile(nowarn_export_all).
-compile(export_all).
%%%===================================================================
%%% API functions
%%%===================================================================
sql() ->
    [
        {<<"DELETE FROM `online_log` WHERE `time` < ~w LIMIT 1000">>, 2592000},
        {<<"DELETE FROM `login_log` WHERE `time` < ~w LIMIT 1000">>, 2592000},
        {<<"DELETE FROM `role_log` WHERE `time` < ~w LIMIT 1000">>, 2592000},
        {<<"DELETE FROM `item_produce_log` WHERE `time` < ~w LIMIT 1000">>, 2592000},
        {<<"DELETE FROM `item_consume_log` WHERE `time` < ~w LIMIT 1000">>, 2592000},
        {<<"DELETE FROM `shop_log` WHERE `time` < ~w LIMIT 1000">>, 2592000},
        {<<"DELETE FROM `quest_log` WHERE `time` < ~w LIMIT 1000">>, 2592000},
        {<<"DELETE FROM `title_log` WHERE `time` < ~w LIMIT 1000">>, 2592000},
        {<<"DELETE FROM `auction_log` WHERE `time` < ~w LIMIT 1000">>, 2592000}
    ].

%%%-------------------------------------------------------------------
%%% @doc
%%% log sql clean
%%% @end
%%%-------------------------------------------------------------------
-module(log_sql_clean).
-export([sql/0]).
%%%===================================================================
%%% API functions
%%%===================================================================
%% @doc the log clean sql
-spec sql() -> [{DeleteSql :: binary(), Time :: non_neg_integer()}].
sql() ->
    [
        {<<"DELETE FROM `online_log` WHERE `time` < ~w LIMIT 1000">>, 259200},
        {<<"DELETE FROM `total_online_log` WHERE `time` < ~w LIMIT 1000">>, 259200},
        {<<"DELETE FROM `login_log` WHERE `time` < ~w LIMIT 1000">>, 259200},
        {<<"DELETE FROM `total_login_log` WHERE `time` < ~w LIMIT 1000">>, 259200},
        {<<"DELETE FROM `role_log` WHERE `time` < ~w LIMIT 1000">>, 259200},
        {<<"DELETE FROM `asset_produce_log` WHERE `time` < ~w LIMIT 1000">>, 259200},
        {<<"DELETE FROM `asset_consume_log` WHERE `time` < ~w LIMIT 1000">>, 259200},
        {<<"DELETE FROM `item_produce_log` WHERE `time` < ~w LIMIT 1000">>, 259200},
        {<<"DELETE FROM `item_consume_log` WHERE `time` < ~w LIMIT 1000">>, 259200},
        {<<"DELETE FROM `shop_log` WHERE `time` < ~w LIMIT 1000">>, 259200},
        {<<"DELETE FROM `task_log` WHERE `time` < ~w LIMIT 1000">>, 259200},
        {<<"DELETE FROM `achievement_log` WHERE `time` < ~w LIMIT 1000">>, 259200},
        {<<"DELETE FROM `fashion_log` WHERE `time` < ~w LIMIT 1000">>, 259200},
        {<<"DELETE FROM `title_log` WHERE `time` < ~w LIMIT 1000">>, 259200},
        {<<"DELETE FROM `bubble_log` WHERE `time` < ~w LIMIT 1000">>, 259200},
        {<<"DELETE FROM `auction_log` WHERE `time` < ~w LIMIT 1000">>, 259200}
    ].

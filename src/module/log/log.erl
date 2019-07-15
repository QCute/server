%%%-------------------------------------------------------------------
%%% @doc
%%% module log
%%% @end
%%%-------------------------------------------------------------------
-module(log).
-compile(nowarn_export_all).
-compile(export_all).
%%%===================================================================
%%% API
%%%===================================================================
role_log(RoleId, Exp, Time) ->
    log_server:log(role_log, [RoleId, Exp, Time, time:zero(Time)]).

item_log(RoleId, ItemId, Operation, Source, Time) ->
    log_server:log(item_log, [RoleId, ItemId, Operation, Source, Time, time:zero(Time)]).

shop_log(RoleId, ShopId, Number, Time) ->
    log_server:log(shop_log, [RoleId, ShopId, Number, Time, time:zero(Time)]).

quest_log(RoleId, QuestId, Time) ->
    log_server:log(quest_log, [RoleId, QuestId, Time, time:zero(Time)]).


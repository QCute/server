%%%-------------------------------------------------------------------
%%% @doc
%%% module log
%%% @end
%%%-------------------------------------------------------------------
-module(log).
-compile(nowarn_export_all).
-compile(export_all).
%%%===================================================================
%%% API functions
%%%===================================================================
online_log(All, Online, Hosting, Hour, Time) ->
    log_server:log(online_log, [All, Online, Hosting, Hour, Time]).

login_log(RoleId, Ip, DeviceId, LoginTime, OnlineTime, Time) ->
    log_server:log(login_log, [RoleId, Ip, DeviceId, LoginTime, OnlineTime, Time]).

role_log(RoleId, Exp, Time) ->
    log_server:log(role_log, [RoleId, Exp, Time]).

item_produce_log(RoleId, ItemId, Operation, Source, Time) ->
    log_server:log(item_produce_log, [RoleId, ItemId, Operation, Source, Time]).

item_consume_log(RoleId, ItemId, Operation, Source, Time) ->
    log_server:log(item_consume_log, [RoleId, ItemId, Operation, Source, Time]).

shop_log(RoleId, ShopId, Number, Time) ->
    log_server:log(shop_log, [RoleId, ShopId, Number, Time]).

quest_log(RoleId, QuestId, Time) ->
    log_server:log(quest_log, [RoleId, QuestId, Time]).

auction_log(AuctionId, Number, BidNumber, Price, RoleId, RoleName, ServerId, Time) ->
    log_server:log(auction_log, [AuctionId, Number, BidNumber, Price, RoleId, RoleName, ServerId, Time]).

title_log(RoleId, TitleId, From, Time) ->
    log_server:log(title_log, [RoleId, TitleId, From, Time]).


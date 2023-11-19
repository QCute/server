%%%-------------------------------------------------------------------
%%% @doc
%%% log
%%% @end
%%%-------------------------------------------------------------------
-module(log).
-export([achievement_log/3]).
-export([asset_consume_log/7]).
-export([asset_produce_log/7]).
-export([auction_log/8]).
-export([bubble_log/4]).
-export([fashion_log/4]).
-export([item_consume_log/5]).
-export([item_produce_log/5]).
-export([login_log/7]).
-export([online_log/5]).
-export([role_log/3]).
-export([shop_log/4]).
-export([task_log/3]).
-export([title_log/4]).
-export([total_login_log/2]).
-export([total_online_log/3]).
%%%===================================================================
%%% API functions
%%%===================================================================
-spec achievement_log(RoleId :: non_neg_integer(), AchievementId :: non_neg_integer(), Time :: non_neg_integer()) -> ok.
achievement_log(RoleId, AchievementId, Time) ->
    log_server:log(achievement_log, {RoleId, AchievementId, Time}).

-spec asset_consume_log(RoleId :: non_neg_integer(), Asset :: term(), AssetId :: non_neg_integer(), Number :: non_neg_integer(), Operation :: term(), From :: term(), Time :: non_neg_integer()) -> ok.
asset_consume_log(RoleId, Asset, AssetId, Number, Operation, From, Time) ->
    log_server:log(asset_consume_log, {RoleId, Asset, AssetId, Number, Operation, From, Time}).

-spec asset_produce_log(RoleId :: non_neg_integer(), Asset :: term(), AssetId :: non_neg_integer(), Number :: non_neg_integer(), Operation :: term(), From :: term(), Time :: non_neg_integer()) -> ok.
asset_produce_log(RoleId, Asset, AssetId, Number, Operation, From, Time) ->
    log_server:log(asset_produce_log, {RoleId, Asset, AssetId, Number, Operation, From, Time}).

-spec auction_log(AuctionId :: non_neg_integer(), Number :: non_neg_integer(), BidNumber :: non_neg_integer(), Price :: non_neg_integer(), RoleId :: non_neg_integer(), RoleName :: binary(), ServerId :: non_neg_integer(), Time :: non_neg_integer()) -> ok.
auction_log(AuctionId, Number, BidNumber, Price, RoleId, RoleName, ServerId, Time) ->
    log_server:log(auction_log, {AuctionId, Number, BidNumber, Price, RoleId, RoleName, ServerId, Time}).

-spec bubble_log(RoleId :: non_neg_integer(), BubbleId :: non_neg_integer(), From :: term(), Time :: non_neg_integer()) -> ok.
bubble_log(RoleId, BubbleId, From, Time) ->
    log_server:log(bubble_log, {RoleId, BubbleId, From, Time}).

-spec fashion_log(RoleId :: non_neg_integer(), FashionId :: non_neg_integer(), From :: term(), Time :: non_neg_integer()) -> ok.
fashion_log(RoleId, FashionId, From, Time) ->
    log_server:log(fashion_log, {RoleId, FashionId, From, Time}).

-spec item_consume_log(RoleId :: non_neg_integer(), ItemId :: non_neg_integer(), Operation :: term(), From :: term(), Time :: non_neg_integer()) -> ok.
item_consume_log(RoleId, ItemId, Operation, From, Time) ->
    log_server:log(item_consume_log, {RoleId, ItemId, Operation, From, Time}).

-spec item_produce_log(RoleId :: non_neg_integer(), ItemId :: non_neg_integer(), Operation :: term(), From :: term(), Time :: non_neg_integer()) -> ok.
item_produce_log(RoleId, ItemId, Operation, From, Time) ->
    log_server:log(item_produce_log, {RoleId, ItemId, Operation, From, Time}).

-spec login_log(RoleId :: non_neg_integer(), Ip :: binary(), DeviceId :: binary(), LoginTime :: non_neg_integer(), OnlineTime :: non_neg_integer(), LogoutTime :: non_neg_integer(), Time :: non_neg_integer()) -> ok.
login_log(RoleId, Ip, DeviceId, LoginTime, OnlineTime, LogoutTime, Time) ->
    log_server:log(login_log, {RoleId, Ip, DeviceId, LoginTime, OnlineTime, LogoutTime, Time}).

-spec online_log(Total :: non_neg_integer(), Online :: non_neg_integer(), Hosting :: non_neg_integer(), Hour :: non_neg_integer(), Time :: non_neg_integer()) -> ok.
online_log(Total, Online, Hosting, Hour, Time) ->
    log_server:log(online_log, {Total, Online, Hosting, Hour, Time}).

-spec role_log(RoleId :: non_neg_integer(), Exp :: non_neg_integer(), Time :: non_neg_integer()) -> ok.
role_log(RoleId, Exp, Time) ->
    log_server:log(role_log, {RoleId, Exp, Time}).

-spec shop_log(RoleId :: non_neg_integer(), ShopId :: non_neg_integer(), Number :: non_neg_integer(), Time :: non_neg_integer()) -> ok.
shop_log(RoleId, ShopId, Number, Time) ->
    log_server:log(shop_log, {RoleId, ShopId, Number, Time}).

-spec task_log(RoleId :: non_neg_integer(), TaskId :: non_neg_integer(), Time :: non_neg_integer()) -> ok.
task_log(RoleId, TaskId, Time) ->
    log_server:log(task_log, {RoleId, TaskId, Time}).

-spec title_log(RoleId :: non_neg_integer(), TitleId :: non_neg_integer(), From :: term(), Time :: non_neg_integer()) -> ok.
title_log(RoleId, TitleId, From, Time) ->
    log_server:log(title_log, {RoleId, TitleId, From, Time}).

-spec total_login_log(Number :: non_neg_integer(), Time :: non_neg_integer()) -> ok.
total_login_log(Number, Time) ->
    log_server:log(total_login_log, {Number, Time}).

-spec total_online_log(RoleId :: non_neg_integer(), OnlineTime :: non_neg_integer(), Time :: non_neg_integer()) -> ok.
total_online_log(RoleId, OnlineTime, Time) ->
    log_server:log(total_online_log, {RoleId, OnlineTime, Time}).


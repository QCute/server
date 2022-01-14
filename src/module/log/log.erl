%%%-------------------------------------------------------------------
%%% @doc
%%% log
%%% @end
%%%-------------------------------------------------------------------
-module(log).
-export([online_log/5]).
-export([login_log/7]).
-export([role_log/3]).
-export([item_produce_log/5]).
-export([item_consume_log/5]).
-export([shop_log/4]).
-export([task_log/3]).
-export([achievement_log/3]).
-export([fashion_log/4]).
-export([title_log/4]).
-export([bubble_log/4]).
-export([auction_log/8]).

%%%===================================================================
%%% API functions
%%%===================================================================
-spec online_log(Total :: integer(), Online :: integer(), Hosting :: integer(), Hour :: integer(), Time :: integer()) -> ok.
online_log(Total, Online, Hosting, Hour, Time) ->
    log_server:log(online_log, [Total, Online, Hosting, Hour, Time]).

-spec login_log(RoleId :: integer(), Ip :: binary(), DeviceId :: binary(), LoginTime :: integer(), OnlineTime :: integer(), LogoutTime :: integer(), Time :: integer()) -> ok.
login_log(RoleId, Ip, DeviceId, LoginTime, OnlineTime, LogoutTime, Time) ->
    log_server:log(login_log, [RoleId, Ip, DeviceId, LoginTime, OnlineTime, LogoutTime, Time]).

-spec role_log(RoleId :: integer(), Exp :: integer(), Time :: integer()) -> ok.
role_log(RoleId, Exp, Time) ->
    log_server:log(role_log, [RoleId, Exp, Time]).

-spec item_produce_log(RoleId :: integer(), ItemId :: integer(), Operation :: term(), From :: term(), Time :: integer()) -> ok.
item_produce_log(RoleId, ItemId, Operation, From, Time) ->
    log_server:log(item_produce_log, [RoleId, ItemId, Operation, From, Time]).

-spec item_consume_log(RoleId :: integer(), ItemId :: integer(), Operation :: term(), From :: term(), Time :: integer()) -> ok.
item_consume_log(RoleId, ItemId, Operation, From, Time) ->
    log_server:log(item_consume_log, [RoleId, ItemId, Operation, From, Time]).

-spec shop_log(RoleId :: integer(), ShopId :: integer(), Number :: integer(), Time :: integer()) -> ok.
shop_log(RoleId, ShopId, Number, Time) ->
    log_server:log(shop_log, [RoleId, ShopId, Number, Time]).

-spec task_log(RoleId :: integer(), TaskId :: integer(), Time :: integer()) -> ok.
task_log(RoleId, TaskId, Time) ->
    log_server:log(task_log, [RoleId, TaskId, Time]).

-spec achievement_log(RoleId :: integer(), AchievementId :: integer(), Time :: integer()) -> ok.
achievement_log(RoleId, AchievementId, Time) ->
    log_server:log(achievement_log, [RoleId, AchievementId, Time]).

-spec fashion_log(RoleId :: integer(), FashionId :: integer(), From :: term(), Time :: integer()) -> ok.
fashion_log(RoleId, FashionId, From, Time) ->
    log_server:log(fashion_log, [RoleId, FashionId, From, Time]).

-spec title_log(RoleId :: integer(), TitleId :: integer(), From :: term(), Time :: integer()) -> ok.
title_log(RoleId, TitleId, From, Time) ->
    log_server:log(title_log, [RoleId, TitleId, From, Time]).

-spec bubble_log(RoleId :: integer(), BubbleId :: integer(), From :: term(), Time :: integer()) -> ok.
bubble_log(RoleId, BubbleId, From, Time) ->
    log_server:log(bubble_log, [RoleId, BubbleId, From, Time]).

-spec auction_log(AuctionId :: integer(), Number :: integer(), BidNumber :: integer(), Price :: integer(), RoleId :: integer(), RoleName :: binary(), ServerId :: integer(), Time :: integer()) -> ok.
auction_log(AuctionId, Number, BidNumber, Price, RoleId, RoleName, ServerId, Time) ->
    log_server:log(auction_log, [AuctionId, Number, BidNumber, Price, RoleId, RoleName, ServerId, Time]).


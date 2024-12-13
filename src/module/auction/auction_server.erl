%%%-------------------------------------------------------------------
%%% @doc
%%% auction server
%%% @end
%%%-------------------------------------------------------------------
-module(auction_server).
-behaviour(gen_server).
%% API
-export([start/0, start_link/0]).
-export([add/5, query/1, query/2, bid/3]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
%% includes
-include_lib("stdlib/include/ms_transform.hrl").
-include("common.hrl").
-include("time.hrl").
-include("journal.hrl").
-include("user.hrl").
-include("role.hrl").
-include("guild.hrl").
-include("auction.hrl").
%% Macros
%% auction type
-define(AUCTION_TYPE_ALL,         1).
-define(AUCTION_TYPE_GUILD,       2).
%% bid type
-define(AUCTION_BID_TYPE_NORMAL,  1).
-define(AUCTION_BID_TYPE_ONCE,    2).
%% role type
-define(AUCTION_ROLE_TYPE_SELLER, 1).
-define(AUCTION_ROLE_TYPE_BIDDER, 2).
%%%===================================================================
%%% API functions
%%%===================================================================
%% @doc start
-spec start() -> {ok, pid()} | {error, term()}.
start() ->
    process:start(?MODULE).

%% @doc server start
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc add
-spec add(AuctionList :: list(), Type :: non_neg_integer(), GuildId :: non_neg_integer(), From :: term(), SellerList :: list()) -> ok.
add(AuctionList, Type, GuildId, From, SellerList) ->
    gen_server:cast(?MODULE, {add, AuctionList, Type, GuildId, From, SellerList}).

%% @doc query
-spec query(User :: #user{}) -> ok().
query(_) ->
    {ok, ?MODULE}.

%% @doc query
-spec query(User :: #user{}, GuildId :: non_neg_integer()) -> ok().
query(_, GuildId) ->
    {ok, ets:select(?MODULE, ets:fun2ms(fun(Auction = #auction{type = ?AUCTION_TYPE_GUILD, guild_id = ThisGuildId}) when GuildId == ThisGuildId -> Auction end))}.

%% @doc bid
-spec bid(User :: #user{}, AuctionNo :: non_neg_integer(), NextPrice :: non_neg_integer()) -> ok() | error().
bid(User, AuctionNo, NextPrice) ->
    case asset:cost(User, [{gold, NextPrice}], auction) of
        {ok, NewUser} ->
            do_bid(NewUser, AuctionNo, NextPrice);
        _ ->
            {error, {gold_not_enough, 0, #auction{}}}
    end.

do_bid(NewUser = #user{role_id = RoleId, role_name = RoleName, role = #role{server_id = ServerId}, guild = #guild_role{guild_id = GuildId, guild_name = GuildName}}, AuctionNo, NextPrice) ->
    case catch gen_server:call(?MODULE, {bid, AuctionNo, NextPrice, ServerId, RoleId, RoleName, GuildId, GuildName}) of
        {ok, Result} ->
            asset:push(NewUser),
            {ok, Result, NewUser};
        {'EXIT', {timeout, _}} ->
            {ok, {timeout, 0, #auction{}}, NewUser};
        Error ->
            Error
    end.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
%% @doc init
-spec init(Args :: term()) -> {ok, State :: []}.
init([]) ->
    erlang:process_flag(trap_exit, true),
    ets:new(?MODULE, [named_table, set, {keypos, #auction.auction_no}, {read_concurrency, true}, {write_concurrency, true}]),
    %% filter different type auction role
    {SellerList, BidderList} = lists:partition(fun(#auction_role{type = Type}) -> Type == ?AUCTION_ROLE_TYPE_SELLER end, auction_role_sql:select()),
    %% collect per auction role
    SellerRoleList = listing:key_group(#auction_role.auction_no, SellerList),
    BidderRoleList = listing:key_group(#auction_role.auction_no, BidderList),
    %% auction
    [ets:insert(?MODULE, update_timer(Auction#auction{seller_list = element(2, listing:key_find(AuctionNo, 1, SellerRoleList, {AuctionNo, []})), bidder_list = element(2, listing:key_find(AuctionNo, 1, BidderRoleList, {AuctionNo, []})), timer = undefined}, time:now())) || Auction = #auction{auction_no = AuctionNo} <- auction_sql:select()],
    %% 1. select last/max id on the server start.
    %% MySQL AUTO_INCREMENT will recalculate with the max(`id`) from the table on reboot
    %% select last/max auto increment auction no (start with auction no + 1) like this
    %% AuctionNo = db:select_one("SELECT MAX(`auction_no`) FROM `auction`"),
    %% 2. query AUTO_INCREMENT from information_schema.`TABLES` like this (not recommend)
    %% AuctionNo = db:select_one("SELECT AUTO_INCREMENT FROM information_schema.`TABLES` WHERE `TABLE_SCHEMA` = DATABASE() AND `TABLE_NAME` = 'auction'"),
    %% 3. insert and delete(optionally), it looks better.
    %% save timer
    erlang:send_after(?MINUTE_MILLISECONDS(3), self(), loop),
    %% set start auction no
    {ok, []}.

%% @doc handle_call
-spec handle_call(Request :: term(), From :: {pid(), Tag :: term()}, State :: []) -> {reply, Reply :: term(), NewState :: []}.
handle_call(Request, From, State) ->
    try
        do_call(Request, From, State)
    catch ?EXCEPTION(Class, Reason, Stacktrace) ->
        ?STACKTRACE(Class, Reason, ?GET_STACKTRACE(Stacktrace)),
        {reply, ok, State}
    end.

%% @doc handle_cast
-spec handle_cast(Request :: term(), State :: []) -> {noreply, NewState :: []}.
handle_cast(Request, State) ->
    try
        do_cast(Request, State)
    catch ?EXCEPTION(Class, Reason, Stacktrace) ->
        ?STACKTRACE(Class, Reason, ?GET_STACKTRACE(Stacktrace)),
        {noreply, State}
    end.

%% @doc handle_info
-spec handle_info(Request :: term(), State :: []) -> {noreply, NewState :: []}.
handle_info(Info, State) ->
    try
        do_info(Info, State)
    catch ?EXCEPTION(Class, Reason, Stacktrace) ->
        ?STACKTRACE(Class, Reason, ?GET_STACKTRACE(Stacktrace)),
        {noreply, State}
    end.

%% @doc terminate
-spec terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()), State :: []) -> {ok, NewState :: []}.
terminate(_Reason, State) ->
    try
        %% save auction
        auction_sql:save(?MODULE),
        %% save role
        ess:foreach(fun([#auction{seller_list = SellerList, bidder_list = BidderList}]) -> auction_role_sql:save(SellerList), auction_role_sql:save(BidderList) end, ?MODULE)
    catch ?EXCEPTION(Class, Reason, Stacktrace) ->
        ?STACKTRACE(Class, Reason, ?GET_STACKTRACE(Stacktrace))
    end,
    {ok, State}.

%% @doc code_change
-spec code_change(OldVsn :: (term() | {down, term()}), State :: [], Extra :: term()) -> {ok, NewState :: []}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
do_call({bid, AuctionNo, NextPrice, ServerId, RoleId, RoleName, GuildId, GuildName}, _From, State) ->
    {reply, inner_bid(AuctionNo, NextPrice, ServerId, RoleId, RoleName, GuildId, GuildName), State};
do_call(_Request, _From, State) ->
    {reply, ok, State}.

do_cast({add, AuctionList, Type, GuildId, From, SellerList}, State) ->
    List = add_auction_loop(AuctionList, time:now(), Type, GuildId, From, SellerList, []),
    NewList = auction_sql:save(List),
    ets:insert(?MODULE, NewList),
    {noreply, State};
do_cast(_Request, State) ->
    {noreply, State}.

do_info(loop, State) ->
    %% save timer
    erlang:send_after(?MINUTE_MILLISECONDS(3), self(), loop),
    %% save auction
    auction_sql:save(?MODULE),
    %% save role
    ess:foreach(fun([#auction{seller_list = SellerList, bidder_list = BidderList}]) -> auction_role_sql:save(SellerList), auction_role_sql:save(BidderList) end, ?MODULE),
    {noreply, State};
do_info({timeout, Timer, AuctionNo}, State) ->
    %% timeout
    auction_over(ets:lookup(?MODULE, AuctionNo), Timer),
    {noreply, State};
do_info(_Info, State) ->
    {noreply, State}.

%% bit in auction server
inner_bid(AuctionNo, NextPrice, ServerId, RoleId, RoleName, GuildId, GuildName) ->
    AuctionRole = #auction_role{auction_no = AuctionNo, server_id = ServerId, role_id = RoleId, role_name = RoleName, guild_id = GuildId, guild_name = GuildName, type = ?AUCTION_ROLE_TYPE_BIDDER, price = NextPrice, time = time:now(), flag = 1},
    case ets:lookup(?MODULE, AuctionNo) of
        [Auction = #auction{type = ?AUCTION_TYPE_GUILD, guild_id = GuildId, bid_type = ?AUCTION_BID_TYPE_NORMAL, next_price = NextPrice}] ->
            NewAuction = auction_update(Auction, AuctionRole),
            ets:insert(?MODULE, NewAuction),
            {ok, {ok, 0, NewAuction}};
        [Auction = #auction{type = ?AUCTION_TYPE_ALL, bid_type = ?AUCTION_BID_TYPE_NORMAL, next_price = NextPrice}] ->
            NewAuction = auction_update(Auction, AuctionRole),
            ets:insert(?MODULE, NewAuction),
            {ok, {ok, 0, NewAuction}};
        [Auction = #auction{type = ?AUCTION_TYPE_GUILD, guild_id = GuildId, bid_type = ?AUCTION_BID_TYPE_ONCE, next_price = NextPrice, timer = Timer}] ->
            %% update top bidder
            NewAuction = auction_update(Auction, AuctionRole),
            auction_over(NewAuction, Timer),
            {ok, {ok, 0, NewAuction}};
        [Auction = #auction{type = ?AUCTION_TYPE_ALL, bid_type = ?AUCTION_BID_TYPE_ONCE, next_price = NextPrice, timer = Timer}] ->
            %% update top bidder
            NewAuction = auction_update(Auction, AuctionRole),
            auction_over(NewAuction, Timer),
            {ok, {ok, 0, NewAuction}};
        [#auction{next_price = OtherNextPrice}] ->
            {error, {auction_price_changed, OtherNextPrice, #auction{}}};
        _ ->
            {error, {auction_not_found, 0, #auction{}}}
    end.

%% auction update
auction_update(Auction = #auction{auction_id = AuctionId, next_price = NextPrice, bid_number = BidNumber, end_time = EndTime, bidder_list = BidderList}, AuctionRole = #auction_role{role_id = RoleId}) ->
    Now = time:now(),
    %% change end time
    #auction_data{add_price = AddPrice, overtime = DelayTime, critical_time = CriticalTime} = auction_data:get(AuctionId),
    case Now - EndTime < CriticalTime of
        true ->
            NewEndTime = EndTime + DelayTime;
        false ->
            NewEndTime = EndTime
    end,
    %% update top bidder
    NewBidderList = [AuctionRole | lists:keydelete(RoleId, #auction_role.role_id, BidderList)],
    update_timer(Auction#auction{now_price = NextPrice, next_price = NextPrice + AddPrice, bid_number = BidNumber + 1, end_time = NewEndTime, bidder_list = NewBidderList, flag = 1}, Now).

%% auction over
auction_over([Auction | _], Timer) ->
    auction_over(Auction, Timer);
auction_over(Auction, Timer) ->
    case Auction of
        #auction{auction_no = AuctionNo, type = ?AUCTION_TYPE_ALL, bidder_list = [], timer = Timer} ->
            %% all auction failed, take off it
            ets:delete(?MODULE, AuctionNo),
            auction_sql:delete(AuctionNo),
            auction_role_sql:delete_by_auction_no(AuctionNo);
        #auction{auction_id = AuctionId, type = ?AUCTION_TYPE_GUILD, bidder_list = [], timer = Timer} ->
            %% guild auction failed, transfer to all auction
            Now = time:now(),
            #auction_data{show_time = ShowTime, auction_time = AuctionTime} = auction_data:get(AuctionId),
            ets:insert(?MODULE, update_timer(Auction#auction{type = ?AUCTION_TYPE_ALL, end_time = Now + ShowTime + AuctionTime}, Now));
        #auction{auction_no = AuctionNo, auction_id = AuctionId, number = Number, now_price = NowPrice, seller_list = SellerList, bidder_list = [#auction_role{role_id = RoleId} | _], timer = Timer} ->
            ets:delete(?MODULE, AuctionNo),
            auction_sql:delete(AuctionNo),
            auction_role_sql:delete_by_auction_no(AuctionNo),
            %% calculate tex and seller income
            #auction_data{tax = Tax} = auction_data:get(AuctionId),
            Income = erlang:round((NowPrice - erlang:round(NowPrice * (Tax / 100))) / length(SellerList)),
            %% sellers income
            [mail:send(ThisRoleId, mail_text_auction_income_title, mail_text_auction_income_content, ?MODULE, asset:convert([{gold, Income}])) || #auction_role{role_id = ThisRoleId} <- SellerList],
            %% final the top bidder items
            mail:send(RoleId, mail_text_auction_success_title, mail_text_auction_success_content, ?MODULE, [{AuctionId, Number}]);
        _ ->
            skip
    end.

%% add auction item
add_auction_loop([], _, _, _, _, _, List) ->
    List;
add_auction_loop([{AuctionId, Number} | T], Now, Type, GuildId, From, SellerList, List) ->
    #auction_data{bid_type = BidType, begin_price = BeginPrice, add_price = AddPrice, show_time = ShowTime, auction_time = AuctionTime} = auction_data:get(AuctionId),
    AuctionNo = increment_server:next(auction),
    SellerRoleList = [to_auction_role(AuctionNo, Seller, Now) || Seller <- SellerList],
    Auction = update_timer(#auction{auction_no = AuctionNo, auction_id = AuctionId, now_price = BeginPrice, next_price = BeginPrice + AddPrice, number = Number, seller_list = SellerRoleList, bidder_list = [], start_time = Now + ShowTime, end_time = Now + ShowTime + AuctionTime, type = Type, guild_id = GuildId, bid_type = BidType, from = From, flag = 1}, Now),
    add_auction_loop(T, Now, Type, GuildId, From, SellerList, [Auction | List]).

%% convert to auction role
to_auction_role(AuctionNo, {ServerId, RoleId, RoleName}, Now) ->
    #auction_role{auction_no = AuctionNo, server_id = ServerId, role_id = RoleId, role_name = RoleName, type = ?AUCTION_ROLE_TYPE_SELLER, time = Now, flag = 1};
to_auction_role(AuctionNo, {ServerId, RoleId, RoleName, GuildId, GuildName}, Now) ->
    #auction_role{auction_no = AuctionNo, server_id = ServerId, role_id = RoleId, role_name = RoleName, guild_id = GuildId, guild_name = GuildName, type = ?AUCTION_ROLE_TYPE_SELLER, time = Now, flag = 1}.

%% update finish timer
update_timer(Auction = #auction{auction_no = AuctionNo, timer = Timer, end_time = EndTime}, Now) when Now < EndTime ->
    catch erlang:cancel_timer(Timer),
    Ref = erlang:start_timer(?SECOND_MILLISECONDS(EndTime - Now), self(), AuctionNo),
    Auction#auction{timer = Ref};
update_timer(Auction, _) ->
    auction_over(Auction, undefined),
    [].

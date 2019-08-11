%%%-------------------------------------------------------------------
%%% @doc
%%% module auction server
%%% @end
%%%-------------------------------------------------------------------
-module(auction_server).
-behaviour(gen_server).
%% API
-export([start/0, start_link/0]).
-export([add/4, bid/2]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
%% includes
-include("common.hrl").
-include("user.hrl").
-include("auction.hrl").
-include("protocol.hrl").
%%%===================================================================
%%% API
%%%===================================================================
%% @doc start
-spec start() -> {ok, Pid :: pid()} | {error, term()}.
start() ->
    process:start(?MODULE).

%% @doc server start
-spec start_link() -> {ok, Pid :: pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc add
-spec add(AuctionList :: list(), Type :: non_neg_integer(), From :: term(), SellerList :: list()) -> ok.
add(AuctionList, Type, From, SellerList) ->
    gen_server:cast(?MODULE, {add, AuctionList, Type, From, SellerList}).

%% @doc bid
-spec bid(User :: #user{}, UniqueId :: non_neg_integer()) -> {ok, term(), #user{}} | {error, term()}.
bid(User = #user{role_id = RoleId, role_name = RoleName, server_id = ServerId}, UniqueId) ->
    case ets:lookup(?MODULE, UniqueId) of
        [#auction{price = Price, bid_number = BidNumber, auction_id = AuctionId}] ->
            #auction_data{add_price = AddPrice} = auction_data:get(AuctionId),
            NewPrice = Price + AddPrice * BidNumber,
            case asset:cost(User, [{gold, NewPrice}]) of
                {ok, NewUser} ->
                    case gen_server:call(?MODULE, {bid, UniqueId, Price, NewPrice, RoleId, RoleName, ServerId}, 5000) of
                        {ok, Result} ->
                            {ok, Result, NewUser};
                        _ ->
                            {ok, [5, 0, #auction{}], NewUser}
                    end;
                _ ->
                    {error, [4, 0, #auction{}]}
            end;
        _ ->
            {error, [3, 0, #auction{}]}
    end.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([]) ->
    ets:new(?MODULE, [named_table, set, {keypos, #auction.unique_id}, {write_concurrency, true}, {read_concurrency, true}]),
    Now = time:ts(),
    parser:convert(auction_sql:select(), auction, fun(X) -> ets:insert(?MODULE, update_timer(X, Now)) end),
    %% save timer
    erlang:send_after(?MINUTE_SECONDS * 3 * 1000, self(), loop),
    {ok, []}.

handle_call(Request, From, State) ->
    try
        do_call(Request, From, State)
    catch ?EXCEPTION(_Class, Reason, Stacktrace) ->
        ?STACKTRACE(Reason, ?GET_STACKTRACE(Stacktrace)),
        {reply, ok, State}
    end.

handle_cast(Request, State) ->
    try
        do_cast(Request, State)
    catch ?EXCEPTION(_Class, Reason, Stacktrace) ->
        ?STACKTRACE(Reason, ?GET_STACKTRACE(Stacktrace)),
        {noreply, State}
    end.

handle_info(Info, State) ->
    try
        do_info(Info, State)
    catch ?EXCEPTION(_Class, Reason, Stacktrace) ->
        ?STACKTRACE(Reason, ?GET_STACKTRACE(Stacktrace)),
        {noreply, State}
    end.

terminate(_Reason, _State) ->
    try
        auction_sql:update_into(?MODULE)
    catch ?EXCEPTION(_Class, Reason, Stacktrace) ->
        ?STACKTRACE(Reason, ?GET_STACKTRACE(Stacktrace)),
        ok
    end.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%-------------------------------------------------------------------
%% main sync role process call back
%%-------------------------------------------------------------------
do_call({bid, UniqueId, Price, NewPrice, RoleId, RoleName, ServerId}, _From, State) ->
    case ets:lookup(?MODULE, UniqueId) of
        [Auction = #auction{auction_id = AuctionId, price = Price, bid_number = BidNumber, end_time = EndTime, bidder_id = BidderId, bidder_name = BidderName}] ->
            %% match price (no other bid in this period)
            #auction_data{delay_time = DelayTime, critical_time = CriticalTime} = auction_data:get(AuctionId),
            Now = time:ts(),
            case Now - EndTime < CriticalTime of
                true ->
                    NewEndTime = EndTime + DelayTime;
                false ->
                    NewEndTime = EndTime
            end,
            NewAuction = update_timer(Auction#auction{
                price = NewPrice,
                bid_number = BidNumber + 1,
                end_time = NewEndTime,
                bidder_id = RoleId,
                bidder_name = RoleName,
                bidder_server_id = ServerId,
                flag = 1
            }, Now),
            %% return gold
            mail:send(BidderId, BidderName, auction_return_title, auction_return_content, auction, asset:convert([{gold, Price}])),
            ets:insert(?MODULE, NewAuction),
            {reply, {ok, [1, 0, NewAuction]}, State};
        [#auction{price = NewPrice}] ->
            {reply, {error, [2, NewPrice, #auction{}]}, State};
        [] ->
            {reply, {error, [3, 0, #auction{}]}, State}
    end;
do_call(_Request, _From, State) ->
    {reply, ok, State}.

%%-------------------------------------------------------------------
%% main async role process call back
%%-------------------------------------------------------------------
do_cast({add, AuctionList, Type, From, SellerList}, State) ->
    %% last/max auto increment unique id attention
    UniqueId = sql:select_one("select max(`unique_id`) from `auction`"),
    List = add_auction_loop(AuctionList, UniqueId + 1, time:ts(), Type, From, SellerList, []),
    NewList = auction_sql:update_into(List),
    ets:insert(?MODULE, NewList),
    {noreply, State};
do_cast(_Request, State) ->
    {noreply, State}.


%%-------------------------------------------------------------------
%% self message call back
%%-------------------------------------------------------------------
do_info(loop, State) ->
    %% save timer
    erlang:send_after(?MINUTE_SECONDS * 3 * 1000, self(), loop),
    %% save loop
    auction_sql:update_into(?MODULE),
    {noreply, State};
do_info({timeout, UniqueId, 'stop'}, State) ->
    case ets:lookup(?MODULE, UniqueId) of
        [#auction{auction_id = AuctionId, number = Number, bidder_id = BidderId, bidder_name = BidderName}] ->
            ets:delete(?MODULE, UniqueId),
            auction_sql:delete(UniqueId),
            mail:send(BidderId, BidderName, auction_success_title, auction_success_content, auction, [{AuctionId, Number, 0}]);
        [] ->
            {noreply, State}
    end;
do_info(_Info, State) ->
    {noreply, State}.


%%%===================================================================
%%% Internal functions
%%%===================================================================
%% update finish timer
update_timer(Auction = #auction{unique_id = UniqueId, timer = Timer, end_time = EndTime}, Now) ->
    catch erlang:cancel_timer(Timer),
    Ref = erlang:start_timer(EndTime - Now, self(), UniqueId),
    Auction#auction{timer = Ref}.

%% add auction item
add_auction_loop([], _, _, _, _, _, List) ->
    List;
add_auction_loop([{AuctionId, Number} | T], UniqueId, Now, Type, From, SellerList, List) ->
    #auction_data{show_time = ShowTime, auction_time = AuctionTime} = auction_data:get(AuctionId),
    AuctionInfo = update_timer(#auction{
        unique_id = UniqueId,
        auction_id = AuctionId,
        number = Number,
        start_time = Now + ShowTime,
        end_time = Now + ShowTime + AuctionTime,
        type = Type,
        from = From,
        seller_list = SellerList,
        flag = 2
    }, Now),
    add_auction_loop(T, UniqueId + 1, Now, Type, From, SellerList, [AuctionInfo | List]).

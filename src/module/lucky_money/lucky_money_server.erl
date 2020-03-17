%%%------------------------------------------------------------------
%%% @doc
%%% module lucky money server
%%% @end
%%%------------------------------------------------------------------
-module(lucky_money_server).
-behaviour(gen_server).
%% API
-export([start/0, start_link/0]).
-export([query/0, add/7, receive_lucky_money/2]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
%% includes
-include("common.hrl").
-include("protocol.hrl").
-include("user.hrl").
-include("guild.hrl").
-include("lucky_money.hrl").
%%%==================================================================
%%% API functions
%%%==================================================================
%% @doc start
-spec start() -> {ok, pid()} | {error, term()}.
start() ->
    process:start(?MODULE).

%% @doc server start
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc query
-spec query() -> ok().
query() ->
    {ok, lucky_money}.

%% @doc add
-spec add(ServerId :: non_neg_integer(), RoleId :: non_neg_integer(), RoleName :: binary(), GuildId :: non_neg_integer(), GuildName :: binary(), TotalGold :: non_neg_integer(), TotalNumber :: non_neg_integer()) -> ok.
add(ServerId, RoleId, RoleName, GuildId, GuildName, TotalGold, TotalNumber) ->
    gen_server:cast(?MODULE, {add, ServerId, RoleId, RoleName, GuildId, GuildName, TotalGold, TotalNumber}).

%% @doc receive lucky money
-spec receive_lucky_money(User :: #user{}, LuckyMoneyId :: non_neg_integer()) -> ok() | error().
receive_lucky_money(User = #user{server_id = ServerId, role_id = RoleId, role_name = RoleName}, LuckyMoneyId) ->
    case ets:lookup(lucky_money, LuckyMoneyId) of
        [#lucky_money{remain_gold = RemainGold}] when RemainGold > 0 ->
            GuildId = role:guild_id(User),
            GuildName = role:guild_name(User),
            case process:call(?MODULE, {receive_lucky_money, LuckyMoneyId, ServerId, RoleId, RoleName, GuildId, GuildName}) of
                {ok, Gold} ->
                    {ok, NewUser} = asset:add(User, [{gold, Gold}], lucky_money),
                    {ok, [ok, Gold], NewUser};
                {error, timeout} ->
                    {error, [timeout, 0]};
                Error ->
                    Error
            end;
        _ ->
            {error, [no_such_lucky_money, 0]}
    end.

%%%==================================================================
%%% gen_server callbacks
%%%==================================================================
init([]) ->
    ets:new(lucky_money, [named_table, set, {keypos, #lucky_money.lucky_money_id}, {write_concurrency, true}, {read_concurrency, true}]),
    RoleList = listing:key_merge(#lucky_money_role.lucky_money_id, lucky_money_role_sql:select()),
    lists:foreach(fun(LuckyMoney = #lucky_money{lucky_money_id = LuckyMoneyId}) -> ets:insert(lucky_money, LuckyMoney#lucky_money{receive_list = element(2, listing:key_find(LuckyMoneyId, 1, RoleList, {0, []}))}) end, lucky_money_sql:select()),
    %% save timer
    erlang:send_after(?MINUTE_MILLISECONDS(3), self(), loop),
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
        ess:foreach(fun(#lucky_money{receive_list = ReceiveList}) -> lucky_money_role_sql:insert_update(ReceiveList) end, lucky_money),
        lucky_money_sql:insert_update(lucky_money)
    catch ?EXCEPTION(_Class, Reason, Stacktrace) ->
        ?STACKTRACE(Reason, ?GET_STACKTRACE(Stacktrace)),
        ok
    end.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%==================================================================
%%% Internal functions
%%%==================================================================
do_call({receive_lucky_money, LuckyMoneyId, ServerId, RoleId, RoleName, GuildId, GuildName}, _From, State) ->
    Now = time:ts(),
    case ets:lookup(lucky_money, LuckyMoneyId) of
        [LuckyMoney = #lucky_money{remain_gold = RemainGold, total_number = TotalNumber, receive_number = ReceiveNumber, receive_list = ReceiveList, time = Time}] when Time + ?DAY_SECONDS < Now andalso ReceiveNumber + 1 == TotalNumber ->
            case lists:keymember(RoleId, #lucky_money_role.role_id, ReceiveList) of
                false ->
                    Role = #lucky_money_role{lucky_money_id = LuckyMoneyId, server_id = ServerId, role_id = RoleId, role_name = RoleName, guild_id = GuildId, guild_name = GuildName, gold = RemainGold, time = time:ts(), flag = 1},
                    ets:insert(lucky_money, LuckyMoney#lucky_money{remain_gold = 0, receive_number = ReceiveNumber + 1, receive_list = [Role | ReceiveList], flag = 1}),
                    {reply, {ok, RemainGold}, State};
                true ->
                    {reply, {error, [lucky_money_already_receive, 0]}, State}
            end;
        [LuckyMoney = #lucky_money{remain_gold = RemainGold, total_number = TotalNumber, receive_number = ReceiveNumber, receive_list = ReceiveList, time = Time}] when Time + ?DAY_SECONDS < Now andalso ReceiveNumber < TotalNumber ->
            case lists:keymember(RoleId, #lucky_money_role.role_id, ReceiveList) of
                false ->
                    %% min radix
                    Radix = 1,
                    %% when n > 1
                    Gold = randomness:rand(Radix, (RemainGold - ((TotalNumber - ReceiveNumber - 1) * Radix))),
                    Role = #lucky_money_role{lucky_money_id = LuckyMoneyId, server_id = ServerId, role_id = RoleId, role_name = RoleName, guild_id = GuildId, guild_name = GuildName, gold = Gold, time = time:ts(), flag = 1},
                    ets:insert(lucky_money, LuckyMoney#lucky_money{remain_gold = RemainGold - Gold, receive_number = ReceiveNumber + 1, receive_list = [Role | ReceiveList], flag = 1}),
                    {reply, {ok, Gold}, State};
                true ->
                    {reply, {error, [lucky_money_already_receive, 0]}, State}
            end;
        [#lucky_money{time = Time}] when Time + ?DAY_SECONDS < Now ->
            {reply, {error, [lucky_money_expire, 0]}, State};
        [#lucky_money{}] ->
            {reply, {error, [no_more_lucky_money, 0]}, State};
        [] ->
            {reply, {error, [no_such_lucky_money, 0]}, State}
    end;
do_call(_Request, _From, State) ->
    {reply, ok, State}.


do_cast({add, ServerId, RoleId, RoleName, GuildId, GuildName, TotalGold, TotalNumber}, State) ->
    LuckyMoney = #lucky_money{server_id = ServerId, role_id = RoleId, role_name = RoleName, guild_id = GuildId, guild_name = GuildName, total_gold = TotalGold, total_number = TotalNumber, flag = 1},
    LuckyMoneyId = lucky_money_sql:insert(LuckyMoney),
    ets:insert(lucky_money, LuckyMoney#lucky_money{lucky_money_id = LuckyMoneyId}),
    {ok, Binary} = user_router:write(?PROTOCOL_LUCKY_MONEY_LIST, [LuckyMoney]),
    user_manager:broadcast(Binary),
    {noreply, State};
do_cast(_Request, State) ->
    {noreply, State}.


do_info(loop, State) ->
    %% save timer
    erlang:send_after(?MINUTE_MILLISECONDS(3), self(), loop),
    %% save loop
    ess:foreach(fun([#lucky_money{receive_list = ReceiveList}]) -> lucky_money_role_sql:insert_update(ReceiveList) end, lucky_money),
    lucky_money_sql:insert_update(lucky_money),
    {noreply, State};
do_info(_Info, State) ->
    {noreply, State}.
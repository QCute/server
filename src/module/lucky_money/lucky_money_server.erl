%%%-------------------------------------------------------------------
%%% @doc
%%% lucky money server
%%% @end
%%%-------------------------------------------------------------------
-module(lucky_money_server).
-behaviour(gen_server).
%% API
-export([start/0, start_link/0]).
-export([query/2, add/7, receive_lucky_money/2]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
%% includes
-include_lib("stdlib/include/ms_transform.hrl").
-include("common.hrl").
-include("time.hrl").
-include("journal.hrl").
-include("user.hrl").
-include("role.hrl").
-include("chat.hrl").
-include("guild.hrl").
-include("lucky_money.hrl").
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

%% @doc query
-spec query(User :: #user{}, LuckyMoneyNo :: non_neg_integer()) -> ok().
query(_, LuckyMoneyNo) ->
    case ets:lookup(?MODULE, LuckyMoneyNo) of
        [LuckyMoney] ->
            {ok, LuckyMoney};
        [] ->
            {ok, #lucky_money{}}
    end.

%% @doc add
-spec add(User :: #user{}, TotalGold :: non_neg_integer(), TotalNumber :: non_neg_integer(), Scope :: atom(), Restrict :: non_neg_integer(), Skin :: non_neg_integer(), Message :: binary()) -> ok() | error().
add(User = #user{role_id = RoleId, role_name = RoleName, role = #role{server_id = ServerId}, guild = #guild_role{guild_id = GuildId, guild_name = GuildName}}, TotalGold, TotalNumber, Scope, Restrict, Skin, Message) ->
    %% add lucky money
    LuckyMoneyNo = increment_server:next(lucky_money),
    LuckyMoney = #lucky_money{lucky_money_no = LuckyMoneyNo, server_id = ServerId, role_id = RoleId, role_name = RoleName, guild_id = GuildId, guild_name = GuildName, total_gold = TotalGold, remain_gold = TotalGold, total_number = TotalNumber, scope = Scope, restrict = Restrict, skin = Skin, message = Message, time = time:now()},
    ets:insert(?MODULE, LuckyMoney),
    %% notify
    case Scope of
        world ->
            chat:world_notify(User, [Skin, LuckyMoneyNo, 0, ?CHAT_TYPE_LUCKY_MONEY, Message]);
        guild ->
            chat:guild_notify(User, [Skin, LuckyMoneyNo, 0, ?CHAT_TYPE_LUCKY_MONEY, Message]);
        private ->
            chat:private_notify(User, Restrict, [Restrict, Skin, LuckyMoneyNo, 0, ?CHAT_TYPE_LUCKY_MONEY, Message])
    end.

%% @doc receive lucky money
-spec receive_lucky_money(User :: #user{}, LuckyMoneyNo :: non_neg_integer()) -> ok() | error().
receive_lucky_money(User = #user{role_id = RoleId, role_name = RoleName, role = #role{server_id = ServerId}, guild = #guild_role{guild_id = GuildId, guild_name = GuildName}}, LuckyMoneyNo) ->
    case catch gen_server:call(?MODULE, {receive_lucky_money, LuckyMoneyNo, ServerId, RoleId, RoleName, GuildId, GuildName}) of
        {ok, Gold} ->
            {ok, NewUser} = asset:add(User, [{gold, Gold}], ?MODULE),
            {ok, {ok, Gold}, NewUser};
        {'EXIT', {timeout, _}} ->
            {error, {timeout, 0}};
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
    ets:new(?MODULE, [named_table, set, public, {keypos, #lucky_money.lucky_money_no}, {read_concurrency, true}, {write_concurrency, true}]),
    RoleList = listing:key_group(#lucky_money_role.lucky_money_no, lucky_money_role_sql:select()),
    lists:foreach(fun(LuckyMoney = #lucky_money{lucky_money_no = LuckyMoneyNo}) -> ets:insert(?MODULE, LuckyMoney#lucky_money{receive_list = element(2, listing:key_find(LuckyMoneyNo, 1, RoleList, {0, []}))}) end, lucky_money_sql:select()),
    %% save timer
    erlang:send_after(?MINUTE_MILLISECONDS(3), self(), {loop, time:now()}),
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
        ess:foreach(fun([#lucky_money{receive_list = ReceiveList}]) -> lucky_money_role_sql:save(ReceiveList) end, ?MODULE),
        lucky_money_sql:save(?MODULE)
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
do_call({receive_lucky_money, LuckyMoneyNo, ServerId, RoleId, RoleName, GuildId, GuildName}, _From, State) ->
    case ets:lookup(?MODULE, LuckyMoneyNo) of
        [LuckyMoney = #lucky_money{}] ->
            receive_check_scope(ServerId, RoleId, RoleName, GuildId, GuildName, LuckyMoney, State);
        [] ->
            {reply, {error, {lucky_money_not_found, 0}}, State}
    end;
do_call(_Request, _From, State) ->
    {reply, ok, State}.


do_cast(_Request, State) ->
    {noreply, State}.


do_info({loop, Before}, State) ->
    Now = time:now(),
    Date = time:zero(Now),
    %% next timer
    erlang:send_after(?MINUTE_MILLISECONDS(3), self(), {loop, Now}),
    case time:is_cross_day(Before, 0, Now) of
        true ->
            %% filter expire lucky money
            ExpireList = ets:select(?MODULE, ets:fun2ms(fun(LuckyMoney = #lucky_money{remain_gold = 0, time = Time}) when Time + ?DAY_SECONDS < Date -> LuckyMoney end)),
            %% delete ets info, delete database this lucky role info, collect lucky money id
            ExpireIdList = [begin ets:delete(?MODULE, LuckyMoneyNo), lucky_money_role_sql:delete_by_lucky_money_no(LuckyMoneyNo), LuckyMoneyNo end || #lucky_money{lucky_money_no = LuckyMoneyNo} <- ExpireList],
            %% delete database lucky money by id list
            lucky_money_sql:delete_in_lucky_money_no(ExpireIdList);
        false ->
            skip
    end,
    %% save loop
    ess:foreach(fun([#lucky_money{receive_list = ReceiveList}]) -> lucky_money_role_sql:save(ReceiveList) end, ?MODULE),
    lucky_money_sql:save(?MODULE),
    {noreply, State};
do_info(_Info, State) ->
    {noreply, State}.


receive_check_scope(ServerId, RoleId, RoleName, GuildId, GuildName, LuckyMoney = #lucky_money{scope = Scope}, State) ->
    case Scope of
        world ->
            receive_check_time(ServerId, RoleId, RoleName, GuildId, GuildName, LuckyMoney, State);
        guild when LuckyMoney#lucky_money.guild_id == GuildId ->
            %% guild restrict
            receive_check_time(ServerId, RoleId, RoleName, GuildId, GuildName, LuckyMoney, State);
        guild ->
            {reply, {error, {lucky_money_not_found, 0}}, State};
        private when LuckyMoney#lucky_money.restrict == RoleId ->
            %% role restrict
            receive_check_time(ServerId, RoleId, RoleName, GuildId, GuildName, LuckyMoney, State);
        private ->
            {reply, {error, {lucky_money_not_found, 0}}, State}
    end.

receive_check_time(ServerId, RoleId, RoleName, GuildId, GuildName, LuckyMoney = #lucky_money{time = Time}, State) ->
    Now = time:now(),
    case Now < Time + ?DAY_SECONDS of
        true ->
            receive_check_number(ServerId, RoleId, RoleName, GuildId, GuildName, LuckyMoney, State);
        false ->
            {reply, {error, {lucky_money_expired, 0}}, State}
    end.

receive_check_number(ServerId, RoleId, RoleName, GuildId, GuildName, LuckyMoney = #lucky_money{receive_number = ReceiveNumber, total_number = TotalNumber}, State) ->
    case ReceiveNumber < TotalNumber of
        true ->
            receive_check(ServerId, RoleId, RoleName, GuildId, GuildName, LuckyMoney, State);
        false ->
            {reply, {error, {lucky_money_not_found, 0}}, State}
    end.

receive_check(ServerId, RoleId, RoleName, GuildId, GuildName, LuckyMoney = #lucky_money{receive_list = ReceiveList}, State) ->
    case lists:keymember(RoleId, #lucky_money_role.role_id, ReceiveList) of
        false ->
            receive_update(ServerId, RoleId, RoleName, GuildId, GuildName, LuckyMoney, State);
        true ->
            {reply, {error, {lucky_money_already_received, 0}}, State}
    end.

receive_update(ServerId, RoleId, RoleName, GuildId, GuildName, LuckyMoney = #lucky_money{lucky_money_no = LuckyMoneyNo, remain_gold = RemainGold, total_number = TotalNumber, receive_number = ReceiveNumber, receive_list = ReceiveList}, State) ->
    case ReceiveNumber + 1 == TotalNumber of
        true ->
            Gold = RemainGold;
        false ->
            %% min radix
            Radix = 1,
            %% when n > 1
            Gold = randomness:rand(Radix, (RemainGold - ((TotalNumber - ReceiveNumber - 1) * Radix)))
    end,
    Role = #lucky_money_role{lucky_money_no = LuckyMoneyNo, server_id = ServerId, role_id = RoleId, role_name = RoleName, guild_id = GuildId, guild_name = GuildName, gold = Gold, time = time:now(), flag = 1},
    ets:insert(?MODULE, LuckyMoney#lucky_money{remain_gold = RemainGold - Gold, receive_number = ReceiveNumber + 1, receive_list = [Role | ReceiveList], flag = 1}),
    {reply, {ok, Gold}, State}.

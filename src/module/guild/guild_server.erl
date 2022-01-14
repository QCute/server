%%%-------------------------------------------------------------------
%%% @doc
%%% guild server
%%% @end
%%%-------------------------------------------------------------------
-module(guild_server).
-behaviour(gen_server).
%% API
-export([start/0, start_link/0]).
-export([apply_call/2, apply_call/3, apply_cast/2, apply_cast/3]).
-export([pure_call/2, pure_call/3, pure_cast/2, pure_cast/3]).
-export([call/1, cast/1, info/1]).
-export([
    %% query
    query_guild/0,
    query_role/1,
    query_apply/1,
    query_self_guild/1,
    query_self_role/1,
    query_self_apply/1,
    %% operation
    create/3,
    apply/2,
    cancel_apply/2,
    cancel_all_apply/1,
    approve_apply/2,
    approve_all_apply/1,
    reject_apply/2,
    reject_all_apply/1,
    leave/1,
    dismiss/1,
    kick/2,
    update_job/3,
    upgrade_level/1,
    change_notice/2
]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
%% Includes
-include("common.hrl").
-include("time.hrl").
-include("journal.hrl").
-include("event.hrl").
-include("user.hrl").
-include("role.hrl").
-include("vip.hrl").
-include("guild.hrl").
%%%===================================================================
%%% API functions
%%%===================================================================
%% @doc server start
-spec start() -> {ok, pid()} | {error, term()}.
start() ->
    process:start(?MODULE).

%% @doc server start
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc pure call, apply f,a with state
-spec apply_call(Function :: atom() | function(), Args :: []) -> term().
apply_call(Function, Args) ->
    gen_server:call(?MODULE, {'APPLY_CALL', Function, Args}).

%% @doc pure call, apply m,f,a with state
-spec apply_call(Module :: atom(), Function :: atom() | function(), Args :: []) -> term().
apply_call(Module, Function, Args) ->
    gen_server:call(?MODULE, {'APPLY_CALL', Module, Function, Args}).

%% @doc pure call, apply f,a without state
-spec pure_call(Function :: atom() | function(), Args :: []) -> term().
pure_call(Function, Args) ->
    gen_server:call(?MODULE, {'PURE_CALL', Function, Args}).

%% @doc pure call, apply m,f,a without state
-spec pure_call(Module :: atom(), Function :: atom() | function(), Args :: []) -> term().
pure_call(Module, Function, Args) ->
    gen_server:call(?MODULE, {'PURE_CALL', Module, Function, Args}).

%% @doc apply cast, apply f,a with state
-spec apply_cast(Function :: atom() | function(), Args :: []) -> term().
apply_cast(Function, Args) ->
    gen_server:cast(?MODULE, {'APPLY_CAST', Function, Args}).

%% @doc apply cast, apply m,f,a with state
-spec apply_cast(Module :: atom(), Function :: atom() | function(), Args :: []) -> term().
apply_cast(Module, Function, Args) ->
    gen_server:cast(?MODULE, {'APPLY_CAST', Module, Function, Args}).

%% @doc pure cast, apply f,a without state
-spec pure_cast(Function :: atom() | function(), Args :: []) -> term().
pure_cast(Function, Args) ->
    gen_server:cast(?MODULE, {'PURE_CAST', Function, Args}).

%% @doc pure cast, apply m,f,a without state
-spec pure_cast(Module :: atom(), Function :: atom() | function(), Args :: []) -> term().
pure_cast(Module, Function, Args) ->
    gen_server:cast(?MODULE, {'PURE_CAST', Module, Function, Args}).

%% @doc call
-spec call(Request :: term()) -> Result :: term().
call(Request) ->
    gen_server:call(?MODULE, Request).

%% @doc cast
-spec cast(Request :: term()) -> Result :: term().
cast(Request) ->
    gen_server:cast(?MODULE, Request).

%% @doc info
-spec info(Request :: term()) -> Result :: term().
info(Request) ->
    erlang:send(?MODULE, Request).

%% @doc guild list
-spec query_guild() -> ok().
query_guild() ->
    {ok, guild:guild_table()}.

%% @doc role list
-spec query_role(#user{}) -> ok().
query_role(#user{guild = #guild_role{guild_id = GuildId}}) ->
    {ok, guild:role_table(GuildId)}.

%% @doc apply list
-spec query_apply(#user{}) -> ok().
query_apply(#user{guild = #guild_role{guild_id = GuildId}}) ->
    {ok, guild:apply_table(GuildId)}.

%% @doc self guild info
-spec query_self_guild(#user{}) -> {ok, #guild{}}.
query_self_guild(#user{guild = #guild_role{guild_id = GuildId}}) ->
    {ok, guild:get_guild(GuildId)}.

%% @doc self role info
-spec query_self_role(#user{}) -> {ok, #guild_role{}}.
query_self_role(#user{role_id = RoleId, guild = #guild_role{guild_id = GuildId}}) ->
    {ok, guild:get_role(RoleId, GuildId)}.

%% @doc self apply list
-spec query_self_apply(#user{}) -> {ok, [#guild_apply{}]}.
query_self_apply(#user{role_id = RoleId}) ->
    List = ess:collect(fun(GuildId) -> ets:lookup(guild:apply_table(GuildId), RoleId) end, guild:guild_table()),
    {ok, List}.

%% @doc create guild
-spec create(User :: #user{}, Type :: non_neg_integer(), GuildName :: binary()) -> ok() | error().
create(User, Type, GuildName) ->
    case guild_data:create_type(Type) of
        {_, Condition, Cost} ->
            create_check_condition(User, Type, GuildName, Condition, Cost);
        _ ->
            {error, invalid_type}
    end.

create_check_condition(User, Type, GuildName, Condition, Cost) ->
    case user_checker:check(User, Condition) of
        ok ->
            create_check_cost(User, Type, GuildName, Cost);
        _ ->
            {error, condition_not_met}
    end.

create_check_cost(User, Type, GuildName, Cost) ->
    case item:check(User, Cost, guild_create) of
        {ok, CostList} ->
            create_request(User, Type, GuildName, CostList);
        _ ->
            {error, item_not_enough}
    end.

create_request(User = #user{role_id = RoleId, role_name = RoleName, role = #role{sex = Sex, avatar = Avatar, classes = Classes, level = Level}, vip = #vip{vip_level = VipLevel}}, Type, GuildName, CostList) ->
    case catch call({create, RoleId, RoleName, Sex, Avatar, Classes, Level, VipLevel, Type, GuildName}) of
        {ok, GuildId} ->
            {ok, CostUser} = item:reduce(User, CostList, guild_create),
            NewUser = CostUser#user{guild = guild:get_role(RoleId, GuildId)},
            FinalUser = user_event:trigger(NewUser, #event{name = event_guild_create}),
            {ok, ok, FinalUser};
        {'EXIT', {timeout, _}} ->
            {ok, NewUser} = item:reduce(User, CostList, guild_create),
            {ok, ok, NewUser};
        Error ->
            Error
    end.

%% @doc apply
-spec apply(User :: #user{}, GuildId :: non_neg_integer()) -> ok() | error().
apply(#user{role_id = RoleId, role_name = RoleName, role = #role{sex = Sex, avatar = Avatar, classes = Classes, level = Level}, vip = #vip{vip_level = VipLevel}}, GuildId) ->
    call({apply, GuildId, RoleId, RoleName, Sex, Avatar, Classes, Level, VipLevel}).

%% @doc cancel apply
-spec cancel_apply(User :: #user{}, GuildId :: non_neg_integer()) -> ok() | error().
cancel_apply(#user{role_id = RoleId}, GuildId) ->
    call({cancel_apply, GuildId, RoleId}).

%% @doc cancel all apply
-spec cancel_all_apply(User :: #user{}) -> ok() | error().
cancel_all_apply(#user{role_id = RoleId}) ->
    call({cancel_all_apply, RoleId}).

%% @doc approve apply
-spec approve_apply(User :: #user{}, MemberId :: non_neg_integer()) -> ok() | error().
approve_apply(#user{role_id = RoleId}, MemberId) ->
    call({approve_apply, RoleId, MemberId}).

%% @doc approve all apply
-spec approve_all_apply(User :: #user{}) -> ok() | error().
approve_all_apply(#user{role_id = RoleId}) ->
    call({approve_all_apply, RoleId}).

%% @doc reject apply
-spec reject_apply(User :: #user{}, MemberId :: non_neg_integer()) -> ok() | error().
reject_apply(#user{role_id = RoleId}, MemberId) ->
    call({reject_apply, RoleId, MemberId}).

%% @doc reject all apply
-spec reject_all_apply(User :: #user{}) -> ok() | error().
reject_all_apply(#user{role_id = RoleId}) ->
    call({reject_all_apply, RoleId}).

%% @doc leave
-spec leave(User :: #user{}) -> ok() | error().
leave(#user{role_id = RoleId}) ->
    call({leave, RoleId}).

%% @doc dismiss
-spec dismiss(User :: #user{}) -> ok() | error().
dismiss(#user{role_id = RoleId}) ->
    call({dismiss, RoleId}).

%% @doc kick
-spec kick(User :: #user{}, MemberId :: non_neg_integer()) -> ok() | error().
kick(#user{role_id = RoleId}, MemberId) ->
    call({kick, RoleId, MemberId}).

%% @doc update job
-spec update_job(User :: #user{}, MemberId :: non_neg_integer(), Job :: non_neg_integer()) -> ok() | error().
update_job(#user{role_id = RoleId}, MemberId, Job) ->
    call({update_job, RoleId, MemberId, Job}).

%% @doc upgrade level
-spec upgrade_level(User :: #user{}) -> ok() | error().
upgrade_level(#user{role_id = RoleId}) ->
    call({upgrade_level, RoleId}).

%% @doc change notice
-spec change_notice(User :: #user{}, Notice :: binary()) -> ok() | error().
change_notice(#user{role_id = RoleId}, Notice) ->
    call({change_notice, RoleId, Notice}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
%% @doc init
-spec init(Args :: term()) -> {ok, State :: #guild_state{}}.
init([]) ->
    erlang:process_flag(trap_exit, true),
    guild:server_start().

%% @doc handle_call
-spec handle_call(Request :: term(), From :: {pid(), Tag :: term()}, State :: #guild_state{}) -> {reply, Reply :: term(), NewState :: #guild_state{}}.
handle_call(Request, From, State) ->
    try
        do_call(Request, From, State)
    catch ?EXCEPTION(Class, Reason, Stacktrace) ->
        ?STACKTRACE(Class, Reason, ?GET_STACKTRACE(Stacktrace)),
        {reply, ok, State}
    end.

%% @doc handle_cast
-spec handle_cast(Request :: term(), State :: #guild_state{}) -> {noreply, NewState :: #guild_state{}}.
handle_cast(Request, State) ->
    try
        do_cast(Request, State)
    catch ?EXCEPTION(Class, Reason, Stacktrace) ->
        ?STACKTRACE(Class, Reason, ?GET_STACKTRACE(Stacktrace)),
        {noreply, State}
    end.

%% @doc handle_info
-spec handle_info(Request :: term(), State :: #guild_state{}) -> {noreply, NewState :: #guild_state{}}.
handle_info(Info, State) ->
    try
        do_info(Info, State)
    catch ?EXCEPTION(Class, Reason, Stacktrace) ->
        ?STACKTRACE(Class, Reason, ?GET_STACKTRACE(Stacktrace)),
        {noreply, State}
    end.

%% @doc terminate
-spec terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()), State :: #guild_state{}) -> {ok, NewState :: #guild_state{}}.
terminate(_Reason, State) ->
    try
        guild:server_stop()
    catch ?EXCEPTION(Class, Reason, Stacktrace) ->
        ?STACKTRACE(Class, Reason, ?GET_STACKTRACE(Stacktrace))
    end,
    {ok, State}.

%% @doc code_change
-spec code_change(OldVsn :: (term() | {down, term()}), State :: #guild_state{}, Extra :: term()) -> {ok, NewState :: #guild_state{}}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
do_call({'APPLY_CALL', Function, Args}, _From, State) ->
    case erlang:apply(Function, [State | Args]) of
        {ok, Reply, NewState = #guild_state{}} ->
            {reply, Reply, NewState};
        {ok, NewState = #guild_state{}} ->
            {reply, ok, NewState};
        Reply ->
            {reply, Reply, State}
    end;
do_call({'PURE_CALL', Function, Args}, _From, State) ->
    case erlang:apply(Function, Args) of
        {ok, Reply, NewState = #guild_state{}} ->
            {reply, Reply, NewState};
        {ok, NewState = #guild_state{}} ->
            {reply, ok, NewState};
        Reply ->
            {reply, Reply, State}
    end;
do_call({'APPLY_CALL', Module, Function, Args}, _From, State) ->
    case erlang:apply(Module, Function, [State | Args]) of
        {ok, Reply, NewState = #guild_state{}} ->
            {reply, Reply, NewState};
        {ok, NewState = #guild_state{}} ->
            {reply, ok, NewState};
        Reply ->
            {reply, Reply, State}
    end;
do_call({'PURE_CALL', Module, Function, Args}, _From, State) ->
    case erlang:apply(Module, Function, Args) of
        {ok, Reply, NewState = #guild_state{}} ->
            {reply, Reply, NewState};
        {ok, NewState = #guild_state{}} ->
            {reply, ok, NewState};
        Reply ->
            {reply, Reply, State}
    end;
do_call({create, RoleId, RoleName, Sex, Avatar, Classes, Level, VipLevel, Type, GuildName}, _From, State) ->
    Reply = guild:create(RoleId, RoleName, Sex, Avatar, Classes, Level, VipLevel, Type, GuildName),
    {reply, Reply, State};

do_call({apply, GuildId, RoleId, RoleName, Sex, Avatar, Classes, Level, VipLevel}, _From, State) ->
    Reply = guild:apply(GuildId, RoleId, RoleName, Sex, Avatar, Classes, Level, VipLevel),
    {reply, Reply, State};

do_call({cancel_apply, GuildId, RoleId}, _From, State) ->
    Reply = guild:cancel_apply(GuildId, RoleId),
    {reply, Reply, State};

do_call({cancel_all_apply, RoleId}, _From, State) ->
    Reply = guild:cancel_all_apply(RoleId),
    {reply, Reply, State};

do_call({approve_apply, LeaderRoleId, MemberId}, _From, State) ->
    Reply = guild:approve_apply(LeaderRoleId, MemberId),
    {reply, Reply, State};

do_call({approve_all_apply, LeaderRoleId}, _From, State) ->
    Reply = guild:approve_all_apply(LeaderRoleId),
    {reply, Reply, State};

do_call({reject_apply, LeaderRoleId, MemberId}, _From, State) ->
    Reply = guild:reject_apply(LeaderRoleId, MemberId),
    {reply, Reply, State};

do_call({reject_all_apply, LeaderRoleId}, _From, State) ->
    Reply = guild:reject_all_apply(LeaderRoleId),
    {reply, Reply, State};

do_call({leave, MemberId}, _From, State) ->
    Reply = guild:leave(MemberId),
    {reply, Reply, State};

do_call({dismiss, LeaderRoleId}, _From, State) ->
    Reply = guild:dismiss(LeaderRoleId),
    {reply, Reply, State};

do_call({kick, LeaderRoleId, MemberId}, _From, State) ->
    Reply = guild:kick(LeaderRoleId, MemberId),
    {reply, Reply, State};

do_call({update_job, LeaderRoleId, MemberId, Job}, _From, State) ->
    Reply = guild:update_job(LeaderRoleId, MemberId, Job),
    {reply, Reply, State};

do_call({upgrade_level, LeaderRoleId}, _From, State) ->
    Reply = guild:upgrade_level(LeaderRoleId),
    {reply, Reply, State};

do_call({change_notice, LeaderRoleId, Notice}, _From, State) ->
    Reply = guild:change_notice(LeaderRoleId, Notice),
    {reply, Reply, State};

do_call(_Request, _From, State) ->
    {reply, ok, State}.

do_cast({'APPLY_CAST', Function, Args}, State) ->
    case erlang:apply(Function, [State | Args]) of
        {ok, NewState = #guild_state{}} ->
            {noreply, NewState};
        _ ->
            {noreply, State}
    end;
do_cast({'PURE_CAST', Function, Args}, State) ->
    case erlang:apply(Function, Args) of
        {ok, NewState = #guild_state{}} ->
            {noreply, NewState};
        _ ->
            {noreply, State}
    end;
do_cast({'APPLY_CAST', Module, Function, Args}, State) ->
    case erlang:apply(Module, Function, [State | Args]) of
        {ok, NewState = #guild_state{}} ->
            {noreply, NewState};
        _ ->
            {noreply, State}
    end;
do_cast({'PURE_CAST', Module, Function, Args}, State) ->
    case erlang:apply(Module, Function, Args) of
        {ok, NewState = #guild_state{}} ->
            {noreply, NewState};
        _ ->
            {noreply, State}
    end;
do_cast(_Request, State) ->
    {noreply, State}.

do_info({loop, Tick}, State) ->
    %% save all data
    _ = Tick div 3 == 0 andalso guild:save() == ok,
    %% 3 times save another secondary data
    erlang:send_after(?MINUTE_MILLISECONDS, self(), {loop, Tick + 1}),
    %% other times do something etc...
    {noreply, State};

do_info(_Info, State) ->
    {noreply, State}.

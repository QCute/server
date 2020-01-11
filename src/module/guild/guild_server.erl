%%%------------------------------------------------------------------
%%% @doc
%%% module guild server
%%% @end
%%%------------------------------------------------------------------
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
    kick/2,
    dismiss/1,
    update_job/3,
    upgrade_level/1,
    devote/2
]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
%% Includes
-include("common.hrl").
-include("user.hrl").
-include("event.hrl").
-include("guild.hrl").
%%%==================================================================
%%% API functions
%%%==================================================================
%% @doc server start
-spec start() -> {ok, Pid :: pid()} | {error, term()}.
start() ->
    process:start(?MODULE).

%% @doc server start
-spec start_link() -> {ok, Pid :: pid()} | {error, term()}.
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
    gen_server:call(?MODULE, Request, 5000).

%% @doc cast
-spec cast(Request :: term()) -> Result :: term().
cast(Request) ->
    gen_server:cast(?MODULE, Request).

%% @doc info
-spec info(Request :: term()) -> Result :: term().
info(Request) ->
    erlang:send(?MODULE, Request).

%% @doc guild list
-spec query_guild() -> atom().
query_guild() ->
    {ok, guild:guild_table()}.

%% @doc role list
-spec query_role(#user{}) -> atom().
query_role(#user{role_id = RoleId}) ->
    {ok, guild:role_table(guild:role_guild_id(RoleId))}.

%% @doc apply list
-spec query_apply(#user{}) -> atom().
query_apply(#user{role_id = RoleId}) ->
    {ok, guild:apply_table(guild:role_guild_id(RoleId))}.

%% @doc self guild info
-spec query_self_guild(#user{}) -> {ok, #guild{}}.
query_self_guild(#user{role_id = RoleId}) ->
    {ok, hd(tool:default(guild:get_guild(guild:role_guild_id(RoleId)), [#guild{}]))}.

%% @doc self role info
-spec query_self_role(#user{}) -> {ok, #guild_role{}}.
query_self_role(#user{role_id = RoleId}) ->
    {ok, hd(tool:default(guild:get_role(guild:role_guild_id(RoleId)), [#guild_role{}]))}.

%% @doc self apply list
-spec query_self_apply(#user{}) -> {ok, [#guild_apply{}]}.
query_self_apply(#user{role_id = RoleId}) ->
    List = ets:lookup(guild:apply_index_table(), RoleId),
    {ok, [hd(ets:lookup(guild:apply_table(GuildId), RoleId)) || {GuildId, _} <- List]}.

%% @doc create guild
-spec create(User :: #user{}, Type :: non_neg_integer(), GuildName :: binary()) -> ok() | error().
create(User, Type, GuildName) ->
    case lists:keyfind(Type, 1, parameter_data:get(guild_create)) of
        {_, Condition} ->
            create_check(User, Type, GuildName, Condition);
        _ ->
            {error, condition_not_found}
    end.

create_check(User, Type, GuildName, Condition) ->
    case user_checker:check(User, Condition) of
        {ok, Cost} ->
            {ok, NewUser} = item:reduce(User, Cost, ?MODULE),
            do_create(NewUser, Type, GuildName);
        _ ->
            {error, condition_not_met}
    end.

do_create(User = #user{role_id = RoleId, role_name = RoleName}, Type, GuildName) ->
    case catch call({create, RoleId, RoleName, Type, GuildName}) of
        {ok, GuildId} ->
            FireUser = user_event:handle(User, #event{name = event_guild_join}),
            notice:broadcast(FireUser, [guild_create, GuildId, GuildName]),
            {ok, ok, FireUser};
        {'EXIT', {timeout, _}} ->
            FireUser = user_event:handle(User, #event{name = event_guild_join}),
            {ok, ok, FireUser};
        Error ->
            Error
    end.

%% @doc apply
-spec apply(User :: #user{}, GuildId :: non_neg_integer()) -> ok() | error().
apply(#user{role_id = RoleId, role_name = RoleName}, GuildId) ->
    call({apply, GuildId, RoleId, RoleName}).

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

%% @doc kick
-spec kick(User :: #user{}, MemberId :: non_neg_integer()) -> ok() | error().
kick(#user{role_id = RoleId}, MemberId) ->
    call({kick, RoleId, MemberId}).

%% @doc dismiss
-spec dismiss(User :: #user{}) -> ok() | error().
dismiss(#user{role_id = RoleId}) ->
    call({dismiss, RoleId}).

%% @doc update job
-spec update_job(User :: #user{}, MemberId :: non_neg_integer(), Job :: non_neg_integer()) -> ok() | error().
update_job(#user{role_id = RoleId}, MemberId, Job) ->
    call({update_job, RoleId, MemberId, Job}).

%% @doc upgrade level
-spec upgrade_level(User :: #user{}) -> ok() | error().
upgrade_level(#user{role_id = RoleId}) ->
    call({upgrade_level, RoleId}).

%% @doc devote
-spec devote(User :: #user{}, Type :: non_neg_integer()) -> ok() | error().
devote(#user{role_id = RoleId}, Type) ->
    call({devote, RoleId, Type}).

%%%==================================================================
%%% gen_server callbacks
%%%==================================================================
init([]) ->
    erlang:process_flag(trap_exit, true),
    guild:server_start().

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
        guild:server_stop()
    catch ?EXCEPTION(_Class, _Reason, _Stacktrace) ->
        ok
    end.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%==================================================================
%%% Internal functions
%%%==================================================================
do_call({'APPLY_CALL', Function, Args}, _From, User) ->
    %% alert !!! call it debug only
    case erlang:apply(Function, [User | Args]) of
        {ok, Reply, NewUser = #user{}} ->
            {reply, Reply, NewUser};
        {ok, NewUser = #user{}} ->
            {reply, ok, NewUser};
        Reply ->
            {reply, Reply, User}
    end;
do_call({'PURE_CALL', Function, Args}, _From, User) ->
    %% alert !!! call it debug only
    case erlang:apply(Function, Args) of
        {ok, Reply, NewUser = #user{}} ->
            {reply, Reply, NewUser};
        {ok, NewUser = #user{}} ->
            {reply, ok, NewUser};
        Reply ->
            {reply, Reply, User}
    end;
do_call({'APPLY_CALL', Module, Function, Args}, _From, User) ->
    %% alert !!! call it debug only
    case erlang:apply(Module, Function, [User | Args]) of
        {ok, Reply, NewUser = #user{}} ->
            {reply, Reply, NewUser};
        {ok, NewUser = #user{}} ->
            {reply, ok, NewUser};
        Reply ->
            {reply, Reply, User}
    end;
do_call({'PURE_CALL', Module, Function, Args}, _From, User) ->
    %% alert !!! call it debug only
    case erlang:apply(Module, Function, Args) of
        {ok, Reply, NewUser = #user{}} ->
            {reply, Reply, NewUser};
        {ok, NewUser = #user{}} ->
            {reply, ok, NewUser};
        Reply ->
            {reply, Reply, User}
    end;
do_call({create, RoleId, UserName, Level, GuildName}, _From, State) ->
    Reply = guild:create(RoleId, UserName, Level, GuildName),
    {reply, Reply, State};

do_call({apply, GuildId, RoleId, RoleName}, _From, State) ->
    Reply = guild:apply(GuildId, RoleId, RoleName),
    {reply, Reply, State};

do_call({cancel_apply, GuildId, RoleId}, _From, State) ->
    Reply = guild:cancel_apply(GuildId, RoleId),
    {reply, Reply, State};

do_call({cancel_all_apply, RoleId}, _From, State) ->
    Reply = guild:cancel_all_apply(RoleId),
    {reply, Reply, State};

do_call({approve_apply, LeaderId, MemberId}, _From, State) ->
    Reply = guild:approve_apply(LeaderId, MemberId),
    {reply, Reply, State};

do_call({approve_all_apply, LeaderId}, _From, State) ->
    Reply = guild:approve_all_apply(LeaderId),
    {reply, Reply, State};

do_call({reject_apply, LeaderId, MemberId}, _From, State) ->
    Reply = guild:reject_apply(LeaderId, MemberId),
    {reply, Reply, State};

do_call({reject_all_apply, LeaderId}, _From, State) ->
    Reply = guild:reject_all_apply(LeaderId),
    {reply, Reply, State};

do_call({leave, MemberId}, _From, State) ->
    Reply = guild:leave(MemberId),
    {reply, Reply, State};

do_call({kick, LeaderId, MemberId}, _From, State) ->
    Reply = guild:kick(LeaderId, MemberId),
    {reply, Reply, State};

do_call({dismiss, LeaderId}, _From, State) ->
    Reply = guild:dismiss(LeaderId),
    {reply, Reply, State};

do_call({update_job, LeaderId, MemberId, Job}, _From, State) ->
    Reply = guild:update_job(LeaderId, MemberId, Job),
    {reply, Reply, State};

do_call(_Request, _From, State) ->
    {reply, ok, State}.

do_cast({'APPLY_CAST', Function, Args}, User) ->
    case erlang:apply(Function, [User | Args]) of
        {ok, NewUser = #user{}} ->
            {noreply, NewUser};
        _ ->
            {noreply, User}
    end;
do_cast({'PURE_CAST', Function, Args}, User) ->
    case erlang:apply(Function, Args) of
        {ok, NewUser = #user{}} ->
            {noreply, NewUser};
        _ ->
            {noreply, User}
    end;
do_cast({'APPLY_CAST', Module, Function, Args}, User) ->
    case erlang:apply(Module, Function, [User | Args]) of
        {ok, NewUser = #user{}} ->
            {noreply, NewUser};
        _ ->
            {noreply, User}
    end;
do_cast({'PURE_CAST', Module, Function, Args}, User) ->
    case erlang:apply(Module, Function, Args) of
        {ok, NewUser = #user{}} ->
            {noreply, NewUser};
        _ ->
            {noreply, User}
    end;
do_cast(_Request, State) ->
    {noreply, State}.

do_info(loop, State = #guild_state{tick = Tick}) when Tick div 3 == 0 ->
    %% 3 times save another secondary data
    erlang:send_after(?MINUTE_MILLISECONDS, self(), loop),
    %% save all data
    guild:save(),
    {noreply, State#guild_state{tick = Tick + 1}};

do_info(loop, State = #guild_state{tick = Tick}) ->
    %% other times do something etc...
    erlang:send_after(?MINUTE_MILLISECONDS, self(), loop),
    {noreply, State#guild_state{tick = Tick + 1}};

do_info(_Info, State) ->
    {noreply, State}.
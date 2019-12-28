%%%------------------------------------------------------------------
%%% @doc
%%% module guild server
%%% @end
%%%------------------------------------------------------------------
-module(guild_server).
-behaviour(gen_server).
%% API
-export([call/1, cast/1, info/1]).
-export([start/0, start_link/0]).
-export([
    %% query
    query_guild/0,
    query_role/1,
    query_apply/1,
    query_self_guild/1,
    query_self_role/1,
    query_self_apply/1,
    %% operation
    create/3
]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
%% Includes
-include("common.hrl").
-include("guild.hrl").
-include("user.hrl").
-include("event.hrl").
%%%==================================================================
%%% API functions
%%%==================================================================
%% @doc call
-spec call(Request :: term()) -> Result :: term().
call(Request) ->
    process:call(?MODULE, Request).

%% @doc cast
-spec cast(Request :: term()) -> Result :: term().
cast(Request) ->
    process:cast(?MODULE, Request).

%% @doc info
-spec info(Request :: term()) -> Result :: term().
info(Request) ->
    process:info(?MODULE, Request).

%% @doc server start
-spec start() -> {ok, Pid :: pid()} | {error, term()}.
start() ->
    process:start(?MODULE).

%% @doc server start
-spec start_link() -> {ok, Pid :: pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

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
-spec create(User :: #user{}, Type :: non_neg_integer(), GuildName :: binary()) -> {update, #user{}} | error().
create(User = #user{role_id = RoleId, role_name = RoleName}, Type, GuildName) ->
    Param = parameter_data:get({guild_create, Type}),
    case user_checker:check(User, Param) of
        {ok, _} ->
            case call({create, RoleId, RoleName, Type, GuildName}) of
                {ok, GuildId} ->
                    {ok, CostUser} = asset:cost(User, Param),
                    FireUser = user_event:handle(CostUser, #event_guild_join{}),
                    notice:broadcast(FireUser, [guild_create, GuildId, GuildName]),
                    {update, FireUser};
                Error ->
                    Error
            end;
        Error ->
            Error
    end.

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
do_call({create, RoleId, UserName, Level, GuildName}, _From, State) ->
    Reply = guild:create(RoleId, UserName, Level, GuildName),
    {reply, Reply, State};

do_call(_Request, _From, State) ->
    {reply, ok, State}.

do_cast({apply, GuildId, RoleId, Name}, State) ->
    guild:apply(GuildId, RoleId, Name),
    {noreply, State};

do_cast({cancel_apply, GuildId, RoleId}, State) ->
    guild:cancel_apply(GuildId, RoleId),
    {noreply, State};

do_cast({approve, LeaderId, MemberId}, State) ->
    guild:approve(LeaderId, MemberId),
    {noreply, State};

do_cast({approve_all, LeaderId}, State) ->
    guild:approve_all(LeaderId),
    {noreply, State};

do_cast({reject, LeaderId, MemberId}, State) ->
    guild:reject(LeaderId, MemberId),
    {noreply, State};

do_cast({reject_all, LeaderId}, State) ->
    guild:reject_all(LeaderId),
    {noreply, State};

do_cast({leave, MemberId}, State) ->
    guild:leave(MemberId),
    {noreply, State};

do_cast({dismiss, LeaderId}, State) ->
    guild:dismiss(LeaderId),
    {noreply, State};

do_cast({update_job, LeaderId, MemberId, Job}, State) ->
    guild:update_job(LeaderId, MemberId, Job),
    {noreply, State};

do_cast(_Request, State) ->
    {noreply, State}.

do_info(loop, State = #guild_state{tick = Tick}) when Tick div 6 == 0 ->
    %% 6 times save another secondary data
    erlang:send_after(?MINUTE_MILLISECONDS, self(), loop),
    guild:server_stop(),
    {noreply, State#guild_state{tick = Tick + 1}};

do_info(loop, State = #guild_state{tick = Tick}) ->
    %% other times do something etc...
    erlang:send_after(?MINUTE_MILLISECONDS, self(), loop),
    {noreply, State#guild_state{tick = Tick + 1}};

do_info(_Info, State) ->
    {noreply, State}.
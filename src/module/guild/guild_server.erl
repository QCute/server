%%%-------------------------------------------------------------------
%%% @doc
%%% module guild server
%%% @end
%%%-------------------------------------------------------------------
-module(guild_server).
-behaviour(gen_server).
-compile(nowarn_deprecated_function).
%% API
-export([call/1, cast/1, info/1]).
-export([start/0, start_link/0]).
-export([
    create/3
]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
%% Includes
-include("common.hrl").
-include("guild.hrl").
-include("user.hrl").
-include("role.hrl").
-include("event.hrl").
%%%===================================================================
%%% API
%%%===================================================================
%% @doc call
call(Request) ->
    gen_server:call(process:pid(?MODULE), Request).

%% @doc cast
cast(Request) ->
    gen_server:cast(process:pid(?MODULE), Request).

%% @doc info
info(Request) ->
    erlang:send(process:pid(?MODULE), Request).


%% @doc server start
start() ->
    process:start(?MODULE).

%% @doc server start
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc create guild
-spec create(User :: #user{}, Type :: non_neg_integer(), GuildName :: binary()) -> {update, #user{}} | error().
create(User = #user{id = UserId, name = UserName}, Type, GuildName) ->
    Param = data_parameter:get({guild_create, Type}),
    case role_condition:check(User, Param) of
        ok ->
            Args = {UserId, UserName, Type, GuildName},
            case call({'create', Args}) of
                {ok, ClubId} ->
                    {ok, CostUser} = role_assets:cost(User, Param),
                    FireUser = role_event:handle(CostUser, #event_guild_create{}),
                    notice:broadcast(FireUser, [guild_create, ClubId, GuildName]),
                    {update, FireUser};
                Error ->
                    Error
            end;
        Error ->
            Error
    end.


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
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

%%%===================================================================
%%% Internal functions
%%%===================================================================
do_call({'create', {UserId, UserName, Level, GuildName}}, _From, State) ->
    Reply = guild:create(UserId, UserName, Level, GuildName),
    {reply, Reply, State};

do_call(_Request, _From, State) ->
    {reply, ok, State}.

do_cast({apply, GuildId, UserId, Name, Pid, SenderPid}, State) ->
    guild:apply(GuildId, UserId, Name, Pid, SenderPid),
    {noreply, State};

do_cast({cancel_apply, GuildId, UserId}, State) ->
    guild:cancel_apply(GuildId, UserId),
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

do_cast({job_update, LeaderId, MemberId, Job}, State) ->
    guild:job_update(LeaderId, MemberId, Job),
    {noreply, State};

do_cast(_Request, State) ->
    {noreply, State}.

do_info(loop, State = #guild_state{tick = Tick, timeout = Timeout}) when Tick div 6 == 0 ->
    %% 6 times save another secondary data
    erlang:send_after(Timeout, self(), loop),
    guild:server_stop(),
    {noreply, State#guild_state{tick = Tick + 1}};

do_info(loop, State = #guild_state{tick = Tick, timeout = Timeout}) ->
    %% other times do something etc...
    erlang:send_after(Timeout, self(), loop),
    {noreply, State#guild_state{tick = Tick + 1}};

do_info(_Info, State) ->
    {noreply, State}.
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
    create/3,
    broadcast/2,
    broadcast/3,
    player_guild_id/1,
    player_status/1
]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
%% Includes
-include("common.hrl").
-include("guild.hrl").
-include("user.hrl").
-include("player.hrl").
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

%% @doc send data to local server all online player
-spec broadcast(GuildId :: non_neg_integer(), Data :: binary()) -> ok.
broadcast(GuildId, Data) ->
    parser:foreach(fun(#guild_player{guild_id = G, player_sender_pid = Pid}) when G == GuildId -> player_sender:send(Pid, Data); (_) -> ok end, guild_player).
-spec broadcast(GuildId :: non_neg_integer(), Data :: binary(), ExceptId :: non_neg_integer()) -> ok.
broadcast(GuildId, Data, ExceptId) ->
    parser:foreach(fun(#guild_player{guild_id = G, player_id = Id, player_sender_pid = Pid}) when G == GuildId andalso Id =/= ExceptId -> player_sender:send(Pid, Data); (_) -> ok end, guild_player).

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
    PlayerStatus = player_status(UserId),
    case player_condition:check(User, Param) of
        ok when PlayerStatus == none orelse PlayerStatus == ever ->
            Args = {UserId, UserName, Type, GuildName},
            case call({'create', Args}) of
                {ok, ClubId} ->
                    {ok, CostUser} = player_assets:cost(User, Param),
                    FireUser = player_event:handle(CostUser, #event_guild_create{}),
                    notice:broadcast(FireUser, [guild_create, ClubId, GuildName]),
                    {update, FireUser};
                Error ->
                    Error
            end;
        Error ->
            Error
    end.

%% @doc player guild status
-spec player_guild_id(UserId :: non_neg_integer()) -> non_neg_integer().
player_guild_id(UserId) ->
    case ets:lookup(guild_player, UserId) of
        #guild_player{guild_id = GuildId} ->
            GuildId;
        _ ->
            0
    end.

%% @doc player guild status
-spec player_status(UserId :: non_neg_integer()) -> none | ever | joined | bad.
player_status(UserId) ->
    case ets:lookup(guild_player, UserId) of
        [] ->
            none;
        #guild_player{guild_id = 0} ->
            ever;
         #guild_player{guild_id = GuildId} when GuildId > 0 ->
            joined;
        _ ->
            bad
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
do_call(_Request, _From, State) ->
    {reply, ok, State}.

do_cast(_Request, State) ->
    {noreply, State}.

do_info(_Info, State) ->
    {noreply, State}.
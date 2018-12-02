%%%-------------------------------------------------------------------
%%% @doc
%%% module guild server
%%% @end
%%%-------------------------------------------------------------------
-module(guild_server).
-compile(nowarn_deprecated_function).
-include("common.hrl").
-include("guild.hrl").
-include("player.hrl").
-include("trigger.hrl").
-behaviour(gen_server).
%% API
-export([call/1, cast/1, info/1]).
-export([start/0, start_link/0]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
%% api
-export([
	create/3
]).

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

%% @doc 创建帮派
create(User = #user{id = UserId, name = UserName, nick = UserNick}, Type, Name) ->
	Param = data_guild:param(create, Type),
	NameValid = word:valid(Name),
	GuildPlayer = ets:lookup(guild_player, UserId),
	case player_condition:check(User, Param) of
		true when NameValid andalso GuildPlayer == [] ->
			Args = {UserId, UserName, UserNick, Type, Name},
			case call({'create', Args}) of
				{ok, ClubId} ->
					{ok, CostUser} = player:cost(User, Param),
					FireUser = player_trigger:fire(CostUser, #event_guild_create{}),
					notice:broadcast(FireUser, [notice, world, ClubId, Name]),
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
    guild:server_start().

handle_call(Request, From, State) ->
    ?STACK_TRACE(do_call(Request, From, State), {reply, ok, State}).

handle_cast(Request, State) ->
    ?STACK_TRACE(do_cast(Request, State), {noreply, State}).

handle_info(Info, State) ->
    ?STACK_TRACE(do_info(Info, State), {noreply, State}).

terminate(_Reason, _State) ->
    catch guild:server_stop(),
    ok.

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
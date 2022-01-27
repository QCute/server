%%%-------------------------------------------------------------------
%%% @doc
%%% chat server
%%% @end
%%%-------------------------------------------------------------------
-module(chat_server).
-behaviour(gen_server).
%% API
-export([start/0, start_link/0]).
-export([chat_system/1, chat_world/1, chat_guild/1, chat_private/1]).
-export([get_system_list/1, get_world_list/1, get_guild_list/2, get_private_list/3]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
%% Includes
-include("common.hrl").
-include("journal.hrl").
-include("user.hrl").
-include("guild.hrl").
-include("chat.hrl").
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

%% @doc chat system
-spec chat_system(SystemChat :: #system_chat{}) -> ok.
chat_system(SystemChat) ->
    %% save
    ets:insert(?SYSTEM_CHAT, SystemChat),
    %% strip
    _ = parameter_data:get(chat_system_size_limit) < ets:info(?SYSTEM_CHAT, size) andalso ets:delete(?SYSTEM_CHAT, ets:first(?SYSTEM_CHAT)) == true,
    ok.

%% @doc chat world
-spec chat_world(WorldChat :: #world_chat{}) -> ok.
chat_world(WorldChat) ->
    %% save
    ets:insert(?WORLD_CHAT, WorldChat),
    %% strip
    _ = parameter_data:get(chat_world_size_limit) < ets:info(?WORLD_CHAT, size) andalso ets:delete(?WORLD_CHAT, ets:first(?WORLD_CHAT)) == true,
    ok.

%% @doc chat guild
-spec chat_guild(GuildChat :: #guild_chat{}) -> ok.
chat_guild(GuildChat = #guild_chat{guild_id = GuildId}) ->
    %% save
    NewList = [GuildChat | ess:lookup_element(?GUILD_CHAT, GuildId, 2)],
    %% strip
    StripList = lists:sublist(NewList, parameter_data:get(chat_guild_size_limit)),
    %% insert or update
    ets:insert(?GUILD_CHAT, {GuildId, StripList}),
    ok.

%% @doc chat private
-spec chat_private(PrivateChat :: #private_chat{}) -> ok.
chat_private(PrivateChat = #private_chat{sender_id = SenderId, receiver_id = ReceiverId}) ->
    Key = {min(SenderId, ReceiverId), max(SenderId, ReceiverId)},
    %% save
    NewList = [PrivateChat | ess:lookup_element(?PRIVATE_CHAT, Key, 2)],
    %% strip
    StripList = lists:sublist(NewList, parameter_data:get(chat_private_size_limit)),
    %% insert or update
    ets:insert(?PRIVATE_CHAT, {Key, StripList}),
    ok.

%% @doc get system chat history
-spec get_system_list(Page :: non_neg_integer()) -> ok().
get_system_list(Page) ->
    {ok, ess:page(?SYSTEM_CHAT, Page, 10)}.

%% @doc get world chat history
-spec get_world_list(Page :: non_neg_integer()) -> ok().
get_world_list(Page) ->
    {ok, ess:page(?WORLD_CHAT, Page, 10)}.

%% @doc get guild chat history
-spec get_guild_list(User :: #user{}, Page :: non_neg_integer()) -> ok().
get_guild_list(#user{guild = #guild_role{guild_id = GuildId}}, Page) ->
    {ok, listing:page(ess:lookup_element(?GUILD_CHAT, GuildId, 2), Page, 10)}.

%% @doc get private chat history
-spec get_private_list(User :: #user{}, AnotherRoleId :: non_neg_integer(), Page :: non_neg_integer()) -> ok().
get_private_list(#user{role_id = RoleId}, AnotherRoleId, Page) ->
    {ok, listing:page(ess:lookup_element(?PRIVATE_CHAT, {min(RoleId, AnotherRoleId), max(RoleId, AnotherRoleId)}, 2), Page, 10)}.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
%% @doc init
-spec init(Args :: term()) -> {ok, State :: []}.
init(_) ->
    erlang:process_flag(trap_exit, true),
    ets:new(?SYSTEM_CHAT, [named_table, set, public, {keypos, #system_chat.id}, {write_concurrency, true}, {read_concurrency, true}]),
    ets:new(?WORLD_CHAT, [named_table, set, public, {keypos, #world_chat.id}, {write_concurrency, true}, {read_concurrency, true}]),
    ets:new(?GUILD_CHAT, [named_table, set, public, {write_concurrency, true}, {read_concurrency, true}]),
    ets:new(?PRIVATE_CHAT, [named_table, set, public, {write_concurrency, true}, {read_concurrency, true}]),
    {ok, []}.

%% @doc handle_call
-spec handle_call(Request :: term(), From :: {pid(), Tag :: term()}, State :: []) -> {reply, Reply :: term(), NewState :: []}.
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

%% @doc handle_cast
-spec handle_cast(Request :: term(), State :: []) -> {noreply, NewState :: []}.
handle_cast(_Request, State) ->
    {noreply, State}.

%% @doc handle_info
-spec handle_info(Request :: term(), State :: []) -> {noreply, NewState :: []}.
handle_info(_Info, State) ->
    {noreply, State}.

%% @doc terminate
-spec terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()), State :: []) -> {ok, NewState :: []}.
terminate(_Reason, State) ->
    {ok, State}.

%% @doc code_change
-spec code_change(OldVsn :: (term() | {down, term()}), State :: [], Extra :: term()) -> {ok, NewState :: []}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%%-------------------------------------------------------------------
%%% @doc
%%% module guild
%%% @end
%%%-------------------------------------------------------------------
-module(guild).
-compile(export_all).
-compile(nowarn_export_all).
-export([
    server_start/0,
    server_stop/0
]).
%% includes
-include("common.hrl").
-include("guild.hrl").

%%%===================================================================
%%% API
%%%===================================================================
%% @doc guild server start
server_start() ->
    ets:new(guild, [named_table, {keypos, #guild.guild_id}, {read_concurrency, true}]),
    ets:new(guild_player, [named_table, {keypos, #guild_player.player_id}, {read_concurrency, true}]),
    SaveGuild = fun(X) -> ets:insert(guild, X) end,
    data_tool:load(guild_sql:select(), guild, SaveGuild),
    SaveGuildPlayer = fun(X) -> ets:insert(guild_player, X) end,
    data_tool:load(guild_player_sql:select_join(), guild_player, SaveGuildPlayer),
    {ok, []}.

%% @doc guild server stop
server_stop() ->
    guild_sql:update_into(guild),
    guild_player_sql:update_into(guild_player),
    ok.

%% @doc create
-spec create(UserId :: non_neg_integer(), UserName :: binary() | string(), Level :: non_neg_integer(), GuildName :: binary() | string()) -> {ok, GuildId :: non_neg_integer()} | {error, Code :: non_neg_integer()}.
create(UserId, UserName, Level, GuildName) ->
    Now = time:ts(),
    CdTime = data_parameter:get({guild_create, cd}),
    case ets:lookup(guild_player, UserId) of
        [] ->
            GuildPlayer = #guild_player{player_id = UserId, player_name = UserName, job = 1, join_time = Now, extra = update},
            do_create(UserId, UserName, Level, GuildName, Now, GuildPlayer);
        [OldGuildPlayer = #guild_player{guild_id = 0, leave_time = LeaveTime}] when Now - LeaveTime >= CdTime ->
            GuildPlayer = OldGuildPlayer#guild_player{player_id = UserId, player_name = UserName, job = 1, join_time = Now, extra = update},
            do_create(UserId, UserName, Level, GuildName, Now, GuildPlayer);
        [#guild_player{guild_id = 0, leave_time = LeaveTime}] when Now - LeaveTime < CdTime ->
            {error, 2};
        _ ->
            {error, 3}
    end.
do_create(UserId, UserName, Level, GuildName, Now, GuildPlayer) ->
    case validate_name(GuildName) of
        true ->
            Guild = #guild{guild_name = GuildName, leader_id = UserId, leader_name = UserName, level = Level, create_time = Now},
            GuildId = guild_sql:insert(Guild),
            NewGuild = Guild#guild{guild_id = GuildId},
            ets:insert(guild, NewGuild),
            NewGuildPlayer = GuildPlayer#guild_player{guild_id = GuildId},
            guild_player_sql:update_into([NewGuildPlayer]),
            {ok, GuildId};
        Error ->
            Error
    end.

%% @doc validate guild name
-spec validate_name(String :: binary() | list()) -> true | {false, Reason :: term()} | {false, atom(), Reason :: term()}.
validate_name(GuildName) ->
    Condition = [{length, 1, 6}, sensitive, {sql, io_lib:format("SELECT `guild_id` FROM `guild` WHERE `guild_name` = '~s'", [GuildName])}],
    word:validate(Condition, GuildName).

%% @doc check guild player
-spec check_player(GuildPlayer :: #guild_player{}, List :: list()) -> ok | error.
check_player(_, []) ->
    ok;
check_player(GuildPlayer = #guild_player{guild_id = GuildId}, [{guild_id, GuildId} | T]) ->
    check_player(GuildPlayer, T);
check_player(GuildPlayer = #guild_player{job = Job}, [{job, MinJob} | T]) when MinJob =< Job ->
    check_player(GuildPlayer, T);
check_player(GuildPlayer = #guild_player{job = 1}, [leader | T]) ->
    check_player(GuildPlayer, T);
check_player(GuildPlayer = #guild_player{job = 2}, [vice | T]) ->
    check_player(GuildPlayer, T);
check_player(GuildPlayer = #guild_player{job = 3}, [elite | T]) ->
    check_player(GuildPlayer, T);
check_player(GuildPlayer = #guild_player{job = 4}, [member | T]) ->
    check_player(GuildPlayer, T);
check_player(_, _) ->
    error.

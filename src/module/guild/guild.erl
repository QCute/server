%%%-------------------------------------------------------------------
%%% @doc
%%% module guild
%%% @end
%%%-------------------------------------------------------------------
-module(guild).
-compile(export_all).
-compile(nowarn_export_all).
%% API
-export([
    server_start/0,
    server_stop/0,
    player_guild_id/1,
    player_status/1,
    request/5,
    cancel_request/2,
    approve/2,
    approve_all/1,
    reject/2,
    reject_all/1,
    leave/1,
    kick/2,
    job_update/3
]).
%% Includes
-include("common.hrl").
-include("guild.hrl").
%% Macros
-define(GUILD, guild).
-define(GUILD_PLAYER(GuildId), (player_table(GuildId))).
%%%===================================================================
%%% API
%%%===================================================================
%% @doc guild server start
server_start() ->
    %% guild data
    ets:new(guild, [named_table, {keypos, #guild.guild_id}, {read_concurrency, true}]),
    %% extra 0 guild id player data
    ets:new(player_table(0), [named_table, {keypos, #guild_player.player_id}, {read_concurrency, true}]),
    %% guild
    SaveGuild = fun(X = #guild{guild_id = GuildId}) ->
        %% new player table
        ets:new(player_table(GuildId), [named_table, {keypos, #guild_player.player_id}, {read_concurrency, true}]),
        %% new request table
        ets:new(request_table(GuildId), [named_table, {keypos, #guild_request.player_id}, {read_concurrency, true}]),
        %% save guild data
        ets:insert(guild, X)
    end,
    parser:convert(guild_sql:select(), guild, SaveGuild),
    %% guild player
    SavePlayer = fun(X = #guild_player{guild_id = GuildId}) -> ets:insert(player_table(GuildId), X) end,
    parser:convert(guild_player_sql:select_join(), guild_player, SavePlayer),
    %% guild request
    SaveRequest = fun(X = #guild_request{guild_id = GuildId}) -> ets:insert(request_table(GuildId), X) end,
    parser:convert(guild_request_sql:select_join(), guild_request, SaveRequest),
    %% save timer
    erlang:send_after(?MINUTE_SECONDS * 1000, self(), loop),
    {ok, #guild_state{tick = 0, timeout = ?MINUTE_SECONDS * 1000}}.

%% @doc guild server stop
server_stop() ->
    F = fun([Guild = #guild{guild_id = GuildId}]) ->
        %% save guild
        guild_sql:update(Guild),
        %% save player
        guild_player_sql:update_into(player_table(GuildId)),
        %% save request
        guild_request_sql:update_into(request_table(GuildId)),
        %% change save flag
        Guild#guild{extra = 0}
    end,
    ess:map(F, guild),
    ok.

%% @doc send data to local server all online player
-spec broadcast(GuildId :: non_neg_integer(), Data :: binary()) -> ok.
broadcast(GuildId, Data) ->
    ess:foreach(fun([#guild_player{player_sender_pid = Pid}]) -> player_sender:send(Pid, Data) end, player_table(GuildId)).
-spec broadcast(GuildId :: non_neg_integer(), Data :: binary(), ExceptId :: non_neg_integer()) -> ok.
broadcast(GuildId, Data, ExceptId) ->
    ess:foreach(fun([#guild_player{player_id = Id, player_sender_pid = Pid}]) -> Id =/= ExceptId andalso player_sender:send(Pid, Data) == ok end, player_table(GuildId)).

%% @doc player guild status
-spec player_guild_id(UserId :: non_neg_integer()) -> non_neg_integer().
player_guild_id(UserId) ->
    [#guild_player{guild_id = Id} | _] = ess:first(fun([#guild{guild_id = Id}]) -> ets:lookup(player_table(Id), UserId) end, guild, [#guild_player{}]),
    Id.

%% @doc player guild cd
-spec player_guild_cd(UserId :: non_neg_integer()) -> non_neg_integer().
player_guild_cd(UserId) ->
    GuildId = player_guild_id(UserId),
    case ets:lookup(player_table(GuildId), UserId) of
        [#guild_player{leave_time = LeaveTime}] ->
            LeaveTime;
        _ ->
            0
    end.

%% @doc player guild status
-spec player_status(UserId :: non_neg_integer()) -> none | ever | joined | bad.
player_status(UserId) ->
    case ets:lookup(guild_player, UserId) of
        [] ->
            none;
        [{0, _}] ->
            ever;
        [{GuildId, _}] when GuildId > 0 ->
            joined;
        _ ->
            bad
    end.

%% @doc create
-spec create(UserId :: non_neg_integer(), UserName :: binary() | string(), Level :: non_neg_integer(), GuildName :: binary() | string()) -> {ok, GuildId :: non_neg_integer()} | {error, Code :: non_neg_integer()}.
create(UserId, UserName, Level, GuildName) ->
    Now = time:ts(),
    GuildId = player_guild_id(UserId),
    CdTime = data_parameter:get({guild_create, cd}),
    case ets:lookup(player_table(GuildId), UserId) of
        [] ->
            %% no old guild
            GuildPlayer = #guild_player{player_id = UserId, player_name = UserName, job = 1, join_time = Now, extra = update},
            do_create(UserId, UserName, Level, GuildName, Now, GuildPlayer);
        [OldGuildPlayer = #guild_player{guild_id = 0, leave_time = LeaveTime}] when Now - LeaveTime >= CdTime ->
            %% has old guild but leave cd invalid
            GuildPlayer = OldGuildPlayer#guild_player{player_id = UserId, player_name = UserName, job = 1, join_time = Now, extra = update},
            do_create(UserId, UserName, Level, GuildName, Now, GuildPlayer);
        [#guild_player{guild_id = 0, leave_time = LeaveTime}] when Now - LeaveTime < CdTime ->
            %% has old guild and leave cd valid
            {error, 2};
        _ ->
            {error, 3}
    end.
do_create(UserId, UserName, Level, GuildName, Now, GuildPlayer) ->
    case validate_name(GuildName) of
        true ->
            %% save guild
            Guild = #guild{guild_name = GuildName, leader_id = UserId, leader_name = UserName, level = Level, create_time = Now},
            GuildId = guild_sql:insert(Guild),
            NewGuild = Guild#guild{guild_id = GuildId},
            ets:insert(guild, NewGuild),
            %% save guild player
            NewGuildPlayer = GuildPlayer#guild_player{guild_id = GuildId},
            guild_player_sql:update_into([NewGuildPlayer]),
            Table = player_table(GuildId),
            ets:new(Table, [named_table, {keypos, #guild_player.player_id}, {read_concurrency, true}]),
            ets:insert(Table, NewGuildPlayer),
            %% new request table
            ets:new(request_table(GuildId), [named_table, {keypos, #guild_request.player_id}, {read_concurrency, true}]),
            {ok, GuildId};
        {false, length, _} ->
            {error, 4};
        {false, asn1, _} ->
            {error, 5};
        {false, duplicate} ->
            {error, 6}
    end.

%% @doc request
-spec request(GuildId :: non_neg_integer(),  UserId :: non_neg_integer(), Name :: binary(), Pid :: pid(), SenderPid :: pid()) -> ok | {error, non_neg_integer()}.
request(GuildId, UserId, Name, Pid, SenderPid) ->
    OldGuildId = player_guild_id(UserId),
    case ets:lookup(guild, GuildId) of
        [#guild{}] when OldGuildId == 0 ->
            Request = #guild_request{
                guild_id = GuildId,
                player_id = UserId,
                player_name = Name,
                player_pid = Pid,
                sender_pid = SenderPid,
                extra = {UserId, GuildId},
                flag = insert
            },
            ets:insert(request_table(GuildId), Request),
            %% @todo notify msg to leader
            ok;
        [] ->
            {error, 2};
        _ ->
            {error, 3}
    end.

%% @doc cancel request
-spec cancel_request(GuildId :: non_neg_integer(), UserId :: non_neg_integer()) -> ok.
cancel_request(GuildId, UserId) ->
    %% delete db data
    guild_request_sql:delete(UserId, GuildId),
    %% clear ets request data
    ets:delete(request_table(GuildId), UserId),
    ok.

%% @doc approve request
-spec approve(LeaderId :: non_neg_integer(), MemberId :: non_neg_integer()) -> ok | {error, non_neg_integer()}.
approve(LeaderId, MemberId) ->
    GuildId = player_guild_id(LeaderId),
    Table = player_table(GuildId),
    case ets:lookup(guild, GuildId) of
        [#guild{level = Level}] ->
            case ets:lookup(Table, LeaderId) of
                [#guild_player{job = Job}] when Job == 1 orelse Job == 2 ->
                    Limit = data_parameter:get({guild_member, limit, Level}),
                    case ets:lookup(request_table(GuildId), MemberId) of
                        [Request = #guild_request{}] ->
                            approve_join_check(GuildId, Limit, Table, Request);
                        _ ->
                            {error, 5}
                    end;

                _ ->
                    {error, 3}
            end;
        _ ->
            {error, 2}
    end.

approve_join_check(GuildId, Limit, Table, Request = #guild_request{player_id = PlayerId}) ->
    case ets:info(Table, size) < Limit of
        true ->
            OldGuildId = player_guild_id(PlayerId),
            case ets:lookup(player_table(OldGuildId), PlayerId) of
                [Player = #guild_player{guild_id = 0}] ->
                    join(Table, GuildId, Player, Request);
                [] ->
                    join(Table, GuildId, #guild_player{}, Request);
                _ ->
                    {error, 6}
            end;
        _ ->
            {error, 4}
    end.

%% request info to player info
join(Table, GuildId, Player, Request) ->
    #guild_request{player_id = PlayerId, player_name = PlayerName, player_pid = Pid, sender_pid = SenderPid} = Request,
    NewPlayer = Player#guild_player{
        guild_id = GuildId,
        player_id = PlayerId,
        job = 4,
        player_name = PlayerName,
        player_pid = Pid,
        player_sender_pid = SenderPid,
        extra = update
    },
    %% save new player
    ets:insert(Table, NewPlayer),
    %% delete old player if exists
    ets:delete(guild_player_0, PlayerId),
    %% clear db data
    guild_request_sql:delete_player(PlayerId),
    %% clear all old request
    ess:foreach(fun([#guild{guild_id = Id}]) -> ets:delete(request_table(Id), PlayerId) end, guild),
    %% @todo broadcast join msg
    ok.

%% @doc approve all request
-spec approve_all(LeaderId :: non_neg_integer()) -> ok | {error, non_neg_integer()}.
approve_all(LeaderId) ->
    GuildId = player_guild_id(LeaderId),
    Table = player_table(GuildId),
    case ets:lookup(guild, GuildId) of
        [#guild{level = Level}] ->
            case ets:lookup(Table, LeaderId) of
                [#guild_player{job = Job}] when Job == 1 orelse Job == 2 ->
                    Limit = data_parameter:get({guild_member, limit, Level}),
                    RequestTable = request_table(GuildId),
                    ess:first(fun([Request]) -> approve_join_check(GuildId, Limit, Table, Request) == ok end, RequestTable),
                    ets:delete_all_objects(RequestTable);
                _ ->
                    {error, 3}
            end;
        _ ->
            {error, 2}
    end.

%% @doc reject request
-spec reject(LeaderId :: non_neg_integer(), MemberId :: non_neg_integer()) -> ok | {error, non_neg_integer()}.
reject(LeaderId, MemberId) ->
    GuildId = player_guild_id(LeaderId),
    Table = player_table(GuildId),
    case ets:lookup(Table, LeaderId) of
        [#guild_player{job = Job}] when Job == 1 orelse Job == 2 ->
            %% delete db data
            guild_request_sql:delete(MemberId, GuildId),
            %% clear ets request data
            ets:delete(request_table(GuildId), MemberId),
            ok;
        [#guild_player{}] ->
            {error, 2};
        _ ->
            {error, 3}
    end.

%% @doc reject all request
-spec reject_all(LeaderId :: non_neg_integer()) -> ok | {error, non_neg_integer()}.
reject_all(LeaderId) ->
    GuildId = player_guild_id(LeaderId),
    Table = player_table(GuildId),
    case ets:lookup(Table, LeaderId) of
        [#guild_player{job = Job}] when Job == 1 orelse Job == 2 ->
            %% delete db data
            guild_request_sql:delete_guild(GuildId),
            %% clear ets request data
            ets:delete_all_objects(request_table(GuildId)),
            ok;
        [#guild_player{}] ->
            {error, 2};
        _ ->
            {error, 3}
    end.

%% @doc leave
-spec leave(UserId :: non_neg_integer()) -> ok | {error, non_neg_integer()}.
leave(UserId) ->
    GuildId = player_guild_id(UserId),
    Table = player_table(GuildId),
    case ets:lookup(Table, UserId) of
        [#guild_player{job = 1}] ->
            %% leader leave dismiss it
            do_dismiss(Table, GuildId);
        [Player = #guild_player{}] ->
            %% @todo broadcast leave msg
            NewPlayer = Player#guild_player{guild_id = 0, leave_time = time:ts(), extra = update},
            ets:insert(Table, NewPlayer),
            ok;
        _ ->
            {error, 2}
    end.

%% @doc dismiss
-spec dismiss(LeaderId :: non_neg_integer()) -> ok | {error, non_neg_integer()}.
dismiss(LeaderId) ->
    GuildId = player_guild_id(LeaderId),
    Table = player_table(GuildId),
    case ets:lookup(Table, LeaderId) of
        [#guild_player{job = 1}] ->
            do_dismiss(Table, GuildId);
        [#guild_player{}] ->
            {error, 2};
        _ ->
            {error, 3}
    end.

do_dismiss(Table, GuildId) ->
    %% delete db guild data
    guild_sql:delete(GuildId),
    Now = time:ts(),
    %% @todo broadcast dismiss msg
    ess:map(fun([X]) -> X#guild_player{guild_id = 0, job = 0, leave_time = Now, extra = update} end, Table),
    %% delete db data
    guild_request_sql:delete_guild(GuildId),
    %% clear ets request data
    ets:delete(request_table(GuildId)),
    ok.

%% @doc kick
-spec kick(LeaderId :: non_neg_integer(), MemberId :: non_neg_integer()) -> ok | {error, non_neg_integer()}.
kick(LeaderId, MemberId) ->
    GuildId = player_guild_id(LeaderId),
    Table = player_table(GuildId),
    case ets:lookup(Table, LeaderId) of
        [#guild_player{job = LeaderJob}] when LeaderJob == 1 orelse LeaderJob == 2 ->
            case ets:lookup(Table, MemberId) of
                [Player = #guild_player{job = MemberJob}] when LeaderJob =/= MemberJob ->
                    %% @todo broadcast be kick msg
                    NewPlayer = Player#guild_player{guild_id = 0, leave_time = time:ts(), extra = update},
                    ets:insert(Table, NewPlayer),
                    ok;
                [#guild_player{}] ->
                    {error, 4};
                _ when LeaderId =/= MemberId ->
                    {error, 5};
                _ ->
                    {error, 6}
            end;
        [#guild_player{}] ->
            {error, 2};
        _ ->
            {error, 3}
    end.

-spec job_update(LeaderId :: non_neg_integer(), MemberId :: non_neg_integer(), Job :: pos_integer()) -> ok | {error, non_neg_integer()}.
job_update(LeaderId, MemberId, Job) ->
    GuildId = player_guild_id(LeaderId),
    Table = player_table(GuildId),
    case ets:lookup(Table, LeaderId) of
        [#guild_player{job = LeaderJob}] when Job < LeaderJob ->
            case ets:lookup(Table, MemberId) of
                [Player = #guild_player{job = MemberJob}] when MemberJob < LeaderJob ->
                    NewPlayer = Player#guild_player{job = Job, extra = update},
                    ets:insert(Table, NewPlayer),
                    %% @todo broadcast be job update msg
                    ok;
                [#guild_player{}] ->
                    {error, 4};
                _ ->
                    {error, 5}
            end;
        [#guild_player{}] ->
            {error, 2};
        _ ->
            {error, 3}
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
%%%===================================================================
%%% Internal functions
%%%===================================================================
%% guild player ets name
player_table(GuildId) ->
    type:to_atom(lists:concat([guild_player_, GuildId])).

%% guild request ets name
request_table(GuildId) ->
    type:to_atom(lists:concat([guild_request_, GuildId])).

%% find guild
get_guild(GuildId) ->
    ets:lookup(guild, GuildId).

%% find player
get_player(PlayerId) ->
    GuildId = player_guild_id(PlayerId),
    ets:lookup(player_table(GuildId), PlayerId).

%% find request
get_request(PlayerId) ->
    GuildId = player_guild_id(PlayerId),
    ets:lookup(request_table(GuildId), PlayerId).
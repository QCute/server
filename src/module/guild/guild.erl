%%%------------------------------------------------------------------
%%% @doc
%%% module guild
%%% @end
%%%------------------------------------------------------------------
-module(guild).
%% API
-export([
    server_start/0,
    server_stop/0,
    %% operation
    create/4,
    apply/3,
    cancel_apply/2,
    approve/2,
    approve_all/1,
    reject/2,
    reject_all/1,
    leave/1,
    dismiss/1,
    kick/2,
    update_job/3,
    %% assist
    broadcast/2,
    broadcast/3,
    role_guild_id/1,
    %% table
    guild_table/0,
    role_index_table/0,
    role_table/1,
    apply_index_table/0,
    apply_table/1,
    get_guild/1,
    get_role/1,
    get_role/2,
    get_apply/1,
    get_apply/2
]).

%% Includes
-include("common.hrl").
-include("guild.hrl").
-include_lib("stdlib/include/ms_transform.hrl").

%%%==================================================================
%%% API functions
%%%==================================================================
%% @doc guild server start
-spec server_start() -> ok.
server_start() ->
    %% guild
    ets:new(guild_table(), [named_table, set, {keypos, #guild.guild_id}, {read_concurrency, true}]),
    %% save guild
    SaveGuild = fun(Guild = #guild{guild_id = GuildId}) ->
        %% save guild data
        ets:insert(guild_table(), Guild),
        %% new role table
        ets:new(role_table(GuildId), [named_table, set, {keypos, #guild_role.role_id}, {read_concurrency, true}]),
        %% new apply table
        ets:new(apply_table(GuildId), [named_table, set, {keypos, #guild_apply.role_id}, {read_concurrency, true}])
    end,
    parser:convert(guild_sql:select(), guild, SaveGuild),
    %% set guild leader digest info
    SetLeader = fun
        (#guild_role{guild_id = GuildId, role_id = RoleId, role_name = RoleName, job = ?GUILD_JOB_LEADER}) ->
            [Guild] = ets:lookup(guild_table(), GuildId),
            NewGuild = Guild#guild{leader_id = RoleId, leader_name = RoleName},
            ets:insert(guild_table(), NewGuild);
        (_) ->
            ok
    end,
    %% guild role
    SaveRole = fun(X = #guild_role{guild_id = GuildId, role_id = RoleId}) -> ets:insert(role_table(GuildId), X), SetLeader(X), {GuildId, RoleId} end,
    GuildRoleIndexList = parser:convert(guild_role_sql:select_join(), guild_role, SaveRole),
    ets:new(role_index_table(), [named_table, set, {keypos, 2}, {read_concurrency, true}]),
    ets:insert(role_index_table(), GuildRoleIndexList),
    %% guild apply
    SaveApply = fun(X = #guild_apply{guild_id = GuildId, role_id = RoleId}) -> ets:insert(apply_table(GuildId), X), {GuildId, RoleId} end,
    GuildApplyIndexList = parser:convert(guild_apply_sql:select_join(), guild_apply, SaveApply),
    ets:new(apply_index_table(), [named_table, bag, {keypos, 2}, {read_concurrency, true}]),
    ets:insert(apply_index_table(), GuildApplyIndexList),
    %% save timer
    erlang:send_after(?MINUTE_MILLISECONDS, self(), loop),
    {ok, #guild_state{tick = 0}}.

%% @doc guild server stop
-spec server_stop() -> ok.
server_stop() ->
    guild_table(),
    F = fun([Guild = #guild{guild_id = GuildId}]) ->
        %% save guild
        %% guild_sql:update(Guild),
        %% save role
        guild_role_sql:insert_update(role_table(GuildId)),
        %% save apply
        guild_apply_sql:insert_update(apply_table(GuildId)),
        %% change save flag
        Guild#guild{flag = 0}
    end,
    ess:map(F, guild_table()),
    %% save guild
    guild_role_sql:insert_update(guild_table()),
    ok.

%% @doc create
-spec create(RoleId :: non_neg_integer(), UserName :: binary() | string(), Level :: non_neg_integer(), GuildName :: binary() | string()) -> {ok, GuildId :: non_neg_integer()} | {error, Code :: non_neg_integer()}.
create(RoleId, UserName, Level, GuildName) ->
    Now = time:ts(),
    GuildId = role_guild_id(RoleId),
    %% CdTime = parameter_data:get({guild_create, cd}),
    case ets:lookup(role_table(GuildId), RoleId) of
        [] ->
            %% no old guild
            GuildRole = #guild_role{role_id = RoleId, role_name = UserName, job = ?GUILD_JOB_MEMBER, join_time = Now, flag = update},
            %% do_create(RoleId, UserName, Level, GuildName, Now, GuildRole);
            case validate_name(GuildName) of
                true ->
                    %% save guild
                    Guild = #guild{guild_name = GuildName, leader_id = RoleId, leader_name = UserName, level = Level, create_time = Now},
                    GuildId = guild_sql:insert(Guild),
                    NewGuild = Guild#guild{guild_id = GuildId},
                    ets:insert(guild_table(), NewGuild),
                    %% save guild role
                    NewGuildRole = GuildRole#guild_role{guild_id = GuildId},
                    guild_role_sql:insert_update([NewGuildRole]),
                    RoleTable = role_table(GuildId),
                    ets:new(RoleTable, [named_table, {keypos, #guild_role.role_id}, {read_concurrency, true}]),
                    ets:insert(RoleTable, NewGuildRole),
                    %% new apply table
                    ets:new(apply_table(GuildId), [named_table, {keypos, #guild_apply.role_id}, {read_concurrency, true}]),
                    {ok, GuildId};
                {false, length, _} ->
                    {error, 4};
                {false, asn1, _} ->
                    {error, 5};
                {false, duplicate} ->
                    {error, 6}
            end;
        _ ->
            {error, 3}
    end.

%% @doc apply
-spec apply(GuildId :: non_neg_integer(),  RoleId :: non_neg_integer(), Name :: binary()) -> ok | {error, non_neg_integer()}.
apply(GuildId, RoleId, Name) ->
    OldGuildId = role_guild_id(RoleId),
    case ets:lookup(guild_table(), GuildId) of
        [#guild{}] when OldGuildId == 0 ->
            Apply = #guild_apply{
                guild_id = GuildId,
                role_id = RoleId,
                role_name = Name,
                flag = insert
            },
            ets:insert(apply_table(GuildId), Apply),
            ets:insert(apply_index_table(), {GuildId, RoleId}),
            %% @todo notify msg to leader
            ok;
        [] ->
            {error, 2};
        _ ->
            {error, 3}
    end.

%% @doc cancel apply
-spec cancel_apply(GuildId :: non_neg_integer(), RoleId :: non_neg_integer()) -> ok.
cancel_apply(GuildId, RoleId) ->
    %% delete db data
    guild_apply_sql:delete(RoleId, GuildId),
    %% clear ets apply data
    ets:delete(apply_table(GuildId), RoleId),
    %% clear index
    List = lists:keydelete(1, GuildId, ets:lookup(apply_index_table(), RoleId)),
    ets:insert(apply_index_table(), List),
    ok.

%% @doc approve apply
-spec approve(LeaderId :: non_neg_integer(), MemberId :: non_neg_integer()) -> ok | {error, non_neg_integer()}.
approve(LeaderId, MemberId) ->
    GuildId = role_guild_id(LeaderId),
    RoleTable = role_table(GuildId),
    case check_role(ets:lookup(RoleTable, LeaderId), [{job, ?GUILD_JOB_VICE}]) of
        ok ->
            join_check(GuildId, RoleTable, MemberId);
        Error ->
            Error
    end.

join_check(GuildId, RoleTable, MemberId) ->
    case ets:lookup(RoleTable, MemberId) of
        [] ->
            case ets:lookup(apply_table(GuildId), MemberId) of
                [#guild_apply{role_name = RoleName}] ->
                    join_check_limit(GuildId, RoleTable, MemberId, RoleName);
                _ ->
                    {error, 5}
            end;
        _ ->
            {error, 3}
    end.

join_check_limit(GuildId, RoleTable, RoleId, RoleName) ->
    case ets:lookup(guild_table(), GuildId) of
        [#guild{level = Level}] ->
            Limit = parameter_data:get({guild_member_limit, Level}),
            case ets:info(RoleTable, size) < Limit of
                true ->
                    join(RoleTable, GuildId, RoleId, RoleName);
                _ ->
                    {error, 4}
            end;
        _ ->
            {error, 2}
    end.

%% apply info to role info
join(RoleTable, GuildId, RoleId, RoleName) ->
    Role = #guild_role{
        guild_id = GuildId,
        role_id = RoleId,
        job = ?GUILD_JOB_MEMBER,
        role_name = RoleName,
        flag = update
    },
    %% save new role
    ets:insert(RoleTable, Role),
    ets:insert(role_index_table(), {GuildId, RoleId}),
    %% clear db data
    guild_apply_sql:delete_role_id(RoleId),
    %% clear apply data
    lists:foreach(fun({ApplyGuildId, ApplyRoleId}) -> ets:delete(apply_table(ApplyGuildId), ApplyRoleId) end, ets:lookup(apply_index_table(), RoleId)),
    %% delete index data
    ets:delete(apply_index_table(), RoleId),
    %% @todo broadcast join msg
    ok.

%% @doc approve all apply
-spec approve_all(LeaderId :: non_neg_integer()) -> ok | {error, non_neg_integer()}.
approve_all(LeaderId) ->
    GuildId = role_guild_id(LeaderId),
    RoleTable = role_table(GuildId),
    case check_role(ets:lookup(RoleTable, LeaderId), [{job, ?GUILD_JOB_VICE}]) of
        ok ->
            ess:foreach(fun([#guild_apply{role_id = RoleId, role_name = RoleName}]) -> join_check_limit(GuildId, RoleTable, RoleId, RoleName) end, apply_table(GuildId));
        Error ->
            Error
    end.

%% @doc reject apply
-spec reject(LeaderId :: non_neg_integer(), MemberId :: non_neg_integer()) -> ok | {error, non_neg_integer()}.
reject(LeaderId, MemberId) ->
    GuildId = role_guild_id(LeaderId),
    RoleTable = role_table(GuildId),
    case check_role(ets:lookup(RoleTable, LeaderId), [{job, ?GUILD_JOB_VICE}]) of
        ok ->
            %% delete db data
            guild_apply_sql:delete(MemberId, GuildId),
            %% clear ets apply data
            ets:delete(apply_table(GuildId), MemberId),
            %% clear index
            List = lists:keydelete(1, GuildId, ets:lookup(apply_index_table(), MemberId)),
            ets:insert(apply_index_table(), List),
            ok;
        [#guild_role{}] ->
            {error, 2};
        _ ->
            {error, 3}
    end.

%% @doc reject all apply
-spec reject_all(LeaderId :: non_neg_integer()) -> ok | {error, non_neg_integer()}.
reject_all(LeaderId) ->
    GuildId = role_guild_id(LeaderId),
    RoleTable = role_table(GuildId),
    case check_role(ets:lookup(RoleTable, LeaderId), [{job, ?GUILD_JOB_VICE}]) of
        ok ->
            %% delete db data
            guild_apply_sql:delete_guild_id(GuildId),
            %% clear ets apply data
            ets:delete(apply_table(GuildId)),
            %% clear index
            ets:insert(apply_index_table(), ets:select(ets:fun2ms(fun(Index = {ThisGuildId, _}) when ThisGuildId =/= GuildId -> Index end), apply_index_table())),
            ok;
        _ ->
            {error, 3}
    end.

%% @doc leave
-spec leave(RoleId :: non_neg_integer()) -> ok | {error, non_neg_integer()}.
leave(RoleId) ->
    GuildId = role_guild_id(RoleId),
    RoleTable = role_table(GuildId),
    case ets:lookup(RoleTable, RoleId) of
        [#guild_role{job = ?GUILD_JOB_LEADER}] ->
            %% leader leave dismiss it
            do_dismiss(RoleTable, GuildId);
        [#guild_role{}] ->
            %% @todo broadcast leave msg
            guild_role_sql:delete(GuildId, RoleId),
            ets:delete(RoleTable, RoleId),
            ets:delete(role_index_table(), RoleId),
            ok;
        _ ->
            {error, 2}
    end.

%% @doc dismiss
-spec dismiss(LeaderId :: non_neg_integer()) -> ok | {error, non_neg_integer()}.
dismiss(LeaderId) ->
    GuildId = role_guild_id(LeaderId),
    RoleTable = role_table(GuildId),
    case ets:lookup(RoleTable, LeaderId) of
        [#guild_role{job = ?GUILD_JOB_LEADER}] ->
            do_dismiss(RoleTable, GuildId);
        [#guild_role{}] ->
            {error, 2};
        _ ->
            {error, 3}
    end.

do_dismiss(RoleTable, GuildId) ->
    %% delete db guild data
    guild_sql:delete(GuildId),
    ets:delete(guild_table()),
    %% @todo broadcast dismiss msg
    %% delete role
    guild_role_sql:delete_guild_id(GuildId),
    ets:delete(RoleTable),
    ets:insert(role_index_table(), ets:select(ets:fun2ms(fun(Index = {ThisGuildId, _}) when ThisGuildId =/= GuildId -> Index end), role_index_table())),
    %% delete apply
    guild_apply_sql:delete_guild_id(GuildId),
    ets:delete(apply_table(GuildId)),
    ok.

%% @doc kick
-spec kick(LeaderId :: non_neg_integer(), MemberId :: non_neg_integer()) -> ok | {error, non_neg_integer()}.
kick(LeaderId, MemberId) ->
    GuildId = role_guild_id(LeaderId),
    RoleTable = role_table(GuildId),
    case ets:lookup(RoleTable, LeaderId) of
        [#guild_role{job = Job = ?GUILD_JOB_LEADER}] ->
            case ets:lookup(RoleTable, MemberId) of
                [#guild_role{job = MemberJob}] when Job =/= MemberJob ->
                    %% @todo broadcast be kick msg
                    ets:delete(RoleTable, MemberId),
                    guild_role_sql:delete(GuildId, MemberId),
                    ets:delete(role_index_table(), MemberId),
                    ok;
                [#guild_role{}] ->
                    {error, 4};
                _ when LeaderId =/= MemberId ->
                    {error, 5};
                _ ->
                    {error, 6}
            end;
        [#guild_role{}] ->
            {error, 2};
        _ ->
            {error, 3}
    end.

-spec update_job(LeaderId :: non_neg_integer(), MemberId :: non_neg_integer(), Job :: pos_integer()) -> ok | {error, non_neg_integer()}.
update_job(LeaderId, MemberId, Job) ->
    GuildId = role_guild_id(LeaderId),
    RoleTable = role_table(GuildId),
    case ets:lookup(RoleTable, LeaderId) of
        [#guild_role{job = LeaderJob}] when Job < LeaderJob ->
            case ets:lookup(RoleTable, MemberId) of
                [Role = #guild_role{job = MemberJob}] when MemberJob < LeaderJob ->
                    NewRole = Role#guild_role{job = Job, flag = update},
                    ets:insert(RoleTable, NewRole),
                    %% @todo broadcast be job update msg
                    ok;
                [#guild_role{}] ->
                    {error, 4};
                _ ->
                    {error, 5}
            end;
        [#guild_role{}] ->
            {error, 2};
        _ ->
            {error, 3}
    end.

%% @doc validate guild name
-spec validate_name(String :: binary() | list()) -> true | {false, Reason :: term()} | {false, atom(), Reason :: term()}.
validate_name(GuildName) ->
    Condition = [{length, 1, 6}, sensitive, {sql, io_lib:format("SELECT `guild_id` FROM `guild` WHERE `guild_name` = '~s'", [GuildName])}],
    word:validate(Condition, GuildName).


%%%==================================================================
%%% common tool
%%%==================================================================

%% @doc check guild role
-spec check_role(GuildRole :: #guild_role{}, List :: list()) -> ok | error.
check_role(_, []) ->
    ok;
check_role(GuildRole = #guild_role{guild_id = GuildId}, [{guild_id, GuildId} | T]) ->
    check_role(GuildRole, T);
check_role(GuildRole = #guild_role{job = Job}, [{job, MinJob} | T]) when MinJob =< Job ->
    check_role(GuildRole, T);
check_role(GuildRole = #guild_role{job = 1}, [leader | T]) ->
    check_role(GuildRole, T);
check_role(GuildRole = #guild_role{job = 2}, [vice | T]) ->
    check_role(GuildRole, T);
check_role(GuildRole = #guild_role{job = 3}, [elite | T]) ->
    check_role(GuildRole, T);
check_role(GuildRole = #guild_role{job = 4}, [member | T]) ->
    check_role(GuildRole, T);
check_role(_, [{What, _} | _]) ->
    {error, What};
check_role(_, [What | _]) ->
    {error, What}.

%% @doc send data to local server all online role
-spec broadcast(GuildId :: non_neg_integer(), Data :: binary()) -> ok.
broadcast(GuildId, Data) ->
    ess:foreach(fun([#guild_role{role_id = RoleId}]) -> user_sender:send(RoleId, Data) end, role_table(GuildId)).

-spec broadcast(GuildId :: non_neg_integer(), Data :: binary(), ExceptId :: non_neg_integer()) -> ok.
broadcast(GuildId, Data, ExceptId) ->
    ess:foreach(fun([#guild_role{role_id = RoleId}]) -> RoleId =/= ExceptId andalso user_sender:send(RoleId, Data) == ok end, role_table(GuildId)).

%% @doc role guild status
-spec role_guild_id(RoleId :: non_neg_integer()) -> non_neg_integer().
role_guild_id(RoleId) ->
    element(2, hd(tool:default(ets:lookup(role_index_table(), RoleId), [{0, 0}]))).

%%%==================================================================
%%% ets data set get/set
%%%==================================================================

%% @doc guild ets name
-spec guild_table() -> atom().
guild_table() ->
    guild.

-spec role_index_table() -> atom().
role_index_table() ->
    guild_role_index.

%% @doc guild role ets name
-spec role_table(GuildId :: non_neg_integer()) -> atom().
role_table(GuildId) ->
    type:to_atom(lists:concat([guild_role_, GuildId])).

-spec apply_index_table() -> atom().
apply_index_table() ->
    guild_apply_index.

%% @doc guild apply ets name
-spec apply_table(GuildId :: non_neg_integer()) -> atom().
apply_table(GuildId) ->
    type:to_atom(lists:concat([guild_apply_, GuildId])).

%% @doc find guild
-spec get_guild(GuildId :: non_neg_integer()) -> [#guild{}] | [].
get_guild(GuildId) ->
    ets:lookup(guild_table(), GuildId).

%% @doc find role
-spec get_role(RoleId :: non_neg_integer()) -> [#guild_role{}] | [].
get_role(RoleId) ->
    GuildId = role_guild_id(RoleId),
    get_role(RoleId, GuildId).

%% @doc find role
-spec get_role(RoleId :: non_neg_integer(), GuildId :: non_neg_integer()) -> [#guild_role{}] | [].
get_role(RoleId, GuildId) ->
    ets:lookup(role_table(GuildId), RoleId).

%% @doc find apply
-spec get_apply(RoleId :: non_neg_integer()) -> [#guild_apply{}] | [].
get_apply(RoleId) ->
    GuildId = role_guild_id(RoleId),
    get_apply(RoleId, GuildId).

%% @doc find apply
-spec get_apply(RoleId :: non_neg_integer(), GuildId :: non_neg_integer()) -> [#guild_apply{}] | [].
get_apply(RoleId, GuildId) ->
    ets:lookup(apply_table(GuildId), RoleId).

%%%==================================================================
%%% Internal functions
%%%==================================================================
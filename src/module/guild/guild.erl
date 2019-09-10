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
    role_guild_id/1,
    role_status/1,
    apply/5,
    cancel_apply/2,
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

%%%===================================================================
%%% API
%%%===================================================================
%% @doc guild server start
-spec server_start() -> ok.
server_start() ->
    %% guild data
    ets:new(guild, [named_table, {keypos, #guild.guild_id}, {read_concurrency, true}]),
    %% extra 0 guild id role data
    ets:new(role_table(0), [named_table, {keypos, #guild_role.role_id}, {read_concurrency, true}]),
    %% guild
    SaveGuild = fun(X = #guild{guild_id = GuildId}) ->
        %% new role table
        ets:new(role_table(GuildId), [named_table, {keypos, #guild_role.role_id}, {read_concurrency, true}]),
        %% new apply table
        ets:new(apply_table(GuildId), [named_table, {keypos, #guild_apply.role_id}, {read_concurrency, true}]),
        %% save guild data
        ets:insert(guild, X)
    end,
    parser:convert(guild_sql:select(), guild, SaveGuild),
    %% guild role
    SaveRole = fun(X = #guild_role{guild_id = GuildId}) -> ets:insert(role_table(GuildId), X) end,
    parser:convert(guild_role_sql:select_join(), guild_role, SaveRole),
    %% guild apply
    SaveRequest = fun(X = #guild_apply{guild_id = GuildId}) -> ets:insert(apply_table(GuildId), X) end,
    parser:convert(guild_apply_sql:select_join(), guild_apply, SaveRequest),
    %% save timer
    erlang:send_after(?MINUTE_SECONDS * 1000, self(), loop),
    {ok, #guild_state{tick = 0, timeout = ?MINUTE_SECONDS * 1000}}.

%% @doc guild server stop
-spec server_stop() -> ok.
server_stop() ->
    F = fun([Guild = #guild{guild_id = GuildId}]) ->
        %% save guild
        guild_sql:update(Guild),
        %% save role
        guild_role_sql:insert_update(role_table(GuildId)),
        %% save apply
        guild_apply_sql:insert_update(apply_table(GuildId)),
        %% change save flag
        Guild#guild{flag = 0}
    end,
    ess:map(F, guild),
    ok.

%% @doc send data to local server all online role
-spec broadcast(GuildId :: non_neg_integer(), Data :: binary()) -> ok.
broadcast(GuildId, Data) ->
    ess:foreach(fun([#guild_role{role_sender_pid = Pid}]) -> user_sender:send(Pid, Data) end, role_table(GuildId)).
-spec broadcast(GuildId :: non_neg_integer(), Data :: binary(), ExceptId :: non_neg_integer()) -> ok.
broadcast(GuildId, Data, ExceptId) ->
    ess:foreach(fun([#guild_role{role_id = RoleId, role_sender_pid = Pid}]) -> RoleId =/= ExceptId andalso user_sender:send(Pid, Data) == ok end, role_table(GuildId)).

%% @doc role guild status
-spec role_guild_id(RoleId :: non_neg_integer()) -> non_neg_integer().
role_guild_id(RoleId) ->
    [#guild_role{guild_id = GuildId} | _] = ess:first(fun([#guild{guild_id = GuildId}]) -> ets:lookup(role_table(GuildId), RoleId) end, guild, [#guild_role{}]),
    GuildId.

%% @doc role guild cd
-spec role_guild_cd(RoleId :: non_neg_integer()) -> non_neg_integer().
role_guild_cd(RoleId) ->
    GuildId = role_guild_id(RoleId),
    case ets:lookup(role_table(GuildId), RoleId) of
        [#guild_role{leave_time = LeaveTime}] ->
            LeaveTime;
        _ ->
            0
    end.

%% @doc role guild status
-spec role_status(RoleId :: non_neg_integer()) -> none | ever | joined | bad.
role_status(RoleId) ->
    case ets:lookup(guild_role, RoleId) of
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
-spec create(RoleId :: non_neg_integer(), UserName :: binary() | string(), Level :: non_neg_integer(), GuildName :: binary() | string()) -> {ok, GuildId :: non_neg_integer()} | {error, Code :: non_neg_integer()}.
create(RoleId, UserName, Level, GuildName) ->
    Now = time:ts(),
    GuildId = role_guild_id(RoleId),
    CdTime = parameter_data:get({guild_create, cd}),
    case ets:lookup(role_table(GuildId), RoleId) of
        [] ->
            %% no old guild
            GuildRole = #guild_role{role_id = RoleId, role_name = UserName, job = 1, join_time = Now, flag = update},
            do_create(RoleId, UserName, Level, GuildName, Now, GuildRole);
        [OldGuildRole = #guild_role{guild_id = 0, leave_time = LeaveTime}] when Now - LeaveTime >= CdTime ->
            %% has old guild but leave cd invalid
            GuildRole = OldGuildRole#guild_role{role_id = RoleId, role_name = UserName, job = 1, join_time = Now, flag = update},
            do_create(RoleId, UserName, Level, GuildName, Now, GuildRole);
        [#guild_role{guild_id = 0, leave_time = LeaveTime}] when Now - LeaveTime < CdTime ->
            %% has old guild and leave cd valid
            {error, 2};
        _ ->
            {error, 3}
    end.
do_create(RoleId, UserName, Level, GuildName, Now, GuildRole) ->
    case validate_name(GuildName) of
        true ->
            %% save guild
            Guild = #guild{guild_name = GuildName, leader_id = RoleId, leader_name = UserName, level = Level, create_time = Now},
            GuildId = guild_sql:insert(Guild),
            NewGuild = Guild#guild{guild_id = GuildId},
            ets:insert(guild, NewGuild),
            %% save guild role
            NewGuildRole = GuildRole#guild_role{guild_id = GuildId},
            guild_role_sql:insert_update([NewGuildRole]),
            Table = role_table(GuildId),
            ets:new(Table, [named_table, {keypos, #guild_role.role_id}, {read_concurrency, true}]),
            ets:insert(Table, NewGuildRole),
            %% new apply table
            ets:new(apply_table(GuildId), [named_table, {keypos, #guild_apply.role_id}, {read_concurrency, true}]),
            {ok, GuildId};
        {false, length, _} ->
            {error, 4};
        {false, asn1, _} ->
            {error, 5};
        {false, duplicate} ->
            {error, 6}
    end.

%% @doc apply
-spec apply(GuildId :: non_neg_integer(),  RoleId :: non_neg_integer(), Name :: binary(), Pid :: pid(), SenderPid :: pid()) -> ok | {error, non_neg_integer()}.
apply(GuildId, RoleId, Name, Pid, SenderPid) ->
    OldGuildId = role_guild_id(RoleId),
    case ets:lookup(guild, GuildId) of
        [#guild{}] when OldGuildId == 0 ->
            Request = #guild_apply{
                guild_id = GuildId,
                role_id = RoleId,
                role_name = Name,
                role_pid = Pid,
                sender_pid = SenderPid,
                flag = insert
            },
            ets:insert(apply_table(GuildId), Request),
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
    ok.

%% @doc approve apply
-spec approve(LeaderId :: non_neg_integer(), MemberId :: non_neg_integer()) -> ok | {error, non_neg_integer()}.
approve(LeaderId, MemberId) ->
    GuildId = role_guild_id(LeaderId),
    Table = role_table(GuildId),
    case ets:lookup(guild, GuildId) of
        [#guild{level = Level}] ->
            case ets:lookup(Table, LeaderId) of
                [#guild_role{job = Job}] when Job == 1 orelse Job == 2 ->
                    Limit = parameter_data:get({guild_member, limit, Level}),
                    case ets:lookup(apply_table(GuildId), MemberId) of
                        [Request = #guild_apply{}] ->
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

approve_join_check(GuildId, Limit, Table, Request = #guild_apply{role_id = RoleId}) ->
    case ets:info(Table, size) < Limit of
        true ->
            OldGuildId = role_guild_id(RoleId),
            case ets:lookup(role_table(OldGuildId), RoleId) of
                [Role = #guild_role{guild_id = 0}] ->
                    join(Table, GuildId, Role, Request);
                [] ->
                    join(Table, GuildId, #guild_role{}, Request);
                _ ->
                    {error, 6}
            end;
        _ ->
            {error, 4}
    end.

%% apply info to role info
join(Table, GuildId, Role, Request) ->
    #guild_apply{role_id = RoleId, role_name = RoleName, role_pid = Pid, sender_pid = SenderPid} = Request,
    NewRole = Role#guild_role{
        guild_id = GuildId,
        role_id = RoleId,
        job = 4,
        role_name = RoleName,
        role_pid = Pid,
        role_sender_pid = SenderPid,
        flag = update
    },
    %% save new role
    ets:insert(Table, NewRole),
    %% delete old role if exists
    ets:delete(guild_role_0, RoleId),
    %% clear db data
    guild_apply_sql:delete_role_id(RoleId),
    %% clear all old apply
    ess:foreach(fun([Guild]) -> ets:delete(apply_table(Guild#guild.guild_id), RoleId) end, guild),
    %% @todo broadcast join msg
    ok.

%% @doc approve all apply
-spec approve_all(LeaderId :: non_neg_integer()) -> ok | {error, non_neg_integer()}.
approve_all(LeaderId) ->
    GuildId = role_guild_id(LeaderId),
    Table = role_table(GuildId),
    case ets:lookup(guild, GuildId) of
        [#guild{level = Level}] ->
            case ets:lookup(Table, LeaderId) of
                [#guild_role{job = Job}] when Job == 1 orelse Job == 2 ->
                    Limit = parameter_data:get({guild_member, limit, Level}),
                    RequestTable = apply_table(GuildId),
                    ess:first(fun([Request]) -> approve_join_check(GuildId, Limit, Table, Request) == ok end, RequestTable),
                    ets:delete_all_objects(RequestTable);
                _ ->
                    {error, 3}
            end;
        _ ->
            {error, 2}
    end.

%% @doc reject apply
-spec reject(LeaderId :: non_neg_integer(), MemberId :: non_neg_integer()) -> ok | {error, non_neg_integer()}.
reject(LeaderId, MemberId) ->
    GuildId = role_guild_id(LeaderId),
    Table = role_table(GuildId),
    case ets:lookup(Table, LeaderId) of
        [#guild_role{job = Job}] when Job == 1 orelse Job == 2 ->
            %% delete db data
            guild_apply_sql:delete(MemberId, GuildId),
            %% clear ets apply data
            ets:delete(apply_table(GuildId), MemberId),
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
    Table = role_table(GuildId),
    case ets:lookup(Table, LeaderId) of
        [#guild_role{job = Job}] when Job == 1 orelse Job == 2 ->
            %% delete db data
            guild_apply_sql:delete_guild_id(GuildId),
            %% clear ets apply data
            ets:delete_all_objects(apply_table(GuildId)),
            ok;
        [#guild_role{}] ->
            {error, 2};
        _ ->
            {error, 3}
    end.

%% @doc leave
-spec leave(RoleId :: non_neg_integer()) -> ok | {error, non_neg_integer()}.
leave(RoleId) ->
    GuildId = role_guild_id(RoleId),
    Table = role_table(GuildId),
    case ets:lookup(Table, RoleId) of
        [#guild_role{job = 1}] ->
            %% leader leave dismiss it
            do_dismiss(Table, GuildId);
        [Role = #guild_role{}] ->
            %% @todo broadcast leave msg
            NewRole = Role#guild_role{guild_id = 0, leave_time = time:ts(), flag = update},
            ets:insert(Table, NewRole),
            ok;
        _ ->
            {error, 2}
    end.

%% @doc dismiss
-spec dismiss(LeaderId :: non_neg_integer()) -> ok | {error, non_neg_integer()}.
dismiss(LeaderId) ->
    GuildId = role_guild_id(LeaderId),
    Table = role_table(GuildId),
    case ets:lookup(Table, LeaderId) of
        [#guild_role{job = 1}] ->
            do_dismiss(Table, GuildId);
        [#guild_role{}] ->
            {error, 2};
        _ ->
            {error, 3}
    end.

do_dismiss(Table, GuildId) ->
    %% delete db guild data
    guild_sql:delete(GuildId),
    Now = time:ts(),
    %% @todo broadcast dismiss msg
    ess:map(fun([X]) -> X#guild_role{guild_id = 0, job = 0, leave_time = Now, flag = update} end, Table),
    %% delete db data
    guild_apply_sql:delete_guild_id(GuildId),
    %% clear ets apply data
    ets:delete(apply_table(GuildId)),
    ok.

%% @doc kick
-spec kick(LeaderId :: non_neg_integer(), MemberId :: non_neg_integer()) -> ok | {error, non_neg_integer()}.
kick(LeaderId, MemberId) ->
    GuildId = role_guild_id(LeaderId),
    Table = role_table(GuildId),
    case ets:lookup(Table, LeaderId) of
        [#guild_role{job = LeaderJob}] when LeaderJob == 1 orelse LeaderJob == 2 ->
            case ets:lookup(Table, MemberId) of
                [Role = #guild_role{job = MemberJob}] when LeaderJob =/= MemberJob ->
                    %% @todo broadcast be kick msg
                    NewRole = Role#guild_role{guild_id = 0, leave_time = time:ts(), flag = update},
                    ets:insert(Table, NewRole),
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

-spec job_update(LeaderId :: non_neg_integer(), MemberId :: non_neg_integer(), Job :: pos_integer()) -> ok | {error, non_neg_integer()}.
job_update(LeaderId, MemberId, Job) ->
    GuildId = role_guild_id(LeaderId),
    Table = role_table(GuildId),
    case ets:lookup(Table, LeaderId) of
        [#guild_role{job = LeaderJob}] when Job < LeaderJob ->
            case ets:lookup(Table, MemberId) of
                [Role = #guild_role{job = MemberJob}] when MemberJob < LeaderJob ->
                    NewRole = Role#guild_role{job = Job, flag = update},
                    ets:insert(Table, NewRole),
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
check_role(_, _) ->
    error.
%%%===================================================================
%%% Internal functions
%%%===================================================================
%% guild role ets name
role_table(GuildId) ->
    type:to_atom(lists:concat([guild_role_, GuildId])).

%% guild apply ets name
apply_table(GuildId) ->
    type:to_atom(lists:concat([guild_apply_, GuildId])).

%% find guild
get_guild(GuildId) ->
    ets:lookup(guild, GuildId).

%% find role
get_role(RoleId) ->
    GuildId = role_guild_id(RoleId),
    ets:lookup(role_table(GuildId), RoleId).

%% find apply
get_apply(RoleId) ->
    GuildId = role_guild_id(RoleId),
    ets:lookup(apply_table(GuildId), RoleId).
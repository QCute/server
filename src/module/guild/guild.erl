%%%-------------------------------------------------------------------
%%% @doc
%%% guild
%%% @end
%%%-------------------------------------------------------------------
-module(guild).
%% API
-export([
    server_start/0,
    server_stop/0,
    save/0,
    %% operation
    create/8,
    apply/3,
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
    add_exp/3,
    upgrade_level/1,
    change_notice/2,
    add_guild_wealth/3,
    add_role_wealth/4,
    %% assist
    check_role/2,
    broadcast/2,
    broadcast/3,
    role_guild_id/1,
    %% table
    guild_table/0,
    %% role_index_table/0,
    role_table/1,
    %% apply_index_table/0,
    apply_table/1,
    get_guild/1,
    get_role/2,
    get_apply/2
]).

%% Includes
-include_lib("stdlib/include/ms_transform.hrl").
-include("common.hrl").
-include("time.hrl").
-include("event.hrl").
-include("guild.hrl").
%%%===================================================================
%%% API functions
%%%===================================================================
%% @doc guild server start
-spec server_start() -> {ok, #guild_state{}}.
server_start() ->
    %% guild
    ets:new(guild_table(), [named_table, set, {keypos, #guild.guild_id}, {read_concurrency, true}]),
    %% guild
    GuildList = guild_sql:select_join(),
    ets:insert(guild_table(), GuildList),
    %% new role and apply table
    lists:foreach(fun(#guild{guild_id = GuildId}) -> ets:new(role_table(GuildId), [named_table, set, {keypos, #guild_role.role_id}, {read_concurrency, true}]), ets:new(apply_table(GuildId), [named_table, set, {keypos, #guild_apply.role_id}, {read_concurrency, true}]) end, GuildList),
    %% new without guild id role table
    ets:new(role_table(0), [named_table, set, {keypos, #guild_role.role_id}, {read_concurrency, true}]),
    %% guild role
    lists:foreach(fun(GuildRole = #guild_role{guild_id = GuildId}) -> ets:insert(role_table(GuildId), GuildRole) end, guild_role_sql:select_join()),
    %% guild apply
    lists:foreach(fun(GuildRole = #guild_apply{guild_id = GuildId}) -> ets:insert(apply_table(GuildId), GuildRole) end, guild_apply_sql:select_join()),
    %% save timer first after 3 minutes
    erlang:send_after(?MINUTE_MILLISECONDS(3), self(), {loop, 0}),
    {ok, #guild_state{}}.

%% @doc guild server stop
-spec server_stop() -> ok.
server_stop() ->
    save().

%% @doc save data
-spec save() -> ok.
save() ->
    guild_table(),
    F = fun(GuildId) ->
        %% save role
        guild_role_sql:insert_update(role_table(GuildId)),
        %% save apply
        guild_apply_sql:insert_update(apply_table(GuildId))
    end,
    ess:walk(F, guild_table()),
    %% save guild
    guild_sql:insert_update(guild_table()),
    %% save non guild role
    guild_role_sql:insert_update(role_table(0)),
    ok.

%% @doc create
-spec create(RoleId :: non_neg_integer(), RoleName :: binary() | string(), Level :: non_neg_integer(), Sex :: non_neg_integer(), Classes :: non_neg_integer(), VipLevel :: non_neg_integer(), Type :: non_neg_integer(), GuildName :: binary() | string()) -> {ok, GuildId :: non_neg_integer()} | {error, term()}.
create(RoleId, RoleName, Sex, Classes, Level, VipLevel, Type, GuildName) ->
    Now = time:now(),
    OldGuildId = role_guild_id(RoleId),
    Cd = parameter_data:get(guild_create_cd),
    case ets:lookup(role_table(OldGuildId), RoleId) of
        [] ->
            do_create(RoleId, RoleName, Sex, Classes, Level, VipLevel, Type, GuildName, Now);
        [#guild_role{guild_id = 0, leave_time = LeaveTime}] when LeaveTime + Cd < Now ->
            do_create(RoleId, RoleName, Sex, Classes, Level, VipLevel, Type, GuildName, Now);
        [_] when 0 < OldGuildId ->
            {error, already_join_guild};
        [_] ->
            {error, time_in_join_cd}
    end.

do_create(RoleId, RoleName, Sex, Classes, Level, VipLevel, Type, GuildName, Now) ->
    GuildRole = #guild_role{role_id = RoleId, role_name = RoleName, job = ?GUILD_JOB_LEADER, join_time = Now, sex = Sex, classes = Classes, level = Level, vip_level = VipLevel, flag = 1},
    SameNameGuild = ets:select(guild_table(), ets:fun2ms(fun(#guild{guild_name = ThisGuildName}) when GuildName == ThisGuildName -> 1 end), 1),
    case word:validate(GuildName) of
        true when SameNameGuild == '$end_of_table' ->
            %% save guild
            Guild = #guild{guild_name = GuildName, leader_id = RoleId, leader_name = RoleName, level = Type, create_time = Now},
            GuildId = guild_sql:insert(Guild),
            NewGuild = Guild#guild{guild_id = GuildId},
            ets:insert(guild_table(), NewGuild),
            %% save guild role
            NewGuildRole = GuildRole#guild_role{guild_id = GuildId},
            guild_role_sql:insert_update([NewGuildRole]),
            RoleTable = role_table(GuildId),
            ets:new(RoleTable, [named_table, set, {keypos, #guild_role.role_id}, {read_concurrency, true}]),
            ets:insert(RoleTable, NewGuildRole),
            %% new apply table
            ets:new(apply_table(GuildId), [named_table, set, {keypos, #guild_apply.role_id}, {read_concurrency, true}]),
            {ok, GuildId};
        true ->
            {error, duplicate};
        {false, length, _} ->
            {error, length};
        {false, asn1, _} ->
            {error, not_utf8};
        {false, sensitive} ->
            {error, sensitive}
    end.

%% @doc apply
-spec apply(GuildId :: non_neg_integer(),  RoleId :: non_neg_integer(), Name :: binary()) -> {ok, ok} | {error, term()}.
apply(GuildId, RoleId, Name) ->
    Now = time:now(),
    Cd = parameter_data:get(guild_join_cd),
    OldGuildId = role_guild_id(RoleId),
    case ets:lookup(role_table(OldGuildId), RoleId) of
        [] ->
            do_apply(GuildId, RoleId, Name);
        [#guild_role{guild_id = 0, leave_time = LeaveTime}] when LeaveTime + Cd < Now ->
            do_apply(GuildId, RoleId, Name);
        [_] when 0 < OldGuildId ->
            {error, already_join_guild};
        [_] ->
            {error, time_in_join_cd}
    end.

do_apply(GuildId, RoleId, Name) ->
    case ets:lookup(guild_table(), GuildId) of
        [#guild{}] ->
            Apply = #guild_apply{
                guild_id = GuildId,
                role_id = RoleId,
                role_name = Name,
                flag = 1
            },
            ets:insert(apply_table(GuildId), Apply),
            %% @todo notify msg to leader
            {ok, ok};
        _ ->
            {error, no_such_guild}
    end.

%% @doc cancel apply
-spec cancel_apply(GuildId :: non_neg_integer(), RoleId :: non_neg_integer()) -> {ok, ok}.
cancel_apply(GuildId, RoleId) ->
    %% delete db data
    guild_apply_sql:delete(RoleId, GuildId),
    %% clear ets apply data
    ets:delete(apply_table(GuildId), RoleId),
    %% clear index
    {ok, ok}.

%% @doc cancel all apply
-spec cancel_all_apply(RoleId :: non_neg_integer()) -> {ok, ok}.
cancel_all_apply(RoleId) ->
    ess:walk(fun(ApplyGuildId) -> ets:delete(apply_table(ApplyGuildId), RoleId) end, guild_table()),
    {ok, ok}.

%% @doc approve apply
-spec approve_apply(SuperiorId :: non_neg_integer(), MemberId :: non_neg_integer()) -> {ok, ok} | {error, term()}.
approve_apply(SuperiorId, MemberId) ->
    GuildId = role_guild_id(SuperiorId),
    RoleTable = role_table(GuildId),
    case check_role(ets:lookup(RoleTable, SuperiorId), [{job, ?GUILD_JOB_VICE}]) of
        ok ->
            join_check_member(GuildId, RoleTable, MemberId);
        _ ->
            {error, permission_denied}
    end.

join_check_member(GuildId, RoleTable, MemberId) ->
    case ets:lookup(RoleTable, MemberId) of
        [] ->
            case ets:lookup(apply_table(GuildId), MemberId) of
                [#guild_apply{role_name = RoleName}] ->
                    join_check_limit(GuildId, RoleTable, MemberId, RoleName);
                _ ->
                    {error, no_such_apply}
            end;
        _ ->
            {error, already_join_guild}
    end.

join_check_limit(GuildId, RoleTable, RoleId, RoleName) ->
    case ets:lookup(guild_table(), GuildId) of
        [#guild{level = Level}] ->
            LimitList = parameter_data:get(guild_member_limit),
            {_, Limit} = listing:key_find(Level, 1, LimitList, {Level, 0}),
            case ets:info(RoleTable, size) < Limit of
                true ->
                    join(RoleTable, GuildId, RoleId, RoleName);
                _ ->
                    {error, member_number_limit}
            end;
        _ ->
            {error, no_such_guild}
    end.

%% apply info to role info
join(RoleTable, GuildId, RoleId, RoleName) ->
    Role = #guild_role{
        guild_id = GuildId,
        role_id = RoleId,
        job = ?GUILD_JOB_MEMBER,
        role_name = RoleName,
        flag = 1
    },
    %% save new role
    ets:insert(RoleTable, Role),
    %% clear db data
    guild_apply_sql:delete_by_role_id(RoleId),
    %% clear apply data
    ess:walk(fun(ApplyGuildId) -> ets:delete(apply_table(ApplyGuildId), RoleId) end, guild_table()),
    %% join guild event
    user_event:trigger(RoleId, #event{name = event_guild_join}),
    %% @todo broadcast join msg
    {ok, ok}.

%% @doc approve all apply
-spec approve_all_apply(SuperiorId :: non_neg_integer()) -> {ok, ok} | {error, term()}.
approve_all_apply(SuperiorId) ->
    GuildId = role_guild_id(SuperiorId),
    RoleTable = role_table(GuildId),
    case check_role(ets:lookup(RoleTable, SuperiorId), [{job, ?GUILD_JOB_VICE}]) of
        ok ->
            ess:foreach(fun([#guild_apply{role_id = RoleId, role_name = RoleName}]) -> join_check_limit(GuildId, RoleTable, RoleId, RoleName) end, apply_table(GuildId));
        _ ->
            {error, permission_denied}
    end.

%% @doc reject apply
-spec reject_apply(SuperiorId :: non_neg_integer(), MemberId :: non_neg_integer()) -> {ok, ok} | {error, term()}.
reject_apply(SuperiorId, MemberId) ->
    GuildId = role_guild_id(SuperiorId),
    RoleTable = role_table(GuildId),
    case check_role(ets:lookup(RoleTable, SuperiorId), [{job, ?GUILD_JOB_VICE}]) of
        ok ->
            %% delete db data
            guild_apply_sql:delete(MemberId, GuildId),
            %% clear ets apply data
            ets:delete(apply_table(GuildId), MemberId),
            {ok, ok};
        {error, role} ->
            {error, you_are_not_join_guild};
        {error, job} ->
            {error, permission_denied}

    end.

%% @doc reject all apply
-spec reject_all_apply(SuperiorId :: non_neg_integer()) -> {ok, ok} | {error, term()}.
reject_all_apply(SuperiorId) ->
    GuildId = role_guild_id(SuperiorId),
    RoleTable = role_table(GuildId),
    case check_role(ets:lookup(RoleTable, SuperiorId), [{job, ?GUILD_JOB_VICE}]) of
        ok ->
            %% delete db data
            guild_apply_sql:delete_by_guild_id(GuildId),
            %% clear ets apply data
            ets:delete(apply_table(GuildId)),
            {ok, ok};
        _ ->
            {error, permission_denied}
    end.

%% @doc leave
-spec leave(RoleId :: non_neg_integer()) -> {ok, ok} | {error, term()}.
leave(RoleId) ->
    GuildId = role_guild_id(RoleId),
    RoleTable = role_table(GuildId),
    case ets:lookup(RoleTable, RoleId) of
        [#guild_role{job = ?GUILD_JOB_LEADER}] ->
            %% leader leave dismiss it
            dismiss_final(RoleTable, GuildId);
        [GuildRole = #guild_role{}] ->
            %% @todo broadcast leave msg
            ets:insert(role_table(0), GuildRole#guild_role{guild_id = 0}),
            ets:delete(RoleTable, RoleId),
            {ok, ok};
        _ ->
            {error, you_are_not_join_guild}
    end.

%% @doc dismiss
-spec dismiss(LeaderId :: non_neg_integer()) -> {ok, ok} | {error, term()}.
dismiss(LeaderId) ->
    GuildId = role_guild_id(LeaderId),
    RoleTable = role_table(GuildId),
    case ets:lookup(RoleTable, LeaderId) of
        [#guild_role{job = ?GUILD_JOB_LEADER}] ->
            dismiss_final(RoleTable, GuildId);
        [#guild_role{}] ->
            {error, permission_denied};
        _ ->
            {error, you_are_not_join_guild}
    end.

dismiss_final(RoleTable, GuildId) ->
    %% delete db guild data
    guild_sql:delete(GuildId),
    ets:delete(guild_table(), GuildId),
    %% @todo broadcast dismiss msg
    %% delete role
    NonGuildRoleTable = role_table(0),
    ess:foreach(fun([Role]) -> ets:insert(NonGuildRoleTable, Role#guild_role{guild_id = 0}) end, RoleTable),
    %% delete apply
    guild_apply_sql:delete_by_guild_id(GuildId),
    ets:delete(apply_table(GuildId)),
    {ok, ok}.

%% @doc kick
-spec kick(SuperiorId :: non_neg_integer(), MemberId :: non_neg_integer()) -> {ok, ok} | {error, term()}.
kick(SuperiorId, MemberId) ->
    case SuperiorId =/= MemberId of
        true ->
            kick_check_self(SuperiorId, MemberId);
        false ->
            {error, cannot_kick_self}
    end.

kick_check_self(SuperiorId, MemberId) ->
    GuildId = role_guild_id(SuperiorId),
    RoleTable = role_table(GuildId),
    case ets:lookup(RoleTable, SuperiorId) of
        [#guild_role{job = SuperiorJob}] when SuperiorJob == ?GUILD_JOB_LEADER orelse SuperiorJob == ?GUILD_JOB_VICE ->
            kick_check_member(RoleTable, SuperiorJob, MemberId);
        [#guild_role{}] ->
            {error, permission_denied};
        _ ->
            {error, you_are_not_join_guild}
    end.

kick_check_member(RoleTable, SuperiorJob, MemberId) ->
    case ets:lookup(RoleTable, MemberId) of
        [Member = #guild_role{job = MemberJob}] when SuperiorJob < MemberJob ->
            kick_final(RoleTable, Member);
        [#guild_role{}] ->
            {error, permission_denied};
        _ ->
            {error, he_is_not_join_guild}
    end.

kick_final(RoleTable, Member = #guild_role{role_id = MemberId}) ->
    ets:insert(role_table(0), Member#guild_role{guild_id = 0}),
    ets:delete(RoleTable, MemberId),
    %% @todo broadcast be kick msg
    {ok, ok}.

-spec update_job(SuperiorId :: non_neg_integer(), MemberId :: non_neg_integer(), Job :: pos_integer()) -> {ok, ok} | {error, term()}.
update_job(SuperiorId, MemberId, Job) ->
    case SuperiorId =/= MemberId of
        true ->
            update_job_check_self(SuperiorId, MemberId, Job);
        false ->
            {error, cannot_update_self}
    end.

update_job_check_self(SuperiorId, MemberId, Job) ->
    GuildId = role_guild_id(SuperiorId),
    RoleTable = role_table(GuildId),
    case ets:lookup(RoleTable, SuperiorId) of
        [#guild_role{job = SuperiorJob}] when SuperiorJob < Job ->
            update_job_check_member(RoleTable, SuperiorJob, MemberId, Job);
        [#guild_role{}] ->
            {error, permission_denied};
        _ ->
            {error, you_are_not_join_guild}
    end.

update_job_check_member(RoleTable, SuperiorJob, MemberId, Job) ->
    case ets:lookup(RoleTable, MemberId) of
        [Member = #guild_role{job = MemberJob}] when SuperiorJob < MemberJob ->
            update_job_final(RoleTable, Member, Job);
        [#guild_role{}] ->
            {error, permission_denied};
        _ ->
            {error, he_is_not_join_guild}
    end.

update_job_final(RoleTable, Member, Job) ->
    NewRole = Member#guild_role{job = Job, flag = 1},
    ets:insert(RoleTable, NewRole),
    %% @todo broadcast be job update msg
    {ok, ok}.

%% @doc add exp
-spec add_exp(GuildId :: non_neg_integer(), AddExp :: non_neg_integer(), From :: term()) -> {ok, ok} | {error, term()}.
add_exp(GuildId, AddExp, _From) ->
    case ets:lookup(guild_table(), GuildId) of
        [Guild = #guild{exp = Exp}] ->
            NewExp = Exp + AddExp,
            NewGuild = Guild#guild{exp = NewExp},
            ets:insert(guild_table(), NewGuild),
            {ok, ok};
        _ ->
            {error, no_such_guild}
    end.

%% @doc upgrade level
-spec upgrade_level(LeaderId :: non_neg_integer()) -> {ok, ok} | {error, term()}.
upgrade_level(LeaderId) ->
    GuildId = role_guild_id(LeaderId),
    RoleTable = role_table(GuildId),
    Result = check_role(ets:lookup(RoleTable, LeaderId), [{job, ?GUILD_JOB_VICE}]),
    case ets:lookup(guild_table(), GuildId) of
        [Guild = #guild{level = Level, exp = Exp}] when Result == ok ->
            TopLevel = guild_data:level(Exp),
            case Level + 1 < TopLevel of
                true ->
                    NewGuild = Guild#guild{level = Level + 1},
                    ets:insert(guild_table(), NewGuild),
                    {ok, ok};
                false ->
                    {error, level_in_top}
            end;
        _ ->
            {error, no_such_guild}
    end.

%% @doc change notice
-spec change_notice(LeaderId :: non_neg_integer(), Notice :: binary()) -> {ok, ok} | {error, term()}.
change_notice(LeaderId, Notice) ->
    GuildId = role_guild_id(LeaderId),
    RoleTable = role_table(GuildId),
    Result = check_role(ets:lookup(RoleTable, LeaderId), [{job, ?GUILD_JOB_LEADER}]),
    case ets:lookup(guild_table(), GuildId) of
        [Guild = #guild{}] when Result == ok ->
            NewGuild = Guild#guild{notice = Notice},
            ets:insert(guild_table(), NewGuild),
            {ok, ok};
        [_] ->
            {error, you_are_not_leader};
        _ ->
            {error, no_such_guild}
    end.


%% @doc add guild wealth
-spec add_guild_wealth(GuildId :: non_neg_integer(), AddWealth :: non_neg_integer(), From :: term()) -> {ok, ok} | {error, term()}.
add_guild_wealth(GuildId, AddWealth, _From) ->
    case ets:lookup(guild_table(), GuildId) of
        [Guild = #guild{wealth = Wealth}] ->
            NewGuild = Guild#guild{wealth = Wealth + AddWealth},
            ets:insert(guild_table(), NewGuild),
            {ok, ok};
        _ ->
            {error, no_such_guild}
    end.

%% @doc add role wealth
-spec add_role_wealth(GuildId :: non_neg_integer(), RoleId :: non_neg_integer(), AddWealth :: non_neg_integer(), From :: term()) -> {ok, ok} | {error, term()}.
add_role_wealth(GuildId, RoleId, AddWealth, _From) ->
    RoleTable = role_table(GuildId),
    case ets:lookup(RoleTable, RoleId) of
        [GuildRole = #guild_role{wealth = Wealth}] ->
            NewGuildRole = GuildRole#guild_role{wealth = Wealth + AddWealth},
            ets:insert(RoleTable, NewGuildRole),
            {ok, ok};
        _ ->
            {error, role_not_join_guild}
    end.

%%%===================================================================
%%% common tool
%%%===================================================================

%% @doc check guild role
-spec check_role([GuildRole :: #guild_role{}], List :: list()) -> ok | {error, Reason :: term()}.
check_role(_, []) ->
    ok;
check_role([GuildRole = #guild_role{guild_id = GuildId}], [{guild_id, GuildId} | T]) ->
    check_role([GuildRole], T);
check_role([GuildRole = #guild_role{job = Job}], [{job, ThisJob} | T]) when Job =< ThisJob ->
    check_role([GuildRole], T);
check_role([GuildRole = #guild_role{job = 1}], [leader | T]) ->
    check_role([GuildRole], T);
check_role([GuildRole = #guild_role{job = 2}], [vice | T]) ->
    check_role([GuildRole], T);
check_role([GuildRole = #guild_role{job = 3}], [elite | T]) ->
    check_role([GuildRole], T);
check_role([GuildRole = #guild_role{job = 4}], [assistant | T]) ->
    check_role([GuildRole], T);
check_role([GuildRole = #guild_role{job = 5}], [member | T]) ->
    check_role([GuildRole], T);
check_role([], _) ->
    {error, role};
check_role(_, [{What, _} | _]) ->
    {error, What};
check_role(_, [What | _]) ->
    {error, What}.

%% @doc send data to all online role
-spec broadcast(GuildId :: non_neg_integer(), Data :: binary()) -> ok.
broadcast(GuildId, Data) ->
    ess:walk(fun(RoleId) -> user_sender:send(RoleId, Data) end, role_table(GuildId)).

%% @doc send data to all online role except id
-spec broadcast(GuildId :: non_neg_integer(), Data :: binary(), ExceptId :: non_neg_integer()) -> ok.
broadcast(GuildId, Data, ExceptId) ->
    ess:walk(fun(RoleId) -> RoleId =/= ExceptId andalso user_sender:send(RoleId, Data) == ok end, role_table(GuildId)).

%% @doc role guild status
-spec role_guild_id(RoleId :: non_neg_integer()) -> non_neg_integer().
role_guild_id(RoleId) ->
    [#guild_role{guild_id = GuildId}] = ess:walk_if(fun(GuildId) -> ets:lookup(role_table(GuildId), RoleId) end, guild_table(), [#guild_role{}]),
    GuildId.

%%%===================================================================
%%% ets data set get/set
%%%===================================================================

%% @doc guild ets name
-spec guild_table() -> atom().
guild_table() ->
    guild.

%% @doc guild role ets name
-spec role_table(GuildId :: non_neg_integer()) -> atom().
role_table(GuildId) ->
    binary_to_atom(<<"guild_role_", (integer_to_binary(GuildId))/binary>>, utf8).

%% @doc guild apply ets name
-spec apply_table(GuildId :: non_neg_integer()) -> atom().
apply_table(GuildId) ->
    binary_to_atom(<<"guild_apply_", (integer_to_binary(GuildId))/binary>>, utf8).

%% @doc find guild
-spec get_guild(GuildId :: non_neg_integer()) -> #guild{}.
get_guild(GuildId) ->
    case ets:lookup(guild_table(), GuildId) of
        [Guild] ->
            Guild;
        [] ->
            #guild{}
    end.

%% @doc find role
-spec get_role(RoleId :: non_neg_integer(), GuildId :: non_neg_integer()) -> #guild_role{}.
get_role(RoleId, GuildId) ->
    case ets:lookup(role_table(GuildId), RoleId) of
        [Role] ->
            Role;
        [] ->
            #guild_role{}
    end.

%% @doc find apply
-spec get_apply(RoleId :: non_neg_integer(), GuildId :: non_neg_integer()) -> [#guild_apply{}].
get_apply(RoleId, GuildId) ->
    ets:lookup(apply_table(GuildId), RoleId).

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%%------------------------------------------------------------------
%%% @doc
%%% module battle role
%%% @end
%%%------------------------------------------------------------------
-module(battle_role).
%% API
-export([attack/4]).
%% Includes
-include("common.hrl").
-include("map.hrl").
-include("attribute.hrl").
-include("skill.hrl").
-include("protocol.hrl").
%%%==================================================================
%%% API functions
%%%==================================================================
%% @doc attack start
-spec attack(State :: #map_state{}, AttackerId :: non_neg_integer(), SkillId :: non_neg_integer(), TargetList :: [non_neg_integer()]) -> {ok, #map_state{}} | error().
attack(State, AttackerId, SkillId, TargetList) ->
    Now = time:ts(),
    case validate_attacker(State, AttackerId, SkillId, Now) of
        {ok, Attacker, Skill} ->
            case validate_target(State, Attacker, Skill, TargetList) of
                ok ->
                    perform_skill(State, Attacker, Skill, TargetList, Now);
                Error ->
                    Error
            end;
        Error ->
            Error
    end.

%% validate attacker
validate_attacker(State = #map_state{fighters = Fighters}, AttackerId, SkillId, Now) ->
    case lists:keyfind(AttackerId, #fighter.id, Fighters) of
        Attacker = #fighter{attribute = #attribute{}} ->
            %% check attacker can attack or not
            case battle_attribute:check(Attacker, cannot_attack) of
                false ->
                    validate_skill(State, Attacker, SkillId, Now);
                true ->
                    {error, cannot_attack}
            end;
        false ->
            {error, no_such_attacker}
    end.

%% validate attacker battle skill
validate_skill(_State, Attacker = #fighter{skills = Skills}, SkillId, Now) ->
    case lists:keyfind(SkillId, #battle_skill.skill_id, Skills) of
        Skill = #battle_skill{cd = Cd} when Cd =< Now ->
            {ok, Attacker, Skill};
        #battle_skill{} ->
            {error, skill_cd};
        false ->
            {error, no_such_skill}
    end.

%% check skill effect number great then or equals target number
validate_target(_State, _Attacker, #battle_skill{number = Number}, TargetList) ->
    case length(TargetList) =< Number of
        true ->
            ok;
        false ->
            {error, too_many_target}
    end.

%% perform skill
perform_skill(State, Attacker = #fighter{id = Id, sender_pid = SenderPid, skills = Skills, x = X, y = Y}, Skill = #battle_skill{skill_id = SkillId}, TargetList, Now) ->
    {NewState = #map_state{fighters = Fighters}, NewAttacker, List} = perform_skill_loop(State, Attacker, Skill, TargetList, []),
    %% update skill cd
    NewSkills = lists:keyreplace(SkillId, #battle_skill.skill_id, Skills, Skill#battle_skill{time = Now}),
    FinalAttacker = NewAttacker#fighter{skills = NewSkills},
    %% update self data
    user_sender:send(SenderPid, ?PROTOCOL_MAP_SELF, FinalAttacker),
    %% update attacker
    NewFighters = lists:keyreplace(Id, #fighter.id, Fighters, FinalAttacker),
    %% notify target data to client
    {ok, Binary} = user_sender:send(SenderPid, ?PROTOCOL_MAP_FIGHTER, List),
    map:notify(NewState, X, Y, Binary),
    %% return new state
    {ok, NewState#map_state{fighters = NewFighters}}.

%% perform skill for each one target
perform_skill_loop(State, Attacker, _, [], List) ->
    {State, Attacker, List};
perform_skill_loop(State = #map_state{fighters = Fighters}, Attacker = #fighter{id = Id, camp = Camp}, Skill = #battle_skill{distance = Distance}, [TargetId | TargetList], List) ->
    case lists:keyfind(TargetId, #fighter.id, Fighters) of
        false ->
            %% no such target
            perform_skill_loop(State, Attacker, Skill, TargetList, []);
        #fighter{attribute = #attribute{hp = 0}} ->
            %% filter dead object
            perform_skill_loop(State, Attacker, Skill, TargetList, []);
        #fighter{camp = Camp} ->
            %% same camp
            perform_skill_loop(State, Attacker, Skill, TargetList, []);
        Target = #fighter{hatreds = Hatreds} ->
            %% check attacker can be-attack or not, and check they distance
            case not battle_attribute:check(Target, cannot_be_attack) and map:is_in_distance(Attacker, Target, Distance) of
                true ->
                    %% base attribute hurt
                    Hurt = battle_attribute:calculate_hurt(Attacker, Target),
                    %% perform skill, execute skill effect
                    {NewState, NewAttacker, NewTarget, NewHurt} = battle_skill:perform(State, Attacker, Target, Skill, Hurt),
                    %% perform passive skill, execute skill effect
                    {FinalState, FinalAttacker, FinalTarget, _FinalHurt} = battle_skill:perform_passive(NewState, NewAttacker, NewTarget, Skill, NewHurt),
                    %% update target
                    NewHatreds = lists:sublist([#hatred{id = Id, type = ?MAP_OBJECT_ROLE} | Hatreds], 3),
                    NewFighters = lists:keyreplace(TargetId, #fighter.id, Fighters, FinalTarget#fighter{hatreds = NewHatreds}),
                    %% continue
                    perform_skill_loop(FinalState#map_state{fighters = NewFighters}, FinalAttacker, Skill, TargetList, [FinalTarget | List]);
                false ->
                    perform_skill_loop(State, Attacker, Skill, TargetList, [])
            end
    end;
perform_skill_loop(State, Attacker, Skill, [_ | TargetList], List) ->
    %% health point less then or equal zero state
    perform_skill_loop(State, Attacker, Skill, TargetList, List).


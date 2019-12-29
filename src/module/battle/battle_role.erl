%%%------------------------------------------------------------------
%%% @doc
%%% module battle fighter
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
-spec attack(State :: #map_state{}, AttackerId :: non_neg_integer(), SkillId :: non_neg_integer(), DefenderIdList :: [non_neg_integer()]) -> {ok, #map_state{}} | error().
attack(State, AttackerId, SkillId, DefenderIdList) ->
    Now = time:ts(),
    case validate_attacker(State, AttackerId, SkillId, Now) of
        {ok, Attacker, Skill, SkillData = #skill_data{distance = Distance, number = Number}} ->
            case validate_target(State, Attacker, Distance, Number, DefenderIdList) of
                {ok, TargetList} ->
                    perform_skill(State, Attacker, Skill, SkillData, TargetList, Now);
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
                    {error, 0}
            end;
        false ->
            {error, 0}
    end.

%% validate attacker battle skill
validate_skill(_State, Attacker = #fighter{skills = Skills}, SkillId, Now) ->
    case skill_data:get(SkillId) of
        SkillData = #skill_data{} ->
            case lists:keyfind(SkillId, #battle_skill.skill_id, Skills) of
                Skill = #battle_skill{cd = Cd} when Cd =< Now ->
                    {ok, Attacker, Skill, SkillData};
                #battle_skill{} ->
                    {error, 0};
                false ->
                    {error, 0}
            end;
        _ ->
            {error, 0}
    end.

%% check skill effect number great then or equals target number
validate_target(State, Attacker, Distance, Number, DefenderIdList) ->
    case length(DefenderIdList) =< Number of
        true ->
            validate_target_loop(State, Attacker, Distance, DefenderIdList, []);
        false ->
            {error, 0}
    end.

%% check target exists, hp great then zero and they in skill effect distance
validate_target_loop(_, _, _, [], List) ->
    {ok, List};
validate_target_loop(_State, #fighter{id = Id}, _Distance, [{Id, _} | _T], _List) ->
    %% self
    {error, 0};
validate_target_loop(State = #map_state{fighters = Fighters}, Attacker = #fighter{camp = Camp}, Distance, [Id | T], List) ->
    case lists:keymember(Id, #fighter.id, Fighters) of
        false ->
            %% no such target
            {error, 0};
        #fighter{attribute = #attribute{hp = 0}} ->
            %% filter dead object
            validate_target_loop(State, Attacker, Distance, T, List);
        #fighter{camp = Camp} ->
            %% same camp
            validate_target_loop(State, Attacker, Distance, T, List);
        Fighter = #fighter{} ->
            %% check attacker can be-attack or not
            case battle_attribute:check(Fighter, cannot_be_attack) of
                false ->
                    %% check they distance
                    case map:is_in_distance(Attacker, Fighter, Distance) of
                        true ->
                            validate_target_loop(State, Attacker, Distance, T, [Fighter | List]);
                        false ->
                            {error, 0}
                    end;
                true ->
                    {error, 0}
            end
    end.

%% perform skill
perform_skill(State = #map_state{fighters = Fighters}, Attacker = #fighter{id = Id, sender_pid = SenderPid, skills = Skills}, Skill, #skill_data{cd = Cd}, TargetList, Now) ->
    {NewState, List} = perform_skill_loop(State, Attacker, Skill, TargetList, []),
    %% update skill cd
    NewSkill = #battle_skill{cd = Cd + Now},
    NewAttacker = Attacker#fighter{skills = lists:keyreplace(Id, #battle_skill.skill_id, Skills, NewSkill)},
    %% update self data
    user_sender:send(SenderPid, ?PROTOCOL_MAP_SELF, NewAttacker),
    %% update attacker
    NewFighters = lists:keyreplace(Id, #fighter.id, Fighters, NewAttacker),
    %% notify target data to client
    {ok, Binary} = user_sender:send(SenderPid, ?PROTOCOL_MAP_FIGHTER, List),
    map:broadcast(NewState, Binary),
    %% return new state
    {ok, NewState#map_state{fighters = NewFighters}}.

%% perform skill for each one target
perform_skill_loop(State, _, _, [], List) ->
    {State, List};
perform_skill_loop(State = #map_state{fighters = Fighters}, Attacker, Skill, [Target = #fighter{id = Id} | TargetList], List) ->
    %% base attribute hurt
    Hurt = battle_attribute:calculate_hurt(Attacker, Target),
    %% perform skill, execute skill effect
    {NewState, NewAttacker, NewTarget, NewHurt} = battle_skill:perform(State, Attacker, Target, Skill, Hurt),
    %% perform passive skill, execute skill effect
    {FinalState, FinalAttacker, FinalTarget} = battle_skill:perform_passive(NewState, NewAttacker, NewTarget, Skill, NewHurt),
    %% update self
    NewFighters = lists:keyreplace(Id, #fighter.id, Fighters, FinalAttacker),
    %% update target
    FinalFighters = lists:keyreplace(Id, #fighter.id, NewFighters, FinalTarget),
    %% continue
    perform_skill_loop(FinalState#map_state{fighters = FinalFighters}, Attacker, Skill, TargetList, [FinalTarget | List]);
perform_skill_loop(State, Attacker, Skill, [_ | TargetList], List) ->
    %% health point less then or equal zero state
    perform_skill_loop(State, Attacker, Skill, TargetList, List).


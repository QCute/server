%%%-------------------------------------------------------------------
%%% @doc
%%% module attribute
%%% @end
%%%-------------------------------------------------------------------
-module(attribute).
%% API
-export([merge/1, merge/2]).
-export_type([attribute/0]).
%% includes
-include("attribute.hrl").
-type attribute() :: {Key :: non_neg_integer(), Value :: non_neg_integer()}.
%%%===================================================================
%%% API
%%%===================================================================
%% @doc merge
-spec merge(Attribute :: [#attribute{}] | #attribute{} | [attribute()] | attribute()) -> #attribute{}.
merge(Attribute) ->
    merge(Attribute, #attribute{}).

%% @doc merge, single and list value compatible
-spec merge(X :: [#attribute{}] | #attribute{}, Y :: #attribute{}) -> #attribute{}.
merge(H = #attribute{}, Attribute) ->
    merge_record(H, Attribute);
merge([], Attribute) ->
    Attribute;
merge([H = #attribute{} | T], Attribute) ->
    New = merge_record(H, Attribute),
    merge(T, New);
merge([H = {_, _} | T], Attribute) ->
    New = merge_kv(H, Attribute),
    merge(T, New);
merge([_ | T], Attribute) ->
    merge(T, Attribute).

%% merge with #attribute record type data
merge_record(X, Y) ->
    Y#attribute{
        power = X#attribute.power + Y#attribute.power,
        dexterity = X#attribute.dexterity + Y#attribute.dexterity,
        vitality = X#attribute.vitality + Y#attribute.vitality,
        intellect = X#attribute.intellect + Y#attribute.intellect,
        attack = X#attribute.attack + Y#attribute.attack,
        attack_min = X#attribute.attack_min + Y#attribute.attack_min,
        attack_max = X#attribute.attack_max + Y#attribute.attack_max,
        defense = X#attribute.defense + Y#attribute.defense,
        total_hp = X#attribute.total_hp + Y#attribute.total_hp,
        hit = X#attribute.hit + Y#attribute.hit,
        duck = X#attribute.duck + Y#attribute.duck,
        hit_rate = X#attribute.hit_rate + Y#attribute.hit_rate,
        duck_rate = X#attribute.duck_rate + Y#attribute.duck_rate,
        attack_speed = X#attribute.attack_speed + Y#attribute.attack_speed,
        skill_hurt_per = X#attribute.skill_hurt_per + Y#attribute.skill_hurt_per,
        skill_hurt = X#attribute.skill_hurt + Y#attribute.skill_hurt,
        hurt_add_per = X#attribute.hurt_add_per + Y#attribute.hurt_add_per,
        hurt_add_per_4_show = X#attribute.hurt_add_per_4_show + Y#attribute.hurt_add_per_4_show,
        hurt_dec_per = X#attribute.hurt_dec_per + Y#attribute.hurt_dec_per,
        attack_fixed = X#attribute.attack_fixed + Y#attribute.attack_fixed,
        defense_fixed = X#attribute.defense_fixed + Y#attribute.defense_fixed,
        ignore_def_rate = X#attribute.ignore_def_rate + Y#attribute.ignore_def_rate,
        resist_ignore_def = X#attribute.resist_ignore_def + Y#attribute.resist_ignore_def,
        power_hit_rate = X#attribute.power_hit_rate + Y#attribute.power_hit_rate,
        diligence_rate = X#attribute.diligence_rate + Y#attribute.diligence_rate,
        power_hit_add_per = X#attribute.power_hit_add_per + Y#attribute.power_hit_add_per,
        power_hit_dec_per = X#attribute.power_hit_dec_per + Y#attribute.power_hit_dec_per,
        power_hit_add_fixed = X#attribute.power_hit_add_fixed + Y#attribute.power_hit_add_fixed,
        power_hit_dec_fixed = X#attribute.power_hit_dec_fixed + Y#attribute.power_hit_dec_fixed,
        move_speed = X#attribute.move_speed + Y#attribute.move_speed,
        critical_hit_rate = X#attribute.critical_hit_rate + Y#attribute.critical_hit_rate,
        resist_critical_hit = X#attribute.resist_critical_hit + Y#attribute.resist_critical_hit,
        critical_hit_add_per = X#attribute.critical_hit_add_per + Y#attribute.critical_hit_add_per,
        critical_hit_dec_per = X#attribute.critical_hit_dec_per + Y#attribute.critical_hit_dec_per,
        critical_hit_add_fixed = X#attribute.critical_hit_add_fixed + Y#attribute.critical_hit_add_fixed,
        critical_hit_dec_fixed = X#attribute.critical_hit_dec_fixed + Y#attribute.critical_hit_dec_fixed,
        total_mp = X#attribute.total_mp + Y#attribute.total_mp,
        magic_defense = X#attribute.magic_defense + Y#attribute.magic_defense,
        ignore_strike_hurt_add_per = X#attribute.ignore_strike_hurt_add_per + Y#attribute.ignore_strike_hurt_add_per,
        ignore_strike_hurt_dec_per = X#attribute.ignore_strike_hurt_dec_per + Y#attribute.ignore_strike_hurt_dec_per,
        act_hurt_max = X#attribute.act_hurt_max + Y#attribute.act_hurt_max,
        act_hurt_min = X#attribute.act_hurt_min + Y#attribute.act_hurt_min,
        target_hurt_max = X#attribute.target_hurt_max + Y#attribute.target_hurt_max,
        target_hurt_min = X#attribute.target_hurt_min + Y#attribute.target_hurt_min,
        paralysis = X#attribute.paralysis + Y#attribute.paralysis,
        resist_paralysis = X#attribute.resist_paralysis + Y#attribute.resist_paralysis,
        reduce_speed = X#attribute.reduce_speed + Y#attribute.reduce_speed,
        resist_reduce_speed = X#attribute.resist_reduce_speed + Y#attribute.resist_reduce_speed,
        vertigo = X#attribute.vertigo + Y#attribute.vertigo,
        resist_vertigo = X#attribute.resist_vertigo + Y#attribute.resist_vertigo,
        silence = X#attribute.silence + Y#attribute.silence,
        resist_silence = X#attribute.resist_silence + Y#attribute.resist_silence,
        shuck_hp_per = X#attribute.shuck_hp_per + Y#attribute.shuck_hp_per,
        kill_mon_exp = X#attribute.kill_mon_exp + Y#attribute.kill_mon_exp,
        kill_mon_copper = X#attribute.kill_mon_copper + Y#attribute.kill_mon_copper,
        parry_per = X#attribute.parry_per + Y#attribute.parry_per,
        skill_hurt_add_per = X#attribute.skill_hurt_add_per + Y#attribute.skill_hurt_add_per,
        attack_add_hp_fixed = X#attribute.attack_add_hp_fixed + Y#attribute.attack_add_hp_fixed,
        combo_attack_rate = X#attribute.combo_attack_rate + Y#attribute.combo_attack_rate,
        resist_control = X#attribute.resist_control + Y#attribute.resist_control,
        attack_fixed_by_level = X#attribute.attack_fixed_by_level + Y#attribute.attack_fixed_by_level,
        attack_add_hp_fixed_by_level = X#attribute.attack_add_hp_fixed_by_level + Y#attribute.attack_add_hp_fixed_by_level,
        attack_add_hp_fixed_only_pvp = X#attribute.attack_add_hp_fixed_only_pvp + Y#attribute.attack_add_hp_fixed_only_pvp,
        ack_weapon = X#attribute.ack_weapon + Y#attribute.ack_weapon,
        ack_jewelry = X#attribute.ack_jewelry + Y#attribute.ack_jewelry,
        def_armor = X#attribute.def_armor + Y#attribute.def_armor,
        hp_armor = X#attribute.hp_armor + Y#attribute.hp_armor,
        ack_elements = X#attribute.ack_elements + Y#attribute.ack_elements,
        def_elements = X#attribute.def_elements + Y#attribute.def_elements,
        base_hp = X#attribute.base_hp + Y#attribute.base_hp,
        counter_ack_fixed = X#attribute.counter_ack_fixed + Y#attribute.counter_ack_fixed,
        counter_ack_per = X#attribute.counter_ack_per + Y#attribute.counter_ack_per,
        ignore_strike_rate = X#attribute.ignore_strike_rate + Y#attribute.ignore_strike_rate,
        add_hp_per_3 = X#attribute.add_hp_per_3 + Y#attribute.add_hp_per_3,
        add_att_per_3 = X#attribute.add_att_per_3 + Y#attribute.add_att_per_3,
        add_def_per_3 = X#attribute.add_def_per_3 + Y#attribute.add_def_per_3,
        add_hp_per_2 = X#attribute.add_hp_per_2 + Y#attribute.add_hp_per_2,
        add_att_per_2 = X#attribute.add_att_per_2 + Y#attribute.add_att_per_2,
        add_def_per_2 = X#attribute.add_def_per_2 + Y#attribute.add_def_per_2,
        add_hp_per_1 = X#attribute.add_hp_per_1 + Y#attribute.add_hp_per_1,
        add_att_per_1 = X#attribute.add_att_per_1 + Y#attribute.add_att_per_1,
        add_def_per_1 = X#attribute.add_def_per_1 + Y#attribute.add_def_per_1,
        attack_add_hp_per = X#attribute.attack_add_hp_per + Y#attribute.attack_add_hp_per,
        be_attack_add_hp_per = X#attribute.be_attack_add_hp_per + Y#attribute.be_attack_add_hp_per,
        wd = X#attribute.wd + Y#attribute.wd,
        hp_fastening = X#attribute.hp_fastening + Y#attribute.hp_fastening,
        is_multiple_hurt = X#attribute.is_multiple_hurt + Y#attribute.is_multiple_hurt,
        passive_add_min_attack = X#attribute.passive_add_min_attack + Y#attribute.passive_add_min_attack,
        passive_add_duck_by_dex = X#attribute.passive_add_duck_by_dex + Y#attribute.passive_add_duck_by_dex,
        passive_add_attack_by_dex = X#attribute.passive_add_attack_by_dex + Y#attribute.passive_add_attack_by_dex,
        passive_add_def_by_pow = X#attribute.passive_add_def_by_pow + Y#attribute.passive_add_def_by_pow,
        passive_add_hp_by_int = X#attribute.passive_add_hp_by_int + Y#attribute.passive_add_hp_by_int,
        passive_add_hp_by_per = X#attribute.passive_add_hp_by_per + Y#attribute.passive_add_hp_by_per,
        passive_fan_recover_be_hit = X#attribute.passive_fan_recover_be_hit + Y#attribute.passive_fan_recover_be_hit,
        passive_power_hit_must_next = X#attribute.passive_power_hit_must_next + Y#attribute.passive_power_hit_must_next,
        passive_add_skill_hurt_when_duck = X#attribute.passive_add_skill_hurt_when_duck + Y#attribute.passive_add_skill_hurt_when_duck,
        passive_add_counter_ack_by_pow = X#attribute.passive_add_counter_ack_by_pow + Y#attribute.passive_add_counter_ack_by_pow,
        passive_add_buff_when_low_hp = X#attribute.passive_add_buff_when_low_hp + Y#attribute.passive_add_buff_when_low_hp,
        passive_protect = X#attribute.passive_protect + Y#attribute.passive_protect,
        reborn = X#attribute.reborn + Y#attribute.reborn,
        shield_can_boom = X#attribute.shield_can_boom + Y#attribute.shield_can_boom,
        use_skill_when_dead = X#attribute.use_skill_when_dead + Y#attribute.use_skill_when_dead,
        pet_protect_per = X#attribute.pet_protect_per + Y#attribute.pet_protect_per,
        pet_dead_boom = X#attribute.pet_dead_boom + Y#attribute.pet_dead_boom,
        speed = X#attribute.speed + Y#attribute.speed,
        hp_max = X#attribute.hp_max + Y#attribute.hp_max,
        mp_max = X#attribute.mp_max + Y#attribute.mp_max,
        atk_speed = X#attribute.atk_speed + Y#attribute.atk_speed,
        physic_dmg = X#attribute.physic_dmg + Y#attribute.physic_dmg,
        magic_dmg = X#attribute.magic_dmg + Y#attribute.magic_dmg,
        physic_def = X#attribute.physic_def + Y#attribute.physic_def,
        magic_def = X#attribute.magic_def + Y#attribute.magic_def,
        critical = X#attribute.critical + Y#attribute.critical,
        tenacity = X#attribute.tenacity + Y#attribute.tenacity,
        accuracy = X#attribute.accuracy + Y#attribute.accuracy,
        evasion = X#attribute.evasion + Y#attribute.evasion,
        holy_dmg = X#attribute.holy_dmg + Y#attribute.holy_dmg,
        critical_dmg = X#attribute.critical_dmg + Y#attribute.critical_dmg,
        dmg_ratio = X#attribute.dmg_ratio + Y#attribute.dmg_ratio,
        def_ratio = X#attribute.def_ratio + Y#attribute.def_ratio,
        enhance_control = X#attribute.enhance_control + Y#attribute.enhance_control,
        anti_control = X#attribute.anti_control + Y#attribute.anti_control,
        escape = X#attribute.escape + Y#attribute.escape,
        anti_escape = X#attribute.anti_escape + Y#attribute.anti_escape,
        capture = X#attribute.capture + Y#attribute.capture,
        physic_def_ratio = X#attribute.physic_def_ratio + Y#attribute.physic_def_ratio,
        magic_def_ratio = X#attribute.magic_def_ratio + Y#attribute.magic_def_ratio,
        physic_accuracy = X#attribute.physic_accuracy + Y#attribute.physic_accuracy,
        magic_accuracy = X#attribute.magic_accuracy + Y#attribute.magic_accuracy,
        physic_evasion = X#attribute.physic_evasion + Y#attribute.physic_evasion,
        magic_evasion = X#attribute.magic_evasion + Y#attribute.magic_evasion,
        physic_critical = X#attribute.physic_critical + Y#attribute.physic_critical,
        magic_critical = X#attribute.magic_critical + Y#attribute.magic_critical,
        physic_tenacity = X#attribute.physic_tenacity + Y#attribute.physic_tenacity,
        magic_tenacity = X#attribute.magic_tenacity + Y#attribute.magic_tenacity,
        heal_val = X#attribute.heal_val + Y#attribute.heal_val,
        heal_ratio = X#attribute.heal_ratio + Y#attribute.heal_ratio,
        eff_heal_ratio = X#attribute.eff_heal_ratio + Y#attribute.eff_heal_ratio,
        physic_dmg_ratio = X#attribute.physic_dmg_ratio + Y#attribute.physic_dmg_ratio,
        magic_dmg_ratio = X#attribute.magic_dmg_ratio + Y#attribute.magic_dmg_ratio,
        fc = X#attribute.fc + Y#attribute.fc,
        strength = X#attribute.strength + Y#attribute.strength,
        constitution = X#attribute.constitution + Y#attribute.constitution,
        magic = X#attribute.magic + Y#attribute.magic,
        agility = X#attribute.agility + Y#attribute.agility,
        endurance = X#attribute.endurance + Y#attribute.endurance,
        exp_ratio = X#attribute.exp_ratio + Y#attribute.exp_ratio
    }.

%% merge with k,v type data
merge_kv({1, Value}, Attribute = #attribute{power = Power}) ->
    Attribute#attribute{power = Power + Value};
merge_kv({2, Value}, Attribute = #attribute{dexterity = Dexterity}) ->
    Attribute#attribute{dexterity = Dexterity + Value};
merge_kv({3, Value}, Attribute = #attribute{vitality = Vitality}) ->
    Attribute#attribute{vitality = Vitality + Value};
merge_kv({4, Value}, Attribute = #attribute{intellect = Intellect}) ->
    Attribute#attribute{intellect = Intellect + Value};
merge_kv({5, Value}, Attribute = #attribute{attack = Attack}) ->
    Attribute#attribute{attack = Attack + Value};
merge_kv({6, Value}, Attribute = #attribute{attack_min = AttackMin}) ->
    Attribute#attribute{attack_min = AttackMin + Value};
merge_kv({7, Value}, Attribute = #attribute{attack_max = AttackMax}) ->
    Attribute#attribute{attack_max = AttackMax + Value};
merge_kv({8, Value}, Attribute = #attribute{defense = Defense}) ->
    Attribute#attribute{defense = Defense + Value};
merge_kv({9, Value}, Attribute = #attribute{total_hp = TotalHp}) ->
    Attribute#attribute{total_hp = TotalHp + Value};
merge_kv({10, Value}, Attribute = #attribute{hit = Hit}) ->
    Attribute#attribute{hit = Hit + Value};
merge_kv({11, Value}, Attribute = #attribute{duck = Duck}) ->
    Attribute#attribute{duck = Duck + Value};
merge_kv({12, Value}, Attribute = #attribute{hit_rate = HitRate}) ->
    Attribute#attribute{hit_rate = HitRate + Value};
merge_kv({13, Value}, Attribute = #attribute{duck_rate = DuckRate}) ->
    Attribute#attribute{duck_rate = DuckRate + Value};
merge_kv({14, Value}, Attribute = #attribute{attack_speed = AttackSpeed}) ->
    Attribute#attribute{attack_speed = AttackSpeed + Value};
merge_kv({15, Value}, Attribute = #attribute{skill_hurt_per = SkillHurtPer}) ->
    Attribute#attribute{skill_hurt_per = SkillHurtPer + Value};
merge_kv({16, Value}, Attribute = #attribute{skill_hurt = SkillHurt}) ->
    Attribute#attribute{skill_hurt = SkillHurt + Value};
merge_kv({17, Value}, Attribute = #attribute{hurt_add_per = HurtAddPer}) ->
    Attribute#attribute{hurt_add_per = HurtAddPer + Value};
merge_kv({18, Value}, Attribute = #attribute{hurt_add_per_4_show = HurtAddPer4Show}) ->
    Attribute#attribute{hurt_add_per_4_show = HurtAddPer4Show + Value};
merge_kv({19, Value}, Attribute = #attribute{hurt_dec_per = HurtDecPer}) ->
    Attribute#attribute{hurt_dec_per = HurtDecPer + Value};
merge_kv({20, Value}, Attribute = #attribute{attack_fixed = AttackFixed}) ->
    Attribute#attribute{attack_fixed = AttackFixed + Value};
merge_kv({21, Value}, Attribute = #attribute{defense_fixed = DefenseFixed}) ->
    Attribute#attribute{defense_fixed = DefenseFixed + Value};
merge_kv({22, Value}, Attribute = #attribute{ignore_def_rate = IgnoreDefRate}) ->
    Attribute#attribute{ignore_def_rate = IgnoreDefRate + Value};
merge_kv({23, Value}, Attribute = #attribute{resist_ignore_def = ResistIgnoreDef}) ->
    Attribute#attribute{resist_ignore_def = ResistIgnoreDef + Value};
merge_kv({24, Value}, Attribute = #attribute{power_hit_rate = PowerHitRate}) ->
    Attribute#attribute{power_hit_rate = PowerHitRate + Value};
merge_kv({25, Value}, Attribute = #attribute{diligence_rate = DiligenceRate}) ->
    Attribute#attribute{diligence_rate = DiligenceRate + Value};
merge_kv({26, Value}, Attribute = #attribute{power_hit_add_per = PowerHitAddPer}) ->
    Attribute#attribute{power_hit_add_per = PowerHitAddPer + Value};
merge_kv({27, Value}, Attribute = #attribute{power_hit_dec_per = PowerHitDecPer}) ->
    Attribute#attribute{power_hit_dec_per = PowerHitDecPer + Value};
merge_kv({28, Value}, Attribute = #attribute{power_hit_add_fixed = PowerHitAddFixed}) ->
    Attribute#attribute{power_hit_add_fixed = PowerHitAddFixed + Value};
merge_kv({29, Value}, Attribute = #attribute{power_hit_dec_fixed = PowerHitDecFixed}) ->
    Attribute#attribute{power_hit_dec_fixed = PowerHitDecFixed + Value};
merge_kv({30, Value}, Attribute = #attribute{move_speed = MoveSpeed}) ->
    Attribute#attribute{move_speed = MoveSpeed + Value};
merge_kv({31, Value}, Attribute = #attribute{critical_hit_rate = CriticalHitRate}) ->
    Attribute#attribute{critical_hit_rate = CriticalHitRate + Value};
merge_kv({32, Value}, Attribute = #attribute{resist_critical_hit = ResistCriticalHit}) ->
    Attribute#attribute{resist_critical_hit = ResistCriticalHit + Value};
merge_kv({33, Value}, Attribute = #attribute{critical_hit_add_per = CriticalHitAddPer}) ->
    Attribute#attribute{critical_hit_add_per = CriticalHitAddPer + Value};
merge_kv({34, Value}, Attribute = #attribute{critical_hit_dec_per = CriticalHitDecPer}) ->
    Attribute#attribute{critical_hit_dec_per = CriticalHitDecPer + Value};
merge_kv({35, Value}, Attribute = #attribute{critical_hit_add_fixed = CriticalHitAddFixed}) ->
    Attribute#attribute{critical_hit_add_fixed = CriticalHitAddFixed + Value};
merge_kv({36, Value}, Attribute = #attribute{critical_hit_dec_fixed = CriticalHitDecFixed}) ->
    Attribute#attribute{critical_hit_dec_fixed = CriticalHitDecFixed + Value};
merge_kv({37, Value}, Attribute = #attribute{total_mp = TotalMp}) ->
    Attribute#attribute{total_mp = TotalMp + Value};
merge_kv({38, Value}, Attribute = #attribute{magic_defense = MagicDefense}) ->
    Attribute#attribute{magic_defense = MagicDefense + Value};
merge_kv({39, Value}, Attribute = #attribute{ignore_strike_hurt_add_per = IgnoreStrikeHurtAddPer}) ->
    Attribute#attribute{ignore_strike_hurt_add_per = IgnoreStrikeHurtAddPer + Value};
merge_kv({40, Value}, Attribute = #attribute{ignore_strike_hurt_dec_per = IgnoreStrikeHurtDecPer}) ->
    Attribute#attribute{ignore_strike_hurt_dec_per = IgnoreStrikeHurtDecPer + Value};
merge_kv({41, Value}, Attribute = #attribute{act_hurt_max = ActHurtMax}) ->
    Attribute#attribute{act_hurt_max = ActHurtMax + Value};
merge_kv({42, Value}, Attribute = #attribute{act_hurt_min = ActHurtMin}) ->
    Attribute#attribute{act_hurt_min = ActHurtMin + Value};
merge_kv({43, Value}, Attribute = #attribute{target_hurt_max = TargetHurtMax}) ->
    Attribute#attribute{target_hurt_max = TargetHurtMax + Value};
merge_kv({44, Value}, Attribute = #attribute{target_hurt_min = TargetHurtMin}) ->
    Attribute#attribute{target_hurt_min = TargetHurtMin + Value};
merge_kv({45, Value}, Attribute = #attribute{paralysis = Paralysis}) ->
    Attribute#attribute{paralysis = Paralysis + Value};
merge_kv({46, Value}, Attribute = #attribute{resist_paralysis = ResistParalysis}) ->
    Attribute#attribute{resist_paralysis = ResistParalysis + Value};
merge_kv({47, Value}, Attribute = #attribute{reduce_speed = ReduceSpeed}) ->
    Attribute#attribute{reduce_speed = ReduceSpeed + Value};
merge_kv({48, Value}, Attribute = #attribute{resist_reduce_speed = ResistReduceSpeed}) ->
    Attribute#attribute{resist_reduce_speed = ResistReduceSpeed + Value};
merge_kv({49, Value}, Attribute = #attribute{vertigo = Vertigo}) ->
    Attribute#attribute{vertigo = Vertigo + Value};
merge_kv({50, Value}, Attribute = #attribute{resist_vertigo = ResistVertigo}) ->
    Attribute#attribute{resist_vertigo = ResistVertigo + Value};
merge_kv({51, Value}, Attribute = #attribute{silence = Silence}) ->
    Attribute#attribute{silence = Silence + Value};
merge_kv({52, Value}, Attribute = #attribute{resist_silence = ResistSilence}) ->
    Attribute#attribute{resist_silence = ResistSilence + Value};
merge_kv({53, Value}, Attribute = #attribute{shuck_hp_per = ShuckHpPer}) ->
    Attribute#attribute{shuck_hp_per = ShuckHpPer + Value};
merge_kv({54, Value}, Attribute = #attribute{kill_mon_exp = KillMonExp}) ->
    Attribute#attribute{kill_mon_exp = KillMonExp + Value};
merge_kv({55, Value}, Attribute = #attribute{kill_mon_copper = KillMonCopper}) ->
    Attribute#attribute{kill_mon_copper = KillMonCopper + Value};
merge_kv({56, Value}, Attribute = #attribute{parry_per = ParryPer}) ->
    Attribute#attribute{parry_per = ParryPer + Value};
merge_kv({57, Value}, Attribute = #attribute{skill_hurt_add_per = SkillHurtAddPer}) ->
    Attribute#attribute{skill_hurt_add_per = SkillHurtAddPer + Value};
merge_kv({58, Value}, Attribute = #attribute{attack_add_hp_fixed = AttackAddHpFixed}) ->
    Attribute#attribute{attack_add_hp_fixed = AttackAddHpFixed + Value};
merge_kv({59, Value}, Attribute = #attribute{combo_attack_rate = ComboAttackRate}) ->
    Attribute#attribute{combo_attack_rate = ComboAttackRate + Value};
merge_kv({60, Value}, Attribute = #attribute{resist_control = ResistControl}) ->
    Attribute#attribute{resist_control = ResistControl + Value};
merge_kv({61, Value}, Attribute = #attribute{attack_fixed_by_level = AttackFixedByLevel}) ->
    Attribute#attribute{attack_fixed_by_level = AttackFixedByLevel + Value};
merge_kv({62, Value}, Attribute = #attribute{attack_add_hp_fixed_by_level = AttackAddHpFixedByLevel}) ->
    Attribute#attribute{attack_add_hp_fixed_by_level = AttackAddHpFixedByLevel + Value};
merge_kv({63, Value}, Attribute = #attribute{attack_add_hp_fixed_only_pvp = AttackAddHpFixedOnlyPvp}) ->
    Attribute#attribute{attack_add_hp_fixed_only_pvp = AttackAddHpFixedOnlyPvp + Value};
merge_kv({64, Value}, Attribute = #attribute{ack_weapon = AckWeapon}) ->
    Attribute#attribute{ack_weapon = AckWeapon + Value};
merge_kv({65, Value}, Attribute = #attribute{ack_jewelry = AckJewelry}) ->
    Attribute#attribute{ack_jewelry = AckJewelry + Value};
merge_kv({66, Value}, Attribute = #attribute{def_armor = DefArmor}) ->
    Attribute#attribute{def_armor = DefArmor + Value};
merge_kv({67, Value}, Attribute = #attribute{hp_armor = HpArmor}) ->
    Attribute#attribute{hp_armor = HpArmor + Value};
merge_kv({68, Value}, Attribute = #attribute{ack_elements = AckElements}) ->
    Attribute#attribute{ack_elements = AckElements + Value};
merge_kv({69, Value}, Attribute = #attribute{def_elements = DefElements}) ->
    Attribute#attribute{def_elements = DefElements + Value};
merge_kv({70, Value}, Attribute = #attribute{base_hp = BaseHp}) ->
    Attribute#attribute{base_hp = BaseHp + Value};
merge_kv({71, Value}, Attribute = #attribute{counter_ack_fixed = CounterAckFixed}) ->
    Attribute#attribute{counter_ack_fixed = CounterAckFixed + Value};
merge_kv({72, Value}, Attribute = #attribute{counter_ack_per = CounterAckPer}) ->
    Attribute#attribute{counter_ack_per = CounterAckPer + Value};
merge_kv({73, Value}, Attribute = #attribute{ignore_strike_rate = IgnoreStrikeRate}) ->
    Attribute#attribute{ignore_strike_rate = IgnoreStrikeRate + Value};
merge_kv({74, Value}, Attribute = #attribute{add_hp_per_3 = AddHpPer3}) ->
    Attribute#attribute{add_hp_per_3 = AddHpPer3 + Value};
merge_kv({75, Value}, Attribute = #attribute{add_att_per_3 = AddAttPer3}) ->
    Attribute#attribute{add_att_per_3 = AddAttPer3 + Value};
merge_kv({76, Value}, Attribute = #attribute{add_def_per_3 = AddDefPer3}) ->
    Attribute#attribute{add_def_per_3 = AddDefPer3 + Value};
merge_kv({77, Value}, Attribute = #attribute{add_hp_per_2 = AddHpPer2}) ->
    Attribute#attribute{add_hp_per_2 = AddHpPer2 + Value};
merge_kv({78, Value}, Attribute = #attribute{add_att_per_2 = AddAttPer2}) ->
    Attribute#attribute{add_att_per_2 = AddAttPer2 + Value};
merge_kv({79, Value}, Attribute = #attribute{add_def_per_2 = AddDefPer2}) ->
    Attribute#attribute{add_def_per_2 = AddDefPer2 + Value};
merge_kv({80, Value}, Attribute = #attribute{add_hp_per_1 = AddHpPer1}) ->
    Attribute#attribute{add_hp_per_1 = AddHpPer1 + Value};
merge_kv({81, Value}, Attribute = #attribute{add_att_per_1 = AddAttPer1}) ->
    Attribute#attribute{add_att_per_1 = AddAttPer1 + Value};
merge_kv({82, Value}, Attribute = #attribute{add_def_per_1 = AddDefPer1}) ->
    Attribute#attribute{add_def_per_1 = AddDefPer1 + Value};
merge_kv({83, Value}, Attribute = #attribute{attack_add_hp_per = AttackAddHpPer}) ->
    Attribute#attribute{attack_add_hp_per = AttackAddHpPer + Value};
merge_kv({84, Value}, Attribute = #attribute{be_attack_add_hp_per = BeAttackAddHpPer}) ->
    Attribute#attribute{be_attack_add_hp_per = BeAttackAddHpPer + Value};
merge_kv({85, Value}, Attribute = #attribute{wd = Wd}) ->
    Attribute#attribute{wd = Wd + Value};
merge_kv({86, Value}, Attribute = #attribute{hp_fastening = HpFastening}) ->
    Attribute#attribute{hp_fastening = HpFastening + Value};
merge_kv({87, Value}, Attribute = #attribute{is_multiple_hurt = IsMultipleHurt}) ->
    Attribute#attribute{is_multiple_hurt = IsMultipleHurt + Value};
merge_kv({88, Value}, Attribute = #attribute{passive_add_min_attack = PassiveAddMinAttack}) ->
    Attribute#attribute{passive_add_min_attack = PassiveAddMinAttack + Value};
merge_kv({89, Value}, Attribute = #attribute{passive_add_duck_by_dex = PassiveAddDuckByDex}) ->
    Attribute#attribute{passive_add_duck_by_dex = PassiveAddDuckByDex + Value};
merge_kv({90, Value}, Attribute = #attribute{passive_add_attack_by_dex = PassiveAddAttackByDex}) ->
    Attribute#attribute{passive_add_attack_by_dex = PassiveAddAttackByDex + Value};
merge_kv({91, Value}, Attribute = #attribute{passive_add_def_by_pow = PassiveAddDefByPow}) ->
    Attribute#attribute{passive_add_def_by_pow = PassiveAddDefByPow + Value};
merge_kv({92, Value}, Attribute = #attribute{passive_add_hp_by_int = PassiveAddHpByInt}) ->
    Attribute#attribute{passive_add_hp_by_int = PassiveAddHpByInt + Value};
merge_kv({93, Value}, Attribute = #attribute{passive_add_hp_by_per = PassiveAddHpByPer}) ->
    Attribute#attribute{passive_add_hp_by_per = PassiveAddHpByPer + Value};
merge_kv({94, Value}, Attribute = #attribute{passive_fan_recover_be_hit = PassiveFanRecoverBeHit}) ->
    Attribute#attribute{passive_fan_recover_be_hit = PassiveFanRecoverBeHit + Value};
merge_kv({95, Value}, Attribute = #attribute{passive_power_hit_must_next = PassivePowerHitMustNext}) ->
    Attribute#attribute{passive_power_hit_must_next = PassivePowerHitMustNext + Value};
merge_kv({96, Value}, Attribute = #attribute{passive_add_skill_hurt_when_duck = PassiveAddSkillHurtWhenDuck}) ->
    Attribute#attribute{passive_add_skill_hurt_when_duck = PassiveAddSkillHurtWhenDuck + Value};
merge_kv({97, Value}, Attribute = #attribute{passive_add_counter_ack_by_pow = PassiveAddCounterAckByPow}) ->
    Attribute#attribute{passive_add_counter_ack_by_pow = PassiveAddCounterAckByPow + Value};
merge_kv({98, Value}, Attribute = #attribute{passive_add_buff_when_low_hp = PassiveAddBuffWhenLowHp}) ->
    Attribute#attribute{passive_add_buff_when_low_hp = PassiveAddBuffWhenLowHp + Value};
merge_kv({99, Value}, Attribute = #attribute{passive_protect = PassiveProtect}) ->
    Attribute#attribute{passive_protect = PassiveProtect + Value};
merge_kv({100, Value}, Attribute = #attribute{reborn = Reborn}) ->
    Attribute#attribute{reborn = Reborn + Value};
merge_kv({101, Value}, Attribute = #attribute{shield_can_boom = ShieldCanBoom}) ->
    Attribute#attribute{shield_can_boom = ShieldCanBoom + Value};
merge_kv({102, Value}, Attribute = #attribute{use_skill_when_dead = UseSkillWhenDead}) ->
    Attribute#attribute{use_skill_when_dead = UseSkillWhenDead + Value};
merge_kv({103, Value}, Attribute = #attribute{pet_protect_per = PetProtectPer}) ->
    Attribute#attribute{pet_protect_per = PetProtectPer + Value};
merge_kv({104, Value}, Attribute = #attribute{pet_dead_boom = PetDeadBoom}) ->
    Attribute#attribute{pet_dead_boom = PetDeadBoom + Value};
merge_kv({105, Value}, Attribute = #attribute{speed = Speed}) ->
    Attribute#attribute{speed = Speed + Value};
merge_kv({106, Value}, Attribute = #attribute{hp_max = HpMax}) ->
    Attribute#attribute{hp_max = HpMax + Value};
merge_kv({107, Value}, Attribute = #attribute{mp_max = MpMax}) ->
    Attribute#attribute{mp_max = MpMax + Value};
merge_kv({108, Value}, Attribute = #attribute{atk_speed = AtkSpeed}) ->
    Attribute#attribute{atk_speed = AtkSpeed + Value};
merge_kv({109, Value}, Attribute = #attribute{physic_dmg = PhysicDmg}) ->
    Attribute#attribute{physic_dmg = PhysicDmg + Value};
merge_kv({110, Value}, Attribute = #attribute{magic_dmg = MagicDmg}) ->
    Attribute#attribute{magic_dmg = MagicDmg + Value};
merge_kv({111, Value}, Attribute = #attribute{physic_def = PhysicDef}) ->
    Attribute#attribute{physic_def = PhysicDef + Value};
merge_kv({112, Value}, Attribute = #attribute{magic_def = MagicDef}) ->
    Attribute#attribute{magic_def = MagicDef + Value};
merge_kv({113, Value}, Attribute = #attribute{critical = Critical}) ->
    Attribute#attribute{critical = Critical + Value};
merge_kv({114, Value}, Attribute = #attribute{tenacity = Tenacity}) ->
    Attribute#attribute{tenacity = Tenacity + Value};
merge_kv({115, Value}, Attribute = #attribute{accuracy = Accuracy}) ->
    Attribute#attribute{accuracy = Accuracy + Value};
merge_kv({116, Value}, Attribute = #attribute{evasion = Evasion}) ->
    Attribute#attribute{evasion = Evasion + Value};
merge_kv({117, Value}, Attribute = #attribute{holy_dmg = HolyDmg}) ->
    Attribute#attribute{holy_dmg = HolyDmg + Value};
merge_kv({118, Value}, Attribute = #attribute{critical_dmg = CriticalDmg}) ->
    Attribute#attribute{critical_dmg = CriticalDmg + Value};
merge_kv({119, Value}, Attribute = #attribute{dmg_ratio = DmgRatio}) ->
    Attribute#attribute{dmg_ratio = DmgRatio + Value};
merge_kv({120, Value}, Attribute = #attribute{def_ratio = DefRatio}) ->
    Attribute#attribute{def_ratio = DefRatio + Value};
merge_kv({121, Value}, Attribute = #attribute{enhance_control = EnhanceControl}) ->
    Attribute#attribute{enhance_control = EnhanceControl + Value};
merge_kv({122, Value}, Attribute = #attribute{anti_control = AntiControl}) ->
    Attribute#attribute{anti_control = AntiControl + Value};
merge_kv({123, Value}, Attribute = #attribute{escape = Escape}) ->
    Attribute#attribute{escape = Escape + Value};
merge_kv({124, Value}, Attribute = #attribute{anti_escape = AntiEscape}) ->
    Attribute#attribute{anti_escape = AntiEscape + Value};
merge_kv({125, Value}, Attribute = #attribute{capture = Capture}) ->
    Attribute#attribute{capture = Capture + Value};
merge_kv({126, Value}, Attribute = #attribute{physic_def_ratio = PhysicDefRatio}) ->
    Attribute#attribute{physic_def_ratio = PhysicDefRatio + Value};
merge_kv({127, Value}, Attribute = #attribute{magic_def_ratio = MagicDefRatio}) ->
    Attribute#attribute{magic_def_ratio = MagicDefRatio + Value};
merge_kv({128, Value}, Attribute = #attribute{physic_accuracy = PhysicAccuracy}) ->
    Attribute#attribute{physic_accuracy = PhysicAccuracy + Value};
merge_kv({129, Value}, Attribute = #attribute{magic_accuracy = MagicAccuracy}) ->
    Attribute#attribute{magic_accuracy = MagicAccuracy + Value};
merge_kv({130, Value}, Attribute = #attribute{physic_evasion = PhysicEvasion}) ->
    Attribute#attribute{physic_evasion = PhysicEvasion + Value};
merge_kv({131, Value}, Attribute = #attribute{magic_evasion = MagicEvasion}) ->
    Attribute#attribute{magic_evasion = MagicEvasion + Value};
merge_kv({132, Value}, Attribute = #attribute{physic_critical = PhysicCritical}) ->
    Attribute#attribute{physic_critical = PhysicCritical + Value};
merge_kv({133, Value}, Attribute = #attribute{magic_critical = MagicCritical}) ->
    Attribute#attribute{magic_critical = MagicCritical + Value};
merge_kv({134, Value}, Attribute = #attribute{physic_tenacity = PhysicTenacity}) ->
    Attribute#attribute{physic_tenacity = PhysicTenacity + Value};
merge_kv({135, Value}, Attribute = #attribute{magic_tenacity = MagicTenacity}) ->
    Attribute#attribute{magic_tenacity = MagicTenacity + Value};
merge_kv({136, Value}, Attribute = #attribute{heal_val = HealVal}) ->
    Attribute#attribute{heal_val = HealVal + Value};
merge_kv({137, Value}, Attribute = #attribute{heal_ratio = HealRatio}) ->
    Attribute#attribute{heal_ratio = HealRatio + Value};
merge_kv({138, Value}, Attribute = #attribute{eff_heal_ratio = EffHealRatio}) ->
    Attribute#attribute{eff_heal_ratio = EffHealRatio + Value};
merge_kv({139, Value}, Attribute = #attribute{physic_dmg_ratio = PhysicDmgRatio}) ->
    Attribute#attribute{physic_dmg_ratio = PhysicDmgRatio + Value};
merge_kv({140, Value}, Attribute = #attribute{magic_dmg_ratio = MagicDmgRatio}) ->
    Attribute#attribute{magic_dmg_ratio = MagicDmgRatio + Value};
merge_kv({141, Value}, Attribute = #attribute{fc = Fc}) ->
    Attribute#attribute{fc = Fc + Value};
merge_kv({142, Value}, Attribute = #attribute{strength = Strength}) ->
    Attribute#attribute{strength = Strength + Value};
merge_kv({143, Value}, Attribute = #attribute{constitution = Constitution}) ->
    Attribute#attribute{constitution = Constitution + Value};
merge_kv({144, Value}, Attribute = #attribute{magic = Magic}) ->
    Attribute#attribute{magic = Magic + Value};
merge_kv({145, Value}, Attribute = #attribute{agility = Agility}) ->
    Attribute#attribute{agility = Agility + Value};
merge_kv({146, Value}, Attribute = #attribute{endurance = Endurance}) ->
    Attribute#attribute{endurance = Endurance + Value};
merge_kv({147, Value}, Attribute = #attribute{exp_ratio = ExpRatio}) ->
    Attribute#attribute{exp_ratio = ExpRatio + Value};
merge_kv(_, Attribute) ->
    Attribute.

-module(skill_data).
-export([get/1]).

-include("skill.hrl").

-spec get(SkillId :: integer()) -> SkillData :: #skill_data{} | Default :: [].
get(1) ->
    #skill_data{skill_id = 1, type = active, classes = 0, name = <<"普攻技能"/utf8>>, condition = [], cost = [], attribute = [], effect = [1], cd = 1, radius = 1000, distance = 1000, number = 1, buffs = [], before_effects = [], hit_effects = [], after_effects = [], description = <<"对目标造成180%的伤害"/utf8>>};
get(2) ->
    #skill_data{skill_id = 2, type = active, classes = 0, name = <<"群攻技能"/utf8>>, condition = [], cost = [], attribute = [], effect = [2], cd = 1, radius = 1000, distance = 1000, number = 30, buffs = [], before_effects = [], hit_effects = [], after_effects = [], description = <<"对3个目标造成150%的伤害"/utf8>>};
get(3) ->
    #skill_data{skill_id = 3, type = passive, classes = 0, name = <<"增益"/utf8>>, condition = [], cost = [], attribute = [], effect = [8], cd = 10, radius = 1, distance = 1, number = 1, buffs = [], before_effects = [], hit_effects = [], after_effects = [], description = <<"每秒扣血，总血量万分之50"/utf8>>};
get(4) ->
    #skill_data{skill_id = 4, type = active, classes = 0, name = <<"增益"/utf8>>, condition = [], cost = [], attribute = [], effect = [8], cd = 10, radius = 1, distance = 1, number = 1, buffs = [], before_effects = [], hit_effects = [], after_effects = [], description = <<"每秒扣血，总血量万分之50"/utf8>>};
get(5) ->
    #skill_data{skill_id = 5, type = active, classes = 0, name = <<"普攻技能"/utf8>>, condition = [], cost = [], attribute = [], effect = [1], cd = 1, radius = 1, distance = 1, number = 1, buffs = [], before_effects = [], hit_effects = [], after_effects = [], description = <<"普通技能"/utf8>>};
get(_) ->
    [].



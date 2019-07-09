%%%-------------------------------------------------------------------
%%% @doc
%%% attribute define
%%% @end
%%%-------------------------------------------------------------------

-define(ATTRIBUTE_POWER,                              1).   %% 力量
-define(ATTRIBUTE_DEXTERITY,                          2).   %% 敏捷
-define(ATTRIBUTE_INTELLECT,                          3).   %% 智力
-define(ATTRIBUTE_VITALITY,                           4).   %% 体力

-define(ATTRIBUTE_ATTACK,                             10).  %% 攻击
-define(ATTRIBUTE_DEFENSE,                            13).  %% 防御
-define(ATTRIBUTE_HP,                                 14).  %% 生命
-define(ATTRIBUTE_HIT,                                15).  %% 命中
-define(ATTRIBUTE_DUCK,                               16).  %% 闪避
-define(ATTRIBUTE_HIT_RATE,                           17).  %% 命中率
-define(ATTRIBUTE_DUCK_RATE,                          18).  %% 闪避率

-define(ATTRIBUTE_ATTACK_SPEED,                       20).  %% 攻速
-define(ATTRIBUTE_MOVE_SPEED,                         21).  %% 移动速度


%% 属性配置表
%% data_attribute =====> attribute
-record(attribute, {
    attack = 0,
    attack_speed = 0,
    defense = 0,
    dexterity = 0,
    duck = 0,
    duck_rate = 0,
    hit = 0,
    hit_rate = 0,
    hp = 0,
    intellect = 0,
    move_speed = 0,
    power = 0,
    vitality = 0
}).

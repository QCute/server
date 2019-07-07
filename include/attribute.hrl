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


%% 属性
-record(attribute, {
    power = 0,                                        %% 力量
    dexterity = 0,                                    %% 敏捷
    vitality = 0,                                     %% 体力
    intellect = 0,                                    %% 智力
    attack = 0,                                       %% 攻击
    defense = 0,                                      %% 防御
    hp = 0,                                           %% 生命
    hit = 0,                                          %% 命中
    duck = 0,                                         %% 闪避
    hit_rate = 0,                                     %% 命中率
    duck_rate = 0,                                    %% 闪避率
    attack_speed = 0,                                 %% 攻速
    move_speed = 0                                    %% 移动速度
}).

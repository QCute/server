%%%------------------------------------------------------------------
%%% @doc
%%% attribute define
%%% @end
%%%------------------------------------------------------------------

%% 属性配置表
%% attribute_data =====> attribute
-record(attribute, {
    fc = 0,                                           %% 战力 
    hp = 0,                                           %% 血量 
    attack = 0,                                       %% 攻击 
    defense = 0,                                      %% 防御 
    health = 0,                                       %% 生命 
    hit = 0,                                          %% 命中 
    duck = 0,                                         %% 闪避 
    freeze = 0,                                       %% 冰冻 
    destroy = 0,                                      %% 毁灭 
    vertigo = 0                                       %% 眩晕 
}).

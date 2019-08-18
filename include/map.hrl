%%%-------------------------------------------------------------------
%%% @doc
%%% map define
%%% @end
%%%-------------------------------------------------------------------

%% 地图对象定义
-define(MAP_OBJECT_FIGHTER,                           1).
-define(MAP_OBJECT_MONSTER,                           2).
-define(MAP_OBJECT_NPC,                               3).
-define(MAP_OBJECT_DROP,                              4).

%% 地图配置表
%% map_data =====> map_data
-record(data_map, {
    map_id = 0,                                       %% 数值id
    type,                                             %% 广播类型 slice/full
    reconnect,                                        %% 是否重连
    monsters,                                         %% 随地图启动的怪物
    hurt_rank,                                        %% 是否记录伤害排行
    pk_mode,                                          %% PK模式
    enter_point = [],                                 %% 进入点
    enter_script,                                     %% 进入脚本
    relive_script,                                    %% 复活脚本
    leave_script                                      %% 离开脚本
}).

%% 角色地图状态
-record(map, {
    id,                                               %% 唯一id
    map_id = 0,                                       %% 数值id
    pid,                                              %% 地图Pid
    x,                                                %% x坐标
    y                                                 %% y坐标
}).

%% 九宫格切片
-record(slice, {
    left,                                             %% 左
    right,                                            %% 右
    top,                                              %% 上
    bottom                                            %% 下
}).

%% 地图状态
-record(map_state, {
    id = 0,                                           %% 唯一id
    map_id = 0,                                       %% 数值id
    multi_map = false,                                %% 是否分线地图
    type = slice,                                     %% 类型 slice/full
    pid = undefined,                                  %% Pid
    code = undefined,                                 %% 代码模块
    unique = 0,                                       %% 唯一值
    fighters = [],                                    %% 角色数据
    monsters = [],                                    %% 怪物数据
    npc = [],                                         %% NPC数据
    drop = []                                         %% 掉落
}).

-record(battle_state, {fighter, monster, npc}).
%% 战斗对象
-record(battle_object, {
    id = 0,                                           %% ID
    type = 0,                                         %% 类型
    hp = 0,                                           %% 血量
    position = 0,                                     %% 位置
    extra = 0                                         %% 附加
}).

%% 玩家
-record(fighter, {
    id = 0,                                           %% 角色ID
    name = 0,                                         %% 名字
    sex = 0,                                          %% 性别
    classes = 0,                                      %% 职业
    fc = 0,                                           %% 战力
    %hp = 0,                                           %% 血量
    type = 1,                                         %% 类型
    pets = [],                                        %% 宠物
    attribute = [],                                   %% 属性
    skills = [],                                      %% 技能
    buffs = [],                                       %% buff
    pid,                                              %% pid
    sender_pid,                                       %% sender pid
    camp = 0,                                         %% 阵营
    x,                                                %% x
    y,                                                %% y
    hatred = [],                                      %% 仇恨列表
    extra = 0                                         %% 附加
}).

%% 怪物
-record(monster, {
    id = 0,                                           %% 怪物唯一ID
    type = 2,                                         %% 类型
    monster_id = 0,                                   %% 数值ID
    group_id = 0,                                     %% 组ID
    %hp = 0,                                           %% 血量
    act_type = 0,                                     %% 动作类型
    act_script = [],                                  %% 目标
    state = guard,                                    %% 状态
    act = 0,                                          %% 怪物动作AI
    camp = 0,                                         %% 阵营
    speed = 0,                                        %% 速度
    attribute,                                        %% 属性
    x,                                                %% x
    y,                                                %% y
    hatred = [],                                      %% 仇恨列表
    path = [],                                        %% 路径
    slave = [],                                       %% 随从
    extra = 0                                         %% 附加
}).

%% 战斗技能
-record(battle_skill, {
    skill_id,
    level,
    cd,
    extra
}).

%% 战斗Buff
-record(battle_buff, {
    buff_id,
    time,
    extra
}).


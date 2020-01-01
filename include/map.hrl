%%%------------------------------------------------------------------
%%% @doc
%%% map define
%%% @end
%%%------------------------------------------------------------------

%% 地图对象定义
-define(MAP_OBJECT_ANY,                               0).
-define(MAP_OBJECT_ROLE,                              1).
-define(MAP_OBJECT_MONSTER,                           2).
-define(MAP_OBJECT_NPC,                               3).
-define(MAP_OBJECT_DROP,                              4).

%% 地图配置表
%% map_data =====> map_data
-record(map_data, {
    map_id = 0,                                       %%  
    type = [],                                        %% 广播类型(validate(`map_type`)) 
    reconnect = 0,                                    %% 是否重连 
    monsters = [],                                    %% 随地图启动的怪物 
    rank_key = [],                                    %% 榜键类型(validate(`map_rank_key`)) 
    rank_value = [],                                  %% 榜值类型(validate(`map_rank_value`)) 
    rank_mode = [],                                   %% 榜模式(validate(`map_rank_mode`)) 
    enter_points = [],                                %% 进入点 
    pk_mode = [],                                     %% PK模式 
    enter_script = [],                                %% 进入脚本 
    relive_script = [],                               %% 复活脚本 
    leave_script = []                                 %% 离开脚本 
}).

%% 角色地图状态
-record(map, {
    unique_id = 0,                                    %% 唯一id
    map_id = 0,                                       %% 数值id
    pid,                                              %% 地图Pid
    x = 0,                                            %% x坐标
    y = 0                                             %% y坐标
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
    unique_id = 0,                                    %% 唯一id
    map_id = 0,                                       %% 数值id
    multi_map = false,                                %% 是否分线地图
    type = slice,                                     %% 类型 slice/full
    pid,                                              %% Pid
    code,                                             %% 代码模块
    rank,                                             %% 排行榜
    unique = 0,                                       %% 唯一值
    fighters = [],                                    %% 战斗对象
    npc = [],                                         %% NPC数据
    drop = [],                                        %% 掉落
    tick = 0                                          %% 频率
}).

%% 战斗对象
-record(battle_object, {
    id = 0,                                           %% ID
    type = 0,                                         %% 类型
    hp = 0,                                           %% 血量
    position = 0,                                     %% 位置
    extra = 0                                         %% 附加
}).

%% 战斗对象
-record(fighter, {
    id = 0,                                           %% 角色ID/怪物ID/NPC/掉落ID
    name = 0,                                         %% 名字
    sex = 0,                                          %% 性别
    classes = 0,                                      %% 职业
    fc = 0,                                           %% 战力
    type = 0,                                         %% 战斗者类型(1:玩家/2:怪物/3:NPC/4:掉落)
    pets = [],                                        %% 宠物
    attribute = [],                                   %% 属性
    skills = [],                                      %% 技能
    buffs = [],                                       %% buff
    pid,                                              %% pid
    sender_pid,                                       %% sender pid
    camp = 0,                                         %% 阵营
    hatreds = [],                                     %% 仇恨列表
    monster_id = 0,                                   %% 数值ID
    group_id = 0,                                     %% 组ID
    act = 0,                                          %% 怪物动作AI
    act_type = 0,                                     %% 动作类型
    act_script = [],                                  %% 目标
    path = [],                                        %% 路径
    range = 0,                                        %% 攻击距离
    distance = 0,                                     %% 搜索距离
    slave = [],                                       %% 随从
    state = guard,                                    %% 状态
    x,                                                %% x
    y,                                                %% y
    extra = 0                                         %% 附加
}).

%% 仇恨
-record(hatred, {
    id = 0,                                           %% ID
    type = 0,                                         %% 类型
    group_id = 0,                                     %% 组ID
    camp = 0                                          %% 阵营
}).

%% 战斗技能
-record(battle_skill, {
    skill_id = 0,                                     %% 技能ID
    type = 0,                                         %% 技能类型
    level = 0,                                        %% 技能等级
    time = 0,                                         %% 使用时间
    cd = 0,                                           %% 冷却时间
    distance = 0,                                     %% 有效距离
    number = 0,                                       %% 作用数量
    effect = [],                                      %% 作用效果
    extra = []                                        %% 附加
}).

%% 战斗Buff
-record(battle_buff, {
    buff_id = 0,                                      %% Buff ID
    expire_time = 0,                                  %% 过期时间
    extra = []                                        %% 附加
}).


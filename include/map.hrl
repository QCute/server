-ifndef(MAP_HRL).
-define(MAP_HRL, 'MAP_HRL').

%% 地图对象定义
-define(MAP_OBJECT_ANY,                               0).
-define(MAP_OBJECT_ROLE,                              1).
-define(MAP_OBJECT_MONSTER,                           2).
-define(MAP_OBJECT_NPC,                               3).
-define(MAP_OBJECT_DROP,                              4).

%% 地图配置表
-record(map_data, {
    map_id = 0,                                       %% 地图ID
    type = [],                                        %% 广播类型
    reconnect = [],                                   %% 是否重连
    monsters = [],                                    %% 随地图启动的怪物
    rank_key = [],                                    %% 榜键类型
    rank_value = [],                                  %% 榜值类型
    rank_mode = [],                                   %% 榜模式
    enter_points = [],                                %% 进入点
    pk_mode = [],                                     %% PK模式
    enter_script = [],                                %% 进入脚本
    relive_script = [],                               %% 复活脚本
    leave_script = []                                 %% 离开脚本
}).

%% 角色地点状态
-record(location, {
    role_id = 0,                                      %% 角色ID
    map_no = 0,                                       %% 地图编号
    map_id = 0,                                       %% 地图ID
    pid,                                              %% 地图Pid
    type,                                             %% 类型(city/dungeon/war)
    x = 0,                                            %% x坐标
    y = 0                                             %% y坐标
}).

%% 九宫格切片
-record(slice, {
    left = 0,                                         %% 左
    right = 0,                                        %% 右
    top = 0,                                          %% 上
    bottom = 0                                        %% 下
}).

%% 地图状态
-record(map, {
    map_no = 0,                                       %% 地图编号
    map_id = 0,                                       %% 地图ID
    type = full,                                      %% 类型(slice:九宫格/full:全图)
    pid,                                              %% Pid
    sorter,                                           %% 排序器
    fighter = [],                                     %% 战斗对象
    npc = [],                                         %% NPC数据
    drop = [],                                        %% 掉落
    trigger = [],                                     %% 触发器
    data                                              %% 附加数据
}).

%% 战斗对象
-record(fighter, {
    id = 0,                                           %% 角色ID/怪物ID/NPC/掉落ID
    type = 0,                                         %% 战斗者类型(1:玩家/2:怪物/3:NPC/4:掉落)
    camp = 0,                                         %% 阵营
    attribute,                                        %% 属性
    skill = [],                                       %% 技能
    buff = [],                                        %% 增益状态Buff
    x = 0,                                            %% x
    y = 0,                                            %% y
    data                                              %% 附加数据
}).

%% 玩家
-record(fighter_role, {
    role_name = <<>>,                                 %% 名字
    level = 0,                                        %% 等级
    sex = 0,                                          %% 性别
    classes = 0,                                      %% 职业
    guild_id = 0,                                     %% 公会ID
    guild_name = <<>>,                                %% 公会名
    guild_job = 0,                                    %% 公会职位
    team_id = 0,                                      %% 队伍ID
    team_name = <<>>,                                 %% 队伍名
    pid,                                              %% 玩家Pid
    sender_pid,                                       %% 玩家SenderPid
    extra                                             %% 附加数据
}).

%% 怪物
-record(fighter_monster, {
    monster_id = 0,                                   %% 怪物数值ID
    monster_type = 0,                                 %% 怪物类型
    hatreds = [],                                     %% 仇恨列表
    act_type = 0,                                     %% 动作类型
    act_script = [],                                  %% 目标
    state = guard,                                    %% 状态
    path = [],                                        %% 路径
    range = 0,                                        %% 攻击距离
    distance = 0,                                     %% 可视/搜索距离
    extra                                             %% 附加数据
}).

%% 仇恨
-record(hatred, {
    id = 0,                                           %% ID
    type = 0,                                         %% 类型
    subtype = 0,                                      %% 子类型
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
    effect = []                                       %% 作用效果
}).

%% 战斗Buff
-record(battle_buff, {
    buff_id = 0,                                      %% Buff ID
    type = 0,                                         %% 类型
    expire_time = 0,                                  %% 过期时间
    overlap = 1,                                      %% 叠加数
    effect = []                                       %% 作用效果
}).

-endif.

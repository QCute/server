
%% BOSS 信息
-record(boss, {
    monster_id = 0,                                   %% 怪物ID
    hp = 0,                                           %% 血量
    map_no = 0,                                       %% 地图编号
    map_id = 0,                                       %% 地图ID
    map_pid,                                          %% 地图Pid
    relive_time = 0,                                  %% 复活时间
    timer                                             %% 定时器
}).

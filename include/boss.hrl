%% BOSS 信息
-record(boss, {
    monster_id = 0,                                   %% 怪物ID
    hp = 0,                                           %% 血量
    map_unique_id = 0,                                %% 地图唯一ID
    map_id = 0,                                       %% 地图ID
    map_pid,                                          %% 地图Pid
    relive_time = 0,                                  %% 复活时间
    timer                                             %% 定时器
}).

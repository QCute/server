%% 角色排行表
%% rank =====> rank
-record(rank, {
    type = 0,                                         %% 类型(select)
    rank = 0,                                         %% 排名
    key = 0,                                          %% 键 
    value = 0,                                        %% 值 
    time = 0,                                         %% 时间
    name = <<>>,                                      %% 名字 
    other = [],                                       %% 附加数据 
    flag = 1                                          %% 标识(flag)/default(1)
}).

%% 排行榜类型
-define(RANK_TYPE_LEVEL,                              1). %% 等级
-define(RANK_TYPE_FIGHT_COUNT,                        2). %% 战力
-define(RANK_TYPE_ACHIEVEMENT,                        3). %% 成就
-define(RANK_TYPE_WEALTH,                             4). %% 财富
-define(RANK_TYPE_VEIN,                               5). %% 经脉

%% 全部排行榜
-define(RANK_TYPE_LIST, [
    ?RANK_TYPE_LEVEL,
    ?RANK_TYPE_FIGHT_COUNT,
    ?RANK_TYPE_ACHIEVEMENT,
    ?RANK_TYPE_WEALTH,
    ?RANK_TYPE_VEIN
]).

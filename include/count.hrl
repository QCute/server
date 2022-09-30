%%%-------------------------------------------------------------------
%%% @doc
%%% count define
%%% @end
%%%-------------------------------------------------------------------

%% count type
-define(COUNT_TYPE_CHARGE,                            1).
-define(COUNT_TYPE_COST_GOLD,                         2).
-define(COUNT_TYPE_SHOP_BUY,                          3).
-define(COUNT_TYPE_DUNGEON_PASS,                      11).
-define(COUNT_TYPE_DUNGEON_KILL_BOSS,                 12).


%% 角色计数表
%% count =====> count
-record(count, {
    role_id = 0,                                      %% 角色ID(select_by_role_id)
    type = 0,                                         %% 计数类型
    today_number = 0,                                 %% 今天数量
    week_number = 0,                                  %% 今周数量
    total_number = 0,                                 %% 总数
    time = 0,                                         %% 时间
    flag = 0                                          %% 标识(flag)
}).


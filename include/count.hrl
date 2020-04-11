%%%-------------------------------------------------------------------
%%% @doc
%%% count define
%%% @end
%%%-------------------------------------------------------------------

%% count type shop
-define(COUNT_TYPE_SHOP,                              1).
%% count type dungeon
-define(COUNT_TYPE_DUNGEON_PASS,                      11).
-define(COUNT_TYPE_DUNGEON_KILL_BOSS,                 12).


%% 角色计数表
%% count =====> count
-record(count, {
    role_id = 0,                                      %% 角色ID(select) 
    type = 0,                                         %% 计数类型 
    today_number = 0,                                 %% 今天数量 
    week_number = 0,                                  %% 今周数量 
    total_number = 0,                                 %% 总数 
    time = 0,                                         %% 时间 
    flag = 0                                          %% 标识(flag) 
}).


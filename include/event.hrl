-ifndef(EVENT_HRL).
-define(EVENT_HRL, 'EVENT_HRL').

%% 触发器
-record(trigger, {
    name,                                             %% 触发器名字
    pure = false,                                     %% 回调是否不携带State
    module,                                           %% 回调模块名
    function,                                         %% 回调函数名
    args = []                                         %% 回调函数参数
}).

%% 角色事件
-record(event, {
    name,                                             %% 事件名字
    target = 0,                                       %% 目标
    number = 1,                                       %% 目标数量
    extra = 0                                         %% 额外数据
}).

%% 公会事件
-record(guild_event, {
    name,                                             %% 事件名字
    target = 0,                                       %% 目标
    number = 1                                        %% 目标数量
}).

%% 战斗事件
-record(battle_event, {
    name,                                             %% 事件名字
    object,                                           %% 事件发起对象
    target,                                           %% 事件目标对象
    number = 1                                        %% 目标数量
}).

-endif.

%%%------------------------------------------------------------------
%%% @doc
%%% event define
%%% @end
%%%------------------------------------------------------------------
%% 触发器
-record(trigger, {
    name,                                             %% 触发器名字
    pure = false,                                     %% 回调是否不携带State
    module,                                           %% 回调模块名
    function,                                         %% 回调函数名
    args = []                                         %% 回调函数参数
}).

%% 事件,不满足情况可自行添加自定义事件
-record(event, {
    name,                                            %% 事件名字
    target = 0,                                      %% 目标
    number = 1                                       %% 目标数量
}).

%% 事件检查器
-record(event_checker, {
    data,                                            %% 数据 (list:列表/integer:整数)
    key,                                             %% 列表数据键位置
    value                                            %% 列表数据值位置
}).

%% 事件,不满足情况可自行添加自定义事件
-record(battle_event, {
    name,                                            %% 事件名字
    object,                                          %% 事件发起对象
    target,                                          %% 事件目标对象
    number = 1                                       %% 目标数量
}).

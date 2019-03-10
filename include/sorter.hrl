%%----------------------------------------------------
%% @doc
%% sorter define
%% @end
%%----------------------------------------------------
%% sorter
-record(sorter, {
    name,                                             %% 名字
    mode = share,                                     %% 模式
    type = replace,                                   %% 类型
    limit = 100,                                      %% 限制
    pid,                                              %% Pid
    list = [],                                        %% 数据
    key = 0,                                          %% 更新键
    value = 0,                                        %% 更新值
    time = 0,                                         %% 更新时间
    rank = 0                                          %% 排名
}).
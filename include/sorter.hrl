%%----------------------------------------------------
%% @doc
%% sorter define
%% @end
%%----------------------------------------------------
%% sorter
-record(sorter, {
    name,                         %% 名字
    mode,                         %% 模式
    type,                         %% 类型
    limit,                        %% 限制
    pid,                          %% Pid
    key,                          %% 更新键
    value,                        %% 更新值
    time,                         %% 更新时间
    rank                          %% 排名
}).
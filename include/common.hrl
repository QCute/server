%%----------------------------------------------------
%% @doc
%% common define
%% @end
%%----------------------------------------------------

%% 时间相关
-define(MINUTE_SECONDS,                 60).            %% 一分钟的时间（秒）
-define(HOUR_SECONDS,                   3600).          %% 一小时的时间（秒）
-define(DAY_SECONDS,                    86400).         %% 一天的时间（秒）
-define(WEEK_SECONDS,                   604800).        %% 一周的时间（秒）
-define(DIFF_SECONDS_0000_1900,         59958230400).
-define(DIFF_SECONDS_1900_1970,         2208988800).
-define(DIFF_SECONDS_0000_1970,         62167219200).

%% 仅调试环境打印
-ifdef(DEBUG).
%% 打印
-define(PRINT(Msg),                     console:print(?MODULE, ?LINE, Msg, [])).
-define(PRINT(Msg, Args),               console:print(?MODULE, ?LINE, Msg, Args)).
%% 调试 (蓝色)
-define(DEBUG(Msg),                     console:debug(?MODULE, ?LINE, Msg, [])).
-define(DEBUG(Msg, Args),               console:debug(?MODULE, ?LINE, Msg, Args)).
-else.
%% 打印
-define(PRINT(Msg),                     ok).
-define(PRINT(Msg, Args),               ok).
%% 调试 (蓝色)
-define(DEBUG(Msg),                     ok).
-define(DEBUG(Msg, Args),               ok).
-endif.
%% 信息(绿色)
-define(INFO(Msg),                      console:info(?MODULE, ?LINE, Msg, [])).
-define(INFO(Msg, Args),                console:info(?MODULE, ?LINE, Msg, Args)).
%% 警告(黄色)
-define(WARMING(Msg),                   console:warming(?MODULE, ?LINE, Msg, [])).
-define(WARMING(Msg, Args),             console:warming(?MODULE, ?LINE, Msg, Args)).
%% 错误(红色)
-define(ERROR(Msg),                     console:error(?MODULE, ?LINE, Msg, [])).
-define(ERROR(Msg, Args),               console:error(?MODULE, ?LINE, Msg, Args)).
%% 打印 catch exit 信息
-define(STACK_TRACE(Msg),               console:stack_trace(catch Msg)).
-define(STACK_TRACE(Msg, Return),       console:stack_trace(catch Msg, Return)).

%% 数据库名
-define(DB_GAME,                        game).
-define(DB_ADMIN,                       admin).

%% 数据修改状态
-define(UPDATE_STATE_ORIGIN,	        0).			%% 没有变化
-define(UPDATE_STATE_MODIFY,	        1).			%% 修改过
-define(UPDATE_STATE_ADD, 	            2).			%% 新增

%% 背包类型
-define(BAG_TYPE_COMMON,  			    1).  		%% 普通背包(道具背包)
-define(BAG_TYPE_EQUIPMENT,  		    2).    		%% 装备背包
-define(BAG_TYPE_STORE,  		        3).    		%% 仓库背包
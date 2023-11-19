## 独立服务模块

#### 1. 准备SQL文件`script/版本/auction.sql`
```sql

DROP TABLE IF EXISTS `auction`;
-- 配置表
DROP TABLE IF EXISTS `auction_data`;
CREATE TABLE `auction_data` (
  `auction_id` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '拍品ID',
  `bid_type` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '竞拍类型(1:竞价/2:一口价)',
  `begin_price` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '底价',
  `add_price` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '每次加价',
  `tax` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '税收',
  `show_time` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '预览时间',
  `auction_time` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '拍卖时间',
  `critical_time` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '临界时间(出价加时的临界时间)',
  `over_time` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '延迟时间(出价加时的时间)',
  PRIMARY KEY (`auction_id`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci ROW_FORMAT=DYNAMIC COMMENT='拍卖配置表';

-- 数据表
CREATE TABLE `auction` (
  `auction_no` bigint(20) unsigned NOT NULL AUTO_INCREMENT COMMENT '拍品编号',
  `auction_id` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '拍品ID',
  `number` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '拍品数量',
  `type` tinyint(3) unsigned NOT NULL DEFAULT 0 COMMENT '拍卖类型(1:全服/2:公会)',
  `bid_type` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '竞拍类型(1:竞价/2:一口价)',
  `start_time` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '开始时间',
  `end_time` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '结束时间',
  `from` varchar(32) NOT NULL DEFAULT '' COMMENT '物品来源',
  `bid_number` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '加价次数',
  `now_price` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '当前价格',
  `next_price` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '下次出价的价格',
  `seller_list` varchar(0) GENERATED ALWAYS AS ('') VIRTUAL COMMENT '卖家列表',
  `bidder_list` varchar(0) GENERATED ALWAYS AS ('') VIRTUAL COMMENT '买家列表',
  `guild_id` bigint(20) unsigned NOT NULL DEFAULT 0 COMMENT '公会ID',
  `timer` tinyint(3) unsigned GENERATED ALWAYS AS (0) VIRTUAL COMMENT '定时器',
  `flag` tinyint(3) unsigned GENERATED ALWAYS AS (0) VIRTUAL COMMENT '标识',
  PRIMARY KEY (`auction_no`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci ROW_FORMAT=DYNAMIC COMMENT='拍卖信息表';

DROP TABLE IF EXISTS `auction_role`;
CREATE TABLE `auction_role` (
  `auction_no` bigint(20) unsigned NOT NULL DEFAULT 0 COMMENT '拍品编号',
  `server_id` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '服务器ID',
  `role_id` bigint(20) unsigned NOT NULL DEFAULT 0 COMMENT '出价者ID',
  `role_name` char(16) NOT NULL DEFAULT '' COMMENT '出价者名字',
  `guild_id` bigint(20) unsigned NOT NULL DEFAULT 0 COMMENT '出价者公会ID',
  `guild_name` char(16) NOT NULL DEFAULT '' COMMENT '出价者公会名字',
  `type` tinyint(3) unsigned NOT NULL DEFAULT 0 COMMENT '角色类型(1:卖家/2:买家)',
  `price` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '当前价格',
  `time` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '时间',
  `flag` tinyint(3) unsigned GENERATED ALWAYS AS (0) VIRTUAL COMMENT '标识',
  PRIMARY KEY (`auction_no`,`role_id`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci ROW_FORMAT=DYNAMIC COMMENT='拍卖角色表';

-- 日志表
DROP TABLE IF EXISTS `auction_log`;
CREATE TABLE `auction_log` (
  `id` bigint(20) unsigned NOT NULL AUTO_INCREMENT COMMENT 'ID',
  `auction_id` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '拍品ID',
  `number` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '拍品数量',
  `bid_number` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '加价次数',
  `price` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '成交价',
  `role_id` bigint(20) unsigned NOT NULL DEFAULT 0 COMMENT '获得者ID',
  `role_name` char(16) NOT NULL DEFAULT '' COMMENT '获得者名字',
  `server_id` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '获得者服务器ID',
  `time` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '时间',
  PRIMARY KEY (`id`) USING BTREE,
  KEY `role_id` (`role_id`) USING BTREE,
  KEY `time` (`time`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci ROW_FORMAT=DYNAMIC COMMENT='拍卖日志表';

```

* 执行命令生成头文件`include/auction.hrl`
```sh
maker record auction
```

#### 2. 生成配置文件`script/make/erl/data/auction_data.erl`

* 配置`script/make/erl/erl_script.erl`
```erl
        #{
            file => "script/make/erl/data/auction_data.erl",
            comment => "拍卖配置",
            sql => [

                %% get auction data by auction_id
                #{
                    select => [],
                    from => auction_data,
                    by => auction_id,
                    as => get
                }
            ]
        }
```

* 执行命令
```sh
maker erl auction
```

#### 3. 生成SQL文件`script/make/sql/data/auction_sql.erl`

* 配置`script/make/sql/sql_script.erl`
```erl
        #{
            file => "src/module/role/role_sql.erl",
            sql => [

                %% retrieve all rows
                #{
                    select => [],
                    from => auction,
                    as => select
                }
            ]
        }
```

* 执行命令
```sh
maker sql 
```

#### 4. 生成协议编码/解码文件`script/make/protocol/erl/auction_protocol.erl`

* 配置`script/make/protocol/protocol_script_auction.erl`
```erl
    #protocol{
        number = 111,
        comment = "物品",
        erl = "script/make/protocol/erl/auction_protocol.erl",
        html = "script/make/protocol/html/AuctionProtocol.html",
        lua = "script/make/protocol/lua/AuctionProtocol.lua",
        js = "script/make/protocol/js/AuctionProtocol.js",
        cs = "script/make/protocol/cs/AuctionProtocol.cs",
        io = [
            #io{
                number = 11101,
                comment = "道具列表",
                handler = #handler{module = auction_server, function = query},
                decode = {},
                encode = [                                 %% 拍品列表
                    [] = #auction{                         %% 拍品
                        auction_no = u64(),                %% 拍品编号
                        auction_id = u32(),                %% 拍品ID
                        type = u8(),                       %% 拍卖类型(1:全服/2:公会)
                        number = u16(),                    %% 数量
                        now_price = u32(),                 %% 当前价格
                        next_price = u32(),                %% 下次出价的价格
                        end_time = u32()                   %% 结束时间
                    }
                ]
            }
        ]
    }
```

* 执行命令
```sh
maker pt auction
```

### 5. 添加启动项`src/application/service.erl`
```erl
start(Node = local) ->
    ...

    %% auction
    {ok, _} = auction_server:start(),

    ...
```

#### 6. 添加控制器文件`src/module/auction/auction_server.erl`
```erl
%%%-------------------------------------------------------------------
%%% @doc
%%% auction server
%%% @end
%%%-------------------------------------------------------------------
-module(auction_server).
-behaviour(gen_server).
%% API
-export([start/0, start_link/0]).
-export([query/1]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
%% includes
-include_lib("stdlib/include/ms_transform.hrl").
-include("common.hrl").
-include("time.hrl").
-include("journal.hrl").
-include("auction.hrl").
%%%===================================================================
%%% API functions
%%%===================================================================
%% @doc start
-spec start() -> {ok, pid()} | {error, term()}.
start() ->
    process:start(?MODULE).

%% @doc server start
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc query
-spec query(User :: #user{}) -> ok().
query(_) ->
    {ok, ?MODULE}.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
%% @doc init
-spec init(Args :: term()) -> {ok, State :: []}.
init([]) ->
    erlang:process_flag(trap_exit, true),
    ets:new(?MODULE, [named_table, set, {keypos, #auction.auction_no}, {read_concurrency, true}, {write_concurrency, true}]),
    {ok, []}.

%% @doc handle_call
-spec handle_call(Request :: term(), From :: {pid(), Tag :: term()}, State :: []) -> {reply, Reply :: term(), NewState :: []}.
handle_call(Request, From, State) ->
    try
        do_call(Request, From, State)
    catch ?EXCEPTION(Class, Reason, Stacktrace) ->
        ?STACKTRACE(Class, Reason, ?GET_STACKTRACE(Stacktrace)),
        {reply, ok, State}
    end.

%% @doc handle_cast
-spec handle_cast(Request :: term(), State :: []) -> {noreply, NewState :: []}.
handle_cast(Request, State) ->
    try
        do_cast(Request, State)
    catch ?EXCEPTION(Class, Reason, Stacktrace) ->
        ?STACKTRACE(Class, Reason, ?GET_STACKTRACE(Stacktrace)),
        {noreply, State}
    end.

%% @doc handle_info
-spec handle_info(Request :: term(), State :: []) -> {noreply, NewState :: []}.
handle_info(Info, State) ->
    try
        do_info(Info, State)
    catch ?EXCEPTION(Class, Reason, Stacktrace) ->
        ?STACKTRACE(Class, Reason, ?GET_STACKTRACE(Stacktrace)),
        {noreply, State}
    end.

%% @doc terminate
-spec terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()), State :: []) -> {ok, NewState :: []}.
terminate(_Reason, State) ->
    {ok, State}.

%% @doc code_change
-spec code_change(OldVsn :: (term() | {down, term()}), State :: [], Extra :: term()) -> {ok, NewState :: []}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

do_call(_Request, _From, State) ->
    {reply, ok, State}.


do_cast(_Request, State) ->
    {noreply, State}.


do_info(_Info, State) ->
    {noreply, State}.

```

#### 文件树
    └── include
        └── auction.hrl                               : 头文件  
    └── script
        └── sql
            └── version
                └── auction.sql                       : SQL文件
        └── make
            └── erl
                └── data
                    └── auction_data.erl              : 配置生成数据
            └── sql
                └── data
                    └── auction_sql.erl               : 数据库生成的模型
            └── protocol
                └── erl
                    └── auction_protocol.erl          : 协议编码/解码
                └── handler
                    └── auction_handler.erl           : 协议路由
    └── src
        └── auction
            └── auction_server.erl                    : 单独服务与控制器


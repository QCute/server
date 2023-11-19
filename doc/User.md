## 玩家数据模块

#### 1. 准备SQL文件`script/版本/item.sql`
```sql
-- 配置表
DROP TABLE IF EXISTS `item_data`;
CREATE TABLE `item_data` (
  `item_id` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '物品id',
  `type` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '类型',
  `overlap` int(10) unsigned NOT NULL DEFAULT 1 COMMENT '叠加数',
  `category` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '分类ID',
  `expire_time` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '过期时间',
  `use_number` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '使用数量(0:不能直接使用/1:一个/N:N个)',
  `use_effect` enum('none','gold','silver','copper','exp','coin') NOT NULL COMMENT '使用效果',
  `use_value` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '使用效果数值',
  `name` char(255) NOT NULL DEFAULT '' COMMENT '名字',
  `icon` char(255) NOT NULL DEFAULT '' COMMENT '图标',
  `description` char(255) NOT NULL DEFAULT '' COMMENT '描述',
  PRIMARY KEY (`item_id`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci ROW_FORMAT=DYNAMIC COMMENT='物品配置表';


-- 数据表
DROP TABLE IF EXISTS `item`;
CREATE TABLE `item` (
  `item_no` bigint(20) unsigned NOT NULL AUTO_INCREMENT COMMENT '物品编号',
  `role_id` bigint(20) unsigned NOT NULL DEFAULT 0 COMMENT '角色ID',
  `item_id` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '物品ID',
  `type` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '类型',
  `number` int(10) unsigned NOT NULL DEFAULT 1 COMMENT '数量',
  `expire_time` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '过期时间',
  `flag` tinyint(3) unsigned GENERATED ALWAYS AS (0) VIRTUAL COMMENT '标识',
  PRIMARY KEY (`item_no`) USING BTREE,
  KEY `role_id` (`role_id`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci ROW_FORMAT=DYNAMIC COMMENT='角色物品表';
```

* 执行命令生成头文件`include/item.hrl`
```sh
maker record item
```

#### 2. 生成配置文件`script/make/erl/data/item_data.erl`

* 配置`script/make/erl/erl_script.erl`
```erl
        #{
            file => "script/make/erl/data/item_data.erl",
            comment => "物品配置",
            sql => [

                %% get item data by item_id
                #{
                    select => [],
                    from => item_data,
                    by => item_id,
                    as => get
                }
            ]
        }
```

* 执行命令
```sh
maker erl item
```

#### 3. 生成SQL文件`script/make/sql/data/item_sql.erl`

* 配置`script/make/sql/sql_script.erl`
```erl
        #{
            file => "src/module/role/role_sql.erl",
            sql => [

                %% retrieve rows by role_id
                #{
                    select => [],
                    from => item,
                    by => role_id,
                    as => select
                }
            ]
        }
```

* 执行命令
```sh
maker sql 
```

#### 4. 生成协议编码/解码文件`script/make/protocol/erl/item_protocol.erl`

* 配置`script/make/protocol/protocol_script_item.erl`
```erl
    #protocol{
        number = 111,
        comment = "物品",
        erl = "script/make/protocol/erl/item_protocol.erl",
        html = "script/make/protocol/html/ItemProtocol.html",
        lua = "script/make/protocol/lua/ItemProtocol.lua",
        js = "script/make/protocol/js/ItemProtocol.js",
        cs = "script/make/protocol/cs/ItemProtocol.cs",
        io = [
            #io{
                number = 11101,
                comment = "道具列表",
                handler = #handler{module = item, function = query_item},
                decode = {},
                encode = [                                 %% 物品列表
                    #item{                                 %% 物品记录结构
                        item_no = u64(),                   %% 物品编号
                        item_id = u64(),                   %% 物品ID
                        type = u8(),                       %% 类型
                        number = u16()                     %% 数量
                    }
                ]
            }
        ]
    }
```

* 执行命令
```sh
maker pt item
```

#### 5. 添加记录字段`include/user.hrl`
```sh
    item = [],                                        %% 物品 
```

#### 6. 添加控制器文件`src/module/item/item.erl`
```erl
-module(item).
-export([on_load/1, query_item/1]).
-include("user.hrl").
-include("item.hrl").

%% @doc on load
-spec on_load(User :: #user{}) -> NewUser :: #user{}.
on_load(User = #user{role_id = RoleId}) ->
    DataList = item_sql:select(RoleId),
    User#user{item = DataList}.

%% @doc query item
-spec query_item(User :: #user{}) -> ok().
query_item(#user{item = Item}) ->
    {ok, Item}.

```

#### 文件树
    └── include
        └── item.hrl                                  : 头文件  
    └── script
        └── sql
            └── version
                └── item.sql                          : SQL文件
        └── make
            └── erl
                └── data
                    └── item_data.erl                 : 配置生成数据
            └── sql
                └── data
                    └── item_sql.erl                  : 数据库生成的模型
            └── protocol
                └── erl
                    └── item_protocol.erl             : 协议编码/解码
                └── handler
                    └── item_handler.erl              : 协议路由
    └── src
        └── item
            └── item.erl                              : 控制器

#### 数据流向
#### 请求
1. listener
2. acceptor
3. receiver < - > user_router < - > item_protocol
4. account_handler
5. account
6. user_server
7. user_router
8. item_handler
9. item

#### 2. 响应
1. user_server < - > user_router < - > item_protocol
2. user_sender
3. sender

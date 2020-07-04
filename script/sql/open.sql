DROP TABLE IF EXISTS `activity_data`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `activity_data` (
  `activity_id` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '活动ID',
  `mode` tinyint(3) unsigned NOT NULL DEFAULT 0 COMMENT '活动模式(validate(node_type_integer))',
  `service` varchar(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '服务进程模块(validate(module))',
  `type` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '类型',
  `subtype` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '子类型',
  `award_type` tinyint(3) unsigned NOT NULL DEFAULT 0 COMMENT '领奖类型(自动:0/手动:1)',
  `show_time` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '图标展示时间(时间戳)',
  `start_time` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '开始时间(时间戳)',
  `over_time` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '结束时间(时间戳)',
  `award_time` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '领奖时间(时间戳)',
  `stop_time` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '图标消失时间(时间戳)',
  `show_hour` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '每天展示小时',
  `start_hour` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '每天开始小时',
  `over_hour` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '每天结束小时',
  `start_award_hour` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '每天领奖开始小时',
  `over_award_hour` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '每天领奖结束小时',
  `min_open_days` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '最小开服天数',
  `max_open_days` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '最大开服天数',
  `name` char(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '活动名',
  `icon` char(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '活动图标',
  `entrance` char(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '活动入口',
  `description` char(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '活动描述',
  PRIMARY KEY (`activity_id`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci ROW_FORMAT=DYNAMIC COMMENT='活动配置表';
/*!40101 SET character_set_client = @saved_cs_client */;
DROP TABLE IF EXISTS `asset`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `asset` (
  `role_id` bigint(20) unsigned NOT NULL DEFAULT 0 COMMENT '角色ID',
  `gold` bigint(20) unsigned NOT NULL DEFAULT 0 COMMENT '金币',
  `silver` bigint(20) unsigned NOT NULL DEFAULT 0 COMMENT '银币',
  `copper` bigint(20) unsigned NOT NULL DEFAULT 0 COMMENT '铜币',
  `coin` bigint(20) unsigned NOT NULL DEFAULT 0 COMMENT '硬币',
  `exp` bigint(20) unsigned NOT NULL DEFAULT 0 COMMENT '经验',
  PRIMARY KEY (`role_id`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci ROW_FORMAT=DYNAMIC COMMENT='角色资产表';
/*!40101 SET character_set_client = @saved_cs_client */;
DROP TABLE IF EXISTS `asset_data`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `asset_data` (
  `asset` varchar(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '资产类型',
  `item_id` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '物品配置ID',
  PRIMARY KEY (`asset`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci ROW_FORMAT=DYNAMIC COMMENT='资产物品映射配置表';
/*!40101 SET character_set_client = @saved_cs_client */;
DROP TABLE IF EXISTS `attribute_data`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `attribute_data` (
  `attribute_id` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '属性ID',
  `attribute` varchar(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '属性',
  `type` varchar(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '类型(固定值/万分比)',
  `merge` varchar(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '合并计算公式',
  `effect` varchar(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '效果',
  `name` char(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '名字',
  `description` char(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '描述',
  PRIMARY KEY (`attribute_id`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci ROW_FORMAT=DYNAMIC COMMENT='属性配置表';
/*!40101 SET character_set_client = @saved_cs_client */;
DROP TABLE IF EXISTS `auction`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `auction` (
  `auction_no` bigint(20) unsigned NOT NULL AUTO_INCREMENT COMMENT '拍品编号',
  `auction_id` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '拍品ID',
  `number` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '拍品数量',
  `type` tinyint(3) unsigned NOT NULL DEFAULT 0 COMMENT '拍卖类型(1:全服/2:公会)',
  `bid_type` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '竞拍类型(1:竞价/2:一口价)',
  `start_time` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '开始时间',
  `end_time` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '结束时间',
  `from` char(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '物品来源',
  `bid_number` smallint(5) unsigned NOT NULL DEFAULT 0 COMMENT '加价次数',
  `now_price` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '当前价格',
  `next_price` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '下次出价的价格',
  `seller_list` varchar(0) GENERATED ALWAYS AS ('') VIRTUAL COMMENT '卖家列表',
  `bidder_list` varchar(0) GENERATED ALWAYS AS ('') VIRTUAL COMMENT '买家列表',
  `guild_id` bigint(20) unsigned NOT NULL DEFAULT 0 COMMENT '公会ID',
  `timer` tinyint(3) unsigned GENERATED ALWAYS AS (0) VIRTUAL COMMENT '定时器',
  `flag` tinyint(3) unsigned GENERATED ALWAYS AS (0) VIRTUAL COMMENT '标识(flag)',
  PRIMARY KEY (`auction_no`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci ROW_FORMAT=DYNAMIC COMMENT='拍卖信息表';
/*!40101 SET character_set_client = @saved_cs_client */;
DROP TABLE IF EXISTS `auction_data`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `auction_data` (
  `auction_id` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '拍品ID',
  `bid_type` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '竞拍类型(1:竞价/2:一口价)',
  `begin_price` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '底价',
  `add_price` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '每次加价',
  `tax` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '税收',
  `show_time` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '预览时间',
  `auction_time` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '拍卖时间',
  `critical_time` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '临界时间(出价加时的临界时间)',
  `overtime` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '延迟时间(出价加时的时间)',
  PRIMARY KEY (`auction_id`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci ROW_FORMAT=DYNAMIC COMMENT='拍卖配置表';
/*!40101 SET character_set_client = @saved_cs_client */;
DROP TABLE IF EXISTS `auction_log`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `auction_log` (
  `id` bigint(20) unsigned NOT NULL AUTO_INCREMENT COMMENT 'ID',
  `auction_id` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '拍品ID',
  `number` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '拍品数量',
  `bid_number` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '加价次数',
  `price` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '成交价',
  `role_id` bigint(20) unsigned NOT NULL DEFAULT 0 COMMENT '获得者ID',
  `role_name` char(16) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '获得者名字',
  `server_id` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '获得者服ID',
  `time` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '时间',
  PRIMARY KEY (`id`) USING BTREE,
  KEY `role_id` (`role_id`) USING BTREE,
  KEY `time` (`time`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci ROW_FORMAT=COMPRESSED COMMENT='拍卖日志表';
/*!40101 SET character_set_client = @saved_cs_client */;
DROP TABLE IF EXISTS `auction_role`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `auction_role` (
  `auction_no` bigint(20) unsigned NOT NULL DEFAULT 0 COMMENT '拍品编号(delete_no)',
  `server_id` smallint(5) unsigned NOT NULL DEFAULT 0 COMMENT '服务器ID',
  `role_id` int(20) unsigned NOT NULL DEFAULT 0 COMMENT '出价者ID',
  `role_name` char(16) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '出价者名字',
  `guild_id` int(20) unsigned NOT NULL DEFAULT 0 COMMENT '出价者公会ID',
  `guild_name` char(16) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '出价者公会名字',
  `type` tinyint(3) unsigned NOT NULL DEFAULT 0 COMMENT '角色类型(1:卖家/2:买家)',
  `price` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '当前价格',
  `time` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '时间',
  `flag` tinyint(3) unsigned GENERATED ALWAYS AS (0) VIRTUAL COMMENT '标识(flag)',
  PRIMARY KEY (`auction_no`,`role_id`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci ROW_FORMAT=DYNAMIC COMMENT='拍卖角色表';
/*!40101 SET character_set_client = @saved_cs_client */;
DROP TABLE IF EXISTS `buff`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `buff` (
  `role_id` bigint(20) unsigned NOT NULL DEFAULT 0 COMMENT '角色ID(select)',
  `buff_id` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '状态增益ID',
  `expire_time` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '结束时间',
  `overlap` int(10) unsigned NOT NULL DEFAULT 1 COMMENT '叠加数',
  `flag` tinyint(3) unsigned GENERATED ALWAYS AS (0) VIRTUAL COMMENT '标识(flag)',
  PRIMARY KEY (`role_id`,`buff_id`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci ROW_FORMAT=DYNAMIC COMMENT='角色buff表';
/*!40101 SET character_set_client = @saved_cs_client */;
DROP TABLE IF EXISTS `buff_data`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `buff_data` (
  `buff_id` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '增益状态(Buff)ID',
  `type` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '类型',
  `time` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '有效时间',
  `effect` varchar(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '效果',
  `temporary` tinyint(3) unsigned NOT NULL DEFAULT 0 COMMENT '是否临时的(切地图失效)',
  `overlap_type` tinyint(3) unsigned NOT NULL DEFAULT 0 COMMENT '叠加类型(0:不叠加/1:时间/2:数值/3:都叠加)',
  `name` char(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '名字',
  `description` varchar(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '描述',
  PRIMARY KEY (`buff_id`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci ROW_FORMAT=DYNAMIC COMMENT='buff配置表';
/*!40101 SET character_set_client = @saved_cs_client */;
DROP TABLE IF EXISTS `count`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `count` (
  `role_id` bigint(20) unsigned NOT NULL DEFAULT 0 COMMENT '角色ID(select)',
  `type` int(64) unsigned NOT NULL DEFAULT 0 COMMENT '计数类型',
  `today_number` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '今天数量',
  `week_number` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '今周数量',
  `total_number` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '总数',
  `time` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '时间',
  `flag` tinyint(3) unsigned GENERATED ALWAYS AS (0) VIRTUAL COMMENT '标识(flag)',
  PRIMARY KEY (`role_id`,`type`) USING BTREE,
  KEY `type` (`type`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci ROW_FORMAT=DYNAMIC COMMENT='角色计数表';
/*!40101 SET character_set_client = @saved_cs_client */;
DROP TABLE IF EXISTS `dungeon`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `dungeon` (
  `role_id` bigint(20) unsigned NOT NULL DEFAULT 0 COMMENT '玩家ID(select)',
  `dungeon_id` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '副本ID',
  `type` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '类型',
  `today_number` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '今天次数',
  `total_number` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '历史总次数',
  `flag` tinyint(3) unsigned GENERATED ALWAYS AS (0) VIRTUAL COMMENT '标识(flag)',
  PRIMARY KEY (`role_id`,`type`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci ROW_FORMAT=DYNAMIC COMMENT='角色副本表';
/*!40101 SET character_set_client = @saved_cs_client */;
DROP TABLE IF EXISTS `dungeon_data`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `dungeon_data` (
  `dungeon_id` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '副本ID',
  `type` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '类型(validate(dungeon_type))',
  `event` varchar(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '事件(validate(event))',
  `condition` varchar(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '条件',
  `cost` varchar(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '消耗',
  `day_number` varchar(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '每日次数',
  `buy_number` varchar(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '购买次数',
  `module` varchar(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '代码模块(validate(module))',
  `function` varchar(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '入口函数(validate(function))',
  `map_id` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '地图ID',
  `monsters` varchar(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '怪物',
  `boss` varchar(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT 'Boss',
  `time` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '时间',
  `award` varchar(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '奖励',
  `name` char(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '名字',
  `description` char(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '描述',
  PRIMARY KEY (`dungeon_id`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci ROW_FORMAT=DYNAMIC COMMENT='副本配置表';
/*!40101 SET character_set_client = @saved_cs_client */;
DROP TABLE IF EXISTS `effect_data`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `effect_data` (
  `effect_id` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '效果ID',
  `type` varchar(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '类型(validate(effect_type))',
  `scope` varchar(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '作用范围(validate(effect_scope))',
  `condition` varchar(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '条件',
  `ratio` varchar(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '概率',
  `restrict` varchar(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '约束',
  `operation` varchar(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '操作(validate(effect_operation))',
  `object` varchar(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '作用对象(validate(effect_object))',
  `attribute` varchar(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '操作属性(validate(effect_attribute))',
  `field` varchar(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '操作属性字段(validate(effect_field))',
  `value` varchar(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '属性值',
  `time` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '效果时间',
  `extra` varchar(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '额外',
  `description` varchar(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '描述',
  PRIMARY KEY (`effect_id`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci ROW_FORMAT=DYNAMIC COMMENT='作用效果配置表';
/*!40101 SET character_set_client = @saved_cs_client */;
DROP TABLE IF EXISTS `error_code_data`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `error_code_data` (
  `type` varchar(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '类型',
  `key` varchar(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '键',
  `en` char(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '英文',
  `sc` char(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '简体中文',
  `tc` char(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '繁体中文',
  PRIMARY KEY (`type`,`key`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci ROW_FORMAT=DYNAMIC COMMENT='游戏文本配置表';
/*!40101 SET character_set_client = @saved_cs_client */;
DROP TABLE IF EXISTS `friend`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `friend` (
  `role_id` bigint(20) unsigned NOT NULL DEFAULT 0 COMMENT '用户ID(select)',
  `friend_id` bigint(20) unsigned NOT NULL DEFAULT 0 COMMENT '好友ID(join(`role`.`role_id`)/join(`vip`.`role_id`))',
  `friend_name` char(0) GENERATED ALWAYS AS ('') VIRTUAL COMMENT '好友名字(join(`role`.`role_name`))',
  `sex` tinyint(3) unsigned GENERATED ALWAYS AS (0) VIRTUAL COMMENT '好友性别(join(`role`.`sex`))',
  `classes` tinyint(3) unsigned GENERATED ALWAYS AS (0) VIRTUAL COMMENT '好友职业(join(`role`.`classes`))',
  `vip_level` tinyint(3) unsigned GENERATED ALWAYS AS (0) VIRTUAL COMMENT 'VIP等级(join(`vip`.`vip_level`))',
  `online` tinyint(3) unsigned GENERATED ALWAYS AS (0) VIRTUAL COMMENT '好友在线状态(join(`role`.`online`))',
  `relation` tinyint(3) unsigned NOT NULL DEFAULT 0 COMMENT '友好状态(0:申请/1:好友/2:黑名单)',
  `time` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '时间',
  `flag` tinyint(3) unsigned GENERATED ALWAYS AS (0) VIRTUAL COMMENT '标识(flag)',
  PRIMARY KEY (`role_id`,`friend_id`) USING BTREE,
  KEY `friend_id` (`friend_id`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci ROW_FORMAT=DYNAMIC COMMENT='角色好友表';
/*!40101 SET character_set_client = @saved_cs_client */;
DROP TABLE IF EXISTS `guild`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `guild` (
  `guild_id` bigint(20) unsigned NOT NULL AUTO_INCREMENT COMMENT '公会id',
  `exp` bigint(20) unsigned NOT NULL DEFAULT 0 COMMENT '经验',
  `wealth` bigint(20) unsigned NOT NULL DEFAULT 0 COMMENT '财富',
  `level` tinyint(3) unsigned NOT NULL DEFAULT 0 COMMENT '等级',
  `create_time` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '时间',
  `guild_name` char(16) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '名字((once)/(update_name))',
  `notice` char(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '公告((once)/(update_notice))',
  `leader_id` bigint(20) unsigned NOT NULL DEFAULT 0 COMMENT '会长id(join(`role`.`role_id`)/join(`vip`.`role_id`))',
  `leader_name` char(0) GENERATED ALWAYS AS ('') VIRTUAL COMMENT '会长名字(join(`role`.`role_name`))',
  `leader_sex` tinyint(3) unsigned GENERATED ALWAYS AS (0) VIRTUAL COMMENT '性别(join(`role`.`sex`))',
  `leader_class` tinyint(3) unsigned GENERATED ALWAYS AS (0) VIRTUAL COMMENT '会长名字(join(`role`.`classes`))',
  `leader_level` tinyint(3) unsigned GENERATED ALWAYS AS (0) VIRTUAL COMMENT '职业(join(`role`.`level`))',
  `leader_vip_level` tinyint(3) unsigned GENERATED ALWAYS AS (0) VIRTUAL COMMENT '会长名字(join(`vip`.`vip_level`))',
  `flag` tinyint(3) unsigned GENERATED ALWAYS AS (0) VIRTUAL COMMENT '标识(flag)',
  PRIMARY KEY (`guild_id`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci ROW_FORMAT=DYNAMIC COMMENT='公会表';
/*!40101 SET character_set_client = @saved_cs_client */;
DROP TABLE IF EXISTS `guild_apply`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `guild_apply` (
  `guild_id` bigint(20) unsigned NOT NULL DEFAULT 0 COMMENT '公会ID(join(`guild`.`guild_id`)/(delete_guild_id))',
  `role_id` bigint(20) unsigned NOT NULL DEFAULT 0 COMMENT '角色ID(join(`role`.`role_id`)/join(`vip`.`role_id`)/(delete_role_id))',
  `apply_time` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '时间',
  `guild_name` char(0) GENERATED ALWAYS AS ('') VIRTUAL COMMENT '帮派名(join(`guild`.`guild_name`))',
  `role_name` char(0) GENERATED ALWAYS AS ('') VIRTUAL COMMENT '角色名(join(`role`.`role_name`))',
  `sex` tinyint(3) unsigned GENERATED ALWAYS AS (0) VIRTUAL COMMENT '性别(join(`role`.`sex`))',
  `classes` tinyint(3) unsigned GENERATED ALWAYS AS (0) VIRTUAL COMMENT '职业(join(`role`.`classes`))',
  `level` tinyint(3) unsigned GENERATED ALWAYS AS (0) VIRTUAL COMMENT '职业(join(`role`.`level`))',
  `vip_level` tinyint(3) unsigned GENERATED ALWAYS AS (0) VIRTUAL COMMENT 'VIP等级(join(`vip`.`vip_level`))',
  `flag` tinyint(3) unsigned GENERATED ALWAYS AS (0) VIRTUAL COMMENT '标识(flag)',
  PRIMARY KEY (`guild_id`,`role_id`) USING BTREE,
  KEY `role_id` (`role_id`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci ROW_FORMAT=DYNAMIC COMMENT='公会申请表';
/*!40101 SET character_set_client = @saved_cs_client */;
DROP TABLE IF EXISTS `guild_level_data`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `guild_level_data` (
  `level` bigint(20) unsigned NOT NULL DEFAULT 0 COMMENT '等级',
  `exp` bigint(20) unsigned NOT NULL DEFAULT 0 COMMENT '经验'
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci ROW_FORMAT=DYNAMIC COMMENT='等级配置表';
/*!40101 SET character_set_client = @saved_cs_client */;
DROP TABLE IF EXISTS `guild_role`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `guild_role` (
  `guild_id` int(20) unsigned NOT NULL DEFAULT 0 COMMENT '公会ID(join(`guild`.`guild_id`))',
  `role_id` int(20) unsigned NOT NULL DEFAULT 0 COMMENT '角色ID(join(`role`.`role_id`)/join(`vip`.`role_id`))',
  `job` tinyint(3) unsigned NOT NULL DEFAULT 0 COMMENT '职位',
  `wealth` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '财富',
  `join_time` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '加入时间',
  `leave_time` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '离开时间',
  `guild_name` char(0) GENERATED ALWAYS AS ('') VIRTUAL COMMENT '帮派名(join(`guild`.`guild_name`))',
  `role_name` char(0) GENERATED ALWAYS AS ('') VIRTUAL COMMENT '角色名(join(`role`.`role_name`))',
  `sex` tinyint(3) unsigned GENERATED ALWAYS AS (0) VIRTUAL COMMENT '性别(join(`role`.`sex`))',
  `classes` tinyint(3) unsigned GENERATED ALWAYS AS (0) VIRTUAL COMMENT '职业(join(`role`.`classes`))',
  `level` tinyint(3) unsigned GENERATED ALWAYS AS (0) VIRTUAL COMMENT '职业(join(`role`.`level`))',
  `vip_level` tinyint(3) unsigned GENERATED ALWAYS AS (0) VIRTUAL COMMENT 'VIP等级(join(`vip`.`vip_level`))',
  `flag` tinyint(3) unsigned GENERATED ALWAYS AS (0) VIRTUAL COMMENT '标识(flag)',
  PRIMARY KEY (`role_id`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci ROW_FORMAT=DYNAMIC COMMENT='公会角色表';
/*!40101 SET character_set_client = @saved_cs_client */;
DROP TABLE IF EXISTS `increment`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `increment` (
  `name` char(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '0' COMMENT '名字',
  `value` bigint(20) unsigned NOT NULL DEFAULT 0 COMMENT '数值',
  PRIMARY KEY (`name`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci ROW_FORMAT=DYNAMIC COMMENT='自增表';
/*!40101 SET character_set_client = @saved_cs_client */;
DROP TABLE IF EXISTS `item`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `item` (
  `item_no` bigint(20) unsigned NOT NULL AUTO_INCREMENT COMMENT '物品编号',
  `role_id` bigint(20) unsigned NOT NULL DEFAULT 0 COMMENT '角色ID(select)',
  `item_id` int(20) unsigned NOT NULL DEFAULT 0 COMMENT '物品ID',
  `type` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '类型',
  `number` int(20) unsigned NOT NULL DEFAULT 1 COMMENT '数量',
  `expire_time` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '过期时间',
  `flag` tinyint(3) unsigned GENERATED ALWAYS AS (0) VIRTUAL COMMENT '标识(flag)',
  PRIMARY KEY (`item_no`) USING BTREE,
  KEY `role_id` (`role_id`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci ROW_FORMAT=DYNAMIC COMMENT='角色物品表';
/*!40101 SET character_set_client = @saved_cs_client */;
DROP TABLE IF EXISTS `item_consume_log`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `item_consume_log` (
  `id` int(10) unsigned NOT NULL AUTO_INCREMENT COMMENT 'ID',
  `role_id` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '角色ID',
  `item_id` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '物品ID',
  `operation` varchar(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '操作',
  `source` varchar(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '来源',
  `time` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '时间',
  PRIMARY KEY (`id`) USING BTREE,
  KEY `role_id` (`role_id`) USING BTREE,
  KEY `time` (`time`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci ROW_FORMAT=COMPRESSED COMMENT='物品消费日志表';
/*!40101 SET character_set_client = @saved_cs_client */;
DROP TABLE IF EXISTS `item_data`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `item_data` (
  `item_id` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '物品id',
  `type` tinyint(3) unsigned NOT NULL DEFAULT 0 COMMENT '类型(validate(item_type))',
  `overlap` int(10) unsigned NOT NULL DEFAULT 1 COMMENT '叠加数',
  `category` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '分类ID',
  `time` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '有效时间',
  `use_number` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '使用数量(0:不能直接使用/1:一个/N:N个)',
  `use_effect` varchar(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '使用效果(validate(use_effect))',
  `use_value` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '使用效果数值',
  `name` char(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '名字',
  `icon` char(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '图标',
  `description` char(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '描述',
  PRIMARY KEY (`item_id`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci ROW_FORMAT=DYNAMIC COMMENT='物品配置表';
/*!40101 SET character_set_client = @saved_cs_client */;
DROP TABLE IF EXISTS `item_produce_log`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `item_produce_log` (
  `id` int(10) unsigned NOT NULL AUTO_INCREMENT COMMENT 'ID',
  `role_id` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '角色ID',
  `item_id` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '物品ID',
  `operation` varchar(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '操作',
  `source` varchar(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '来源',
  `time` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '时间',
  PRIMARY KEY (`id`) USING BTREE,
  KEY `role_id` (`role_id`) USING BTREE,
  KEY `time` (`time`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci ROW_FORMAT=COMPRESSED COMMENT='物品产出日志表';
/*!40101 SET character_set_client = @saved_cs_client */;
DROP TABLE IF EXISTS `key`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `key` (
  `role_id` bigint(20) unsigned NOT NULL DEFAULT 0 COMMENT '角色ID',
  `key` varchar(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '码',
  PRIMARY KEY (`role_id`,`key`) USING BTREE,
  KEY `key` (`key`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci ROW_FORMAT=DYNAMIC COMMENT='角色兑换码表';
/*!40101 SET character_set_client = @saved_cs_client */;
DROP TABLE IF EXISTS `key_award_data`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `key_award_data` (
  `type` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '类型',
  `unique` varchar(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '是否唯一(validate(boolean))',
  `award` varchar(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '奖励',
  PRIMARY KEY (`type`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci ROW_FORMAT=DYNAMIC COMMENT='兑换码奖励配置表';
/*!40101 SET character_set_client = @saved_cs_client */;
DROP TABLE IF EXISTS `key_data`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `key_data` (
  `key` char(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '码',
  `type` tinyint(3) unsigned NOT NULL DEFAULT 0 COMMENT '类型',
  PRIMARY KEY (`key`) USING BTREE,
  KEY `key` (`key`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci ROW_FORMAT=DYNAMIC COMMENT='兑换码配置表';
/*!40101 SET character_set_client = @saved_cs_client */;
DROP TABLE IF EXISTS `level_data`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `level_data` (
  `level` bigint(20) unsigned NOT NULL DEFAULT 0 COMMENT '等级',
  `exp` bigint(20) unsigned NOT NULL DEFAULT 0 COMMENT '经验'
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci ROW_FORMAT=DYNAMIC COMMENT='等级配置表';
/*!40101 SET character_set_client = @saved_cs_client */;
DROP TABLE IF EXISTS `login_log`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `login_log` (
  `id` bigint(20) unsigned NOT NULL AUTO_INCREMENT COMMENT 'ID',
  `role_id` bigint(20) unsigned NOT NULL DEFAULT 0 COMMENT '角色ID',
  `ip` varchar(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '登录IP',
  `device_id` varchar(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '设备ID',
  `login_time` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '登录时间',
  `online_time` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '在线时间',
  `time` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '登出时间',
  PRIMARY KEY (`id`) USING BTREE,
  KEY `role_id` (`role_id`) USING BTREE,
  KEY `time` (`time`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci ROW_FORMAT=COMPRESSED COMMENT='登录日志';
/*!40101 SET character_set_client = @saved_cs_client */;
DROP TABLE IF EXISTS `lucky_money`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `lucky_money` (
  `lucky_money_id` bigint(20) unsigned NOT NULL AUTO_INCREMENT COMMENT '红包ID',
  `server_id` smallint(5) unsigned NOT NULL DEFAULT 0 COMMENT '服务器ID',
  `role_id` bigint(20) unsigned NOT NULL DEFAULT 0 COMMENT '角色ID',
  `role_name` char(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '角色名',
  `guild_id` bigint(20) unsigned NOT NULL DEFAULT 0 COMMENT '公会ID',
  `guild_name` char(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '公会名',
  `total_gold` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '总金币',
  `remain_gold` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '剩余金币',
  `total_number` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '总人数',
  `receive_number` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '已领取人数',
  `receive_list` varchar(0) GENERATED ALWAYS AS ('') VIRTUAL COMMENT '领取列表',
  `time` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '发送时间',
  `flag` tinyint(3) unsigned GENERATED ALWAYS AS (0) VIRTUAL COMMENT '标识(flag)',
  PRIMARY KEY (`lucky_money_id`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci ROW_FORMAT=DYNAMIC COMMENT='红包信息表';
/*!40101 SET character_set_client = @saved_cs_client */;
DROP TABLE IF EXISTS `lucky_money_role`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `lucky_money_role` (
  `lucky_money_id` bigint(20) unsigned NOT NULL DEFAULT 0 COMMENT '红包ID',
  `server_id` smallint(5) unsigned NOT NULL DEFAULT 0 COMMENT '服务器ID',
  `role_id` bigint(20) unsigned NOT NULL DEFAULT 0 COMMENT '角色ID',
  `role_name` char(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '角色名',
  `guild_id` bigint(20) unsigned NOT NULL DEFAULT 0 COMMENT '公会ID',
  `guild_name` char(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '公会名',
  `gold` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '领取金币数',
  `time` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '领取时间',
  `flag` tinyint(3) unsigned GENERATED ALWAYS AS (0) VIRTUAL COMMENT '标识(flag)',
  PRIMARY KEY (`lucky_money_id`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci ROW_FORMAT=DYNAMIC COMMENT='红包角色表';
/*!40101 SET character_set_client = @saved_cs_client */;
DROP TABLE IF EXISTS `mail`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `mail` (
  `mail_id` bigint(20) unsigned NOT NULL AUTO_INCREMENT COMMENT '邮件ID',
  `sender_id` bigint(20) unsigned NOT NULL DEFAULT 0 COMMENT '发送者',
  `sender_nick` char(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '发送者昵称',
  `receiver_id` bigint(20) unsigned NOT NULL DEFAULT 0 COMMENT '接收者(select)',
  `receiver_nick` char(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '接受者昵称',
  `receive_time` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '接收时间',
  `is_read` tinyint(3) unsigned NOT NULL DEFAULT 0 COMMENT '是否已经读取(update_read)',
  `read_time` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '读取时间(update_read)',
  `expire_time` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '过期时间',
  `is_receive_attachment` tinyint(3) unsigned NOT NULL DEFAULT 0 COMMENT '是否领取附件(update_receive)',
  `receive_attachment_time` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '领取附件时间(update_receive)',
  `from` varchar(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '来源',
  `title` char(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '标题',
  `content` char(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '内容',
  `attachment` varchar(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '附件',
  `flag` tinyint(3) unsigned GENERATED ALWAYS AS (0) VIRTUAL COMMENT '标识(flag)',
  PRIMARY KEY (`mail_id`) USING BTREE,
  KEY `receiver_id` (`receiver_id`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci ROW_FORMAT=DYNAMIC COMMENT='角色邮件表';
/*!40101 SET character_set_client = @saved_cs_client */;
DROP TABLE IF EXISTS `map_data`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `map_data` (
  `map_id` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '地图ID',
  `type` varchar(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '广播类型(validate(map_type))',
  `reconnect` varchar(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '是否重连(validate(boolean))',
  `monsters` varchar(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '随地图启动的怪物',
  `rank_key` varchar(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '榜键类型(validate(map_rank_key))',
  `rank_value` varchar(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '榜值类型(validate(map_rank_value))',
  `rank_mode` varchar(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '榜模式(validate(map_rank_mode))',
  `enter_points` varchar(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '进入点',
  `pk_mode` varchar(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT 'PK模式',
  `enter_script` varchar(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '进入脚本',
  `relive_script` varchar(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '复活脚本',
  `leave_script` varchar(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '离开脚本',
  PRIMARY KEY (`map_id`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci ROW_FORMAT=DYNAMIC COMMENT='地图配置表';
/*!40101 SET character_set_client = @saved_cs_client */;
DROP TABLE IF EXISTS `monster_data`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `monster_data` (
  `monster_id` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '怪物ID',
  `type` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '组ID',
  `name` char(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '怪物名称',
  `description` char(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '怪物描述',
  `level` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '等级',
  `hp` bigint(20) unsigned NOT NULL DEFAULT 0 COMMENT '血量',
  `map_id` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '地图ID',
  `camp` tinyint(3) unsigned NOT NULL DEFAULT 0 COMMENT '阵营',
  `range` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '攻击距离',
  `distance` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '搜索距离',
  `relive_time` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '复活时间',
  `act_type` varchar(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '动作类型(validate(act_type))',
  `act_script` varchar(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '动作脚本(enemy:敌人/role:玩家/monster:怪物/{monster,组ID}:特定怪物)',
  `skills` varchar(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '技能',
  `born_points` varchar(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '出生点',
  `award` varchar(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '奖励',
  PRIMARY KEY (`monster_id`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci ROW_FORMAT=DYNAMIC COMMENT='怪物配置表';
/*!40101 SET character_set_client = @saved_cs_client */;
DROP TABLE IF EXISTS `online_log`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `online_log` (
  `id` bigint(20) unsigned NOT NULL AUTO_INCREMENT COMMENT 'ID',
  `all` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '全部',
  `online` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '在线',
  `hosting` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '挂机',
  `hour` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '当前小时',
  `time` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '当前时间',
  PRIMARY KEY (`id`) USING BTREE,
  KEY `time` (`time`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci ROW_FORMAT=COMPRESSED COMMENT='在线统计日志';
/*!40101 SET character_set_client = @saved_cs_client */;
DROP TABLE IF EXISTS `parameter_data`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `parameter_data` (
  `key` varchar(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '参数键',
  `value` varchar(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '参数值',
  `description` char(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '参数名称',
  PRIMARY KEY (`key`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci ROW_FORMAT=DYNAMIC COMMENT='游戏参数配置表';
/*!40101 SET character_set_client = @saved_cs_client */;
DROP TABLE IF EXISTS `quest`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `quest` (
  `role_id` bigint(20) unsigned NOT NULL DEFAULT 0 COMMENT '角色ID(select)',
  `quest_id` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '任务ID',
  `type` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '类型',
  `event` varchar(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '事件',
  `target` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '目标',
  `number` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '数量',
  `compare` varchar(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '比较',
  `award` tinyint(3) unsigned NOT NULL DEFAULT 0 COMMENT '是否领取奖励',
  `flag` tinyint(3) unsigned GENERATED ALWAYS AS (0) VIRTUAL COMMENT '标识(flag)',
  PRIMARY KEY (`role_id`,`type`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci ROW_FORMAT=DYNAMIC COMMENT='角色任务表';
/*!40101 SET character_set_client = @saved_cs_client */;
DROP TABLE IF EXISTS `quest_data`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `quest_data` (
  `quest_id` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '任务ID',
  `type` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '类型',
  `pre_id` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '前置任务',
  `next_id` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '后置任务',
  `module` varchar(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '模块(validate(module))',
  `function` varchar(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '函数(validate(function))',
  `event` varchar(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '事件(validate(event))',
  `compare` varchar(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '比较模式(validate(compare))',
  `target` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '目标',
  `number` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '数量',
  `condition` varchar(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '条件',
  `cost` varchar(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '消耗',
  `award` varchar(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '奖励',
  `title` char(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '标题',
  `content` char(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '内容',
  `description` char(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '描述',
  PRIMARY KEY (`quest_id`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci ROW_FORMAT=DYNAMIC COMMENT='任务配置表';
/*!40101 SET character_set_client = @saved_cs_client */;
DROP TABLE IF EXISTS `quest_log`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `quest_log` (
  `id` int(10) unsigned NOT NULL AUTO_INCREMENT COMMENT 'ID',
  `role_id` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '角色ID',
  `quest_id` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '任务ID',
  `time` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '时间',
  PRIMARY KEY (`id`) USING BTREE,
  KEY `role_id` (`role_id`) USING BTREE,
  KEY `time` (`time`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci ROW_FORMAT=COMPRESSED COMMENT='任务日志表';
/*!40101 SET character_set_client = @saved_cs_client */;
DROP TABLE IF EXISTS `rank`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `rank` (
  `type` tinyint(3) unsigned NOT NULL DEFAULT 0 COMMENT '类型(select)(delete_type)',
  `order` bigint(20) unsigned NOT NULL DEFAULT 0 COMMENT '排名',
  `key` bigint(20) unsigned NOT NULL DEFAULT 0 COMMENT '键',
  `value` bigint(20) unsigned NOT NULL DEFAULT 0 COMMENT '值',
  `time` int(20) unsigned NOT NULL DEFAULT 0 COMMENT '时间',
  `name` char(16) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '名字',
  `server_id` smallint(5) unsigned NOT NULL DEFAULT 0 COMMENT '服务器ID',
  `digest` varchar(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '摘要数据',
  `extra` varchar(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '额外数据',
  `other` varchar(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '其他数据',
  `flag` tinyint(3) unsigned GENERATED ALWAYS AS (1) VIRTUAL COMMENT '标识(flag)',
  PRIMARY KEY (`type`,`order`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci ROW_FORMAT=DYNAMIC COMMENT='角色排行表';
/*!40101 SET character_set_client = @saved_cs_client */;
DROP TABLE IF EXISTS `recharge`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `recharge` (
  `recharge_no` bigint(20) unsigned NOT NULL AUTO_INCREMENT COMMENT '充值编号',
  `recharge_id` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '充值ID',
  `channel` char(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '0' COMMENT '渠道',
  `server_id` smallint(5) unsigned NOT NULL DEFAULT 0 COMMENT '区服ID',
  `role_id` bigint(20) unsigned NOT NULL DEFAULT 0 COMMENT '玩家ID',
  `role_name` char(16) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '玩家名称',
  `account` char(16) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '平台账号ID',
  `money` decimal(10,2) unsigned NOT NULL DEFAULT 0.00 COMMENT '充值金额',
  `gold` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '金币',
  `status` tinyint(3) unsigned NOT NULL DEFAULT 0 COMMENT '状态(0:未发放/1:已发放)',
  `time` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '订单时间',
  `receive_time` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '发放时间',
  PRIMARY KEY (`recharge_no`) USING BTREE,
  KEY `channel` (`channel`) USING BTREE,
  KEY `time` (`time`) USING BTREE,
  KEY `role_id` (`role_id`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci ROW_FORMAT=DYNAMIC COMMENT='角色充值订单表';
/*!40101 SET character_set_client = @saved_cs_client */;
DROP TABLE IF EXISTS `recharge_data`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `recharge_data` (
  `recharge_id` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '充值ID',
  `type` tinyint(3) unsigned NOT NULL DEFAULT 0 COMMENT '类型(普通充值:0/购买月卡:1)',
  `limit` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '限制数量',
  `original_price` decimal(10,2) unsigned NOT NULL DEFAULT 0.00 COMMENT '原价',
  `now_price` decimal(10,2) unsigned NOT NULL DEFAULT 0.00 COMMENT '现价',
  `gold` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '金币',
  `gift_gold` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '赠送金币',
  `begin_open_days` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '结束时间，跟开服相关，填天数',
  `end_open_days` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '结束时间，跟开服相关，填天数',
  `sort` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '排序',
  `icon` char(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '图片',
  `name` char(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '名字',
  `description` char(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '描述',
  PRIMARY KEY (`recharge_id`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci ROW_FORMAT=DYNAMIC COMMENT='充值配置表';
/*!40101 SET character_set_client = @saved_cs_client */;
DROP TABLE IF EXISTS `role`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `role` (
  `role_id` bigint(20) unsigned NOT NULL AUTO_INCREMENT COMMENT '角色ID',
  `role_name` char(16) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '角色名((once)/(update_name))',
  `server_id` smallint(5) unsigned NOT NULL DEFAULT 0 COMMENT '服ID',
  `account` char(16) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '账户(once)',
  `type` tinyint(3) unsigned NOT NULL DEFAULT 0 COMMENT '账户类型',
  `level` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '等级',
  `sex` tinyint(3) unsigned NOT NULL DEFAULT 0 COMMENT '性别',
  `classes` tinyint(3) unsigned NOT NULL DEFAULT 0 COMMENT '职业',
  `item_size` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '普通背包大小',
  `bag_size` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '装备背包大小',
  `store_size` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '仓库背包大小',
  `online` tinyint(3) unsigned NOT NULL DEFAULT 0 COMMENT '是否在线',
  `online_time` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '在线时间',
  `register_time` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '注册时间',
  `first_recharge_time` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '首充时间',
  `channel` char(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '渠道',
  `map` varchar(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '地图',
  `device_id` char(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '设备ID',
  `device_type` char(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '设备类型',
  `mac` char(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT 'Mac地址',
  `ip` char(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT 'IP地址',
  PRIMARY KEY (`role_id`) USING BTREE,
  KEY `account` (`account`) USING BTREE,
  KEY `online_time` (`online_time`) USING BTREE,
  KEY `register_time` (`register_time`) USING BTREE,
  KEY `first_recharge_time` (`first_recharge_time`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci ROW_FORMAT=DYNAMIC COMMENT='角色信息表';
/*!40101 SET character_set_client = @saved_cs_client */;
DROP TABLE IF EXISTS `role_log`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `role_log` (
  `id` bigint(20) unsigned NOT NULL AUTO_INCREMENT COMMENT 'ID',
  `role_id` bigint(20) unsigned NOT NULL DEFAULT 0 COMMENT '角色ID',
  `exp` bigint(20) unsigned NOT NULL DEFAULT 0 COMMENT '经验',
  `time` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '时间',
  PRIMARY KEY (`id`) USING BTREE,
  KEY `role_id` (`role_id`) USING BTREE,
  KEY `time` (`time`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci ROW_FORMAT=COMPRESSED COMMENT='角色日志表';
/*!40101 SET character_set_client = @saved_cs_client */;
DROP TABLE IF EXISTS `sensitive_word_data`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `sensitive_word_data` (
  `word` varchar(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '敏感词',
  PRIMARY KEY (`word`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci ROW_FORMAT=DYNAMIC COMMENT='敏感词配置表';
/*!40101 SET character_set_client = @saved_cs_client */;
DROP TABLE IF EXISTS `shop`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `shop` (
  `role_id` bigint(20) unsigned NOT NULL DEFAULT 0 COMMENT '角色ID(select)',
  `shop_id` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '商店ID',
  `number` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '数量',
  `flag` tinyint(3) unsigned GENERATED ALWAYS AS (0) VIRTUAL COMMENT '标识(flag)',
  PRIMARY KEY (`role_id`,`shop_id`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci ROW_FORMAT=DYNAMIC COMMENT='角色商店表';
/*!40101 SET character_set_client = @saved_cs_client */;
DROP TABLE IF EXISTS `shop_data`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `shop_data` (
  `shop_id` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '商店ID',
  `item_id` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '物品配置ID',
  `type` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '商店类型',
  `pay_asset` varchar(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '货币类型(validate(asset))',
  `price` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '价格',
  `number` int(10) unsigned NOT NULL DEFAULT 1 COMMENT '数量',
  `level` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '等级限制',
  `limit` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '购买上限',
  `vip_level` int(10) unsigned NOT NULL DEFAULT 0 COMMENT 'vip等级限购',
  `vip_limit` varchar(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT 'vip等级购买上限',
  `description` char(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '描述',
  PRIMARY KEY (`shop_id`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci ROW_FORMAT=DYNAMIC COMMENT='商店配置表';
/*!40101 SET character_set_client = @saved_cs_client */;
DROP TABLE IF EXISTS `shop_log`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `shop_log` (
  `id` int(10) unsigned NOT NULL AUTO_INCREMENT COMMENT 'ID',
  `role_id` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '角色ID',
  `shop_id` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '商店ID',
  `number` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '购买数量',
  `time` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '时间',
  PRIMARY KEY (`id`) USING BTREE,
  KEY `role_id` (`role_id`) USING BTREE,
  KEY `time` (`time`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci ROW_FORMAT=COMPRESSED COMMENT='商店日志表';
/*!40101 SET character_set_client = @saved_cs_client */;
DROP TABLE IF EXISTS `sign`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `sign` (
  `role_id` bigint(20) unsigned NOT NULL DEFAULT 0 COMMENT '角色ID',
  `login_day` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '登录天数',
  `sign_total` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '签到总数',
  `is_sign_today` tinyint(3) unsigned NOT NULL DEFAULT 0 COMMENT '今天是否签到',
  PRIMARY KEY (`role_id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci COMMENT='角色签到表';
/*!40101 SET character_set_client = @saved_cs_client */;
DROP TABLE IF EXISTS `sign_data`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `sign_data` (
  `day` tinyint(3) unsigned NOT NULL DEFAULT 0 COMMENT '签到天数',
  `award` varchar(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '奖励',
  PRIMARY KEY (`day`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci COMMENT='签到配置表';
/*!40101 SET character_set_client = @saved_cs_client */;
DROP TABLE IF EXISTS `skill`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `skill` (
  `role_id` bigint(20) unsigned NOT NULL DEFAULT 0 COMMENT '角色ID(select)',
  `skill_id` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '技能ID',
  `level` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '等级',
  `flag` tinyint(3) unsigned GENERATED ALWAYS AS (0) VIRTUAL COMMENT '标识(flag)',
  PRIMARY KEY (`role_id`,`skill_id`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci ROW_FORMAT=DYNAMIC COMMENT='角色技能表';
/*!40101 SET character_set_client = @saved_cs_client */;
DROP TABLE IF EXISTS `skill_data`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `skill_data` (
  `skill_id` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '技能ID',
  `type` varchar(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '类型(validate(skill_type))',
  `name` char(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '名字',
  `condition` varchar(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '学习条件',
  `cost` varchar(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '升级消耗',
  `effect` varchar(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '作用效果',
  `cd` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '冷却时间',
  `radius` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '作用半径',
  `distance` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '作用距离',
  `number` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '作用对象数',
  `buffs` varchar(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '作用Buff',
  `before_effects` varchar(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '效果前',
  `hit_effects` varchar(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '击中效果',
  `after_effects` varchar(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '效果后',
  `description` char(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '描述',
  PRIMARY KEY (`skill_id`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci ROW_FORMAT=DYNAMIC COMMENT='技能配置表';
/*!40101 SET character_set_client = @saved_cs_client */;
DROP TABLE IF EXISTS `text_data`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `text_data` (
  `key` varchar(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '键',
  `en` char(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '英文',
  `sc` char(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '简体中文',
  `tc` char(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '翻译中文',
  `description` char(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '描述',
  PRIMARY KEY (`key`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci ROW_FORMAT=DYNAMIC COMMENT='游戏文本配置表';
/*!40101 SET character_set_client = @saved_cs_client */;
DROP TABLE IF EXISTS `title`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `title` (
  `role_id` bigint(20) unsigned NOT NULL DEFAULT 0 COMMENT '角色ID(select)(update_role_id)',
  `title_id` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '称号ID(select_id)',
  `type` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '类型',
  `expire_time` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '过期时间',
  `flag` tinyint(3) unsigned GENERATED ALWAYS AS (0) VIRTUAL COMMENT '标识(flag)',
  PRIMARY KEY (`role_id`,`title_id`) USING BTREE,
  KEY ```title_id``` (`title_id`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci ROW_FORMAT=DYNAMIC COMMENT='角色称号表';
/*!40101 SET character_set_client = @saved_cs_client */;
DROP TABLE IF EXISTS `title_data`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `title_data` (
  `title_id` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '称号ID',
  `type` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '类型',
  `multi` varchar(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '同类型可否拥有多个(validate(boolean))',
  `unique` varchar(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '是否全服唯一(validate(boolean))',
  `time` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '有效时间',
  `attribute` varchar(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '属性',
  `name` char(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '称号名字',
  `description` char(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '称号描述',
  PRIMARY KEY (`title_id`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci ROW_FORMAT=DYNAMIC COMMENT='称号配置表';
/*!40101 SET character_set_client = @saved_cs_client */;
DROP TABLE IF EXISTS `title_log`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `title_log` (
  `id` bigint(20) unsigned NOT NULL AUTO_INCREMENT COMMENT 'ID',
  `role_id` bigint(20) unsigned NOT NULL DEFAULT 0 COMMENT '角色ID',
  `title_id` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '称号ID',
  `from` varchar(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '来源',
  `time` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '时间',
  PRIMARY KEY (`id`) USING BTREE,
  KEY `role_id` (`role_id`) USING BTREE,
  KEY `time` (`time`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci ROW_FORMAT=COMPRESSED COMMENT='称号日志表';
/*!40101 SET character_set_client = @saved_cs_client */;
DROP TABLE IF EXISTS `total_login_log`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `total_login_log` (
  `id` bigint(20) unsigned NOT NULL AUTO_INCREMENT COMMENT 'ID',
  `total` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '总数',
  `hour_list` varchar(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '每小时总数列表',
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci ROW_FORMAT=COMPRESSED COMMENT='总登录日志';
/*!40101 SET character_set_client = @saved_cs_client */;
DROP TABLE IF EXISTS `vip`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `vip` (
  `role_id` bigint(20) unsigned NOT NULL DEFAULT 0 COMMENT '角色id',
  `vip_level` tinyint(3) unsigned NOT NULL DEFAULT 0 COMMENT 'vip等级',
  `exp` bigint(20) unsigned NOT NULL DEFAULT 0 COMMENT 'vip经验',
  `expire_time` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '过期时间',
  PRIMARY KEY (`role_id`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci ROW_FORMAT=DYNAMIC COMMENT='角色vip表';
/*!40101 SET character_set_client = @saved_cs_client */;
DROP TABLE IF EXISTS `vip_data`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `vip_data` (
  `vip` tinyint(3) unsigned NOT NULL DEFAULT 0 COMMENT 'VIP等级',
  `exp` bigint(20) unsigned NOT NULL DEFAULT 0 COMMENT '经验',
  PRIMARY KEY (`vip`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci ROW_FORMAT=DYNAMIC COMMENT='vip配置表';
/*!40101 SET character_set_client = @saved_cs_client */;

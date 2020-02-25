/*
 Navicat Premium Data Transfer

 Source Server         : ubuntu
 Source Server Type    : MariaDB
 Source Server Version : 100412
 Source Host           : 192.168.1.77:3306
 Source Schema         : main

 Target Server Type    : MariaDB
 Target Server Version : 100412
 File Encoding         : 65001

 Date: 24/02/2020 19:51:51
*/

SET NAMES utf8mb4;
SET FOREIGN_KEY_CHECKS = 0;

-- ----------------------------
-- Table structure for activity_data
-- ----------------------------
DROP TABLE IF EXISTS `activity_data`;
CREATE TABLE `activity_data`  (
  `activity_id` int(10) UNSIGNED NOT NULL DEFAULT 0 COMMENT '活动ID',
  `mode` tinyint(1) NOT NULL DEFAULT 0 COMMENT '活动模式(validate(node_type_integer))',
  `service` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL DEFAULT '' COMMENT '服务进程模块(validate(activity_service))',
  `type` int(10) UNSIGNED NOT NULL DEFAULT 0 COMMENT '类型',
  `subtype` int(10) UNSIGNED NOT NULL DEFAULT 0 COMMENT '子类型',
  `award_type` tinyint(1) UNSIGNED NOT NULL DEFAULT 0 COMMENT '领奖类型(自动:0/手动:1)',
  `show_time` int(10) UNSIGNED NOT NULL DEFAULT 0 COMMENT '图标展示时间(时间戳)',
  `start_time` int(10) UNSIGNED NOT NULL DEFAULT 0 COMMENT '开始时间(时间戳)',
  `over_time` int(10) UNSIGNED NOT NULL DEFAULT 0 COMMENT '结束时间(时间戳)',
  `award_time` int(10) UNSIGNED NOT NULL DEFAULT 0 COMMENT '领奖时间(时间戳)',
  `stop_time` int(10) UNSIGNED NOT NULL DEFAULT 0 COMMENT '图标消失时间(时间戳)',
  `show_hour` int(10) UNSIGNED NOT NULL DEFAULT 0 COMMENT '每天展示小时',
  `start_hour` int(10) UNSIGNED NOT NULL DEFAULT 0 COMMENT '每天开始小时',
  `over_hour` int(10) UNSIGNED NOT NULL DEFAULT 0 COMMENT '每天结束小时',
  `start_award_hour` int(10) UNSIGNED NOT NULL DEFAULT 0 COMMENT '每天领奖开始小时',
  `over_award_hour` int(10) UNSIGNED NOT NULL DEFAULT 0 COMMENT '每天领奖结束小时',
  `min_open_days` int(10) UNSIGNED NOT NULL DEFAULT 0 COMMENT '最小开服天数',
  `max_open_days` int(10) UNSIGNED NOT NULL DEFAULT 0 COMMENT '最大开服天数',
  `name` char(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL DEFAULT '' COMMENT '活动名',
  `icon` char(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL DEFAULT '' COMMENT '活动图标',
  `entrance` char(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL DEFAULT '' COMMENT '活动入口',
  `description` char(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL DEFAULT '' COMMENT '活动描述',
  PRIMARY KEY (`activity_id`) USING BTREE
) ENGINE = InnoDB CHARACTER SET = utf8mb4 COLLATE = utf8mb4_general_ci COMMENT = '活动配置表' ROW_FORMAT = Dynamic;

-- ----------------------------
-- Records of activity_data
-- ----------------------------
INSERT INTO `activity_data` VALUES (1, 1, '', 1, 1, 0, 1577808000, 1577808000, 1580486400, 1580486400, 1580486400, 9, 10, 22, 22, 23, 3, 7, '活动名', 'activity.icon', 'activity', '活动描述');
INSERT INTO `activity_data` VALUES (2, 2, '', 1, 1, 0, 1577808000, 1577808000, 1580486400, 1580486400, 1580486400, 9, 10, 22, 22, 23, 3, 7, '活动名', 'activity.icon', 'activity', '活动描述');
INSERT INTO `activity_data` VALUES (3, 4, '', 1, 1, 0, 1577808000, 1577808000, 1580486400, 1580486400, 1580486400, 9, 10, 22, 22, 23, 3, 7, '活动名', 'activity.icon', 'activity', '活动描述');

-- ----------------------------
-- Table structure for asset
-- ----------------------------
DROP TABLE IF EXISTS `asset`;
CREATE TABLE `asset`  (
  `role_id` bigint(20) UNSIGNED NOT NULL DEFAULT 0 COMMENT '角色ID',
  `gold` bigint(20) UNSIGNED NOT NULL DEFAULT 0 COMMENT '金币',
  `silver` bigint(20) UNSIGNED NOT NULL DEFAULT 0 COMMENT '银币',
  `copper` bigint(20) UNSIGNED NOT NULL DEFAULT 0 COMMENT '铜币',
  `coin` bigint(20) UNSIGNED NOT NULL DEFAULT 0 COMMENT '硬币',
  `exp` bigint(20) UNSIGNED NOT NULL DEFAULT 0 COMMENT '经验',
  PRIMARY KEY (`role_id`) USING BTREE
) ENGINE = InnoDB CHARACTER SET = utf8mb4 COLLATE = utf8mb4_general_ci COMMENT = '角色资产表' ROW_FORMAT = Dynamic;

-- ----------------------------
-- Records of asset
-- ----------------------------
INSERT INTO `asset` VALUES (1, 1000000, 1000000, 1000000, 1011200, 1000000);

-- ----------------------------
-- Table structure for asset_data
-- ----------------------------
DROP TABLE IF EXISTS `asset_data`;
CREATE TABLE `asset_data`  (
  `asset` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL DEFAULT '' COMMENT '资产类型',
  `item_id` int(10) UNSIGNED NOT NULL DEFAULT 0 COMMENT '物品配置ID',
  PRIMARY KEY (`asset`) USING BTREE
) ENGINE = InnoDB CHARACTER SET = utf8mb4 COLLATE = utf8mb4_general_ci COMMENT = '资产物品映射配置表' ROW_FORMAT = Dynamic;

-- ----------------------------
-- Records of asset_data
-- ----------------------------
INSERT INTO `asset_data` VALUES ('coin', 100004);
INSERT INTO `asset_data` VALUES ('copper', 100003);
INSERT INTO `asset_data` VALUES ('exp', 100005);
INSERT INTO `asset_data` VALUES ('gold', 100001);
INSERT INTO `asset_data` VALUES ('silver', 100002);

-- ----------------------------
-- Table structure for attribute_data
-- ----------------------------
DROP TABLE IF EXISTS `attribute_data`;
CREATE TABLE `attribute_data`  (
  `attribute_id` int(10) UNSIGNED NOT NULL DEFAULT 0 COMMENT '属性ID',
  `attribute` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL DEFAULT '' COMMENT '属性',
  `type` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL DEFAULT '' COMMENT '类型(固定值/万分比)',
  `merge` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL DEFAULT '' COMMENT '合并计算公式',
  `effect` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL DEFAULT '' COMMENT '效果',
  `name` char(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL DEFAULT '' COMMENT '名字',
  `description` char(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL DEFAULT '' COMMENT '描述',
  PRIMARY KEY (`attribute_id`) USING BTREE
) ENGINE = InnoDB CHARACTER SET = utf8mb4 COLLATE = utf8mb4_general_ci COMMENT = '属性配置表' ROW_FORMAT = Dynamic;

-- ----------------------------
-- Records of attribute_data
-- ----------------------------
INSERT INTO `attribute_data` VALUES (1, 'fc', 'fix', 'fc', '', '战力', '战力');
INSERT INTO `attribute_data` VALUES (2, 'hp', 'fix', '', '', '血量', '血量');
INSERT INTO `attribute_data` VALUES (3, 'attack', 'fix', 'attack', '', '攻击', '攻击');
INSERT INTO `attribute_data` VALUES (4, 'defense', 'fix', 'defense', '', '防御', '防御');
INSERT INTO `attribute_data` VALUES (5, 'health', 'fix', 'health', '', '生命', '生命');
INSERT INTO `attribute_data` VALUES (6, 'hit', 'fix', 'hit', '', '命中', '命中');
INSERT INTO `attribute_data` VALUES (7, 'duck', 'fix', 'duck', '', '闪避', '闪避');
INSERT INTO `attribute_data` VALUES (8, 'freeze', 'fix', '', 'cannot_be_attack', '冰冻', '冰冻');
INSERT INTO `attribute_data` VALUES (9, 'destroy', 'fix', '', '', '毁灭', '毁灭');
INSERT INTO `attribute_data` VALUES (10, 'vertigo', 'fix', '', '', '眩晕', '眩晕');

-- ----------------------------
-- Table structure for auction
-- ----------------------------
DROP TABLE IF EXISTS `auction`;
CREATE TABLE `auction`  (
  `unique_id` bigint(20) UNSIGNED NOT NULL AUTO_INCREMENT COMMENT '唯一ID',
  `auction_id` int(10) UNSIGNED NOT NULL DEFAULT 0 COMMENT '拍品ID',
  `number` int(10) UNSIGNED NOT NULL DEFAULT 0 COMMENT '拍品数量',
  `type` tinyint(1) UNSIGNED NOT NULL DEFAULT 0 COMMENT '拍卖类型(1:公会拍卖/2:全服拍卖/3:个人拍卖)',
  `start_time` int(10) UNSIGNED NOT NULL DEFAULT 0 COMMENT '开始时间',
  `end_time` int(10) UNSIGNED NOT NULL DEFAULT 0 COMMENT '结束时间',
  `from` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL DEFAULT '' COMMENT '物品来源',
  `bid_number` smallint(5) UNSIGNED NOT NULL DEFAULT 0 COMMENT '加价次数',
  `price` int(10) UNSIGNED NOT NULL DEFAULT 0 COMMENT '当前价格',
  `seller_list` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL DEFAULT '' COMMENT '卖家列表',
  `role_id` bigint(20) UNSIGNED NOT NULL DEFAULT 0 COMMENT '出价者ID',
  `role_name` char(16) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL DEFAULT '' COMMENT '出价者名字',
  `server_id` smallint(5) UNSIGNED NOT NULL DEFAULT 0 COMMENT '出价者服ID',
  `timer` varchar(0) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL DEFAULT '' COMMENT '定时器',
  `flag` varchar(0) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL DEFAULT '' COMMENT '标识(flag)',
  PRIMARY KEY (`unique_id`) USING BTREE
) ENGINE = InnoDB AUTO_INCREMENT = 2 CHARACTER SET = utf8mb4 COLLATE = utf8mb4_general_ci COMMENT = '拍卖信息表' ROW_FORMAT = Dynamic;

-- ----------------------------
-- Records of auction
-- ----------------------------

-- ----------------------------
-- Table structure for auction_data
-- ----------------------------
DROP TABLE IF EXISTS `auction_data`;
CREATE TABLE `auction_data`  (
  `auction_id` int(10) UNSIGNED NOT NULL DEFAULT 0 COMMENT '拍品ID',
  `auction_type` int(10) UNSIGNED NOT NULL DEFAULT 0 COMMENT '竞拍类型(1:竞价/2:一口价)',
  `begin_price` int(10) UNSIGNED NOT NULL DEFAULT 0 COMMENT '底价',
  `add_price` int(10) UNSIGNED NOT NULL DEFAULT 0 COMMENT '每次加价',
  `tax` int(10) UNSIGNED NOT NULL DEFAULT 0 COMMENT '税收',
  `show_time` int(10) UNSIGNED NOT NULL DEFAULT 0 COMMENT '预览时间',
  `auction_time` int(10) UNSIGNED NOT NULL DEFAULT 0 COMMENT '拍卖时间',
  `critical_time` int(10) UNSIGNED NOT NULL DEFAULT 0 COMMENT '临界时间(出价加时的临界时间)',
  `overtime` int(10) UNSIGNED NOT NULL DEFAULT 0 COMMENT '延迟时间(出价加时的时间)',
  PRIMARY KEY (`auction_id`) USING BTREE
) ENGINE = InnoDB CHARACTER SET = utf8mb4 COLLATE = utf8mb4_general_ci COMMENT = '拍卖配置表' ROW_FORMAT = Dynamic;

-- ----------------------------
-- Records of auction_data
-- ----------------------------
INSERT INTO `auction_data` VALUES (1, 1, 1, 1, 0, 0, 0, 0, 0);

-- ----------------------------
-- Table structure for auction_log
-- ----------------------------
DROP TABLE IF EXISTS `auction_log`;
CREATE TABLE `auction_log`  (
  `id` bigint(20) UNSIGNED NOT NULL AUTO_INCREMENT COMMENT 'ID',
  `auction_id` int(10) UNSIGNED NOT NULL DEFAULT 0 COMMENT '拍品ID',
  `number` int(10) UNSIGNED NOT NULL DEFAULT 0 COMMENT '拍品数量',
  `bid_number` int(10) UNSIGNED NOT NULL DEFAULT 0 COMMENT '加价次数',
  `price` int(10) UNSIGNED NOT NULL DEFAULT 0 COMMENT '成交价',
  `role_id` bigint(20) UNSIGNED NOT NULL DEFAULT 0 COMMENT '获得者ID',
  `role_name` char(16) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL DEFAULT '' COMMENT '获得者名字',
  `server_id` int(10) UNSIGNED NOT NULL DEFAULT 0 COMMENT '获得者服ID',
  `time` int(10) UNSIGNED NOT NULL DEFAULT 0 COMMENT '时间',
  PRIMARY KEY (`id`) USING BTREE,
  INDEX `role_id`(`role_id`) USING BTREE,
  INDEX `time`(`time`) USING BTREE
) ENGINE = InnoDB AUTO_INCREMENT = 1 CHARACTER SET = utf8mb4 COLLATE = utf8mb4_general_ci COMMENT = '拍卖日志表' ROW_FORMAT = Compressed;

-- ----------------------------
-- Records of auction_log
-- ----------------------------

-- ----------------------------
-- Table structure for buff
-- ----------------------------
DROP TABLE IF EXISTS `buff`;
CREATE TABLE `buff`  (
  `role_id` bigint(20) UNSIGNED NOT NULL DEFAULT 0 COMMENT '角色ID(select)',
  `buff_id` int(10) UNSIGNED NOT NULL DEFAULT 0 COMMENT '状态增益ID',
  `expire_time` int(10) UNSIGNED NOT NULL DEFAULT 0 COMMENT '结束时间',
  `overlap` int(10) UNSIGNED NOT NULL DEFAULT 1 COMMENT '叠加数',
  `flag` varchar(0) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL DEFAULT '' COMMENT '标识(flag)',
  PRIMARY KEY (`role_id`, `buff_id`) USING BTREE
) ENGINE = InnoDB CHARACTER SET = utf8mb4 COLLATE = utf8mb4_general_ci COMMENT = '角色buff表' ROW_FORMAT = Dynamic;

-- ----------------------------
-- Records of buff
-- ----------------------------
INSERT INTO `buff` VALUES (1, 1, 0, 1, '');
INSERT INTO `buff` VALUES (1, 2, 0, 1, '');

-- ----------------------------
-- Table structure for buff_data
-- ----------------------------
DROP TABLE IF EXISTS `buff_data`;
CREATE TABLE `buff_data`  (
  `buff_id` int(10) UNSIGNED NOT NULL DEFAULT 0 COMMENT '增益状态(Buff)ID',
  `type` int(10) UNSIGNED NOT NULL DEFAULT 0 COMMENT '类型',
  `time` int(10) UNSIGNED NOT NULL DEFAULT 0 COMMENT '有效时间',
  `effect` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL DEFAULT '' COMMENT '效果',
  `temporary` tinyint(1) UNSIGNED NOT NULL DEFAULT 0 COMMENT '是否临时的(切地图失效)',
  `overlap_type` tinyint(1) UNSIGNED NOT NULL DEFAULT 0 COMMENT '叠加类型(0:不叠加/1:时间/2:数值/3:都叠加)',
  `name` char(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL DEFAULT '' COMMENT '名字',
  `description` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL DEFAULT '' COMMENT '描述',
  PRIMARY KEY (`buff_id`) USING BTREE
) ENGINE = InnoDB CHARACTER SET = utf8mb4 COLLATE = utf8mb4_general_ci COMMENT = 'buff配置表' ROW_FORMAT = Dynamic;

-- ----------------------------
-- Records of buff_data
-- ----------------------------
INSERT INTO `buff_data` VALUES (1, 1, 0, '[9]', 0, 1, '铜币', '');
INSERT INTO `buff_data` VALUES (2, 1, 0, '[10]', 0, 1, '经验', '');

-- ----------------------------
-- Table structure for count
-- ----------------------------
DROP TABLE IF EXISTS `count`;
CREATE TABLE `count`  (
  `role_id` bigint(20) UNSIGNED NOT NULL DEFAULT 0 COMMENT '角色ID(select)',
  `type` int(64) UNSIGNED NOT NULL DEFAULT 0 COMMENT '计数类型',
  `today_number` int(10) UNSIGNED NOT NULL DEFAULT 0 COMMENT '今天数量',
  `week_number` int(10) UNSIGNED NOT NULL DEFAULT 0 COMMENT '今周数量',
  `total_number` int(10) UNSIGNED NOT NULL DEFAULT 0 COMMENT '总数',
  `time` int(10) UNSIGNED NOT NULL DEFAULT 0 COMMENT '时间',
  `flag` varchar(0) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL DEFAULT '' COMMENT '标识(flag)',
  PRIMARY KEY (`role_id`, `type`) USING BTREE,
  INDEX `type`(`type`) USING BTREE
) ENGINE = InnoDB CHARACTER SET = utf8mb4 COLLATE = utf8mb4_general_ci COMMENT = '角色计数表' ROW_FORMAT = Dynamic;

-- ----------------------------
-- Records of count
-- ----------------------------
INSERT INTO `count` VALUES (1, 1, 0, 0, 1, 1578540442, '');
INSERT INTO `count` VALUES (1, 2, 0, 0, 1, 1578540442, '');
INSERT INTO `count` VALUES (1, 3, 0, 0, 1, 1578540442, '');

-- ----------------------------
-- Table structure for dungeon
-- ----------------------------
DROP TABLE IF EXISTS `dungeon`;
CREATE TABLE `dungeon`  (
  `role_id` bigint(20) UNSIGNED NOT NULL DEFAULT 0 COMMENT '玩家ID(select)',
  `dungeon_id` int(10) UNSIGNED NOT NULL DEFAULT 0 COMMENT '副本ID',
  `type` int(10) UNSIGNED NOT NULL DEFAULT 0 COMMENT '类型',
  `today_number` int(10) UNSIGNED NOT NULL DEFAULT 0 COMMENT '今天次数',
  `total_number` int(10) UNSIGNED NOT NULL DEFAULT 0 COMMENT '历史总次数',
  `flag` varchar(0) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL DEFAULT '' COMMENT '标识(flag)',
  PRIMARY KEY (`role_id`, `type`) USING BTREE
) ENGINE = InnoDB CHARACTER SET = utf8mb4 COLLATE = utf8mb4_general_ci COMMENT = '角色副本表' ROW_FORMAT = Dynamic;

-- ----------------------------
-- Records of dungeon
-- ----------------------------
INSERT INTO `dungeon` VALUES (1, 1, 1, 2, 1, '');

-- ----------------------------
-- Table structure for dungeon_data
-- ----------------------------
DROP TABLE IF EXISTS `dungeon_data`;
CREATE TABLE `dungeon_data`  (
  `dungeon_id` int(10) UNSIGNED NOT NULL DEFAULT 0 COMMENT '副本ID',
  `type` int(10) UNSIGNED NOT NULL DEFAULT 0 COMMENT '类型(validate(dungeon_type))',
  `event` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL DEFAULT '' COMMENT '事件(validate(event))',
  `condition` varchar(255) CHARACTER SET utf8 COLLATE utf8_general_ci NOT NULL DEFAULT '' COMMENT '条件',
  `cost` varchar(255) CHARACTER SET utf8 COLLATE utf8_general_ci NOT NULL DEFAULT '' COMMENT '消耗',
  `day_number` varchar(255) CHARACTER SET utf8 COLLATE utf8_general_ci NOT NULL DEFAULT '' COMMENT '每日次数',
  `buy_number` varchar(255) CHARACTER SET utf8 COLLATE utf8_general_ci NOT NULL DEFAULT '' COMMENT '购买次数',
  `module` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL DEFAULT '' COMMENT '代码模块(validate(module))',
  `function` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL DEFAULT '' COMMENT '入口函数(validate(function))',
  `map_id` int(10) UNSIGNED NOT NULL DEFAULT 0 COMMENT '地图ID',
  `monsters` varchar(255) CHARACTER SET utf8 COLLATE utf8_general_ci NOT NULL DEFAULT '' COMMENT '怪物',
  `boss` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL DEFAULT '' COMMENT 'Boss',
  `time` int(10) UNSIGNED NOT NULL DEFAULT 0 COMMENT '时间',
  `award` varchar(255) CHARACTER SET utf8 COLLATE utf8_general_ci NOT NULL DEFAULT '' COMMENT '奖励',
  `name` char(255) CHARACTER SET utf8 COLLATE utf8_general_ci NOT NULL DEFAULT '' COMMENT '名字',
  `description` char(255) CHARACTER SET utf8 COLLATE utf8_general_ci NOT NULL DEFAULT '' COMMENT '描述',
  PRIMARY KEY (`dungeon_id`) USING BTREE
) ENGINE = InnoDB CHARACTER SET = utf8 COLLATE = utf8_general_ci COMMENT = '副本配置表' ROW_FORMAT = Dynamic;

-- ----------------------------
-- Records of dungeon_data
-- ----------------------------
INSERT INTO `dungeon_data` VALUES (1, 1, 'event_dungeon_passed', '[{level,10}]', '[{100004,100}]', '[{0,1},{1,2},{2,3},{3,4},{4,5},{5,6}]', '[{0,1,100},{1,2,200},{2,3,300},{3,4,400},{4,5,500},{5,6,600}]', 'dungeon_map', 'start', 100001, '[{1,10},{1,20},{1,10},{1,20},{2,1}]', '', 600, '[{100005,100}]', '经验副本', '经验副本');
INSERT INTO `dungeon_data` VALUES (2, 1, 'event_dungeon_passed', '[{level,20}]', '[{100004,200}]', '[{0,1},{1,2},{2,3},{3,4},{4,5},{5,6}]', '[{0,1,100},{1,2,200},{2,3,300},{3,4,400},{4,5,500},{5,6,600}]', 'dungeon_map', 'start', 100002, '[{1,10},{1,20},{1,10},{1,20},{2,1}]', '', 600, '[{100005,200}]', '经验副本', '经验副本');
INSERT INTO `dungeon_data` VALUES (3, 1, 'event_dungeon_passed', '[{level,30}]', '[{100004,300}]', '[{0,1},{1,2},{2,3},{3,4},{4,5},{5,6}]', '[{0,1,100},{1,2,200},{2,3,300},{3,4,400},{4,5,500},{5,6,600}]', 'dungeon_map', 'start', 100003, '[{1,10},{1,20},{1,10},{1,20},{2,1}]', '', 600, '[{100005,300}]', '经验副本', '经验副本');
INSERT INTO `dungeon_data` VALUES (4, 2, 'event_dungeon_passed', '[{level,10}]', '[{100004,100}]', '[{0,1},{1,2},{2,3},{3,4},{4,5},{5,6}]', '[{0,1,100},{1,2,200},{2,3,300},{3,4,400},{4,5,500},{5,6,600}]', 'dungeon_map', 'start', 200001, '[{1,10},{1,20},{1,10},{1,20},{2,1}]', '', 600, '[{100003,100}]', '铜币副本', '铜币副本');
INSERT INTO `dungeon_data` VALUES (5, 2, 'event_dungeon_passed', '[{level,20}]', '[{100004,200}]', '[{0,1},{1,2},{2,3},{3,4},{4,5},{5,6}]', '[{0,1,100},{1,2,200},{2,3,300},{3,4,400},{4,5,500},{5,6,600}]', 'dungeon_map', 'start', 200002, '[{1,10},{1,20},{1,10},{1,20},{2,1}]', '', 600, '[{100003,200}]', '铜币副本', '铜币副本');
INSERT INTO `dungeon_data` VALUES (6, 2, 'event_dungeon_passed', '[{level,30}]', '[{100004,300}]', '[{0,1},{1,2},{2,3},{3,4},{4,5},{5,6}]', '[{0,1,100},{1,2,200},{2,3,300},{3,4,400},{4,5,500},{5,6,600}]', 'dungeon_map', 'start', 200003, '[{1,10},{1,20},{1,10},{1,20},{2,1}]', '', 600, '[{100003,300}]', '铜币副本', '铜币副本');

-- ----------------------------
-- Table structure for effect_data
-- ----------------------------
DROP TABLE IF EXISTS `effect_data`;
CREATE TABLE `effect_data`  (
  `effect_id` int(10) UNSIGNED NOT NULL DEFAULT 0 COMMENT '效果ID',
  `type` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL DEFAULT '' COMMENT '类型(validate(effect_type))',
  `scope` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL DEFAULT '' COMMENT '作用范围(validate(effect_scope))',
  `condition` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL DEFAULT '' COMMENT '条件',
  `ratio` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL DEFAULT '' COMMENT '概率',
  `restrict` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL DEFAULT '' COMMENT '约束',
  `operation` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL DEFAULT '' COMMENT '操作(validate(effect_operation))',
  `object` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL DEFAULT '' COMMENT '作用对象(validate(effect_object))',
  `attribute` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL DEFAULT '' COMMENT '操作属性(validate(effect_attribute))',
  `field` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL DEFAULT '' COMMENT '操作属性字段(validate(effect_field))',
  `value` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL DEFAULT '' COMMENT '属性值',
  `time` int(10) UNSIGNED NOT NULL DEFAULT 0 COMMENT '效果时间',
  `extra` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL DEFAULT '' COMMENT '额外',
  `description` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL DEFAULT '' COMMENT '描述',
  PRIMARY KEY (`effect_id`) USING BTREE
) ENGINE = InnoDB CHARACTER SET = utf8mb4 COLLATE = utf8mb4_general_ci COMMENT = '作用效果配置表' ROW_FORMAT = Dynamic;

-- ----------------------------
-- Records of effect_data
-- ----------------------------
INSERT INTO `effect_data` VALUES (1, 'active', 'battle', '', '10000', '_', 'add', 'self', 'hurt', '', 'Hurt * 1.8', 0, '', '增加80%伤害');
INSERT INTO `effect_data` VALUES (2, 'active', 'battle', '', '10000', '_', 'add', 'self', 'hurt', '', 'Hurt * 1.5', 0, '', '增加50%伤害');
INSERT INTO `effect_data` VALUES (3, 'active', 'battle', 'SelfAttribute.hp == 0', '10000', '_', 'add', 'self', 'attribute', 'hp', 'Self.Attribute.total_hp', 0, '', '死亡立即复活');
INSERT INTO `effect_data` VALUES (4, 'active', 'battle', '', '10000', '_', 'set', 'self', 'attribute', 'vertigo', '0', 0, '', '清除眩晕');
INSERT INTO `effect_data` VALUES (5, 'active', 'battle', '', '10000', '_', 'reduce', 'rival', 'attribute', 'hp', 'Rival.Attribute.total_hp * (50 / 10000)', 5, '', '每秒扣血，总血量万分之50');
INSERT INTO `effect_data` VALUES (6, 'active', 'battle', '', '10000', '_', 'add', 'mate', 'attribute', 'attack', 'Mate.Attribute.attack * 1.5', 3, '', '增加队友攻击150%');
INSERT INTO `effect_data` VALUES (7, 'active', 'battle', '', '10000', '_', 'add', 'mate', 'attribute', 'defense', 'Mate.Attribute.defense * 1.5', 3, '', '增加队友防御150%');
INSERT INTO `effect_data` VALUES (8, 'active', 'battle', '', '10000', '_', 'add', 'self', 'buff', '', '[1]', 0, '', '添加Buff');
INSERT INTO `effect_data` VALUES (9, 'active', 'user', '', '10000', '_', 'add', 'self', 'asset', 'copper', '1.5', 0, '', '增加150%铜币');
INSERT INTO `effect_data` VALUES (10, 'active', 'user', '', '10000', '_', 'add', 'self', 'asset', 'exp', '2', 0, '', '增加200%经验');

-- ----------------------------
-- Table structure for friend
-- ----------------------------
DROP TABLE IF EXISTS `friend`;
CREATE TABLE `friend`  (
  `role_id` bigint(20) UNSIGNED NOT NULL DEFAULT 0 COMMENT '用户ID(select)',
  `friend_id` bigint(20) UNSIGNED NOT NULL DEFAULT 0 COMMENT '好友ID(join(`role`.`role_id`)/join(`vip`.`role_id`))',
  `friend_name` char(0) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL DEFAULT '' COMMENT '好友名字(join(`role`.`role_name`))',
  `sex` varchar(0) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL DEFAULT '' COMMENT '好友性别(join(`role`.`sex`)/default(0))',
  `classes` varchar(0) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL DEFAULT '' COMMENT '好友职业(join(`role`.`classes`)/default(0))',
  `vip_level` varchar(0) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL DEFAULT '' COMMENT 'VIP等级(join(`vip`.`vip_level`)/default(0))',
  `online` varchar(0) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL DEFAULT '' COMMENT '好友在线状态(join(`role`.`online`)/default(0))',
  `relation` tinyint(1) UNSIGNED NOT NULL DEFAULT 0 COMMENT '友好状态(0:申请/1:好友/2:黑名单)',
  `time` int(10) UNSIGNED NOT NULL DEFAULT 0 COMMENT '时间',
  `flag` varchar(0) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL DEFAULT '' COMMENT '标识(flag)',
  PRIMARY KEY (`role_id`, `friend_id`) USING BTREE,
  INDEX `friend_id`(`friend_id`) USING BTREE
) ENGINE = InnoDB CHARACTER SET = utf8mb4 COLLATE = utf8mb4_general_ci COMMENT = '角色好友表' ROW_FORMAT = Dynamic;

-- ----------------------------
-- Records of friend
-- ----------------------------
INSERT INTO `friend` VALUES (1, 2, '', '', '', '', '', 1, 0, '');
INSERT INTO `friend` VALUES (2, 1, '', '', '', '', '', 1, 0, '');

-- ----------------------------
-- Table structure for guild
-- ----------------------------
DROP TABLE IF EXISTS `guild`;
CREATE TABLE `guild`  (
  `guild_id` bigint(20) UNSIGNED NOT NULL AUTO_INCREMENT COMMENT '公会id',
  `exp` bigint(20) UNSIGNED NOT NULL DEFAULT 0 COMMENT '经验',
  `wealth` bigint(20) UNSIGNED NOT NULL DEFAULT 0 COMMENT '财富',
  `level` tinyint(1) UNSIGNED NOT NULL DEFAULT 0 COMMENT '等级',
  `create_time` int(10) UNSIGNED NOT NULL DEFAULT 0 COMMENT '时间(once)',
  `guild_name` char(16) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL DEFAULT '' COMMENT '名字((once)/(update_name))',
  `notice` char(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL DEFAULT '' COMMENT '公告((once)/(update_notice))',
  `leader_id` bigint(20) UNSIGNED NOT NULL DEFAULT 0 COMMENT '会长id(join(`role`.`role_id`)/join(`vip`.`role_id`))',
  `leader_name` char(0) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL DEFAULT '' COMMENT '会长名字(join(`role`.`role_name`))',
  `leader_sex` varchar(0) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL DEFAULT '' COMMENT '性别(join(`role`.`sex`)/default(0))',
  `leader_class` varchar(0) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL DEFAULT '' COMMENT '会长名字(join(`role`.`classes`))',
  `leader_level` varchar(0) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL DEFAULT '' COMMENT '职业(join(`role`.`level`)/default(0))',
  `leader_vip_level` varchar(0) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL DEFAULT '' COMMENT '会长名字(join(`vip`.`vip_level`))',
  `flag` varchar(0) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL DEFAULT '' COMMENT '标识(flag)',
  PRIMARY KEY (`guild_id`) USING BTREE
) ENGINE = InnoDB AUTO_INCREMENT = 6 CHARACTER SET = utf8mb4 COLLATE = utf8mb4_general_ci COMMENT = '公会表' ROW_FORMAT = Dynamic;

-- ----------------------------
-- Records of guild
-- ----------------------------
INSERT INTO `guild` VALUES (1, 1, 0, 0, 0, '1', '', 1, '', '', '', '', '', '');
INSERT INTO `guild` VALUES (2, 2, 0, 0, 0, '2', '', 2, '', '', '', '', '', '');
INSERT INTO `guild` VALUES (3, 3, 0, 0, 0, '3', '', 3, '', '', '', '', '', '');

-- ----------------------------
-- Table structure for guild_apply
-- ----------------------------
DROP TABLE IF EXISTS `guild_apply`;
CREATE TABLE `guild_apply`  (
  `guild_id` bigint(20) UNSIGNED NOT NULL DEFAULT 0 COMMENT '公会ID(join(`guild`.`guild_id`)/(delete_guild_id))',
  `role_id` bigint(20) UNSIGNED NOT NULL DEFAULT 0 COMMENT '角色ID(join(`role`.`role_id`)/join(`vip`.`role_id`)/(delete_role_id))',
  `apply_time` int(10) UNSIGNED NOT NULL DEFAULT 0 COMMENT '时间',
  `guild_name` char(0) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL DEFAULT '' COMMENT '帮派名(join(`guild`.`guild_name`))',
  `role_name` char(0) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL DEFAULT '' COMMENT '角色名(join(`role`.`role_name`))',
  `sex` varchar(0) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL DEFAULT '' COMMENT '性别(join(`role`.`sex`)/default(0))',
  `classes` varchar(0) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL DEFAULT '' COMMENT '职业(join(`role`.`classes`)/default(0))',
  `level` varchar(0) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL DEFAULT '' COMMENT '职业(join(`role`.`level`)/default(0))',
  `vip_level` varchar(0) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL DEFAULT '' COMMENT 'VIP等级(join(`vip`.`vip_level`)/default(0))',
  `flag` varchar(0) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL DEFAULT '' COMMENT '标识(flag)',
  PRIMARY KEY (`guild_id`, `role_id`) USING BTREE,
  INDEX `role_id`(`role_id`) USING BTREE
) ENGINE = InnoDB CHARACTER SET = utf8mb4 COLLATE = utf8mb4_general_ci COMMENT = '公会申请表' ROW_FORMAT = Dynamic;

-- ----------------------------
-- Records of guild_apply
-- ----------------------------
INSERT INTO `guild_apply` VALUES (1, 3, 0, '', '', '', '', '', '', '');
INSERT INTO `guild_apply` VALUES (1, 4, 0, '', '', '', '', '', '', '');
INSERT INTO `guild_apply` VALUES (2, 3, 0, '', '', '', '', '', '', '');
INSERT INTO `guild_apply` VALUES (2, 5, 0, '', '', '', '', '', '', '');

-- ----------------------------
-- Table structure for guild_level_data
-- ----------------------------
DROP TABLE IF EXISTS `guild_level_data`;
CREATE TABLE `guild_level_data`  (
  `level` bigint(20) UNSIGNED NOT NULL DEFAULT 0 COMMENT '等级',
  `exp` bigint(20) UNSIGNED NOT NULL DEFAULT 0 COMMENT '经验'
) ENGINE = InnoDB CHARACTER SET = utf8mb4 COLLATE = utf8mb4_general_ci COMMENT = '等级配置表' ROW_FORMAT = Dynamic;

-- ----------------------------
-- Records of guild_level_data
-- ----------------------------
INSERT INTO `guild_level_data` VALUES (0, 100);
INSERT INTO `guild_level_data` VALUES (1, 200);
INSERT INTO `guild_level_data` VALUES (2, 300);
INSERT INTO `guild_level_data` VALUES (3, 400);
INSERT INTO `guild_level_data` VALUES (4, 500);
INSERT INTO `guild_level_data` VALUES (5, 600);
INSERT INTO `guild_level_data` VALUES (6, 700);
INSERT INTO `guild_level_data` VALUES (7, 800);
INSERT INTO `guild_level_data` VALUES (8, 900);
INSERT INTO `guild_level_data` VALUES (9, 1000);

-- ----------------------------
-- Table structure for guild_role
-- ----------------------------
DROP TABLE IF EXISTS `guild_role`;
CREATE TABLE `guild_role`  (
  `guild_id` int(20) UNSIGNED NOT NULL DEFAULT 0 COMMENT '公会ID(join(`guild`.`guild_id`))',
  `role_id` int(20) UNSIGNED NOT NULL DEFAULT 0 COMMENT '角色ID(join(`role`.`role_id`)/join(`vip`.`role_id`))',
  `job` tinyint(1) UNSIGNED NOT NULL DEFAULT 0 COMMENT '职位',
  `join_time` int(10) UNSIGNED NOT NULL DEFAULT 0 COMMENT '加入时间',
  `leave_time` int(10) UNSIGNED NOT NULL DEFAULT 0 COMMENT '离开时间',
  `guild_name` char(0) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL DEFAULT '' COMMENT '帮派名(join(`guild`.`guild_name`))',
  `role_name` char(0) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL DEFAULT '' COMMENT '角色名(join(`role`.`role_name`))',
  `sex` varchar(0) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL DEFAULT '' COMMENT '性别(join(`role`.`sex`)/default(0))',
  `classes` varchar(0) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL DEFAULT '' COMMENT '职业(join(`role`.`classes`)/default(0))',
  `level` varchar(0) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL DEFAULT '' COMMENT '职业(join(`role`.`level`)/default(0))',
  `vip_level` varchar(0) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL DEFAULT '' COMMENT 'VIP等级(join(`vip`.`vip_level`)/default(0))',
  `flag` varchar(0) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL DEFAULT '' COMMENT '标识(flag)',
  PRIMARY KEY (`role_id`) USING BTREE
) ENGINE = InnoDB CHARACTER SET = utf8mb4 COLLATE = utf8mb4_general_ci COMMENT = '公会角色表' ROW_FORMAT = Dynamic;

-- ----------------------------
-- Records of guild_role
-- ----------------------------
INSERT INTO `guild_role` VALUES (1, 1, 1, 0, 0, '', '', '', '', '', '', '');
INSERT INTO `guild_role` VALUES (2, 2, 1, 0, 0, '', '', '', '', '', '', '');
INSERT INTO `guild_role` VALUES (3, 3, 1, 0, 0, '', '', '', '', '', '', '');

-- ----------------------------
-- Table structure for increment
-- ----------------------------
DROP TABLE IF EXISTS `increment`;
CREATE TABLE `increment`  (
  `name` char(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL DEFAULT '0' COMMENT '名字',
  `value` bigint(20) UNSIGNED NOT NULL DEFAULT 0 COMMENT '数值',
  PRIMARY KEY (`name`) USING BTREE
) ENGINE = InnoDB CHARACTER SET = utf8mb4 COLLATE = utf8mb4_general_ci COMMENT = '自增表' ROW_FORMAT = Dynamic;

-- ----------------------------
-- Records of increment
-- ----------------------------
INSERT INTO `increment` VALUES ('increment_server', 0);
INSERT INTO `increment` VALUES ('map', 0);
INSERT INTO `increment` VALUES ('monster', 10010);

-- ----------------------------
-- Table structure for item
-- ----------------------------
DROP TABLE IF EXISTS `item`;
CREATE TABLE `item`  (
  `unique_id` bigint(20) UNSIGNED NOT NULL AUTO_INCREMENT COMMENT '唯一ID',
  `role_id` bigint(20) UNSIGNED NOT NULL DEFAULT 0 COMMENT '角色ID((select)/(once))',
  `item_id` int(20) UNSIGNED NOT NULL DEFAULT 0 COMMENT '物品ID(once)',
  `type` int(10) UNSIGNED NOT NULL DEFAULT 0 COMMENT '类型',
  `number` int(20) UNSIGNED NOT NULL DEFAULT 1 COMMENT '数量',
  `expire_time` int(10) UNSIGNED NOT NULL DEFAULT 0 COMMENT '过期时间',
  `flag` varchar(0) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL DEFAULT '' COMMENT '标识(flag)',
  PRIMARY KEY (`unique_id`) USING BTREE,
  INDEX `role_id`(`role_id`) USING BTREE
) ENGINE = InnoDB AUTO_INCREMENT = 28 CHARACTER SET = utf8mb4 COLLATE = utf8mb4_general_ci COMMENT = '角色物品表' ROW_FORMAT = Dynamic;

-- ----------------------------
-- Records of item
-- ----------------------------
INSERT INTO `item` VALUES (3, 1, 2, 1, 7, 0, '');
INSERT INTO `item` VALUES (4, 1, 3, 1, 10, 0, '');
INSERT INTO `item` VALUES (5, 1, 4, 2, 1, 0, '');
INSERT INTO `item` VALUES (6, 1, 5, 3, 1, 0, '');

-- ----------------------------
-- Table structure for item_consume_log
-- ----------------------------
DROP TABLE IF EXISTS `item_consume_log`;
CREATE TABLE `item_consume_log`  (
  `id` int(10) UNSIGNED NOT NULL AUTO_INCREMENT COMMENT 'ID',
  `role_id` int(10) UNSIGNED NOT NULL DEFAULT 0 COMMENT '角色ID',
  `item_id` int(10) UNSIGNED NOT NULL DEFAULT 0 COMMENT '物品ID',
  `operation` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL DEFAULT '' COMMENT '操作',
  `source` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL DEFAULT '' COMMENT '来源',
  `time` int(10) UNSIGNED NOT NULL DEFAULT 0 COMMENT '时间',
  PRIMARY KEY (`id`) USING BTREE,
  INDEX `role_id`(`role_id`) USING BTREE,
  INDEX `time`(`time`) USING BTREE
) ENGINE = InnoDB AUTO_INCREMENT = 4 CHARACTER SET = utf8mb4 COLLATE = utf8mb4_general_ci COMMENT = '物品消费日志表' ROW_FORMAT = Compressed;

-- ----------------------------
-- Records of item_consume_log
-- ----------------------------
INSERT INTO `item_consume_log` VALUES (1, 1, 1, 'reduce', 't', 1578536243);
INSERT INTO `item_consume_log` VALUES (2, 1, 1, 'reduce', 't', 1578536272);

-- ----------------------------
-- Table structure for item_data
-- ----------------------------
DROP TABLE IF EXISTS `item_data`;
CREATE TABLE `item_data`  (
  `item_id` int(10) UNSIGNED NOT NULL DEFAULT 0 COMMENT '物品id',
  `type` tinyint(3) UNSIGNED NOT NULL DEFAULT 0 COMMENT '类型(validate(item_type))',
  `overlap` int(10) UNSIGNED NOT NULL DEFAULT 1 COMMENT '叠加数',
  `category` int(10) UNSIGNED NOT NULL DEFAULT 0 COMMENT '分类ID',
  `time` int(10) UNSIGNED NOT NULL DEFAULT 0 COMMENT '有效时间',
  `use_number` int(10) UNSIGNED NOT NULL DEFAULT 0 COMMENT '使用数量(0:不能直接使用/1:一个/N:N个)',
  `use_effect` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL DEFAULT '' COMMENT '使用效果(validate(use_effect))',
  `use_value` int(10) UNSIGNED NOT NULL DEFAULT 0 COMMENT '使用效果数值',
  `name` char(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL DEFAULT '' COMMENT '名字',
  `icon` char(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL DEFAULT '' COMMENT '图标',
  `description` char(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL DEFAULT '' COMMENT '描述',
  PRIMARY KEY (`item_id`) USING BTREE
) ENGINE = InnoDB CHARACTER SET = utf8mb4 COLLATE = utf8mb4_general_ci COMMENT = '物品配置表' ROW_FORMAT = Dynamic;

-- ----------------------------
-- Records of item_data
-- ----------------------------
INSERT INTO `item_data` VALUES (1, 1, 1000, 0, 0, 0, '', 0, 'rust', 'file_type_rust.svg', '');
INSERT INTO `item_data` VALUES (2, 1, 100, 0, 0, 0, '', 0, 'erlang', 'file_type_erlang.svg', '');
INSERT INTO `item_data` VALUES (3, 1, 10, 0, 0, 0, '', 0, 'php', 'file_type_php.svg', '');
INSERT INTO `item_data` VALUES (4, 2, 1, 0, 0, 0, '', 0, 'lua', 'file_type_lua.svg', '');
INSERT INTO `item_data` VALUES (5, 2, 1, 0, 0, 0, '', 0, 'js', 'file_type_js.svg', '');
INSERT INTO `item_data` VALUES (6, 2, 1, 0, 0, 0, '', 0, 'html', 'file_type_html.svg', '');
INSERT INTO `item_data` VALUES (7, 2, 1, 0, 0, 0, '', 0, 'css', 'file_type_css.svg', '');
INSERT INTO `item_data` VALUES (100001, 10, 1, 0, 0, 0, 'gold', 0, 'gold', 'file_type_gold.svg', '');
INSERT INTO `item_data` VALUES (100002, 10, 1, 0, 0, 0, 'sliver', 0, 'silver', 'file_type_sliver.svg', '');
INSERT INTO `item_data` VALUES (100003, 10, 1, 0, 0, 0, 'copper', 0, 'copper', 'file_type_copper.svg', '');
INSERT INTO `item_data` VALUES (100004, 10, 1, 0, 0, 0, 'exp', 0, 'exp', 'file_type_exp.svg', '');
INSERT INTO `item_data` VALUES (100005, 10, 1, 0, 0, 0, 'coin', 0, 'coin', 'file_type_coin.svg', '');

-- ----------------------------
-- Table structure for item_produce_log
-- ----------------------------
DROP TABLE IF EXISTS `item_produce_log`;
CREATE TABLE `item_produce_log`  (
  `id` int(10) UNSIGNED NOT NULL AUTO_INCREMENT COMMENT 'ID',
  `role_id` int(10) UNSIGNED NOT NULL DEFAULT 0 COMMENT '角色ID',
  `item_id` int(10) UNSIGNED NOT NULL DEFAULT 0 COMMENT '物品ID',
  `operation` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL DEFAULT '' COMMENT '操作',
  `source` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL DEFAULT '' COMMENT '来源',
  `time` int(10) UNSIGNED NOT NULL DEFAULT 0 COMMENT '时间',
  PRIMARY KEY (`id`) USING BTREE,
  INDEX `role_id`(`role_id`) USING BTREE,
  INDEX `time`(`time`) USING BTREE
) ENGINE = InnoDB AUTO_INCREMENT = 1 CHARACTER SET = utf8mb4 COLLATE = utf8mb4_general_ci COMMENT = '物品产出日志表' ROW_FORMAT = Compressed;

-- ----------------------------
-- Records of item_produce_log
-- ----------------------------

-- ----------------------------
-- Table structure for key
-- ----------------------------
DROP TABLE IF EXISTS `key`;
CREATE TABLE `key`  (
  `role_id` bigint(20) UNSIGNED NOT NULL DEFAULT 0 COMMENT '角色ID',
  `key` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL DEFAULT '' COMMENT '码',
  PRIMARY KEY (`role_id`, `key`) USING BTREE,
  INDEX `key`(`key`) USING BTREE
) ENGINE = InnoDB CHARACTER SET = utf8mb4 COLLATE = utf8mb4_general_ci COMMENT = '角色兑换码表' ROW_FORMAT = Dynamic;

-- ----------------------------
-- Records of key
-- ----------------------------

-- ----------------------------
-- Table structure for key_award_data
-- ----------------------------
DROP TABLE IF EXISTS `key_award_data`;
CREATE TABLE `key_award_data`  (
  `type` int(10) UNSIGNED NOT NULL DEFAULT 0 COMMENT '类型',
  `only` tinyint(1) UNSIGNED NOT NULL DEFAULT 0 COMMENT '唯一',
  `award` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL DEFAULT '' COMMENT '奖励',
  PRIMARY KEY (`type`) USING BTREE
) ENGINE = InnoDB CHARACTER SET = utf8mb4 COLLATE = utf8mb4_general_ci COMMENT = '兑换码奖励配置表' ROW_FORMAT = Dynamic;

-- ----------------------------
-- Records of key_award_data
-- ----------------------------
INSERT INTO `key_award_data` VALUES (1, 0, '[{700001,1},{700002,2},{700003,3}]');
INSERT INTO `key_award_data` VALUES (2, 0, '[{700001,1},{700002,2},{700003,3}]');

-- ----------------------------
-- Table structure for key_data
-- ----------------------------
DROP TABLE IF EXISTS `key_data`;
CREATE TABLE `key_data`  (
  `key` char(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL DEFAULT '' COMMENT '码',
  `type` tinyint(1) UNSIGNED NOT NULL DEFAULT 0 COMMENT '类型',
  PRIMARY KEY (`key`) USING BTREE,
  INDEX `key`(`key`) USING BTREE
) ENGINE = InnoDB CHARACTER SET = utf8mb4 COLLATE = utf8mb4_general_ci COMMENT = '兑换码配置表' ROW_FORMAT = Dynamic;

-- ----------------------------
-- Records of key_data
-- ----------------------------

-- ----------------------------
-- Table structure for level_data
-- ----------------------------
DROP TABLE IF EXISTS `level_data`;
CREATE TABLE `level_data`  (
  `level` bigint(20) UNSIGNED NOT NULL DEFAULT 0 COMMENT '等级',
  `exp` bigint(20) UNSIGNED NOT NULL DEFAULT 0 COMMENT '经验'
) ENGINE = InnoDB CHARACTER SET = utf8mb4 COLLATE = utf8mb4_general_ci COMMENT = '等级配置表' ROW_FORMAT = Dynamic;

-- ----------------------------
-- Records of level_data
-- ----------------------------
INSERT INTO `level_data` VALUES (0, 100);
INSERT INTO `level_data` VALUES (1, 200);
INSERT INTO `level_data` VALUES (2, 300);
INSERT INTO `level_data` VALUES (3, 400);
INSERT INTO `level_data` VALUES (4, 500);
INSERT INTO `level_data` VALUES (5, 600);
INSERT INTO `level_data` VALUES (6, 700);
INSERT INTO `level_data` VALUES (7, 800);
INSERT INTO `level_data` VALUES (8, 900);
INSERT INTO `level_data` VALUES (9, 1000);

-- ----------------------------
-- Table structure for login_log
-- ----------------------------
DROP TABLE IF EXISTS `login_log`;
CREATE TABLE `login_log`  (
  `id` bigint(20) UNSIGNED NOT NULL AUTO_INCREMENT COMMENT 'ID',
  `role_id` bigint(20) UNSIGNED NOT NULL DEFAULT 0 COMMENT '角色ID',
  `ip` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL DEFAULT '' COMMENT '登录IP',
  `device_id` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL DEFAULT '' COMMENT '设备ID',
  `login_time` int(10) UNSIGNED NOT NULL DEFAULT 0 COMMENT '登录时间',
  `online_time` int(10) UNSIGNED NOT NULL DEFAULT 0 COMMENT '在线时间',
  `time` int(10) UNSIGNED NOT NULL DEFAULT 0 COMMENT '登出时间',
  PRIMARY KEY (`id`) USING BTREE,
  INDEX `role_id`(`role_id`) USING BTREE,
  INDEX `time`(`time`) USING BTREE
) ENGINE = InnoDB AUTO_INCREMENT = 1 CHARACTER SET = utf8mb4 COLLATE = utf8mb4_general_ci COMMENT = '登录日志' ROW_FORMAT = Compressed;

-- ----------------------------
-- Records of login_log
-- ----------------------------

-- ----------------------------
-- Table structure for mail
-- ----------------------------
DROP TABLE IF EXISTS `mail`;
CREATE TABLE `mail`  (
  `mail_id` bigint(20) UNSIGNED NOT NULL AUTO_INCREMENT COMMENT '邮件ID',
  `sender_id` bigint(20) UNSIGNED NOT NULL DEFAULT 0 COMMENT '发送者',
  `sender_nick` char(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL DEFAULT '' COMMENT '发送者昵称',
  `receiver_id` bigint(20) UNSIGNED NOT NULL DEFAULT 0 COMMENT '接收者(select)',
  `receiver_nick` char(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL DEFAULT '' COMMENT '接受者昵称',
  `receive_time` int(10) UNSIGNED NOT NULL DEFAULT 0 COMMENT '接收时间',
  `is_read` tinyint(1) UNSIGNED NOT NULL DEFAULT 0 COMMENT '是否已经读取(update_read)',
  `read_time` int(10) UNSIGNED NOT NULL DEFAULT 0 COMMENT '读取时间(update_read)',
  `expire_time` int(10) UNSIGNED NOT NULL DEFAULT 0 COMMENT '过期时间',
  `is_receive_attachment` tinyint(1) UNSIGNED NOT NULL DEFAULT 0 COMMENT '是否领取附件(update_receive)',
  `receive_attachment_time` int(10) UNSIGNED NOT NULL DEFAULT 0 COMMENT '领取附件时间(update_receive)',
  `from` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL DEFAULT '' COMMENT '来源',
  `title` char(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL DEFAULT '' COMMENT '标题',
  `content` char(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL DEFAULT '' COMMENT '内容',
  `attachment` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL DEFAULT '' COMMENT '附件',
  `flag` varchar(0) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL DEFAULT '' COMMENT '标识(flag)',
  PRIMARY KEY (`mail_id`) USING BTREE,
  INDEX `receiver_id`(`receiver_id`) USING BTREE
) ENGINE = InnoDB AUTO_INCREMENT = 2 CHARACTER SET = utf8mb4 COLLATE = utf8mb4_general_ci COMMENT = '角色邮件表' ROW_FORMAT = Dynamic;

-- ----------------------------
-- Records of mail
-- ----------------------------
INSERT INTO `mail` VALUES (1, 0, '', 1, '1', 0, 0, 0, 0, 0, 0, '', '标题', '内容', '[{1,1},{2,2},{3,3}]', '');

-- ----------------------------
-- Table structure for map_data
-- ----------------------------
DROP TABLE IF EXISTS `map_data`;
CREATE TABLE `map_data`  (
  `map_id` int(10) UNSIGNED NOT NULL DEFAULT 0 COMMENT '地图ID',
  `type` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL DEFAULT '' COMMENT '广播类型(validate(map_type))',
  `reconnect` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL DEFAULT '' COMMENT '是否重连(validate(boolean))',
  `monsters` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL DEFAULT '' COMMENT '随地图启动的怪物',
  `rank_key` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL DEFAULT '' COMMENT '榜键类型(validate(map_rank_key))',
  `rank_value` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL DEFAULT '' COMMENT '榜值类型(validate(map_rank_value))',
  `rank_mode` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL DEFAULT '' COMMENT '榜模式(validate(map_rank_mode))',
  `enter_points` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL DEFAULT '' COMMENT '进入点',
  `pk_mode` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL DEFAULT '' COMMENT 'PK模式',
  `enter_script` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL DEFAULT '' COMMENT '进入脚本',
  `relive_script` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL DEFAULT '' COMMENT '复活脚本',
  `leave_script` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL DEFAULT '' COMMENT '离开脚本',
  PRIMARY KEY (`map_id`) USING BTREE
) ENGINE = InnoDB CHARACTER SET = utf8mb4 COLLATE = utf8mb4_general_ci COMMENT = '地图配置表' ROW_FORMAT = Dynamic;

-- ----------------------------
-- Records of map_data
-- ----------------------------
INSERT INTO `map_data` VALUES (100000, 'slice', 'false', '', '', '', '', '[{10,10},{20,10},{30,10},{40,10},{50,10},{60,10},{70,10},{10,10},{90,10},{100,10}]', '', '', '', '');
INSERT INTO `map_data` VALUES (100001, 'full', 'false', '', '', '', '', '[{10,10},{20,10},{30,10},{40,10},{50,10},{60,10},{70,10},{10,10},{90,10},{100,10}]', '', '', '', '');
INSERT INTO `map_data` VALUES (100002, 'full', 'false', '', '', '', '', '[{10,10},{20,10},{30,10},{40,10},{50,10},{60,10},{70,10},{10,10},{90,10},{100,10}]', '', '', '', '');
INSERT INTO `map_data` VALUES (100003, 'full', 'false', '', '', '', '', '[{10,10},{20,10},{30,10},{40,10},{50,10},{60,10},{70,10},{10,10},{90,10},{100,10}]', '', '', '', '');
INSERT INTO `map_data` VALUES (200001, 'full', 'false', '', '', '', '', '[{10,10},{20,10},{30,10},{40,10},{50,10},{60,10},{70,10},{10,10},{90,10},{100,10}]', '', '', '', '');
INSERT INTO `map_data` VALUES (200002, 'full', 'false', '', '', '', '', '[{10,10},{20,10},{30,10},{40,10},{50,10},{60,10},{70,10},{10,10},{90,10},{100,10}]', '', '', '', '');
INSERT INTO `map_data` VALUES (200003, 'full', 'false', '', '', '', '', '[{10,10},{20,10},{30,10},{40,10},{50,10},{60,10},{70,10},{10,10},{90,10},{100,10}]', '', '', '', '');

-- ----------------------------
-- Table structure for monster_data
-- ----------------------------
DROP TABLE IF EXISTS `monster_data`;
CREATE TABLE `monster_data`  (
  `monster_id` int(10) UNSIGNED NOT NULL DEFAULT 0 COMMENT '怪物ID',
  `type` int(10) UNSIGNED NOT NULL DEFAULT 0 COMMENT '组ID',
  `name` char(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL DEFAULT '' COMMENT '怪物名称',
  `description` char(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL DEFAULT '' COMMENT '怪物描述',
  `level` int(10) UNSIGNED NOT NULL DEFAULT 0 COMMENT '等级',
  `hp` bigint(20) UNSIGNED NOT NULL DEFAULT 0 COMMENT '血量',
  `map_id` int(10) UNSIGNED NOT NULL DEFAULT 0 COMMENT '地图ID',
  `camp` tinyint(1) UNSIGNED NOT NULL DEFAULT 0 COMMENT '阵营',
  `range` int(10) UNSIGNED NOT NULL DEFAULT 0 COMMENT '攻击距离',
  `distance` int(10) UNSIGNED NOT NULL DEFAULT 0 COMMENT '搜索距离',
  `relive_time` int(10) UNSIGNED NOT NULL DEFAULT 0 COMMENT '复活时间',
  `act_type` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL DEFAULT '' COMMENT '动作类型(validate(act_type))',
  `act_script` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL DEFAULT '' COMMENT '动作脚本(enemy:敌人/role:玩家/monster:怪物/{monster,组ID}:特定怪物)',
  `skills` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL DEFAULT '' COMMENT '技能',
  `born_points` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL DEFAULT '' COMMENT '出生点',
  `award` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL DEFAULT '' COMMENT '奖励',
  PRIMARY KEY (`monster_id`) USING BTREE
) ENGINE = InnoDB CHARACTER SET = utf8mb4 COLLATE = utf8mb4_general_ci COMMENT = '怪物配置表' ROW_FORMAT = Dynamic;

-- ----------------------------
-- Records of monster_data
-- ----------------------------
INSERT INTO `monster_data` VALUES (1, 1, 'active', 'active', 1, 100, 100001, 1, 1, 300, 0, 'active', '[role]', '[5]', '[{10,10},{20,10},{30,10},{40,10},{50,10},{60,10},{70,10},{10,10},{90,10},{100,10}]', '[{100005,100}]');
INSERT INTO `monster_data` VALUES (2, 2, 'passive', 'passive', 1, 200, 100001, 1, 2, 300, 0, 'passive', '[enemy]', '', '[{40,10}]', '[{100005,200}]');
INSERT INTO `monster_data` VALUES (3, 3, 'movable', 'movable', 1, 300, 0, 1, 3, 300, 0, 'movable', '', '', '[{60,10}]', '[{100005,300}]');
INSERT INTO `monster_data` VALUES (4, 4, 'fix', 'fix', 1, 400, 0, 1, 4, 300, 0, 'fix', '', '', '[{80,10}]', '');
INSERT INTO `monster_data` VALUES (5, 5, 'act', 'act', 1, 500, 0, 1, 5, 300, 0, 'fix', '[enemy]', '', '[{100,10}]', '');
INSERT INTO `monster_data` VALUES (6, 6, 'boom', 'boom', 1, 600, 0, 1, 6, 300, 0, 'active', '[{monster, 20}, {monster, 50}, role]', '', '[{120,10}]', '[{100005,600}]');

-- ----------------------------
-- Table structure for node_data
-- ----------------------------
DROP TABLE IF EXISTS `node_data`;
CREATE TABLE `node_data`  (
  `server_node` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL DEFAULT '' COMMENT '游戏服节点',
  `server_name` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL DEFAULT '' COMMENT '游戏服名',
  `server_host` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL DEFAULT '' COMMENT '游戏服域名',
  `server_ip` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL DEFAULT '' COMMENT '游戏服IP',
  `server_port` int(10) UNSIGNED NOT NULL DEFAULT 0 COMMENT '游戏服端口',
  `server_id` int(10) UNSIGNED NOT NULL DEFAULT 0 COMMENT '游戏服编号',
  `server_type` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL DEFAULT '' COMMENT '服务器类型',
  `center_node` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL DEFAULT '' COMMENT '中央服节点',
  `center_name` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL DEFAULT '' COMMENT '中央服名',
  `center_host` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL DEFAULT '' COMMENT '中央服域名',
  `center_ip` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL DEFAULT '' COMMENT '中央服IP',
  `center_port` int(10) UNSIGNED NOT NULL DEFAULT 0 COMMENT '中央服端口',
  `center_id` int(10) UNSIGNED NOT NULL DEFAULT 0 COMMENT '中央服编号',
  PRIMARY KEY (`server_node`) USING BTREE
) ENGINE = InnoDB CHARACTER SET = utf8mb4 COLLATE = utf8mb4_general_ci COMMENT = '节点配置表' ROW_FORMAT = Dynamic;

-- ----------------------------
-- Records of node_data
-- ----------------------------
INSERT INTO `node_data` VALUES ('center', '小跨服', '', '', 0, 1001, 'center', '', '', '', '', 0, 0);
INSERT INTO `node_data` VALUES ('dev', '开发服', '', '', 10004, 4, 'local', '', '小跨服', '', '', 0, 0);
INSERT INTO `node_data` VALUES ('main', '主测服', '', '', 10001, 1, 'local', 'center', '小跨服', '', '', 0, 0);
INSERT INTO `node_data` VALUES ('publish', '版署服', '', '', 10005, 5, 'local', 'center', '小跨服', '', '', 0, 0);
INSERT INTO `node_data` VALUES ('stable', '稳定服', '', '', 10002, 2, 'local', 'center', '小跨服', '', '', 0, 0);
INSERT INTO `node_data` VALUES ('test', '测试服', '', '', 10003, 3, 'local', 'center', '小跨服', '', '', 0, 0);
INSERT INTO `node_data` VALUES ('world', '大世界', '', '', 0, 0, 'world', '', '', '', '', 0, 0);

-- ----------------------------
-- Table structure for online_log
-- ----------------------------
DROP TABLE IF EXISTS `online_log`;
CREATE TABLE `online_log`  (
  `id` bigint(20) UNSIGNED NOT NULL AUTO_INCREMENT COMMENT 'ID',
  `all` int(10) UNSIGNED NOT NULL DEFAULT 0 COMMENT '全部',
  `online` int(10) UNSIGNED NOT NULL DEFAULT 0 COMMENT '在线',
  `hosting` int(10) UNSIGNED NOT NULL DEFAULT 0 COMMENT '挂机',
  `hour` int(10) UNSIGNED NOT NULL DEFAULT 0 COMMENT '当前小时',
  `time` int(10) UNSIGNED NOT NULL DEFAULT 0 COMMENT '当前时间',
  PRIMARY KEY (`id`) USING BTREE,
  INDEX `time`(`time`) USING BTREE
) ENGINE = InnoDB AUTO_INCREMENT = 611 CHARACTER SET = utf8mb4 COLLATE = utf8mb4_general_ci COMMENT = '在线统计日志' ROW_FORMAT = Compressed;

-- ----------------------------
-- Records of online_log
-- ----------------------------
INSERT INTO `online_log` VALUES (1, 0, 0, 0, 15, 1578900361);
INSERT INTO `online_log` VALUES (2, 0, 0, 0, 19, 1578916134);
INSERT INTO `online_log` VALUES (3, 0, 0, 0, 19, 1578916195);
INSERT INTO `online_log` VALUES (4, 0, 0, 0, 19, 1578916255);
INSERT INTO `online_log` VALUES (5, 0, 0, 0, 19, 1578916315);
INSERT INTO `online_log` VALUES (6, 0, 0, 0, 19, 1578916375);
INSERT INTO `online_log` VALUES (7, 0, 0, 0, 10, 1579141678);
INSERT INTO `online_log` VALUES (8, 0, 0, 0, 10, 1579141738);
INSERT INTO `online_log` VALUES (9, 0, 0, 0, 10, 1579141798);
INSERT INTO `online_log` VALUES (10, 0, 0, 0, 10, 1579141858);
INSERT INTO `online_log` VALUES (11, 0, 0, 0, 10, 1579141918);
INSERT INTO `online_log` VALUES (12, 0, 0, 0, 10, 1579141978);
INSERT INTO `online_log` VALUES (13, 0, 0, 0, 10, 1579142038);
INSERT INTO `online_log` VALUES (14, 0, 0, 0, 10, 1579142098);
INSERT INTO `online_log` VALUES (15, 0, 0, 0, 10, 1579142158);
INSERT INTO `online_log` VALUES (16, 0, 0, 0, 10, 1579142218);
INSERT INTO `online_log` VALUES (17, 0, 0, 0, 10, 1579142278);
INSERT INTO `online_log` VALUES (18, 0, 0, 0, 10, 1579142338);
INSERT INTO `online_log` VALUES (19, 0, 0, 0, 10, 1579142399);
INSERT INTO `online_log` VALUES (20, 0, 0, 0, 10, 1579142459);
INSERT INTO `online_log` VALUES (21, 0, 0, 0, 10, 1579142519);
INSERT INTO `online_log` VALUES (22, 0, 0, 0, 10, 1579142579);
INSERT INTO `online_log` VALUES (23, 0, 0, 0, 10, 1579142639);
INSERT INTO `online_log` VALUES (24, 0, 0, 0, 10, 1579142699);
INSERT INTO `online_log` VALUES (25, 0, 0, 0, 10, 1579142759);
INSERT INTO `online_log` VALUES (26, 0, 0, 0, 10, 1579142819);
INSERT INTO `online_log` VALUES (27, 0, 0, 0, 10, 1579142879);
INSERT INTO `online_log` VALUES (28, 0, 0, 0, 10, 1579142939);
INSERT INTO `online_log` VALUES (29, 0, 0, 0, 10, 1579142999);
INSERT INTO `online_log` VALUES (30, 0, 0, 0, 10, 1579143059);
INSERT INTO `online_log` VALUES (31, 0, 0, 0, 10, 1579143119);
INSERT INTO `online_log` VALUES (32, 0, 0, 0, 10, 1579143179);
INSERT INTO `online_log` VALUES (33, 0, 0, 0, 10, 1579143239);
INSERT INTO `online_log` VALUES (34, 0, 0, 0, 11, 1579144414);
INSERT INTO `online_log` VALUES (35, 0, 0, 0, 11, 1579144474);
INSERT INTO `online_log` VALUES (36, 0, 0, 0, 11, 1579144534);
INSERT INTO `online_log` VALUES (37, 0, 0, 0, 11, 1579144594);
INSERT INTO `online_log` VALUES (38, 0, 0, 0, 11, 1579144654);
INSERT INTO `online_log` VALUES (39, 0, 0, 0, 11, 1579144714);
INSERT INTO `online_log` VALUES (40, 0, 0, 0, 11, 1579144774);
INSERT INTO `online_log` VALUES (41, 0, 0, 0, 11, 1579144834);
INSERT INTO `online_log` VALUES (42, 0, 0, 0, 11, 1579144894);
INSERT INTO `online_log` VALUES (43, 0, 0, 0, 11, 1579144954);
INSERT INTO `online_log` VALUES (44, 0, 0, 0, 11, 1579145014);
INSERT INTO `online_log` VALUES (45, 0, 0, 0, 11, 1579145074);
INSERT INTO `online_log` VALUES (46, 0, 0, 0, 11, 1579145134);
INSERT INTO `online_log` VALUES (47, 0, 0, 0, 11, 1579145194);
INSERT INTO `online_log` VALUES (48, 0, 0, 0, 11, 1579145254);
INSERT INTO `online_log` VALUES (49, 0, 0, 0, 11, 1579145314);
INSERT INTO `online_log` VALUES (50, 0, 0, 0, 11, 1579145374);
INSERT INTO `online_log` VALUES (51, 0, 0, 0, 11, 1579145434);
INSERT INTO `online_log` VALUES (52, 0, 0, 0, 11, 1579145494);
INSERT INTO `online_log` VALUES (53, 0, 0, 0, 11, 1579145554);
INSERT INTO `online_log` VALUES (54, 0, 0, 0, 11, 1579145614);
INSERT INTO `online_log` VALUES (55, 0, 0, 0, 11, 1579145674);
INSERT INTO `online_log` VALUES (56, 0, 0, 0, 11, 1579145734);
INSERT INTO `online_log` VALUES (57, 0, 0, 0, 11, 1579145794);
INSERT INTO `online_log` VALUES (58, 0, 0, 0, 11, 1579145854);
INSERT INTO `online_log` VALUES (59, 0, 0, 0, 11, 1579145914);
INSERT INTO `online_log` VALUES (60, 0, 0, 0, 11, 1579145974);
INSERT INTO `online_log` VALUES (61, 0, 0, 0, 11, 1579146034);
INSERT INTO `online_log` VALUES (62, 0, 0, 0, 11, 1579146094);
INSERT INTO `online_log` VALUES (63, 0, 0, 0, 11, 1579146154);
INSERT INTO `online_log` VALUES (64, 0, 0, 0, 11, 1579146214);
INSERT INTO `online_log` VALUES (65, 0, 0, 0, 11, 1579146274);
INSERT INTO `online_log` VALUES (66, 0, 0, 0, 11, 1579146334);
INSERT INTO `online_log` VALUES (67, 0, 0, 0, 11, 1579146394);
INSERT INTO `online_log` VALUES (68, 0, 0, 0, 11, 1579146454);
INSERT INTO `online_log` VALUES (69, 0, 0, 0, 11, 1579146514);
INSERT INTO `online_log` VALUES (70, 0, 0, 0, 11, 1579146574);
INSERT INTO `online_log` VALUES (71, 0, 0, 0, 11, 1579146634);
INSERT INTO `online_log` VALUES (72, 0, 0, 0, 11, 1579146694);
INSERT INTO `online_log` VALUES (73, 0, 0, 0, 11, 1579146754);
INSERT INTO `online_log` VALUES (74, 0, 0, 0, 11, 1579146814);
INSERT INTO `online_log` VALUES (75, 0, 0, 0, 11, 1579146874);
INSERT INTO `online_log` VALUES (76, 0, 0, 0, 11, 1579146934);
INSERT INTO `online_log` VALUES (77, 0, 0, 0, 11, 1579146994);
INSERT INTO `online_log` VALUES (78, 0, 0, 0, 11, 1579147054);
INSERT INTO `online_log` VALUES (79, 0, 0, 0, 11, 1579147114);
INSERT INTO `online_log` VALUES (80, 0, 0, 0, 11, 1579147174);
INSERT INTO `online_log` VALUES (81, 0, 0, 0, 12, 1579147234);
INSERT INTO `online_log` VALUES (82, 0, 0, 0, 12, 1579147294);
INSERT INTO `online_log` VALUES (83, 0, 0, 0, 12, 1579147354);
INSERT INTO `online_log` VALUES (84, 0, 0, 0, 12, 1579147414);
INSERT INTO `online_log` VALUES (85, 0, 0, 0, 12, 1579147474);
INSERT INTO `online_log` VALUES (86, 0, 0, 0, 12, 1579147534);
INSERT INTO `online_log` VALUES (87, 0, 0, 0, 12, 1579147594);
INSERT INTO `online_log` VALUES (88, 0, 0, 0, 12, 1579147654);
INSERT INTO `online_log` VALUES (89, 0, 0, 0, 12, 1579147714);
INSERT INTO `online_log` VALUES (90, 0, 0, 0, 12, 1579147774);
INSERT INTO `online_log` VALUES (91, 0, 0, 0, 12, 1579147834);
INSERT INTO `online_log` VALUES (92, 0, 0, 0, 12, 1579147894);
INSERT INTO `online_log` VALUES (93, 0, 0, 0, 12, 1579147954);
INSERT INTO `online_log` VALUES (94, 0, 0, 0, 12, 1579148014);
INSERT INTO `online_log` VALUES (95, 0, 0, 0, 12, 1579148074);
INSERT INTO `online_log` VALUES (96, 0, 0, 0, 12, 1579148134);
INSERT INTO `online_log` VALUES (97, 0, 0, 0, 12, 1579148194);
INSERT INTO `online_log` VALUES (98, 0, 0, 0, 12, 1579148254);
INSERT INTO `online_log` VALUES (99, 0, 0, 0, 12, 1579148315);
INSERT INTO `online_log` VALUES (100, 0, 0, 0, 12, 1579148375);
INSERT INTO `online_log` VALUES (101, 0, 0, 0, 12, 1579148435);
INSERT INTO `online_log` VALUES (102, 0, 0, 0, 12, 1579148495);
INSERT INTO `online_log` VALUES (103, 0, 0, 0, 12, 1579148555);
INSERT INTO `online_log` VALUES (104, 0, 0, 0, 12, 1579148615);
INSERT INTO `online_log` VALUES (105, 0, 0, 0, 12, 1579148675);
INSERT INTO `online_log` VALUES (106, 0, 0, 0, 12, 1579148735);
INSERT INTO `online_log` VALUES (107, 0, 0, 0, 12, 1579148795);
INSERT INTO `online_log` VALUES (108, 0, 0, 0, 12, 1579148855);
INSERT INTO `online_log` VALUES (109, 0, 0, 0, 12, 1579148915);
INSERT INTO `online_log` VALUES (110, 0, 0, 0, 12, 1579148975);
INSERT INTO `online_log` VALUES (111, 0, 0, 0, 12, 1579149035);
INSERT INTO `online_log` VALUES (112, 0, 0, 0, 12, 1579149095);
INSERT INTO `online_log` VALUES (113, 0, 0, 0, 12, 1579149155);
INSERT INTO `online_log` VALUES (114, 0, 0, 0, 12, 1579149215);
INSERT INTO `online_log` VALUES (115, 0, 0, 0, 12, 1579149275);
INSERT INTO `online_log` VALUES (116, 0, 0, 0, 12, 1579149335);
INSERT INTO `online_log` VALUES (117, 0, 0, 0, 12, 1579149395);
INSERT INTO `online_log` VALUES (118, 0, 0, 0, 12, 1579149455);
INSERT INTO `online_log` VALUES (119, 0, 0, 0, 12, 1579149515);
INSERT INTO `online_log` VALUES (120, 0, 0, 0, 12, 1579149575);
INSERT INTO `online_log` VALUES (121, 0, 0, 0, 12, 1579149635);
INSERT INTO `online_log` VALUES (122, 0, 0, 0, 12, 1579149695);
INSERT INTO `online_log` VALUES (123, 0, 0, 0, 12, 1579149755);
INSERT INTO `online_log` VALUES (124, 0, 0, 0, 12, 1579149815);
INSERT INTO `online_log` VALUES (125, 0, 0, 0, 12, 1579149875);
INSERT INTO `online_log` VALUES (126, 0, 0, 0, 12, 1579149935);
INSERT INTO `online_log` VALUES (127, 0, 0, 0, 12, 1579149995);
INSERT INTO `online_log` VALUES (128, 0, 0, 0, 12, 1579150055);
INSERT INTO `online_log` VALUES (129, 0, 0, 0, 12, 1579150115);
INSERT INTO `online_log` VALUES (130, 0, 0, 0, 12, 1579150175);
INSERT INTO `online_log` VALUES (131, 0, 0, 0, 12, 1579150235);
INSERT INTO `online_log` VALUES (132, 0, 0, 0, 12, 1579150295);
INSERT INTO `online_log` VALUES (133, 0, 0, 0, 12, 1579150355);
INSERT INTO `online_log` VALUES (134, 0, 0, 0, 12, 1579150415);
INSERT INTO `online_log` VALUES (135, 0, 0, 0, 12, 1579150475);
INSERT INTO `online_log` VALUES (136, 0, 0, 0, 12, 1579150535);
INSERT INTO `online_log` VALUES (137, 0, 0, 0, 12, 1579150595);
INSERT INTO `online_log` VALUES (138, 0, 0, 0, 12, 1579150655);
INSERT INTO `online_log` VALUES (139, 0, 0, 0, 12, 1579150715);
INSERT INTO `online_log` VALUES (140, 0, 0, 0, 12, 1579150775);
INSERT INTO `online_log` VALUES (141, 0, 0, 0, 13, 1579150835);
INSERT INTO `online_log` VALUES (142, 0, 0, 0, 13, 1579150895);
INSERT INTO `online_log` VALUES (143, 0, 0, 0, 13, 1579150955);
INSERT INTO `online_log` VALUES (144, 0, 0, 0, 13, 1579151015);
INSERT INTO `online_log` VALUES (145, 0, 0, 0, 13, 1579151075);
INSERT INTO `online_log` VALUES (146, 0, 0, 0, 13, 1579151135);
INSERT INTO `online_log` VALUES (147, 0, 0, 0, 13, 1579151195);
INSERT INTO `online_log` VALUES (148, 0, 0, 0, 13, 1579151255);
INSERT INTO `online_log` VALUES (149, 0, 0, 0, 13, 1579151315);
INSERT INTO `online_log` VALUES (150, 0, 0, 0, 13, 1579151375);
INSERT INTO `online_log` VALUES (151, 0, 0, 0, 13, 1579151435);
INSERT INTO `online_log` VALUES (152, 0, 0, 0, 13, 1579151495);
INSERT INTO `online_log` VALUES (153, 0, 0, 0, 13, 1579151555);
INSERT INTO `online_log` VALUES (154, 0, 0, 0, 13, 1579151615);
INSERT INTO `online_log` VALUES (155, 0, 0, 0, 13, 1579151675);
INSERT INTO `online_log` VALUES (156, 0, 0, 0, 13, 1579151735);
INSERT INTO `online_log` VALUES (157, 0, 0, 0, 13, 1579151795);
INSERT INTO `online_log` VALUES (158, 0, 0, 0, 13, 1579151855);
INSERT INTO `online_log` VALUES (159, 0, 0, 0, 13, 1579151915);
INSERT INTO `online_log` VALUES (160, 0, 0, 0, 13, 1579151975);
INSERT INTO `online_log` VALUES (161, 0, 0, 0, 13, 1579152035);
INSERT INTO `online_log` VALUES (162, 0, 0, 0, 13, 1579152095);
INSERT INTO `online_log` VALUES (163, 0, 0, 0, 13, 1579152155);
INSERT INTO `online_log` VALUES (164, 0, 0, 0, 13, 1579152215);
INSERT INTO `online_log` VALUES (165, 0, 0, 0, 13, 1579152275);
INSERT INTO `online_log` VALUES (166, 0, 0, 0, 13, 1579152335);
INSERT INTO `online_log` VALUES (167, 0, 0, 0, 13, 1579152395);
INSERT INTO `online_log` VALUES (168, 0, 0, 0, 13, 1579152455);
INSERT INTO `online_log` VALUES (169, 0, 0, 0, 13, 1579152515);
INSERT INTO `online_log` VALUES (170, 0, 0, 0, 13, 1579152575);
INSERT INTO `online_log` VALUES (171, 0, 0, 0, 13, 1579152635);
INSERT INTO `online_log` VALUES (172, 0, 0, 0, 13, 1579152695);
INSERT INTO `online_log` VALUES (173, 0, 0, 0, 13, 1579152755);
INSERT INTO `online_log` VALUES (174, 0, 0, 0, 13, 1579152815);
INSERT INTO `online_log` VALUES (175, 0, 0, 0, 13, 1579152875);
INSERT INTO `online_log` VALUES (176, 0, 0, 0, 13, 1579152935);
INSERT INTO `online_log` VALUES (177, 0, 0, 0, 13, 1579152995);
INSERT INTO `online_log` VALUES (178, 0, 0, 0, 13, 1579153055);
INSERT INTO `online_log` VALUES (179, 0, 0, 0, 13, 1579153115);
INSERT INTO `online_log` VALUES (180, 0, 0, 0, 13, 1579153175);
INSERT INTO `online_log` VALUES (181, 0, 0, 0, 13, 1579153235);
INSERT INTO `online_log` VALUES (182, 0, 0, 0, 13, 1579153295);
INSERT INTO `online_log` VALUES (183, 0, 0, 0, 13, 1579153355);
INSERT INTO `online_log` VALUES (184, 0, 0, 0, 13, 1579153415);
INSERT INTO `online_log` VALUES (185, 0, 0, 0, 13, 1579153475);
INSERT INTO `online_log` VALUES (186, 0, 0, 0, 13, 1579153535);
INSERT INTO `online_log` VALUES (187, 0, 0, 0, 13, 1579153595);
INSERT INTO `online_log` VALUES (188, 0, 0, 0, 13, 1579153655);
INSERT INTO `online_log` VALUES (189, 0, 0, 0, 13, 1579153715);
INSERT INTO `online_log` VALUES (190, 0, 0, 0, 13, 1579153775);
INSERT INTO `online_log` VALUES (191, 0, 0, 0, 13, 1579153835);
INSERT INTO `online_log` VALUES (192, 0, 0, 0, 13, 1579153895);
INSERT INTO `online_log` VALUES (193, 0, 0, 0, 13, 1579153955);
INSERT INTO `online_log` VALUES (194, 0, 0, 0, 13, 1579154015);
INSERT INTO `online_log` VALUES (195, 0, 0, 0, 13, 1579154075);
INSERT INTO `online_log` VALUES (196, 0, 0, 0, 13, 1579154135);
INSERT INTO `online_log` VALUES (197, 0, 0, 0, 13, 1579154195);
INSERT INTO `online_log` VALUES (198, 0, 0, 0, 13, 1579154255);
INSERT INTO `online_log` VALUES (199, 0, 0, 0, 13, 1579154315);
INSERT INTO `online_log` VALUES (200, 0, 0, 0, 13, 1579154375);
INSERT INTO `online_log` VALUES (201, 0, 0, 0, 14, 1579154435);
INSERT INTO `online_log` VALUES (202, 0, 0, 0, 14, 1579154495);
INSERT INTO `online_log` VALUES (203, 0, 0, 0, 14, 1579154555);
INSERT INTO `online_log` VALUES (204, 0, 0, 0, 14, 1579154615);
INSERT INTO `online_log` VALUES (205, 0, 0, 0, 14, 1579154675);
INSERT INTO `online_log` VALUES (206, 0, 0, 0, 14, 1579154735);
INSERT INTO `online_log` VALUES (207, 0, 0, 0, 12, 1581049463);
INSERT INTO `online_log` VALUES (208, 0, 0, 0, 12, 1581049523);
INSERT INTO `online_log` VALUES (209, 0, 0, 0, 12, 1581049583);
INSERT INTO `online_log` VALUES (210, 1, 0, 0, 12, 1581049799);
INSERT INTO `online_log` VALUES (211, 1, 0, 0, 12, 1581049859);
INSERT INTO `online_log` VALUES (212, 1, 0, 0, 12, 1581049919);
INSERT INTO `online_log` VALUES (213, 1, 0, 0, 12, 1581049979);
INSERT INTO `online_log` VALUES (214, 1, 0, 0, 12, 1581050039);
INSERT INTO `online_log` VALUES (215, 1, 0, 0, 12, 1581050099);
INSERT INTO `online_log` VALUES (216, 1, 0, 0, 12, 1581050159);
INSERT INTO `online_log` VALUES (217, 1, 0, 0, 12, 1581050219);
INSERT INTO `online_log` VALUES (218, 1, 0, 0, 12, 1581050279);
INSERT INTO `online_log` VALUES (219, 1, 0, 0, 12, 1581050339);
INSERT INTO `online_log` VALUES (220, 1, 0, 0, 12, 1581050399);
INSERT INTO `online_log` VALUES (221, 1, 0, 0, 12, 1581050459);
INSERT INTO `online_log` VALUES (222, 1, 0, 0, 12, 1581050519);
INSERT INTO `online_log` VALUES (223, 1, 0, 0, 12, 1581050579);
INSERT INTO `online_log` VALUES (224, 1, 0, 0, 12, 1581050639);
INSERT INTO `online_log` VALUES (225, 1, 0, 0, 12, 1581050699);
INSERT INTO `online_log` VALUES (226, 1, 0, 0, 12, 1581050759);
INSERT INTO `online_log` VALUES (227, 1, 0, 0, 12, 1581050819);
INSERT INTO `online_log` VALUES (228, 1, 0, 0, 12, 1581050879);
INSERT INTO `online_log` VALUES (229, 1, 0, 0, 12, 1581050939);
INSERT INTO `online_log` VALUES (230, 1, 0, 0, 12, 1581050999);
INSERT INTO `online_log` VALUES (231, 1, 0, 0, 12, 1581051059);
INSERT INTO `online_log` VALUES (232, 1, 0, 0, 12, 1581051119);
INSERT INTO `online_log` VALUES (233, 1, 0, 0, 12, 1581051179);
INSERT INTO `online_log` VALUES (234, 1, 0, 0, 12, 1581051239);
INSERT INTO `online_log` VALUES (235, 1, 0, 0, 12, 1581051299);
INSERT INTO `online_log` VALUES (236, 1, 0, 0, 12, 1581051359);
INSERT INTO `online_log` VALUES (237, 1, 0, 0, 12, 1581051419);
INSERT INTO `online_log` VALUES (238, 1, 0, 0, 12, 1581051479);
INSERT INTO `online_log` VALUES (239, 1, 0, 0, 12, 1581051539);
INSERT INTO `online_log` VALUES (240, 1, 0, 0, 12, 1581051599);
INSERT INTO `online_log` VALUES (241, 1, 0, 0, 13, 1581051659);
INSERT INTO `online_log` VALUES (242, 1, 0, 0, 13, 1581051719);
INSERT INTO `online_log` VALUES (243, 1, 0, 0, 13, 1581051779);
INSERT INTO `online_log` VALUES (244, 1, 0, 0, 13, 1581051839);
INSERT INTO `online_log` VALUES (245, 1, 0, 0, 13, 1581051899);
INSERT INTO `online_log` VALUES (246, 1, 0, 0, 13, 1581051959);
INSERT INTO `online_log` VALUES (247, 1, 0, 0, 13, 1581052019);
INSERT INTO `online_log` VALUES (248, 1, 0, 0, 13, 1581052079);
INSERT INTO `online_log` VALUES (249, 1, 0, 0, 13, 1581052139);
INSERT INTO `online_log` VALUES (250, 1, 0, 0, 13, 1581052199);
INSERT INTO `online_log` VALUES (251, 1, 0, 0, 13, 1581052259);
INSERT INTO `online_log` VALUES (252, 1, 0, 0, 13, 1581052319);
INSERT INTO `online_log` VALUES (253, 1, 0, 0, 13, 1581052379);
INSERT INTO `online_log` VALUES (254, 1, 0, 0, 13, 1581052439);
INSERT INTO `online_log` VALUES (255, 1, 0, 0, 13, 1581052499);
INSERT INTO `online_log` VALUES (256, 1, 0, 0, 13, 1581052559);
INSERT INTO `online_log` VALUES (257, 1, 0, 0, 13, 1581052619);
INSERT INTO `online_log` VALUES (258, 1, 0, 0, 13, 1581052679);
INSERT INTO `online_log` VALUES (259, 1, 0, 0, 13, 1581052739);
INSERT INTO `online_log` VALUES (260, 1, 0, 0, 17, 1581069429);
INSERT INTO `online_log` VALUES (261, 1, 0, 0, 17, 1581069489);
INSERT INTO `online_log` VALUES (262, 1, 0, 0, 18, 1581069689);
INSERT INTO `online_log` VALUES (263, 1, 0, 0, 18, 1581069749);
INSERT INTO `online_log` VALUES (264, 1, 0, 0, 18, 1581069809);
INSERT INTO `online_log` VALUES (265, 1, 0, 0, 18, 1581070304);
INSERT INTO `online_log` VALUES (266, 1, 0, 0, 18, 1581070364);
INSERT INTO `online_log` VALUES (267, 1, 0, 0, 18, 1581070424);
INSERT INTO `online_log` VALUES (268, 1, 0, 0, 18, 1581070484);
INSERT INTO `online_log` VALUES (269, 1, 0, 0, 18, 1581070544);
INSERT INTO `online_log` VALUES (270, 1, 0, 0, 18, 1581070604);
INSERT INTO `online_log` VALUES (271, 1, 0, 0, 18, 1581070664);
INSERT INTO `online_log` VALUES (272, 1, 0, 0, 18, 1581070724);
INSERT INTO `online_log` VALUES (273, 1, 0, 0, 18, 1581070784);
INSERT INTO `online_log` VALUES (274, 1, 0, 0, 18, 1581070844);
INSERT INTO `online_log` VALUES (275, 1, 0, 0, 18, 1581070904);
INSERT INTO `online_log` VALUES (276, 1, 0, 0, 18, 1581070964);
INSERT INTO `online_log` VALUES (277, 1, 0, 0, 18, 1581071024);
INSERT INTO `online_log` VALUES (278, 1, 0, 0, 18, 1581071084);
INSERT INTO `online_log` VALUES (279, 1, 0, 0, 18, 1581071144);
INSERT INTO `online_log` VALUES (280, 1, 0, 0, 18, 1581071204);
INSERT INTO `online_log` VALUES (281, 1, 0, 0, 18, 1581071264);
INSERT INTO `online_log` VALUES (282, 1, 0, 0, 18, 1581071324);
INSERT INTO `online_log` VALUES (283, 1, 0, 0, 18, 1581071384);
INSERT INTO `online_log` VALUES (284, 1, 0, 0, 18, 1581071444);
INSERT INTO `online_log` VALUES (285, 1, 0, 0, 18, 1581071504);
INSERT INTO `online_log` VALUES (286, 1, 0, 0, 18, 1581071564);
INSERT INTO `online_log` VALUES (287, 1, 0, 0, 18, 1581071624);
INSERT INTO `online_log` VALUES (288, 1, 0, 0, 18, 1581071684);
INSERT INTO `online_log` VALUES (289, 1, 0, 0, 18, 1581071744);
INSERT INTO `online_log` VALUES (290, 1, 0, 0, 18, 1581071804);
INSERT INTO `online_log` VALUES (291, 1, 0, 0, 18, 1581071864);
INSERT INTO `online_log` VALUES (292, 1, 0, 0, 18, 1581071924);
INSERT INTO `online_log` VALUES (293, 1, 0, 0, 18, 1581071984);
INSERT INTO `online_log` VALUES (294, 1, 0, 0, 18, 1581072044);
INSERT INTO `online_log` VALUES (295, 1, 0, 0, 18, 1581072104);
INSERT INTO `online_log` VALUES (296, 1, 0, 0, 18, 1581072164);
INSERT INTO `online_log` VALUES (297, 1, 0, 0, 18, 1581072224);
INSERT INTO `online_log` VALUES (298, 1, 0, 0, 18, 1581072284);
INSERT INTO `online_log` VALUES (299, 1, 0, 0, 18, 1581072344);
INSERT INTO `online_log` VALUES (300, 1, 0, 0, 18, 1581072404);
INSERT INTO `online_log` VALUES (301, 1, 0, 0, 18, 1581072464);
INSERT INTO `online_log` VALUES (302, 1, 0, 0, 18, 1581072524);
INSERT INTO `online_log` VALUES (303, 1, 0, 0, 18, 1581072584);
INSERT INTO `online_log` VALUES (304, 1, 0, 0, 18, 1581072644);
INSERT INTO `online_log` VALUES (305, 1, 0, 0, 18, 1581072704);
INSERT INTO `online_log` VALUES (306, 1, 0, 0, 18, 1581072764);
INSERT INTO `online_log` VALUES (307, 1, 0, 0, 18, 1581072824);
INSERT INTO `online_log` VALUES (308, 1, 0, 0, 18, 1581072884);
INSERT INTO `online_log` VALUES (309, 1, 0, 0, 14, 1581230412);
INSERT INTO `online_log` VALUES (310, 1, 0, 0, 14, 1581230472);
INSERT INTO `online_log` VALUES (311, 1, 0, 0, 14, 1581230532);
INSERT INTO `online_log` VALUES (312, 1, 0, 0, 14, 1581230592);
INSERT INTO `online_log` VALUES (313, 1, 0, 0, 14, 1581230652);
INSERT INTO `online_log` VALUES (314, 1, 0, 0, 14, 1581230712);
INSERT INTO `online_log` VALUES (315, 1, 0, 0, 14, 1581230772);
INSERT INTO `online_log` VALUES (316, 1, 0, 0, 14, 1581230918);
INSERT INTO `online_log` VALUES (317, 1, 0, 0, 14, 1581230978);
INSERT INTO `online_log` VALUES (318, 1, 0, 0, 14, 1581231038);
INSERT INTO `online_log` VALUES (319, 1, 0, 0, 14, 1581231098);
INSERT INTO `online_log` VALUES (320, 1, 0, 0, 14, 1581231158);
INSERT INTO `online_log` VALUES (321, 1, 0, 0, 14, 1581231218);
INSERT INTO `online_log` VALUES (322, 1, 0, 0, 14, 1581231278);
INSERT INTO `online_log` VALUES (323, 1, 0, 0, 14, 1581231419);
INSERT INTO `online_log` VALUES (324, 1, 0, 0, 14, 1581231479);
INSERT INTO `online_log` VALUES (325, 1, 0, 0, 14, 1581231539);
INSERT INTO `online_log` VALUES (326, 1, 0, 0, 14, 1581231599);
INSERT INTO `online_log` VALUES (327, 1, 0, 0, 15, 1581231659);
INSERT INTO `online_log` VALUES (328, 1, 0, 0, 15, 1581231719);
INSERT INTO `online_log` VALUES (329, 1, 0, 0, 15, 1581231779);
INSERT INTO `online_log` VALUES (330, 1, 0, 0, 15, 1581231839);
INSERT INTO `online_log` VALUES (331, 1, 0, 0, 15, 1581231899);
INSERT INTO `online_log` VALUES (332, 1, 0, 0, 15, 1581231959);
INSERT INTO `online_log` VALUES (333, 1, 0, 0, 15, 1581232019);
INSERT INTO `online_log` VALUES (334, 1, 0, 0, 15, 1581232079);
INSERT INTO `online_log` VALUES (335, 1, 0, 0, 15, 1581232139);
INSERT INTO `online_log` VALUES (336, 1, 0, 0, 15, 1581232199);
INSERT INTO `online_log` VALUES (337, 1, 0, 0, 15, 1581232259);
INSERT INTO `online_log` VALUES (338, 1, 0, 0, 15, 1581232319);
INSERT INTO `online_log` VALUES (339, 1, 0, 0, 15, 1581232379);
INSERT INTO `online_log` VALUES (340, 1, 0, 0, 15, 1581232439);
INSERT INTO `online_log` VALUES (341, 1, 0, 0, 15, 1581232499);
INSERT INTO `online_log` VALUES (342, 1, 0, 0, 15, 1581232559);
INSERT INTO `online_log` VALUES (343, 1, 0, 0, 15, 1581232619);
INSERT INTO `online_log` VALUES (344, 1, 0, 0, 15, 1581232679);
INSERT INTO `online_log` VALUES (345, 1, 0, 0, 15, 1581232739);
INSERT INTO `online_log` VALUES (346, 1, 0, 0, 15, 1581232799);
INSERT INTO `online_log` VALUES (347, 1, 0, 0, 15, 1581232859);
INSERT INTO `online_log` VALUES (348, 1, 0, 0, 15, 1581232919);
INSERT INTO `online_log` VALUES (349, 1, 0, 0, 15, 1581232979);
INSERT INTO `online_log` VALUES (350, 1, 0, 0, 15, 1581233039);
INSERT INTO `online_log` VALUES (351, 1, 0, 0, 15, 1581233099);
INSERT INTO `online_log` VALUES (352, 1, 0, 0, 15, 1581233159);
INSERT INTO `online_log` VALUES (353, 1, 0, 0, 15, 1581233219);
INSERT INTO `online_log` VALUES (354, 1, 0, 0, 15, 1581233279);
INSERT INTO `online_log` VALUES (355, 1, 0, 0, 15, 1581233339);
INSERT INTO `online_log` VALUES (356, 1, 0, 0, 15, 1581233399);
INSERT INTO `online_log` VALUES (357, 1, 0, 0, 15, 1581233657);
INSERT INTO `online_log` VALUES (358, 1, 0, 0, 15, 1581233717);
INSERT INTO `online_log` VALUES (359, 1, 0, 0, 15, 1581233777);
INSERT INTO `online_log` VALUES (360, 1, 0, 0, 15, 1581233837);
INSERT INTO `online_log` VALUES (361, 1, 0, 0, 15, 1581233897);
INSERT INTO `online_log` VALUES (362, 1, 0, 0, 15, 1581233957);
INSERT INTO `online_log` VALUES (363, 1, 0, 0, 15, 1581234017);
INSERT INTO `online_log` VALUES (364, 1, 0, 0, 15, 1581234077);
INSERT INTO `online_log` VALUES (365, 1, 0, 0, 15, 1581234137);
INSERT INTO `online_log` VALUES (366, 1, 0, 0, 15, 1581234197);
INSERT INTO `online_log` VALUES (367, 1, 0, 0, 15, 1581234257);
INSERT INTO `online_log` VALUES (368, 1, 0, 0, 15, 1581234317);
INSERT INTO `online_log` VALUES (369, 1, 0, 0, 15, 1581234377);
INSERT INTO `online_log` VALUES (370, 1, 0, 0, 15, 1581234437);
INSERT INTO `online_log` VALUES (371, 1, 0, 0, 15, 1581234497);
INSERT INTO `online_log` VALUES (372, 1, 0, 0, 15, 1581234650);
INSERT INTO `online_log` VALUES (373, 1, 0, 0, 15, 1581234710);
INSERT INTO `online_log` VALUES (374, 1, 0, 0, 15, 1581234770);
INSERT INTO `online_log` VALUES (375, 1, 0, 0, 15, 1581234830);
INSERT INTO `online_log` VALUES (376, 1, 0, 0, 15, 1581234890);
INSERT INTO `online_log` VALUES (377, 1, 0, 0, 15, 1581234950);
INSERT INTO `online_log` VALUES (378, 1, 0, 0, 15, 1581235010);
INSERT INTO `online_log` VALUES (379, 1, 0, 0, 15, 1581235070);
INSERT INTO `online_log` VALUES (380, 1, 0, 0, 15, 1581235130);
INSERT INTO `online_log` VALUES (381, 1, 0, 0, 15, 1581235190);
INSERT INTO `online_log` VALUES (382, 1, 0, 0, 16, 1581235250);
INSERT INTO `online_log` VALUES (383, 1, 0, 0, 16, 1581235310);
INSERT INTO `online_log` VALUES (384, 1, 0, 0, 16, 1581235370);
INSERT INTO `online_log` VALUES (385, 1, 0, 0, 16, 1581235430);
INSERT INTO `online_log` VALUES (386, 1, 0, 0, 16, 1581235490);
INSERT INTO `online_log` VALUES (387, 1, 0, 0, 16, 1581235550);
INSERT INTO `online_log` VALUES (388, 1, 0, 0, 16, 1581235610);
INSERT INTO `online_log` VALUES (389, 1, 0, 0, 16, 1581235670);
INSERT INTO `online_log` VALUES (390, 1, 0, 0, 16, 1581235730);
INSERT INTO `online_log` VALUES (391, 1, 0, 0, 16, 1581235911);
INSERT INTO `online_log` VALUES (392, 1, 0, 0, 16, 1581235971);
INSERT INTO `online_log` VALUES (393, 1, 0, 0, 16, 1581236031);
INSERT INTO `online_log` VALUES (394, 1, 0, 0, 16, 1581236091);
INSERT INTO `online_log` VALUES (395, 1, 0, 0, 16, 1581236151);
INSERT INTO `online_log` VALUES (396, 1, 0, 0, 16, 1581236211);
INSERT INTO `online_log` VALUES (397, 1, 0, 0, 16, 1581236271);
INSERT INTO `online_log` VALUES (398, 1, 0, 0, 16, 1581236331);
INSERT INTO `online_log` VALUES (399, 1, 0, 0, 16, 1581236391);
INSERT INTO `online_log` VALUES (400, 1, 0, 0, 16, 1581236451);
INSERT INTO `online_log` VALUES (401, 1, 0, 0, 16, 1581236511);
INSERT INTO `online_log` VALUES (402, 1, 0, 0, 16, 1581236687);
INSERT INTO `online_log` VALUES (403, 1, 0, 0, 16, 1581236815);
INSERT INTO `online_log` VALUES (404, 1, 0, 0, 16, 1581236875);
INSERT INTO `online_log` VALUES (405, 1, 0, 0, 16, 1581236935);
INSERT INTO `online_log` VALUES (406, 1, 0, 0, 16, 1581236995);
INSERT INTO `online_log` VALUES (407, 1, 0, 0, 16, 1581237055);
INSERT INTO `online_log` VALUES (408, 1, 0, 0, 16, 1581237115);
INSERT INTO `online_log` VALUES (409, 1, 0, 0, 16, 1581237175);
INSERT INTO `online_log` VALUES (410, 1, 0, 0, 16, 1581237235);
INSERT INTO `online_log` VALUES (411, 1, 0, 0, 16, 1581237295);
INSERT INTO `online_log` VALUES (412, 1, 0, 0, 16, 1581237355);
INSERT INTO `online_log` VALUES (413, 1, 0, 0, 16, 1581237415);
INSERT INTO `online_log` VALUES (414, 1, 0, 0, 16, 1581237475);
INSERT INTO `online_log` VALUES (415, 1, 0, 0, 16, 1581237535);
INSERT INTO `online_log` VALUES (416, 1, 0, 0, 16, 1581237595);
INSERT INTO `online_log` VALUES (417, 1, 0, 0, 16, 1581237655);
INSERT INTO `online_log` VALUES (418, 1, 0, 0, 16, 1581237715);
INSERT INTO `online_log` VALUES (419, 1, 0, 0, 16, 1581237775);
INSERT INTO `online_log` VALUES (420, 1, 0, 0, 16, 1581237835);
INSERT INTO `online_log` VALUES (421, 1, 0, 0, 16, 1581237895);
INSERT INTO `online_log` VALUES (422, 1, 0, 0, 16, 1581237955);
INSERT INTO `online_log` VALUES (423, 1, 0, 0, 16, 1581238015);
INSERT INTO `online_log` VALUES (424, 1, 0, 0, 16, 1581238075);
INSERT INTO `online_log` VALUES (425, 1, 0, 0, 16, 1581238135);
INSERT INTO `online_log` VALUES (426, 1, 0, 0, 16, 1581238195);
INSERT INTO `online_log` VALUES (427, 1, 0, 0, 16, 1581238255);
INSERT INTO `online_log` VALUES (428, 0, 0, 0, 10, 1581300462);
INSERT INTO `online_log` VALUES (429, 0, 0, 0, 10, 1581300522);
INSERT INTO `online_log` VALUES (430, 0, 0, 0, 10, 1581300582);
INSERT INTO `online_log` VALUES (431, 0, 0, 0, 10, 1581300642);
INSERT INTO `online_log` VALUES (432, 0, 0, 0, 10, 1581300702);
INSERT INTO `online_log` VALUES (433, 0, 0, 0, 10, 1581300762);
INSERT INTO `online_log` VALUES (434, 0, 0, 0, 10, 1581300822);
INSERT INTO `online_log` VALUES (435, 0, 0, 0, 10, 1581300882);
INSERT INTO `online_log` VALUES (436, 0, 0, 0, 10, 1581300942);
INSERT INTO `online_log` VALUES (437, 0, 0, 0, 10, 1581301002);
INSERT INTO `online_log` VALUES (438, 0, 0, 0, 10, 1581301062);
INSERT INTO `online_log` VALUES (439, 0, 0, 0, 10, 1581301122);
INSERT INTO `online_log` VALUES (440, 0, 0, 0, 10, 1581301182);
INSERT INTO `online_log` VALUES (441, 0, 0, 0, 10, 1581301242);
INSERT INTO `online_log` VALUES (442, 0, 0, 0, 10, 1581301302);
INSERT INTO `online_log` VALUES (443, 0, 0, 0, 10, 1581301362);
INSERT INTO `online_log` VALUES (444, 0, 0, 0, 10, 1581301422);
INSERT INTO `online_log` VALUES (445, 0, 0, 0, 10, 1581301482);
INSERT INTO `online_log` VALUES (446, 0, 0, 0, 10, 1581301542);
INSERT INTO `online_log` VALUES (447, 0, 0, 0, 10, 1581301602);
INSERT INTO `online_log` VALUES (448, 0, 0, 0, 10, 1581301662);
INSERT INTO `online_log` VALUES (449, 0, 0, 0, 10, 1581301722);
INSERT INTO `online_log` VALUES (450, 0, 0, 0, 10, 1581301782);
INSERT INTO `online_log` VALUES (451, 0, 0, 0, 10, 1581301842);
INSERT INTO `online_log` VALUES (452, 0, 0, 0, 10, 1581301902);
INSERT INTO `online_log` VALUES (453, 0, 0, 0, 10, 1581301962);
INSERT INTO `online_log` VALUES (454, 0, 0, 0, 10, 1581302022);
INSERT INTO `online_log` VALUES (455, 0, 0, 0, 10, 1581302082);
INSERT INTO `online_log` VALUES (456, 0, 0, 0, 10, 1581302142);
INSERT INTO `online_log` VALUES (457, 0, 0, 0, 10, 1581302202);
INSERT INTO `online_log` VALUES (458, 0, 0, 0, 10, 1581302262);
INSERT INTO `online_log` VALUES (459, 0, 0, 0, 10, 1581302322);
INSERT INTO `online_log` VALUES (460, 0, 0, 0, 10, 1581302382);
INSERT INTO `online_log` VALUES (461, 0, 0, 0, 10, 1581302442);
INSERT INTO `online_log` VALUES (462, 0, 0, 0, 10, 1581302502);
INSERT INTO `online_log` VALUES (463, 0, 0, 0, 10, 1581302562);
INSERT INTO `online_log` VALUES (464, 1, 0, 0, 11, 1581306994);
INSERT INTO `online_log` VALUES (465, 1, 0, 0, 12, 1581307276);
INSERT INTO `online_log` VALUES (466, 1, 0, 0, 12, 1581307336);
INSERT INTO `online_log` VALUES (467, 1, 0, 0, 12, 1581307396);
INSERT INTO `online_log` VALUES (468, 1, 0, 0, 12, 1581307456);
INSERT INTO `online_log` VALUES (469, 1, 0, 0, 12, 1581307516);
INSERT INTO `online_log` VALUES (470, 1, 0, 0, 12, 1581307576);
INSERT INTO `online_log` VALUES (471, 1, 0, 0, 12, 1581307636);
INSERT INTO `online_log` VALUES (472, 1, 0, 0, 12, 1581307696);
INSERT INTO `online_log` VALUES (473, 1, 0, 0, 12, 1581307756);
INSERT INTO `online_log` VALUES (474, 1, 0, 0, 12, 1581307977);
INSERT INTO `online_log` VALUES (475, 1, 0, 0, 12, 1581308037);
INSERT INTO `online_log` VALUES (476, 1, 0, 0, 12, 1581308097);
INSERT INTO `online_log` VALUES (477, 1, 0, 0, 12, 1581308157);
INSERT INTO `online_log` VALUES (478, 1, 0, 0, 12, 1581308299);
INSERT INTO `online_log` VALUES (479, 1, 0, 0, 12, 1581308359);
INSERT INTO `online_log` VALUES (480, 1, 0, 0, 12, 1581308419);
INSERT INTO `online_log` VALUES (481, 1, 0, 0, 12, 1581308479);
INSERT INTO `online_log` VALUES (482, 1, 0, 0, 12, 1581308539);
INSERT INTO `online_log` VALUES (483, 1, 0, 0, 12, 1581308599);
INSERT INTO `online_log` VALUES (484, 1, 0, 0, 12, 1581308659);
INSERT INTO `online_log` VALUES (485, 1, 0, 0, 12, 1581308719);
INSERT INTO `online_log` VALUES (486, 1, 0, 0, 13, 1581311087);
INSERT INTO `online_log` VALUES (487, 1, 0, 0, 13, 1581311148);
INSERT INTO `online_log` VALUES (488, 1, 0, 0, 13, 1581311208);
INSERT INTO `online_log` VALUES (489, 1, 0, 0, 13, 1581311268);
INSERT INTO `online_log` VALUES (490, 1, 0, 0, 13, 1581311328);
INSERT INTO `online_log` VALUES (491, 1, 0, 0, 13, 1581311388);
INSERT INTO `online_log` VALUES (492, 1, 0, 0, 13, 1581311448);
INSERT INTO `online_log` VALUES (493, 1, 0, 0, 13, 1581311508);
INSERT INTO `online_log` VALUES (494, 1, 0, 0, 13, 1581311568);
INSERT INTO `online_log` VALUES (495, 1, 0, 0, 13, 1581311628);
INSERT INTO `online_log` VALUES (496, 1, 0, 0, 13, 1581311688);
INSERT INTO `online_log` VALUES (497, 1, 0, 0, 13, 1581311748);
INSERT INTO `online_log` VALUES (498, 1, 0, 0, 13, 1581311808);
INSERT INTO `online_log` VALUES (499, 1, 0, 0, 13, 1581311868);
INSERT INTO `online_log` VALUES (500, 1, 0, 0, 13, 1581311928);
INSERT INTO `online_log` VALUES (501, 1, 0, 0, 13, 1581311988);
INSERT INTO `online_log` VALUES (502, 1, 0, 0, 13, 1581312048);
INSERT INTO `online_log` VALUES (503, 1, 0, 0, 13, 1581312108);
INSERT INTO `online_log` VALUES (504, 1, 0, 0, 13, 1581312168);
INSERT INTO `online_log` VALUES (505, 1, 0, 0, 13, 1581312228);
INSERT INTO `online_log` VALUES (506, 1, 0, 0, 13, 1581312288);
INSERT INTO `online_log` VALUES (507, 1, 0, 0, 13, 1581312348);
INSERT INTO `online_log` VALUES (508, 1, 0, 0, 13, 1581312408);
INSERT INTO `online_log` VALUES (509, 1, 0, 0, 13, 1581312468);
INSERT INTO `online_log` VALUES (510, 1, 0, 0, 13, 1581312528);
INSERT INTO `online_log` VALUES (511, 1, 0, 0, 13, 1581312588);
INSERT INTO `online_log` VALUES (512, 1, 0, 0, 13, 1581312648);
INSERT INTO `online_log` VALUES (513, 1, 0, 0, 13, 1581312708);
INSERT INTO `online_log` VALUES (514, 1, 0, 0, 13, 1581312768);
INSERT INTO `online_log` VALUES (515, 1, 0, 0, 13, 1581312828);
INSERT INTO `online_log` VALUES (516, 1, 0, 0, 13, 1581312888);
INSERT INTO `online_log` VALUES (517, 1, 0, 0, 13, 1581312948);
INSERT INTO `online_log` VALUES (518, 1, 0, 0, 13, 1581313008);
INSERT INTO `online_log` VALUES (519, 1, 0, 0, 13, 1581313068);
INSERT INTO `online_log` VALUES (520, 1, 0, 0, 13, 1581313128);
INSERT INTO `online_log` VALUES (521, 1, 0, 0, 13, 1581313188);
INSERT INTO `online_log` VALUES (522, 1, 0, 0, 13, 1581313248);
INSERT INTO `online_log` VALUES (523, 1, 0, 0, 13, 1581313308);
INSERT INTO `online_log` VALUES (524, 1, 0, 0, 13, 1581313368);
INSERT INTO `online_log` VALUES (525, 1, 0, 0, 13, 1581313428);
INSERT INTO `online_log` VALUES (526, 1, 0, 0, 13, 1581313488);
INSERT INTO `online_log` VALUES (527, 1, 0, 0, 13, 1581313548);
INSERT INTO `online_log` VALUES (528, 1, 0, 0, 13, 1581313608);
INSERT INTO `online_log` VALUES (529, 1, 0, 0, 13, 1581313668);
INSERT INTO `online_log` VALUES (530, 1, 0, 0, 13, 1581313728);
INSERT INTO `online_log` VALUES (531, 1, 0, 0, 13, 1581313788);
INSERT INTO `online_log` VALUES (532, 1, 0, 0, 13, 1581313848);
INSERT INTO `online_log` VALUES (533, 1, 0, 0, 13, 1581313908);
INSERT INTO `online_log` VALUES (534, 1, 0, 0, 13, 1581313968);
INSERT INTO `online_log` VALUES (535, 1, 0, 0, 13, 1581314190);
INSERT INTO `online_log` VALUES (536, 1, 0, 0, 13, 1581314250);
INSERT INTO `online_log` VALUES (537, 1, 0, 0, 13, 1581314310);
INSERT INTO `online_log` VALUES (538, 1, 0, 0, 13, 1581314370);
INSERT INTO `online_log` VALUES (539, 0, 0, 0, 14, 1581314695);
INSERT INTO `online_log` VALUES (540, 0, 0, 0, 14, 1581314755);
INSERT INTO `online_log` VALUES (541, 0, 0, 0, 14, 1581314815);
INSERT INTO `online_log` VALUES (542, 0, 0, 0, 14, 1581314875);
INSERT INTO `online_log` VALUES (543, 0, 0, 0, 14, 1581314935);
INSERT INTO `online_log` VALUES (544, 0, 0, 0, 14, 1581314995);
INSERT INTO `online_log` VALUES (545, 0, 0, 0, 14, 1581315055);
INSERT INTO `online_log` VALUES (546, 0, 0, 0, 14, 1581315115);
INSERT INTO `online_log` VALUES (547, 0, 0, 0, 14, 1581315175);
INSERT INTO `online_log` VALUES (548, 0, 0, 0, 14, 1581315235);
INSERT INTO `online_log` VALUES (549, 0, 0, 0, 14, 1581315295);
INSERT INTO `online_log` VALUES (550, 0, 0, 0, 14, 1581315355);
INSERT INTO `online_log` VALUES (551, 0, 0, 0, 14, 1581315415);
INSERT INTO `online_log` VALUES (552, 0, 0, 0, 14, 1581315475);
INSERT INTO `online_log` VALUES (553, 0, 0, 0, 14, 1581315535);
INSERT INTO `online_log` VALUES (554, 0, 0, 0, 14, 1581315595);
INSERT INTO `online_log` VALUES (555, 0, 0, 0, 14, 1581315655);
INSERT INTO `online_log` VALUES (556, 0, 0, 0, 14, 1581315715);
INSERT INTO `online_log` VALUES (557, 0, 0, 0, 14, 1581315775);
INSERT INTO `online_log` VALUES (558, 0, 0, 0, 14, 1581315835);
INSERT INTO `online_log` VALUES (559, 0, 0, 0, 14, 1581315895);
INSERT INTO `online_log` VALUES (560, 0, 0, 0, 14, 1581315955);
INSERT INTO `online_log` VALUES (561, 0, 0, 0, 14, 1581316015);
INSERT INTO `online_log` VALUES (562, 0, 0, 0, 14, 1581316075);
INSERT INTO `online_log` VALUES (563, 0, 0, 0, 14, 1581316135);
INSERT INTO `online_log` VALUES (564, 0, 0, 0, 14, 1581316195);
INSERT INTO `online_log` VALUES (565, 0, 0, 0, 14, 1581316255);
INSERT INTO `online_log` VALUES (566, 0, 0, 0, 14, 1581316315);
INSERT INTO `online_log` VALUES (567, 0, 0, 0, 14, 1581316375);
INSERT INTO `online_log` VALUES (568, 0, 0, 0, 14, 1581316435);
INSERT INTO `online_log` VALUES (569, 0, 0, 0, 14, 1581316495);
INSERT INTO `online_log` VALUES (570, 0, 0, 0, 14, 1581316555);
INSERT INTO `online_log` VALUES (571, 0, 0, 0, 14, 1581316615);
INSERT INTO `online_log` VALUES (572, 0, 0, 0, 14, 1581316675);
INSERT INTO `online_log` VALUES (573, 0, 0, 0, 14, 1581316735);
INSERT INTO `online_log` VALUES (574, 0, 0, 0, 14, 1581316795);
INSERT INTO `online_log` VALUES (575, 0, 0, 0, 14, 1581316855);
INSERT INTO `online_log` VALUES (576, 0, 0, 0, 14, 1581316915);
INSERT INTO `online_log` VALUES (577, 0, 0, 0, 14, 1581316975);
INSERT INTO `online_log` VALUES (578, 0, 0, 0, 10, 1581389192);
INSERT INTO `online_log` VALUES (579, 0, 0, 0, 10, 1581389252);
INSERT INTO `online_log` VALUES (580, 0, 0, 0, 10, 1581389312);
INSERT INTO `online_log` VALUES (581, 1, 0, 0, 13, 1581400189);
INSERT INTO `online_log` VALUES (582, 1, 0, 0, 13, 1581400249);
INSERT INTO `online_log` VALUES (583, 1, 0, 0, 13, 1581400309);
INSERT INTO `online_log` VALUES (584, 1, 0, 0, 13, 1581400369);
INSERT INTO `online_log` VALUES (585, 1, 0, 0, 13, 1581400429);
INSERT INTO `online_log` VALUES (586, 1, 0, 0, 13, 1581400489);
INSERT INTO `online_log` VALUES (587, 1, 0, 0, 13, 1581400549);
INSERT INTO `online_log` VALUES (588, 1, 0, 0, 13, 1581400609);
INSERT INTO `online_log` VALUES (589, 1, 0, 0, 13, 1581400669);
INSERT INTO `online_log` VALUES (590, 1, 0, 0, 13, 1581400729);
INSERT INTO `online_log` VALUES (591, 1, 0, 0, 13, 1581400789);
INSERT INTO `online_log` VALUES (592, 1, 0, 0, 14, 1581400849);
INSERT INTO `online_log` VALUES (593, 1, 0, 0, 14, 1581400909);
INSERT INTO `online_log` VALUES (594, 1, 0, 0, 14, 1581400969);
INSERT INTO `online_log` VALUES (595, 1, 0, 0, 14, 1581401029);
INSERT INTO `online_log` VALUES (596, 1, 0, 0, 14, 1581401167);
INSERT INTO `online_log` VALUES (597, 1, 0, 0, 14, 1581401227);
INSERT INTO `online_log` VALUES (598, 1, 0, 0, 14, 1581401287);
INSERT INTO `online_log` VALUES (599, 1, 0, 0, 14, 1581401347);
INSERT INTO `online_log` VALUES (600, 1, 0, 0, 14, 1581401407);
INSERT INTO `online_log` VALUES (601, 1, 0, 0, 14, 1581401467);
INSERT INTO `online_log` VALUES (602, 1, 0, 0, 14, 1581401527);
INSERT INTO `online_log` VALUES (603, 1, 0, 0, 14, 1581401587);
INSERT INTO `online_log` VALUES (604, 1, 0, 0, 14, 1581401647);
INSERT INTO `online_log` VALUES (605, 1, 0, 0, 14, 1581401707);
INSERT INTO `online_log` VALUES (606, 1, 0, 0, 14, 1581401767);
INSERT INTO `online_log` VALUES (607, 1, 0, 0, 15, 1581405687);
INSERT INTO `online_log` VALUES (608, 1, 0, 0, 15, 1581405747);
INSERT INTO `online_log` VALUES (609, 1, 0, 0, 15, 1581405807);
INSERT INTO `online_log` VALUES (610, 1, 0, 0, 15, 1581405867);

-- ----------------------------
-- Table structure for parameter_data
-- ----------------------------
DROP TABLE IF EXISTS `parameter_data`;
CREATE TABLE `parameter_data`  (
  `key` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL DEFAULT '' COMMENT '参数键',
  `value` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL DEFAULT '' COMMENT '参数值',
  `description` char(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL DEFAULT '' COMMENT '参数名称',
  PRIMARY KEY (`key`) USING BTREE
) ENGINE = InnoDB CHARACTER SET = utf8mb4 COLLATE = utf8mb4_general_ci COMMENT = '游戏参数配置表' ROW_FORMAT = Dynamic;

-- ----------------------------
-- Records of parameter_data
-- ----------------------------
INSERT INTO `parameter_data` VALUES ('chat_cd', '30', '聊天冷却时间');
INSERT INTO `parameter_data` VALUES ('chat_level', '10', '聊天开放等级');
INSERT INTO `parameter_data` VALUES ('friend_level', '30', '好友开放等级');
INSERT INTO `parameter_data` VALUES ('friend_number', '50', '好友上限');
INSERT INTO `parameter_data` VALUES ('guild_create', '[{1, [{level, 10}, {vip, 0}, {gold, 0}]}, {2, [{level, 50}, {vip, 1}, {gold, 100}]},{3, [{level, 100}, {vip, 3}, {gold, 500}]}]', '创建一级公会条件');
INSERT INTO `parameter_data` VALUES ('guild_create_cd', '86400', '公会创建冷却时间');
INSERT INTO `parameter_data` VALUES ('guild_join_cd', '86400', '公会加入冷却时间');
INSERT INTO `parameter_data` VALUES ('guild_member_limit', '[{0, 50}, {1, 60}, {2, 70}, {3, 80}, {4, 90}, {5, 100}]', '公会人员数');
INSERT INTO `parameter_data` VALUES ('language', 'sc', '默认语言');
INSERT INTO `parameter_data` VALUES ('language_set', '[{1, sc}, {2, tc}, {3, en}, {4, kr}, {5, vi}]', '支持语言');
INSERT INTO `parameter_data` VALUES ('login_cd', '180', '登录时间间隔');
INSERT INTO `parameter_data` VALUES ('time_zone', '+8', '时区');

-- ----------------------------
-- Table structure for quest
-- ----------------------------
DROP TABLE IF EXISTS `quest`;
CREATE TABLE `quest`  (
  `role_id` bigint(20) UNSIGNED NOT NULL DEFAULT 0 COMMENT '角色ID(select)',
  `quest_id` int(10) UNSIGNED NOT NULL DEFAULT 0 COMMENT '任务ID',
  `type` int(10) UNSIGNED NOT NULL DEFAULT 0 COMMENT '类型',
  `event` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL DEFAULT '' COMMENT '事件',
  `target` int(10) UNSIGNED NOT NULL DEFAULT 0 COMMENT '目标',
  `number` int(10) UNSIGNED NOT NULL DEFAULT 0 COMMENT '数量',
  `compare` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL DEFAULT '' COMMENT '比较',
  `award` tinyint(1) UNSIGNED NOT NULL DEFAULT 0 COMMENT '是否领取奖励',
  `flag` varchar(0) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL DEFAULT '' COMMENT '标识(flag)',
  PRIMARY KEY (`role_id`, `type`) USING BTREE
) ENGINE = InnoDB CHARACTER SET = utf8mb4 COLLATE = utf8mb4_general_ci COMMENT = '角色任务表' ROW_FORMAT = Dynamic;

-- ----------------------------
-- Records of quest
-- ----------------------------
INSERT INTO `quest` VALUES (1, 1, 1, 'event_kill_monster', 0, 0, 'nc', 1, '');
INSERT INTO `quest` VALUES (1, 1001, 2, 'event_dungeon_passed', 100, 1, 'ge', 0, '');
INSERT INTO `quest` VALUES (1, 100001, 3, 'event_shop_buy', 1, 1, 'eq', 0, '');

-- ----------------------------
-- Table structure for quest_data
-- ----------------------------
DROP TABLE IF EXISTS `quest_data`;
CREATE TABLE `quest_data`  (
  `quest_id` int(10) UNSIGNED NOT NULL DEFAULT 0 COMMENT '任务ID',
  `type` int(10) UNSIGNED NOT NULL DEFAULT 0 COMMENT '类型',
  `pre_id` int(10) UNSIGNED NOT NULL DEFAULT 0 COMMENT '前置任务',
  `next_id` int(10) UNSIGNED NOT NULL DEFAULT 0 COMMENT '后置任务',
  `module` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL DEFAULT '' COMMENT '模块(validate(module))',
  `function` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL DEFAULT '' COMMENT '函数(validate(function))',
  `event` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL DEFAULT '' COMMENT '事件(validate(event))',
  `compare` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL DEFAULT '' COMMENT '比较模式(validate(compare))',
  `target` int(10) UNSIGNED NOT NULL DEFAULT 0 COMMENT '目标',
  `number` int(10) UNSIGNED NOT NULL DEFAULT 0 COMMENT '数量',
  `condition` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL DEFAULT '' COMMENT '条件',
  `cost` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL DEFAULT '' COMMENT '消耗',
  `award` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL DEFAULT '' COMMENT '奖励',
  `title` char(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL DEFAULT '' COMMENT '标题',
  `content` char(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL DEFAULT '' COMMENT '内容',
  `description` char(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL DEFAULT '' COMMENT '描述',
  PRIMARY KEY (`quest_id`) USING BTREE
) ENGINE = InnoDB CHARACTER SET = utf8mb4 COLLATE = utf8mb4_general_ci COMMENT = '任务配置表' ROW_FORMAT = Dynamic;

-- ----------------------------
-- Records of quest_data
-- ----------------------------
INSERT INTO `quest_data` VALUES (1, 1, 0, 2, '', '', 'event_kill_monster', 'nc', 0, 3, '', '', '[{1,1}]', '', '', '');
INSERT INTO `quest_data` VALUES (2, 1, 1, 3, 'role', 'check_quest', 'event_level_upgrade', 'ge', 5, 1, '', '[{100003, 100}]', '[{1,10}]', '', '', '');
INSERT INTO `quest_data` VALUES (3, 1, 2, 4, '', '', 'event_dungeon_passed', 'ge', 100001, 1, '[{level, 10}]', '', '[{1,100}]', '', '', '');
INSERT INTO `quest_data` VALUES (4, 1, 3, 5, '', '', 'event_shop_buy', 'eq', 1, 1, '', '', '[{1,1000}]', '', '', '');
INSERT INTO `quest_data` VALUES (5, 1, 4, 0, '', '', 'event_guild_join', 'nc', 0, 1, '', '', '[{1,1000}]', '', '', '');
INSERT INTO `quest_data` VALUES (6, 1, 5, 0, 'friend', 'check_quest', 'event_friend_add', 'nc', 0, 5, '', '', '[{1,10}]', '', '', '');
INSERT INTO `quest_data` VALUES (1001, 2, 0, 1002, '', '', 'event_dungeon_passed', 'ge', 100, 1, '', '', '[{1,10}]', '', '', '');
INSERT INTO `quest_data` VALUES (1002, 2, 1001, 0, '', '', 'event_friend_add', 'eq', 1, 1, '', '', '[{1,10}]', '', '', '');
INSERT INTO `quest_data` VALUES (100001, 3, 0, 100002, 'shop', 'check_quest', 'event_shop_buy', 'eq', 1, 1, '', '', '[{1,10}]', '', '', '');
INSERT INTO `quest_data` VALUES (100002, 3, 100001, 0, '', '', 'event_guild_join', 'nc', 0, 1, '', '', '[{1,10}]', '', '', '');

-- ----------------------------
-- Table structure for quest_log
-- ----------------------------
DROP TABLE IF EXISTS `quest_log`;
CREATE TABLE `quest_log`  (
  `id` int(10) UNSIGNED NOT NULL AUTO_INCREMENT COMMENT 'ID',
  `role_id` int(10) UNSIGNED NOT NULL DEFAULT 0 COMMENT '角色ID',
  `quest_id` int(10) UNSIGNED NOT NULL DEFAULT 0 COMMENT '任务ID',
  `time` int(10) UNSIGNED NOT NULL DEFAULT 0 COMMENT '时间',
  PRIMARY KEY (`id`) USING BTREE,
  INDEX `role_id`(`role_id`) USING BTREE,
  INDEX `time`(`time`) USING BTREE
) ENGINE = InnoDB AUTO_INCREMENT = 1 CHARACTER SET = utf8mb4 COLLATE = utf8mb4_general_ci COMMENT = '任务日志表' ROW_FORMAT = Compressed;

-- ----------------------------
-- Records of quest_log
-- ----------------------------

-- ----------------------------
-- Table structure for rank
-- ----------------------------
DROP TABLE IF EXISTS `rank`;
CREATE TABLE `rank`  (
  `type` tinyint(3) UNSIGNED NOT NULL DEFAULT 0 COMMENT '类型(select)(delete_type)',
  `rank` bigint(20) UNSIGNED NOT NULL DEFAULT 0 COMMENT '排名',
  `key` bigint(20) UNSIGNED NOT NULL DEFAULT 0 COMMENT '键',
  `value` bigint(20) UNSIGNED NOT NULL DEFAULT 0 COMMENT '值',
  `time` int(20) UNSIGNED NOT NULL DEFAULT 0 COMMENT '时间',
  `name` char(16) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL DEFAULT '' COMMENT '名字',
  `digest` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL DEFAULT '' COMMENT '摘要数据',
  `extra` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL DEFAULT '' COMMENT '额外数据',
  `other` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL DEFAULT '' COMMENT '其他数据',
  `flag` varchar(0) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL DEFAULT '' COMMENT '标识((flag)/default(1))',
  PRIMARY KEY (`type`, `rank`) USING BTREE
) ENGINE = InnoDB CHARACTER SET = utf8mb4 COLLATE = utf8mb4_general_ci COMMENT = '角色排行表' ROW_FORMAT = Dynamic;

-- ----------------------------
-- Records of rank
-- ----------------------------
INSERT INTO `rank` VALUES (1, 1, 1, 1, 1, '1', '[]', '[]', '[]', '');
INSERT INTO `rank` VALUES (1, 2, 7, 7, 7, '7', '[]', '[]', '[]', '');
INSERT INTO `rank` VALUES (1, 3, 6, 6, 6, '6', '[]', '[]', '[]', '');
INSERT INTO `rank` VALUES (1, 4, 5, 5, 5, '5', '[]', '[]', '[]', '');
INSERT INTO `rank` VALUES (1, 5, 4, 4, 4, '4', '[]', '[]', '[]', '');
INSERT INTO `rank` VALUES (1, 6, 3, 3, 3, '3', '[]', '[]', '[]', '');
INSERT INTO `rank` VALUES (1, 7, 2, 2, 2, '2', '[]', '[]', '[]', '');

-- ----------------------------
-- Table structure for recharge
-- ----------------------------
DROP TABLE IF EXISTS `recharge`;
CREATE TABLE `recharge`  (
  `unique_id` bigint(11) UNSIGNED NOT NULL AUTO_INCREMENT COMMENT '唯一ID',
  `recharge_id` int(11) UNSIGNED NOT NULL DEFAULT 0 COMMENT '充值ID',
  `account` char(16) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL DEFAULT '' COMMENT '平台账号ID',
  `channel_id` smallint(5) UNSIGNED NOT NULL DEFAULT 0 COMMENT '渠道ID',
  `server_id` smallint(5) UNSIGNED NOT NULL DEFAULT 0 COMMENT '区服ID',
  `role_id` bigint(20) UNSIGNED NOT NULL DEFAULT 0 COMMENT '玩家ID',
  `role_name` char(16) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL DEFAULT '' COMMENT '玩家名称',
  `money` decimal(12, 2) UNSIGNED NOT NULL DEFAULT 0 COMMENT '充值金额',
  `gold` int(11) UNSIGNED NOT NULL DEFAULT 0 COMMENT '金币',
  `status` tinyint(1) UNSIGNED NOT NULL DEFAULT 0 COMMENT '状态(0:未发放/1:已发放)',
  `time` int(11) UNSIGNED NOT NULL DEFAULT 0 COMMENT '订单时间',
  `receive_time` int(11) UNSIGNED NOT NULL DEFAULT 0 COMMENT '发放时间',
  PRIMARY KEY (`unique_id`) USING BTREE,
  INDEX `role_id`(`role_id`, `status`) USING BTREE,
  INDEX `channel_id`(`channel_id`) USING BTREE
) ENGINE = InnoDB AUTO_INCREMENT = 3 CHARACTER SET = utf8mb4 COLLATE = utf8mb4_general_ci COMMENT = '角色充值订单表' ROW_FORMAT = Dynamic;

-- ----------------------------
-- Records of recharge
-- ----------------------------

-- ----------------------------
-- Table structure for recharge_data
-- ----------------------------
DROP TABLE IF EXISTS `recharge_data`;
CREATE TABLE `recharge_data`  (
  `recharge_id` int(10) UNSIGNED NOT NULL DEFAULT 0 COMMENT '充值ID',
  `type` tinyint(3) UNSIGNED NOT NULL DEFAULT 0 COMMENT '类型(普通充值:0/购买月卡:1)',
  `channel_id` tinyint(3) UNSIGNED NOT NULL DEFAULT 0 COMMENT '渠道ID',
  `limit` int(10) UNSIGNED NOT NULL DEFAULT 0 COMMENT '限制数量',
  `original_price` int(10) UNSIGNED NOT NULL DEFAULT 0 COMMENT '原价',
  `now_price` int(10) UNSIGNED NOT NULL DEFAULT 0 COMMENT '现价',
  `gold` int(10) UNSIGNED NOT NULL DEFAULT 0 COMMENT '金币',
  `gift_gold` int(10) UNSIGNED NOT NULL DEFAULT 0 COMMENT '赠送金币',
  `begin_open_days` int(10) UNSIGNED NOT NULL DEFAULT 0 COMMENT '结束时间，跟开服相关，填天数',
  `end_open_days` int(10) UNSIGNED NOT NULL DEFAULT 0 COMMENT '结束时间，跟开服相关，填天数',
  `sort` int(10) UNSIGNED NOT NULL DEFAULT 0 COMMENT '排序',
  `icon` char(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL DEFAULT '' COMMENT '图片',
  `name` char(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL DEFAULT '' COMMENT '名字',
  `description` char(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL DEFAULT '' COMMENT '描述',
  PRIMARY KEY (`recharge_id`) USING BTREE
) ENGINE = InnoDB CHARACTER SET = utf8mb4 COLLATE = utf8mb4_general_ci COMMENT = '充值配置表' ROW_FORMAT = Dynamic;

-- ----------------------------
-- Records of recharge_data
-- ----------------------------
INSERT INTO `recharge_data` VALUES (1, 3, 0, 1, 18, 18, 180, 0, 1, 9999, 1, '0', '至尊神兵宝箱', '');
INSERT INTO `recharge_data` VALUES (2, 1, 0, 1, 6, 6, 60, 5, 1, 9999, 2, '1', '元宝', '');
INSERT INTO `recharge_data` VALUES (3, 1, 0, 1, 30, 30, 300, 40, 1, 9999, 3, '2', '元宝', '');
INSERT INTO `recharge_data` VALUES (4, 1, 0, 1, 68, 68, 680, 90, 1, 9999, 4, '3', '元宝', '');
INSERT INTO `recharge_data` VALUES (5, 1, 0, 1, 128, 128, 1280, 190, 1, 9999, 5, '4', '元宝', '');
INSERT INTO `recharge_data` VALUES (6, 1, 0, 1, 198, 198, 1980, 330, 1, 9999, 6, '5', '元宝', '');
INSERT INTO `recharge_data` VALUES (7, 1, 0, 1, 328, 328, 3280, 590, 1, 9999, 7, '6', '元宝', '');
INSERT INTO `recharge_data` VALUES (8, 1, 0, 1, 648, 648, 6480, 1300, 1, 9999, 8, '7', '元宝', '');
INSERT INTO `recharge_data` VALUES (9, 2, 0, 1, 18, 18, 180, 0, 1, 9999, 0, '', '周卡', '');
INSERT INTO `recharge_data` VALUES (10, 4, 0, 1, 30, 30, 300, 0, 1, 9999, 0, '', 'vip1', '');
INSERT INTO `recharge_data` VALUES (11, 5, 0, 1, 128, 128, 1280, 0, 1, 9999, 0, '', 'vip3', '');
INSERT INTO `recharge_data` VALUES (12, 6, 0, 1, 45, 45, 450, 0, 1, 9999, 0, '', '月卡', '');

-- ----------------------------
-- Table structure for role
-- ----------------------------
DROP TABLE IF EXISTS `role`;
CREATE TABLE `role`  (
  `role_id` bigint(20) UNSIGNED NOT NULL AUTO_INCREMENT COMMENT '角色ID',
  `role_name` char(16) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL DEFAULT '' COMMENT '角色名((once)/(update_name))',
  `account` char(16) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL DEFAULT '' COMMENT '账户(once)',
  `type` tinyint(255) UNSIGNED NOT NULL DEFAULT 0 COMMENT '账户类型',
  `level` int(10) UNSIGNED NOT NULL DEFAULT 0 COMMENT '等级',
  `sex` tinyint(1) UNSIGNED NOT NULL DEFAULT 0 COMMENT '性别',
  `classes` tinyint(1) UNSIGNED NOT NULL DEFAULT 0 COMMENT '职业',
  `item_size` int(10) UNSIGNED NOT NULL DEFAULT 0 COMMENT '普通背包大小',
  `bag_size` int(10) UNSIGNED NOT NULL DEFAULT 0 COMMENT '装备背包大小',
  `store_size` int(10) UNSIGNED NOT NULL DEFAULT 0 COMMENT '仓库背包大小',
  `online` tinyint(1) UNSIGNED NOT NULL DEFAULT 0 COMMENT '是否在线',
  `online_time` int(10) UNSIGNED NOT NULL DEFAULT 0 COMMENT '在线时间',
  `server_id` smallint(5) UNSIGNED NOT NULL DEFAULT 0 COMMENT '服ID',
  `channel_id` int(10) UNSIGNED NOT NULL DEFAULT 0 COMMENT '渠道ID',
  `map` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL DEFAULT '' COMMENT '地图',
  `device_id` char(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL DEFAULT '' COMMENT '设备ID',
  `device_type` char(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL DEFAULT '' COMMENT '设备类型',
  `mac` char(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL DEFAULT '' COMMENT 'Mac地址',
  PRIMARY KEY (`role_id`) USING BTREE,
  INDEX `account`(`account`) USING BTREE
) ENGINE = InnoDB AUTO_INCREMENT = 8 CHARACTER SET = utf8mb4 COLLATE = utf8mb4_general_ci COMMENT = '角色信息表' ROW_FORMAT = Dynamic;

-- ----------------------------
-- Records of role
-- ----------------------------
INSERT INTO `role` VALUES (1, '1', '1', 3, 100, 1, 1, 100, 100, 100, 0, 1581405899, 1, 1, '{map,1000000000000000,100000,<0.150.0>,undefined,30,10}', '', '', '');
INSERT INTO `role` VALUES (2, '2', '2', 2, 200, 2, 2, 100, 100, 100, 0, 0, 1, 1, '', '', '', '');
INSERT INTO `role` VALUES (3, '3', '3', 2, 300, 1, 3, 100, 100, 100, 0, 0, 1, 1, '', '', '', '');
INSERT INTO `role` VALUES (4, '4', '4', 1, 400, 2, 4, 100, 100, 100, 0, 0, 1, 1, '', '', '', '');
INSERT INTO `role` VALUES (5, '5', '5', 1, 500, 1, 5, 100, 100, 100, 0, 0, 1, 1, '', '', '', '');
INSERT INTO `role` VALUES (6, '6', '6', 1, 600, 2, 6, 100, 100, 100, 0, 0, 1, 1, '', '', '', '');
INSERT INTO `role` VALUES (7, '7', '7', 1, 700, 0, 7, 100, 100, 100, 0, 0, 1, 1, '', '', '', '');

-- ----------------------------
-- Table structure for role_log
-- ----------------------------
DROP TABLE IF EXISTS `role_log`;
CREATE TABLE `role_log`  (
  `id` bigint(20) UNSIGNED NOT NULL AUTO_INCREMENT COMMENT 'ID',
  `role_id` bigint(20) UNSIGNED NOT NULL DEFAULT 0 COMMENT '角色ID',
  `exp` bigint(20) UNSIGNED NOT NULL DEFAULT 0 COMMENT '经验',
  `time` int(10) UNSIGNED NOT NULL DEFAULT 0 COMMENT '时间',
  PRIMARY KEY (`id`) USING BTREE,
  INDEX `role_id`(`role_id`) USING BTREE,
  INDEX `time`(`time`) USING BTREE
) ENGINE = InnoDB AUTO_INCREMENT = 1 CHARACTER SET = utf8mb4 COLLATE = utf8mb4_general_ci COMMENT = '角色日志表' ROW_FORMAT = Compressed;

-- ----------------------------
-- Records of role_log
-- ----------------------------

-- ----------------------------
-- Table structure for sensitive_word_data
-- ----------------------------
DROP TABLE IF EXISTS `sensitive_word_data`;
CREATE TABLE `sensitive_word_data`  (
  `word` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL DEFAULT '' COMMENT '敏感词',
  PRIMARY KEY (`word`) USING BTREE
) ENGINE = InnoDB CHARACTER SET = utf8mb4 COLLATE = utf8mb4_general_ci COMMENT = '敏感词配置表' ROW_FORMAT = Dynamic;

-- ----------------------------
-- Records of sensitive_word_data
-- ----------------------------

-- ----------------------------
-- Table structure for shop
-- ----------------------------
DROP TABLE IF EXISTS `shop`;
CREATE TABLE `shop`  (
  `role_id` bigint(20) UNSIGNED NOT NULL DEFAULT 0 COMMENT '角色ID(select)',
  `shop_id` int(10) UNSIGNED NOT NULL DEFAULT 0 COMMENT '商店ID',
  `number` int(10) UNSIGNED NOT NULL DEFAULT 0 COMMENT '数量',
  `flag` varchar(0) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL DEFAULT '' COMMENT '标识(flag)',
  PRIMARY KEY (`role_id`, `shop_id`) USING BTREE
) ENGINE = InnoDB CHARACTER SET = utf8mb4 COLLATE = utf8mb4_general_ci COMMENT = '角色商店表' ROW_FORMAT = Dynamic;

-- ----------------------------
-- Records of shop
-- ----------------------------
INSERT INTO `shop` VALUES (1, 1, 1, '');

-- ----------------------------
-- Table structure for shop_data
-- ----------------------------
DROP TABLE IF EXISTS `shop_data`;
CREATE TABLE `shop_data`  (
  `shop_id` int(10) UNSIGNED NOT NULL DEFAULT 0 COMMENT '商店ID',
  `item_id` int(10) UNSIGNED NOT NULL DEFAULT 0 COMMENT '物品配置ID',
  `type` int(10) UNSIGNED NOT NULL DEFAULT 0 COMMENT '商店类型',
  `pay_assets` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL DEFAULT '' COMMENT '货币类型',
  `price` int(10) UNSIGNED NOT NULL DEFAULT 0 COMMENT '价格',
  `number` int(10) UNSIGNED NOT NULL DEFAULT 1 COMMENT '数量',
  `level` int(10) UNSIGNED NOT NULL DEFAULT 0 COMMENT '等级限制',
  `limit` int(10) UNSIGNED NOT NULL DEFAULT 0 COMMENT '购买上限',
  `vip_level` int(10) UNSIGNED NOT NULL DEFAULT 0 COMMENT 'vip等级限购',
  `vip_limit` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL DEFAULT '' COMMENT 'vip等级购买上限',
  `description` char(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL DEFAULT '' COMMENT '描述',
  PRIMARY KEY (`shop_id`) USING BTREE
) ENGINE = InnoDB CHARACTER SET = utf8mb4 COLLATE = utf8mb4_general_ci COMMENT = '商店配置表' ROW_FORMAT = Dynamic;

-- ----------------------------
-- Records of shop_data
-- ----------------------------
INSERT INTO `shop_data` VALUES (1, 1, 1, 'gold', 10, 1, 0, 0, 0, '', '');

-- ----------------------------
-- Table structure for shop_log
-- ----------------------------
DROP TABLE IF EXISTS `shop_log`;
CREATE TABLE `shop_log`  (
  `id` int(10) UNSIGNED NOT NULL AUTO_INCREMENT COMMENT 'ID',
  `role_id` int(10) UNSIGNED NOT NULL DEFAULT 0 COMMENT '角色ID',
  `shop_id` int(10) UNSIGNED NOT NULL DEFAULT 0 COMMENT '商店ID',
  `number` int(10) UNSIGNED NOT NULL DEFAULT 0 COMMENT '购买数量',
  `time` int(10) UNSIGNED NOT NULL DEFAULT 0 COMMENT '时间',
  PRIMARY KEY (`id`) USING BTREE,
  INDEX `role_id`(`role_id`) USING BTREE,
  INDEX `time`(`time`) USING BTREE
) ENGINE = InnoDB AUTO_INCREMENT = 1 CHARACTER SET = utf8mb4 COLLATE = utf8mb4_general_ci COMMENT = '商店日志表' ROW_FORMAT = Compressed;

-- ----------------------------
-- Records of shop_log
-- ----------------------------

-- ----------------------------
-- Table structure for skill
-- ----------------------------
DROP TABLE IF EXISTS `skill`;
CREATE TABLE `skill`  (
  `role_id` bigint(20) UNSIGNED NOT NULL DEFAULT 0 COMMENT '角色ID(select)',
  `skill_id` int(10) UNSIGNED NOT NULL DEFAULT 0 COMMENT '技能ID',
  `level` int(10) UNSIGNED NOT NULL DEFAULT 0 COMMENT '等级',
  `flag` varchar(0) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL DEFAULT '' COMMENT '标识(flag)',
  PRIMARY KEY (`role_id`, `skill_id`) USING BTREE
) ENGINE = InnoDB CHARACTER SET = utf8mb4 COLLATE = utf8mb4_general_ci COMMENT = '角色技能表' ROW_FORMAT = Dynamic;

-- ----------------------------
-- Records of skill
-- ----------------------------
INSERT INTO `skill` VALUES (1, 1, 1, '');
INSERT INTO `skill` VALUES (1, 2, 1, '');

-- ----------------------------
-- Table structure for skill_data
-- ----------------------------
DROP TABLE IF EXISTS `skill_data`;
CREATE TABLE `skill_data`  (
  `skill_id` int(10) UNSIGNED NOT NULL DEFAULT 0 COMMENT '技能ID',
  `type` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL DEFAULT '' COMMENT '类型(validate(skill_type))',
  `name` char(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL DEFAULT '' COMMENT '名字',
  `condition` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL DEFAULT '' COMMENT '学习条件',
  `cost` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL DEFAULT '' COMMENT '升级消耗',
  `effect` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL DEFAULT '' COMMENT '作用效果',
  `cd` int(10) UNSIGNED NOT NULL DEFAULT 0 COMMENT '冷却时间',
  `radius` int(10) UNSIGNED NOT NULL DEFAULT 0 COMMENT '作用半径',
  `distance` int(10) UNSIGNED NOT NULL DEFAULT 0 COMMENT '作用距离',
  `number` int(10) UNSIGNED NOT NULL DEFAULT 0 COMMENT '作用对象数',
  `buffs` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL DEFAULT '' COMMENT '作用Buff',
  `before_effects` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL DEFAULT '' COMMENT '效果前',
  `hit_effects` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL DEFAULT '' COMMENT '击中效果',
  `after_effects` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL DEFAULT '' COMMENT '效果后',
  `description` char(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL DEFAULT '' COMMENT '描述',
  PRIMARY KEY (`skill_id`) USING BTREE
) ENGINE = InnoDB CHARACTER SET = utf8mb4 COLLATE = utf8mb4_general_ci COMMENT = '技能配置表' ROW_FORMAT = Dynamic;

-- ----------------------------
-- Records of skill_data
-- ----------------------------
INSERT INTO `skill_data` VALUES (1, 'active', '普攻技能', '', '', '[1]', 1, 1000, 1000, 1, '', '', '', '', '对目标造成180%的伤害');
INSERT INTO `skill_data` VALUES (2, 'active', '群攻技能', '', '', '[2]', 1, 1000, 1000, 30, '', '', '', '', '对3个目标造成150%的伤害');
INSERT INTO `skill_data` VALUES (3, 'passive', '增益', '', '', '[8]', 10, 1, 1, 1, '', '', '', '', '每秒扣血，总血量万分之50');
INSERT INTO `skill_data` VALUES (5, 'active', '普攻技能', '', '', '', 1, 1, 1, 1, '', '', '', '', '普通技能');

-- ----------------------------
-- Table structure for text_data
-- ----------------------------
DROP TABLE IF EXISTS `text_data`;
CREATE TABLE `text_data`  (
  `key` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL DEFAULT '' COMMENT '参数键',
  `value` char(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL DEFAULT '' COMMENT '参数值',
  `description` char(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL DEFAULT '' COMMENT '描述',
  PRIMARY KEY (`key`) USING BTREE
) ENGINE = InnoDB CHARACTER SET = utf8mb4 COLLATE = utf8mb4_general_ci COMMENT = '游戏文本配置表' ROW_FORMAT = Dynamic;

-- ----------------------------
-- Records of text_data
-- ----------------------------
INSERT INTO `text_data` VALUES ('1', '不用买，随便爆', '成龙台词');
INSERT INTO `text_data` VALUES ('2', '是兄弟就来砍我', '古天乐台词');
INSERT INTO `text_data` VALUES ('3', '卸载掉手机那个假传奇', '甄子丹台词');
INSERT INTO `text_data` VALUES ('add_item_content', '你的益达', '背包满内容');
INSERT INTO `text_data` VALUES ('add_item_title', '背包满', '背包满标题');
INSERT INTO `text_data` VALUES ('test', '😂', '😒');

-- ----------------------------
-- Table structure for title
-- ----------------------------
DROP TABLE IF EXISTS `title`;
CREATE TABLE `title`  (
  `role_id` bigint(20) UNSIGNED NOT NULL DEFAULT 0 COMMENT '角色ID(select)(update_role_id)',
  `title_id` int(10) NOT NULL DEFAULT 0 COMMENT '称号ID(select_id)',
  `type` int(10) UNSIGNED NOT NULL DEFAULT 0 COMMENT '类型',
  `expire_time` int(10) UNSIGNED NOT NULL DEFAULT 0 COMMENT '过期时间',
  `flag` varchar(0) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL DEFAULT '' COMMENT '标识(flag)',
  PRIMARY KEY (`role_id`, `title_id`) USING BTREE,
  INDEX ```title_id```(`title_id`) USING BTREE
) ENGINE = InnoDB CHARACTER SET = utf8mb4 COLLATE = utf8mb4_unicode_ci COMMENT = '角色称号表' ROW_FORMAT = Dynamic;

-- ----------------------------
-- Records of title
-- ----------------------------
INSERT INTO `title` VALUES (1, 1, 1, 0, '');

-- ----------------------------
-- Table structure for title_data
-- ----------------------------
DROP TABLE IF EXISTS `title_data`;
CREATE TABLE `title_data`  (
  `title_id` int(10) UNSIGNED NOT NULL DEFAULT 0 COMMENT '称号ID',
  `type` int(10) UNSIGNED NOT NULL DEFAULT 0 COMMENT '类型',
  `multi` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL DEFAULT '' COMMENT '同类型可否拥有多个(validate(boolean))',
  `unique` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL DEFAULT '' COMMENT '是否全服唯一(validate(boolean))',
  `time` int(10) UNSIGNED NOT NULL DEFAULT 0 COMMENT '有效时间',
  `attribute` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL DEFAULT '' COMMENT '属性',
  `name` char(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL DEFAULT '' COMMENT '称号名字',
  `description` char(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL DEFAULT '' COMMENT '称号描述',
  PRIMARY KEY (`title_id`) USING BTREE
) ENGINE = InnoDB CHARACTER SET = utf8mb4 COLLATE = utf8mb4_unicode_ci COMMENT = '称号配置表' ROW_FORMAT = Dynamic;

-- ----------------------------
-- Records of title_data
-- ----------------------------
INSERT INTO `title_data` VALUES (101, 1, 'false', 'false', 0, '[{3,30},{4,40}]', '小试牛刀', 'VIP1可获得');
INSERT INTO `title_data` VALUES (102, 1, 'false', 'false', 0, '[{3,30},{4,40}]', '有钱任性', 'VIP2可获得');
INSERT INTO `title_data` VALUES (103, 1, 'false', 'false', 0, '[{3,30},{4,40}]', '一掷千金', 'VIP3可获得');
INSERT INTO `title_data` VALUES (104, 1, 'false', 'false', 0, '[{3,30},{4,40}]', '腰缠万贯', 'VIP4可获得');
INSERT INTO `title_data` VALUES (105, 1, 'false', 'false', 0, '[{3,30},{4,40}]', '挥金如土', 'VIP5可获得');
INSERT INTO `title_data` VALUES (106, 1, 'false', 'false', 0, '[{3,30},{4,40}]', '富甲天下', 'VIP6可获得');
INSERT INTO `title_data` VALUES (107, 1, 'false', 'false', 0, '[{3,30},{4,40}]', '富可敌国', 'VIP7可获得');
INSERT INTO `title_data` VALUES (108, 1, 'false', 'false', 0, '[{3,30},{4,40}]', '人生巅峰', 'VIP8可获得');
INSERT INTO `title_data` VALUES (109, 1, 'false', 'false', 0, '[{3,30},{4,40}]', '至尊王者', 'VIP9可获得');
INSERT INTO `title_data` VALUES (110, 1, 'false', 'false', 0, '[{3,30},{4,40}]', '高手对决', 'VIP0可获得');
INSERT INTO `title_data` VALUES (201, 2, 'true', 'false', 0, '[{6,60},{7,70}]', '武艺超群', '开服冲榜活动获取');
INSERT INTO `title_data` VALUES (202, 2, 'true', 'false', 0, '[{6,60},{7,70}]', '出神入化', '开服冲榜活动获取');
INSERT INTO `title_data` VALUES (203, 2, 'true', 'false', 0, '[{6,60},{7,70}]', '仙武主宰', '开服冲榜活动获取');
INSERT INTO `title_data` VALUES (204, 2, 'true', 'false', 0, '[{6,60},{7,70}]', '锻造大师', '开服冲榜活动获取');
INSERT INTO `title_data` VALUES (205, 2, 'true', 'false', 0, '[{6,60},{7,70}]', '黑暗主宰', '开服冲榜活动获取');
INSERT INTO `title_data` VALUES (206, 2, 'true', 'false', 0, '[{6,60},{7,70}]', '聚魂先锋', '开服冲榜活动获取');
INSERT INTO `title_data` VALUES (207, 2, 'true', 'false', 0, '[{6,60},{7,70}]', '全职高手', '开服冲榜活动获取');
INSERT INTO `title_data` VALUES (208, 2, 'true', 'false', 0, '[{6,60},{7,70}]', '人中之龙', '开服冲榜活动获取');
INSERT INTO `title_data` VALUES (209, 2, 'true', 'false', 0, '[{6,60},{7,70}]', '勇者无畏', '开服冲榜活动获取');
INSERT INTO `title_data` VALUES (210, 2, 'true', 'false', 0, '[{6,60},{7,70}]', '称霸天下', '开服冲榜活动获取');
INSERT INTO `title_data` VALUES (10010, 3, 'false', 'true', 0, '[{5,50}]', '归隐山林', '充值获取');

-- ----------------------------
-- Table structure for title_log
-- ----------------------------
DROP TABLE IF EXISTS `title_log`;
CREATE TABLE `title_log`  (
  `id` bigint(20) UNSIGNED NOT NULL AUTO_INCREMENT COMMENT 'ID',
  `role_id` bigint(20) UNSIGNED NOT NULL DEFAULT 0 COMMENT '角色ID',
  `title_id` int(10) UNSIGNED NOT NULL DEFAULT 0 COMMENT '称号ID',
  `from` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL DEFAULT '' COMMENT '来源',
  `time` int(10) UNSIGNED NOT NULL DEFAULT 0 COMMENT '时间',
  PRIMARY KEY (`id`) USING BTREE
) ENGINE = InnoDB AUTO_INCREMENT = 1 CHARACTER SET = utf8mb4 COLLATE = utf8mb4_unicode_ci COMMENT = '称号日志表' ROW_FORMAT = Dynamic;

-- ----------------------------
-- Records of title_log
-- ----------------------------

-- ----------------------------
-- Table structure for vip
-- ----------------------------
DROP TABLE IF EXISTS `vip`;
CREATE TABLE `vip`  (
  `role_id` bigint(20) UNSIGNED NOT NULL DEFAULT 0 COMMENT '角色id',
  `vip_level` tinyint(2) UNSIGNED NOT NULL DEFAULT 0 COMMENT 'vip等级',
  `exp` bigint(20) UNSIGNED NOT NULL DEFAULT 0 COMMENT 'vip经验',
  `expire_time` int(10) UNSIGNED NOT NULL DEFAULT 0 COMMENT '过期时间',
  PRIMARY KEY (`role_id`) USING BTREE
) ENGINE = InnoDB CHARACTER SET = utf8mb4 COLLATE = utf8mb4_general_ci COMMENT = '角色vip表' ROW_FORMAT = Dynamic;

-- ----------------------------
-- Records of vip
-- ----------------------------
INSERT INTO `vip` VALUES (1, 1, 0, 0);

-- ----------------------------
-- Table structure for vip_data
-- ----------------------------
DROP TABLE IF EXISTS `vip_data`;
CREATE TABLE `vip_data`  (
  `vip` tinyint(1) UNSIGNED NOT NULL DEFAULT 0 COMMENT 'VIP等级',
  `exp` bigint(20) UNSIGNED NOT NULL DEFAULT 0 COMMENT '经验',
  PRIMARY KEY (`vip`) USING BTREE
) ENGINE = InnoDB CHARACTER SET = utf8mb4 COLLATE = utf8mb4_general_ci COMMENT = 'vip配置表' ROW_FORMAT = Dynamic;

-- ----------------------------
-- Records of vip_data
-- ----------------------------
INSERT INTO `vip_data` VALUES (1, 6);
INSERT INTO `vip_data` VALUES (2, 30);
INSERT INTO `vip_data` VALUES (3, 100);
INSERT INTO `vip_data` VALUES (4, 150);
INSERT INTO `vip_data` VALUES (5, 300);
INSERT INTO `vip_data` VALUES (6, 600);
INSERT INTO `vip_data` VALUES (7, 1000);
INSERT INTO `vip_data` VALUES (8, 2000);
INSERT INTO `vip_data` VALUES (9, 3000);
INSERT INTO `vip_data` VALUES (10, 5000);
INSERT INTO `vip_data` VALUES (11, 10000);
INSERT INTO `vip_data` VALUES (12, 30000);
INSERT INTO `vip_data` VALUES (13, 60000);
INSERT INTO `vip_data` VALUES (14, 100000);
INSERT INTO `vip_data` VALUES (15, 200000);

SET FOREIGN_KEY_CHECKS = 1;

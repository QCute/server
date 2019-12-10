/*
 Navicat Premium Data Transfer

 Source Server         : localhost
 Source Server Type    : MariaDB
 Source Server Version : 100406
 Source Host           : localhost:3306
 Source Schema         : main

 Target Server Type    : MariaDB
 Target Server Version : 100406
 File Encoding         : 65001

 Date: 10/12/2019 22:36:11
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
  `hide_time` int(10) UNSIGNED NOT NULL DEFAULT 0 COMMENT '图标消失时间(时间戳)',
  `clean_time` int(10) UNSIGNED NOT NULL DEFAULT 0 COMMENT '数据清理时间(时间戳)',
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
INSERT INTO `activity_data` VALUES (1, 1, '', 1, 1, 0, 0, 0, 0, 0, 0, 0, 8, 10, 10, 0, 0, 3, 7, '活动名', 'activity.icon', 'activity', '活动描述');
INSERT INTO `activity_data` VALUES (2, 2, '', 1, 1, 0, 0, 0, 0, 0, 0, 0, 8, 10, 10, 0, 0, 3, 7, '活动名', 'activity.icon', 'activity', '活动描述');
INSERT INTO `activity_data` VALUES (3, 4, '', 1, 1, 0, 0, 0, 0, 0, 0, 0, 8, 10, 10, 0, 0, 3, 7, '活动名', 'activity.icon', 'activity', '活动描述');

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
  `sliver_rate` varchar(0) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL DEFAULT '' COMMENT '银币倍率(default(0))',
  `copper_rate` varchar(0) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL DEFAULT '' COMMENT '铜币倍率(default(0))',
  `coin_rate` varchar(0) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL DEFAULT '' COMMENT '硬币倍率(default(0))',
  `exp_rate` varchar(0) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL DEFAULT '' COMMENT '经验倍率(default(0))',
  PRIMARY KEY (`role_id`) USING BTREE
) ENGINE = InnoDB CHARACTER SET = utf8mb4 COLLATE = utf8mb4_general_ci COMMENT = '角色资产表' ROW_FORMAT = Dynamic;

-- ----------------------------
-- Records of asset
-- ----------------------------
INSERT INTO `asset` VALUES (1, 0, 0, 0, 0, 0, '', '', '', '');

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
  `role_server_id` smallint(5) UNSIGNED NOT NULL DEFAULT 0 COMMENT '出价者服ID',
  `timer` varchar(0) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL DEFAULT '' COMMENT '定时器',
  `flag` varchar(0) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL DEFAULT '' COMMENT '标识(flag)',
  PRIMARY KEY (`unique_id`) USING BTREE
) ENGINE = InnoDB AUTO_INCREMENT = 1 CHARACTER SET = utf8mb4 COLLATE = utf8mb4_general_ci COMMENT = '拍卖信息表' ROW_FORMAT = Compact;

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
) ENGINE = InnoDB CHARACTER SET = utf8mb4 COLLATE = utf8mb4_general_ci COMMENT = '拍卖配置表' ROW_FORMAT = Compact;

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
  `bidder_id` bigint(20) UNSIGNED NOT NULL DEFAULT 0 COMMENT '获得者ID',
  `bidder_name` char(16) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL DEFAULT '' COMMENT '获得者名字',
  `bidder_server_id` int(10) UNSIGNED NOT NULL DEFAULT 0 COMMENT '获得者服ID',
  `time` int(10) UNSIGNED NOT NULL DEFAULT 0 COMMENT '时间',
  `daily_time` int(10) UNSIGNED NOT NULL DEFAULT 0 COMMENT '零点时间',
  PRIMARY KEY (`id`) USING BTREE
) ENGINE = InnoDB AUTO_INCREMENT = 1 CHARACTER SET = utf8mb4 COLLATE = utf8mb4_general_ci COMMENT = '拍卖日志表' ROW_FORMAT = Dynamic;

-- ----------------------------
-- Table structure for buff
-- ----------------------------
DROP TABLE IF EXISTS `buff`;
CREATE TABLE `buff`  (
  `role_id` bigint(20) UNSIGNED NOT NULL DEFAULT 0 COMMENT '角色ID(select)',
  `buff_id` int(10) UNSIGNED NOT NULL DEFAULT 0 COMMENT '状态增益ID',
  `start_time` int(10) UNSIGNED NOT NULL DEFAULT 0 COMMENT '生效时间',
  `expire_time` int(10) UNSIGNED NOT NULL DEFAULT 0 COMMENT '结束时间',
  `overlap` int(10) UNSIGNED NOT NULL DEFAULT 1 COMMENT '叠加数',
  `flag` varchar(0) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL DEFAULT '' COMMENT '标识(flag)',
  PRIMARY KEY (`role_id`, `buff_id`) USING BTREE
) ENGINE = InnoDB CHARACTER SET = utf8mb4 COLLATE = utf8mb4_general_ci COMMENT = '角色buff表' ROW_FORMAT = Dynamic;

-- ----------------------------
-- Records of buff
-- ----------------------------
INSERT INTO `buff` VALUES (1, 1, 1568649600, 0, 1, '');

-- ----------------------------
-- Table structure for buff_data
-- ----------------------------
DROP TABLE IF EXISTS `buff_data`;
CREATE TABLE `buff_data`  (
  `buff_id` int(10) UNSIGNED NOT NULL DEFAULT 0 COMMENT '增益状态(Buff)ID',
  `group_id` int(10) UNSIGNED NOT NULL DEFAULT 0 COMMENT '组ID',
  `type` int(10) UNSIGNED NOT NULL DEFAULT 0 COMMENT '类型',
  `time` int(10) UNSIGNED NOT NULL DEFAULT 0 COMMENT '有效时间',
  `name` char(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL DEFAULT '' COMMENT '名字',
  `effect` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL DEFAULT '' COMMENT '效果',
  `temporary` tinyint(1) UNSIGNED NOT NULL DEFAULT 0 COMMENT '是否临时的(切地图失效)',
  `overlap_type` tinyint(1) UNSIGNED NOT NULL DEFAULT 0 COMMENT '叠加类型(0:不叠加/1:时间/2:数值/3:都叠加)',
  `replace_buffs` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL DEFAULT '' COMMENT '替换Buffs',
  `description` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL DEFAULT '' COMMENT '描述',
  PRIMARY KEY (`buff_id`) USING BTREE
) ENGINE = InnoDB CHARACTER SET = utf8mb4 COLLATE = utf8mb4_general_ci COMMENT = 'buff配置表' ROW_FORMAT = Dynamic;

-- ----------------------------
-- Records of buff_data
-- ----------------------------
INSERT INTO `buff_data` VALUES (1, 1, 1, 0, '扣血', '[5]', 0, 0, '', '');

-- ----------------------------
-- Table structure for count
-- ----------------------------
DROP TABLE IF EXISTS `count`;
CREATE TABLE `count`  (
  `role_id` bigint(20) UNSIGNED NOT NULL DEFAULT 0 COMMENT '角色ID(select)',
  `type` int(64) UNSIGNED NOT NULL DEFAULT 0 COMMENT '计数类型',
  `today_number` int(10) UNSIGNED NOT NULL DEFAULT 0 COMMENT '今天数量',
  `total_number` int(10) UNSIGNED NOT NULL DEFAULT 0 COMMENT '总数',
  `flag` varchar(0) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL DEFAULT '' COMMENT '标识(flag)',
  PRIMARY KEY (`role_id`, `type`) USING BTREE,
  INDEX `type`(`type`) USING BTREE
) ENGINE = InnoDB CHARACTER SET = utf8mb4 COLLATE = utf8mb4_general_ci COMMENT = '角色计数表' ROW_FORMAT = Dynamic;

-- ----------------------------
-- Records of count
-- ----------------------------
INSERT INTO `count` VALUES (1, 1, 0, 0, '');
INSERT INTO `count` VALUES (1, 2, 0, 0, '');
INSERT INTO `count` VALUES (1, 3, 0, 0, '');

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
INSERT INTO `effect_data` VALUES (1, 'active', 'battle', '', '10000', 'add', 'Self', 'Hurt', '', 'Hurt * 1.8', 0, '', '增加80%伤害');
INSERT INTO `effect_data` VALUES (2, 'active', 'battle', '', '10000', 'add', 'Self', 'Hurt', '', 'Hurt * 1.5', 0, '', '增加50%伤害');
INSERT INTO `effect_data` VALUES (3, 'active', 'battle', 'SelfAttribute.hp == 0', '10000', 'add', 'Self', 'Attribute', 'hp', 'Self.Attribute.total_hp', 0, '', '死亡立即复活');
INSERT INTO `effect_data` VALUES (4, 'active', 'battle', '', '10000', 'set', 'Self', 'Attribute', 'vertigo', '0', 0, '', '清除眩晕');
INSERT INTO `effect_data` VALUES (5, 'active', 'battle', '', '10000', 'reduce', 'Rival', 'Attribute', 'hp', 'Rival.Attribute.total_hp * (50 / 10000)', 5, '', '每秒扣血，总血量万分之50');
INSERT INTO `effect_data` VALUES (6, 'active', 'battle', '', '10000', 'add', 'Mate', 'Attribute', 'attack', 'Mate.Attribute.attack * 1.5', 3, '', '增加队友攻击150%');
INSERT INTO `effect_data` VALUES (7, 'active', 'battle', '', '10000', 'add', 'Mate', 'Attribute', 'defense', 'Mate.Attribute.defense * 1.5', 3, '', '增加队友防御150%');
INSERT INTO `effect_data` VALUES (8, 'active', 'battle', '', '10000', 'add', 'Self', 'Buff', '', '[1]', 0, '', '添加Buff');
INSERT INTO `effect_data` VALUES (9, 'active', 'user', '', '10000', 'add', 'Self', 'Asset', 'copper_rate', '1.5', 0, '', '增加150%铜币');
INSERT INTO `effect_data` VALUES (10, 'active', 'user', '', '10000', 'add', 'Self', 'Asset', 'exp_rate', '2', 0, '', '增加200%经验');

-- ----------------------------
-- Table structure for error_code_data
-- ----------------------------
DROP TABLE IF EXISTS `error_code_data`;
CREATE TABLE `error_code_data`  (
  `protocol` int(255) UNSIGNED NOT NULL DEFAULT 0 COMMENT '协议',
  `code` int(255) UNSIGNED NOT NULL DEFAULT 0 COMMENT '错误码',
  `content` char(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL DEFAULT '' COMMENT '内容',
  PRIMARY KEY (`protocol`, `code`) USING BTREE
) ENGINE = InnoDB CHARACTER SET = utf8mb4 COLLATE = utf8mb4_general_ci COMMENT = '错误码配置表' ROW_FORMAT = Dynamic;

-- ----------------------------
-- Records of error_code_data
-- ----------------------------
INSERT INTO `error_code_data` VALUES (10001, 0, 'failed');
INSERT INTO `error_code_data` VALUES (10002, 2, 'length');
INSERT INTO `error_code_data` VALUES (10002, 3, 'asn1');
INSERT INTO `error_code_data` VALUES (10002, 4, 'sensitive');
INSERT INTO `error_code_data` VALUES (10002, 5, 'duplicate');

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
) ENGINE = InnoDB CHARACTER SET = utf8mb4 COLLATE = utf8mb4_general_ci COMMENT = '角色好友表' ROW_FORMAT = Compact;

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
  `leader_id` char(0) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL DEFAULT '' COMMENT '会长id',
  `leader_name` char(0) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL DEFAULT '' COMMENT '会长名字',
  `flag` varchar(0) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL DEFAULT '' COMMENT '标识(flag)',
  PRIMARY KEY (`guild_id`) USING BTREE
) ENGINE = InnoDB AUTO_INCREMENT = 6 CHARACTER SET = utf8mb4 COLLATE = utf8mb4_general_ci COMMENT = '公会表' ROW_FORMAT = Dynamic;

-- ----------------------------
-- Records of guild
-- ----------------------------
INSERT INTO `guild` VALUES (1, 1, 0, 0, 0, '1', '', '', '', '');
INSERT INTO `guild` VALUES (2, 2, 0, 0, 0, '2', '', '', '', '');
INSERT INTO `guild` VALUES (3, 3, 0, 0, 0, '3', '', '', '', '');
INSERT INTO `guild` VALUES (4, 4, 0, 0, 0, '4', '', '', '', '');
INSERT INTO `guild` VALUES (5, 5, 0, 0, 0, '5', '', '', '', '');

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
  `vip_level` varchar(0) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL DEFAULT '' COMMENT 'VIP等级(join(`vip`.`vip_level`)/default(0))',
  `flag` varchar(0) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL DEFAULT '' COMMENT '标识(flag)',
  PRIMARY KEY (`guild_id`, `role_id`) USING BTREE,
  INDEX `role_id`(`role_id`) USING BTREE
) ENGINE = InnoDB CHARACTER SET = utf8mb4 COLLATE = utf8mb4_general_ci COMMENT = '公会申请表' ROW_FORMAT = Dynamic;

-- ----------------------------
-- Records of guild_apply
-- ----------------------------
INSERT INTO `guild_apply` VALUES (1, 3, 0, '', '', '', '', '', '');
INSERT INTO `guild_apply` VALUES (1, 4, 0, '', '', '', '', '', '');
INSERT INTO `guild_apply` VALUES (2, 3, 0, '', '', '', '', '', '');
INSERT INTO `guild_apply` VALUES (2, 5, 0, '', '', '', '', '', '');

-- ----------------------------
-- Table structure for guild_role
-- ----------------------------
DROP TABLE IF EXISTS `guild_role`;
CREATE TABLE `guild_role`  (
  `guild_id` int(20) UNSIGNED NOT NULL DEFAULT 0 COMMENT '公会ID(join(`guild`.`guild_id`)/(delete_guild_id))',
  `role_id` int(20) UNSIGNED NOT NULL DEFAULT 0 COMMENT '角色ID(join(`role`.`role_id`)/join(`vip`.`role_id`)/(delete_role_id))',
  `job` tinyint(1) UNSIGNED NOT NULL DEFAULT 0 COMMENT '职位',
  `join_time` int(10) UNSIGNED NOT NULL DEFAULT 0 COMMENT '加入时间',
  `leave_time` int(10) UNSIGNED NOT NULL DEFAULT 0 COMMENT '离开时间',
  `guild_name` char(0) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL DEFAULT '' COMMENT '帮派名(join(`guild`.`guild_name`))',
  `role_name` char(0) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL DEFAULT '' COMMENT '角色名(join(`role`.`role_name`))',
  `sex` varchar(0) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL DEFAULT '' COMMENT '性别(join(`role`.`sex`)/default(0))',
  `classes` varchar(0) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL DEFAULT '' COMMENT '职业(join(`role`.`classes`)/default(0))',
  `vip_level` varchar(0) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL DEFAULT '' COMMENT 'VIP等级(join(`vip`.`vip_level`)/default(0))',
  `flag` varchar(0) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL DEFAULT '' COMMENT '标识(flag)',
  PRIMARY KEY (`guild_id`, `role_id`) USING BTREE,
  INDEX `role_id`(`role_id`) USING BTREE
) ENGINE = InnoDB CHARACTER SET = utf8mb4 COLLATE = utf8mb4_general_ci COMMENT = '公会角色表' ROW_FORMAT = Dynamic;

-- ----------------------------
-- Records of guild_role
-- ----------------------------
INSERT INTO `guild_role` VALUES (1, 1, 1, 0, 0, '', '', '', '', '', '');
INSERT INTO `guild_role` VALUES (2, 2, 1, 0, 0, '', '', '', '', '', '');

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
  `bind` tinyint(1) UNSIGNED NOT NULL DEFAULT 0 COMMENT '绑定',
  `expire_time` int(10) UNSIGNED NOT NULL DEFAULT 0 COMMENT '过期时间',
  `flag` varchar(0) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL DEFAULT '' COMMENT '标识(flag)',
  PRIMARY KEY (`unique_id`) USING BTREE,
  INDEX `role_id`(`role_id`) USING BTREE
) ENGINE = InnoDB AUTO_INCREMENT = 7 CHARACTER SET = utf8mb4 COLLATE = utf8mb4_general_ci COMMENT = '角色物品表' ROW_FORMAT = Dynamic;

-- ----------------------------
-- Records of item
-- ----------------------------
INSERT INTO `item` VALUES (1, 1, 1, 1, 1000, 0, 0, '');
INSERT INTO `item` VALUES (2, 1, 1, 1, 6, 0, 0, '');
INSERT INTO `item` VALUES (3, 1, 2, 1, 9, 0, 0, '');
INSERT INTO `item` VALUES (4, 1, 3, 1, 10, 0, 0, '');
INSERT INTO `item` VALUES (5, 1, 1, 1, 1000, 0, 0, '');
INSERT INTO `item` VALUES (6, 1, 1, 1, 1000, 0, 0, '');

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
  `daily_time` int(10) UNSIGNED NOT NULL DEFAULT 0 COMMENT '零点时间',
  PRIMARY KEY (`id`) USING BTREE
) ENGINE = InnoDB AUTO_INCREMENT = 1 CHARACTER SET = utf8mb4 COLLATE = utf8mb4_general_ci COMMENT = '物品消费日志表' ROW_FORMAT = Dynamic;

-- ----------------------------
-- Table structure for item_data
-- ----------------------------
DROP TABLE IF EXISTS `item_data`;
CREATE TABLE `item_data`  (
  `item_id` int(10) UNSIGNED NOT NULL DEFAULT 0 COMMENT '物品id',
  `type` tinyint(3) UNSIGNED NOT NULL DEFAULT 0 COMMENT '类型',
  `asset` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL DEFAULT '' COMMENT '资产类型',
  `overlap` int(10) UNSIGNED NOT NULL DEFAULT 1 COMMENT '叠加数',
  `category` int(10) UNSIGNED NOT NULL DEFAULT 0 COMMENT '分类ID',
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
INSERT INTO `item_data` VALUES (1, 1, '', 1000, 0, 0, '', 0, 'rust', 'file_type_rust.svg', '');
INSERT INTO `item_data` VALUES (2, 1, '', 100, 0, 0, '', 0, 'erlang', 'file_type_erlang.svg', '');
INSERT INTO `item_data` VALUES (3, 1, '', 10, 0, 0, '', 0, 'php', 'file_type_php.svg', '');
INSERT INTO `item_data` VALUES (4, 2, '', 1, 0, 0, '', 0, 'lua', 'file_type_lua.svg', '');
INSERT INTO `item_data` VALUES (5, 2, '', 1, 0, 0, '', 0, 'js', 'file_type_js.svg', '');
INSERT INTO `item_data` VALUES (6, 2, '', 1, 0, 0, '', 0, 'html', 'file_type_html.svg', '');
INSERT INTO `item_data` VALUES (7, 2, '', 1, 0, 0, '', 0, 'css', 'file_type_css.svg', '');
INSERT INTO `item_data` VALUES (100001, 101, 'gold', 1, 0, 0, '', 0, 'gold', 'file_type_gold.svg', '');
INSERT INTO `item_data` VALUES (100002, 102, 'sliver', 1, 0, 0, '', 0, 'silver', 'file_type_sliver.svg', '');
INSERT INTO `item_data` VALUES (100003, 103, 'copper', 1, 0, 0, '', 0, 'copper', 'file_type_copper.svg', '');
INSERT INTO `item_data` VALUES (100004, 104, 'exp', 1, 0, 0, '', 0, 'exp', 'file_type_exp.svg', '');
INSERT INTO `item_data` VALUES (100005, 105, 'coin', 1, 0, 0, '', 0, 'coin', 'file_type_coin.svg', '');

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
  `daily_time` int(10) UNSIGNED NOT NULL DEFAULT 0 COMMENT '零点时间',
  PRIMARY KEY (`id`) USING BTREE
) ENGINE = InnoDB AUTO_INCREMENT = 1 CHARACTER SET = utf8mb4 COLLATE = utf8mb4_general_ci COMMENT = '物品产出日志表' ROW_FORMAT = Dynamic;

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
INSERT INTO `key_data` VALUES ('a8ardtxylvfqyyle', 1);
INSERT INTO `key_data` VALUES ('a9gcubps2yjctchv', 1);
INSERT INTO `key_data` VALUES ('aimayurrmcetztvn', 1);
INSERT INTO `key_data` VALUES ('b9brmtyp7chmwbdg', 1);
INSERT INTO `key_data` VALUES ('bbkmoihr3bczzues', 1);
INSERT INTO `key_data` VALUES ('bj0u3lpcaljkoj9o', 1);
INSERT INTO `key_data` VALUES ('blpiv3prm2eyhm3i', 1);
INSERT INTO `key_data` VALUES ('bmyvplydzdekmmwa', 1);
INSERT INTO `key_data` VALUES ('bseibu7ktqxff0tr', 1);
INSERT INTO `key_data` VALUES ('btsyfs0e2qiwnxzl', 1);
INSERT INTO `key_data` VALUES ('bvf71zwlasxmwhui', 1);
INSERT INTO `key_data` VALUES ('d0oxyasntgy6h1c7', 1);
INSERT INTO `key_data` VALUES ('dcav9xjajyrmacr9', 1);
INSERT INTO `key_data` VALUES ('ddupvxnqt22ynztc', 1);
INSERT INTO `key_data` VALUES ('diyqjmruhkkqavbg', 1);
INSERT INTO `key_data` VALUES ('dk3oxg9ckxwtv7sz', 1);
INSERT INTO `key_data` VALUES ('dschlymnv9paze3h', 1);
INSERT INTO `key_data` VALUES ('duj8w3t76afhmwfh', 1);
INSERT INTO `key_data` VALUES ('egc48o42frtkmawa', 1);
INSERT INTO `key_data` VALUES ('escg8lh4w1nkklj6', 1);
INSERT INTO `key_data` VALUES ('ewmdjvgdyns6cerw', 1);
INSERT INTO `key_data` VALUES ('eyanxqdursyon5eu', 1);
INSERT INTO `key_data` VALUES ('fmhilfvdwrtyovcb', 1);
INSERT INTO `key_data` VALUES ('ftj1vxepq4aa61tu', 1);
INSERT INTO `key_data` VALUES ('ftlguwfcpjy7nvnz', 1);
INSERT INTO `key_data` VALUES ('fzxjljsp4shp11ui', 1);
INSERT INTO `key_data` VALUES ('giapm1ezfkm0qzf1', 1);
INSERT INTO `key_data` VALUES ('gnrzcrwoimsrvlho', 1);
INSERT INTO `key_data` VALUES ('hmm4gpbb3kedqtbj', 1);
INSERT INTO `key_data` VALUES ('hovfvlem6ga4ltjg', 1);
INSERT INTO `key_data` VALUES ('hq7wjxyo5ce5zelw', 1);
INSERT INTO `key_data` VALUES ('hzdyhdnfzixcovhn', 1);
INSERT INTO `key_data` VALUES ('i1fmmw3odaxc5vmm', 1);
INSERT INTO `key_data` VALUES ('i59f46xstfsdpkid', 1);
INSERT INTO `key_data` VALUES ('ibh4qifm0hkxocrm', 1);
INSERT INTO `key_data` VALUES ('ibuxbhgmypzkr1go', 1);
INSERT INTO `key_data` VALUES ('ikvdulsft0lgpwcq', 1);
INSERT INTO `key_data` VALUES ('ipsnnqua4vh9arxd', 1);
INSERT INTO `key_data` VALUES ('iqp9vm1yfe9j9xm2', 1);
INSERT INTO `key_data` VALUES ('iy9vloj60g7dktye', 1);
INSERT INTO `key_data` VALUES ('jfvdenplsigotc6o', 1);
INSERT INTO `key_data` VALUES ('jsews1izondoeanq', 1);
INSERT INTO `key_data` VALUES ('ks5kpfsehjmwgjec', 1);
INSERT INTO `key_data` VALUES ('l0kifxz1ynjjc7vd', 1);
INSERT INTO `key_data` VALUES ('lfcjjenkdr0kjvi4', 1);
INSERT INTO `key_data` VALUES ('lradcq8nk99eidqi', 1);
INSERT INTO `key_data` VALUES ('luyn2mvuye94s0xr', 1);
INSERT INTO `key_data` VALUES ('lxru2igur17ydwks', 1);
INSERT INTO `key_data` VALUES ('mdyyxxhi34hpgsu5', 1);
INSERT INTO `key_data` VALUES ('mjiboc4uo6gcgoe6', 1);
INSERT INTO `key_data` VALUES ('mphmbgupepgwb8hi', 1);
INSERT INTO `key_data` VALUES ('mr60cidrgp0jeqtk', 1);
INSERT INTO `key_data` VALUES ('muzhboayjuttx3rs', 1);
INSERT INTO `key_data` VALUES ('nfi6lbfjbf6avd6l', 1);
INSERT INTO `key_data` VALUES ('ohvmmz4tvwkzidct', 1);
INSERT INTO `key_data` VALUES ('onqon5wfmbhjnj25', 1);
INSERT INTO `key_data` VALUES ('oxcelzrjaa8hlhza', 1);
INSERT INTO `key_data` VALUES ('pzprlhmckv690724', 1);
INSERT INTO `key_data` VALUES ('q23u1geceq7tjooo', 1);
INSERT INTO `key_data` VALUES ('qmjlpysp0zk2qsh6', 1);
INSERT INTO `key_data` VALUES ('qoidlze7wkldptwk', 1);
INSERT INTO `key_data` VALUES ('recl4rbowaywydbm', 1);
INSERT INTO `key_data` VALUES ('riqfwonvkyvb3rdz', 1);
INSERT INTO `key_data` VALUES ('rkqjcvyxjoeo3c6r', 1);
INSERT INTO `key_data` VALUES ('rr9wktgmzjr1akwn', 1);
INSERT INTO `key_data` VALUES ('rz3xa1hf2gclppk4', 1);
INSERT INTO `key_data` VALUES ('s83q3iqfxisjycfs', 1);
INSERT INTO `key_data` VALUES ('sassaih7jmxc2p4b', 1);
INSERT INTO `key_data` VALUES ('slxxs41pvtenebnv', 1);
INSERT INTO `key_data` VALUES ('sosllotfz3oqvw07', 1);
INSERT INTO `key_data` VALUES ('sslxxqbqdqnylrl0', 1);
INSERT INTO `key_data` VALUES ('stg9ve5nud8lizdw', 1);
INSERT INTO `key_data` VALUES ('swo35omvohqooz7l', 1);
INSERT INTO `key_data` VALUES ('tbdae69wkbxfvd4i', 1);
INSERT INTO `key_data` VALUES ('tdle5in5dqgp7hi9', 1);
INSERT INTO `key_data` VALUES ('tk7bcpmlrsdxfuea', 1);
INSERT INTO `key_data` VALUES ('tmzobf6x92y8qhxc', 1);
INSERT INTO `key_data` VALUES ('ucbjquwfe4lkidqi', 1);
INSERT INTO `key_data` VALUES ('ucbyzqgjwa2p2hjq', 1);
INSERT INTO `key_data` VALUES ('uorfmj7uerhvgqt7', 1);
INSERT INTO `key_data` VALUES ('urymqqgb6sk2zkgy', 1);
INSERT INTO `key_data` VALUES ('uyj1rfj8broypvpv', 1);
INSERT INTO `key_data` VALUES ('v28l34eblqbeczbp', 1);
INSERT INTO `key_data` VALUES ('v3j8q0n1hmub1jgt', 1);
INSERT INTO `key_data` VALUES ('vbzed8v34kmzkud6', 1);
INSERT INTO `key_data` VALUES ('vff2s6fpbaqyjhqe', 1);
INSERT INTO `key_data` VALUES ('vkdjbjxnptc2ny40', 1);
INSERT INTO `key_data` VALUES ('vkzk9aeq4mgfd8eo', 1);
INSERT INTO `key_data` VALUES ('w9crdeddwd9gjfow', 1);
INSERT INTO `key_data` VALUES ('wzysei8qu8vimk9p', 1);
INSERT INTO `key_data` VALUES ('x1qghnustruqgan0', 1);
INSERT INTO `key_data` VALUES ('xdkub7elcqigleua', 1);
INSERT INTO `key_data` VALUES ('y1frissbf8iyuanu', 1);
INSERT INTO `key_data` VALUES ('y7ortn34luquyrsh', 1);
INSERT INTO `key_data` VALUES ('yulmaiiqawn6edp3', 1);
INSERT INTO `key_data` VALUES ('yvf6ta32ea4w7sar', 1);
INSERT INTO `key_data` VALUES ('zdn9pe9cacpplqqq', 1);
INSERT INTO `key_data` VALUES ('zdq2uskl5abvi55d', 1);
INSERT INTO `key_data` VALUES ('zm9dypapkssxepff', 1);
INSERT INTO `key_data` VALUES ('ztxxcn817qkxspls', 1);

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
  `login_time` int(10) UNSIGNED NOT NULL DEFAULT 0 COMMENT '登录时间',
  `logout_time` int(10) UNSIGNED NOT NULL DEFAULT 0 COMMENT '登出时间',
  `online_time` int(10) UNSIGNED NOT NULL DEFAULT 0 COMMENT '在线时间',
  `ip` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL DEFAULT '' COMMENT '登录IP',
  `device_id` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL DEFAULT '' COMMENT '设备ID',
  PRIMARY KEY (`id`) USING BTREE
) ENGINE = InnoDB AUTO_INCREMENT = 12 CHARACTER SET = utf8mb4 COLLATE = utf8mb4_general_ci COMMENT = '角色登录日志' ROW_FORMAT = Dynamic;

-- ----------------------------
-- Records of login_log
-- ----------------------------
INSERT INTO `login_log` VALUES (1, 1, 1567070969, 1567070972, 3, '192.168.1.149', '0cbe44690da8d7436039d107befa4e3074ed259c');
INSERT INTO `login_log` VALUES (2, 1, 1567070996, 1567070972, 0, '192.168.1.149', '0cbe44690da8d7436039d107befa4e3074ed259c');
INSERT INTO `login_log` VALUES (3, 2, 1567071337, 1567071338, 1, '192.168.1.218', '8d7db2299475809bb282ab41fb09b54fdfa16d4a');
INSERT INTO `login_log` VALUES (4, 2, 1567071346, 1567072728, 1382, '192.168.1.218', '8d7db2299475809bb282ab41fb09b54fdfa16d4a');
INSERT INTO `login_log` VALUES (5, 2, 1567072752, 1567073136, 384, '192.168.1.218', '8d7db2299475809bb282ab41fb09b54fdfa16d4a');
INSERT INTO `login_log` VALUES (6, 3, 1567071571, 1567071575, 4, '192.168.1.218', '8d7db2299475809bb282ab41fb09b54fdfa16d4a');
INSERT INTO `login_log` VALUES (7, 3, 1567071584, 1567072738, 1154, '192.168.1.218', '8d7db2299475809bb282ab41fb09b54fdfa16d4a');
INSERT INTO `login_log` VALUES (8, 3, 1567072738, 1567072743, 5, '192.168.1.218', '8d7db2299475809bb282ab41fb09b54fdfa16d4a');
INSERT INTO `login_log` VALUES (9, 3, 1567072743, 1567073136, 393, '192.168.1.218', '8d7db2299475809bb282ab41fb09b54fdfa16d4a');
INSERT INTO `login_log` VALUES (10, 4, 1567072133, 1567072136, 3, '192.168.1.99', '0cbe44690da8d7436039d107befa4e3074ed259c');
INSERT INTO `login_log` VALUES (11, 4, 1567072147, 1567127825, 55678, '192.168.1.99', '0cbe44690da8d7436039d107befa4e3074ed259c');

-- ----------------------------
-- Table structure for mail
-- ----------------------------
DROP TABLE IF EXISTS `mail`;
CREATE TABLE `mail`  (
  `mail_id` bigint(20) UNSIGNED NOT NULL AUTO_INCREMENT COMMENT '邮件ID',
  `sender_id` bigint(20) UNSIGNED NOT NULL DEFAULT 0 COMMENT '发送者',
  `sender_nick` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL DEFAULT '' COMMENT '发送者昵称',
  `receiver_id` bigint(20) UNSIGNED NOT NULL DEFAULT 0 COMMENT '接收者(select)',
  `receiver_nick` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL DEFAULT '' COMMENT '接受者昵称',
  `receive_time` int(10) UNSIGNED NOT NULL DEFAULT 0 COMMENT '接收时间',
  `is_read` tinyint(1) UNSIGNED NOT NULL DEFAULT 0 COMMENT '是否已经读取(update_read)',
  `read_time` int(10) UNSIGNED NOT NULL DEFAULT 0 COMMENT '读取时间(update_read)',
  `expire_time` int(10) UNSIGNED NOT NULL DEFAULT 0 COMMENT '过期时间',
  `is_receive_attachment` tinyint(1) UNSIGNED NOT NULL DEFAULT 0 COMMENT '是否领取附件(update_receive)',
  `receive_attachment_time` int(10) UNSIGNED NOT NULL DEFAULT 0 COMMENT '领取附件时间(update_receive)',
  `from` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL DEFAULT '' COMMENT '来源',
  `title` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL DEFAULT '' COMMENT '标题',
  `content` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL DEFAULT '' COMMENT '内容',
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
  `map_id` int(11) UNSIGNED NOT NULL DEFAULT 0,
  `type` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL DEFAULT '' COMMENT '广播类型(validate(`map_type`))',
  `reconnect` tinyint(1) UNSIGNED NOT NULL DEFAULT 0 COMMENT '是否重连',
  `monster` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL DEFAULT '' COMMENT '随地图启动的怪物',
  `rank_key` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL DEFAULT '' COMMENT '榜键类型(validate(`map_rank_key`))',
  `rank_value` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL DEFAULT '' COMMENT '榜值类型(validate(`map_rank_value`))',
  `rank_mode` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL DEFAULT '' COMMENT '榜模式(validate(`map_rank_mode`))',
  `enter_points` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL DEFAULT '' COMMENT '进入点',
  `pk_mode` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL DEFAULT '' COMMENT 'PK模式',
  `enter_script` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL DEFAULT '' COMMENT '进入脚本',
  `relive_script` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL DEFAULT '' COMMENT '复活脚本',
  `leave_script` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL DEFAULT '' COMMENT '离开脚本',
  PRIMARY KEY (`map_id`) USING BTREE
) ENGINE = InnoDB CHARACTER SET = utf8mb4 COLLATE = utf8mb4_general_ci COMMENT = '地图配置表' ROW_FORMAT = Dynamic;

-- ----------------------------
-- Table structure for monster_data
-- ----------------------------
DROP TABLE IF EXISTS `monster_data`;
CREATE TABLE `monster_data`  (
  `monster_id` int(10) UNSIGNED NOT NULL DEFAULT 0 COMMENT '怪物ID',
  `group_id` int(10) UNSIGNED NOT NULL DEFAULT 0 COMMENT '组ID',
  `monster_name` char(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL DEFAULT '' COMMENT '怪物名称',
  `type` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL DEFAULT '' COMMENT '怪物类型',
  `level` int(10) UNSIGNED NOT NULL DEFAULT 0 COMMENT '等级',
  `hp` bigint(20) UNSIGNED NOT NULL DEFAULT 0 COMMENT '血量',
  `map_id` int(10) UNSIGNED NOT NULL DEFAULT 0 COMMENT '地图ID',
  `camp` tinyint(1) UNSIGNED NOT NULL DEFAULT 0 COMMENT '阵营',
  `range` int(10) UNSIGNED NOT NULL DEFAULT 0 COMMENT '攻击距离',
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
INSERT INTO `monster_data` VALUES (1, 10, 'active', 'monster', 1, 100, 0, 1, 100, 0, 'active', '[role]', '[5]', '[{20,10}]', '[{100005,100}]');
INSERT INTO `monster_data` VALUES (2, 20, 'passive', 'guard', 1, 200, 0, 1, 200, 0, 'passive', '[enemy]', '', '[{40,10}]', '[{100005,200}]');
INSERT INTO `monster_data` VALUES (3, 30, 'movable', 'boom', 1, 300, 0, 1, 300, 0, 'movable', '', '', '[{60,10}]', '[{100005,300}]');
INSERT INTO `monster_data` VALUES (4, 40, 'fix', 'statue', 1, 400, 0, 1, 400, 0, 'fix', '', '', '[{80,10}]', '');
INSERT INTO `monster_data` VALUES (5, 50, 'act', 'boom', 1, 500, 0, 1, 500, 0, 'fix', '[enemy]', '', '[{100,10}]', '');
INSERT INTO `monster_data` VALUES (6, 60, 'boom', 'boss', 1, 600, 0, 1, 600, 0, 'active', '[{monster, 20}, {monster, 50}, role]', '', '[{120,10}]', '[{100005,600}]');

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
  `server_no` int(10) UNSIGNED NOT NULL DEFAULT 0 COMMENT '游戏服编号',
  `server_type` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL DEFAULT '' COMMENT '服务器类型',
  `center_node` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL DEFAULT '' COMMENT '中央服节点',
  `center_name` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL DEFAULT '' COMMENT '中央服名',
  `center_host` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL DEFAULT '' COMMENT '中央服域名',
  `center_ip` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL DEFAULT '' COMMENT '中央服IP',
  `center_port` int(10) UNSIGNED NOT NULL DEFAULT 0 COMMENT '中央服端口',
  `center_no` int(10) UNSIGNED NOT NULL DEFAULT 0 COMMENT '中央服编号',
  PRIMARY KEY (`server_node`) USING BTREE
) ENGINE = InnoDB CHARACTER SET = utf8mb4 COLLATE = utf8mb4_general_ci COMMENT = '节点配置表' ROW_FORMAT = Dynamic;

-- ----------------------------
-- Records of node_data
-- ----------------------------
INSERT INTO `node_data` VALUES ('center', '小跨服', '', '', 0, 1001, 'center', '', '', '', '', 0, 0);
INSERT INTO `node_data` VALUES ('dev', '开发服', '', '', 0, 4, 'local', '', '小跨服', '', '', 0, 0);
INSERT INTO `node_data` VALUES ('main', '主测服', '', '', 0, 1, 'local', 'center', '小跨服', '', '', 0, 0);
INSERT INTO `node_data` VALUES ('stable', '稳定服', '', '', 0, 2, 'local', 'center', '小跨服', '', '', 0, 0);
INSERT INTO `node_data` VALUES ('test', '测试服', '', '', 0, 3, 'local', 'center', '小跨服', '', '', 0, 0);
INSERT INTO `node_data` VALUES ('world', '大世界', '', '', 0, 0, 'world', '', '', '', '', 0, 0);

-- ----------------------------
-- Table structure for online_log
-- ----------------------------
DROP TABLE IF EXISTS `online_log`;
CREATE TABLE `online_log`  (
  `time` int(10) UNSIGNED NOT NULL DEFAULT 0 COMMENT '当前时间',
  `hour` int(10) UNSIGNED NOT NULL DEFAULT 0 COMMENT '当前小时',
  `all` int(10) UNSIGNED NOT NULL DEFAULT 0 COMMENT '全部',
  `online` int(10) UNSIGNED NOT NULL DEFAULT 0 COMMENT '在线',
  `hosting` int(10) UNSIGNED NOT NULL DEFAULT 0 COMMENT '挂机',
  PRIMARY KEY (`time`) USING BTREE
) ENGINE = InnoDB CHARACTER SET = utf8mb4 COLLATE = utf8mb4_general_ci COMMENT = '在线统计日志' ROW_FORMAT = Dynamic;

-- ----------------------------
-- Records of online_log
-- ----------------------------
INSERT INTO `online_log` VALUES (1575089721, 12, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575089781, 12, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575089910, 12, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575089970, 12, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575090030, 13, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575090090, 13, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575090150, 13, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575090210, 13, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575090270, 13, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575090330, 13, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575090390, 13, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575090579, 13, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575090639, 13, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575090699, 13, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575090759, 13, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575090819, 13, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575090879, 13, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575090939, 13, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575090999, 13, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575091059, 13, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575091119, 13, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575091179, 13, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575091239, 13, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575091299, 13, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575091359, 13, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575091419, 13, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575091479, 13, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575091539, 13, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575091599, 13, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575091659, 13, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575091719, 13, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575091779, 13, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575091839, 13, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575091899, 13, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575091959, 13, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575092019, 13, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575092079, 13, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575092139, 13, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575092199, 13, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575092259, 13, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575092319, 13, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575092379, 13, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575092439, 13, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575092499, 13, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575092559, 13, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575092619, 13, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575094713, 14, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575094773, 14, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575094833, 14, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575094893, 14, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575094953, 14, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575095013, 14, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575095073, 14, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575095133, 14, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575095193, 14, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575095253, 14, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575095313, 14, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575095373, 14, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575095433, 14, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575095493, 14, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575095553, 14, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575095613, 14, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575095673, 14, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575095733, 14, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575095793, 14, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575095853, 14, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575095913, 14, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575095973, 14, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575096033, 14, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575096093, 14, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575096153, 14, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575096213, 14, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575096273, 14, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575096333, 14, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575096393, 14, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575096453, 14, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575096513, 14, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575096573, 14, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575096633, 14, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575096693, 14, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575096753, 14, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575096813, 14, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575096873, 14, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575096933, 14, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575096993, 14, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575097053, 14, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575097113, 14, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575097173, 14, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575097233, 15, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575097293, 15, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575097353, 15, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575097413, 15, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575097473, 15, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575097533, 15, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575097593, 15, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575097653, 15, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575097713, 15, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575097773, 15, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575097833, 15, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575097893, 15, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575097953, 15, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575098013, 15, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575098073, 15, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575098133, 15, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575098193, 15, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575098253, 15, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575098313, 15, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575098373, 15, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575098433, 15, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575098493, 15, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575098553, 15, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575098613, 15, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575098673, 15, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575098733, 15, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575098793, 15, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575098853, 15, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575098913, 15, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575098973, 15, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575099033, 15, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575099093, 15, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575099153, 15, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575099213, 15, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575099273, 15, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575099333, 15, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575099393, 15, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575099453, 15, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575099513, 15, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575099573, 15, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575099633, 15, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575099693, 15, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575099753, 15, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575099813, 15, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575099873, 15, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575099933, 15, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575099993, 15, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575100053, 15, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575100113, 15, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575100173, 15, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575100233, 15, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575100293, 15, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575100353, 15, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575100413, 15, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575100473, 15, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575100533, 15, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575100593, 15, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575100653, 15, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575100713, 15, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575100773, 15, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575100833, 16, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575100893, 16, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575100953, 16, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575101013, 16, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575101073, 16, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575101133, 16, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575101193, 16, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575101253, 16, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575101313, 16, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575101373, 16, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575101433, 16, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575101493, 16, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575101553, 16, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575101613, 16, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575101673, 16, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575101733, 16, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575101793, 16, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575101853, 16, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575101913, 16, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575101973, 16, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575102033, 16, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575102093, 16, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575102153, 16, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575102213, 16, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575102273, 16, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575102333, 16, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575102393, 16, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575102453, 16, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575102513, 16, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575102573, 16, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575102633, 16, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575102693, 16, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575102753, 16, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575102813, 16, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575102873, 16, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575102933, 16, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575102993, 16, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575103053, 16, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575103113, 16, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575103173, 16, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575103233, 16, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575103293, 16, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575103353, 16, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575103413, 16, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575103473, 16, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575103533, 16, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575103593, 16, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575103653, 16, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575103713, 16, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575103773, 16, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575103833, 16, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575103893, 16, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575103953, 16, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575104013, 16, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575104073, 16, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575104133, 16, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575104193, 16, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575104253, 16, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575104313, 16, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575104373, 16, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575104433, 17, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575104493, 17, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575104553, 17, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575104613, 17, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575104673, 17, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575104733, 17, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575104793, 17, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575104853, 17, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575104913, 17, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575104973, 17, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575105033, 17, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575105093, 17, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575105153, 17, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575105213, 17, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575105273, 17, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575105333, 17, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575105393, 17, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575105453, 17, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575105513, 17, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575105573, 17, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575105633, 17, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575105693, 17, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575105753, 17, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575105813, 17, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575105873, 17, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575105933, 17, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575105993, 17, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575106053, 17, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575106113, 17, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575106173, 17, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575106233, 17, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575106293, 17, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575106353, 17, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575106413, 17, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575106473, 17, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575106533, 17, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575106593, 17, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575106653, 17, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575106713, 17, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575106773, 17, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575106833, 17, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575106893, 17, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575106953, 17, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575107013, 17, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575107073, 17, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575107133, 17, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575107193, 17, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575107253, 17, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575107313, 17, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575107373, 17, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575107433, 17, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575107493, 17, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575107553, 17, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575107613, 17, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575107673, 17, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575107733, 17, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575107793, 17, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575107853, 17, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575107913, 17, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575107973, 17, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575108033, 18, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575108093, 18, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575108153, 18, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575108213, 18, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575108273, 18, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575108333, 18, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575108393, 18, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575108453, 18, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575108513, 18, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575108573, 18, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575108633, 18, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575108693, 18, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575108753, 18, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575108813, 18, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575108873, 18, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575108933, 18, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575108993, 18, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575109053, 18, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575109113, 18, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575109173, 18, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575109233, 18, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575109293, 18, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575109353, 18, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575109413, 18, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575109473, 18, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575109533, 18, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575109593, 18, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575109653, 18, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575109713, 18, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575109773, 18, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575109833, 18, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575109893, 18, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575109953, 18, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575110013, 18, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575110073, 18, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575110133, 18, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575110193, 18, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575110253, 18, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575110313, 18, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575110373, 18, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575110433, 18, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575110493, 18, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575110553, 18, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575110613, 18, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575110673, 18, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575110733, 18, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575110793, 18, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575110853, 18, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575110913, 18, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575110973, 18, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575111033, 18, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575111093, 18, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575111153, 18, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575111213, 18, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575111273, 18, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575111333, 18, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575111393, 18, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575111453, 18, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575111513, 18, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575111573, 18, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575111633, 19, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575111693, 19, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575111753, 19, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575111813, 19, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575111873, 19, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575111933, 19, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575111993, 19, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575112053, 19, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575112113, 19, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575112173, 19, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575112233, 19, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575112293, 19, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575112353, 19, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575112413, 19, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575112473, 19, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575112533, 19, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575112593, 19, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575112653, 19, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575112713, 19, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575112773, 19, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575112833, 19, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575112893, 19, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575112953, 19, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575113013, 19, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575113073, 19, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575113133, 19, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575113193, 19, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575113253, 19, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575113313, 19, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575113373, 19, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575113433, 19, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575113493, 19, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575113553, 19, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575113613, 19, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575113673, 19, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575113733, 19, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575113793, 19, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575113853, 19, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575113913, 19, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575113973, 19, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575114033, 19, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575114093, 19, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575114153, 19, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575114213, 19, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575114273, 19, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575114333, 19, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575114393, 19, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575114453, 19, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575114513, 19, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575114573, 19, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575114633, 19, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575114693, 19, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575114753, 19, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575114813, 19, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575114873, 19, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575114933, 19, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575114993, 19, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575115053, 19, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575115113, 19, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575115173, 19, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575115233, 20, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575115293, 20, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575115353, 20, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575115413, 20, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575115473, 20, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575115533, 20, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575115593, 20, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575115653, 20, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575115713, 20, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575115773, 20, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575115833, 20, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575115893, 20, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575115953, 20, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575116013, 20, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575116073, 20, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575116133, 20, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575116193, 20, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575116253, 20, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575116313, 20, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575116373, 20, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575116433, 20, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575116493, 20, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575116553, 20, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575116613, 20, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575116673, 20, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575116733, 20, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575116793, 20, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575116853, 20, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575116913, 20, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575116973, 20, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575117033, 20, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575117093, 20, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575117153, 20, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575117213, 20, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575117273, 20, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575117333, 20, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575117393, 20, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575117453, 20, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575117513, 20, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575117573, 20, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575117633, 20, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575117693, 20, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575117753, 20, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575117813, 20, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575117873, 20, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575117933, 20, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575117993, 20, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575118053, 20, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575118113, 20, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575118173, 20, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575118233, 20, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575118293, 20, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575118353, 20, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575118413, 20, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575118473, 20, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575118533, 20, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575118593, 20, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575118653, 20, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575118713, 20, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575118773, 20, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575118833, 21, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575118893, 21, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575118953, 21, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575119013, 21, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575119073, 21, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575119133, 21, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575119193, 21, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575119253, 21, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575119313, 21, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575119373, 21, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575119433, 21, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575119493, 21, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575119553, 21, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575119613, 21, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575119673, 21, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575119733, 21, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575119793, 21, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575119853, 21, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575119913, 21, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575119973, 21, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575120033, 21, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575120093, 21, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575120153, 21, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575120213, 21, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575120273, 21, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575120333, 21, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575120393, 21, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575120453, 21, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575120513, 21, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575120573, 21, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575120633, 21, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575120693, 21, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575120753, 21, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575120813, 21, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575120873, 21, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575120933, 21, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575120993, 21, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575121053, 21, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575121113, 21, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575121173, 21, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575121233, 21, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575121293, 21, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575121353, 21, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575121413, 21, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575121473, 21, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575121533, 21, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575121593, 21, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575121653, 21, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575121713, 21, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575121773, 21, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575121833, 21, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575121893, 21, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575121953, 21, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575122013, 21, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575122073, 21, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575122133, 21, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575122193, 21, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575122253, 21, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575122313, 21, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575122373, 21, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575122433, 22, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575122493, 22, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575122553, 22, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575122613, 22, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575122673, 22, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575122733, 22, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575122793, 22, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575122853, 22, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575122913, 22, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575122973, 22, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575123033, 22, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575123093, 22, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575123153, 22, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575123213, 22, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575123273, 22, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575123333, 22, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575123393, 22, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575123453, 22, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575123513, 22, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575123573, 22, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575123633, 22, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575123693, 22, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575123753, 22, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575123813, 22, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575123873, 22, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575123933, 22, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575123993, 22, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575124053, 22, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575124113, 22, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575981045, 20, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575981105, 20, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575981165, 20, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575981225, 20, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575981285, 20, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575981345, 20, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575981405, 20, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575981465, 20, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575982082, 20, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575982383, 20, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575982443, 20, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575982503, 20, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575982563, 20, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575982623, 20, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575982683, 20, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575982743, 20, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575982803, 21, 0, 0, 0);
INSERT INTO `online_log` VALUES (1575982863, 21, 0, 0, 0);

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
INSERT INTO `parameter_data` VALUES ('chat_level', '10', '聊天开放等级');
INSERT INTO `parameter_data` VALUES ('friend_level', '30', '好友开放等级');
INSERT INTO `parameter_data` VALUES ('friend_number', '50', '好友上限');
INSERT INTO `parameter_data` VALUES ('guild_create_cd', '86400', '公会创建冷却时间');
INSERT INTO `parameter_data` VALUES ('guild_join_cd', '86400', '公会加入冷却时间');
INSERT INTO `parameter_data` VALUES ('login_cd', '180', '登录时间间隔');
INSERT INTO `parameter_data` VALUES ('{guild_create, 1}', '[{level, 10}, {vip, 0}, {gold, 0}]', '创建一级公会条件');
INSERT INTO `parameter_data` VALUES ('{guild_create, 2}', '[{level, 50}, {vip, 1}, {gold, 100}]', '创建二级公会条件');
INSERT INTO `parameter_data` VALUES ('{guild_create, 3}', '[{level, 100}, {vip, 3}, {gold, 500}]', '创建三级公会条件');
INSERT INTO `parameter_data` VALUES ('{guild_member_limit, 0}', '50', '公会人员数');
INSERT INTO `parameter_data` VALUES ('{guild_member_limit, 1}', '60', '公会人员数');
INSERT INTO `parameter_data` VALUES ('{guild_member_limit, 2}', '70', '公会人员数');
INSERT INTO `parameter_data` VALUES ('{guild_member_limit, 3}', '80', '公会人员数');
INSERT INTO `parameter_data` VALUES ('{guild_member_limit, 4}', '90', '公会人员数');
INSERT INTO `parameter_data` VALUES ('{guild_member_limit, 5}', '100', '公会人员数');

-- ----------------------------
-- Table structure for quest
-- ----------------------------
DROP TABLE IF EXISTS `quest`;
CREATE TABLE `quest`  (
  `role_id` bigint(20) UNSIGNED NOT NULL DEFAULT 0 COMMENT '角色ID(select)',
  `quest_id` int(10) UNSIGNED NOT NULL DEFAULT 0 COMMENT '任务ID',
  `group_id` int(10) UNSIGNED NOT NULL DEFAULT 0 COMMENT '组ID',
  `event` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL DEFAULT '' COMMENT '事件',
  `target` int(10) UNSIGNED NOT NULL DEFAULT 0 COMMENT '目标',
  `number` int(10) UNSIGNED NOT NULL DEFAULT 0 COMMENT '数量',
  `compare` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL DEFAULT '' COMMENT '比较',
  `award` tinyint(1) UNSIGNED NOT NULL DEFAULT 0 COMMENT '是否领取奖励',
  `flag` varchar(0) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL DEFAULT '' COMMENT '额外(flag)',
  PRIMARY KEY (`role_id`, `quest_id`) USING BTREE
) ENGINE = InnoDB CHARACTER SET = utf8mb4 COLLATE = utf8mb4_general_ci COMMENT = '角色任务表' ROW_FORMAT = Dynamic;

-- ----------------------------
-- Records of quest
-- ----------------------------
INSERT INTO `quest` VALUES (1, 1, 1, 'event_kill_monster', 0, 3, 'gte', 0, '');

-- ----------------------------
-- Table structure for quest_data
-- ----------------------------
DROP TABLE IF EXISTS `quest_data`;
CREATE TABLE `quest_data`  (
  `quest_id` int(10) UNSIGNED NOT NULL DEFAULT 0 COMMENT '任务ID',
  `group_id` int(10) UNSIGNED NOT NULL DEFAULT 0 COMMENT '组ID',
  `pre_id` int(10) UNSIGNED NOT NULL DEFAULT 0 COMMENT '前置任务',
  `next_id` int(10) UNSIGNED NOT NULL DEFAULT 0 COMMENT '后置任务',
  `event` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL DEFAULT '' COMMENT '事件(validate(event))',
  `target` int(10) UNSIGNED NOT NULL DEFAULT 0 COMMENT '目标',
  `number` int(10) UNSIGNED NOT NULL DEFAULT 0 COMMENT '数量',
  `compare` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL DEFAULT '' COMMENT '比较模式(validate(compare))',
  `condition` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL DEFAULT '' COMMENT '条件',
  `award` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL DEFAULT '' COMMENT '奖励',
  `title` char(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL DEFAULT '' COMMENT '标题',
  `content` char(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL DEFAULT '' COMMENT '内容',
  `description` char(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL DEFAULT '' COMMENT '描述',
  PRIMARY KEY (`quest_id`) USING BTREE
) ENGINE = InnoDB CHARACTER SET = utf8mb4 COLLATE = utf8mb4_general_ci COMMENT = '任务配置表' ROW_FORMAT = Dynamic;

-- ----------------------------
-- Records of quest_data
-- ----------------------------
INSERT INTO `quest_data` VALUES (1, 1, 0, 2, 'event_kill_monster', 0, 3, 'nc', '', '[{1,1}]', '', '', '');
INSERT INTO `quest_data` VALUES (2, 1, 1, 3, 'event_level_upgrade', 5, 1, 'ge', '[{copper, 100}]', '[{1,10}]', '', '', '');
INSERT INTO `quest_data` VALUES (3, 1, 2, 4, 'event_pass_dungeon', 100001, 1, 'ge', '[{level, 10}]', '[{1,100}]', '', '', '');
INSERT INTO `quest_data` VALUES (4, 1, 3, 5, 'event_shop_buy', 100001, 1, 'eq', '', '[{1,1000}]', '', '', '');
INSERT INTO `quest_data` VALUES (5, 1, 4, 0, 'event_guild_join', 0, 1, 'nc', '', '[{1,1000}]', '', '', '');

-- ----------------------------
-- Table structure for quest_log
-- ----------------------------
DROP TABLE IF EXISTS `quest_log`;
CREATE TABLE `quest_log`  (
  `id` int(10) UNSIGNED NOT NULL AUTO_INCREMENT COMMENT 'ID',
  `role_id` int(10) UNSIGNED NOT NULL DEFAULT 0 COMMENT '角色ID',
  `quest_id` int(10) UNSIGNED NOT NULL DEFAULT 0 COMMENT '任务ID',
  `time` int(10) UNSIGNED NOT NULL DEFAULT 0 COMMENT '时间',
  `daily_time` int(10) UNSIGNED NOT NULL DEFAULT 0 COMMENT '零点时间',
  PRIMARY KEY (`id`) USING BTREE
) ENGINE = InnoDB AUTO_INCREMENT = 1 CHARACTER SET = utf8mb4 COLLATE = utf8mb4_general_ci COMMENT = '任务日志表' ROW_FORMAT = Dynamic;

-- ----------------------------
-- Table structure for rank
-- ----------------------------
DROP TABLE IF EXISTS `rank`;
CREATE TABLE `rank`  (
  `type` tinyint(2) UNSIGNED NOT NULL DEFAULT 0 COMMENT '类型(select)',
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
  `unique_id` int(11) NOT NULL AUTO_INCREMENT COMMENT '唯一ID',
  `recharge_id` int(11) UNSIGNED NOT NULL DEFAULT 0 COMMENT '充值ID',
  `order_id` varchar(150) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL DEFAULT '' COMMENT '平台订单号',
  `account_id` varchar(150) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL DEFAULT '' COMMENT '平台账号ID',
  `role_id` int(11) NOT NULL DEFAULT 0 COMMENT '玩家ID',
  `role_name` varchar(50) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL DEFAULT '' COMMENT '玩家名称',
  `money` decimal(12, 2) NOT NULL DEFAULT 0 COMMENT '充值金额',
  `gold` int(11) UNSIGNED NOT NULL DEFAULT 0 COMMENT '金币',
  `gift_gold` int(11) UNSIGNED NOT NULL DEFAULT 0 COMMENT '赠送金币',
  `time` int(11) UNSIGNED NOT NULL DEFAULT 0 COMMENT '订单时间',
  `receive_time` int(11) UNSIGNED NOT NULL DEFAULT 0 COMMENT '领取时间',
  `status` tinyint(1) UNSIGNED NOT NULL DEFAULT 0 COMMENT '状态(0:未取/1:已领取)',
  `channel_id` varchar(100) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL DEFAULT '' COMMENT '渠道ID',
  `server_id` varchar(100) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL DEFAULT '' COMMENT '区服ID',
  PRIMARY KEY (`unique_id`) USING BTREE,
  UNIQUE INDEX `order_id`(`order_id`) USING BTREE,
  INDEX `role_id`(`role_id`, `status`) USING BTREE,
  INDEX `channel_id`(`channel_id`) USING BTREE
) ENGINE = InnoDB AUTO_INCREMENT = 3 CHARACTER SET = utf8mb4 COLLATE = utf8mb4_general_ci COMMENT = '角色充值订单表' ROW_FORMAT = Dynamic;

-- ----------------------------
-- Records of recharge
-- ----------------------------
INSERT INTO `recharge` VALUES (1, 206, 'gm_1_1567132558078458', '', 1, '大大寒杰', 648.00, 6480, 0, 1567132558, 1567132558, 1, '', '');
INSERT INTO `recharge` VALUES (2, 206, 'gm_1_1567132558235592', '', 1, '大大寒杰', 648.00, 6480, 0, 1567132558, 1567132558, 1, '', '');

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
  `server_id` smallint(5) UNSIGNED NOT NULL DEFAULT 0 COMMENT '服ID',
  `channel_id` int(10) UNSIGNED NOT NULL DEFAULT 0 COMMENT '渠道ID',
  `map` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL DEFAULT '' COMMENT '地图',
  `device_id` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL DEFAULT '' COMMENT '设备ID',
  `device_type` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL DEFAULT '' COMMENT '设备类型',
  `mac` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL DEFAULT '' COMMENT 'Mac地址',
  PRIMARY KEY (`role_id`) USING BTREE,
  UNIQUE INDEX `role_name`(`role_name`) USING BTREE,
  UNIQUE INDEX `account`(`account`) USING BTREE,
  INDEX `server_id`(`server_id`) USING BTREE
) ENGINE = InnoDB AUTO_INCREMENT = 7 CHARACTER SET = utf8mb4 COLLATE = utf8mb4_general_ci COMMENT = '角色信息表' ROW_FORMAT = Dynamic;

-- ----------------------------
-- Records of role
-- ----------------------------
INSERT INTO `role` VALUES (1, '1', '1', 1, 1, 1, 1, 100, 100, 100, 0, 1, 1, '', '', '', '');
INSERT INTO `role` VALUES (2, '2', '2', 2, 1, 2, 2, 100, 100, 100, 0, 1, 1, '', '', '', '');
INSERT INTO `role` VALUES (3, '3', '3', 2, 1, 1, 3, 100, 100, 100, 0, 1, 1, '', '', '', '');
INSERT INTO `role` VALUES (4, '4', '4', 3, 1, 2, 4, 100, 100, 100, 0, 1, 1, '', '', '', '');
INSERT INTO `role` VALUES (5, '5', '5', 3, 1, 1, 5, 100, 100, 100, 0, 1, 1, '', '', '', '');
INSERT INTO `role` VALUES (6, '6', '6', 3, 1, 2, 6, 100, 100, 100, 0, 1, 1, '', '', '', '');

-- ----------------------------
-- Table structure for role_log
-- ----------------------------
DROP TABLE IF EXISTS `role_log`;
CREATE TABLE `role_log`  (
  `id` bigint(20) UNSIGNED NOT NULL AUTO_INCREMENT COMMENT 'ID',
  `role_id` bigint(20) UNSIGNED NOT NULL DEFAULT 0 COMMENT '角色ID',
  `exp` bigint(20) UNSIGNED NOT NULL DEFAULT 0 COMMENT '经验',
  `time` int(10) UNSIGNED NOT NULL DEFAULT 0 COMMENT '时间',
  `daily_time` int(10) UNSIGNED NOT NULL DEFAULT 0 COMMENT '零点时间',
  PRIMARY KEY (`id`) USING BTREE
) ENGINE = InnoDB AUTO_INCREMENT = 1 CHARACTER SET = utf8mb4 COLLATE = utf8mb4_general_ci COMMENT = '角色日志表' ROW_FORMAT = Dynamic;

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
INSERT INTO `sensitive_word_data` VALUES ('010-86515990');
INSERT INTO `sensitive_word_data` VALUES ('139116797372');
INSERT INTO `sensitive_word_data` VALUES ('310事件');
INSERT INTO `sensitive_word_data` VALUES ('3D轮盘');
INSERT INTO `sensitive_word_data` VALUES ('628事件');
INSERT INTO `sensitive_word_data` VALUES ('64式手枪出售');
INSERT INTO `sensitive_word_data` VALUES ('69');
INSERT INTO `sensitive_word_data` VALUES ('69式');
INSERT INTO `sensitive_word_data` VALUES ('6合');
INSERT INTO `sensitive_word_data` VALUES ('6合彩');
INSERT INTO `sensitive_word_data` VALUES ('75事件');
INSERT INTO `sensitive_word_data` VALUES ('85863252');
INSERT INTO `sensitive_word_data` VALUES ('92式手枪出售');
INSERT INTO `sensitive_word_data` VALUES ('985660286');
INSERT INTO `sensitive_word_data` VALUES ('9ping');
INSERT INTO `sensitive_word_data` VALUES ('9浅1深');
INSERT INTO `sensitive_word_data` VALUES ('9评');
INSERT INTO `sensitive_word_data` VALUES ('adrenaline');
INSERT INTO `sensitive_word_data` VALUES ('after-play');
INSERT INTO `sensitive_word_data` VALUES ('androst');
INSERT INTO `sensitive_word_data` VALUES ('ATOM弹制造');
INSERT INTO `sensitive_word_data` VALUES ('a片');
INSERT INTO `sensitive_word_data` VALUES ('BB弹 ');
INSERT INTO `sensitive_word_data` VALUES ('BB枪 ');
INSERT INTO `sensitive_word_data` VALUES ('benzodiazepines');
INSERT INTO `sensitive_word_data` VALUES ('BJ');
INSERT INTO `sensitive_word_data` VALUES ('bjork');
INSERT INTO `sensitive_word_data` VALUES ('Blow Job');
INSERT INTO `sensitive_word_data` VALUES ('C4塑胶炸药');
INSERT INTO `sensitive_word_data` VALUES ('C4炸药');
INSERT INTO `sensitive_word_data` VALUES ('C4炸药的成分');
INSERT INTO `sensitive_word_data` VALUES ('cannabis');
INSERT INTO `sensitive_word_data` VALUES ('cao你');
INSERT INTO `sensitive_word_data` VALUES ('CAR SEX');
INSERT INTO `sensitive_word_data` VALUES ('chairmanmao');
INSERT INTO `sensitive_word_data` VALUES ('chengdu');
INSERT INTO `sensitive_word_data` VALUES ('cocain');
INSERT INTO `sensitive_word_data` VALUES ('communistparty');
INSERT INTO `sensitive_word_data` VALUES ('dajiyuan');
INSERT INTO `sensitive_word_data` VALUES ('diacetylmorphine');
INSERT INTO `sensitive_word_data` VALUES ('diamorphine');
INSERT INTO `sensitive_word_data` VALUES ('DIY原子弹');
INSERT INTO `sensitive_word_data` VALUES ('DIY核弹');
INSERT INTO `sensitive_word_data` VALUES ('erythropoietin');
INSERT INTO `sensitive_word_data` VALUES ('FaLun ');
INSERT INTO `sensitive_word_data` VALUES ('falundafa ');
INSERT INTO `sensitive_word_data` VALUES ('falungong');
INSERT INTO `sensitive_word_data` VALUES ('fangong ');
INSERT INTO `sensitive_word_data` VALUES ('fanhua ');
INSERT INTO `sensitive_word_data` VALUES ('fa轮');
INSERT INTO `sensitive_word_data` VALUES ('Fa轮功 ');
INSERT INTO `sensitive_word_data` VALUES ('flg');
INSERT INTO `sensitive_word_data` VALUES ('fl功');
INSERT INTO `sensitive_word_data` VALUES ('FL大法 ');
INSERT INTO `sensitive_word_data` VALUES ('fuck');
INSERT INTO `sensitive_word_data` VALUES ('gcd');
INSERT INTO `sensitive_word_data` VALUES ('gc党');
INSERT INTO `sensitive_word_data` VALUES ('gongchandang');
INSERT INTO `sensitive_word_data` VALUES ('gong党');
INSERT INTO `sensitive_word_data` VALUES ('gong和');
INSERT INTO `sensitive_word_data` VALUES ('g产');
INSERT INTO `sensitive_word_data` VALUES ('g匪');
INSERT INTO `sensitive_word_data` VALUES ('G点');
INSERT INTO `sensitive_word_data` VALUES ('G点高潮');
INSERT INTO `sensitive_word_data` VALUES ('h1n1');
INSERT INTO `sensitive_word_data` VALUES ('heroin');
INSERT INTO `sensitive_word_data` VALUES ('hjt');
INSERT INTO `sensitive_word_data` VALUES ('HUANGJU');
INSERT INTO `sensitive_word_data` VALUES ('HUANG菊');
INSERT INTO `sensitive_word_data` VALUES ('hujintao');
INSERT INTO `sensitive_word_data` VALUES ('huzhuxi');
INSERT INTO `sensitive_word_data` VALUES ('h图 ');
INSERT INTO `sensitive_word_data` VALUES ('h漫');
INSERT INTO `sensitive_word_data` VALUES ('jiuping');
INSERT INTO `sensitive_word_data` VALUES ('jq的来');
INSERT INTO `sensitive_word_data` VALUES ('jzm');
INSERT INTO `sensitive_word_data` VALUES ('ketamine');
INSERT INTO `sensitive_word_data` VALUES ('KJ');
INSERT INTO `sensitive_word_data` VALUES ('k粉');
INSERT INTO `sensitive_word_data` VALUES ('LHZ ');
INSERT INTO `sensitive_word_data` VALUES ('liuhecai');
INSERT INTO `sensitive_word_data` VALUES ('mdma');
INSERT INTO `sensitive_word_data` VALUES ('morphine');
INSERT INTO `sensitive_word_data` VALUES ('narcotic');
INSERT INTO `sensitive_word_data` VALUES ('nowto');
INSERT INTO `sensitive_word_data` VALUES ('px项目');
INSERT INTO `sensitive_word_data` VALUES ('sb');
INSERT INTO `sensitive_word_data` VALUES ('SIM卡复制器 ');
INSERT INTO `sensitive_word_data` VALUES ('sm');
INSERT INTO `sensitive_word_data` VALUES ('SM用品');
INSERT INTO `sensitive_word_data` VALUES ('soufun');
INSERT INTO `sensitive_word_data` VALUES ('strychnine');
INSERT INTO `sensitive_word_data` VALUES ('suicide');
INSERT INTO `sensitive_word_data` VALUES ('tamoxifen');
INSERT INTO `sensitive_word_data` VALUES ('testosterone');
INSERT INTO `sensitive_word_data` VALUES ('titor');
INSERT INTO `sensitive_word_data` VALUES ('TNT 炸弹的制作');
INSERT INTO `sensitive_word_data` VALUES ('TNT炸弹制作');
INSERT INTO `sensitive_word_data` VALUES ('tnt炸药成分');
INSERT INTO `sensitive_word_data` VALUES ('tnt炸药配方');
INSERT INTO `sensitive_word_data` VALUES ('tuidang');
INSERT INTO `sensitive_word_data` VALUES ('wengan');
INSERT INTO `sensitive_word_data` VALUES ('weng安');
INSERT INTO `sensitive_word_data` VALUES ('wenjiabao');
INSERT INTO `sensitive_word_data` VALUES ('xiaoping ');
INSERT INTO `sensitive_word_data` VALUES ('xiao平');
INSERT INTO `sensitive_word_data` VALUES ('xjp');
INSERT INTO `sensitive_word_data` VALUES ('XX功 ');
INSERT INTO `sensitive_word_data` VALUES ('yangjia');
INSERT INTO `sensitive_word_data` VALUES ('yang佳');
INSERT INTO `sensitive_word_data` VALUES ('yanjiaoshequ');
INSERT INTO `sensitive_word_data` VALUES ('yuce');
INSERT INTO `sensitive_word_data` VALUES ('y佳');
INSERT INTO `sensitive_word_data` VALUES ('Y染色体');
INSERT INTO `sensitive_word_data` VALUES ('zedong');
INSERT INTO `sensitive_word_data` VALUES ('zemin');
INSERT INTO `sensitive_word_data` VALUES ('ze东');
INSERT INTO `sensitive_word_data` VALUES ('ze民');
INSERT INTO `sensitive_word_data` VALUES ('Zha药制造进程');
INSERT INTO `sensitive_word_data` VALUES ('zhengfu');
INSERT INTO `sensitive_word_data` VALUES ('zifen');
INSERT INTO `sensitive_word_data` VALUES ('zi杀');
INSERT INTO `sensitive_word_data` VALUES ('zz炸弹的制作方法');
INSERT INTO `sensitive_word_data` VALUES ('z东');
INSERT INTO `sensitive_word_data` VALUES ('「红烧兔子」大餐');
INSERT INTO `sensitive_word_data` VALUES ('【手枪出售】联系电话');
INSERT INTO `sensitive_word_data` VALUES ('一丝不挂');
INSERT INTO `sensitive_word_data` VALUES ('一个人的奥林匹克');
INSERT INTO `sensitive_word_data` VALUES ('一个妓女的年度总结报告');
INSERT INTO `sensitive_word_data` VALUES ('一个色导航');
INSERT INTO `sensitive_word_data` VALUES ('一个色导航最新地址');
INSERT INTO `sensitive_word_data` VALUES ('一中一台');
INSERT INTO `sensitive_word_data` VALUES ('一之瀬茜迅雷下载');
INSERT INTO `sensitive_word_data` VALUES ('一件代发');
INSERT INTO `sensitive_word_data` VALUES ('一元假硬币 qq');
INSERT INTO `sensitive_word_data` VALUES ('一元假硬币 货到付款');
INSERT INTO `sensitive_word_data` VALUES ('一元假钱出售 货到付款');
INSERT INTO `sensitive_word_data` VALUES ('一元硬币 \"仿真度超过HD90版\"');
INSERT INTO `sensitive_word_data` VALUES ('一元硬币 \"版本为最新版本\"');
INSERT INTO `sensitive_word_data` VALUES ('一元硬币 最新版本 钢芯镀镍');
INSERT INTO `sensitive_word_data` VALUES ('一克冰毒多少钱');
INSERT INTO `sensitive_word_data` VALUES ('一党');
INSERT INTO `sensitive_word_data` VALUES ('一党专制');
INSERT INTO `sensitive_word_data` VALUES ('一党专政');
INSERT INTO `sensitive_word_data` VALUES ('一党独裁');
INSERT INTO `sensitive_word_data` VALUES ('一军两策');
INSERT INTO `sensitive_word_data` VALUES ('一卡多号');
INSERT INTO `sensitive_word_data` VALUES ('一品楼VIP高级帐号');
INSERT INTO `sensitive_word_data` VALUES ('一四我');
INSERT INTO `sensitive_word_data` VALUES ('一四我周容重题工');
INSERT INTO `sensitive_word_data` VALUES ('一四我周容重题工无亮');
INSERT INTO `sensitive_word_data` VALUES ('一夜性');
INSERT INTO `sensitive_word_data` VALUES ('一夜性网');
INSERT INTO `sensitive_word_data` VALUES ('一夜情');
INSERT INTO `sensitive_word_data` VALUES ('一夜情俱乐部');
INSERT INTO `sensitive_word_data` VALUES ('一夜情迷奸');
INSERT INTO `sensitive_word_data` VALUES ('一夜激情 ');
INSERT INTO `sensitive_word_data` VALUES ('一字解特码');
INSERT INTO `sensitive_word_data` VALUES ('一小撮别');
INSERT INTO `sensitive_word_data` VALUES ('一本道');
INSERT INTO `sensitive_word_data` VALUES ('一本道ed2k');
INSERT INTO `sensitive_word_data` VALUES ('一码');
INSERT INTO `sensitive_word_data` VALUES ('一码中特');
INSERT INTO `sensitive_word_data` VALUES ('一级黄电视');
INSERT INTO `sensitive_word_data` VALUES ('一般炸药制作');
INSERT INTO `sensitive_word_data` VALUES ('一色鮎美种子');
INSERT INTO `sensitive_word_data` VALUES ('一诚');
INSERT INTO `sensitive_word_data` VALUES ('一通健康法 ');
INSERT INTO `sensitive_word_data` VALUES ('一通功');
INSERT INTO `sensitive_word_data` VALUES ('一黨');
INSERT INTO `sensitive_word_data` VALUES ('丁一平');
INSERT INTO `sensitive_word_data` VALUES ('丁人林');
INSERT INTO `sensitive_word_data` VALUES ('丁元');
INSERT INTO `sensitive_word_data` VALUES ('丁光训');
INSERT INTO `sensitive_word_data` VALUES ('丁关根');
INSERT INTO `sensitive_word_data` VALUES ('丁子霖');
INSERT INTO `sensitive_word_data` VALUES ('丁家班');
INSERT INTO `sensitive_word_data` VALUES ('丁度巴拉斯');
INSERT INTO `sensitive_word_data` VALUES ('丁石孙');
INSERT INTO `sensitive_word_data` VALUES ('丁香社');
INSERT INTO `sensitive_word_data` VALUES ('七宗罪');
INSERT INTO `sensitive_word_data` VALUES ('万人骚动');
INSERT INTO `sensitive_word_data` VALUES ('万学文');
INSERT INTO `sensitive_word_data` VALUES ('万学远');
INSERT INTO `sensitive_word_data` VALUES ('万晓东');
INSERT INTO `sensitive_word_data` VALUES ('万润南');
INSERT INTO `sensitive_word_data` VALUES ('万科');
INSERT INTO `sensitive_word_data` VALUES ('万科即将宣布破产');
INSERT INTO `sensitive_word_data` VALUES ('万科即将破产');
INSERT INTO `sensitive_word_data` VALUES ('万维读者论坛');
INSERT INTO `sensitive_word_data` VALUES ('万能钥匙');
INSERT INTO `sensitive_word_data` VALUES ('万言书');
INSERT INTO `sensitive_word_data` VALUES ('万达卫浴');
INSERT INTO `sensitive_word_data` VALUES ('万鄂湘');
INSERT INTO `sensitive_word_data` VALUES ('万里大造林');
INSERT INTO `sensitive_word_data` VALUES ('万钢');
INSERT INTO `sensitive_word_data` VALUES ('丈夫');
INSERT INTO `sensitive_word_data` VALUES ('丈母');
INSERT INTO `sensitive_word_data` VALUES ('丈母娘');
INSERT INTO `sensitive_word_data` VALUES ('三三九乘元功');
INSERT INTO `sensitive_word_data` VALUES ('三个代表');
INSERT INTO `sensitive_word_data` VALUES ('三二二攻台作战');
INSERT INTO `sensitive_word_data` VALUES ('三二二攻台作战计划');
INSERT INTO `sensitive_word_data` VALUES ('三個代表');
INSERT INTO `sensitive_word_data` VALUES ('三分钟教你原子弹');
INSERT INTO `sensitive_word_data` VALUES ('三利达弓弩专卖网');
INSERT INTO `sensitive_word_data` VALUES ('三利达弓弩军刀');
INSERT INTO `sensitive_word_data` VALUES ('三利达弓弩直营');
INSERT INTO `sensitive_word_data` VALUES ('三利达弓弩配件');
INSERT INTO `sensitive_word_data` VALUES ('三去车仑');
INSERT INTO `sensitive_word_data` VALUES ('三去车仑工力');
INSERT INTO `sensitive_word_data` VALUES ('三句硬话');
INSERT INTO `sensitive_word_data` VALUES ('三唑');
INSERT INTO `sensitive_word_data` VALUES ('三唑仑');
INSERT INTO `sensitive_word_data` VALUES ('三唑仑片');
INSERT INTO `sensitive_word_data` VALUES ('三唑仑迷魂药');
INSERT INTO `sensitive_word_data` VALUES ('三唑仑麻醉乙醚');
INSERT INTO `sensitive_word_data` VALUES ('三唑侖');
INSERT INTO `sensitive_word_data` VALUES ('三坊七巷火灾');
INSERT INTO `sensitive_word_data` VALUES ('三挫');
INSERT INTO `sensitive_word_data` VALUES ('三挫仑');
INSERT INTO `sensitive_word_data` VALUES ('三本亞美电驴下载');
INSERT INTO `sensitive_word_data` VALUES ('三棱军刺专卖');
INSERT INTO `sensitive_word_data` VALUES ('三棱军刺批发');
INSERT INTO `sensitive_word_data` VALUES ('三步倒弩箭专卖');
INSERT INTO `sensitive_word_data` VALUES ('三步倒捕狗药');
INSERT INTO `sensitive_word_data` VALUES ('三步倒药箭批发');
INSERT INTO `sensitive_word_data` VALUES ('三步倒麻醉弩箭销售');
INSERT INTO `sensitive_word_data` VALUES ('三步倒麻醉箭');
INSERT INTO `sensitive_word_data` VALUES ('三步倒麻醉箭专卖');
INSERT INTO `sensitive_word_data` VALUES ('三水法轮');
INSERT INTO `sensitive_word_data` VALUES ('三浦愛佳');
INSERT INTO `sensitive_word_data` VALUES ('三浦愛佳种子');
INSERT INTO `sensitive_word_data` VALUES ('三班仆人派');
INSERT INTO `sensitive_word_data` VALUES ('三田愛BT下载');
INSERT INTO `sensitive_word_data` VALUES ('三秒倒');
INSERT INTO `sensitive_word_data` VALUES ('三箭气枪出售');
INSERT INTO `sensitive_word_data` VALUES ('三級');
INSERT INTO `sensitive_word_data` VALUES ('三級片=>我該死');
INSERT INTO `sensitive_word_data` VALUES ('三级');
INSERT INTO `sensitive_word_data` VALUES ('三级图片');
INSERT INTO `sensitive_word_data` VALUES ('三级片');
INSERT INTO `sensitive_word_data` VALUES ('三级片=**片');
INSERT INTO `sensitive_word_data` VALUES ('三级片BT下载');
INSERT INTO `sensitive_word_data` VALUES ('三级电影 ');
INSERT INTO `sensitive_word_data` VALUES ('三网友');
INSERT INTO `sensitive_word_data` VALUES ('三退');
INSERT INTO `sensitive_word_data` VALUES ('三面翻');
INSERT INTO `sensitive_word_data` VALUES ('上下其手');
INSERT INTO `sensitive_word_data` VALUES ('上分器');
INSERT INTO `sensitive_word_data` VALUES ('上床');
INSERT INTO `sensitive_word_data` VALUES ('上海交大');
INSERT INTO `sensitive_word_data` VALUES ('上海交警');
INSERT INTO `sensitive_word_data` VALUES ('上海代办四六级证');
INSERT INTO `sensitive_word_data` VALUES ('上海代办驾照文凭');
INSERT INTO `sensitive_word_data` VALUES ('上海代开发');
INSERT INTO `sensitive_word_data` VALUES ('上海出售假钱硬币');
INSERT INTO `sensitive_word_data` VALUES ('上海删贴公司');
INSERT INTO `sensitive_word_data` VALUES ('上海办六级证');
INSERT INTO `sensitive_word_data` VALUES ('上海办毕业证');
INSERT INTO `sensitive_word_data` VALUES ('上海办证 毕业证');
INSERT INTO `sensitive_word_data` VALUES ('上海办证/浦东办证');
INSERT INTO `sensitive_word_data` VALUES ('上海办证中心');
INSERT INTO `sensitive_word_data` VALUES ('上海办证公司');
INSERT INTO `sensitive_word_data` VALUES ('上海办证公司 刻章');
INSERT INTO `sensitive_word_data` VALUES ('上海办证刻章');
INSERT INTO `sensitive_word_data` VALUES ('上海办证刻章qq');
INSERT INTO `sensitive_word_data` VALUES ('上海办证刻章发票');
INSERT INTO `sensitive_word_data` VALUES ('上海办证网 刻章');
INSERT INTO `sensitive_word_data` VALUES ('上海卖冰毒出售');
INSERT INTO `sensitive_word_data` VALUES ('上海各国签证 代办');
INSERT INTO `sensitive_word_data` VALUES ('上海孤儿院');
INSERT INTO `sensitive_word_data` VALUES ('上海市工字气枪 出售');
INSERT INTO `sensitive_word_data` VALUES ('上海带开發票');
INSERT INTO `sensitive_word_data` VALUES ('上海帮');
INSERT INTO `sensitive_word_data` VALUES ('上海帮的黄昏');
INSERT INTO `sensitive_word_data` VALUES ('上海性息');
INSERT INTO `sensitive_word_data` VALUES ('上海拆迁悲喜剧');
INSERT INTO `sensitive_word_data` VALUES ('上海电警棍专卖');
INSERT INTO `sensitive_word_data` VALUES ('上海签证学历证明');
INSERT INTO `sensitive_word_data` VALUES ('上海网上办证');
INSERT INTO `sensitive_word_data` VALUES ('上海网上办证qq');
INSERT INTO `sensitive_word_data` VALUES ('上海职称资格证');
INSERT INTO `sensitive_word_data` VALUES ('上海警官证制作');
INSERT INTO `sensitive_word_data` VALUES ('上海证件 刻章');
INSERT INTO `sensitive_word_data` VALUES ('上海负面信息处理');
INSERT INTO `sensitive_word_data` VALUES ('上海高仿真毕业证');
INSERT INTO `sensitive_word_data` VALUES ('上网文凭 办驾驶证');
INSERT INTO `sensitive_word_data` VALUES ('上网文凭QQ 教育服务');
INSERT INTO `sensitive_word_data` VALUES ('上访');
INSERT INTO `sensitive_word_data` VALUES ('上访军');
INSERT INTO `sensitive_word_data` VALUES ('上门激');
INSERT INTO `sensitive_word_data` VALUES ('下体');
INSERT INTO `sensitive_word_data` VALUES ('下届总理人选');
INSERT INTO `sensitive_word_data` VALUES ('下法轮');
INSERT INTO `sensitive_word_data` VALUES ('下硝化甘油的制作方法');
INSERT INTO `sensitive_word_data` VALUES ('下贱');
INSERT INTO `sensitive_word_data` VALUES ('下阴');
INSERT INTO `sensitive_word_data` VALUES ('不举');
INSERT INTO `sensitive_word_data` VALUES ('不孕');
INSERT INTO `sensitive_word_data` VALUES ('不孕症');
INSERT INTO `sensitive_word_data` VALUES ('不思四化');
INSERT INTO `sensitive_word_data` VALUES ('不是易非毁责。志空服金生');
INSERT INTO `sensitive_word_data` VALUES ('不查全');
INSERT INTO `sensitive_word_data` VALUES ('不查都');
INSERT INTO `sensitive_word_data` VALUES ('不泄');
INSERT INTO `sensitive_word_data` VALUES ('不洁性交');
INSERT INTO `sensitive_word_data` VALUES ('不穿');
INSERT INTO `sensitive_word_data` VALUES ('不育');
INSERT INTO `sensitive_word_data` VALUES ('不良少女日记');
INSERT INTO `sensitive_word_data` VALUES ('不良少少日记');
INSERT INTO `sensitive_word_data` VALUES ('不要沉默');
INSERT INTO `sensitive_word_data` VALUES ('与狗性');
INSERT INTO `sensitive_word_data` VALUES ('与表妹通奸');
INSERT INTO `sensitive_word_data` VALUES ('丑化');
INSERT INTO `sensitive_word_data` VALUES ('专业代');
INSERT INTO `sensitive_word_data` VALUES ('专业代写');
INSERT INTO `sensitive_word_data` VALUES ('专业代考');
INSERT INTO `sensitive_word_data` VALUES ('专业供应四川警官证');
INSERT INTO `sensitive_word_data` VALUES ('专业供应河北警官证');
INSERT INTO `sensitive_word_data` VALUES ('专业供应辽宁警官证');
INSERT INTO `sensitive_word_data` VALUES ('专业公司 本地办证');
INSERT INTO `sensitive_word_data` VALUES ('专业制作上海警官证');
INSERT INTO `sensitive_word_data` VALUES ('专业制作四川警官证');
INSERT INTO `sensitive_word_data` VALUES ('专业制作新版警官证');
INSERT INTO `sensitive_word_data` VALUES ('专业办理');
INSERT INTO `sensitive_word_data` VALUES ('专业办证 发票 刻章');
INSERT INTO `sensitive_word_data` VALUES ('专业办证公司');
INSERT INTO `sensitive_word_data` VALUES ('专业办证刻章电话');
INSERT INTO `sensitive_word_data` VALUES ('专业助');
INSERT INTO `sensitive_word_data` VALUES ('专业弓弩网');
INSERT INTO `sensitive_word_data` VALUES ('专业经销假钞');
INSERT INTO `sensitive_word_data` VALUES ('专业网上办理文凭');
INSERT INTO `sensitive_word_data` VALUES ('专业网上办证 刻章');
INSERT INTO `sensitive_word_data` VALUES ('专业网络证件办理');
INSERT INTO `sensitive_word_data` VALUES ('专业证件办理中心');
INSERT INTO `sensitive_word_data` VALUES ('专业调查');
INSERT INTO `sensitive_word_data` VALUES ('专制');
INSERT INTO `sensitive_word_data` VALUES ('专卖双管猎');
INSERT INTO `sensitive_word_data` VALUES ('专卖各类警用装备');
INSERT INTO `sensitive_word_data` VALUES ('专卖各类防身器材');
INSERT INTO `sensitive_word_data` VALUES ('专卖各类防身电棍');
INSERT INTO `sensitive_word_data` VALUES ('专卖各类防身电警棍');
INSERT INTO `sensitive_word_data` VALUES ('专卖手铐电击棒');
INSERT INTO `sensitive_word_data` VALUES ('专卖手铐警用装备');
INSERT INTO `sensitive_word_data` VALUES ('专卖电击器电棍');
INSERT INTO `sensitive_word_data` VALUES ('专卖电击器电棍网');
INSERT INTO `sensitive_word_data` VALUES ('专卖警服手铐Q');
INSERT INTO `sensitive_word_data` VALUES ('专卖警服手铐电警棍');
INSERT INTO `sensitive_word_data` VALUES ('专卖警服电警棍');
INSERT INTO `sensitive_word_data` VALUES ('专卖警用手铐QQ');
INSERT INTO `sensitive_word_data` VALUES ('专卖防身电棍');
INSERT INTO `sensitive_word_data` VALUES ('专卖防身电警棍');
INSERT INTO `sensitive_word_data` VALUES ('专卖防身电警棍网');
INSERT INTO `sensitive_word_data` VALUES ('专卖驽弓');
INSERT INTO `sensitive_word_data` VALUES ('专卖高压电击棒');
INSERT INTO `sensitive_word_data` VALUES ('专卖麻醉驽箭');
INSERT INTO `sensitive_word_data` VALUES ('专奸');
INSERT INTO `sensitive_word_data` VALUES ('专政');
INSERT INTO `sensitive_word_data` VALUES ('专政机器');
INSERT INTO `sensitive_word_data` VALUES ('专用{4}发票');
INSERT INTO `sensitive_word_data` VALUES ('专用发票代开');
INSERT INTO `sensitive_word_data` VALUES ('专销美国GHB液蒙汗药');
INSERT INTO `sensitive_word_data` VALUES ('专门代办新式警官证');
INSERT INTO `sensitive_word_data` VALUES ('专门出售失身粉');
INSERT INTO `sensitive_word_data` VALUES ('专门批发供应折叠手弩');
INSERT INTO `sensitive_word_data` VALUES ('专门批发供应警用手铐');
INSERT INTO `sensitive_word_data` VALUES ('专门批发秦氏弓弩');
INSERT INTO `sensitive_word_data` VALUES ('专门销售各种手弩');
INSERT INTO `sensitive_word_data` VALUES ('世界之门 ');
INSERT INTO `sensitive_word_data` VALUES ('世界以利亚福音宣教会 ');
INSERT INTO `sensitive_word_data` VALUES ('世界十大独裁者 ');
INSERT INTO `sensitive_word_data` VALUES ('世界基督教统一神灵协会 ');
INSERT INTO `sensitive_word_data` VALUES ('世界经济导报');
INSERT INTO `sensitive_word_data` VALUES ('业力回报 ');
INSERT INTO `sensitive_word_data` VALUES ('业力轮 ');
INSERT INTO `sensitive_word_data` VALUES ('丛斌');
INSERT INTO `sensitive_word_data` VALUES ('丛毛');
INSERT INTO `sensitive_word_data` VALUES ('东京热');
INSERT INTO `sensitive_word_data` VALUES ('东北弓弩价格');
INSERT INTO `sensitive_word_data` VALUES ('东北独立');
INSERT INTO `sensitive_word_data` VALUES ('东北风情熟女之惑');
INSERT INTO `sensitive_word_data` VALUES ('东南亚 文凭证件刻章 发票');
INSERT INTO `sensitive_word_data` VALUES ('东南亚网上办证');
INSERT INTO `sensitive_word_data` VALUES ('东南亚证件 刻章电话');
INSERT INTO `sensitive_word_data` VALUES ('东南亚证件 毕业证');
INSERT INTO `sensitive_word_data` VALUES ('东南亚诚信证件');
INSERT INTO `sensitive_word_data` VALUES ('东南西北论');
INSERT INTO `sensitive_word_data` VALUES ('东南西北论谈');
INSERT INTO `sensitive_word_data` VALUES ('东土耳其斯坦');
INSERT INTO `sensitive_word_data` VALUES ('东复活');
INSERT INTO `sensitive_word_data` VALUES ('东方微点');
INSERT INTO `sensitive_word_data` VALUES ('东方时空');
INSERT INTO `sensitive_word_data` VALUES ('东方红时空');
INSERT INTO `sensitive_word_data` VALUES ('东方闪电');
INSERT INTO `sensitive_word_data` VALUES ('东洲');
INSERT INTO `sensitive_word_data` VALUES ('东社');
INSERT INTO `sensitive_word_data` VALUES ('东突');
INSERT INTO `sensitive_word_data` VALUES ('东突厥斯坦');
INSERT INTO `sensitive_word_data` VALUES ('东突厥斯坦伊斯兰');
INSERT INTO `sensitive_word_data` VALUES ('东突厥斯坦伊斯兰运动');
INSERT INTO `sensitive_word_data` VALUES ('东莞代开发票');
INSERT INTO `sensitive_word_data` VALUES ('东莞电警棍');
INSERT INTO `sensitive_word_data` VALUES ('东莞电警棍专卖');
INSERT INTO `sensitive_word_data` VALUES ('东莞警用电警棍销售');
INSERT INTO `sensitive_word_data` VALUES ('东西南北论坛');
INSERT INTO `sensitive_word_data` VALUES ('丝情侣');
INSERT INTO `sensitive_word_data` VALUES ('丝护士');
INSERT INTO `sensitive_word_data` VALUES ('丝袜');
INSERT INTO `sensitive_word_data` VALUES ('丝袜保');
INSERT INTO `sensitive_word_data` VALUES ('丝袜写真');
INSERT INTO `sensitive_word_data` VALUES ('丝袜妹');
INSERT INTO `sensitive_word_data` VALUES ('丝袜恋');
INSERT INTO `sensitive_word_data` VALUES ('丝袜网');
INSERT INTO `sensitive_word_data` VALUES ('丝袜美');
INSERT INTO `sensitive_word_data` VALUES ('丝袜美女');
INSERT INTO `sensitive_word_data` VALUES ('丝袜聊天室');
INSERT INTO `sensitive_word_data` VALUES ('丝足按');
INSERT INTO `sensitive_word_data` VALUES ('丢了');
INSERT INTO `sensitive_word_data` VALUES ('两个中国');
INSERT INTO `sensitive_word_data` VALUES ('两会');
INSERT INTO `sensitive_word_data` VALUES ('两会代');
INSERT INTO `sensitive_word_data` VALUES ('两会又三');
INSERT INTO `sensitive_word_data` VALUES ('两会报道');
INSERT INTO `sensitive_word_data` VALUES ('两会新闻');
INSERT INTO `sensitive_word_data` VALUES ('两岸三地论坛');
INSERT INTO `sensitive_word_data` VALUES ('两岸关系');
INSERT INTO `sensitive_word_data` VALUES ('两岸才子');
INSERT INTO `sensitive_word_data` VALUES ('两岸才子对话');
INSERT INTO `sensitive_word_data` VALUES ('两性淫乱');
INSERT INTO `sensitive_word_data` VALUES ('两性狂情');
INSERT INTO `sensitive_word_data` VALUES ('两情相悦');
INSERT INTO `sensitive_word_data` VALUES ('两派争斗');
INSERT INTO `sensitive_word_data` VALUES ('两用狙击弩专卖');
INSERT INTO `sensitive_word_data` VALUES ('两腿');
INSERT INTO `sensitive_word_data` VALUES ('两腿之间');
INSERT INTO `sensitive_word_data` VALUES ('严义埙');
INSERT INTO `sensitive_word_data` VALUES ('严家其');
INSERT INTO `sensitive_word_data` VALUES ('严家祺');
INSERT INTO `sensitive_word_data` VALUES ('严晓玲');
INSERT INTO `sensitive_word_data` VALUES ('个人圆满说 ');
INSERT INTO `sensitive_word_data` VALUES ('个人崇拜');
INSERT INTO `sensitive_word_data` VALUES ('个邪的党（魔教）');
INSERT INTO `sensitive_word_data` VALUES ('丫与王益');
INSERT INTO `sensitive_word_data` VALUES ('中gong');
INSERT INTO `sensitive_word_data` VALUES ('中俄密约');
INSERT INTO `sensitive_word_data` VALUES ('中俄边界新约');
INSERT INTO `sensitive_word_data` VALUES ('中共');
INSERT INTO `sensitive_word_data` VALUES ('中共专制');
INSERT INTO `sensitive_word_data` VALUES ('中共中央大换血');
INSERT INTO `sensitive_word_data` VALUES ('中共中央文件');
INSERT INTO `sensitive_word_data` VALUES ('中共中央材料');
INSERT INTO `sensitive_word_data` VALUES ('中共中央资料');
INSERT INTO `sensitive_word_data` VALUES ('中共中央黑幕');
INSERT INTO `sensitive_word_data` VALUES ('中共亡');
INSERT INTO `sensitive_word_data` VALUES ('中共伪政权');
INSERT INTO `sensitive_word_data` VALUES ('中共党魁');
INSERT INTO `sensitive_word_data` VALUES ('中共内斗');
INSERT INTO `sensitive_word_data` VALUES ('中共十七大幕前戏');
INSERT INTO `sensitive_word_data` VALUES ('中共太子');
INSERT INTO `sensitive_word_data` VALUES ('中共媒体');
INSERT INTO `sensitive_word_data` VALUES ('中共小丑');
INSERT INTO `sensitive_word_data` VALUES ('中共当局');
INSERT INTO `sensitive_word_data` VALUES ('中共心中最大的恐惧');
INSERT INTO `sensitive_word_data` VALUES ('中共恶霸');
INSERT INTO `sensitive_word_data` VALUES ('中共成人影视网');
INSERT INTO `sensitive_word_data` VALUES ('中共政坛腐败内幕');
INSERT INTO `sensitive_word_data` VALUES ('中共政权');
INSERT INTO `sensitive_word_data` VALUES ('中共政治新星');
INSERT INTO `sensitive_word_data` VALUES ('中共政治流氓');
INSERT INTO `sensitive_word_data` VALUES ('中共无赖');
INSERT INTO `sensitive_word_data` VALUES ('中共暴政');
INSERT INTO `sensitive_word_data` VALUES ('中共暴行');
INSERT INTO `sensitive_word_data` VALUES ('中共特务');
INSERT INTO `sensitive_word_data` VALUES ('中共独枭');
INSERT INTO `sensitive_word_data` VALUES ('中共独裁');
INSERT INTO `sensitive_word_data` VALUES ('中共王朝');
INSERT INTO `sensitive_word_data` VALUES ('中共监狱');
INSERT INTO `sensitive_word_data` VALUES ('中共第六代');
INSERT INTO `sensitive_word_data` VALUES ('中共统治');
INSERT INTO `sensitive_word_data` VALUES ('中共网特');
INSERT INTO `sensitive_word_data` VALUES ('中共警察');
INSERT INTO `sensitive_word_data` VALUES ('中共走狗');
INSERT INTO `sensitive_word_data` VALUES ('中共迫害');
INSERT INTO `sensitive_word_data` VALUES ('中共邪教');
INSERT INTO `sensitive_word_data` VALUES ('中共邪毒素');
INSERT INTO `sensitive_word_data` VALUES ('中共黑');
INSERT INTO `sensitive_word_data` VALUES ('中功');
INSERT INTO `sensitive_word_data` VALUES ('中华人民实话实说');
INSERT INTO `sensitive_word_data` VALUES ('中华人民正邪');
INSERT INTO `sensitive_word_data` VALUES ('中华养生益智功');
INSERT INTO `sensitive_word_data` VALUES ('中华大众');
INSERT INTO `sensitive_word_data` VALUES ('中华大地思考');
INSERT INTO `sensitive_word_data` VALUES ('中华帝国');
INSERT INTO `sensitive_word_data` VALUES ('中华时事');
INSERT INTO `sensitive_word_data` VALUES ('中华民国');
INSERT INTO `sensitive_word_data` VALUES ('中华真实报道');
INSERT INTO `sensitive_word_data` VALUES ('中华联邦政府');
INSERT INTO `sensitive_word_data` VALUES ('中华讲清');
INSERT INTO `sensitive_word_data` VALUES ('中南海');
INSERT INTO `sensitive_word_data` VALUES ('中南海内幕');
INSERT INTO `sensitive_word_data` VALUES ('中南海内斗');
INSERT INTO `sensitive_word_data` VALUES ('中南海惊现东宫小朝廷');
INSERT INTO `sensitive_word_data` VALUES ('中南海斗争');
INSERT INTO `sensitive_word_data` VALUES ('中南海浓云密布');
INSERT INTO `sensitive_word_data` VALUES ('中南海的权利游戏');
INSERT INTO `sensitive_word_data` VALUES ('中南海秘闻');
INSERT INTO `sensitive_word_data` VALUES ('中南海高层权利斗争');
INSERT INTO `sensitive_word_data` VALUES ('中印边界谈判结果');
INSERT INTO `sensitive_word_data` VALUES ('中原油田');
INSERT INTO `sensitive_word_data` VALUES ('中原綾种子');
INSERT INTO `sensitive_word_data` VALUES ('中国zf');
INSERT INTO `sensitive_word_data` VALUES ('中国不强');
INSERT INTO `sensitive_word_data` VALUES ('中国之春');
INSERT INTO `sensitive_word_data` VALUES ('中国人都在上的四大当');
INSERT INTO `sensitive_word_data` VALUES ('中国六四真相');
INSERT INTO `sensitive_word_data` VALUES ('中国共产党亡');
INSERT INTO `sensitive_word_data` VALUES ('中国军刀出售网');
INSERT INTO `sensitive_word_data` VALUES ('中国军用运输机');
INSERT INTO `sensitive_word_data` VALUES ('中国劳工通讯');
INSERT INTO `sensitive_word_data` VALUES ('中国十大悲情语句');
INSERT INTO `sensitive_word_data` VALUES ('中国国家领导人子女任职名单');
INSERT INTO `sensitive_word_data` VALUES ('中国国家领导人最强阵容');
INSERT INTO `sensitive_word_data` VALUES ('中国国家领导人的最强阵容');
INSERT INTO `sensitive_word_data` VALUES ('中国在统一问题上的投降主义');
INSERT INTO `sensitive_word_data` VALUES ('中国复兴论坛');
INSERT INTO `sensitive_word_data` VALUES ('中国威胁论');
INSERT INTO `sensitive_word_data` VALUES ('中国孤儿院');
INSERT INTO `sensitive_word_data` VALUES ('中国弓努店');
INSERT INTO `sensitive_word_data` VALUES ('中国弓弩专卖网');
INSERT INTO `sensitive_word_data` VALUES ('中国弓弩价格');
INSERT INTO `sensitive_word_data` VALUES ('中国弓弩店');
INSERT INTO `sensitive_word_data` VALUES ('中国弓弩狩猎网');
INSERT INTO `sensitive_word_data` VALUES ('中国弓弩直销');
INSERT INTO `sensitive_word_data` VALUES ('中国弓弩网');
INSERT INTO `sensitive_word_data` VALUES ('中国弓驽网');
INSERT INTO `sensitive_word_data` VALUES ('中国弩弓专卖');
INSERT INTO `sensitive_word_data` VALUES ('中国当局');
INSERT INTO `sensitive_word_data` VALUES ('中国性搜网');
INSERT INTO `sensitive_word_data` VALUES ('中国性爱城中国性城');
INSERT INTO `sensitive_word_data` VALUES ('中国成人网');
INSERT INTO `sensitive_word_data` VALUES ('中国成人网站');
INSERT INTO `sensitive_word_data` VALUES ('中国成人通');
INSERT INTO `sensitive_word_data` VALUES ('中国战神军用弓弩');
INSERT INTO `sensitive_word_data` VALUES ('中国户外刀具网');
INSERT INTO `sensitive_word_data` VALUES ('中国承认影视网中国成人网中国成人通');
INSERT INTO `sensitive_word_data` VALUES ('中国报禁');
INSERT INTO `sensitive_word_data` VALUES ('中国改革年代政治斗争');
INSERT INTO `sensitive_word_data` VALUES ('中国政坛“明日之星”');
INSERT INTO `sensitive_word_data` VALUES ('中国政坛“清华帮”盛极而衰');
INSERT INTO `sensitive_word_data` VALUES ('中国政坛新星');
INSERT INTO `sensitive_word_data` VALUES ('中国政坛新星中的四大天王');
INSERT INTO `sensitive_word_data` VALUES ('中国政治新星');
INSERT INTO `sensitive_word_data` VALUES ('中国教徒');
INSERT INTO `sensitive_word_data` VALUES ('中国无耻语录');
INSERT INTO `sensitive_word_data` VALUES ('中国是全球唯一不能惹的国家');
INSERT INTO `sensitive_word_data` VALUES ('中国民主党联合总部');
INSERT INTO `sensitive_word_data` VALUES ('中国泛蓝联盟');
INSERT INTO `sensitive_word_data` VALUES ('中国海外腐败兵团');
INSERT INTO `sensitive_word_data` VALUES ('中国特色错别字');
INSERT INTO `sensitive_word_data` VALUES ('中国猪');
INSERT INTO `sensitive_word_data` VALUES ('中国真实内容');
INSERT INTO `sensitive_word_data` VALUES ('中国社会的艾滋病');
INSERT INTO `sensitive_word_data` VALUES ('中国社会论坛');
INSERT INTO `sensitive_word_data` VALUES ('中国社会进步党');
INSERT INTO `sensitive_word_data` VALUES ('中国神弩');
INSERT INTO `sensitive_word_data` VALUES ('中国神弩网');
INSERT INTO `sensitive_word_data` VALUES ('中国问题论坛');
INSERT INTO `sensitive_word_data` VALUES ('中国革命党');
INSERT INTO `sensitive_word_data` VALUES ('中国高层人事变动解读');
INSERT INTO `sensitive_word_data` VALUES ('中国高层权力斗争');
INSERT INTO `sensitive_word_data` VALUES ('中國國家領導人子女任職名單');
INSERT INTO `sensitive_word_data` VALUES ('中國當局');
INSERT INTO `sensitive_word_data` VALUES ('中央zf');
INSERT INTO `sensitive_word_data` VALUES ('中央军委');
INSERT INTO `sensitive_word_data` VALUES ('中央文件');
INSERT INTO `sensitive_word_data` VALUES ('中央资料');
INSERT INTO `sensitive_word_data` VALUES ('中央领导');
INSERT INTO `sensitive_word_data` VALUES ('中文搜性网');
INSERT INTO `sensitive_word_data` VALUES ('中断排尿');
INSERT INTO `sensitive_word_data` VALUES ('中日没有不友好的');
INSERT INTO `sensitive_word_data` VALUES ('中村あみ种子');
INSERT INTO `sensitive_word_data` VALUES ('中森藍子种子');
INSERT INTO `sensitive_word_data` VALUES ('中珙');
INSERT INTO `sensitive_word_data` VALUES ('中的班禅');
INSERT INTO `sensitive_word_data` VALUES ('中石化说亏损');
INSERT INTO `sensitive_word_data` VALUES ('中纪委');
INSERT INTO `sensitive_word_data` VALUES ('中美国家领导人的子女职业对照');
INSERT INTO `sensitive_word_data` VALUES ('中谷あいみBT下载');
INSERT INTO `sensitive_word_data` VALUES ('中里愛菜全集');
INSERT INTO `sensitive_word_data` VALUES ('丰乳');
INSERT INTO `sensitive_word_data` VALUES ('丰硕');
INSERT INTO `sensitive_word_data` VALUES ('丰肥');
INSERT INTO `sensitive_word_data` VALUES ('丰腴');
INSERT INTO `sensitive_word_data` VALUES ('丰臀');
INSERT INTO `sensitive_word_data` VALUES ('丰隆');
INSERT INTO `sensitive_word_data` VALUES ('临沂电警棍专卖');
INSERT INTO `sensitive_word_data` VALUES ('临震预报');
INSERT INTO `sensitive_word_data` VALUES ('为党不为国');
INSERT INTO `sensitive_word_data` VALUES ('为您{4}服务');
INSERT INTO `sensitive_word_data` VALUES ('主动');
INSERT INTO `sensitive_word_data` VALUES ('主席像');
INSERT INTO `sensitive_word_data` VALUES ('主席复活');
INSERT INTO `sensitive_word_data` VALUES ('主席忏');
INSERT INTO `sensitive_word_data` VALUES ('主席李世民');
INSERT INTO `sensitive_word_data` VALUES ('主席画像');
INSERT INTO `sensitive_word_data` VALUES ('主神教');
INSERT INTO `sensitive_word_data` VALUES ('丽媛离');
INSERT INTO `sensitive_word_data` VALUES ('丽香');
INSERT INTO `sensitive_word_data` VALUES ('举国体');
INSERT INTO `sensitive_word_data` VALUES ('举高双腿');
INSERT INTO `sensitive_word_data` VALUES ('乃南葵BT下载');
INSERT INTO `sensitive_word_data` VALUES ('久战');
INSERT INTO `sensitive_word_data` VALUES ('义解');
INSERT INTO `sensitive_word_data` VALUES ('乌云其木格');
INSERT INTO `sensitive_word_data` VALUES ('乌兰夫');
INSERT INTO `sensitive_word_data` VALUES ('乌兰木伦');
INSERT INTO `sensitive_word_data` VALUES ('乌日图');
INSERT INTO `sensitive_word_data` VALUES ('乌蝇水');
INSERT INTO `sensitive_word_data` VALUES ('乌鲁木齐');
INSERT INTO `sensitive_word_data` VALUES ('乐趣');
INSERT INTO `sensitive_word_data` VALUES ('乐透码 ');
INSERT INTO `sensitive_word_data` VALUES ('乔晓阳');
INSERT INTO `sensitive_word_data` VALUES ('乔波室内滑雪馆');
INSERT INTO `sensitive_word_data` VALUES ('乔清晨');
INSERT INTO `sensitive_word_data` VALUES ('乔石');
INSERT INTO `sensitive_word_data` VALUES ('乖乖水拍肩粉专卖');
INSERT INTO `sensitive_word_data` VALUES ('乖乖水迷奸药专卖');
INSERT INTO `sensitive_word_data` VALUES ('乖乖粉');
INSERT INTO `sensitive_word_data` VALUES ('乖乖药');
INSERT INTO `sensitive_word_data` VALUES ('乖巧');
INSERT INTO `sensitive_word_data` VALUES ('乖肉');
INSERT INTO `sensitive_word_data` VALUES ('乙醚');
INSERT INTO `sensitive_word_data` VALUES ('九-评');
INSERT INTO `sensitive_word_data` VALUES ('九.评');
INSERT INTO `sensitive_word_data` VALUES ('九jiu评');
INSERT INTO `sensitive_word_data` VALUES ('九ping');
INSERT INTO `sensitive_word_data` VALUES ('九·十·三运动');
INSERT INTO `sensitive_word_data` VALUES ('九十三运动');
INSERT INTO `sensitive_word_data` VALUES ('九浅一深');
INSERT INTO `sensitive_word_data` VALUES ('九评');
INSERT INTO `sensitive_word_data` VALUES ('九评{5}');
INSERT INTO `sensitive_word_data` VALUES ('九评××');
INSERT INTO `sensitive_word_data` VALUES ('九评公产党');
INSERT INTO `sensitive_word_data` VALUES ('九评共');
INSERT INTO `sensitive_word_data` VALUES ('九评共产党');
INSERT INTO `sensitive_word_data` VALUES ('九龙论坛');
INSERT INTO `sensitive_word_data` VALUES ('习仲勋');
INSERT INTO `sensitive_word_data` VALUES ('习太子');
INSERT INTO `sensitive_word_data` VALUES ('习明泽');
INSERT INTO `sensitive_word_data` VALUES ('习晋平');
INSERT INTO `sensitive_word_data` VALUES ('习近平');
INSERT INTO `sensitive_word_data` VALUES ('习进平');
INSERT INTO `sensitive_word_data` VALUES ('乡村如此多娇');
INSERT INTO `sensitive_word_data` VALUES ('书办理');
INSERT INTO `sensitive_word_data` VALUES ('书记');
INSERT INTO `sensitive_word_data` VALUES ('书记处');
INSERT INTO `sensitive_word_data` VALUES ('买买提明·阿不都热依木');
INSERT INTO `sensitive_word_data` VALUES ('买假钱联系');
INSERT INTO `sensitive_word_data` VALUES ('买卖 批发假币');
INSERT INTO `sensitive_word_data` VALUES ('买卖54狗');
INSERT INTO `sensitive_word_data` VALUES ('买卖64狗');
INSERT INTO `sensitive_word_data` VALUES ('买卖假钱 假钞');
INSERT INTO `sensitive_word_data` VALUES ('买卖军狗');
INSERT INTO `sensitive_word_data` VALUES ('买卖军用枪支');
INSERT INTO `sensitive_word_data` VALUES ('买卖台湾版假人民币');
INSERT INTO `sensitive_word_data` VALUES ('买卖枪支');
INSERT INTO `sensitive_word_data` VALUES ('买卖警察枪支');
INSERT INTO `sensitive_word_data` VALUES ('买发票');
INSERT INTO `sensitive_word_data` VALUES ('买广州定额醱票');
INSERT INTO `sensitive_word_data` VALUES ('买春 ');
INSERT INTO `sensitive_word_data` VALUES ('买春买淫');
INSERT INTO `sensitive_word_data` VALUES ('买春堂');
INSERT INTO `sensitive_word_data` VALUES ('买枪 ');
INSERT INTO `sensitive_word_data` VALUES ('买真枪');
INSERT INTO `sensitive_word_data` VALUES ('买答案');
INSERT INTO `sensitive_word_data` VALUES ('乱交');
INSERT INTO `sensitive_word_data` VALUES ('乱交乱淫乱伦');
INSERT INTO `sensitive_word_data` VALUES ('乱交的故事乱伦小说');
INSERT INTO `sensitive_word_data` VALUES ('乱伦 ');
INSERT INTO `sensitive_word_data` VALUES ('乱伦小');
INSERT INTO `sensitive_word_data` VALUES ('乱伦类');
INSERT INTO `sensitive_word_data` VALUES ('乱奸');
INSERT INTO `sensitive_word_data` VALUES ('乱抽');
INSERT INTO `sensitive_word_data` VALUES ('乱揉');
INSERT INTO `sensitive_word_data` VALUES ('乱摸');
INSERT INTO `sensitive_word_data` VALUES ('乱淌');
INSERT INTO `sensitive_word_data` VALUES ('乱舔');
INSERT INTO `sensitive_word_data` VALUES ('乱蹬');
INSERT INTO `sensitive_word_data` VALUES ('乱顶');
INSERT INTO `sensitive_word_data` VALUES ('乳交');
INSERT INTO `sensitive_word_data` VALUES ('乳交乳伦');
INSERT INTO `sensitive_word_data` VALUES ('乳房');
INSERT INTO `sensitive_word_data` VALUES ('乾妈');
INSERT INTO `sensitive_word_data` VALUES ('乾姊');
INSERT INTO `sensitive_word_data` VALUES ('亂倫');
INSERT INTO `sensitive_word_data` VALUES ('了件渔袍');
INSERT INTO `sensitive_word_data` VALUES ('争鸣杂志');
INSERT INTO `sensitive_word_data` VALUES ('争鸣论坛');
INSERT INTO `sensitive_word_data` VALUES ('事实独立');
INSERT INTO `sensitive_word_data` VALUES ('二B');
INSERT INTO `sensitive_word_data` VALUES ('二乙酰吗啡');
INSERT INTO `sensitive_word_data` VALUES ('二奶');
INSERT INTO `sensitive_word_data` VALUES ('二奶大');
INSERT INTO `sensitive_word_data` VALUES ('二奶大奖');
INSERT INTO `sensitive_word_data` VALUES ('二奶大奖赛');
INSERT INTO `sensitive_word_data` VALUES ('二奶大賽');
INSERT INTO `sensitive_word_data` VALUES ('二奶大赛');
INSERT INTO `sensitive_word_data` VALUES ('二期梅毒');
INSERT INTO `sensitive_word_data` VALUES ('二逼');
INSERT INTO `sensitive_word_data` VALUES ('于均波');
INSERT INTO `sensitive_word_data` VALUES ('于大海');
INSERT INTO `sensitive_word_data` VALUES ('于幼军');
INSERT INTO `sensitive_word_data` VALUES ('于永波');
INSERT INTO `sensitive_word_data` VALUES ('于浩成');
INSERT INTO `sensitive_word_data` VALUES ('于珍');
INSERT INTO `sensitive_word_data` VALUES ('五不');
INSERT INTO `sensitive_word_data` VALUES ('五个部长一个省长');
INSERT INTO `sensitive_word_data` VALUES ('五出三进');
INSERT INTO `sensitive_word_data` VALUES ('五大常委');
INSERT INTO `sensitive_word_data` VALUES ('五套功');
INSERT INTO `sensitive_word_data` VALUES ('五套功法');
INSERT INTO `sensitive_word_data` VALUES ('五奶小青');
INSERT INTO `sensitive_word_data` VALUES ('五星宏辉');
INSERT INTO `sensitive_word_data` VALUES ('五月天');
INSERT INTO `sensitive_word_data` VALUES ('五毛党');
INSERT INTO `sensitive_word_data` VALUES ('井上千尋种子');
INSERT INTO `sensitive_word_data` VALUES ('亚太正悟网');
INSERT INTO `sensitive_word_data` VALUES ('亚洲床上色情');
INSERT INTO `sensitive_word_data` VALUES ('亚洲美女总');
INSERT INTO `sensitive_word_data` VALUES ('亚洲自由之声');
INSERT INTO `sensitive_word_data` VALUES ('亚洲自由电台');
INSERT INTO `sensitive_word_data` VALUES ('亚热');
INSERT INTO `sensitive_word_data` VALUES ('亡党');
INSERT INTO `sensitive_word_data` VALUES ('亡共者胡');
INSERT INTO `sensitive_word_data` VALUES ('亡国');
INSERT INTO `sensitive_word_data` VALUES ('亢奋');
INSERT INTO `sensitive_word_data` VALUES ('亢进');
INSERT INTO `sensitive_word_data` VALUES ('交友网');
INSERT INTO `sensitive_word_data` VALUES ('交合');
INSERT INTO `sensitive_word_data` VALUES ('交媾');
INSERT INTO `sensitive_word_data` VALUES ('交媾素');
INSERT INTO `sensitive_word_data` VALUES ('交换夫妻');
INSERT INTO `sensitive_word_data` VALUES ('交欢');
INSERT INTO `sensitive_word_data` VALUES ('交班');
INSERT INTO `sensitive_word_data` VALUES ('交缠');
INSERT INTO `sensitive_word_data` VALUES ('交而不泄');
INSERT INTO `sensitive_word_data` VALUES ('交融');
INSERT INTO `sensitive_word_data` VALUES ('交警');
INSERT INTO `sensitive_word_data` VALUES ('交配');
INSERT INTO `sensitive_word_data` VALUES ('交颈');
INSERT INTO `sensitive_word_data` VALUES ('亦凡');
INSERT INTO `sensitive_word_data` VALUES ('产党共');
INSERT INTO `sensitive_word_data` VALUES ('京地震');
INSERT INTO `sensitive_word_data` VALUES ('京夫子 ');
INSERT INTO `sensitive_word_data` VALUES ('京要地震');
INSERT INTO `sensitive_word_data` VALUES ('京郊旅游');
INSERT INTO `sensitive_word_data` VALUES ('人与兽乱交图片');
INSERT INTO `sensitive_word_data` VALUES ('人事任免');
INSERT INTO `sensitive_word_data` VALUES ('人事推测');
INSERT INTO `sensitive_word_data` VALUES ('人事预测');
INSERT INTO `sensitive_word_data` VALUES ('人事预言');
INSERT INTO `sensitive_word_data` VALUES ('人体炸弹');
INSERT INTO `sensitive_word_data` VALUES ('人体炸弹制作流程');
INSERT INTO `sensitive_word_data` VALUES ('人体艺');
INSERT INTO `sensitive_word_data` VALUES ('人体艺术');
INSERT INTO `sensitive_word_data` VALUES ('人全球春节晚会');
INSERT INTO `sensitive_word_data` VALUES ('人兽乱交');
INSERT INTO `sensitive_word_data` VALUES ('人兽性交');
INSERT INTO `sensitive_word_data` VALUES ('人在云上');
INSERT INTO `sensitive_word_data` VALUES ('人大');
INSERT INTO `sensitive_word_data` VALUES ('人妻兽虐曲');
INSERT INTO `sensitive_word_data` VALUES ('人宇特能功 ');
INSERT INTO `sensitive_word_data` VALUES ('人工少女');
INSERT INTO `sensitive_word_data` VALUES ('人弹 ');
INSERT INTO `sensitive_word_data` VALUES ('人权');
INSERT INTO `sensitive_word_data` VALUES ('人权律');
INSERT INTO `sensitive_word_data` VALUES ('人权恶棍');
INSERT INTO `sensitive_word_data` VALUES ('人民之声论坛');
INSERT INTO `sensitive_word_data` VALUES ('人民内情真相');
INSERT INTO `sensitive_word_data` VALUES ('人民大众时事参考');
INSERT INTO `sensitive_word_data` VALUES ('人民币恶搞');
INSERT INTO `sensitive_word_data` VALUES ('人民报');
INSERT INTO `sensitive_word_data` VALUES ('人民报讯');
INSERT INTO `sensitive_word_data` VALUES ('人民真实');
INSERT INTO `sensitive_word_data` VALUES ('人民真实报道');
INSERT INTO `sensitive_word_data` VALUES ('人民运动');
INSERT INTO `sensitive_word_data` VALUES ('人渣');
INSERT INTO `sensitive_word_data` VALUES ('人游行');
INSERT INTO `sensitive_word_data` VALUES ('人真钱');
INSERT INTO `sensitive_word_data` VALUES ('人类灭亡进程表');
INSERT INTO `sensitive_word_data` VALUES ('人类罪恶论 ');
INSERT INTO `sensitive_word_data` VALUES ('仁吉旺姆');
INSERT INTO `sensitive_word_data` VALUES ('仁寿警方');
INSERT INTO `sensitive_word_data` VALUES ('仁青加');
INSERT INTO `sensitive_word_data` VALUES ('仆不怕饮');
INSERT INTO `sensitive_word_data` VALUES ('仇共');
INSERT INTO `sensitive_word_data` VALUES ('今井明日香电驴下载');
INSERT INTO `sensitive_word_data` VALUES ('今有广东');
INSERT INTO `sensitive_word_data` VALUES ('从圣地寻求财富');
INSERT INTO `sensitive_word_data` VALUES ('他们嫌我挡了城市的道路');
INSERT INTO `sensitive_word_data` VALUES ('他妈的=>他*的');
INSERT INTO `sensitive_word_data` VALUES ('他娘的');
INSERT INTO `sensitive_word_data` VALUES ('他媽的=>他*的');
INSERT INTO `sensitive_word_data` VALUES ('付申奇');
INSERT INTO `sensitive_word_data` VALUES ('仙鹤气枪出售');
INSERT INTO `sensitive_word_data` VALUES ('代-办-证件');
INSERT INTO `sensitive_word_data` VALUES ('代{4}考');
INSERT INTO `sensitive_word_data` VALUES ('代写毕');
INSERT INTO `sensitive_word_data` VALUES ('代写论');
INSERT INTO `sensitive_word_data` VALUES ('代办');
INSERT INTO `sensitive_word_data` VALUES ('代办{5}证件');
INSERT INTO `sensitive_word_data` VALUES ('代办{6}信用卡');
INSERT INTO `sensitive_word_data` VALUES ('代办{8}卡');
INSERT INTO `sensitive_word_data` VALUES ('代办二代身份证');
INSERT INTO `sensitive_word_data` VALUES ('代办二代防伪身份证');
INSERT INTO `sensitive_word_data` VALUES ('代办假证 学历证');
INSERT INTO `sensitive_word_data` VALUES ('代办制');
INSERT INTO `sensitive_word_data` VALUES ('代办发票');
INSERT INTO `sensitive_word_data` VALUES ('代办各类证件');
INSERT INTO `sensitive_word_data` VALUES ('代办国外文凭');
INSERT INTO `sensitive_word_data` VALUES ('代办学');
INSERT INTO `sensitive_word_data` VALUES ('代办学位证');
INSERT INTO `sensitive_word_data` VALUES ('代办文凭');
INSERT INTO `sensitive_word_data` VALUES ('代办文凭学历 学籍档案');
INSERT INTO `sensitive_word_data` VALUES ('代办签证');
INSERT INTO `sensitive_word_data` VALUES ('代办警官证');
INSERT INTO `sensitive_word_data` VALUES ('代办证件');
INSERT INTO `sensitive_word_data` VALUES ('代办证件 上网文凭');
INSERT INTO `sensitive_word_data` VALUES ('代办证件 刻章QQ');
INSERT INTO `sensitive_word_data` VALUES ('代办证件 学位认证');
INSERT INTO `sensitive_word_data` VALUES ('代办证件 学历认证');
INSERT INTO `sensitive_word_data` VALUES ('代办证件 车牌执照');
INSERT INTO `sensitive_word_data` VALUES ('代办证件**');
INSERT INTO `sensitive_word_data` VALUES ('代办证件{MOD}');
INSERT INTO `sensitive_word_data` VALUES ('代卖发票');
INSERT INTO `sensitive_word_data` VALUES ('代售发票');
INSERT INTO `sensitive_word_data` VALUES ('代孕');
INSERT INTO `sensitive_word_data` VALUES ('代开');
INSERT INTO `sensitive_word_data` VALUES ('代开fapiao');
INSERT INTO `sensitive_word_data` VALUES ('代开上海各种行业发票');
INSERT INTO `sensitive_word_data` VALUES ('代开全国各地各行各业');
INSERT INTO `sensitive_word_data` VALUES ('代开北京地税发票');
INSERT INTO `sensitive_word_data` VALUES ('代开发嘌');
INSERT INTO `sensitive_word_data` VALUES ('代开发票');
INSERT INTO `sensitive_word_data` VALUES ('代开发票**');
INSERT INTO `sensitive_word_data` VALUES ('代开发票QQ');
INSERT INTO `sensitive_word_data` VALUES ('代开发票{MOD}');
INSERT INTO `sensitive_word_data` VALUES ('代开发票咨询电话');
INSERT INTO `sensitive_word_data` VALUES ('代开发！票');
INSERT INTO `sensitive_word_data` VALUES ('代开咨询发票');
INSERT INTO `sensitive_word_data` VALUES ('代开商业发票');
INSERT INTO `sensitive_word_data` VALUES ('代开商品发票');
INSERT INTO `sensitive_word_data` VALUES ('代开国税发票');
INSERT INTO `sensitive_word_data` VALUES ('代开国际货运发票');
INSERT INTO `sensitive_word_data` VALUES ('代开地税发票');
INSERT INTO `sensitive_word_data` VALUES ('代开增值税');
INSERT INTO `sensitive_word_data` VALUES ('代开安装发票');
INSERT INTO `sensitive_word_data` VALUES ('代开山东地税发票');
INSERT INTO `sensitive_word_data` VALUES ('代开广告发票');
INSERT INTO `sensitive_word_data` VALUES ('代开广西发票');
INSERT INTO `sensitive_word_data` VALUES ('代开建筑发票');
INSERT INTO `sensitive_word_data` VALUES ('代开普通发票');
INSERT INTO `sensitive_word_data` VALUES ('代开普通商品销售发票');
INSERT INTO `sensitive_word_data` VALUES ('代开服务发票');
INSERT INTO `sensitive_word_data` VALUES ('代开河北地税发票');
INSERT INTO `sensitive_word_data` VALUES ('代开租赁发票');
INSERT INTO `sensitive_word_data` VALUES ('代开税票QQ');
INSERT INTO `sensitive_word_data` VALUES ('代开维修发票');
INSERT INTO `sensitive_word_data` VALUES ('代开运输发票');
INSERT INTO `sensitive_word_data` VALUES ('代开避税发票');
INSERT INTO `sensitive_word_data` VALUES ('代开餐饮发票');
INSERT INTO `sensitive_word_data` VALUES ('代您考');
INSERT INTO `sensitive_word_data` VALUES ('代理{4}开票');
INSERT INTO `sensitive_word_data` VALUES ('代理代开行业税票');
INSERT INTO `sensitive_word_data` VALUES ('代理假币批发');
INSERT INTO `sensitive_word_data` VALUES ('代理办证 刻章电话');
INSERT INTO `sensitive_word_data` VALUES ('代理发票');
INSERT INTO `sensitive_word_data` VALUES ('代理票据');
INSERT INTO `sensitive_word_data` VALUES ('代缴发票');
INSERT INTO `sensitive_word_data` VALUES ('代缴税');
INSERT INTO `sensitive_word_data` VALUES ('代考');
INSERT INTO `sensitive_word_data` VALUES ('代血浆');
INSERT INTO `sensitive_word_data` VALUES ('代表烦');
INSERT INTO `sensitive_word_data` VALUES ('代讨债 ');
INSERT INTO `sensitive_word_data` VALUES ('代追债 ');
INSERT INTO `sensitive_word_data` VALUES ('代開');
INSERT INTO `sensitive_word_data` VALUES ('代開广州税票');
INSERT INTO `sensitive_word_data` VALUES ('代開广州货物销售發票');
INSERT INTO `sensitive_word_data` VALUES ('代開广州醱票');
INSERT INTO `sensitive_word_data` VALUES ('代開發票');
INSERT INTO `sensitive_word_data` VALUES ('代開福建税票');
INSERT INTO `sensitive_word_data` VALUES ('代開醱票');
INSERT INTO `sensitive_word_data` VALUES ('代開醱票㊣');
INSERT INTO `sensitive_word_data` VALUES ('令女人春心荡漾');
INSERT INTO `sensitive_word_data` VALUES ('令女性春心荡漾');
INSERT INTO `sensitive_word_data` VALUES ('令狐安');
INSERT INTO `sensitive_word_data` VALUES ('令狐计划');
INSERT INTO `sensitive_word_data` VALUES ('令计划');
INSERT INTO `sensitive_word_data` VALUES ('以血护法');
INSERT INTO `sensitive_word_data` VALUES ('以身护法');
INSERT INTO `sensitive_word_data` VALUES ('仲兆隆');
INSERT INTO `sensitive_word_data` VALUES ('任于斯国');
INSERT INTO `sensitive_word_data` VALUES ('任克礼');
INSERT INTO `sensitive_word_data` VALUES ('任启兴');
INSERT INTO `sensitive_word_data` VALUES ('任建新');
INSERT INTO `sensitive_word_data` VALUES ('任意显');
INSERT INTO `sensitive_word_data` VALUES ('任文燕');
INSERT INTO `sensitive_word_data` VALUES ('任泽民');
INSERT INTO `sensitive_word_data` VALUES ('任玉岭');
INSERT INTO `sensitive_word_data` VALUES ('任畹町');
INSERT INTO `sensitive_word_data` VALUES ('任茂东');
INSERT INTO `sensitive_word_data` VALUES ('仿品军刺出售');
INSERT INTO `sensitive_word_data` VALUES ('仿品军刺网');
INSERT INTO `sensitive_word_data` VALUES ('仿真假钞');
INSERT INTO `sensitive_word_data` VALUES ('仿真手枪');
INSERT INTO `sensitive_word_data` VALUES ('仿真枪');
INSERT INTO `sensitive_word_data` VALUES ('仿真枪QQ');
INSERT INTO `sensitive_word_data` VALUES ('仿真枪诚信经营');
INSERT INTO `sensitive_word_data` VALUES ('仿真气枪，诚信经营');
INSERT INTO `sensitive_word_data` VALUES ('仿真证');
INSERT INTO `sensitive_word_data` VALUES ('企业如何建立网站');
INSERT INTO `sensitive_word_data` VALUES ('企业汇总会计报表');
INSERT INTO `sensitive_word_data` VALUES ('伊扎布特');
INSERT INTO `sensitive_word_data` VALUES ('伊斯兰');
INSERT INTO `sensitive_word_data` VALUES ('伊斯兰运动');
INSERT INTO `sensitive_word_data` VALUES ('伊東');
INSERT INTO `sensitive_word_data` VALUES ('伊波拉瘟疫');
INSERT INTO `sensitive_word_data` VALUES ('伊济源');
INSERT INTO `sensitive_word_data` VALUES ('伍凡');
INSERT INTO `sensitive_word_data` VALUES ('伍增荣');
INSERT INTO `sensitive_word_data` VALUES ('伍淑清');
INSERT INTO `sensitive_word_data` VALUES ('伍绍祖');
INSERT INTO `sensitive_word_data` VALUES ('伏在');
INSERT INTO `sensitive_word_data` VALUES ('众像羔');
INSERT INTO `sensitive_word_data` VALUES ('众里寻一的视频');
INSERT INTO `sensitive_word_data` VALUES ('优化官员');
INSERT INTO `sensitive_word_data` VALUES ('优昙婆罗花');
INSERT INTO `sensitive_word_data` VALUES ('优香');
INSERT INTO `sensitive_word_data` VALUES ('会阴');
INSERT INTO `sensitive_word_data` VALUES ('会阴中心腱');
INSERT INTO `sensitive_word_data` VALUES ('会阴浅横肌');
INSERT INTO `sensitive_word_data` VALUES ('会阴浅隙');
INSERT INTO `sensitive_word_data` VALUES ('会阴深横肌');
INSERT INTO `sensitive_word_data` VALUES ('会阴深隙');
INSERT INTO `sensitive_word_data` VALUES ('会阴部肌肉群');
INSERT INTO `sensitive_word_data` VALUES ('传中共中央关于17大的人事安排意见');
INSERT INTO `sensitive_word_data` VALUES ('传九退三 ');
INSERT INTO `sensitive_word_data` VALUES ('传染性软疣');
INSERT INTO `sensitive_word_data` VALUES ('传真群发');
INSERT INTO `sensitive_word_data` VALUES ('传统');
INSERT INTO `sensitive_word_data` VALUES ('传说的胡曾联手是一种假象');
INSERT INTO `sensitive_word_data` VALUES ('伦公');
INSERT INTO `sensitive_word_data` VALUES ('伦功');
INSERT INTO `sensitive_word_data` VALUES ('伦攻');
INSERT INTO `sensitive_word_data` VALUES ('伦敦西藏网');
INSERT INTO `sensitive_word_data` VALUES ('伦理大');
INSERT INTO `sensitive_word_data` VALUES ('伦理毛');
INSERT INTO `sensitive_word_data` VALUES ('伦理片');
INSERT INTO `sensitive_word_data` VALUES ('伦理电影');
INSERT INTO `sensitive_word_data` VALUES ('伦理电影qvod');
INSERT INTO `sensitive_word_data` VALUES ('伪民运');
INSERT INTO `sensitive_word_data` VALUES ('伪水');
INSERT INTO `sensitive_word_data` VALUES ('伪火');
INSERT INTO `sensitive_word_data` VALUES ('伪装美女');
INSERT INTO `sensitive_word_data` VALUES ('伯希来');
INSERT INTO `sensitive_word_data` VALUES ('伯母');
INSERT INTO `sensitive_word_data` VALUES ('伯父');
INSERT INTO `sensitive_word_data` VALUES ('伴侣');
INSERT INTO `sensitive_word_data` VALUES ('伴我淫');
INSERT INTO `sensitive_word_data` VALUES ('低价批发');
INSERT INTO `sensitive_word_data` VALUES ('低嚎');
INSERT INTO `sensitive_word_data` VALUES ('低潮期');
INSERT INTO `sensitive_word_data` VALUES ('住英国房');
INSERT INTO `sensitive_word_data` VALUES ('体检');
INSERT INTO `sensitive_word_data` VALUES ('体透视镜');
INSERT INTO `sensitive_word_data` VALUES ('何勇');
INSERT INTO `sensitive_word_data` VALUES ('何厚铧');
INSERT INTO `sensitive_word_data` VALUES ('何家栋');
INSERT INTO `sensitive_word_data` VALUES ('何德普');
INSERT INTO `sensitive_word_data` VALUES ('何晔晖');
INSERT INTO `sensitive_word_data` VALUES ('何柱国');
INSERT INTO `sensitive_word_data` VALUES ('何椿霖');
INSERT INTO `sensitive_word_data` VALUES ('何添发');
INSERT INTO `sensitive_word_data` VALUES ('何清涟');
INSERT INTO `sensitive_word_data` VALUES ('何祚庥');
INSERT INTO `sensitive_word_data` VALUES ('何鲁丽');
INSERT INTO `sensitive_word_data` VALUES ('何鸿燊');
INSERT INTO `sensitive_word_data` VALUES ('余国春');
INSERT INTO `sensitive_word_data` VALUES ('余秋里');
INSERT INTO `sensitive_word_data` VALUES ('余英时');
INSERT INTO `sensitive_word_data` VALUES ('余辉');
INSERT INTO `sensitive_word_data` VALUES ('佛同修');
INSERT INTO `sensitive_word_data` VALUES ('佛展千手法');
INSERT INTO `sensitive_word_data` VALUES ('佛山供应电警棍');
INSERT INTO `sensitive_word_data` VALUES ('佛怀煽仇录 ');
INSERT INTO `sensitive_word_data` VALUES ('作各种证');
INSERT INTO `sensitive_word_data` VALUES ('作弊器');
INSERT INTO `sensitive_word_data` VALUES ('作爱');
INSERT INTO `sensitive_word_data` VALUES ('作硝化甘');
INSERT INTO `sensitive_word_data` VALUES ('你他妈');
INSERT INTO `sensitive_word_data` VALUES ('你吗b');
INSERT INTO `sensitive_word_data` VALUES ('你妈的');
INSERT INTO `sensitive_word_data` VALUES ('你妈逼');
INSERT INTO `sensitive_word_data` VALUES ('你娘的');
INSERT INTO `sensitive_word_data` VALUES ('你怎么用土办法做武器');
INSERT INTO `sensitive_word_data` VALUES ('你爸');
INSERT INTO `sensitive_word_data` VALUES ('你的西域');
INSERT INTO `sensitive_word_data` VALUES ('你说我说论坛');
INSERT INTO `sensitive_word_data` VALUES ('你退了吗');
INSERT INTO `sensitive_word_data` VALUES ('你麻痹');
INSERT INTO `sensitive_word_data` VALUES ('佳静安定');
INSERT INTO `sensitive_word_data` VALUES ('佳静安定片 ');
INSERT INTO `sensitive_word_data` VALUES ('侄儿');
INSERT INTO `sensitive_word_data` VALUES ('侄女');
INSERT INTO `sensitive_word_data` VALUES ('侄子');
INSERT INTO `sensitive_word_data` VALUES ('供产');
INSERT INTO `sensitive_word_data` VALUES ('供应 一粒眠 红豆');
INSERT INTO `sensitive_word_data` VALUES ('供应 硝甲西泮 麻黄素');
INSERT INTO `sensitive_word_data` VALUES ('供应 纯冰 牙签');
INSERT INTO `sensitive_word_data` VALUES ('供应2010电警棍专卖');
INSERT INTO `sensitive_word_data` VALUES ('供应2011高考名单');
INSERT INTO `sensitive_word_data` VALUES ('供应K粉缅甸冰');
INSERT INTO `sensitive_word_data` VALUES ('供应三利达弓弩麻醉箭');
INSERT INTO `sensitive_word_data` VALUES ('供应三唑仑失身粉');
INSERT INTO `sensitive_word_data` VALUES ('供应三步倒麻醉箭');
INSERT INTO `sensitive_word_data` VALUES ('供应假幣');
INSERT INTO `sensitive_word_data` VALUES ('供应军刀');
INSERT INTO `sensitive_word_data` VALUES ('供应军刺');
INSERT INTO `sensitive_word_data` VALUES ('供应军刺军刀');
INSERT INTO `sensitive_word_data` VALUES ('供应军用弓弩专卖');
INSERT INTO `sensitive_word_data` VALUES ('供应军用弩折叠弩');
INSERT INTO `sensitive_word_data` VALUES ('供应军用手枪');
INSERT INTO `sensitive_word_data` VALUES ('供应发票 ');
INSERT INTO `sensitive_word_data` VALUES ('供应失身粉');
INSERT INTO `sensitive_word_data` VALUES ('供应女性催情药苍蝇粉');
INSERT INTO `sensitive_word_data` VALUES ('供应少女迷情药');
INSERT INTO `sensitive_word_data` VALUES ('供应弓弩');
INSERT INTO `sensitive_word_data` VALUES ('供应弓弩麻醉箭');
INSERT INTO `sensitive_word_data` VALUES ('供应弩捕狗箭');
INSERT INTO `sensitive_word_data` VALUES ('供应弩用麻醉箭');
INSERT INTO `sensitive_word_data` VALUES ('供应强效催眠催情水');
INSERT INTO `sensitive_word_data` VALUES ('供应拍肩迷药');
INSERT INTO `sensitive_word_data` VALUES ('供应氯胺酮 价格');
INSERT INTO `sensitive_word_data` VALUES ('供应汽枪');
INSERT INTO `sensitive_word_data` VALUES ('供应狙击钢珠专用弩');
INSERT INTO `sensitive_word_data` VALUES ('供应甲基安非他命 中间体');
INSERT INTO `sensitive_word_data` VALUES ('供应甲基苯丙胺 上海');
INSERT INTO `sensitive_word_data` VALUES ('供应盐酸羟亚胺 中间体');
INSERT INTO `sensitive_word_data` VALUES ('供应眼镜蛇弓努');
INSERT INTO `sensitive_word_data` VALUES ('供应硝甲西泮 咖啡因');
INSERT INTO `sensitive_word_data` VALUES ('供应硝酸胺炸药');
INSERT INTO `sensitive_word_data` VALUES ('供应秦氏弓弩');
INSERT INTO `sensitive_word_data` VALUES ('供应精品弓弩');
INSERT INTO `sensitive_word_data` VALUES ('供应纯缅麻古');
INSERT INTO `sensitive_word_data` VALUES ('供应美国GHB液');
INSERT INTO `sensitive_word_data` VALUES ('供应苍蝇水');
INSERT INTO `sensitive_word_data` VALUES ('供应苍蝇水迷奸药');
INSERT INTO `sensitive_word_data` VALUES ('供应蒙汗药GHB');
INSERT INTO `sensitive_word_data` VALUES ('供应蒙汗药苍蝇水');
INSERT INTO `sensitive_word_data` VALUES ('供应蓝精灵催情水');
INSERT INTO `sensitive_word_data` VALUES ('供应警用防身电击器');
INSERT INTO `sensitive_word_data` VALUES ('供应警用防身电击棒');
INSERT INTO `sensitive_word_data` VALUES ('供应赵氏弓努');
INSERT INTO `sensitive_word_data` VALUES ('供应赵氏弓驽');
INSERT INTO `sensitive_word_data` VALUES ('供应钢珠弓弩');
INSERT INTO `sensitive_word_data` VALUES ('供应钢珠气枪');
INSERT INTO `sensitive_word_data` VALUES ('供应防身警用电击器');
INSERT INTO `sensitive_word_data` VALUES ('供应防身警用电击棒');
INSERT INTO `sensitive_word_data` VALUES ('供应防身警用电警棍');
INSERT INTO `sensitive_word_data` VALUES ('供应阳江刀具');
INSERT INTO `sensitive_word_data` VALUES ('供应香水型迷奸药');
INSERT INTO `sensitive_word_data` VALUES ('供应高压防身电警棒');
INSERT INTO `sensitive_word_data` VALUES ('供应高爆炸药');
INSERT INTO `sensitive_word_data` VALUES ('供应麻古化学冰牙签');
INSERT INTO `sensitive_word_data` VALUES ('供应麻醉箭');
INSERT INTO `sensitive_word_data` VALUES ('供应麻醉箭三步倒');
INSERT INTO `sensitive_word_data` VALUES ('供应麻醉箭批发');
INSERT INTO `sensitive_word_data` VALUES ('供应麻黄素 上海');
INSERT INTO `sensitive_word_data` VALUES ('供应黄色炸药');
INSERT INTO `sensitive_word_data` VALUES ('供应黑寡妇催情水');
INSERT INTO `sensitive_word_data` VALUES ('供应黑火药');
INSERT INTO `sensitive_word_data` VALUES ('供应黑索金');
INSERT INTO `sensitive_word_data` VALUES ('供精');
INSERT INTO `sensitive_word_data` VALUES ('供铲党');
INSERT INTO `sensitive_word_data` VALUES ('供铲裆');
INSERT INTO `sensitive_word_data` VALUES ('供铲谠');
INSERT INTO `sensitive_word_data` VALUES ('侦探设备');
INSERT INTO `sensitive_word_data` VALUES ('侧臀');
INSERT INTO `sensitive_word_data` VALUES ('侮辱老师');
INSERT INTO `sensitive_word_data` VALUES ('侯义斌');
INSERT INTO `sensitive_word_data` VALUES ('侯杰');
INSERT INTO `sensitive_word_data` VALUES ('侯龙涛');
INSERT INTO `sensitive_word_data` VALUES ('侵犯国外专利');
INSERT INTO `sensitive_word_data` VALUES ('便宜出售手铐警服');
INSERT INTO `sensitive_word_data` VALUES ('便宜出售警服');
INSERT INTO `sensitive_word_data` VALUES ('便宜出售警用品');
INSERT INTO `sensitive_word_data` VALUES ('便宜出售警用物品');
INSERT INTO `sensitive_word_data` VALUES ('便宜售警用物品');
INSERT INTO `sensitive_word_data` VALUES ('便携式弓弩');
INSERT INTO `sensitive_word_data` VALUES ('促使');
INSERT INTO `sensitive_word_data` VALUES ('促性腺激素');
INSERT INTO `sensitive_word_data` VALUES ('促红细胞生成素');
INSERT INTO `sensitive_word_data` VALUES ('俄国');
INSERT INTO `sensitive_word_data` VALUES ('俄罗斯轮盘 ');
INSERT INTO `sensitive_word_data` VALUES ('俄羅斯');
INSERT INTO `sensitive_word_data` VALUES ('俊逸');
INSERT INTO `sensitive_word_data` VALUES ('俊龙百美缘下载');
INSERT INTO `sensitive_word_data` VALUES ('俏丽');
INSERT INTO `sensitive_word_data` VALUES ('俏眼');
INSERT INTO `sensitive_word_data` VALUES ('俏脸');
INSERT INTO `sensitive_word_data` VALUES ('保持');
INSERT INTO `sensitive_word_data` VALUES ('保精');
INSERT INTO `sensitive_word_data` VALUES ('保育细胞');
INSERT INTO `sensitive_word_data` VALUES ('保过答案');
INSERT INTO `sensitive_word_data` VALUES ('保钓');
INSERT INTO `sensitive_word_data` VALUES ('俞云波');
INSERT INTO `sensitive_word_data` VALUES ('俞正');
INSERT INTO `sensitive_word_data` VALUES ('俞正声 ');
INSERT INTO `sensitive_word_data` VALUES ('俞泽猷');
INSERT INTO `sensitive_word_data` VALUES ('信号屏蔽器');
INSERT INTO `sensitive_word_data` VALUES ('信号拦截器');
INSERT INTO `sensitive_word_data` VALUES ('信息群发软件');
INSERT INTO `sensitive_word_data` VALUES ('信接收器');
INSERT INTO `sensitive_word_data` VALUES ('信春鹰');
INSERT INTO `sensitive_word_data` VALUES ('信用卡套现 ');
INSERT INTO `sensitive_word_data` VALUES ('信用危机');
INSERT INTO `sensitive_word_data` VALUES ('信访专班');
INSERT INTO `sensitive_word_data` VALUES ('俪影蝎心');
INSERT INTO `sensitive_word_data` VALUES ('修炼');
INSERT INTO `sensitive_word_data` VALUES ('修炼之歌');
INSERT INTO `sensitive_word_data` VALUES ('修煉');
INSERT INTO `sensitive_word_data` VALUES ('俯弄');
INSERT INTO `sensitive_word_data` VALUES ('倉持茜种子');
INSERT INTO `sensitive_word_data` VALUES ('倉木杏种子');
INSERT INTO `sensitive_word_data` VALUES ('倒退');
INSERT INTO `sensitive_word_data` VALUES ('倒骑');
INSERT INTO `sensitive_word_data` VALUES ('倚天屠龙别记');
INSERT INTO `sensitive_word_data` VALUES ('倪国熙');
INSERT INTO `sensitive_word_data` VALUES ('倪岳峰');
INSERT INTO `sensitive_word_data` VALUES ('倪志福');
INSERT INTO `sensitive_word_data` VALUES ('倪润峰');
INSERT INTO `sensitive_word_data` VALUES ('倪育贤');
INSERT INTO `sensitive_word_data` VALUES ('倫公');
INSERT INTO `sensitive_word_data` VALUES ('倫功');
INSERT INTO `sensitive_word_data` VALUES ('倫攻');
INSERT INTO `sensitive_word_data` VALUES ('债务公司 ');
INSERT INTO `sensitive_word_data` VALUES ('假发票 ');
INSERT INTO `sensitive_word_data` VALUES ('假币');
INSERT INTO `sensitive_word_data` VALUES ('假币 \"视频看货\"');
INSERT INTO `sensitive_word_data` VALUES ('假币(台湾最新版)');
INSERT INTO `sensitive_word_data` VALUES ('假币代理');
INSERT INTO `sensitive_word_data` VALUES ('假币假钞假钱QQ');
INSERT INTO `sensitive_word_data` VALUES ('假币出售');
INSERT INTO `sensitive_word_data` VALUES ('假币批发');
INSERT INTO `sensitive_word_data` VALUES ('假币销售 高仿真');
INSERT INTO `sensitive_word_data` VALUES ('假性湿疣');
INSERT INTO `sensitive_word_data` VALUES ('假教育');
INSERT INTO `sensitive_word_data` VALUES ('假文凭 ');
INSERT INTO `sensitive_word_data` VALUES ('假比出售');
INSERT INTO `sensitive_word_data` VALUES ('假硬币QQ');
INSERT INTO `sensitive_word_data` VALUES ('假装');
INSERT INTO `sensitive_word_data` VALUES ('假证件 ');
INSERT INTO `sensitive_word_data` VALUES ('假鈔');
INSERT INTO `sensitive_word_data` VALUES ('假钞');
INSERT INTO `sensitive_word_data` VALUES ('假钞 \"凹凸质感\"');
INSERT INTO `sensitive_word_data` VALUES ('假钞 一元硬币');
INSERT INTO `sensitive_word_data` VALUES ('假钞 迷奸药');
INSERT INTO `sensitive_word_data` VALUES ('假钞交换QQ');
INSERT INTO `sensitive_word_data` VALUES ('假钞交易');
INSERT INTO `sensitive_word_data` VALUES ('假钞存进ATM机');
INSERT INTO `sensitive_word_data` VALUES ('假钞换真钱');
INSERT INTO `sensitive_word_data` VALUES ('假钞换钱');
INSERT INTO `sensitive_word_data` VALUES ('假钱 热敷敏纸');
INSERT INTO `sensitive_word_data` VALUES ('假钱 货到付款');
INSERT INTO `sensitive_word_data` VALUES ('假钱=假币');
INSERT INTO `sensitive_word_data` VALUES ('假钱QQ');
INSERT INTO `sensitive_word_data` VALUES ('假钱买卖');
INSERT INTO `sensitive_word_data` VALUES ('假钱代理');
INSERT INTO `sensitive_word_data` VALUES ('假钱价格');
INSERT INTO `sensitive_word_data` VALUES ('假钱出售 北京');
INSERT INTO `sensitive_word_data` VALUES ('假钱出售一元假硬');
INSERT INTO `sensitive_word_data` VALUES ('假钱批发');
INSERT INTO `sensitive_word_data` VALUES ('做人不能太cctv了');
INSERT INTO `sensitive_word_data` VALUES ('做原子弹');
INSERT INTO `sensitive_word_data` VALUES ('做台 ');
INSERT INTO `sensitive_word_data` VALUES ('做炸弹');
INSERT INTO `sensitive_word_data` VALUES ('做爱');
INSERT INTO `sensitive_word_data` VALUES ('做爱之道');
INSERT INTO `sensitive_word_data` VALUES ('做爱前');
INSERT INTO `sensitive_word_data` VALUES ('做爱后');
INSERT INTO `sensitive_word_data` VALUES ('做爱小');
INSERT INTO `sensitive_word_data` VALUES ('做爱挑逗');
INSERT INTO `sensitive_word_data` VALUES ('做爱时间');
INSERT INTO `sensitive_word_data` VALUES ('做爱经验');
INSERT INTO `sensitive_word_data` VALUES ('做爱节奏');
INSERT INTO `sensitive_word_data` VALUES ('做证件');
INSERT INTO `sensitive_word_data` VALUES ('做雞');
INSERT INTO `sensitive_word_data` VALUES ('做鸡');
INSERT INTO `sensitive_word_data` VALUES ('做鸭');
INSERT INTO `sensitive_word_data` VALUES ('偵探設備');
INSERT INTO `sensitive_word_data` VALUES ('偷 拍');
INSERT INTO `sensitive_word_data` VALUES ('偷偷贪');
INSERT INTO `sensitive_word_data` VALUES ('偷听器');
INSERT INTO `sensitive_word_data` VALUES ('偷情');
INSERT INTO `sensitive_word_data` VALUES ('偷拍');
INSERT INTO `sensitive_word_data` VALUES ('偷欢');
INSERT INTO `sensitive_word_data` VALUES ('偷电');
INSERT INTO `sensitive_word_data` VALUES ('偷窥');
INSERT INTO `sensitive_word_data` VALUES ('偷窥有罪');
INSERT INTO `sensitive_word_data` VALUES ('偷窺有罪');
INSERT INTO `sensitive_word_data` VALUES ('偷肃贪');
INSERT INTO `sensitive_word_data` VALUES ('偷電器');
INSERT INTO `sensitive_word_data` VALUES ('偽裝美女');
INSERT INTO `sensitive_word_data` VALUES ('傅全有');
INSERT INTO `sensitive_word_data` VALUES ('傅家祥');
INSERT INTO `sensitive_word_data` VALUES ('傅志寰');
INSERT INTO `sensitive_word_data` VALUES ('傅怡彬');
INSERT INTO `sensitive_word_data` VALUES ('傅惠民');
INSERT INTO `sensitive_word_data` VALUES ('傅成玉');
INSERT INTO `sensitive_word_data` VALUES ('傅杰');
INSERT INTO `sensitive_word_data` VALUES ('傅申奇');
INSERT INTO `sensitive_word_data` VALUES ('傅铁山');
INSERT INTO `sensitive_word_data` VALUES ('傅锐');
INSERT INTO `sensitive_word_data` VALUES ('储波');
INSERT INTO `sensitive_word_data` VALUES ('催情');
INSERT INTO `sensitive_word_data` VALUES ('催情春药');
INSERT INTO `sensitive_word_data` VALUES ('催情水');
INSERT INTO `sensitive_word_data` VALUES ('催情液');
INSERT INTO `sensitive_word_data` VALUES ('催情粉');
INSERT INTO `sensitive_word_data` VALUES ('催情药');
INSERT INTO `sensitive_word_data` VALUES ('催情藥');
INSERT INTO `sensitive_word_data` VALUES ('催情迷昏药');
INSERT INTO `sensitive_word_data` VALUES ('催泪弹');
INSERT INTO `sensitive_word_data` VALUES ('催泪枪');
INSERT INTO `sensitive_word_data` VALUES ('催眠催情水');
INSERT INTO `sensitive_word_data` VALUES ('催眠水');
INSERT INTO `sensitive_word_data` VALUES ('傲然挺立');
INSERT INTO `sensitive_word_data` VALUES ('傻B');
INSERT INTO `sensitive_word_data` VALUES ('傻X');
INSERT INTO `sensitive_word_data` VALUES ('傻比');
INSERT INTO `sensitive_word_data` VALUES ('傻逼');
INSERT INTO `sensitive_word_data` VALUES ('僵贼');
INSERT INTO `sensitive_word_data` VALUES ('僵贼民');
INSERT INTO `sensitive_word_data` VALUES ('儿园凶');
INSERT INTO `sensitive_word_data` VALUES ('儿园惨');
INSERT INTO `sensitive_word_data` VALUES ('儿园杀');
INSERT INTO `sensitive_word_data` VALUES ('儿园砍');
INSERT INTO `sensitive_word_data` VALUES ('儿媳妇');
INSERT INTO `sensitive_word_data` VALUES ('允吸');
INSERT INTO `sensitive_word_data` VALUES ('元一夜 ');
INSERT INTO `sensitive_word_data` VALUES ('元极功 ');
INSERT INTO `sensitive_word_data` VALUES ('充气娃');
INSERT INTO `sensitive_word_data` VALUES ('充血');
INSERT INTO `sensitive_word_data` VALUES ('先天健康法');
INSERT INTO `sensitive_word_data` VALUES ('先烈的电电');
INSERT INTO `sensitive_word_data` VALUES ('先烈的电话');
INSERT INTO `sensitive_word_data` VALUES ('先烈纷纷');
INSERT INTO `sensitive_word_data` VALUES ('先肾后心');
INSERT INTO `sensitive_word_data` VALUES ('光学真题');
INSERT INTO `sensitive_word_data` VALUES ('光洁无毛');
INSERT INTO `sensitive_word_data` VALUES ('光溜溜');
INSERT INTO `sensitive_word_data` VALUES ('光盘与真相');
INSERT INTO `sensitive_word_data` VALUES ('光端机');
INSERT INTO `sensitive_word_data` VALUES ('光脱脱');
INSERT INTO `sensitive_word_data` VALUES ('光裸');
INSERT INTO `sensitive_word_data` VALUES ('克分析');
INSERT INTO `sensitive_word_data` VALUES ('克千术');
INSERT INTO `sensitive_word_data` VALUES ('克尤木·巴吾东');
INSERT INTO `sensitive_word_data` VALUES ('克透视');
INSERT INTO `sensitive_word_data` VALUES ('免抵押贷款');
INSERT INTO `sensitive_word_data` VALUES ('免疫性不孕');
INSERT INTO `sensitive_word_data` VALUES ('免费偷拍');
INSERT INTO `sensitive_word_data` VALUES ('免费淫电影');
INSERT INTO `sensitive_word_data` VALUES ('免费电影 ');
INSERT INTO `sensitive_word_data` VALUES ('党产共');
INSERT INTO `sensitive_word_data` VALUES ('党保平安');
INSERT INTO `sensitive_word_data` VALUES ('党内内部资料');
INSERT INTO `sensitive_word_data` VALUES ('党内分裂');
INSERT INTO `sensitive_word_data` VALUES ('党内危机');
INSERT INTO `sensitive_word_data` VALUES ('党内权争');
INSERT INTO `sensitive_word_data` VALUES ('党内权力');
INSERT INTO `sensitive_word_data` VALUES ('党内言事潮');
INSERT INTO `sensitive_word_data` VALUES ('党前干劲');
INSERT INTO `sensitive_word_data` VALUES ('党后萎');
INSERT INTO `sensitive_word_data` VALUES ('党国');
INSERT INTO `sensitive_word_data` VALUES ('党妈妈');
INSERT INTO `sensitive_word_data` VALUES ('党委书记');
INSERT INTO `sensitive_word_data` VALUES ('党政人事');
INSERT INTO `sensitive_word_data` VALUES ('党文化');
INSERT INTO `sensitive_word_data` VALUES ('党棍');
INSERT INTO `sensitive_word_data` VALUES ('党的官');
INSERT INTO `sensitive_word_data` VALUES ('党禁');
INSERT INTO `sensitive_word_data` VALUES ('党鞭');
INSERT INTO `sensitive_word_data` VALUES ('党魁');
INSERT INTO `sensitive_word_data` VALUES ('入党申请书');
INSERT INTO `sensitive_word_data` VALUES ('全国*揭晓二奶');
INSERT INTO `sensitive_word_data` VALUES ('全国两会');
INSERT INTO `sensitive_word_data` VALUES ('全国二奶');
INSERT INTO `sensitive_word_data` VALUES ('全国二奶大奖赛');
INSERT INTO `sensitive_word_data` VALUES ('全国二奶大赛');
INSERT INTO `sensitive_word_data` VALUES ('全国各地办证');
INSERT INTO `sensitive_word_data` VALUES ('全国最低价');
INSERT INTO `sensitive_word_data` VALUES ('全国联保');
INSERT INTO `sensitive_word_data` VALUES ('全国退党');
INSERT INTO `sensitive_word_data` VALUES ('全國二奶');
INSERT INTO `sensitive_word_data` VALUES ('全家不得好死');
INSERT INTO `sensitive_word_data` VALUES ('全家死光');
INSERT INTO `sensitive_word_data` VALUES ('全家死绝');
INSERT INTO `sensitive_word_data` VALUES ('全真证');
INSERT INTO `sensitive_word_data` VALUES ('全范围教会 ');
INSERT INTO `sensitive_word_data` VALUES ('全裸');
INSERT INTO `sensitive_word_data` VALUES ('全金属仿真枪专卖');
INSERT INTO `sensitive_word_data` VALUES ('全钢刀具QQ');
INSERT INTO `sensitive_word_data` VALUES ('兩性淫亂');
INSERT INTO `sensitive_word_data` VALUES ('八九');
INSERT INTO `sensitive_word_data` VALUES ('八九六');
INSERT INTO `sensitive_word_data` VALUES ('八九学');
INSERT INTO `sensitive_word_data` VALUES ('八九学潮');
INSERT INTO `sensitive_word_data` VALUES ('八九政治');
INSERT INTO `sensitive_word_data` VALUES ('八九民');
INSERT INTO `sensitive_word_data` VALUES ('八九见证');
INSERT INTO `sensitive_word_data` VALUES ('八级地震毫无预报');
INSERT INTO `sensitive_word_data` VALUES ('八老');
INSERT INTO `sensitive_word_data` VALUES ('八荣八耻');
INSERT INTO `sensitive_word_data` VALUES ('公产专制');
INSERT INTO `sensitive_word_data` VALUES ('公产党');
INSERT INTO `sensitive_word_data` VALUES ('公产小丑');
INSERT INTO `sensitive_word_data` VALUES ('公产王朝');
INSERT INTO `sensitive_word_data` VALUES ('公公');
INSERT INTO `sensitive_word_data` VALUES ('公务员工资');
INSERT INTO `sensitive_word_data` VALUES ('公务员的工资');
INSERT INTO `sensitive_word_data` VALUES ('公务员调资');
INSERT INTO `sensitive_word_data` VALUES ('公司常年出售假钱');
INSERT INTO `sensitive_word_data` VALUES ('公子党');
INSERT INTO `sensitive_word_data` VALUES ('公子族');
INSERT INTO `sensitive_word_data` VALUES ('公安');
INSERT INTO `sensitive_word_data` VALUES ('公安与武警的区别');
INSERT INTO `sensitive_word_data` VALUES ('公安把秩序搞乱');
INSERT INTO `sensitive_word_data` VALUES ('公安文件');
INSERT INTO `sensitive_word_data` VALUES ('公安网监');
INSERT INTO `sensitive_word_data` VALUES ('公安部');
INSERT INTO `sensitive_word_data` VALUES ('公安错打');
INSERT INTO `sensitive_word_data` VALUES ('公开小姐');
INSERT INTO `sensitive_word_data` VALUES ('公检法是流氓');
INSERT INTO `sensitive_word_data` VALUES ('公款');
INSERT INTO `sensitive_word_data` VALUES ('公民大联盟');
INSERT INTO `sensitive_word_data` VALUES ('六HE彩 ');
INSERT INTO `sensitive_word_data` VALUES ('六●四');
INSERT INTO `sensitive_word_data` VALUES ('六一零');
INSERT INTO `sensitive_word_data` VALUES ('六代接班人');
INSERT INTO `sensitive_word_data` VALUES ('六决不');
INSERT INTO `sensitive_word_data` VALUES ('六合 ');
INSERT INTO `sensitive_word_data` VALUES ('六合彩');
INSERT INTO `sensitive_word_data` VALUES ('六合采 ');
INSERT INTO `sensitive_word_data` VALUES ('六和彩');
INSERT INTO `sensitive_word_data` VALUES ('六四');
INSERT INTO `sensitive_word_data` VALUES ('六四之役');
INSERT INTO `sensitive_word_data` VALUES ('六四事');
INSERT INTO `sensitive_word_data` VALUES ('六四事件');
INSERT INTO `sensitive_word_data` VALUES ('六四受难者家属证辞');
INSERT INTO `sensitive_word_data` VALUES ('六四学潮');
INSERT INTO `sensitive_word_data` VALUES ('六四宣言');
INSERT INTO `sensitive_word_data` VALUES ('六四屠城');
INSERT INTO `sensitive_word_data` VALUES ('六四屠杀');
INSERT INTO `sensitive_word_data` VALUES ('六四政变');
INSERT INTO `sensitive_word_data` VALUES ('六四血案');
INSERT INTO `sensitive_word_data` VALUES ('六四资料馆');
INSERT INTO `sensitive_word_data` VALUES ('六四运动');
INSERT INTO `sensitive_word_data` VALUES ('六四青年网');
INSERT INTO `sensitive_word_data` VALUES ('六开网');
INSERT INTO `sensitive_word_data` VALUES ('六彩');
INSERT INTO `sensitive_word_data` VALUES ('六星合彩');
INSERT INTO `sensitive_word_data` VALUES ('六月联盟');
INSERT INTO `sensitive_word_data` VALUES ('六月聯盟');
INSERT INTO `sensitive_word_data` VALUES ('六月飞雪');
INSERT INTO `sensitive_word_data` VALUES ('六死');
INSERT INTO `sensitive_word_data` VALUES ('六级答案');
INSERT INTO `sensitive_word_data` VALUES ('兰州办证');
INSERT INTO `sensitive_word_data` VALUES ('兰州办证服务中心');
INSERT INTO `sensitive_word_data` VALUES ('兰州网上办证');
INSERT INTO `sensitive_word_data` VALUES ('兰香');
INSERT INTO `sensitive_word_data` VALUES ('共+产+党');
INSERT INTO `sensitive_word_data` VALUES ('共+產+黨');
INSERT INTO `sensitive_word_data` VALUES ('共+铲+党');
INSERT INTO `sensitive_word_data` VALUES ('共.{6}产.{6}党');
INSERT INTO `sensitive_word_data` VALUES ('共.{6}產.{6}黨');
INSERT INTO `sensitive_word_data` VALUES ('共.{6}铲.{6}党');
INSERT INTO `sensitive_word_data` VALUES ('共.产.党');
INSERT INTO `sensitive_word_data` VALUES ('共.产党');
INSERT INTO `sensitive_word_data` VALUES ('共chang党');
INSERT INTO `sensitive_word_data` VALUES ('共c党');
INSERT INTO `sensitive_word_data` VALUES ('共x党');
INSERT INTO `sensitive_word_data` VALUES ('共一产一党');
INSERT INTO `sensitive_word_data` VALUES ('共产');
INSERT INTO `sensitive_word_data` VALUES ('共产.党');
INSERT INTO `sensitive_word_data` VALUES ('共产专制');
INSERT INTO `sensitive_word_data` VALUES ('共产主义');
INSERT INTO `sensitive_word_data` VALUES ('共产主义的幽灵');
INSERT INTO `sensitive_word_data` VALUES ('共产主义黑皮书');
INSERT INTO `sensitive_word_data` VALUES ('共产党');
INSERT INTO `sensitive_word_data` VALUES ('共产党的报应');
INSERT INTO `sensitive_word_data` VALUES ('共产党的末日');
INSERT INTO `sensitive_word_data` VALUES ('共产小丑');
INSERT INTO `sensitive_word_data` VALUES ('共产无赖');
INSERT INTO `sensitive_word_data` VALUES ('共产极权');
INSERT INTO `sensitive_word_data` VALUES ('共产王朝');
INSERT INTO `sensitive_word_data` VALUES ('共党');
INSERT INTO `sensitive_word_data` VALUES ('共军');
INSERT INTO `sensitive_word_data` VALUES ('共匪');
INSERT INTO `sensitive_word_data` VALUES ('共匪=共框非');
INSERT INTO `sensitive_word_data` VALUES ('共和国之怒');
INSERT INTO `sensitive_word_data` VALUES ('共奴');
INSERT INTO `sensitive_word_data` VALUES ('共惨');
INSERT INTO `sensitive_word_data` VALUES ('共惨党');
INSERT INTO `sensitive_word_data` VALUES ('共残主义');
INSERT INTO `sensitive_word_data` VALUES ('共残党');
INSERT INTO `sensitive_word_data` VALUES ('共残裆');
INSERT INTO `sensitive_word_data` VALUES ('共浴');
INSERT INTO `sensitive_word_data` VALUES ('共狗');
INSERT INTO `sensitive_word_data` VALUES ('共王储');
INSERT INTO `sensitive_word_data` VALUES ('共贪党');
INSERT INTO `sensitive_word_data` VALUES ('共贼=共则');
INSERT INTO `sensitive_word_data` VALUES ('共铲');
INSERT INTO `sensitive_word_data` VALUES ('共铲党');
INSERT INTO `sensitive_word_data` VALUES ('共黨');
INSERT INTO `sensitive_word_data` VALUES ('关于做好定案材料工作的意见');
INSERT INTO `sensitive_word_data` VALUES ('关于堂明皇与杨贵姬');
INSERT INTO `sensitive_word_data` VALUES ('关于对敌斗争中有关政政策界限汇编');
INSERT INTO `sensitive_word_data` VALUES ('关六如');
INSERT INTO `sensitive_word_data` VALUES ('关卓中');
INSERT INTO `sensitive_word_data` VALUES ('关塔摩 ');
INSERT INTO `sensitive_word_data` VALUES ('关机房');
INSERT INTO `sensitive_word_data` VALUES ('兴中心幼');
INSERT INTO `sensitive_word_data` VALUES ('兴华论谈');
INSERT INTO `sensitive_word_data` VALUES ('兴奋剂');
INSERT INTO `sensitive_word_data` VALUES ('兵种教材');
INSERT INTO `sensitive_word_data` VALUES ('具有中国特色的魔鬼辞典');
INSERT INTO `sensitive_word_data` VALUES ('养殖户的求救书');
INSERT INTO `sensitive_word_data` VALUES ('兼职');
INSERT INTO `sensitive_word_data` VALUES ('兼职上门');
INSERT INTO `sensitive_word_data` VALUES ('兽交');
INSERT INTO `sensitive_word_data` VALUES ('兽奸');
INSERT INTO `sensitive_word_data` VALUES ('兽性');
INSERT INTO `sensitive_word_data` VALUES ('兽用麻醉箭');
INSERT INTO `sensitive_word_data` VALUES ('内争人权');
INSERT INTO `sensitive_word_data` VALUES ('内壁');
INSERT INTO `sensitive_word_data` VALUES ('内射');
INSERT INTO `sensitive_word_data` VALUES ('内斗退党');
INSERT INTO `sensitive_word_data` VALUES ('内生殖器');
INSERT INTO `sensitive_word_data` VALUES ('内睾提肌');
INSERT INTO `sensitive_word_data` VALUES ('内裤');
INSERT INTO `sensitive_word_data` VALUES ('冈本真');
INSERT INTO `sensitive_word_data` VALUES ('冒水');
INSERT INTO `sensitive_word_data` VALUES ('写两会');
INSERT INTO `sensitive_word_data` VALUES ('写的炸弹制作教程');
INSERT INTO `sensitive_word_data` VALUES ('写给十七大人大代表一封信');
INSERT INTO `sensitive_word_data` VALUES ('军事地图');
INSERT INTO `sensitive_word_data` VALUES ('军事标号');
INSERT INTO `sensitive_word_data` VALUES ('军事训练大纲');
INSERT INTO `sensitive_word_data` VALUES ('军事训练评定标准');
INSERT INTO `sensitive_word_data` VALUES ('军刀 卖');
INSERT INTO `sensitive_word_data` VALUES ('军刀专 卖');
INSERT INTO `sensitive_word_data` VALUES ('军刀专卖QQ');
INSERT INTO `sensitive_word_data` VALUES ('军刀专卖军刺');
INSERT INTO `sensitive_word_data` VALUES ('军刀专卖刀具批发');
INSERT INTO `sensitive_word_data` VALUES ('军刀专卖网');
INSERT INTO `sensitive_word_data` VALUES ('军刀价格军用刀具');
INSERT INTO `sensitive_word_data` VALUES ('军刀军刺甩棍');
INSERT INTO `sensitive_word_data` VALUES ('军刀军品网');
INSERT INTO `sensitive_word_data` VALUES ('军刀匕首直销网');
INSERT INTO `sensitive_word_data` VALUES ('军刀图片砍刀');
INSERT INTO `sensitive_word_data` VALUES ('军刀甩棍销售');
INSERT INTO `sensitive_word_data` VALUES ('军刀电棍销售');
INSERT INTO `sensitive_word_data` VALUES ('军刀直刀专卖');
INSERT INTO `sensitive_word_data` VALUES ('军刀直刀军品网');
INSERT INTO `sensitive_word_data` VALUES ('军刀直销网');
INSERT INTO `sensitive_word_data` VALUES ('军刀网');
INSERT INTO `sensitive_word_data` VALUES ('军刀网军刀专卖');
INSERT INTO `sensitive_word_data` VALUES ('军刺');
INSERT INTO `sensitive_word_data` VALUES ('军刺 卖');
INSERT INTO `sensitive_word_data` VALUES ('军刺野营砍刀出售');
INSERT INTO `sensitive_word_data` VALUES ('军品军刺网');
INSERT INTO `sensitive_word_data` VALUES ('军品特');
INSERT INTO `sensitive_word_data` VALUES ('军妓');
INSERT INTO `sensitive_word_data` VALUES ('军委公开信');
INSERT INTO `sensitive_word_data` VALUES ('军政名单');
INSERT INTO `sensitive_word_data` VALUES ('军火');
INSERT INTO `sensitive_word_data` VALUES ('军火价格');
INSERT INTO `sensitive_word_data` VALUES ('军用54出售');
INSERT INTO `sensitive_word_data` VALUES ('军用77出售');
INSERT INTO `sensitive_word_data` VALUES ('军用刀具军品网');
INSERT INTO `sensitive_word_data` VALUES ('军用地图');
INSERT INTO `sensitive_word_data` VALUES ('军用弓弩专卖店');
INSERT INTO `sensitive_word_data` VALUES ('军用弓弩专卖网');
INSERT INTO `sensitive_word_data` VALUES ('军用弓弩公司');
INSERT INTO `sensitive_word_data` VALUES ('军用弓弩批发');
INSERT INTO `sensitive_word_data` VALUES ('军用手');
INSERT INTO `sensitive_word_data` VALUES ('军用手枪');
INSERT INTO `sensitive_word_data` VALUES ('军用手枪钢珠枪');
INSERT INTO `sensitive_word_data` VALUES ('军用教材');
INSERT INTO `sensitive_word_data` VALUES ('军用钢珠弩');
INSERT INTO `sensitive_word_data` VALUES ('军转安置');
INSERT INTO `sensitive_word_data` VALUES ('军转干部');
INSERT INTO `sensitive_word_data` VALUES ('军长发威');
INSERT INTO `sensitive_word_data` VALUES ('军队干部转业复员工作文件汇编');
INSERT INTO `sensitive_word_data` VALUES ('军队走私');
INSERT INTO `sensitive_word_data` VALUES ('军需物质保管');
INSERT INTO `sensitive_word_data` VALUES ('军需物资保管');
INSERT INTO `sensitive_word_data` VALUES ('农夫导航最新地址');
INSERT INTO `sensitive_word_data` VALUES ('农民真正贫困的根源');
INSERT INTO `sensitive_word_data` VALUES ('冠状沟');
INSERT INTO `sensitive_word_data` VALUES ('冤民大');
INSERT INTO `sensitive_word_data` VALUES ('冯东海');
INSERT INTO `sensitive_word_data` VALUES ('冯培恩');
INSERT INTO `sensitive_word_data` VALUES ('冯素英');
INSERT INTO `sensitive_word_data` VALUES ('冰在火上');
INSERT INTO `sensitive_word_data` VALUES ('冰毒');
INSERT INTO `sensitive_word_data` VALUES ('冰毒 手枪 K粉 麻古');
INSERT INTO `sensitive_word_data` VALUES ('冰毒 真正的卖家');
INSERT INTO `sensitive_word_data` VALUES ('冰毒 麻古配方');
INSERT INTO `sensitive_word_data` VALUES ('冰毒买卖 供应 K粉');
INSERT INTO `sensitive_word_data` VALUES ('冰毒价格 好货 安全');
INSERT INTO `sensitive_word_data` VALUES ('冰毒出售 上海');
INSERT INTO `sensitive_word_data` VALUES ('冰毒出售海洛因手槍');
INSERT INTO `sensitive_word_data` VALUES ('冰毒制作配方');
INSERT INTO `sensitive_word_data` VALUES ('冰毒的真正賣傢 冰毒什麼價格');
INSERT INTO `sensitive_word_data` VALUES ('冰毒配方');
INSERT INTO `sensitive_word_data` VALUES ('冰毒配方 植物冰成品');
INSERT INTO `sensitive_word_data` VALUES ('冰淫传');
INSERT INTO `sensitive_word_data` VALUES ('冰火');
INSERT INTO `sensitive_word_data` VALUES ('冰火九重');
INSERT INTO `sensitive_word_data` VALUES ('冰火佳');
INSERT INTO `sensitive_word_data` VALUES ('冰火毒');
INSERT INTO `sensitive_word_data` VALUES ('冰火漫');
INSERT INTO `sensitive_word_data` VALUES ('冲凉死');
INSERT INTO `sensitive_word_data` VALUES ('冲刺');
INSERT INTO `sensitive_word_data` VALUES ('冲插');
INSERT INTO `sensitive_word_data` VALUES ('冷宽');
INSERT INTO `sensitive_word_data` VALUES ('冷溶');
INSERT INTO `sensitive_word_data` VALUES ('冷钢刀具专卖');
INSERT INTO `sensitive_word_data` VALUES ('冷钢刀具军品网');
INSERT INTO `sensitive_word_data` VALUES ('冷钢刀具直销网');
INSERT INTO `sensitive_word_data` VALUES ('冷阴症');
INSERT INTO `sensitive_word_data` VALUES ('净水器');
INSERT INTO `sensitive_word_data` VALUES ('净白体');
INSERT INTO `sensitive_word_data` VALUES ('准备');
INSERT INTO `sensitive_word_data` VALUES ('凌 辱');
INSERT INTO `sensitive_word_data` VALUES ('凌乱');
INSERT INTO `sensitive_word_data` VALUES ('凌虐玉门');
INSERT INTO `sensitive_word_data` VALUES ('凌辱');
INSERT INTO `sensitive_word_data` VALUES ('凌锋');
INSERT INTO `sensitive_word_data` VALUES ('减少');
INSERT INTO `sensitive_word_data` VALUES ('凯他敏');
INSERT INTO `sensitive_word_data` VALUES ('凹晕');
INSERT INTO `sensitive_word_data` VALUES ('出千');
INSERT INTO `sensitive_word_data` VALUES ('出售 \"植药物冰\"');
INSERT INTO `sensitive_word_data` VALUES ('出售 \"水印与金属线\"');
INSERT INTO `sensitive_word_data` VALUES ('出售 \"罂粟壳\"');
INSERT INTO `sensitive_word_data` VALUES ('出售 三唑仑');
INSERT INTO `sensitive_word_data` VALUES ('出售 假币公司');
INSERT INTO `sensitive_word_data` VALUES ('出售 冰毒 纯度99% K粉');
INSERT INTO `sensitive_word_data` VALUES ('出售 化学冰 纯度');
INSERT INTO `sensitive_word_data` VALUES ('出售 批发 化学合成冰');
INSERT INTO `sensitive_word_data` VALUES ('出售 植物冰 化学合成冰');
INSERT INTO `sensitive_word_data` VALUES ('出售 白牙签 黄牙签');
INSERT INTO `sensitive_word_data` VALUES ('出售 麻黄素 纯度');
INSERT INTO `sensitive_word_data` VALUES ('出售07年6月四级答案OR出售四级答案六级答案');
INSERT INTO `sensitive_word_data` VALUES ('出售11年高考考生信息');
INSERT INTO `sensitive_word_data` VALUES ('出售18P2P账号');
INSERT INTO `sensitive_word_data` VALUES ('出售2011年全国高考生名单');
INSERT INTO `sensitive_word_data` VALUES ('出售54式手枪');
INSERT INTO `sensitive_word_data` VALUES ('出售:高仿真枪');
INSERT INTO `sensitive_word_data` VALUES ('出售C1E假币');
INSERT INTO `sensitive_word_data` VALUES ('出售GHB迷奸药水');
INSERT INTO `sensitive_word_data` VALUES ('出售ghb迷情药');
INSERT INTO `sensitive_word_data` VALUES ('出售JIA币');
INSERT INTO `sensitive_word_data` VALUES ('出售K粉 广州');
INSERT INTO `sensitive_word_data` VALUES ('出售K粉麻古');
INSERT INTO `sensitive_word_data` VALUES ('出售M1911');
INSERT INTO `sensitive_word_data` VALUES ('出售{5}假{2}币{5}假{2}钞');
INSERT INTO `sensitive_word_data` VALUES ('出售{5}假{2}钞');
INSERT INTO `sensitive_word_data` VALUES ('出售{5}冰{4}毒');
INSERT INTO `sensitive_word_data` VALUES ('出售{5}手{5}枪');
INSERT INTO `sensitive_word_data` VALUES ('出售{5}手{5}槍');
INSERT INTO `sensitive_word_data` VALUES ('出售一元人民币');
INSERT INTO `sensitive_word_data` VALUES ('出售一元模具');
INSERT INTO `sensitive_word_data` VALUES ('出售一元硬币 \"版本为最新版本\"');
INSERT INTO `sensitive_word_data` VALUES ('出售三唑仑失身粉');
INSERT INTO `sensitive_word_data` VALUES ('出售三唑仑蒙汗药');
INSERT INTO `sensitive_word_data` VALUES ('出售三棱刀');
INSERT INTO `sensitive_word_data` VALUES ('出售上海高考生信息');
INSERT INTO `sensitive_word_data` VALUES ('出售二手走私车');
INSERT INTO `sensitive_word_data` VALUES ('出售仿真枪');
INSERT INTO `sensitive_word_data` VALUES ('出售假 BI');
INSERT INTO `sensitive_word_data` VALUES ('出售假.钞');
INSERT INTO `sensitive_word_data` VALUES ('出售假2钞');
INSERT INTO `sensitive_word_data` VALUES ('出售假人品币');
INSERT INTO `sensitive_word_data` VALUES ('出售假人民币100');
INSERT INTO `sensitive_word_data` VALUES ('出售假币 ');
INSERT INTO `sensitive_word_data` VALUES ('出售假币 变色油墨');
INSERT INTO `sensitive_word_data` VALUES ('出售假币.假钱');
INSERT INTO `sensitive_word_data` VALUES ('出售假币假钞');
INSERT INTO `sensitive_word_data` VALUES ('出售假币假钞QQ 假硬币');
INSERT INTO `sensitive_word_data` VALUES ('出售假幣');
INSERT INTO `sensitive_word_data` VALUES ('出售假钞');
INSERT INTO `sensitive_word_data` VALUES ('出售假钞 假硬币');
INSERT INTO `sensitive_word_data` VALUES ('出售假钞 假钱 硬币');
INSERT INTO `sensitive_word_data` VALUES ('出售假钞QQ');
INSERT INTO `sensitive_word_data` VALUES ('出售假钱 \"过机版\"');
INSERT INTO `sensitive_word_data` VALUES ('出售假钱电 人民币');
INSERT INTO `sensitive_word_data` VALUES ('出售催情水乖乖水');
INSERT INTO `sensitive_word_data` VALUES ('出售催情药');
INSERT INTO `sensitive_word_data` VALUES ('出售催情迷幻药');
INSERT INTO `sensitive_word_data` VALUES ('出售军');
INSERT INTO `sensitive_word_data` VALUES ('出售军刀');
INSERT INTO `sensitive_word_data` VALUES ('出售军刺');
INSERT INTO `sensitive_word_data` VALUES ('出售军刺军刀');
INSERT INTO `sensitive_word_data` VALUES ('出售军品军刀');
INSERT INTO `sensitive_word_data` VALUES ('出售军用手枪');
INSERT INTO `sensitive_word_data` VALUES ('出售冰毒');
INSERT INTO `sensitive_word_data` VALUES ('出售冰毒 海洛因');
INSERT INTO `sensitive_word_data` VALUES ('出售冰毒配方');
INSERT INTO `sensitive_word_data` VALUES ('出售冰毒黄牙签 冰糖 K粉');
INSERT INTO `sensitive_word_data` VALUES ('出售北京电警棍');
INSERT INTO `sensitive_word_data` VALUES ('出售北京高考落榜生名单');
INSERT INTO `sensitive_word_data` VALUES ('出售北朝冰');
INSERT INTO `sensitive_word_data` VALUES ('出售单管猎');
INSERT INTO `sensitive_word_data` VALUES ('出售发票');
INSERT INTO `sensitive_word_data` VALUES ('出售台湾版高仿真假钞');
INSERT INTO `sensitive_word_data` VALUES ('出售各种防身电警棒');
INSERT INTO `sensitive_word_data` VALUES ('出售各种黄冰钻石冰黄牙签水果冰');
INSERT INTO `sensitive_word_data` VALUES ('出售各类猎狗');
INSERT INTO `sensitive_word_data` VALUES ('出售各类警用装备');
INSERT INTO `sensitive_word_data` VALUES ('出售喷雾型迷药');
INSERT INTO `sensitive_word_data` VALUES ('出售国产军用54手枪');
INSERT INTO `sensitive_word_data` VALUES ('出售国产高压电警棍');
INSERT INTO `sensitive_word_data` VALUES ('出售外国枪模');
INSERT INTO `sensitive_word_data` VALUES ('出售失身粉');
INSERT INTO `sensitive_word_data` VALUES ('出售套牌车');
INSERT INTO `sensitive_word_data` VALUES ('出售套牌轿车');
INSERT INTO `sensitive_word_data` VALUES ('出售少女迷情药');
INSERT INTO `sensitive_word_data` VALUES ('出售山西高考考生名单');
INSERT INTO `sensitive_word_data` VALUES ('出售工字');
INSERT INTO `sensitive_word_data` VALUES ('出售工字气枪铅弹');
INSERT INTO `sensitive_word_data` VALUES ('出售开山刀军刺');
INSERT INTO `sensitive_word_data` VALUES ('出售弹簧刀');
INSERT INTO `sensitive_word_data` VALUES ('出售弹药');
INSERT INTO `sensitive_word_data` VALUES ('出售手qiang 手机');
INSERT INTO `sensitive_word_data` VALUES ('出售手机偷聼器');
INSERT INTO `sensitive_word_data` VALUES ('出售手机监听卡');
INSERT INTO `sensitive_word_data` VALUES ('出售手机窃听器');
INSERT INTO `sensitive_word_data` VALUES ('出售手枪');
INSERT INTO `sensitive_word_data` VALUES ('出售手枪 QQ');
INSERT INTO `sensitive_word_data` VALUES ('出售手枪 电话');
INSERT INTO `sensitive_word_data` VALUES ('出售手枪=货到付款');
INSERT INTO `sensitive_word_data` VALUES ('出售手槍');
INSERT INTO `sensitive_word_data` VALUES ('出售手狗');
INSERT INTO `sensitive_word_data` VALUES ('出售手铐警服警棍');
INSERT INTO `sensitive_word_data` VALUES ('出售政协车证');
INSERT INTO `sensitive_word_data` VALUES ('出售日本成人DVD');
INSERT INTO `sensitive_word_data` VALUES ('出售朝鲜版高仿真假钞');
INSERT INTO `sensitive_word_data` VALUES ('出售枪支');
INSERT INTO `sensitive_word_data` VALUES ('出售植物冰');
INSERT INTO `sensitive_word_data` VALUES ('出售槍支');
INSERT INTO `sensitive_word_data` VALUES ('出售步枪');
INSERT INTO `sensitive_word_data` VALUES ('出售毒品配方');
INSERT INTO `sensitive_word_data` VALUES ('出售气枪');
INSERT INTO `sensitive_word_data` VALUES ('出售气枪 QQ');
INSERT INTO `sensitive_word_data` VALUES ('出售气枪 电话');
INSERT INTO `sensitive_word_data` VALUES ('出售气狗');
INSERT INTO `sensitive_word_data` VALUES ('出售氯胺酮 广州');
INSERT INTO `sensitive_word_data` VALUES ('出售氯胺酮技术');
INSERT INTO `sensitive_word_data` VALUES ('出售氯胺酮甲基苯丙胺 技术配方');
INSERT INTO `sensitive_word_data` VALUES ('出售汽枪');
INSERT INTO `sensitive_word_data` VALUES ('出售海洛因');
INSERT INTO `sensitive_word_data` VALUES ('出售火枪 –游戏');
INSERT INTO `sensitive_word_data` VALUES ('出售炸药');
INSERT INTO `sensitive_word_data` VALUES ('出售炸药 电话');
INSERT INTO `sensitive_word_data` VALUES ('出售炸药QQ');
INSERT INTO `sensitive_word_data` VALUES ('出售热缚纸 假');
INSERT INTO `sensitive_word_data` VALUES ('出售狙击枪');
INSERT INTO `sensitive_word_data` VALUES ('出售猎枪');
INSERT INTO `sensitive_word_data` VALUES ('出售猎枪 QQ');
INSERT INTO `sensitive_word_data` VALUES ('出售猎枪 电话');
INSERT INTO `sensitive_word_data` VALUES ('出售猎枪气枪');
INSERT INTO `sensitive_word_data` VALUES ('出售猎狗');
INSERT INTO `sensitive_word_data` VALUES ('出售盐酸羟亚胺 甲基苯丙胺 麻黄碱');
INSERT INTO `sensitive_word_data` VALUES ('出售短信群发器');
INSERT INTO `sensitive_word_data` VALUES ('出售票据');
INSERT INTO `sensitive_word_data` VALUES ('出售答案');
INSERT INTO `sensitive_word_data` VALUES ('出售缅甸冰麻古');
INSERT INTO `sensitive_word_data` VALUES ('出售美军现役军刀');
INSERT INTO `sensitive_word_data` VALUES ('出售考生信息 QQ');
INSERT INTO `sensitive_word_data` VALUES ('出售苍蝇水三唑仑');
INSERT INTO `sensitive_word_data` VALUES ('出售苍蝇水迷奸药');
INSERT INTO `sensitive_word_data` VALUES ('出售袖珍手枪');
INSERT INTO `sensitive_word_data` VALUES ('出售西班牙苍蝇粉');
INSERT INTO `sensitive_word_data` VALUES ('出售警服春秋执勤装');
INSERT INTO `sensitive_word_data` VALUES ('出售警服警棍手铐');
INSERT INTO `sensitive_word_data` VALUES ('出售警服警用品');
INSERT INTO `sensitive_word_data` VALUES ('出售警用品警服');
INSERT INTO `sensitive_word_data` VALUES ('出售警用手铐QQ');
INSERT INTO `sensitive_word_data` VALUES ('出售警用手铐|仿真枪');
INSERT INTO `sensitive_word_data` VALUES ('出售警用手铐装备');
INSERT INTO `sensitive_word_data` VALUES ('出售警用手铐警服');
INSERT INTO `sensitive_word_data` VALUES ('出售警用电击器');
INSERT INTO `sensitive_word_data` VALUES ('出售警用装备手铐');
INSERT INTO `sensitive_word_data` VALUES ('出售警用警棍手铐警服');
INSERT INTO `sensitive_word_data` VALUES ('出售警用警棍装备');
INSERT INTO `sensitive_word_data` VALUES ('出售警用防身电击棒');
INSERT INTO `sensitive_word_data` VALUES ('出售警用高压棍');
INSERT INTO `sensitive_word_data` VALUES ('出售警用高压电棍');
INSERT INTO `sensitive_word_data` VALUES ('出售走私车');
INSERT INTO `sensitive_word_data` VALUES ('出售跳刀');
INSERT INTO `sensitive_word_data` VALUES ('出售车牌隐形喷剂');
INSERT INTO `sensitive_word_data` VALUES ('出售过机假币');
INSERT INTO `sensitive_word_data` VALUES ('出售过机假钞');
INSERT INTO `sensitive_word_data` VALUES ('出售过机假钱');
INSERT INTO `sensitive_word_data` VALUES ('出售进口PCP');
INSERT INTO `sensitive_word_data` VALUES ('出售进口金属枪模');
INSERT INTO `sensitive_word_data` VALUES ('出售迷情药迷奸水');
INSERT INTO `sensitive_word_data` VALUES ('出售迷药');
INSERT INTO `sensitive_word_data` VALUES ('出售迷魂药三唑仑');
INSERT INTO `sensitive_word_data` VALUES ('出售遥控拦截器');
INSERT INTO `sensitive_word_data` VALUES ('出售酒店发票');
INSERT INTO `sensitive_word_data` VALUES ('出售钞票');
INSERT INTO `sensitive_word_data` VALUES ('出售钢珠狗');
INSERT INTO `sensitive_word_data` VALUES ('出售银行');
INSERT INTO `sensitive_word_data` VALUES ('出售银行卡');
INSERT INTO `sensitive_word_data` VALUES ('出售防身电击棒警用');
INSERT INTO `sensitive_word_data` VALUES ('出售防身警用装备');
INSERT INTO `sensitive_word_data` VALUES ('出售雷管');
INSERT INTO `sensitive_word_data` VALUES ('出售雷管炸药');
INSERT INTO `sensitive_word_data` VALUES ('出售雷管炸药自制炸弹');
INSERT INTO `sensitive_word_data` VALUES ('出售高仿真假钞');
INSERT INTO `sensitive_word_data` VALUES ('出售高仿纸币');
INSERT INTO `sensitive_word_data` VALUES ('出售高压气枪');
INSERT INTO `sensitive_word_data` VALUES ('出售高纯冰');
INSERT INTO `sensitive_word_data` VALUES ('出售高纯度冰毒 95%');
INSERT INTO `sensitive_word_data` VALUES ('出售高考名单');
INSERT INTO `sensitive_word_data` VALUES ('出售高考答案');
INSERT INTO `sensitive_word_data` VALUES ('出售麻古');
INSERT INTO `sensitive_word_data` VALUES ('出售麻醉枪免收任何定金');
INSERT INTO `sensitive_word_data` VALUES ('出国留学必备物品');
INSERT INTO `sensitive_word_data` VALUES ('出国签证 刻章办证');
INSERT INTO `sensitive_word_data` VALUES ('出国签证 刻章电话');
INSERT INTO `sensitive_word_data` VALUES ('出国签证 办证电话');
INSERT INTO `sensitive_word_data` VALUES ('出墙红杏');
INSERT INTO `sensitive_word_data` VALUES ('出成绩付');
INSERT INTO `sensitive_word_data` VALUES ('出水');
INSERT INTO `sensitive_word_data` VALUES ('出血');
INSERT INTO `sensitive_word_data` VALUES ('出血性膀胱炎');
INSERT INTO `sensitive_word_data` VALUES ('击打');
INSERT INTO `sensitive_word_data` VALUES ('刀具专卖QQ');
INSERT INTO `sensitive_word_data` VALUES ('刀具直销网');
INSERT INTO `sensitive_word_data` VALUES ('刀架保安');
INSERT INTO `sensitive_word_data` VALUES ('刀美兰');
INSERT INTO `sensitive_word_data` VALUES ('刀述仁');
INSERT INTO `sensitive_word_data` VALUES ('分家在');
INSERT INTO `sensitive_word_data` VALUES ('分析王');
INSERT INTO `sensitive_word_data` VALUES ('分泌');
INSERT INTO `sensitive_word_data` VALUES ('分裂');
INSERT INTO `sensitive_word_data` VALUES ('分身');
INSERT INTO `sensitive_word_data` VALUES ('切听器');
INSERT INTO `sensitive_word_data` VALUES ('切波特');
INSERT INTO `sensitive_word_data` VALUES ('切除子宫');
INSERT INTO `sensitive_word_data` VALUES ('刊文回谢');
INSERT INTO `sensitive_word_data` VALUES ('刑警');
INSERT INTO `sensitive_word_data` VALUES ('划老公');
INSERT INTO `sensitive_word_data` VALUES ('列确');
INSERT INTO `sensitive_word_data` VALUES ('刘书田');
INSERT INTO `sensitive_word_data` VALUES ('刘云山 ');
INSERT INTO `sensitive_word_data` VALUES ('刘亚洲 ');
INSERT INTO `sensitive_word_data` VALUES ('刘亦铭');
INSERT INTO `sensitive_word_data` VALUES ('刘仲藜');
INSERT INTO `sensitive_word_data` VALUES ('刘伯承');
INSERT INTO `sensitive_word_data` VALUES ('刘俊国');
INSERT INTO `sensitive_word_data` VALUES ('刘元仁');
INSERT INTO `sensitive_word_data` VALUES ('刘光复');
INSERT INTO `sensitive_word_data` VALUES ('刘全喜');
INSERT INTO `sensitive_word_data` VALUES ('刘军宁');
INSERT INTO `sensitive_word_data` VALUES ('刘冬冬');
INSERT INTO `sensitive_word_data` VALUES ('刘凤钢');
INSERT INTO `sensitive_word_data` VALUES ('刘凯中');
INSERT INTO `sensitive_word_data` VALUES ('刘刚');
INSERT INTO `sensitive_word_data` VALUES ('刘千石');
INSERT INTO `sensitive_word_data` VALUES ('刘华清');
INSERT INTO `sensitive_word_data` VALUES ('刘国凯');
INSERT INTO `sensitive_word_data` VALUES ('刘士贤');
INSERT INTO `sensitive_word_data` VALUES ('刘大响');
INSERT INTO `sensitive_word_data` VALUES ('刘奇葆');
INSERT INTO `sensitive_word_data` VALUES ('刘宾深');
INSERT INTO `sensitive_word_data` VALUES ('刘宾雁');
INSERT INTO `sensitive_word_data` VALUES ('刘少奇');
INSERT INTO `sensitive_word_data` VALUES ('刘山青');
INSERT INTO `sensitive_word_data` VALUES ('刘应明');
INSERT INTO `sensitive_word_data` VALUES ('刘延东');
INSERT INTO `sensitive_word_data` VALUES ('刘廷焕');
INSERT INTO `sensitive_word_data` VALUES ('刘志军');
INSERT INTO `sensitive_word_data` VALUES ('刘志华');
INSERT INTO `sensitive_word_data` VALUES ('刘志忠');
INSERT INTO `sensitive_word_data` VALUES ('刘忠德');
INSERT INTO `sensitive_word_data` VALUES ('刘振伟');
INSERT INTO `sensitive_word_data` VALUES ('刘政奎');
INSERT INTO `sensitive_word_data` VALUES ('刘文胜');
INSERT INTO `sensitive_word_data` VALUES ('刘明祖');
INSERT INTO `sensitive_word_data` VALUES ('刘春良');
INSERT INTO `sensitive_word_data` VALUES ('刘晓波');
INSERT INTO `sensitive_word_data` VALUES ('刘晓竹');
INSERT INTO `sensitive_word_data` VALUES ('刘柏年');
INSERT INTO `sensitive_word_data` VALUES ('刘民复');
INSERT INTO `sensitive_word_data` VALUES ('刘永好');
INSERT INTO `sensitive_word_data` VALUES ('刘永川');
INSERT INTO `sensitive_word_data` VALUES ('刘永清');
INSERT INTO `sensitive_word_data` VALUES ('刘汉铨');
INSERT INTO `sensitive_word_data` VALUES ('刘泽民');
INSERT INTO `sensitive_word_data` VALUES ('刘淇 ');
INSERT INTO `sensitive_word_data` VALUES ('刘炳森');
INSERT INTO `sensitive_word_data` VALUES ('刘珩');
INSERT INTO `sensitive_word_data` VALUES ('刘瑞龙');
INSERT INTO `sensitive_word_data` VALUES ('刘璞');
INSERT INTO `sensitive_word_data` VALUES ('刘积斌');
INSERT INTO `sensitive_word_data` VALUES ('刘绍先');
INSERT INTO `sensitive_word_data` VALUES ('刘胜玉');
INSERT INTO `sensitive_word_data` VALUES ('刘西峰');
INSERT INTO `sensitive_word_data` VALUES ('刘迎龙');
INSERT INTO `sensitive_word_data` VALUES ('刘镇武');
INSERT INTO `sensitive_word_data` VALUES ('刘青');
INSERT INTO `sensitive_word_data` VALUES ('刘鹤章');
INSERT INTO `sensitive_word_data` VALUES ('则民');
INSERT INTO `sensitive_word_data` VALUES ('创世之子猎艳之旅');
INSERT INTO `sensitive_word_data` VALUES ('初夜');
INSERT INTO `sensitive_word_data` VALUES ('初血');
INSERT INTO `sensitive_word_data` VALUES ('删除负面信息');
INSERT INTO `sensitive_word_data` VALUES ('利他林');
INSERT INTO `sensitive_word_data` VALUES ('别他吗');
INSERT INTO `sensitive_word_data` VALUES ('刮官');
INSERT INTO `sensitive_word_data` VALUES ('刮宫');
INSERT INTO `sensitive_word_data` VALUES ('到花心');
INSERT INTO `sensitive_word_data` VALUES ('制作假警官证');
INSERT INTO `sensitive_word_data` VALUES ('制作原子弹');
INSERT INTO `sensitive_word_data` VALUES ('制作原子弹-新闻');
INSERT INTO `sensitive_word_data` VALUES ('制作广东警察证');
INSERT INTO `sensitive_word_data` VALUES ('制作性能优良的燃烧弹');
INSERT INTO `sensitive_word_data` VALUES ('制作摇头丸配方');
INSERT INTO `sensitive_word_data` VALUES ('制作新版警察证');
INSERT INTO `sensitive_word_data` VALUES ('制作火药配方');
INSERT INTO `sensitive_word_data` VALUES ('制作炸弹首先要');
INSERT INTO `sensitive_word_data` VALUES ('制作炸弹首先要制作炸药');
INSERT INTO `sensitive_word_data` VALUES ('制作硝化甘油');
INSERT INTO `sensitive_word_data` VALUES ('制作简易弓弩 ');
INSERT INTO `sensitive_word_data` VALUES ('制作证件');
INSERT INTO `sensitive_word_data` VALUES ('制做液体炸药');
INSERT INTO `sensitive_word_data` VALUES ('制十军刺 卖');
INSERT INTO `sensitive_word_data` VALUES ('制手枪');
INSERT INTO `sensitive_word_data` VALUES ('制服');
INSERT INTO `sensitive_word_data` VALUES ('制服诱');
INSERT INTO `sensitive_word_data` VALUES ('制服诱惑2地下法庭');
INSERT INTO `sensitive_word_data` VALUES ('制证定金');
INSERT INTO `sensitive_word_data` VALUES ('刷{4}钻');
INSERT INTO `sensitive_word_data` VALUES ('刹笔');
INSERT INTO `sensitive_word_data` VALUES ('刺刀专卖网');
INSERT INTO `sensitive_word_data` VALUES ('刺插');
INSERT INTO `sensitive_word_data` VALUES ('刺激');
INSERT INTO `sensitive_word_data` VALUES ('刻章');
INSERT INTO `sensitive_word_data` VALUES ('刻章 户口本 身份证');
INSERT INTO `sensitive_word_data` VALUES ('刻章办证 上网学历');
INSERT INTO `sensitive_word_data` VALUES ('刻章办证 上网文凭');
INSERT INTO `sensitive_word_data` VALUES ('刻章办证 学籍档案');
INSERT INTO `sensitive_word_data` VALUES ('刻章办证 教育服务');
INSERT INTO `sensitive_word_data` VALUES ('刻章办证 文凭学历');
INSERT INTO `sensitive_word_data` VALUES ('刻章办证 真实文凭');
INSERT INTO `sensitive_word_data` VALUES ('刻章办证件qq');
INSERT INTO `sensitive_word_data` VALUES ('刻章办证毕业证电话');
INSERT INTO `sensitive_word_data` VALUES ('刻章发票证件文凭');
INSERT INTO `sensitive_word_data` VALUES ('前 戏');
INSERT INTO `sensitive_word_data` VALUES ('前列腺');
INSERT INTO `sensitive_word_data` VALUES ('前列腺增生');
INSERT INTO `sensitive_word_data` VALUES ('前列腺小囊');
INSERT INTO `sensitive_word_data` VALUES ('前列腺治疗');
INSERT INTO `sensitive_word_data` VALUES ('前列腺液');
INSERT INTO `sensitive_word_data` VALUES ('前列腺炎');
INSERT INTO `sensitive_word_data` VALUES ('前列腺癌');
INSERT INTO `sensitive_word_data` VALUES ('前列腺素');
INSERT INTO `sensitive_word_data` VALUES ('前列腺静脉');
INSERT INTO `sensitive_word_data` VALUES ('前庭大腺');
INSERT INTO `sensitive_word_data` VALUES ('前庭大腺炎');
INSERT INTO `sensitive_word_data` VALUES ('前庭球');
INSERT INTO `sensitive_word_data` VALUES ('前戏');
INSERT INTO `sensitive_word_data` VALUES ('前期的弓足别提在理睬你');
INSERT INTO `sensitive_word_data` VALUES ('剑教材');
INSERT INTO `sensitive_word_data` VALUES ('剖腹一刀五千几');
INSERT INTO `sensitive_word_data` VALUES ('剥开');
INSERT INTO `sensitive_word_data` VALUES ('剧烈');
INSERT INTO `sensitive_word_data` VALUES ('剩火 ');
INSERT INTO `sensitive_word_data` VALUES ('副主席');
INSERT INTO `sensitive_word_data` VALUES ('副书记');
INSERT INTO `sensitive_word_data` VALUES ('副性腺炎');
INSERT INTO `sensitive_word_data` VALUES ('割肉人');
INSERT INTO `sensitive_word_data` VALUES ('劉奇葆');
INSERT INTO `sensitive_word_data` VALUES ('力斯曼弓弩');
INSERT INTO `sensitive_word_data` VALUES ('力月西');
INSERT INTO `sensitive_word_data` VALUES ('力骗中央');
INSERT INTO `sensitive_word_data` VALUES ('办上网文凭 学历');
INSERT INTO `sensitive_word_data` VALUES ('办上网文凭中智教育');
INSERT INTO `sensitive_word_data` VALUES ('办上网文凭到启而思');
INSERT INTO `sensitive_word_data` VALUES ('办上网文凭北京博远');
INSERT INTO `sensitive_word_data` VALUES ('办上网文凭腾飞教育');
INSERT INTO `sensitive_word_data` VALUES ('办假证件 学位证');
INSERT INTO `sensitive_word_data` VALUES ('办六级证 办身份证');
INSERT INTO `sensitive_word_data` VALUES ('办四六级 学位证');
INSERT INTO `sensitive_word_data` VALUES ('办四六级 毕业证');
INSERT INTO `sensitive_word_data` VALUES ('办四六级 真文凭');
INSERT INTO `sensitive_word_data` VALUES ('办四六级 签证');
INSERT INTO `sensitive_word_data` VALUES ('办四六级 身份证');
INSERT INTO `sensitive_word_data` VALUES ('办四六级 驾照');
INSERT INTO `sensitive_word_data` VALUES ('办四六级证 刻章');
INSERT INTO `sensitive_word_data` VALUES ('办学位证 刻章');
INSERT INTO `sensitive_word_data` VALUES ('办学位证 毕业证');
INSERT INTO `sensitive_word_data` VALUES ('办学位证 真文凭');
INSERT INTO `sensitive_word_data` VALUES ('办学位证 签证');
INSERT INTO `sensitive_word_data` VALUES ('办学位证 身份证');
INSERT INTO `sensitive_word_data` VALUES ('办学位证 驾照');
INSERT INTO `sensitive_word_data` VALUES ('办怔');
INSERT INTO `sensitive_word_data` VALUES ('办文凭');
INSERT INTO `sensitive_word_data` VALUES ('办文凭 北京博远');
INSERT INTO `sensitive_word_data` VALUES ('办文凭 毕业证 身份证');
INSERT INTO `sensitive_word_data` VALUES ('办文凭 腾飞教育');
INSERT INTO `sensitive_word_data` VALUES ('办文凭QQ 上网学历');
INSERT INTO `sensitive_word_data` VALUES ('办文凭QQ 学籍档案');
INSERT INTO `sensitive_word_data` VALUES ('办文凭刻章电话');
INSERT INTO `sensitive_word_data` VALUES ('办文凭找华北教育');
INSERT INTO `sensitive_word_data` VALUES ('办文凭来易得优教育');
INSERT INTO `sensitive_word_data` VALUES ('办本科');
INSERT INTO `sensitive_word_data` VALUES ('办毕业证 刻章');
INSERT INTO `sensitive_word_data` VALUES ('办毕业证 真文凭');
INSERT INTO `sensitive_word_data` VALUES ('办毕业证 签证');
INSERT INTO `sensitive_word_data` VALUES ('办毕业证 身份证');
INSERT INTO `sensitive_word_data` VALUES ('办毕业证 驾照');
INSERT INTO `sensitive_word_data` VALUES ('办毕业证 驾驶证');
INSERT INTO `sensitive_word_data` VALUES ('办毕业证QQ');
INSERT INTO `sensitive_word_data` VALUES ('办理{10}文凭');
INSERT INTO `sensitive_word_data` VALUES ('办理{5}信用卡');
INSERT INTO `sensitive_word_data` VALUES ('办理假证件');
INSERT INTO `sensitive_word_data` VALUES ('办理各种');
INSERT INTO `sensitive_word_data` VALUES ('办理各种证件');
INSERT INTO `sensitive_word_data` VALUES ('办理商业发票');
INSERT INTO `sensitive_word_data` VALUES ('办理学历 四六级证');
INSERT INTO `sensitive_word_data` VALUES ('办理学历 计算机等级证');
INSERT INTO `sensitive_word_data` VALUES ('办理文凭');
INSERT INTO `sensitive_word_data` VALUES ('办理无抵押贷款');
INSERT INTO `sensitive_word_data` VALUES ('办理本科');
INSERT INTO `sensitive_word_data` VALUES ('办理毕业证 刻章办证');
INSERT INTO `sensitive_word_data` VALUES ('办理真实');
INSERT INTO `sensitive_word_data` VALUES ('办理真实学历');
INSERT INTO `sensitive_word_data` VALUES ('办理真实文凭 华北教育');
INSERT INTO `sensitive_word_data` VALUES ('办理真实文凭QQ');
INSERT INTO `sensitive_word_data` VALUES ('办理票据');
INSERT INTO `sensitive_word_data` VALUES ('办理证书');
INSERT INTO `sensitive_word_data` VALUES ('办理证件 ');
INSERT INTO `sensitive_word_data` VALUES ('办理资格');
INSERT INTO `sensitive_word_data` VALUES ('办理高等院校毕业证');
INSERT INTO `sensitive_word_data` VALUES ('办真文凭 办真学历QQ');
INSERT INTO `sensitive_word_data` VALUES ('办证 中智教育');
INSERT INTO `sensitive_word_data` VALUES ('办证 华北教育');
INSERT INTO `sensitive_word_data` VALUES ('办证 易得优教育');
INSERT INTO `sensitive_word_data` VALUES ('办证件刻章 手机');
INSERT INTO `sensitive_word_data` VALUES ('办证件文凭 高仿真假证件');
INSERT INTO `sensitive_word_data` VALUES ('办证件文凭学历认证');
INSERT INTO `sensitive_word_data` VALUES ('办证公司QQ');
INSERT INTO `sensitive_word_data` VALUES ('办证公司电话');
INSERT INTO `sensitive_word_data` VALUES ('办证刻章 教育咨询');
INSERT INTO `sensitive_word_data` VALUES ('办证刻章 毕业证');
INSERT INTO `sensitive_word_data` VALUES ('办证刻章发票');
INSERT INTO `sensitive_word_data` VALUES ('办证刻章有限公司');
INSERT INTO `sensitive_word_data` VALUES ('办证刻章电话 毕业证');
INSERT INTO `sensitive_word_data` VALUES ('办证发票qq');
INSERT INTO `sensitive_word_data` VALUES ('办证当面交易');
INSERT INTO `sensitive_word_data` VALUES ('办证网 身份证 文凭');
INSERT INTO `sensitive_word_data` VALUES ('办身份证 驾驶证 健康证');
INSERT INTO `sensitive_word_data` VALUES ('办高利贷');
INSERT INTO `sensitive_word_data` VALUES ('功友');
INSERT INTO `sensitive_word_data` VALUES ('功友弟子');
INSERT INTO `sensitive_word_data` VALUES ('功学');
INSERT INTO `sensitive_word_data` VALUES ('功学员');
INSERT INTO `sensitive_word_data` VALUES ('功德圆满');
INSERT INTO `sensitive_word_data` VALUES ('功法');
INSERT INTO `sensitive_word_data` VALUES ('加了服');
INSERT INTO `sensitive_word_data` VALUES ('加府');
INSERT INTO `sensitive_word_data` VALUES ('加油机干扰器 ');
INSERT INTO `sensitive_word_data` VALUES ('加盖机密××××');
INSERT INTO `sensitive_word_data` VALUES ('加藤愛美电驴下载');
INSERT INTO `sensitive_word_data` VALUES ('加长');
INSERT INTO `sensitive_word_data` VALUES ('加非猫：现实世界历险记');
INSERT INTO `sensitive_word_data` VALUES ('务员答案');
INSERT INTO `sensitive_word_data` VALUES ('务员考试');
INSERT INTO `sensitive_word_data` VALUES ('劣等人种博彩');
INSERT INTO `sensitive_word_data` VALUES ('动*态*网');
INSERT INTO `sensitive_word_data` VALUES ('动乱');
INSERT INTO `sensitive_word_data` VALUES ('动作');
INSERT INTO `sensitive_word_data` VALUES ('动态代理');
INSERT INTO `sensitive_word_data` VALUES ('动态网');
INSERT INTO `sensitive_word_data` VALUES ('动情区');
INSERT INTO `sensitive_word_data` VALUES ('动欲区');
INSERT INTO `sensitive_word_data` VALUES ('助考');
INSERT INTO `sensitive_word_data` VALUES ('助考枪手');
INSERT INTO `sensitive_word_data` VALUES ('助考网');
INSERT INTO `sensitive_word_data` VALUES ('劳动教养所');
INSERT INTO `sensitive_word_data` VALUES ('劳工观察');
INSERT INTO `sensitive_word_data` VALUES ('劳改');
INSERT INTO `sensitive_word_data` VALUES ('劳教');
INSERT INTO `sensitive_word_data` VALUES ('劳教基金');
INSERT INTO `sensitive_word_data` VALUES ('勃发');
INSERT INTO `sensitive_word_data` VALUES ('勃朗宁军刀');
INSERT INTO `sensitive_word_data` VALUES ('勃起');
INSERT INTO `sensitive_word_data` VALUES ('勃起功能障碍ED');
INSERT INTO `sensitive_word_data` VALUES ('勇猛');
INSERT INTO `sensitive_word_data` VALUES ('動乱');
INSERT INTO `sensitive_word_data` VALUES ('勤捞致');
INSERT INTO `sensitive_word_data` VALUES ('勾清明');
INSERT INTO `sensitive_word_data` VALUES ('包你射');
INSERT INTO `sensitive_word_data` VALUES ('包养');
INSERT INTO `sensitive_word_data` VALUES ('包养情妇12项吉尼斯排名');
INSERT INTO `sensitive_word_data` VALUES ('包办色情娱乐服务');
INSERT INTO `sensitive_word_data` VALUES ('包叙定');
INSERT INTO `sensitive_word_data` VALUES ('包夜');
INSERT INTO `sensitive_word_data` VALUES ('包娃衣');
INSERT INTO `sensitive_word_data` VALUES ('包消费 包赌博');
INSERT INTO `sensitive_word_data` VALUES ('包皮');
INSERT INTO `sensitive_word_data` VALUES ('包皮嵌顿');
INSERT INTO `sensitive_word_data` VALUES ('包皮炎');
INSERT INTO `sensitive_word_data` VALUES ('包皮环切手术');
INSERT INTO `sensitive_word_data` VALUES ('包皮环切术');
INSERT INTO `sensitive_word_data` VALUES ('包皮系带');
INSERT INTO `sensitive_word_data` VALUES ('包皮系带撕裂');
INSERT INTO `sensitive_word_data` VALUES ('包皮腔');
INSERT INTO `sensitive_word_data` VALUES ('包皮过长');
INSERT INTO `sensitive_word_data` VALUES ('包皮龟头炎');
INSERT INTO `sensitive_word_data` VALUES ('包茎');
INSERT INTO `sensitive_word_data` VALUES ('包覆');
INSERT INTO `sensitive_word_data` VALUES ('包过验钞假钞');
INSERT INTO `sensitive_word_data` VALUES ('包退包换');
INSERT INTO `sensitive_word_data` VALUES ('包青天机 ');
INSERT INTO `sensitive_word_data` VALUES ('匕首蝴蝶甩刀专卖');
INSERT INTO `sensitive_word_data` VALUES ('化学冰 牙签 qq');
INSERT INTO `sensitive_word_data` VALUES ('化学扫盲');
INSERT INTO `sensitive_word_data` VALUES ('化工厂爆炸');
INSERT INTO `sensitive_word_data` VALUES ('化工长期供应盐羟亚胺');
INSERT INTO `sensitive_word_data` VALUES ('北京之春');
INSERT INTO `sensitive_word_data` VALUES ('北京京郊');
INSERT INTO `sensitive_word_data` VALUES ('北京代办四六级证');
INSERT INTO `sensitive_word_data` VALUES ('北京代办驾照文凭');
INSERT INTO `sensitive_word_data` VALUES ('北京代开发');
INSERT INTO `sensitive_word_data` VALUES ('北京出售假钱');
INSERT INTO `sensitive_word_data` VALUES ('北京删贴公司');
INSERT INTO `sensitive_word_data` VALUES ('北京办六级证');
INSERT INTO `sensitive_word_data` VALUES ('北京办毕业证');
INSERT INTO `sensitive_word_data` VALUES ('北京办证 驾驶证');
INSERT INTO `sensitive_word_data` VALUES ('北京办证qq');
INSERT INTO `sensitive_word_data` VALUES ('北京办证中心');
INSERT INTO `sensitive_word_data` VALUES ('北京办证公司');
INSERT INTO `sensitive_word_data` VALUES ('北京办证刻章');
INSERT INTO `sensitive_word_data` VALUES ('北京办证刻章发票');
INSERT INTO `sensitive_word_data` VALUES ('北京办证集团/网上办证');
INSERT INTO `sensitive_word_data` VALUES ('北京劲展鹏');
INSERT INTO `sensitive_word_data` VALUES ('北京各国签证 代办');
INSERT INTO `sensitive_word_data` VALUES ('北京宣传部长');
INSERT INTO `sensitive_word_data` VALUES ('北京市委常委');
INSERT INTO `sensitive_word_data` VALUES ('北京市委黑幕');
INSERT INTO `sensitive_word_data` VALUES ('北京帮');
INSERT INTO `sensitive_word_data` VALUES ('北京开机打发票');
INSERT INTO `sensitive_word_data` VALUES ('北京当局');
INSERT INTO `sensitive_word_data` VALUES ('北京战争');
INSERT INTO `sensitive_word_data` VALUES ('北京政坛清华名人');
INSERT INTO `sensitive_word_data` VALUES ('北京政权');
INSERT INTO `sensitive_word_data` VALUES ('北京政法委书记');
INSERT INTO `sensitive_word_data` VALUES ('北京新版警察证件制作');
INSERT INTO `sensitive_word_data` VALUES ('北京海艺');
INSERT INTO `sensitive_word_data` VALUES ('北京电警棍专卖');
INSERT INTO `sensitive_word_data` VALUES ('北京电警棍出售');
INSERT INTO `sensitive_word_data` VALUES ('北京签证学历证明');
INSERT INTO `sensitive_word_data` VALUES ('北京网上办证');
INSERT INTO `sensitive_word_data` VALUES ('北京网上办证qq');
INSERT INTO `sensitive_word_data` VALUES ('北京职称资格证');
INSERT INTO `sensitive_word_data` VALUES ('北京警官证制作');
INSERT INTO `sensitive_word_data` VALUES ('北京负面信息处理');
INSERT INTO `sensitive_word_data` VALUES ('北京高仿真毕业证');
INSERT INTO `sensitive_word_data` VALUES ('北京高层');
INSERT INTO `sensitive_word_data` VALUES ('北京黑幕');
INSERT INTO `sensitive_word_data` VALUES ('北大三角地论坛');
INSERT INTO `sensitive_word_data` VALUES ('北姑');
INSERT INTO `sensitive_word_data` VALUES ('北戴河会议');
INSERT INTO `sensitive_word_data` VALUES ('北方先锋军刀');
INSERT INTO `sensitive_word_data` VALUES ('北省委门');
INSERT INTO `sensitive_word_data` VALUES ('北美巡回讲法');
INSERT INTO `sensitive_word_data` VALUES ('北美自由论坛');
INSERT INTO `sensitive_word_data` VALUES ('北美讲坛');
INSERT INTO `sensitive_word_data` VALUES ('北美讲坛s');
INSERT INTO `sensitive_word_data` VALUES ('北韩');
INSERT INTO `sensitive_word_data` VALUES ('区的雷人');
INSERT INTO `sensitive_word_data` VALUES ('十七人事安排');
INSERT INTO `sensitive_word_data` VALUES ('十七位老部长');
INSERT INTO `sensitive_word_data` VALUES ('十七大');
INSERT INTO `sensitive_word_data` VALUES ('十七大代表');
INSERT INTO `sensitive_word_data` VALUES ('十七大你转折了吗');
INSERT INTO `sensitive_word_data` VALUES ('十七大幕');
INSERT INTO `sensitive_word_data` VALUES ('十七大权力争霸战');
INSERT INTO `sensitive_word_data` VALUES ('十七大风云');
INSERT INTO `sensitive_word_data` VALUES ('十个预言');
INSERT INTO `sensitive_word_data` VALUES ('十八大');
INSERT INTO `sensitive_word_data` VALUES ('十八摸');
INSERT INTO `sensitive_word_data` VALUES ('十八禁');
INSERT INTO `sensitive_word_data` VALUES ('十八等');
INSERT INTO `sensitive_word_data` VALUES ('十大奖项及中奖名单');
INSERT INTO `sensitive_word_data` VALUES ('十大忽悠名言');
INSERT INTO `sensitive_word_data` VALUES ('十大恶词');
INSERT INTO `sensitive_word_data` VALUES ('十大独裁');
INSERT INTO `sensitive_word_data` VALUES ('十大禁');
INSERT INTO `sensitive_word_data` VALUES ('十大谎');
INSERT INTO `sensitive_word_data` VALUES ('十年动乱××');
INSERT INTO `sensitive_word_data` VALUES ('十景缎');
INSERT INTO `sensitive_word_data` VALUES ('十类人不');
INSERT INTO `sensitive_word_data` VALUES ('千变万化');
INSERT INTO `sensitive_word_data` VALUES ('千岛湖之旅');
INSERT INTO `sensitive_word_data` VALUES ('千源');
INSERT INTO `sensitive_word_data` VALUES ('千禧弘法 ');
INSERT INTO `sensitive_word_data` VALUES ('升天');
INSERT INTO `sensitive_word_data` VALUES ('升达');
INSERT INTO `sensitive_word_data` VALUES ('升达毕业证 ');
INSERT INTO `sensitive_word_data` VALUES ('升達');
INSERT INTO `sensitive_word_data` VALUES ('午夜成人裸聊网');
INSERT INTO `sensitive_word_data` VALUES ('午夜极');
INSERT INTO `sensitive_word_data` VALUES ('午夜电');
INSERT INTO `sensitive_word_data` VALUES ('半刺刀');
INSERT INTO `sensitive_word_data` VALUES ('半裸半露');
INSERT INTO `sensitive_word_data` VALUES ('半遮半露');
INSERT INTO `sensitive_word_data` VALUES ('半骚半软');
INSERT INTO `sensitive_word_data` VALUES ('华主席');
INSERT INTO `sensitive_word_data` VALUES ('华国');
INSERT INTO `sensitive_word_data` VALUES ('华国锋');
INSERT INTO `sensitive_word_data` VALUES ('华夏文摘');
INSERT INTO `sensitive_word_data` VALUES ('华岳');
INSERT INTO `sensitive_word_data` VALUES ('华岳时事论坛');
INSERT INTO `sensitive_word_data` VALUES ('华建敏');
INSERT INTO `sensitive_word_data` VALUES ('华惠棋');
INSERT INTO `sensitive_word_data` VALUES ('华藏功');
INSERT INTO `sensitive_word_data` VALUES ('华诚{4}代考');
INSERT INTO `sensitive_word_data` VALUES ('华语世界论坛');
INSERT INTO `sensitive_word_data` VALUES ('华通时事论坛');
INSERT INTO `sensitive_word_data` VALUES ('华门开');
INSERT INTO `sensitive_word_data` VALUES ('华闻 ');
INSERT INTO `sensitive_word_data` VALUES ('协晃悠');
INSERT INTO `sensitive_word_data` VALUES ('协警');
INSERT INTO `sensitive_word_data` VALUES ('单调');
INSERT INTO `sensitive_word_data` VALUES ('卖64手枪');
INSERT INTO `sensitive_word_data` VALUES ('卖92手枪');
INSERT INTO `sensitive_word_data` VALUES ('卖{4}淫');
INSERT INTO `sensitive_word_data` VALUES ('卖冰毒 杨猛');
INSERT INTO `sensitive_word_data` VALUES ('卖发票');
INSERT INTO `sensitive_word_data` VALUES ('卖国');
INSERT INTO `sensitive_word_data` VALUES ('卖地财政');
INSERT INTO `sensitive_word_data` VALUES ('卖春');
INSERT INTO `sensitive_word_data` VALUES ('卖枪');
INSERT INTO `sensitive_word_data` VALUES ('卖比');
INSERT INTO `sensitive_word_data` VALUES ('卖淫');
INSERT INTO `sensitive_word_data` VALUES ('卖答案');
INSERT INTO `sensitive_word_data` VALUES ('卖自考');
INSERT INTO `sensitive_word_data` VALUES ('卖身 ');
INSERT INTO `sensitive_word_data` VALUES ('卖身契系列');
INSERT INTO `sensitive_word_data` VALUES ('卖逼');
INSERT INTO `sensitive_word_data` VALUES ('卖钢珠枪QQ');
INSERT INTO `sensitive_word_data` VALUES ('卖钱QQ');
INSERT INTO `sensitive_word_data` VALUES ('卖钱联系');
INSERT INTO `sensitive_word_data` VALUES ('卖银行卡');
INSERT INTO `sensitive_word_data` VALUES ('南京代开税票');
INSERT INTO `sensitive_word_data` VALUES ('南京删贴公司');
INSERT INTO `sensitive_word_data` VALUES ('南京发票代开');
INSERT INTO `sensitive_word_data` VALUES ('南京大学法学院');
INSERT INTO `sensitive_word_data` VALUES ('南京电警棍专卖');
INSERT INTO `sensitive_word_data` VALUES ('南京站编辑部副主编');
INSERT INTO `sensitive_word_data` VALUES ('南京负面信息处理');
INSERT INTO `sensitive_word_data` VALUES ('南充针');
INSERT INTO `sensitive_word_data` VALUES ('南华早报');
INSERT INTO `sensitive_word_data` VALUES ('南大自由论坛');
INSERT INTO `sensitive_word_data` VALUES ('南彩菜种子');
INSERT INTO `sensitive_word_data` VALUES ('南振中');
INSERT INTO `sensitive_word_data` VALUES ('南方军刀网');
INSERT INTO `sensitive_word_data` VALUES ('南波杏电驴下载');
INSERT INTO `sensitive_word_data` VALUES ('南阳多功能电警棒供应');
INSERT INTO `sensitive_word_data` VALUES ('南韩 \"出售假币\"');
INSERT INTO `sensitive_word_data` VALUES ('博会暂停');
INSERT INTO `sensitive_word_data` VALUES ('博园区伪');
INSERT INTO `sensitive_word_data` VALUES ('博彩');
INSERT INTO `sensitive_word_data` VALUES ('博彩娱');
INSERT INTO `sensitive_word_data` VALUES ('博白县');
INSERT INTO `sensitive_word_data` VALUES ('博翔团队');
INSERT INTO `sensitive_word_data` VALUES ('博讯');
INSERT INTO `sensitive_word_data` VALUES ('卡耐基');
INSERT INTO `sensitive_word_data` VALUES ('卡辛纳大道和三福大道交界处');
INSERT INTO `sensitive_word_data` VALUES ('卡通');
INSERT INTO `sensitive_word_data` VALUES ('卢光琇');
INSERT INTO `sensitive_word_data` VALUES ('卢展工');
INSERT INTO `sensitive_word_data` VALUES ('卢强');
INSERT INTO `sensitive_word_data` VALUES ('卢瑞华');
INSERT INTO `sensitive_word_data` VALUES ('卢登华');
INSERT INTO `sensitive_word_data` VALUES ('卢荣景');
INSERT INTO `sensitive_word_data` VALUES ('卢跃刚');
INSERT INTO `sensitive_word_data` VALUES ('卢邦正');
INSERT INTO `sensitive_word_data` VALUES ('卢雪松');
INSERT INTO `sensitive_word_data` VALUES ('卧槽');
INSERT INTO `sensitive_word_data` VALUES ('卧艹');
INSERT INTO `sensitive_word_data` VALUES ('卫星天线接收器成人频道');
INSERT INTO `sensitive_word_data` VALUES ('卫星安装调试');
INSERT INTO `sensitive_word_data` VALUES ('卫星广播器材 ');
INSERT INTO `sensitive_word_data` VALUES ('卫星接收器');
INSERT INTO `sensitive_word_data` VALUES ('卫星电视 ');
INSERT INTO `sensitive_word_data` VALUES ('卫星电视安装');
INSERT INTO `sensitive_word_data` VALUES ('卫星遭黑客攻击');
INSERT INTO `sensitive_word_data` VALUES ('卫星高频头 ');
INSERT INTO `sensitive_word_data` VALUES ('卫生计划财务参考资料');
INSERT INTO `sensitive_word_data` VALUES ('卫留成');
INSERT INTO `sensitive_word_data` VALUES ('印尼伊斯兰祈祷团');
INSERT INTO `sensitive_word_data` VALUES ('印尼抢劫华人资产');
INSERT INTO `sensitive_word_data` VALUES ('卵原核');
INSERT INTO `sensitive_word_data` VALUES ('卵子');
INSERT INTO `sensitive_word_data` VALUES ('卵巢');
INSERT INTO `sensitive_word_data` VALUES ('卵巢下垂');
INSERT INTO `sensitive_word_data` VALUES ('卵巢囊肿');
INSERT INTO `sensitive_word_data` VALUES ('卵巢激素');
INSERT INTO `sensitive_word_data` VALUES ('卵巢炎');
INSERT INTO `sensitive_word_data` VALUES ('卵母细胞');
INSERT INTO `sensitive_word_data` VALUES ('卵泡');
INSERT INTO `sensitive_word_data` VALUES ('卵泡刺激素');
INSERT INTO `sensitive_word_data` VALUES ('卵泡期');
INSERT INTO `sensitive_word_data` VALUES ('卵泡液');
INSERT INTO `sensitive_word_data` VALUES ('卵细胞');
INSERT INTO `sensitive_word_data` VALUES ('卵蛋');
INSERT INTO `sensitive_word_data` VALUES ('卵蜜蛋');
INSERT INTO `sensitive_word_data` VALUES ('卵裂');
INSERT INTO `sensitive_word_data` VALUES ('卵黄曩');
INSERT INTO `sensitive_word_data` VALUES ('厂家直销');
INSERT INTO `sensitive_word_data` VALUES ('历史上的真实故事');
INSERT INTO `sensitive_word_data` VALUES ('历史篡改者');
INSERT INTO `sensitive_word_data` VALUES ('厉以宁');
INSERT INTO `sensitive_word_data` VALUES ('厉无畏');
INSERT INTO `sensitive_word_data` VALUES ('厉有为');
INSERT INTO `sensitive_word_data` VALUES ('厕奴 ');
INSERT INTO `sensitive_word_data` VALUES ('原一九五七');
INSERT INTO `sensitive_word_data` VALUES ('原千尋迅雷下载');
INSERT INTO `sensitive_word_data` VALUES ('原子弹');
INSERT INTO `sensitive_word_data` VALUES ('原子弹 制作方法');
INSERT INTO `sensitive_word_data` VALUES ('原子弹DIY教程');
INSERT INTO `sensitive_word_data` VALUES ('原子弹专家');
INSERT INTO `sensitive_word_data` VALUES ('原子弹制作');
INSERT INTO `sensitive_word_data` VALUES ('原子弹制作 方法');
INSERT INTO `sensitive_word_data` VALUES ('原子弹制作手册');
INSERT INTO `sensitive_word_data` VALUES ('原子弹制作方法');
INSERT INTO `sensitive_word_data` VALUES ('原子弹制作方法DIY');
INSERT INTO `sensitive_word_data` VALUES ('原子弹制作简明教程');
INSERT INTO `sensitive_word_data` VALUES ('原子弹制造完全手册');
INSERT INTO `sensitive_word_data` VALUES ('原子弹的DIY制作');
INSERT INTO `sensitive_word_data` VALUES ('原子弹的制作方法');
INSERT INTO `sensitive_word_data` VALUES ('原子弹的理论');
INSERT INTO `sensitive_word_data` VALUES ('原子弹的简易制法');
INSERT INTO `sensitive_word_data` VALUES ('原子弹配方');
INSERT INTO `sensitive_word_data` VALUES ('原装弹');
INSERT INTO `sensitive_word_data` VALUES ('原谅朱镕基');
INSERT INTO `sensitive_word_data` VALUES ('厦门大游行');
INSERT INTO `sensitive_word_data` VALUES ('去他娘的');
INSERT INTO `sensitive_word_data` VALUES ('去哪里买假钞');
INSERT INTO `sensitive_word_data` VALUES ('去哪里买冰毒制造配方');
INSERT INTO `sensitive_word_data` VALUES ('去眼袋');
INSERT INTO `sensitive_word_data` VALUES ('参谋业务参考资料');
INSERT INTO `sensitive_word_data` VALUES ('又咬又舔又吸');
INSERT INTO `sensitive_word_data` VALUES ('又稠又粘');
INSERT INTO `sensitive_word_data` VALUES ('又粗又短');
INSERT INTO `sensitive_word_data` VALUES ('又细又嫩');
INSERT INTO `sensitive_word_data` VALUES ('又美又嫩');
INSERT INTO `sensitive_word_data` VALUES ('又肥又厚');
INSERT INTO `sensitive_word_data` VALUES ('又肿又大');
INSERT INTO `sensitive_word_data` VALUES ('叉开');
INSERT INTO `sensitive_word_data` VALUES ('叉我');
INSERT INTO `sensitive_word_data` VALUES ('及川奈央');
INSERT INTO `sensitive_word_data` VALUES ('友崎亜希迅雷下载');
INSERT INTO `sensitive_word_data` VALUES ('双儿篇');
INSERT INTO `sensitive_word_data` VALUES ('双十节');
INSERT INTO `sensitive_word_data` VALUES ('双桶 ');
INSERT INTO `sensitive_word_data` VALUES ('双筒 ');
INSERT INTO `sensitive_word_data` VALUES ('双管平');
INSERT INTO `sensitive_word_data` VALUES ('双管立');
INSERT INTO `sensitive_word_data` VALUES ('双腿间的禁地');
INSERT INTO `sensitive_word_data` VALUES ('双规');
INSERT INTO `sensitive_word_data` VALUES ('双鞋的故事');
INSERT INTO `sensitive_word_data` VALUES ('反中');
INSERT INTO `sensitive_word_data` VALUES ('反中共黑色暴力');
INSERT INTO `sensitive_word_data` VALUES ('反中游行');
INSERT INTO `sensitive_word_data` VALUES ('反人类');
INSERT INTO `sensitive_word_data` VALUES ('反人类罪');
INSERT INTO `sensitive_word_data` VALUES ('反党');
INSERT INTO `sensitive_word_data` VALUES ('反共');
INSERT INTO `sensitive_word_data` VALUES ('反共传单');
INSERT INTO `sensitive_word_data` VALUES ('反共言论');
INSERT INTO `sensitive_word_data` VALUES ('反动');
INSERT INTO `sensitive_word_data` VALUES ('反华');
INSERT INTO `sensitive_word_data` VALUES ('反右题材');
INSERT INTO `sensitive_word_data` VALUES ('反奥');
INSERT INTO `sensitive_word_data` VALUES ('反对08奥运会');
INSERT INTO `sensitive_word_data` VALUES ('反对共产主义');
INSERT INTO `sensitive_word_data` VALUES ('反对共产党');
INSERT INTO `sensitive_word_data` VALUES ('反封锁');
INSERT INTO `sensitive_word_data` VALUES ('反封锁技术');
INSERT INTO `sensitive_word_data` VALUES ('反屏蔽');
INSERT INTO `sensitive_word_data` VALUES ('反攻');
INSERT INTO `sensitive_word_data` VALUES ('反攻大陆');
INSERT INTO `sensitive_word_data` VALUES ('反政府');
INSERT INTO `sensitive_word_data` VALUES ('反日万人游行');
INSERT INTO `sensitive_word_data` VALUES ('反民主');
INSERT INTO `sensitive_word_data` VALUES ('反测速雷');
INSERT INTO `sensitive_word_data` VALUES ('反社会');
INSERT INTO `sensitive_word_data` VALUES ('反腐总攻');
INSERT INTO `sensitive_word_data` VALUES ('反腐败论坛');
INSERT INTO `sensitive_word_data` VALUES ('反雷达测');
INSERT INTO `sensitive_word_data` VALUES ('反雷达测速 ');
INSERT INTO `sensitive_word_data` VALUES ('反雷達測速');
INSERT INTO `sensitive_word_data` VALUES ('反革命政变纲领');
INSERT INTO `sensitive_word_data` VALUES ('发-票');
INSERT INTO `sensitive_word_data` VALUES ('发/票/代/开');
INSERT INTO `sensitive_word_data` VALUES ('发丝');
INSERT INTO `sensitive_word_data` VALUES ('发仑');
INSERT INTO `sensitive_word_data` VALUES ('发仑da发');
INSERT INTO `sensitive_word_data` VALUES ('发伦');
INSERT INTO `sensitive_word_data` VALUES ('发伦功');
INSERT INTO `sensitive_word_data` VALUES ('发伦工 ');
INSERT INTO `sensitive_word_data` VALUES ('发囵');
INSERT INTO `sensitive_word_data` VALUES ('发国难财');
INSERT INTO `sensitive_word_data` VALUES ('发展');
INSERT INTO `sensitive_word_data` VALUES ('发情');
INSERT INTO `sensitive_word_data` VALUES ('发愣');
INSERT INTO `sensitive_word_data` VALUES ('发抖');
INSERT INTO `sensitive_word_data` VALUES ('发抡');
INSERT INTO `sensitive_word_data` VALUES ('发抡功');
INSERT INTO `sensitive_word_data` VALUES ('发春');
INSERT INTO `sensitive_word_data` VALUES ('发正念');
INSERT INTO `sensitive_word_data` VALUES ('发沦');
INSERT INTO `sensitive_word_data` VALUES ('发泄');
INSERT INTO `sensitive_word_data` VALUES ('发浪');
INSERT INTO `sensitive_word_data` VALUES ('发涨');
INSERT INTO `sensitive_word_data` VALUES ('发热');
INSERT INTO `sensitive_word_data` VALUES ('发牌绝');
INSERT INTO `sensitive_word_data` VALUES ('发生关系');
INSERT INTO `sensitive_word_data` VALUES ('发痒');
INSERT INTO `sensitive_word_data` VALUES ('发票');
INSERT INTO `sensitive_word_data` VALUES ('发票 管理');
INSERT INTO `sensitive_word_data` VALUES ('发票买');
INSERT INTO `sensitive_word_data` VALUES ('发票代');
INSERT INTO `sensitive_word_data` VALUES ('发票代开 ');
INSERT INTO `sensitive_word_data` VALUES ('发票代开保真');
INSERT INTO `sensitive_word_data` VALUES ('发票出');
INSERT INTO `sensitive_word_data` VALUES ('发票销');
INSERT INTO `sensitive_word_data` VALUES ('发票顾问有限公司');
INSERT INTO `sensitive_word_data` VALUES ('发纶');
INSERT INTO `sensitive_word_data` VALUES ('发论');
INSERT INTO `sensitive_word_data` VALUES ('发论公');
INSERT INTO `sensitive_word_data` VALUES ('发论功');
INSERT INTO `sensitive_word_data` VALUES ('发论工');
INSERT INTO `sensitive_word_data` VALUES ('发贴工具 ');
INSERT INTO `sensitive_word_data` VALUES ('发贴机 ');
INSERT INTO `sensitive_word_data` VALUES ('发轮');
INSERT INTO `sensitive_word_data` VALUES ('发轮功 ');
INSERT INTO `sensitive_word_data` VALUES ('发轮功陈果 ');
INSERT INTO `sensitive_word_data` VALUES ('发颤');
INSERT INTO `sensitive_word_data` VALUES ('发骚');
INSERT INTO `sensitive_word_data` VALUES ('发麻');
INSERT INTO `sensitive_word_data` VALUES ('变声器');
INSERT INTO `sensitive_word_data` VALUES ('变声电话');
INSERT INTO `sensitive_word_data` VALUES ('变得');
INSERT INTO `sensitive_word_data` VALUES ('变态');
INSERT INTO `sensitive_word_data` VALUES ('变牌绝');
INSERT INTO `sensitive_word_data` VALUES ('变硬');
INSERT INTO `sensitive_word_data` VALUES ('变粗');
INSERT INTO `sensitive_word_data` VALUES ('变软');
INSERT INTO `sensitive_word_data` VALUES ('变革之风');
INSERT INTO `sensitive_word_data` VALUES ('叠股');
INSERT INTO `sensitive_word_data` VALUES ('口中');
INSERT INTO `sensitive_word_data` VALUES ('口交 ');
INSERT INTO `sensitive_word_data` VALUES ('口含');
INSERT INTO `sensitive_word_data` VALUES ('口唇');
INSERT INTO `sensitive_word_data` VALUES ('口头检查');
INSERT INTO `sensitive_word_data` VALUES ('口手枪');
INSERT INTO `sensitive_word_data` VALUES ('口技');
INSERT INTO `sensitive_word_data` VALUES ('口活儿');
INSERT INTO `sensitive_word_data` VALUES ('口淫 ');
INSERT INTO `sensitive_word_data` VALUES ('口爆');
INSERT INTO `sensitive_word_data` VALUES ('口香糖炸弹');
INSERT INTO `sensitive_word_data` VALUES ('古怪歌');
INSERT INTO `sensitive_word_data` VALUES ('古拉格');
INSERT INTO `sensitive_word_data` VALUES ('古方迷香 ');
INSERT INTO `sensitive_word_data` VALUES ('古柯');
INSERT INTO `sensitive_word_data` VALUES ('叫声');
INSERT INTO `sensitive_word_data` VALUES ('叫床');
INSERT INTO `sensitive_word_data` VALUES ('叫春');
INSERT INTO `sensitive_word_data` VALUES ('叫晶晶的女孩');
INSERT INTO `sensitive_word_data` VALUES ('叫自慰');
INSERT INTO `sensitive_word_data` VALUES ('叫鸡');
INSERT INTO `sensitive_word_data` VALUES ('召妓');
INSERT INTO `sensitive_word_data` VALUES ('叮丙诺菲');
INSERT INTO `sensitive_word_data` VALUES ('可儿');
INSERT INTO `sensitive_word_data` VALUES ('可卡因');
INSERT INTO `sensitive_word_data` VALUES ('可塑炸弹制作');
INSERT INTO `sensitive_word_data` VALUES ('可塑炸弹制作方法');
INSERT INTO `sensitive_word_data` VALUES ('可过机假钞QQ');
INSERT INTO `sensitive_word_data` VALUES ('可过机假钱QQ');
INSERT INTO `sensitive_word_data` VALUES ('台海军刀');
INSERT INTO `sensitive_word_data` VALUES ('台海盾牌');
INSERT INTO `sensitive_word_data` VALUES ('台湾');
INSERT INTO `sensitive_word_data` VALUES ('台湾 \"出售假钱\"');
INSERT INTO `sensitive_word_data` VALUES ('台湾18DY电影');
INSERT INTO `sensitive_word_data` VALUES ('台湾共合国');
INSERT INTO `sensitive_word_data` VALUES ('台湾国');
INSERT INTO `sensitive_word_data` VALUES ('台湾应该独立');
INSERT INTO `sensitive_word_data` VALUES ('台湾建国运动组织');
INSERT INTO `sensitive_word_data` VALUES ('台湾总统');
INSERT INTO `sensitive_word_data` VALUES ('台湾政论区');
INSERT INTO `sensitive_word_data` VALUES ('台湾有权独立');
INSERT INTO `sensitive_word_data` VALUES ('台湾版伪钞 最新D版');
INSERT INTO `sensitive_word_data` VALUES ('台湾版假人民币出售QQ');
INSERT INTO `sensitive_word_data` VALUES ('台湾版假币QQ');
INSERT INTO `sensitive_word_data` VALUES ('台湾狗');
INSERT INTO `sensitive_word_data` VALUES ('台湾独');
INSERT INTO `sensitive_word_data` VALUES ('台湾独立');
INSERT INTO `sensitive_word_data` VALUES ('台湾猪');
INSERT INTO `sensitive_word_data` VALUES ('台湾自由联盟');
INSERT INTO `sensitive_word_data` VALUES ('台湾身份证');
INSERT INTO `sensitive_word_data` VALUES ('台湾青年独立联盟');
INSERT INTO `sensitive_word_data` VALUES ('台独');
INSERT INTO `sensitive_word_data` VALUES ('台独=台反文旁虫');
INSERT INTO `sensitive_word_data` VALUES ('台獨');
INSERT INTO `sensitive_word_data` VALUES ('台盟');
INSERT INTO `sensitive_word_data` VALUES ('史上最强阵容的中国国家领导人');
INSERT INTO `sensitive_word_data` VALUES ('史久武 ');
INSERT INTO `sensitive_word_data` VALUES ('右乳');
INSERT INTO `sensitive_word_data` VALUES ('右派');
INSERT INTO `sensitive_word_data` VALUES ('右臀');
INSERT INTO `sensitive_word_data` VALUES ('右转是政');
INSERT INTO `sensitive_word_data` VALUES ('叶剑英');
INSERT INTO `sensitive_word_data` VALUES ('叶大年');
INSERT INTO `sensitive_word_data` VALUES ('叶如棠');
INSERT INTO `sensitive_word_data` VALUES ('叶子楣');
INSERT INTO `sensitive_word_data` VALUES ('叶小文');
INSERT INTO `sensitive_word_data` VALUES ('叶少兰');
INSERT INTO `sensitive_word_data` VALUES ('叶朗');
INSERT INTO `sensitive_word_data` VALUES ('叶连松');
INSERT INTO `sensitive_word_data` VALUES ('叶青');
INSERT INTO `sensitive_word_data` VALUES ('号屏蔽器');
INSERT INTO `sensitive_word_data` VALUES ('号码生成器');
INSERT INTO `sensitive_word_data` VALUES ('司徒华');
INSERT INTO `sensitive_word_data` VALUES ('司法警官');
INSERT INTO `sensitive_word_data` VALUES ('司法黑');
INSERT INTO `sensitive_word_data` VALUES ('司长期有');
INSERT INTO `sensitive_word_data` VALUES ('司马义铁力瓦尔地');
INSERT INTO `sensitive_word_data` VALUES ('司马仿真枪模型QQ');
INSERT INTO `sensitive_word_data` VALUES ('司马晋');
INSERT INTO `sensitive_word_data` VALUES ('司马璐');
INSERT INTO `sensitive_word_data` VALUES ('司马璐回忆录');
INSERT INTO `sensitive_word_data` VALUES ('叼住');
INSERT INTO `sensitive_word_data` VALUES ('各式气枪出售');
INSERT INTO `sensitive_word_data` VALUES ('各种炸药配方大全');
INSERT INTO `sensitive_word_data` VALUES ('各类军用枪');
INSERT INTO `sensitive_word_data` VALUES ('各类军用枪支');
INSERT INTO `sensitive_word_data` VALUES ('各类文凭');
INSERT INTO `sensitive_word_data` VALUES ('各类考试');
INSERT INTO `sensitive_word_data` VALUES ('各类警用装备出售');
INSERT INTO `sensitive_word_data` VALUES ('各类防身器材专卖');
INSERT INTO `sensitive_word_data` VALUES ('各类防身电棍网');
INSERT INTO `sensitive_word_data` VALUES ('合体');
INSERT INTO `sensitive_word_data` VALUES ('合彩');
INSERT INTO `sensitive_word_data` VALUES ('合拢');
INSERT INTO `sensitive_word_data` VALUES ('合理避税，代理税票');
INSERT INTO `sensitive_word_data` VALUES ('合适');
INSERT INTO `sensitive_word_data` VALUES ('吉佩定');
INSERT INTO `sensitive_word_data` VALUES ('吉澤明步种子');
INSERT INTO `sensitive_word_data` VALUES ('吉炳轩');
INSERT INTO `sensitive_word_data` VALUES ('吉祥宝贝');
INSERT INTO `sensitive_word_data` VALUES ('同城商务网');
INSERT INTO `sensitive_word_data` VALUES ('同居社区');
INSERT INTO `sensitive_word_data` VALUES ('同居社區');
INSERT INTO `sensitive_word_data` VALUES ('同房');
INSERT INTO `sensitive_word_data` VALUES ('同样的人权');
INSERT INTO `sensitive_word_data` VALUES ('同胞书');
INSERT INTO `sensitive_word_data` VALUES ('名刀网出售');
INSERT INTO `sensitive_word_data` VALUES ('名器');
INSERT INTO `sensitive_word_data` VALUES ('名山县');
INSERT INTO `sensitive_word_data` VALUES ('后入位');
INSERT INTO `sensitive_word_data` VALUES ('后勤资料手册');
INSERT INTO `sensitive_word_data` VALUES ('后勤集团');
INSERT INTO `sensitive_word_data` VALUES ('后妈');
INSERT INTO `sensitive_word_data` VALUES ('后庭');
INSERT INTO `sensitive_word_data` VALUES ('后庭花');
INSERT INTO `sensitive_word_data` VALUES ('后戏');
INSERT INTO `sensitive_word_data` VALUES ('后方想定的编写');
INSERT INTO `sensitive_word_data` VALUES ('后母');
INSERT INTO `sensitive_word_data` VALUES ('吐血');
INSERT INTO `sensitive_word_data` VALUES ('向巴平措');
INSERT INTO `sensitive_word_data` VALUES ('吕京花');
INSERT INTO `sensitive_word_data` VALUES ('吕加平');
INSERT INTO `sensitive_word_data` VALUES ('吕祖善');
INSERT INTO `sensitive_word_data` VALUES ('吕秀莲');
INSERT INTO `sensitive_word_data` VALUES ('吗啡');
INSERT INTO `sensitive_word_data` VALUES ('吗啡片');
INSERT INTO `sensitive_word_data` VALUES ('吗啡碱cv');
INSERT INTO `sensitive_word_data` VALUES ('君島愛迅雷下载');
INSERT INTO `sensitive_word_data` VALUES ('含');
INSERT INTO `sensitive_word_data` VALUES ('含乳');
INSERT INTO `sensitive_word_data` VALUES ('含住');
INSERT INTO `sensitive_word_data` VALUES ('含入');
INSERT INTO `sensitive_word_data` VALUES ('含吮');
INSERT INTO `sensitive_word_data` VALUES ('含咬');
INSERT INTO `sensitive_word_data` VALUES ('含弄');
INSERT INTO `sensitive_word_data` VALUES ('含春');
INSERT INTO `sensitive_word_data` VALUES ('含有麻黄素的普通药品');
INSERT INTO `sensitive_word_data` VALUES ('含着');
INSERT INTO `sensitive_word_data` VALUES ('启功');
INSERT INTO `sensitive_word_data` VALUES ('吳瓊花問');
INSERT INTO `sensitive_word_data` VALUES ('吳邦國');
INSERT INTO `sensitive_word_data` VALUES ('吴仁华');
INSERT INTO `sensitive_word_data` VALUES ('吴仪 ');
INSERT INTO `sensitive_word_data` VALUES ('吴光宇');
INSERT INTO `sensitive_word_data` VALUES ('吴光正');
INSERT INTO `sensitive_word_data` VALUES ('吴冠中');
INSERT INTO `sensitive_word_data` VALUES ('吴双战');
INSERT INTO `sensitive_word_data` VALUES ('吴国祯');
INSERT INTO `sensitive_word_data` VALUES ('吴基传');
INSERT INTO `sensitive_word_data` VALUES ('吴学灿');
INSERT INTO `sensitive_word_data` VALUES ('吴学璨');
INSERT INTO `sensitive_word_data` VALUES ('吴学谦');
INSERT INTO `sensitive_word_data` VALUES ('吴宏达');
INSERT INTO `sensitive_word_data` VALUES ('吴官正');
INSERT INTO `sensitive_word_data` VALUES ('吴定富');
INSERT INTO `sensitive_word_data` VALUES ('吴弘达');
INSERT INTO `sensitive_word_data` VALUES ('吴德');
INSERT INTO `sensitive_word_data` VALUES ('吴德馨');
INSERT INTO `sensitive_word_data` VALUES ('吴敬琏');
INSERT INTO `sensitive_word_data` VALUES ('吴新涛');
INSERT INTO `sensitive_word_data` VALUES ('吴方城');
INSERT INTO `sensitive_word_data` VALUES ('吴明熹');
INSERT INTO `sensitive_word_data` VALUES ('吴正德');
INSERT INTO `sensitive_word_data` VALUES ('吴润忠');
INSERT INTO `sensitive_word_data` VALUES ('吴爱英');
INSERT INTO `sensitive_word_data` VALUES ('吴琼花问');
INSERT INTO `sensitive_word_data` VALUES ('吴百益');
INSERT INTO `sensitive_word_data` VALUES ('吴祖强');
INSERT INTO `sensitive_word_data` VALUES ('吴胜利');
INSERT INTO `sensitive_word_data` VALUES ('吴蔚然');
INSERT INTO `sensitive_word_data` VALUES ('吴贻弓');
INSERT INTO `sensitive_word_data` VALUES ('吴邦国 ');
INSERT INTO `sensitive_word_data` VALUES ('吴野渡');
INSERT INTO `sensitive_word_data` VALUES ('吴镇南');
INSERT INTO `sensitive_word_data` VALUES ('吸储');
INSERT INTO `sensitive_word_data` VALUES ('吸儲');
INSERT INTO `sensitive_word_data` VALUES ('吸血莱恩');
INSERT INTO `sensitive_word_data` VALUES ('吸血萊恩');
INSERT INTO `sensitive_word_data` VALUES ('吹弹欲破');
INSERT INTO `sensitive_word_data` VALUES ('吹箫');
INSERT INTO `sensitive_word_data` VALUES ('吹萧');
INSERT INTO `sensitive_word_data` VALUES ('吾尔凯西');
INSERT INTO `sensitive_word_data` VALUES ('吾尔开希');
INSERT INTO `sensitive_word_data` VALUES ('吾尔开西');
INSERT INTO `sensitive_word_data` VALUES ('告中国人民解放军广大官兵书');
INSERT INTO `sensitive_word_data` VALUES ('告全体网民书');
INSERT INTO `sensitive_word_data` VALUES ('告全国人大书');
INSERT INTO `sensitive_word_data` VALUES ('告全国股民同胞书');
INSERT INTO `sensitive_word_data` VALUES ('告别蒸笼车');
INSERT INTO `sensitive_word_data` VALUES ('告洋状');
INSERT INTO `sensitive_word_data` VALUES ('告长期');
INSERT INTO `sensitive_word_data` VALUES ('呋塞米');
INSERT INTO `sensitive_word_data` VALUES ('周　济');
INSERT INTO `sensitive_word_data` VALUES ('周伯华');
INSERT INTO `sensitive_word_data` VALUES ('周刊纪事');
INSERT INTO `sensitive_word_data` VALUES ('周国强');
INSERT INTO `sensitive_word_data` VALUES ('周坤仁');
INSERT INTO `sensitive_word_data` VALUES ('周天勇');
INSERT INTO `sensitive_word_data` VALUES ('周天法');
INSERT INTO `sensitive_word_data` VALUES ('周子玉');
INSERT INTO `sensitive_word_data` VALUES ('周宜兴');
INSERT INTO `sensitive_word_data` VALUES ('周容');
INSERT INTO `sensitive_word_data` VALUES ('周容重');
INSERT INTO `sensitive_word_data` VALUES ('周小川');
INSERT INTO `sensitive_word_data` VALUES ('周建南');
INSERT INTO `sensitive_word_data` VALUES ('周恩来后悔');
INSERT INTO `sensitive_word_data` VALUES ('周恩来忏悔');
INSERT INTO `sensitive_word_data` VALUES ('周恩来自责');
INSERT INTO `sensitive_word_data` VALUES ('周正庆');
INSERT INTO `sensitive_word_data` VALUES ('周正毅');
INSERT INTO `sensitive_word_data` VALUES ('周水同志在全省计划工交工作会议上的报告');
INSERT INTO `sensitive_word_data` VALUES ('周永康 ');
INSERT INTO `sensitive_word_data` VALUES ('周玉清');
INSERT INTO `sensitive_word_data` VALUES ('周生贤');
INSERT INTO `sensitive_word_data` VALUES ('周绍熹');
INSERT INTO `sensitive_word_data` VALUES ('周铁农');
INSERT INTO `sensitive_word_data` VALUES ('周鸿陵');
INSERT INTO `sensitive_word_data` VALUES ('呵痒');
INSERT INTO `sensitive_word_data` VALUES ('呻吟');
INSERT INTO `sensitive_word_data` VALUES ('呼呼');
INSERT INTO `sensitive_word_data` VALUES ('呼喊派');
INSERT INTO `sensitive_word_data` VALUES ('命根');
INSERT INTO `sensitive_word_data` VALUES ('命根子');
INSERT INTO `sensitive_word_data` VALUES ('咀唇');
INSERT INTO `sensitive_word_data` VALUES ('和奸成瘾');
INSERT INTO `sensitive_word_data` VALUES ('和平修炼');
INSERT INTO `sensitive_word_data` VALUES ('和平修练');
INSERT INTO `sensitive_word_data` VALUES ('和平演变道路已被中共堵死');
INSERT INTO `sensitive_word_data` VALUES ('和平请愿书');
INSERT INTO `sensitive_word_data` VALUES ('和弦');
INSERT INTO `sensitive_word_data` VALUES ('和狗交');
INSERT INTO `sensitive_word_data` VALUES ('和狗做');
INSERT INTO `sensitive_word_data` VALUES ('和狗性');
INSERT INTO `sensitive_word_data` VALUES ('和解的智慧');
INSERT INTO `sensitive_word_data` VALUES ('和谐');
INSERT INTO `sensitive_word_data` VALUES ('咏妓');
INSERT INTO `sensitive_word_data` VALUES ('咖啡因');
INSERT INTO `sensitive_word_data` VALUES ('咖啡因，麻黄素');
INSERT INTO `sensitive_word_data` VALUES ('咖啡文化');
INSERT INTO `sensitive_word_data` VALUES ('咪咪');
INSERT INTO `sensitive_word_data` VALUES ('咪达唑仑');
INSERT INTO `sensitive_word_data` VALUES ('哈尔滨网上办证');
INSERT INTO `sensitive_word_data` VALUES ('哈批');
INSERT INTO `sensitive_word_data` VALUES ('哈狗帮 ');
INSERT INTO `sensitive_word_data` VALUES ('哈药直销');
INSERT INTO `sensitive_word_data` VALUES ('哥 哥');
INSERT INTO `sensitive_word_data` VALUES ('哥哥');
INSERT INTO `sensitive_word_data` VALUES ('哥疼');
INSERT INTO `sensitive_word_data` VALUES ('哥痛');
INSERT INTO `sensitive_word_data` VALUES ('哥言语录');
INSERT INTO `sensitive_word_data` VALUES ('哪有代开商品销售發票');
INSERT INTO `sensitive_word_data` VALUES ('哪能买到真枪');
INSERT INTO `sensitive_word_data` VALUES ('哪里买仿真枪');
INSERT INTO `sensitive_word_data` VALUES ('哪里买汽狗');
INSERT INTO `sensitive_word_data` VALUES ('哪里卖假钞');
INSERT INTO `sensitive_word_data` VALUES ('哪里可以买到假币');
INSERT INTO `sensitive_word_data` VALUES ('哪里可以买到麻黄素 水晶冰');
INSERT INTO `sensitive_word_data` VALUES ('哪里弄炸药');
INSERT INTO `sensitive_word_data` VALUES ('哪里有假钱出售');
INSERT INTO `sensitive_word_data` VALUES ('哪里有出售假钱厂家');
INSERT INTO `sensitive_word_data` VALUES ('哪里有卖双管猎');
INSERT INTO `sensitive_word_data` VALUES ('哪里有手雷卖');
INSERT INTO `sensitive_word_data` VALUES ('哪里有枪出售');
INSERT INTO `sensitive_word_data` VALUES ('哪里有气枪卖');
INSERT INTO `sensitive_word_data` VALUES ('哪里有火药卖');
INSERT INTO `sensitive_word_data` VALUES ('哪里有炸药买');
INSERT INTO `sensitive_word_data` VALUES ('哪里有炸药卖');
INSERT INTO `sensitive_word_data` VALUES ('哪里有真枪出售');
INSERT INTO `sensitive_word_data` VALUES ('哪里有真枪卖买');
INSERT INTO `sensitive_word_data` VALUES ('哪里有硝酸胺炸药卖');
INSERT INTO `sensitive_word_data` VALUES ('哪里有连发猎枪买卖');
INSERT INTO `sensitive_word_data` VALUES ('哪里有钢珠枪');
INSERT INTO `sensitive_word_data` VALUES ('哪里有雷管买');
INSERT INTO `sensitive_word_data` VALUES ('哪里有雷管卖');
INSERT INTO `sensitive_word_data` VALUES ('哪里有高爆炸药卖');
INSERT INTO `sensitive_word_data` VALUES ('哪里有黑火药卖');
INSERT INTO `sensitive_word_data` VALUES ('哪里有黑索金卖');
INSERT INTO `sensitive_word_data` VALUES ('哪里能买到 摇头丸 冰毒QQ');
INSERT INTO `sensitive_word_data` VALUES ('哪里能买到 纯度99% K粉');
INSERT INTO `sensitive_word_data` VALUES ('哪里能买到假币');
INSERT INTO `sensitive_word_data` VALUES ('哪里能买到假钱');
INSERT INTO `sensitive_word_data` VALUES ('哪里能买到自制手枪  ');
INSERT INTO `sensitive_word_data` VALUES ('哭求=求');
INSERT INTO `sensitive_word_data` VALUES ('哲民');
INSERT INTO `sensitive_word_data` VALUES ('唇片');
INSERT INTO `sensitive_word_data` VALUES ('唇瓣');
INSERT INTO `sensitive_word_data` VALUES ('唇缝');
INSERT INTO `sensitive_word_data` VALUES ('唇肉');
INSERT INTO `sensitive_word_data` VALUES ('唇舌');
INSERT INTO `sensitive_word_data` VALUES ('唇间');
INSERT INTO `sensitive_word_data` VALUES ('唐人电视台');
INSERT INTO `sensitive_word_data` VALUES ('唐家璇');
INSERT INTO `sensitive_word_data` VALUES ('唐捷');
INSERT INTO `sensitive_word_data` VALUES ('唐柏桥');
INSERT INTO `sensitive_word_data` VALUES ('唐淳风');
INSERT INTO `sensitive_word_data` VALUES ('唑仑');
INSERT INTO `sensitive_word_data` VALUES ('唤起');
INSERT INTO `sensitive_word_data` VALUES ('售一元硬');
INSERT INTO `sensitive_word_data` VALUES ('售三棱');
INSERT INTO `sensitive_word_data` VALUES ('售五四');
INSERT INTO `sensitive_word_data` VALUES ('售信用');
INSERT INTO `sensitive_word_data` VALUES ('售假/出售假钱');
INSERT INTO `sensitive_word_data` VALUES ('售假币');
INSERT INTO `sensitive_word_data` VALUES ('售健卫');
INSERT INTO `sensitive_word_data` VALUES ('售冒名');
INSERT INTO `sensitive_word_data` VALUES ('售军用');
INSERT INTO `sensitive_word_data` VALUES ('售单管');
INSERT INTO `sensitive_word_data` VALUES ('售子弹');
INSERT INTO `sensitive_word_data` VALUES ('售左轮');
INSERT INTO `sensitive_word_data` VALUES ('售弹簧刀');
INSERT INTO `sensitive_word_data` VALUES ('售手枪');
INSERT INTO `sensitive_word_data` VALUES ('售手枪QQ');
INSERT INTO `sensitive_word_data` VALUES ('售枪支');
INSERT INTO `sensitive_word_data` VALUES ('售枪购买手枪');
INSERT INTO `sensitive_word_data` VALUES ('售步枪');
INSERT INTO `sensitive_word_data` VALUES ('售氯胺');
INSERT INTO `sensitive_word_data` VALUES ('售火药');
INSERT INTO `sensitive_word_data` VALUES ('售热武');
INSERT INTO `sensitive_word_data` VALUES ('售狗子');
INSERT INTO `sensitive_word_data` VALUES ('售猎枪');
INSERT INTO `sensitive_word_data` VALUES ('售纯度');
INSERT INTO `sensitive_word_data` VALUES ('售虎头');
INSERT INTO `sensitive_word_data` VALUES ('售防身');
INSERT INTO `sensitive_word_data` VALUES ('售麻醉');
INSERT INTO `sensitive_word_data` VALUES ('啃咬');
INSERT INTO `sensitive_word_data` VALUES ('啄木鳥公司');
INSERT INTO `sensitive_word_data` VALUES ('啄木鸟公司');
INSERT INTO `sensitive_word_data` VALUES ('商务快车 ');
INSERT INTO `sensitive_word_data` VALUES ('商务短信 ');
INSERT INTO `sensitive_word_data` VALUES ('啊扁涛哥');
INSERT INTO `sensitive_word_data` VALUES ('喂奶门');
INSERT INTO `sensitive_word_data` VALUES ('善恶有报');
INSERT INTO `sensitive_word_data` VALUES ('喇嘛');
INSERT INTO `sensitive_word_data` VALUES ('喉交');
INSERT INTO `sensitive_word_data` VALUES ('喘叫');
INSERT INTO `sensitive_word_data` VALUES ('喜贪赃');
INSERT INTO `sensitive_word_data` VALUES ('喷出');
INSERT INTO `sensitive_word_data` VALUES ('喷剂');
INSERT INTO `sensitive_word_data` VALUES ('喷发');
INSERT INTO `sensitive_word_data` VALUES ('喷射');
INSERT INTO `sensitive_word_data` VALUES ('喷尿');
INSERT INTO `sensitive_word_data` VALUES ('喷泄');
INSERT INTO `sensitive_word_data` VALUES ('喷涌');
INSERT INTO `sensitive_word_data` VALUES ('喷精');
INSERT INTO `sensitive_word_data` VALUES ('喷雾型');
INSERT INTO `sensitive_word_data` VALUES ('喷雾型迷药');
INSERT INTO `sensitive_word_data` VALUES ('喷雾迷药');
INSERT INTO `sensitive_word_data` VALUES ('喻林祥');
INSERT INTO `sensitive_word_data` VALUES ('嗑药');
INSERT INTO `sensitive_word_data` VALUES ('嘿咻');
INSERT INTO `sensitive_word_data` VALUES ('器具');
INSERT INTO `sensitive_word_data` VALUES ('器官移植');
INSERT INTO `sensitive_word_data` VALUES ('器官贩卖');
INSERT INTO `sensitive_word_data` VALUES ('噶玛吧');
INSERT INTO `sensitive_word_data` VALUES ('四二五事件');
INSERT INTO `sensitive_word_data` VALUES ('四人帮');
INSERT INTO `sensitive_word_data` VALUES ('四人帮材料');
INSERT INTO `sensitive_word_data` VALUES ('四六级学位证 办证');
INSERT INTO `sensitive_word_data` VALUES ('四六级答案');
INSERT INTO `sensitive_word_data` VALUES ('四博会');
INSERT INTO `sensitive_word_data` VALUES ('四大扯个');
INSERT INTO `sensitive_word_data` VALUES ('四季彩香迅雷下载');
INSERT INTO `sensitive_word_data` VALUES ('四小码');
INSERT INTO `sensitive_word_data` VALUES ('四川大地震异象揭密');
INSERT INTO `sensitive_word_data` VALUES ('四川朱昱');
INSERT INTO `sensitive_word_data` VALUES ('四川独');
INSERT INTO `sensitive_word_data` VALUES ('四川独立');
INSERT INTO `sensitive_word_data` VALUES ('四川销售警用手铐');
INSERT INTO `sensitive_word_data` VALUES ('四我周');
INSERT INTO `sensitive_word_data` VALUES ('四海龙女');
INSERT INTO `sensitive_word_data` VALUES ('四海龙女逃亡艳旅');
INSERT INTO `sensitive_word_data` VALUES ('四种当');
INSERT INTO `sensitive_word_data` VALUES ('四種當');
INSERT INTO `sensitive_word_data` VALUES ('四级答案');
INSERT INTO `sensitive_word_data` VALUES ('回忆六四');
INSERT INTO `sensitive_word_data` VALUES ('回民暴');
INSERT INTO `sensitive_word_data` VALUES ('回民暴动');
INSERT INTO `sensitive_word_data` VALUES ('回民猪');
INSERT INTO `sensitive_word_data` VALUES ('回派');
INSERT INTO `sensitive_word_data` VALUES ('回良玉 ');
INSERT INTO `sensitive_word_data` VALUES ('团派');
INSERT INTO `sensitive_word_data` VALUES ('园凶杀');
INSERT INTO `sensitive_word_data` VALUES ('园发生砍');
INSERT INTO `sensitive_word_data` VALUES ('园惨案');
INSERT INTO `sensitive_word_data` VALUES ('园砍杀');
INSERT INTO `sensitive_word_data` VALUES ('园血案');
INSERT INTO `sensitive_word_data` VALUES ('困境');
INSERT INTO `sensitive_word_data` VALUES ('围攻上海');
INSERT INTO `sensitive_word_data` VALUES ('围攻警');
INSERT INTO `sensitive_word_data` VALUES ('围绕土地问题的攻防');
INSERT INTO `sensitive_word_data` VALUES ('国wu院');
INSERT INTO `sensitive_word_data` VALUES ('国一九五七');
INSERT INTO `sensitive_word_data` VALUES ('国之利刃');
INSERT INTO `sensitive_word_data` VALUES ('国之母');
INSERT INTO `sensitive_word_data` VALUES ('国产汽狗专卖|气枪货');
INSERT INTO `sensitive_word_data` VALUES ('国军');
INSERT INTO `sensitive_word_data` VALUES ('国姆');
INSERT INTO `sensitive_word_data` VALUES ('国家吞得');
INSERT INTO `sensitive_word_data` VALUES ('国家妓');
INSERT INTO `sensitive_word_data` VALUES ('国家安全');
INSERT INTO `sensitive_word_data` VALUES ('国家政府档案');
INSERT INTO `sensitive_word_data` VALUES ('国家机密');
INSERT INTO `sensitive_word_data` VALUES ('国家粮油统计报表');
INSERT INTO `sensitive_word_data` VALUES ('国家软弱');
INSERT INTO `sensitive_word_data` VALUES ('国峰');
INSERT INTO `sensitive_word_data` VALUES ('国库折');
INSERT INTO `sensitive_word_data` VALUES ('国新办主任');
INSERT INTO `sensitive_word_data` VALUES ('国歌股歌');
INSERT INTO `sensitive_word_data` VALUES ('国母');
INSERT INTO `sensitive_word_data` VALUES ('国民党=G·M·D');
INSERT INTO `sensitive_word_data` VALUES ('国民党名单');
INSERT INTO `sensitive_word_data` VALUES ('国研新闻邮件');
INSERT INTO `sensitive_word_data` VALUES ('国色天香网');
INSERT INTO `sensitive_word_data` VALUES ('国贼');
INSERT INTO `sensitive_word_data` VALUES ('国锋');
INSERT INTO `sensitive_word_data` VALUES ('国际声援西藏');
INSERT INTO `sensitive_word_data` VALUES ('国际投注');
INSERT INTO `sensitive_word_data` VALUES ('国际援藏网');
INSERT INTO `sensitive_word_data` VALUES ('国际西藏运动');
INSERT INTO `sensitive_word_data` VALUES ('国际足坛五大');
INSERT INTO `sensitive_word_data` VALUES ('图说文化大革命');
INSERT INTO `sensitive_word_data` VALUES ('图道多吉');
INSERT INTO `sensitive_word_data` VALUES ('圆明网');
INSERT INTO `sensitive_word_data` VALUES ('圆润');
INSERT INTO `sensitive_word_data` VALUES ('圆滚');
INSERT INTO `sensitive_word_data` VALUES ('圆满');
INSERT INTO `sensitive_word_data` VALUES ('圆粗');
INSERT INTO `sensitive_word_data` VALUES ('圆翘');
INSERT INTO `sensitive_word_data` VALUES ('圆臀');
INSERT INTO `sensitive_word_data` VALUES ('圆鼓鼓');
INSERT INTO `sensitive_word_data` VALUES ('國內美');
INSERT INTO `sensitive_word_data` VALUES ('土g');
INSERT INTO `sensitive_word_data` VALUES ('土共');
INSERT INTO `sensitive_word_data` VALUES ('土制C4');
INSERT INTO `sensitive_word_data` VALUES ('土枪 ');
INSERT INTO `sensitive_word_data` VALUES ('土炮 ');
INSERT INTO `sensitive_word_data` VALUES ('土炸药成份');
INSERT INTO `sensitive_word_data` VALUES ('圣战不息');
INSERT INTO `sensitive_word_data` VALUES ('圣战组织');
INSERT INTO `sensitive_word_data` VALUES ('圣殿教 ');
INSERT INTO `sensitive_word_data` VALUES ('圣火护卫');
INSERT INTO `sensitive_word_data` VALUES ('圣灵重建教会 ');
INSERT INTO `sensitive_word_data` VALUES ('圣辉');
INSERT INTO `sensitive_word_data` VALUES ('在十月');
INSERT INTO `sensitive_word_data` VALUES ('在家里做原子弹');
INSERT INTO `sensitive_word_data` VALUES ('在家里做原子弹的方法');
INSERT INTO `sensitive_word_data` VALUES ('在火车上被人强奸');
INSERT INTO `sensitive_word_data` VALUES ('在线办理证件');
INSERT INTO `sensitive_word_data` VALUES ('在线博彩网站');
INSERT INTO `sensitive_word_data` VALUES ('地下先烈');
INSERT INTO `sensitive_word_data` VALUES ('地下先烈们纷纷来电询问');
INSERT INTO `sensitive_word_data` VALUES ('地下刊物');
INSERT INTO `sensitive_word_data` VALUES ('地下教会');
INSERT INTO `sensitive_word_data` VALUES ('地下的先烈们电话询问');
INSERT INTO `sensitive_word_data` VALUES ('地下的先烈们纷纷打来电话');
INSERT INTO `sensitive_word_data` VALUES ('地下的先烈们纷纷打来电话询问');
INSERT INTO `sensitive_word_data` VALUES ('地产之歌');
INSERT INTO `sensitive_word_data` VALUES ('地块规划');
INSERT INTO `sensitive_word_data` VALUES ('地塞米松');
INSERT INTO `sensitive_word_data` VALUES ('地奈德');
INSERT INTO `sensitive_word_data` VALUES ('地方召会');
INSERT INTO `sensitive_word_data` VALUES ('地点');
INSERT INTO `sensitive_word_data` VALUES ('地磅仪');
INSERT INTO `sensitive_word_data` VALUES ('地磅儀');
INSERT INTO `sensitive_word_data` VALUES ('地磅解码器 ');
INSERT INTO `sensitive_word_data` VALUES ('地磅遥控器 ');
INSERT INTO `sensitive_word_data` VALUES ('地磅遥控器解码器');
INSERT INTO `sensitive_word_data` VALUES ('地税髮票QQ');
INSERT INTO `sensitive_word_data` VALUES ('地西泮');
INSERT INTO `sensitive_word_data` VALUES ('地西泮片');
INSERT INTO `sensitive_word_data` VALUES ('地震哥');
INSERT INTO `sensitive_word_data` VALUES ('地震天谴');
INSERT INTO `sensitive_word_data` VALUES ('地震来得更猛烈');
INSERT INTO `sensitive_word_data` VALUES ('地震预测');
INSERT INTO `sensitive_word_data` VALUES ('场合');
INSERT INTO `sensitive_word_data` VALUES ('场景');
INSERT INTO `sensitive_word_data` VALUES ('坐交');
INSERT INTO `sensitive_word_data` VALUES ('坐位');
INSERT INTO `sensitive_word_data` VALUES ('坐位性交');
INSERT INTO `sensitive_word_data` VALUES ('坐台');
INSERT INTO `sensitive_word_data` VALUES ('坐姿');
INSERT INTO `sensitive_word_data` VALUES ('坐式性交');
INSERT INTO `sensitive_word_data` VALUES ('坐爱');
INSERT INTO `sensitive_word_data` VALUES ('坐骨海绵体肌');
INSERT INTO `sensitive_word_data` VALUES ('坚实');
INSERT INTO `sensitive_word_data` VALUES ('坚挺');
INSERT INTO `sensitive_word_data` VALUES ('坚挺的东西');
INSERT INTO `sensitive_word_data` VALUES ('坚硬');
INSERT INTO `sensitive_word_data` VALUES ('垂软');
INSERT INTO `sensitive_word_data` VALUES ('型手枪');
INSERT INTO `sensitive_word_data` VALUES ('埋进');
INSERT INTO `sensitive_word_data` VALUES ('城市激情聊天室');
INSERT INTO `sensitive_word_data` VALUES ('城管');
INSERT INTO `sensitive_word_data` VALUES ('城管灭');
INSERT INTO `sensitive_word_data` VALUES ('域名备案');
INSERT INTO `sensitive_word_data` VALUES ('培养');
INSERT INTO `sensitive_word_data` VALUES ('基地组织校园招聘');
INSERT INTO `sensitive_word_data` VALUES ('基本配方是硝酸甘');
INSERT INTO `sensitive_word_data` VALUES ('基本靠吼');
INSERT INTO `sensitive_word_data` VALUES ('基督');
INSERT INTO `sensitive_word_data` VALUES ('基督灵恩布道团 ');
INSERT INTO `sensitive_word_data` VALUES ('堕落指南');
INSERT INTO `sensitive_word_data` VALUES ('塑胶原料的流动性');
INSERT INTO `sensitive_word_data` VALUES ('塑胶炸弹');
INSERT INTO `sensitive_word_data` VALUES ('塔克拉玛干人权联合会');
INSERT INTO `sensitive_word_data` VALUES ('境界');
INSERT INTO `sensitive_word_data` VALUES ('增加');
INSERT INTO `sensitive_word_data` VALUES ('增粗');
INSERT INTO `sensitive_word_data` VALUES ('墨文川');
INSERT INTO `sensitive_word_data` VALUES ('壁肉');
INSERT INTO `sensitive_word_data` VALUES ('士康事件');
INSERT INTO `sensitive_word_data` VALUES ('士的宁 ');
INSERT INTO `sensitive_word_data` VALUES ('士的年 ');
INSERT INTO `sensitive_word_data` VALUES ('壮大');
INSERT INTO `sensitive_word_data` VALUES ('壮盛');
INSERT INTO `sensitive_word_data` VALUES ('壮神鞭');
INSERT INTO `sensitive_word_data` VALUES ('壮阳药');
INSERT INTO `sensitive_word_data` VALUES ('声援');
INSERT INTO `sensitive_word_data` VALUES ('壶腹部');
INSERT INTO `sensitive_word_data` VALUES ('壽綾乃全集');
INSERT INTO `sensitive_word_data` VALUES ('处女');
INSERT INTO `sensitive_word_data` VALUES ('处女终结者**');
INSERT INTO `sensitive_word_data` VALUES ('处女终结者{MOD}');
INSERT INTO `sensitive_word_data` VALUES ('处男');
INSERT INTO `sensitive_word_data` VALUES ('复制地址到地址栏');
INSERT INTO `sensitive_word_data` VALUES ('复制手机卡');
INSERT INTO `sensitive_word_data` VALUES ('复印件制');
INSERT INTO `sensitive_word_data` VALUES ('复印件生');
INSERT INTO `sensitive_word_data` VALUES ('复员军官代表');
INSERT INTO `sensitive_word_data` VALUES ('复员军官诉求书');
INSERT INTO `sensitive_word_data` VALUES ('复转军人');
INSERT INTO `sensitive_word_data` VALUES ('夏培度');
INSERT INTO `sensitive_word_data` VALUES ('夏家骏');
INSERT INTO `sensitive_word_data` VALUES ('夏春荣');
INSERT INTO `sensitive_word_data` VALUES ('夏樹亞矢电驴下载');
INSERT INTO `sensitive_word_data` VALUES ('夏赞忠');
INSERT INTO `sensitive_word_data` VALUES ('夏逸陶');
INSERT INTO `sensitive_word_data` VALUES ('夕樹舞子');
INSERT INTO `sensitive_word_data` VALUES ('外交与方略');
INSERT INTO `sensitive_word_data` VALUES ('外交论坛');
INSERT INTO `sensitive_word_data` VALUES ('外围码');
INSERT INTO `sensitive_word_data` VALUES ('外围赌球');
INSERT INTO `sensitive_word_data` VALUES ('外蒙');
INSERT INTO `sensitive_word_data` VALUES ('外透视镜');
INSERT INTO `sensitive_word_data` VALUES ('多党');
INSERT INTO `sensitive_word_data` VALUES ('多党执政');
INSERT INTO `sensitive_word_data` VALUES ('多吉才让');
INSERT INTO `sensitive_word_data` VALUES ('多囊卵巢综合征');
INSERT INTO `sensitive_word_data` VALUES ('多处财产已被银行查封');
INSERT INTO `sensitive_word_data` VALUES ('多姿');
INSERT INTO `sensitive_word_data` VALUES ('多少次高潮');
INSERT INTO `sensitive_word_data` VALUES ('多情');
INSERT INTO `sensitive_word_data` VALUES ('多毛');
INSERT INTO `sensitive_word_data` VALUES ('多汁');
INSERT INTO `sensitive_word_data` VALUES ('多睾');
INSERT INTO `sensitive_word_data` VALUES ('多维');
INSERT INTO `sensitive_word_data` VALUES ('多维新闻');
INSERT INTO `sensitive_word_data` VALUES ('多美康');
INSERT INTO `sensitive_word_data` VALUES ('多肉');
INSERT INTO `sensitive_word_data` VALUES ('多黨');
INSERT INTO `sensitive_word_data` VALUES ('夜勤病栋');
INSERT INTO `sensitive_word_data` VALUES ('夜半加税');
INSERT INTO `sensitive_word_data` VALUES ('夜总会');
INSERT INTO `sensitive_word_data` VALUES ('夜激情');
INSERT INTO `sensitive_word_data` VALUES ('夜總會');
INSERT INTO `sensitive_word_data` VALUES ('夜话紫禁城');
INSERT INTO `sensitive_word_data` VALUES ('够骚');
INSERT INTO `sensitive_word_data` VALUES ('夢幻麻將館');
INSERT INTO `sensitive_word_data` VALUES ('大b');
INSERT INTO `sensitive_word_data` VALUES ('大sb');
INSERT INTO `sensitive_word_data` VALUES ('大{10}纪{10}元');
INSERT INTO `sensitive_word_data` VALUES ('大丑风流记全文阅读');
INSERT INTO `sensitive_word_data` VALUES ('大中华论坛');
INSERT INTO `sensitive_word_data` VALUES ('大中国论坛');
INSERT INTO `sensitive_word_data` VALUES ('大乳');
INSERT INTO `sensitive_word_data` VALUES ('大众真人真事');
INSERT INTO `sensitive_word_data` VALUES ('大参考');
INSERT INTO `sensitive_word_data` VALUES ('大叔');
INSERT INTO `sensitive_word_data` VALUES ('大史');
INSERT INTO `sensitive_word_data` VALUES ('大史纪');
INSERT INTO `sensitive_word_data` VALUES ('大史记');
INSERT INTO `sensitive_word_data` VALUES ('大哥');
INSERT INTO `sensitive_word_data` VALUES ('大嘴歌');
INSERT INTO `sensitive_word_data` VALUES ('大圆满法 ');
INSERT INTO `sensitive_word_data` VALUES ('大型地网');
INSERT INTO `sensitive_word_data` VALUES ('大奶');
INSERT INTO `sensitive_word_data` VALUES ('大奶头');
INSERT INTO `sensitive_word_data` VALUES ('大奶子');
INSERT INTO `sensitive_word_data` VALUES ('大妈');
INSERT INTO `sensitive_word_data` VALUES ('大妓院');
INSERT INTO `sensitive_word_data` VALUES ('大妹');
INSERT INTO `sensitive_word_data` VALUES ('大姊');
INSERT INTO `sensitive_word_data` VALUES ('大姐');
INSERT INTO `sensitive_word_data` VALUES ('大姨');
INSERT INTO `sensitive_word_data` VALUES ('大娘');
INSERT INTO `sensitive_word_data` VALUES ('大嫂');
INSERT INTO `sensitive_word_data` VALUES ('大学暴动 ');
INSERT INTO `sensitive_word_data` VALUES ('大学骚乱');
INSERT INTO `sensitive_word_data` VALUES ('大學騷亂');
INSERT INTO `sensitive_word_data` VALUES ('大家论坛');
INSERT INTO `sensitive_word_data` VALUES ('大庄');
INSERT INTO `sensitive_word_data` VALUES ('大庆工潮');
INSERT INTO `sensitive_word_data` VALUES ('大彩');
INSERT INTO `sensitive_word_data` VALUES ('大扎荣');
INSERT INTO `sensitive_word_data` VALUES ('大批贪官');
INSERT INTO `sensitive_word_data` VALUES ('大抽');
INSERT INTO `sensitive_word_data` VALUES ('大揭露');
INSERT INTO `sensitive_word_data` VALUES ('大比');
INSERT INTO `sensitive_word_data` VALUES ('大泄');
INSERT INTO `sensitive_word_data` VALUES ('大法');
INSERT INTO `sensitive_word_data` VALUES ('大法之声');
INSERT INTO `sensitive_word_data` VALUES ('大法修炼者 ');
INSERT INTO `sensitive_word_data` VALUES ('大法大福');
INSERT INTO `sensitive_word_data` VALUES ('大法大纪园');
INSERT INTO `sensitive_word_data` VALUES ('大法师傅');
INSERT INTO `sensitive_word_data` VALUES ('大法弟子');
INSERT INTO `sensitive_word_data` VALUES ('大法新闻社');
INSERT INTO `sensitive_word_data` VALUES ('大法洪传');
INSERT INTO `sensitive_word_data` VALUES ('大波');
INSERT INTO `sensitive_word_data` VALUES ('大浦安娜全集');
INSERT INTO `sensitive_word_data` VALUES ('大澤惠');
INSERT INTO `sensitive_word_data` VALUES ('大熊猫被猎杀');
INSERT INTO `sensitive_word_data` VALUES ('大爷');
INSERT INTO `sensitive_word_data` VALUES ('大片');
INSERT INTO `sensitive_word_data` VALUES ('大珐');
INSERT INTO `sensitive_word_data` VALUES ('大田高山茶');
INSERT INTO `sensitive_word_data` VALUES ('大盖帽');
INSERT INTO `sensitive_word_data` VALUES ('大砝弟子');
INSERT INTO `sensitive_word_data` VALUES ('大硞弟子');
INSERT INTO `sensitive_word_data` VALUES ('大祚荣');
INSERT INTO `sensitive_word_data` VALUES ('大禁');
INSERT INTO `sensitive_word_data` VALUES ('大穴');
INSERT INTO `sensitive_word_data` VALUES ('大空明日香迅雷下载');
INSERT INTO `sensitive_word_data` VALUES ('大紀元');
INSERT INTO `sensitive_word_data` VALUES ('大纪');
INSERT INTO `sensitive_word_data` VALUES ('大纪元');
INSERT INTO `sensitive_word_data` VALUES ('大纪元新闻网');
INSERT INTO `sensitive_word_data` VALUES ('大纪元时报');
INSERT INTO `sensitive_word_data` VALUES ('大纪元杂志');
INSERT INTO `sensitive_word_data` VALUES ('大纪元法轮功');
INSERT INTO `sensitive_word_data` VALUES ('大纪园');
INSERT INTO `sensitive_word_data` VALUES ('大肉');
INSERT INTO `sensitive_word_data` VALUES ('大肉棒');
INSERT INTO `sensitive_word_data` VALUES ('大腿');
INSERT INTO `sensitive_word_data` VALUES ('大花逼');
INSERT INTO `sensitive_word_data` VALUES ('大荣');
INSERT INTO `sensitive_word_data` VALUES ('大蓋帽');
INSERT INTO `sensitive_word_data` VALUES ('大记元');
INSERT INTO `sensitive_word_data` VALUES ('大跃进');
INSERT INTO `sensitive_word_data` VALUES ('大逼');
INSERT INTO `sensitive_word_data` VALUES ('大量伪币出售');
INSERT INTO `sensitive_word_data` VALUES ('大量出售成品 麻古');
INSERT INTO `sensitive_word_data` VALUES ('大量批发供应弓驽');
INSERT INTO `sensitive_word_data` VALUES ('大量现货');
INSERT INTO `sensitive_word_data` VALUES ('大陆');
INSERT INTO `sensitive_word_data` VALUES ('大陆同修');
INSERT INTO `sensitive_word_data` VALUES ('大陆官员');
INSERT INTO `sensitive_word_data` VALUES ('大陆官方');
INSERT INTO `sensitive_word_data` VALUES ('大陆当局');
INSERT INTO `sensitive_word_data` VALUES ('大陆当权者');
INSERT INTO `sensitive_word_data` VALUES ('大陆独裁者');
INSERT INTO `sensitive_word_data` VALUES ('大雞巴');
INSERT INTO `sensitive_word_data` VALUES ('大鸡巴');
INSERT INTO `sensitive_word_data` VALUES ('大麻');
INSERT INTO `sensitive_word_data` VALUES ('大黑鹰驽弓专卖');
INSERT INTO `sensitive_word_data` VALUES ('天互数据');
INSERT INTO `sensitive_word_data` VALUES ('天伦王朝');
INSERT INTO `sensitive_word_data` VALUES ('天国乐团');
INSERT INTO `sensitive_word_data` VALUES ('天安門');
INSERT INTO `sensitive_word_data` VALUES ('天安门');
INSERT INTO `sensitive_word_data` VALUES ('天安门一代');
INSERT INTO `sensitive_word_data` VALUES ('天安门事件');
INSERT INTO `sensitive_word_data` VALUES ('天安门大屠杀');
INSERT INTO `sensitive_word_data` VALUES ('天安门屠杀');
INSERT INTO `sensitive_word_data` VALUES ('天安门录影带');
INSERT INTO `sensitive_word_data` VALUES ('天安门时间');
INSERT INTO `sensitive_word_data` VALUES ('天安门纪念基金会');
INSERT INTO `sensitive_word_data` VALUES ('天府广场');
INSERT INTO `sensitive_word_data` VALUES ('天府广场集会');
INSERT INTO `sensitive_word_data` VALUES ('天怒');
INSERT INTO `sensitive_word_data` VALUES ('天推广歌');
INSERT INTO `sensitive_word_data` VALUES ('天朝特');
INSERT INTO `sensitive_word_data` VALUES ('天权');
INSERT INTO `sensitive_word_data` VALUES ('天津事件');
INSERT INTO `sensitive_word_data` VALUES ('天津代办四六级证');
INSERT INTO `sensitive_word_data` VALUES ('天津代办驾照文凭');
INSERT INTO `sensitive_word_data` VALUES ('天津代开 发票');
INSERT INTO `sensitive_word_data` VALUES ('天津出售假钱');
INSERT INTO `sensitive_word_data` VALUES ('天津删贴公司');
INSERT INTO `sensitive_word_data` VALUES ('天津办六级证');
INSERT INTO `sensitive_word_data` VALUES ('天津办毕业证');
INSERT INTO `sensitive_word_data` VALUES ('天津办证刻章qq');
INSERT INTO `sensitive_word_data` VALUES ('天津办证刻章发票');
INSERT INTO `sensitive_word_data` VALUES ('天津各国签证 代办');
INSERT INTO `sensitive_word_data` VALUES ('天津哪里有猎 枪');
INSERT INTO `sensitive_word_data` VALUES ('天津开机打发票');
INSERT INTO `sensitive_word_data` VALUES ('天津签证学历证明');
INSERT INTO `sensitive_word_data` VALUES ('天津网上办证');
INSERT INTO `sensitive_word_data` VALUES ('天津网上办证qq');
INSERT INTO `sensitive_word_data` VALUES ('天津职称资格证');
INSERT INTO `sensitive_word_data` VALUES ('天津负面信息处理');
INSERT INTO `sensitive_word_data` VALUES ('天津高仿真毕业证');
INSERT INTO `sensitive_word_data` VALUES ('天浴');
INSERT INTO `sensitive_word_data` VALUES ('天灭中');
INSERT INTO `sensitive_word_data` VALUES ('天灭中共');
INSERT INTO `sensitive_word_data` VALUES ('天父的儿女');
INSERT INTO `sensitive_word_data` VALUES ('天葬 ');
INSERT INTO `sensitive_word_data` VALUES ('天衣美津迅雷下载');
INSERT INTO `sensitive_word_data` VALUES ('天要亡 ');
INSERT INTO `sensitive_word_data` VALUES ('天要灭');
INSERT INTO `sensitive_word_data` VALUES ('天要灭中');
INSERT INTO `sensitive_word_data` VALUES ('天音功');
INSERT INTO `sensitive_word_data` VALUES ('天鵝之旅');
INSERT INTO `sensitive_word_data` VALUES ('天鹅之旅');
INSERT INTO `sensitive_word_data` VALUES ('太原增值税发票');
INSERT INTO `sensitive_word_data` VALUES ('太子党');
INSERT INTO `sensitive_word_data` VALUES ('太子党名单');
INSERT INTO `sensitive_word_data` VALUES ('太子黨');
INSERT INTO `sensitive_word_data` VALUES ('太王四神');
INSERT INTO `sensitive_word_data` VALUES ('太王四神记');
INSERT INTO `sensitive_word_data` VALUES ('太王死神记');
INSERT INTO `sensitive_word_data` VALUES ('太王记');
INSERT INTO `sensitive_word_data` VALUES ('太阳圣殿教');
INSERT INTO `sensitive_word_data` VALUES ('夫妇乐园');
INSERT INTO `sensitive_word_data` VALUES ('夫妻');
INSERT INTO `sensitive_word_data` VALUES ('夫妻交换');
INSERT INTO `sensitive_word_data` VALUES ('夫妻作爱');
INSERT INTO `sensitive_word_data` VALUES ('夫妻做爱');
INSERT INTO `sensitive_word_data` VALUES ('央视内部晚会');
INSERT INTO `sensitive_word_data` VALUES ('失密');
INSERT INTO `sensitive_word_data` VALUES ('失忆药');
INSERT INTO `sensitive_word_data` VALUES ('失意药');
INSERT INTO `sensitive_word_data` VALUES ('失意藥');
INSERT INTO `sensitive_word_data` VALUES ('失身水');
INSERT INTO `sensitive_word_data` VALUES ('失身粉专卖');
INSERT INTO `sensitive_word_data` VALUES ('失身粉迷奸药专卖');
INSERT INTO `sensitive_word_data` VALUES ('失身粉迷情水供应');
INSERT INTO `sensitive_word_data` VALUES ('失身药');
INSERT INTO `sensitive_word_data` VALUES ('头双管');
INSERT INTO `sensitive_word_data` VALUES ('夹住');
INSERT INTO `sensitive_word_data` VALUES ('夹着');
INSERT INTO `sensitive_word_data` VALUES ('夹紧');
INSERT INTO `sensitive_word_data` VALUES ('夹著');
INSERT INTO `sensitive_word_data` VALUES ('奇淫');
INSERT INTO `sensitive_word_data` VALUES ('奇淫散');
INSERT INTO `sensitive_word_data` VALUES ('奇痒');
INSERT INTO `sensitive_word_data` VALUES ('奇迹的黄');
INSERT INTO `sensitive_word_data` VALUES ('奉恒高');
INSERT INTO `sensitive_word_data` VALUES ('套牌');
INSERT INTO `sensitive_word_data` VALUES ('套牌罢吃');
INSERT INTO `sensitive_word_data` VALUES ('奚国华');
INSERT INTO `sensitive_word_data` VALUES ('奢恋');
INSERT INTO `sensitive_word_data` VALUES ('奥你妈的运');
INSERT INTO `sensitive_word_data` VALUES ('奥孕');
INSERT INTO `sensitive_word_data` VALUES ('奥晕');
INSERT INTO `sensitive_word_data` VALUES ('奧菜亞美BT下载');
INSERT INTO `sensitive_word_data` VALUES ('女上位');
INSERT INTO `sensitive_word_data` VALUES ('女上式');
INSERT INTO `sensitive_word_data` VALUES ('女上门');
INSERT INTO `sensitive_word_data` VALUES ('女下');
INSERT INTO `sensitive_word_data` VALUES ('女下位');
INSERT INTO `sensitive_word_data` VALUES ('女人');
INSERT INTO `sensitive_word_data` VALUES ('女人和狗');
INSERT INTO `sensitive_word_data` VALUES ('女人的BB');
INSERT INTO `sensitive_word_data` VALUES ('女任职名');
INSERT INTO `sensitive_word_data` VALUES ('女优');
INSERT INTO `sensitive_word_data` VALUES ('女优片');
INSERT INTO `sensitive_word_data` VALUES ('女伟哥 ');
INSERT INTO `sensitive_word_data` VALUES ('女優');
INSERT INTO `sensitive_word_data` VALUES ('女儿');
INSERT INTO `sensitive_word_data` VALUES ('女前男后');
INSERT INTO `sensitive_word_data` VALUES ('女卧男立式');
INSERT INTO `sensitive_word_data` VALUES ('女友坊');
INSERT INTO `sensitive_word_data` VALUES ('女器');
INSERT INTO `sensitive_word_data` VALUES ('女士');
INSERT INTO `sensitive_word_data` VALUES ('女士服务 ');
INSERT INTO `sensitive_word_data` VALUES ('女女');
INSERT INTO `sensitive_word_data` VALUES ('女奴 ');
INSERT INTO `sensitive_word_data` VALUES ('女婿');
INSERT INTO `sensitive_word_data` VALUES ('女子性冷淡');
INSERT INTO `sensitive_word_data` VALUES ('女尻');
INSERT INTO `sensitive_word_data` VALUES ('女性');
INSERT INTO `sensitive_word_data` VALUES ('女性不孕症');
INSERT INTO `sensitive_word_data` VALUES ('女性外生殖器');
INSERT INTO `sensitive_word_data` VALUES ('女性性冷淡');
INSERT INTO `sensitive_word_data` VALUES ('女性性功能障碍');
INSERT INTO `sensitive_word_data` VALUES ('女性性洁症');
INSERT INTO `sensitive_word_data` VALUES ('女技师');
INSERT INTO `sensitive_word_data` VALUES ('女教師');
INSERT INTO `sensitive_word_data` VALUES ('女方');
INSERT INTO `sensitive_word_data` VALUES ('女方跪臀位');
INSERT INTO `sensitive_word_data` VALUES ('女死囚');
INSERT INTO `sensitive_word_data` VALUES ('女激情');
INSERT INTO `sensitive_word_data` VALUES ('女畅男欢');
INSERT INTO `sensitive_word_data` VALUES ('女神教');
INSERT INTO `sensitive_word_data` VALUES ('女童');
INSERT INTO `sensitive_word_data` VALUES ('女臀');
INSERT INTO `sensitive_word_data` VALUES ('女色情自拍');
INSERT INTO `sensitive_word_data` VALUES ('女被人家搞');
INSERT INTO `sensitive_word_data` VALUES ('女阴');
INSERT INTO `sensitive_word_data` VALUES ('奴事件');
INSERT INTO `sensitive_word_data` VALUES ('奴役童工');
INSERT INTO `sensitive_word_data` VALUES ('奶头');
INSERT INTO `sensitive_word_data` VALUES ('奶头真红');
INSERT INTO `sensitive_word_data` VALUES ('奶子');
INSERT INTO `sensitive_word_data` VALUES ('奶尖');
INSERT INTO `sensitive_word_data` VALUES ('奶房');
INSERT INTO `sensitive_word_data` VALUES ('奶水');
INSERT INTO `sensitive_word_data` VALUES ('奶頭');
INSERT INTO `sensitive_word_data` VALUES ('奸尸');
INSERT INTO `sensitive_word_data` VALUES ('奸弄');
INSERT INTO `sensitive_word_data` VALUES ('奸成瘾');
INSERT INTO `sensitive_word_data` VALUES ('奸我');
INSERT INTO `sensitive_word_data` VALUES ('奸插');
INSERT INTO `sensitive_word_data` VALUES ('奸杀 ');
INSERT INTO `sensitive_word_data` VALUES ('奸污');
INSERT INTO `sensitive_word_data` VALUES ('奸淫');
INSERT INTO `sensitive_word_data` VALUES ('奸虐');
INSERT INTO `sensitive_word_data` VALUES ('奸辱');
INSERT INTO `sensitive_word_data` VALUES ('奸魔Ⅱ');
INSERT INTO `sensitive_word_data` VALUES ('好性');
INSERT INTO `sensitive_word_data` VALUES ('好棒');
INSERT INTO `sensitive_word_data` VALUES ('好歌分享');
INSERT INTO `sensitive_word_data` VALUES ('好爽');
INSERT INTO `sensitive_word_data` VALUES ('如何做炸弹');
INSERT INTO `sensitive_word_data` VALUES ('如何做炸药');
INSERT INTO `sensitive_word_data` VALUES ('如何制作原子弹');
INSERT INTO `sensitive_word_data` VALUES ('如何制硝化甘油');
INSERT INTO `sensitive_word_data` VALUES ('如何自制燃烧弹');
INSERT INTO `sensitive_word_data` VALUES ('如何造原子弹');
INSERT INTO `sensitive_word_data` VALUES ('如厕死');
INSERT INTO `sensitive_word_data` VALUES ('妇人');
INSERT INTO `sensitive_word_data` VALUES ('妇方');
INSERT INTO `sensitive_word_data` VALUES ('妇的哀羞');
INSERT INTO `sensitive_word_data` VALUES ('妇销魂');
INSERT INTO `sensitive_word_data` VALUES ('妈 咪');
INSERT INTO `sensitive_word_data` VALUES ('妈B=[请文明用语]');
INSERT INTO `sensitive_word_data` VALUES ('妈个');
INSERT INTO `sensitive_word_data` VALUES ('妈了个逼');
INSERT INTO `sensitive_word_data` VALUES ('妈咪');
INSERT INTO `sensitive_word_data` VALUES ('妈妈的=>他*的');
INSERT INTO `sensitive_word_data` VALUES ('妈批');
INSERT INTO `sensitive_word_data` VALUES ('妈逼');
INSERT INTO `sensitive_word_data` VALUES ('妓{4}女');
INSERT INTO `sensitive_word_data` VALUES ('妓女');
INSERT INTO `sensitive_word_data` VALUES ('妓女=>jnv');
INSERT INTO `sensitive_word_data` VALUES ('妓女的口号');
INSERT INTO `sensitive_word_data` VALUES ('妙目');
INSERT INTO `sensitive_word_data` VALUES ('妞上门');
INSERT INTO `sensitive_word_data` VALUES ('妹 妹');
INSERT INTO `sensitive_word_data` VALUES ('妹上门');
INSERT INTO `sensitive_word_data` VALUES ('妹夫');
INSERT INTO `sensitive_word_data` VALUES ('妹妹');
INSERT INTO `sensitive_word_data` VALUES ('妹按摩');
INSERT INTO `sensitive_word_data` VALUES ('妹疼');
INSERT INTO `sensitive_word_data` VALUES ('妹痛');
INSERT INTO `sensitive_word_data` VALUES ('姐 姐');
INSERT INTO `sensitive_word_data` VALUES ('姐{4}妹');
INSERT INTO `sensitive_word_data` VALUES ('姐上门');
INSERT INTO `sensitive_word_data` VALUES ('姐兼职');
INSERT INTO `sensitive_word_data` VALUES ('姐包夜');
INSERT INTO `sensitive_word_data` VALUES ('姐夫');
INSERT INTO `sensitive_word_data` VALUES ('姐妹们');
INSERT INTO `sensitive_word_data` VALUES ('姐姐');
INSERT INTO `sensitive_word_data` VALUES ('姐姐开苞');
INSERT INTO `sensitive_word_data` VALUES ('姐服务');
INSERT INTO `sensitive_word_data` VALUES ('姐疼');
INSERT INTO `sensitive_word_data` VALUES ('姐痛');
INSERT INTO `sensitive_word_data` VALUES ('姑妈');
INSERT INTO `sensitive_word_data` VALUES ('姑姑');
INSERT INTO `sensitive_word_data` VALUES ('姑母');
INSERT INTO `sensitive_word_data` VALUES ('姑爷');
INSERT INTO `sensitive_word_data` VALUES ('姓忽悠');
INSERT INTO `sensitive_word_data` VALUES ('委坐船');
INSERT INTO `sensitive_word_data` VALUES ('姚依林');
INSERT INTO `sensitive_word_data` VALUES ('姚守拙');
INSERT INTO `sensitive_word_data` VALUES ('姚志彬');
INSERT INTO `sensitive_word_data` VALUES ('姚明进去');
INSERT INTO `sensitive_word_data` VALUES ('姚月谦');
INSERT INTO `sensitive_word_data` VALUES ('姚湘成');
INSERT INTO `sensitive_word_data` VALUES ('姜凤阁');
INSERT INTO `sensitive_word_data` VALUES ('姜恩柱');
INSERT INTO `sensitive_word_data` VALUES ('姜春云');
INSERT INTO `sensitive_word_data` VALUES ('姜笑琴');
INSERT INTO `sensitive_word_data` VALUES ('姜颖');
INSERT INTO `sensitive_word_data` VALUES ('姥姥');
INSERT INTO `sensitive_word_data` VALUES ('姦淫');
INSERT INTO `sensitive_word_data` VALUES ('姬木杏奈种子');
INSERT INTO `sensitive_word_data` VALUES ('姬胜德');
INSERT INTO `sensitive_word_data` VALUES ('姿不对死');
INSERT INTO `sensitive_word_data` VALUES ('姿势');
INSERT INTO `sensitive_word_data` VALUES ('娇叫');
INSERT INTO `sensitive_word_data` VALUES ('娇吟');
INSERT INTO `sensitive_word_data` VALUES ('娇呼');
INSERT INTO `sensitive_word_data` VALUES ('娇哼');
INSERT INTO `sensitive_word_data` VALUES ('娇啼');
INSERT INTO `sensitive_word_data` VALUES ('娇喘');
INSERT INTO `sensitive_word_data` VALUES ('娇声');
INSERT INTO `sensitive_word_data` VALUES ('娇娘');
INSERT INTO `sensitive_word_data` VALUES ('娇媚');
INSERT INTO `sensitive_word_data` VALUES ('娇嫩');
INSERT INTO `sensitive_word_data` VALUES ('娇容');
INSERT INTO `sensitive_word_data` VALUES ('娇小');
INSERT INTO `sensitive_word_data` VALUES ('娇弱');
INSERT INTO `sensitive_word_data` VALUES ('娇态');
INSERT INTO `sensitive_word_data` VALUES ('娇笑');
INSERT INTO `sensitive_word_data` VALUES ('娇艳');
INSERT INTO `sensitive_word_data` VALUES ('娇躯');
INSERT INTO `sensitive_word_data` VALUES ('娇软');
INSERT INTO `sensitive_word_data` VALUES ('娘两腿之间');
INSERT INTO `sensitive_word_data` VALUES ('娘西皮');
INSERT INTO `sensitive_word_data` VALUES ('娥眉气枪出售');
INSERT INTO `sensitive_word_data` VALUES ('娱乐透视');
INSERT INTO `sensitive_word_data` VALUES ('娶韩国');
INSERT INTO `sensitive_word_data` VALUES ('婊子 ');
INSERT INTO `sensitive_word_data` VALUES ('婊子养的');
INSERT INTO `sensitive_word_data` VALUES ('婴儿命');
INSERT INTO `sensitive_word_data` VALUES ('媒体封锁');
INSERT INTO `sensitive_word_data` VALUES ('媚力');
INSERT INTO `sensitive_word_data` VALUES ('媚功');
INSERT INTO `sensitive_word_data` VALUES ('媚唇');
INSERT INTO `sensitive_word_data` VALUES ('媚外');
INSERT INTO `sensitive_word_data` VALUES ('媚娘');
INSERT INTO `sensitive_word_data` VALUES ('媚态');
INSERT INTO `sensitive_word_data` VALUES ('媚术');
INSERT INTO `sensitive_word_data` VALUES ('媚液');
INSERT INTO `sensitive_word_data` VALUES ('媚笑');
INSERT INTO `sensitive_word_data` VALUES ('媚肉');
INSERT INTO `sensitive_word_data` VALUES ('媚艳');
INSERT INTO `sensitive_word_data` VALUES ('媽個');
INSERT INTO `sensitive_word_data` VALUES ('媽媽的=>他*的');
INSERT INTO `sensitive_word_data` VALUES ('嫖俄罗');
INSERT INTO `sensitive_word_data` VALUES ('嫖妓');
INSERT INTO `sensitive_word_data` VALUES ('嫖娼');
INSERT INTO `sensitive_word_data` VALUES ('嫖客');
INSERT INTO `sensitive_word_data` VALUES ('嫖雞');
INSERT INTO `sensitive_word_data` VALUES ('嫖鸡');
INSERT INTO `sensitive_word_data` VALUES ('嫩乳');
INSERT INTO `sensitive_word_data` VALUES ('嫩娃');
INSERT INTO `sensitive_word_data` VALUES ('嫩嫩');
INSERT INTO `sensitive_word_data` VALUES ('嫩爽');
INSERT INTO `sensitive_word_data` VALUES ('嫩白');
INSERT INTO `sensitive_word_data` VALUES ('嫩穴');
INSERT INTO `sensitive_word_data` VALUES ('嫩红');
INSERT INTO `sensitive_word_data` VALUES ('嫩肉');
INSERT INTO `sensitive_word_data` VALUES ('嫩脸');
INSERT INTO `sensitive_word_data` VALUES ('嫩腿');
INSERT INTO `sensitive_word_data` VALUES ('嫩臀');
INSERT INTO `sensitive_word_data` VALUES ('嫩舌');
INSERT INTO `sensitive_word_data` VALUES ('嫩逼');
INSERT INTO `sensitive_word_data` VALUES ('嫩阴');
INSERT INTO `sensitive_word_data` VALUES ('子女任职名单');
INSERT INTO `sensitive_word_data` VALUES ('子孙袋子');
INSERT INTO `sensitive_word_data` VALUES ('子宫');
INSERT INTO `sensitive_word_data` VALUES ('子宫下段');
INSERT INTO `sensitive_word_data` VALUES ('子宫下段剖宫产');
INSERT INTO `sensitive_word_data` VALUES ('子宫体');
INSERT INTO `sensitive_word_data` VALUES ('子宫内膜');
INSERT INTO `sensitive_word_data` VALUES ('子宫内膜异位');
INSERT INTO `sensitive_word_data` VALUES ('子宫内膜液');
INSERT INTO `sensitive_word_data` VALUES ('子宫内膜炎');
INSERT INTO `sensitive_word_data` VALUES ('子宫内膜癌');
INSERT INTO `sensitive_word_data` VALUES ('子宫切除手术');
INSERT INTO `sensitive_word_data` VALUES ('子宫切除术');
INSERT INTO `sensitive_word_data` VALUES ('子宫后倾');
INSERT INTO `sensitive_word_data` VALUES ('子宫后屈位');
INSERT INTO `sensitive_word_data` VALUES ('子宫圆韧带');
INSERT INTO `sensitive_word_data` VALUES ('子宫壁');
INSERT INTO `sensitive_word_data` VALUES ('子宫峡部');
INSERT INTO `sensitive_word_data` VALUES ('子宫平滑肌');
INSERT INTO `sensitive_word_data` VALUES ('子宫底');
INSERT INTO `sensitive_word_data` VALUES ('子宫恶性肉瘤');
INSERT INTO `sensitive_word_data` VALUES ('子宫畸形');
INSERT INTO `sensitive_word_data` VALUES ('子宫病变');
INSERT INTO `sensitive_word_data` VALUES ('子宫粘膜');
INSERT INTO `sensitive_word_data` VALUES ('子宫肌瘤');
INSERT INTO `sensitive_word_data` VALUES ('子宫脱垂');
INSERT INTO `sensitive_word_data` VALUES ('子宫腔');
INSERT INTO `sensitive_word_data` VALUES ('子宫腺');
INSERT INTO `sensitive_word_data` VALUES ('子宫膀胱皱襞');
INSERT INTO `sensitive_word_data` VALUES ('子宫角');
INSERT INTO `sensitive_word_data` VALUES ('子宫阔韧带');
INSERT INTO `sensitive_word_data` VALUES ('子宫颈');
INSERT INTO `sensitive_word_data` VALUES ('子宫颈内D松弛');
INSERT INTO `sensitive_word_data` VALUES ('子宫颈炎');
INSERT INTO `sensitive_word_data` VALUES ('子宫颈癌');
INSERT INTO `sensitive_word_data` VALUES ('子宫颈管内膜柱状');
INSERT INTO `sensitive_word_data` VALUES ('子宫颈粘膜');
INSERT INTO `sensitive_word_data` VALUES ('子宫颈阴道部');
INSERT INTO `sensitive_word_data` VALUES ('子宫骶骨韧带');
INSERT INTO `sensitive_word_data` VALUES ('子弹货到付款');
INSERT INTO `sensitive_word_data` VALUES ('孔小均');
INSERT INTO `sensitive_word_data` VALUES ('孔摄像');
INSERT INTO `sensitive_word_data` VALUES ('孕卵');
INSERT INTO `sensitive_word_data` VALUES ('孕激素');
INSERT INTO `sensitive_word_data` VALUES ('孕酮');
INSERT INTO `sensitive_word_data` VALUES ('字牌汽');
INSERT INTO `sensitive_word_data` VALUES ('孙优贤');
INSERT INTO `sensitive_word_data` VALUES ('孙家正');
INSERT INTO `sensitive_word_data` VALUES ('孙悟空和雅典娜的故事');
INSERT INTO `sensitive_word_data` VALUES ('孙文盛');
INSERT INTO `sensitive_word_data` VALUES ('孙晓群');
INSERT INTO `sensitive_word_data` VALUES ('孙淑义');
INSERT INTO `sensitive_word_data` VALUES ('孙淦');
INSERT INTO `sensitive_word_data` VALUES ('孙英');
INSERT INTO `sensitive_word_data` VALUES ('孙连桂');
INSERT INTO `sensitive_word_data` VALUES ('孙金龙');
INSERT INTO `sensitive_word_data` VALUES ('孟令伟');
INSERT INTO `sensitive_word_data` VALUES ('孟建柱');
INSERT INTO `sensitive_word_data` VALUES ('季允石');
INSERT INTO `sensitive_word_data` VALUES ('学习班');
INSERT INTO `sensitive_word_data` VALUES ('学位證');
INSERT INTO `sensitive_word_data` VALUES ('学位证 刻章办证');
INSERT INTO `sensitive_word_data` VALUES ('学位证 办证电话');
INSERT INTO `sensitive_word_data` VALUES ('学姐和我的故事');
INSERT INTO `sensitive_word_data` VALUES ('学潮');
INSERT INTO `sensitive_word_data` VALUES ('学潮事件');
INSERT INTO `sensitive_word_data` VALUES ('学生与警察');
INSERT INTO `sensitive_word_data` VALUES ('学生信仰');
INSERT INTO `sensitive_word_data` VALUES ('学生妹');
INSERT INTO `sensitive_word_data` VALUES ('学生暴动');
INSERT INTO `sensitive_word_data` VALUES ('学生爱国运动正名');
INSERT INTO `sensitive_word_data` VALUES ('学生运动');
INSERT INTO `sensitive_word_data` VALUES ('学生静坐');
INSERT INTO `sensitive_word_data` VALUES ('学联');
INSERT INTO `sensitive_word_data` VALUES ('学自联');
INSERT INTO `sensitive_word_data` VALUES ('学运');
INSERT INTO `sensitive_word_data` VALUES ('学院+暴动');
INSERT INTO `sensitive_word_data` VALUES ('学院暴动');
INSERT INTO `sensitive_word_data` VALUES ('学骚乱');
INSERT INTO `sensitive_word_data` VALUES ('學生妹');
INSERT INTO `sensitive_word_data` VALUES ('宁夏办证/银川办证');
INSERT INTO `sensitive_word_data` VALUES ('它妈的');
INSERT INTO `sensitive_word_data` VALUES ('宇宙主佛 ');
INSERT INTO `sensitive_word_data` VALUES ('宇宙大法 ');
INSERT INTO `sensitive_word_data` VALUES ('宇宙最高法理');
INSERT INTO `sensitive_word_data` VALUES ('宇宙毁灭 ');
INSERT INTO `sensitive_word_data` VALUES ('宇宙真理');
INSERT INTO `sensitive_word_data` VALUES ('宇明网');
INSERT INTO `sensitive_word_data` VALUES ('守所死法');
INSERT INTO `sensitive_word_data` VALUES ('安全期避孕法');
INSERT INTO `sensitive_word_data` VALUES ('安启元');
INSERT INTO `sensitive_word_data` VALUES ('安定片');
INSERT INTO `sensitive_word_data` VALUES ('安局办公楼');
INSERT INTO `sensitive_word_data` VALUES ('安局豪华');
INSERT INTO `sensitive_word_data` VALUES ('安抚');
INSERT INTO `sensitive_word_data` VALUES ('安眠药');
INSERT INTO `sensitive_word_data` VALUES ('安眠藥');
INSERT INTO `sensitive_word_data` VALUES ('安眠酮');
INSERT INTO `sensitive_word_data` VALUES ('安立敏');
INSERT INTO `sensitive_word_data` VALUES ('安街逆');
INSERT INTO `sensitive_word_data` VALUES ('安门事');
INSERT INTO `sensitive_word_data` VALUES ('安非他命');
INSERT INTO `sensitive_word_data` VALUES ('安魂网');
INSERT INTO `sensitive_word_data` VALUES ('宋书元');
INSERT INTO `sensitive_word_data` VALUES ('宋任穷');
INSERT INTO `sensitive_word_data` VALUES ('宋宝瑞');
INSERT INTO `sensitive_word_data` VALUES ('宋平');
INSERT INTO `sensitive_word_data` VALUES ('宋平順');
INSERT INTO `sensitive_word_data` VALUES ('宋平顺');
INSERT INTO `sensitive_word_data` VALUES ('宋瑞祥');
INSERT INTO `sensitive_word_data` VALUES ('宋祖英');
INSERT INTO `sensitive_word_data` VALUES ('宋秀岩');
INSERT INTO `sensitive_word_data` VALUES ('宋金升');
INSERT INTO `sensitive_word_data` VALUES ('完全自杀手册');
INSERT INTO `sensitive_word_data` VALUES ('宏志');
INSERT INTO `sensitive_word_data` VALUES ('宏恩');
INSERT INTO `sensitive_word_data` VALUES ('宏治');
INSERT INTO `sensitive_word_data` VALUES ('宏法');
INSERT INTO `sensitive_word_data` VALUES ('宗凤鸣');
INSERT INTO `sensitive_word_data` VALUES ('宗教压迫');
INSERT INTO `sensitive_word_data` VALUES ('宗顺留');
INSERT INTO `sensitive_word_data` VALUES ('官也不容');
INSERT INTO `sensitive_word_data` VALUES ('官员成为政治新星');
INSERT INTO `sensitive_word_data` VALUES ('官商勾');
INSERT INTO `sensitive_word_data` VALUES ('官商勾结');
INSERT INTO `sensitive_word_data` VALUES ('官因发帖');
INSERT INTO `sensitive_word_data` VALUES ('官场日志');
INSERT INTO `sensitive_word_data` VALUES ('官场段子');
INSERT INTO `sensitive_word_data` VALUES ('官场潜规则');
INSERT INTO `sensitive_word_data` VALUES ('官场顺口溜');
INSERT INTO `sensitive_word_data` VALUES ('官逼民反');
INSERT INTO `sensitive_word_data` VALUES ('宙最高法');
INSERT INTO `sensitive_word_data` VALUES ('定位器');
INSERT INTO `sensitive_word_data` VALUES ('定情粉');
INSERT INTO `sensitive_word_data` VALUES ('定情药');
INSERT INTO `sensitive_word_data` VALUES ('宜昌当阳县级市长');
INSERT INTO `sensitive_word_data` VALUES ('宝在甘肃修');
INSERT INTO `sensitive_word_data` VALUES ('宝贝');
INSERT INTO `sensitive_word_data` VALUES ('实体娃');
INSERT INTO `sensitive_word_data` VALUES ('实学历文');
INSERT INTO `sensitive_word_data` VALUES ('实毕业证');
INSERT INTO `sensitive_word_data` VALUES ('实际神');
INSERT INTO `sensitive_word_data` VALUES ('审查参赛者');
INSERT INTO `sensitive_word_data` VALUES ('宫内膜炎');
INSERT INTO `sensitive_word_data` VALUES ('宫内避孕器');
INSERT INTO `sensitive_word_data` VALUES ('宫口');
INSERT INTO `sensitive_word_data` VALUES ('宫外孕');
INSERT INTO `sensitive_word_data` VALUES ('宫旁组织');
INSERT INTO `sensitive_word_data` VALUES ('宫腔');
INSERT INTO `sensitive_word_data` VALUES ('宫腔粘连');
INSERT INTO `sensitive_word_data` VALUES ('宫颈');
INSERT INTO `sensitive_word_data` VALUES ('宫颈外口');
INSERT INTO `sensitive_word_data` VALUES ('宫颈息肉');
INSERT INTO `sensitive_word_data` VALUES ('宫颈扩张');
INSERT INTO `sensitive_word_data` VALUES ('宫颈炎');
INSERT INTO `sensitive_word_data` VALUES ('宫颈癌');
INSERT INTO `sensitive_word_data` VALUES ('宫颈管内膜刮取术');
INSERT INTO `sensitive_word_data` VALUES ('宫颈管型癌症');
INSERT INTO `sensitive_word_data` VALUES ('宫颈粘液');
INSERT INTO `sensitive_word_data` VALUES ('宫颈粘液观察法');
INSERT INTO `sensitive_word_data` VALUES ('宫颈糜烂');
INSERT INTO `sensitive_word_data` VALUES ('宫颈肿瘤');
INSERT INTO `sensitive_word_data` VALUES ('宫颈腺癌');
INSERT INTO `sensitive_word_data` VALUES ('宫颈锥切术');
INSERT INTO `sensitive_word_data` VALUES ('宫颈阴道段');
INSERT INTO `sensitive_word_data` VALUES ('宫颈鳞状上皮');
INSERT INTO `sensitive_word_data` VALUES ('宮下杏奈BT下载');
INSERT INTO `sensitive_word_data` VALUES ('家le福');
INSERT INTO `sensitive_word_data` VALUES ('家l福');
INSERT INTO `sensitive_word_data` VALUES ('家一样饱');
INSERT INTO `sensitive_word_data` VALUES ('家乐福');
INSERT INTO `sensitive_word_data` VALUES ('家元自称玉皇大帝');
INSERT INTO `sensitive_word_data` VALUES ('家宝');
INSERT INTO `sensitive_word_data` VALUES ('家属被打');
INSERT INTO `sensitive_word_data` VALUES ('家用卫星');
INSERT INTO `sensitive_word_data` VALUES ('家用天线');
INSERT INTO `sensitive_word_data` VALUES ('容弹量 ');
INSERT INTO `sensitive_word_data` VALUES ('宾致网');
INSERT INTO `sensitive_word_data` VALUES ('宿命论');
INSERT INTO `sensitive_word_data` VALUES ('寂寞少妇');
INSERT INTO `sensitive_word_data` VALUES ('密{4}码{4}破{4}解');
INSERT INTO `sensitive_word_data` VALUES ('密合');
INSERT INTO `sensitive_word_data` VALUES ('密处');
INSERT INTO `sensitive_word_data` VALUES ('密汁');
INSERT INTO `sensitive_word_data` VALUES ('密洞');
INSERT INTO `sensitive_word_data` VALUES ('密穴');
INSERT INTO `sensitive_word_data` VALUES ('密窥');
INSERT INTO `sensitive_word_data` VALUES ('富人与农民工');
INSERT INTO `sensitive_word_data` VALUES ('富姐 ');
INSERT INTO `sensitive_word_data` VALUES ('富婆 ');
INSERT INTO `sensitive_word_data` VALUES ('富婆给废');
INSERT INTO `sensitive_word_data` VALUES ('富有');
INSERT INTO `sensitive_word_data` VALUES ('富民穷');
INSERT INTO `sensitive_word_data` VALUES ('察象蚂');
INSERT INTO `sensitive_word_data` VALUES ('对中共的姑息就是对死难者的残忍');
INSERT INTO `sensitive_word_data` VALUES ('对共产党清算');
INSERT INTO `sensitive_word_data` VALUES ('对华广播');
INSERT INTO `sensitive_word_data` VALUES ('对外高层人事');
INSERT INTO `sensitive_word_data` VALUES ('对方');
INSERT INTO `sensitive_word_data` VALUES ('对日强硬');
INSERT INTO `sensitive_word_data` VALUES ('寻找林昭的灵魂');
INSERT INTO `sensitive_word_data` VALUES ('导人最');
INSERT INTO `sensitive_word_data` VALUES ('导人的最');
INSERT INTO `sensitive_word_data` VALUES ('导叫失');
INSERT INTO `sensitive_word_data` VALUES ('导小商');
INSERT INTO `sensitive_word_data` VALUES ('导演专访');
INSERT INTO `sensitive_word_data` VALUES ('导爆索 ');
INSERT INTO `sensitive_word_data` VALUES ('导的情人');
INSERT INTO `sensitive_word_data` VALUES ('导致');
INSERT INTO `sensitive_word_data` VALUES ('封从德');
INSERT INTO `sensitive_word_data` VALUES ('封杀');
INSERT INTO `sensitive_word_data` VALUES ('封锁消');
INSERT INTO `sensitive_word_data` VALUES ('射精');
INSERT INTO `sensitive_word_data` VALUES ('射网枪');
INSERT INTO `sensitive_word_data` VALUES ('射颜');
INSERT INTO `sensitive_word_data` VALUES ('将则民');
INSERT INTO `sensitive_word_data` VALUES ('将嘴套至');
INSERT INTO `sensitive_word_data` VALUES ('專制');
INSERT INTO `sensitive_word_data` VALUES ('專政');
INSERT INTO `sensitive_word_data` VALUES ('尉健行');
INSERT INTO `sensitive_word_data` VALUES ('尊具');
INSERT INTO `sensitive_word_data` VALUES ('尊爵粉');
INSERT INTO `sensitive_word_data` VALUES ('小{4}姐');
INSERT INTO `sensitive_word_data` VALUES ('小倉杏小倉杏全集');
INSERT INTO `sensitive_word_data` VALUES ('小参考');
INSERT INTO `sensitive_word_data` VALUES ('小口径 ');
INSERT INTO `sensitive_word_data` VALUES ('小口径秃鹰沙漠');
INSERT INTO `sensitive_word_data` VALUES ('小型弓弩专卖店');
INSERT INTO `sensitive_word_data` VALUES ('小姐服務');
INSERT INTO `sensitive_word_data` VALUES ('小山渉BT下载');
INSERT INTO `sensitive_word_data` VALUES ('小川明日香全集');
INSERT INTO `sensitive_word_data` VALUES ('小平讲英语');
INSERT INTO `sensitive_word_data` VALUES ('小弟弟');
INSERT INTO `sensitive_word_data` VALUES ('小来子');
INSERT INTO `sensitive_word_data` VALUES ('小松綾乃BT下载');
INSERT INTO `sensitive_word_data` VALUES ('小池亞彌迅雷下载');
INSERT INTO `sensitive_word_data` VALUES ('小泉彩种子');
INSERT INTO `sensitive_word_data` VALUES ('小泉恶搞');
INSERT INTO `sensitive_word_data` VALUES ('小泽圆');
INSERT INTO `sensitive_word_data` VALUES ('小泽玛莉亚');
INSERT INTO `sensitive_word_data` VALUES ('小活佛');
INSERT INTO `sensitive_word_data` VALUES ('小澤園');
INSERT INTO `sensitive_word_data` VALUES ('小澤美电驴下载');
INSERT INTO `sensitive_word_data` VALUES ('小猎人弓弩网');
INSERT INTO `sensitive_word_data` VALUES ('小电影 ');
INSERT INTO `sensitive_word_data` VALUES ('小穴');
INSERT INTO `sensitive_word_data` VALUES ('小穴六四');
INSERT INTO `sensitive_word_data` VALUES ('少儿不宜');
INSERT INTO `sensitive_word_data` VALUES ('少女俱乐部');
INSERT INTO `sensitive_word_data` VALUES ('少女换衣');
INSERT INTO `sensitive_word_data` VALUES ('少女迷情药专卖');
INSERT INTO `sensitive_word_data` VALUES ('少女迷情药苍蝇粉专卖');
INSERT INTO `sensitive_word_data` VALUES ('少女高潮');
INSERT INTO `sensitive_word_data` VALUES ('少妇');
INSERT INTO `sensitive_word_data` VALUES ('少妇白洁');
INSERT INTO `sensitive_word_data` VALUES ('少妇自拍 ');
INSERT INTO `sensitive_word_data` VALUES ('少年大宝全本');
INSERT INTO `sensitive_word_data` VALUES ('少年阿宾');
INSERT INTO `sensitive_word_data` VALUES ('少龙风流txt下载');
INSERT INTO `sensitive_word_data` VALUES ('少龙风流未删节版全文阅读');
INSERT INTO `sensitive_word_data` VALUES ('尖叫');
INSERT INTO `sensitive_word_data` VALUES ('尖挺');
INSERT INTO `sensitive_word_data` VALUES ('尖硬');
INSERT INTO `sensitive_word_data` VALUES ('尖锐湿疣');
INSERT INTO `sensitive_word_data` VALUES ('尚勇');
INSERT INTO `sensitive_word_data` VALUES ('尚福林');
INSERT INTO `sensitive_word_data` VALUES ('尤仁');
INSERT INTO `sensitive_word_data` VALUES ('就爱插');
INSERT INTO `sensitive_word_data` VALUES ('就要色');
INSERT INTO `sensitive_word_data` VALUES ('尸博');
INSERT INTO `sensitive_word_data` VALUES ('尹庆民');
INSERT INTO `sensitive_word_data` VALUES ('尹方明');
INSERT INTO `sensitive_word_data` VALUES ('尼可刹米');
INSERT INTO `sensitive_word_data` VALUES ('尼总务石器吸其目哦轻易');
INSERT INTO `sensitive_word_data` VALUES ('尼泊尔军刀 卖');
INSERT INTO `sensitive_word_data` VALUES ('尼泊尔军刀出售');
INSERT INTO `sensitive_word_data` VALUES ('尽根');
INSERT INTO `sensitive_word_data` VALUES ('尾行');
INSERT INTO `sensitive_word_data` VALUES ('尿后流白');
INSERT INTO `sensitive_word_data` VALUES ('尿味');
INSERT INTO `sensitive_word_data` VALUES ('尿囊膜');
INSERT INTO `sensitive_word_data` VALUES ('尿意');
INSERT INTO `sensitive_word_data` VALUES ('尿末滴白');
INSERT INTO `sensitive_word_data` VALUES ('尿毒');
INSERT INTO `sensitive_word_data` VALUES ('尿毒症');
INSERT INTO `sensitive_word_data` VALUES ('尿水');
INSERT INTO `sensitive_word_data` VALUES ('尿浊');
INSERT INTO `sensitive_word_data` VALUES ('尿液');
INSERT INTO `sensitive_word_data` VALUES ('尿生殖膈');
INSERT INTO `sensitive_word_data` VALUES ('尿生殖膈上盘膜');
INSERT INTO `sensitive_word_data` VALUES ('尿生殖隔');
INSERT INTO `sensitive_word_data` VALUES ('尿生膈下筋膜');
INSERT INTO `sensitive_word_data` VALUES ('尿痛');
INSERT INTO `sensitive_word_data` VALUES ('尿胆素');
INSERT INTO `sensitive_word_data` VALUES ('尿血');
INSERT INTO `sensitive_word_data` VALUES ('尿路感染');
INSERT INTO `sensitive_word_data` VALUES ('尿路结石');
INSERT INTO `sensitive_word_data` VALUES ('尿道');
INSERT INTO `sensitive_word_data` VALUES ('尿道上裂');
INSERT INTO `sensitive_word_data` VALUES ('尿道下裂');
INSERT INTO `sensitive_word_data` VALUES ('尿道口');
INSERT INTO `sensitive_word_data` VALUES ('尿道外口');
INSERT INTO `sensitive_word_data` VALUES ('尿道外括约肌');
INSERT INTO `sensitive_word_data` VALUES ('尿道嵴');
INSERT INTO `sensitive_word_data` VALUES ('尿道憩室');
INSERT INTO `sensitive_word_data` VALUES ('尿道括约肌');
INSERT INTO `sensitive_word_data` VALUES ('尿道旁腺');
INSERT INTO `sensitive_word_data` VALUES ('尿道旁腺炎');
INSERT INTO `sensitive_word_data` VALUES ('尿道海绵体');
INSERT INTO `sensitive_word_data` VALUES ('尿道炎');
INSERT INTO `sensitive_word_data` VALUES ('尿道狭窄');
INSERT INTO `sensitive_word_data` VALUES ('尿道球腺');
INSERT INTO `sensitive_word_data` VALUES ('尿道球腺炎');
INSERT INTO `sensitive_word_data` VALUES ('尿道球部');
INSERT INTO `sensitive_word_data` VALUES ('尿道结石');
INSERT INTO `sensitive_word_data` VALUES ('尿道肉阜');
INSERT INTO `sensitive_word_data` VALUES ('尿道腺');
INSERT INTO `sensitive_word_data` VALUES ('尿道腺液');
INSERT INTO `sensitive_word_data` VALUES ('尿道膨出');
INSERT INTO `sensitive_word_data` VALUES ('尿道附属腺体');
INSERT INTO `sensitive_word_data` VALUES ('尿频');
INSERT INTO `sensitive_word_data` VALUES ('屁 股');
INSERT INTO `sensitive_word_data` VALUES ('屁眼 ');
INSERT INTO `sensitive_word_data` VALUES ('屁穴');
INSERT INTO `sensitive_word_data` VALUES ('屁股');
INSERT INTO `sensitive_word_data` VALUES ('屁道');
INSERT INTO `sensitive_word_data` VALUES ('屁门');
INSERT INTO `sensitive_word_data` VALUES ('屄');
INSERT INTO `sensitive_word_data` VALUES ('屄缝');
INSERT INTO `sensitive_word_data` VALUES ('屌');
INSERT INTO `sensitive_word_data` VALUES ('展露');
INSERT INTO `sensitive_word_data` VALUES ('屙民');
INSERT INTO `sensitive_word_data` VALUES ('属灵教 ');
INSERT INTO `sensitive_word_data` VALUES ('屠光绍');
INSERT INTO `sensitive_word_data` VALUES ('屠杀');
INSERT INTO `sensitive_word_data` VALUES ('屠龙别记');
INSERT INTO `sensitive_word_data` VALUES ('山不过来');
INSERT INTO `sensitive_word_data` VALUES ('山东电警棍专卖');
INSERT INTO `sensitive_word_data` VALUES ('山寨手机');
INSERT INTO `sensitive_word_data` VALUES ('山崎亜彌迅雷下载');
INSERT INTO `sensitive_word_data` VALUES ('山涉黑');
INSERT INTO `sensitive_word_data` VALUES ('山西删贴公司');
INSERT INTO `sensitive_word_data` VALUES ('山西洪洞');
INSERT INTO `sensitive_word_data` VALUES ('山西负面信息处理');
INSERT INTO `sensitive_word_data` VALUES ('山西黑砖窑');
INSERT INTO `sensitive_word_data` VALUES ('岡崎愛电驴下载');
INSERT INTO `sensitive_word_data` VALUES ('岡本真');
INSERT INTO `sensitive_word_data` VALUES ('岩本亞由美电驴下载');
INSERT INTO `sensitive_word_data` VALUES ('岳武');
INSERT INTO `sensitive_word_data` VALUES ('岳母');
INSERT INTO `sensitive_word_data` VALUES ('岳父');
INSERT INTO `sensitive_word_data` VALUES ('島津千秋电驴下载');
INSERT INTO `sensitive_word_data` VALUES ('島谷愛梨BT下载');
INSERT INTO `sensitive_word_data` VALUES ('崔会烈');
INSERT INTO `sensitive_word_data` VALUES ('崔英杰');
INSERT INTO `sensitive_word_data` VALUES ('嵌顿包茎');
INSERT INTO `sensitive_word_data` VALUES ('嵌顿性包茎');
INSERT INTO `sensitive_word_data` VALUES ('川b26931');
INSERT INTO `sensitive_word_data` VALUES ('川島和津實');
INSERT INTO `sensitive_word_data` VALUES ('川島和津實种子');
INSERT INTO `sensitive_word_data` VALUES ('川村亞紀电驴下载');
INSERT INTO `sensitive_word_data` VALUES ('川村藍子迅雷下载');
INSERT INTO `sensitive_word_data` VALUES ('州三箭');
INSERT INTO `sensitive_word_data` VALUES ('州大批贪');
INSERT INTO `sensitive_word_data` VALUES ('州惨案');
INSERT INTO `sensitive_word_data` VALUES ('工业炸药配方设计');
INSERT INTO `sensitive_word_data` VALUES ('工产党');
INSERT INTO `sensitive_word_data` VALUES ('工力');
INSERT INTO `sensitive_word_data` VALUES ('工力人');
INSERT INTO `sensitive_word_data` VALUES ('工商代理');
INSERT INTO `sensitive_word_data` VALUES ('工商代理QQ');
INSERT INTO `sensitive_word_data` VALUES ('工商税务两条狼');
INSERT INTO `sensitive_word_data` VALUES ('工商银行用户登陆');
INSERT INTO `sensitive_word_data` VALUES ('工字汽狗麻醉枪');
INSERT INTO `sensitive_word_data` VALUES ('工字牌气枪出售专卖');
INSERT INTO `sensitive_word_data` VALUES ('工字牌气枪指定销');
INSERT INTO `sensitive_word_data` VALUES ('工程吞得');
INSERT INTO `sensitive_word_data` VALUES ('工自联');
INSERT INTO `sensitive_word_data` VALUES ('工藤綾美电驴下载');
INSERT INTO `sensitive_word_data` VALUES ('左拥右抱');
INSERT INTO `sensitive_word_data` VALUES ('左棍');
INSERT INTO `sensitive_word_data` VALUES ('左转是政');
INSERT INTO `sensitive_word_data` VALUES ('巧春');
INSERT INTO `sensitive_word_data` VALUES ('巨乳');
INSERT INTO `sensitive_word_data` VALUES ('巨枪');
INSERT INTO `sensitive_word_data` VALUES ('巨根');
INSERT INTO `sensitive_word_data` VALUES ('巨棍');
INSERT INTO `sensitive_word_data` VALUES ('巨棒');
INSERT INTO `sensitive_word_data` VALUES ('巨炮');
INSERT INTO `sensitive_word_data` VALUES ('巨物');
INSERT INTO `sensitive_word_data` VALUES ('巨额骗储');
INSERT INTO `sensitive_word_data` VALUES ('巫毒娃娃');
INSERT INTO `sensitive_word_data` VALUES ('巴拉斯');
INSERT INTO `sensitive_word_data` VALUES ('巴氏腺');
INSERT INTO `sensitive_word_data` VALUES ('巴特尔');
INSERT INTO `sensitive_word_data` VALUES ('巴赫');
INSERT INTO `sensitive_word_data` VALUES ('巴金');
INSERT INTO `sensitive_word_data` VALUES ('巴音朝鲁');
INSERT INTO `sensitive_word_data` VALUES ('布卖淫女');
INSERT INTO `sensitive_word_data` VALUES ('师傅法身');
INSERT INTO `sensitive_word_data` VALUES ('师涛');
INSERT INTO `sensitive_word_data` VALUES ('师父');
INSERT INTO `sensitive_word_data` VALUES ('师父法身');
INSERT INTO `sensitive_word_data` VALUES ('希xi望wang之声');
INSERT INTO `sensitive_word_data` VALUES ('希望之声国际广播电台');
INSERT INTO `sensitive_word_data` VALUES ('希脏');
INSERT INTO `sensitive_word_data` VALUES ('帐篷就会脱销');
INSERT INTO `sensitive_word_data` VALUES ('帕巴拉·格列朗杰');
INSERT INTO `sensitive_word_data` VALUES ('帝国之梦');
INSERT INTO `sensitive_word_data` VALUES ('帝國之夢');
INSERT INTO `sensitive_word_data` VALUES ('带状沟');
INSERT INTO `sensitive_word_data` VALUES ('師母');
INSERT INTO `sensitive_word_data` VALUES ('席临终前');
INSERT INTO `sensitive_word_data` VALUES ('席复活');
INSERT INTO `sensitive_word_data` VALUES ('席指着护');
INSERT INTO `sensitive_word_data` VALUES ('帮人怀孕');
INSERT INTO `sensitive_word_data` VALUES ('常万全');
INSERT INTO `sensitive_word_data` VALUES ('常劲');
INSERT INTO `sensitive_word_data` VALUES ('常委');
INSERT INTO `sensitive_word_data` VALUES ('常委汪兆钧');
INSERT INTO `sensitive_word_data` VALUES ('常年出售假币');
INSERT INTO `sensitive_word_data` VALUES ('常盤優子迅雷下载');
INSERT INTO `sensitive_word_data` VALUES ('幕前戲');
INSERT INTO `sensitive_word_data` VALUES ('幕没有不');
INSERT INTO `sensitive_word_data` VALUES ('干 你 妈');
INSERT INTO `sensitive_word_data` VALUES ('干B');
INSERT INTO `sensitive_word_data` VALUES ('干他');
INSERT INTO `sensitive_word_data` VALUES ('干以胜');
INSERT INTO `sensitive_word_data` VALUES ('干你');
INSERT INTO `sensitive_word_data` VALUES ('干你妈');
INSERT INTO `sensitive_word_data` VALUES ('干你妈b');
INSERT INTO `sensitive_word_data` VALUES ('干你妈逼');
INSERT INTO `sensitive_word_data` VALUES ('干你娘');
INSERT INTO `sensitive_word_data` VALUES ('干她');
INSERT INTO `sensitive_word_data` VALUES ('干妳');
INSERT INTO `sensitive_word_data` VALUES ('干妳妈');
INSERT INTO `sensitive_word_data` VALUES ('干妳娘');
INSERT INTO `sensitive_word_data` VALUES ('干妳老母');
INSERT INTO `sensitive_word_data` VALUES ('干扰器 ');
INSERT INTO `sensitive_word_data` VALUES ('干扰赈灾募捐事件回放');
INSERT INTO `sensitive_word_data` VALUES ('干死');
INSERT INTO `sensitive_word_data` VALUES ('干死=[请文明用语]');
INSERT INTO `sensitive_word_data` VALUES ('干死你');
INSERT INTO `sensitive_word_data` VALUES ('干过炮');
INSERT INTO `sensitive_word_data` VALUES ('干逼');
INSERT INTO `sensitive_word_data` VALUES ('平反六四');
INSERT INTO `sensitive_word_data` VALUES ('平叫到床');
INSERT INTO `sensitive_word_data` VALUES ('平安夜自杀');
INSERT INTO `sensitive_word_data` VALUES ('平小邓');
INSERT INTO `sensitive_word_data` VALUES ('平山朝香迅雷下??');
INSERT INTO `sensitive_word_data` VALUES ('平惨案');
INSERT INTO `sensitive_word_data` VALUES ('平滑');
INSERT INTO `sensitive_word_data` VALUES ('平近习');
INSERT INTO `sensitive_word_data` VALUES ('并睾');
INSERT INTO `sensitive_word_data` VALUES ('幸运码');
INSERT INTO `sensitive_word_data` VALUES ('幼交 ');
INSERT INTO `sensitive_word_data` VALUES ('幼嫩');
INSERT INTO `sensitive_word_data` VALUES ('幼稚型子宫');
INSERT INTO `sensitive_word_data` VALUES ('幼齿类');
INSERT INTO `sensitive_word_data` VALUES ('幽户');
INSERT INTO `sensitive_word_data` VALUES ('幽洞');
INSERT INTO `sensitive_word_data` VALUES ('幽谷三');
INSERT INTO `sensitive_word_data` VALUES ('广东疮');
INSERT INTO `sensitive_word_data` VALUES ('庄公惠');
INSERT INTO `sensitive_word_data` VALUES ('床事');
INSERT INTO `sensitive_word_data` VALUES ('床戏');
INSERT INTO `sensitive_word_data` VALUES ('应召');
INSERT INTO `sensitive_word_data` VALUES ('应子弹');
INSERT INTO `sensitive_word_data` VALUES ('底裤');
INSERT INTO `sensitive_word_data` VALUES ('府包庇');
INSERT INTO `sensitive_word_data` VALUES ('府谷');
INSERT INTO `sensitive_word_data` VALUES ('府集中领');
INSERT INTO `sensitive_word_data` VALUES ('庞丽娟');
INSERT INTO `sensitive_word_data` VALUES ('庭保养');
INSERT INTO `sensitive_word_data` VALUES ('庭审直播');
INSERT INTO `sensitive_word_data` VALUES ('康没有不');
INSERT INTO `sensitive_word_data` VALUES ('康生丹');
INSERT INTO `sensitive_word_data` VALUES ('康跳楼');
INSERT INTO `sensitive_word_data` VALUES ('廖晖');
INSERT INTO `sensitive_word_data` VALUES ('廖锡龙');
INSERT INTO `sensitive_word_data` VALUES ('开发');
INSERT INTO `sensitive_word_data` VALUES ('开山刀砍刀出售');
INSERT INTO `sensitive_word_data` VALUES ('开苞 ');
INSERT INTO `sensitive_word_data` VALUES ('开邓选');
INSERT INTO `sensitive_word_data` VALUES ('开锁工具');
INSERT INTO `sensitive_word_data` VALUES ('弄');
INSERT INTO `sensitive_word_data` VALUES ('弄弄');
INSERT INTO `sensitive_word_data` VALUES ('弄湿');
INSERT INTO `sensitive_word_data` VALUES ('弄破');
INSERT INTO `sensitive_word_data` VALUES ('弄穴');
INSERT INTO `sensitive_word_data` VALUES ('式粉推');
INSERT INTO `sensitive_word_data` VALUES ('弓弩  ');
INSERT INTO `sensitive_word_data` VALUES ('弓弩一百专卖店');
INSERT INTO `sensitive_word_data` VALUES ('弓弩专卖');
INSERT INTO `sensitive_word_data` VALUES ('弓弩专卖店');
INSERT INTO `sensitive_word_data` VALUES ('弓弩专卖店网');
INSERT INTO `sensitive_word_data` VALUES ('弓弩专卖网');
INSERT INTO `sensitive_word_data` VALUES ('弓弩专卖网货到付款');
INSERT INTO `sensitive_word_data` VALUES ('弓弩专卖货到付款');
INSERT INTO `sensitive_word_data` VALUES ('弓弩专营');
INSERT INTO `sensitive_word_data` VALUES ('弓弩免定金货到付款');
INSERT INTO `sensitive_word_data` VALUES ('弓弩制作');
INSERT INTO `sensitive_word_data` VALUES ('弓弩商城');
INSERT INTO `sensitive_word_data` VALUES ('弓弩商城专卖');
INSERT INTO `sensitive_word_data` VALUES ('弓弩图纸');
INSERT INTO `sensitive_word_data` VALUES ('弓弩批发');
INSERT INTO `sensitive_word_data` VALUES ('弓弩有限公司');
INSERT INTO `sensitive_word_data` VALUES ('弓弩特许经销');
INSERT INTO `sensitive_word_data` VALUES ('弓弩狩猎网');
INSERT INTO `sensitive_word_data` VALUES ('弓弩直销');
INSERT INTO `sensitive_word_data` VALUES ('弓弩网 ');
INSERT INTO `sensitive_word_data` VALUES ('弓弩营销中心');
INSERT INTO `sensitive_word_data` VALUES ('弓弩论坛 ');
INSERT INTO `sensitive_word_data` VALUES ('弓弩销售');
INSERT INTO `sensitive_word_data` VALUES ('弓弩麻醉箭');
INSERT INTO `sensitive_word_data` VALUES ('弓弩麻醉镖');
INSERT INTO `sensitive_word_data` VALUES ('弟 弟');
INSERT INTO `sensitive_word_data` VALUES ('弟妇');
INSERT INTO `sensitive_word_data` VALUES ('弟弟');
INSERT INTO `sensitive_word_data` VALUES ('张中伟');
INSERT INTO `sensitive_word_data` VALUES ('张丹红');
INSERT INTO `sensitive_word_data` VALUES ('张云川');
INSERT INTO `sensitive_word_data` VALUES ('张京生');
INSERT INTO `sensitive_word_data` VALUES ('张佑才');
INSERT INTO `sensitive_word_data` VALUES ('张俊九');
INSERT INTO `sensitive_word_data` VALUES ('张先玲');
INSERT INTO `sensitive_word_data` VALUES ('张克辉');
INSERT INTO `sensitive_word_data` VALUES ('张博涵');
INSERT INTO `sensitive_word_data` VALUES ('张发强');
INSERT INTO `sensitive_word_data` VALUES ('张合');
INSERT INTO `sensitive_word_data` VALUES ('张吾乐');
INSERT INTO `sensitive_word_data` VALUES ('张国祥');
INSERT INTO `sensitive_word_data` VALUES ('张圣坤');
INSERT INTO `sensitive_word_data` VALUES ('张培莉');
INSERT INTO `sensitive_word_data` VALUES ('张大宁');
INSERT INTO `sensitive_word_data` VALUES ('张学东');
INSERT INTO `sensitive_word_data` VALUES ('张学忠');
INSERT INTO `sensitive_word_data` VALUES ('张宏伟');
INSERT INTO `sensitive_word_data` VALUES ('张宏堡 ');
INSERT INTO `sensitive_word_data` VALUES ('张宏宝 ');
INSERT INTO `sensitive_word_data` VALUES ('张定发');
INSERT INTO `sensitive_word_data` VALUES ('张宝文');
INSERT INTO `sensitive_word_data` VALUES ('张宝明');
INSERT INTO `sensitive_word_data` VALUES ('张宝顺');
INSERT INTO `sensitive_word_data` VALUES ('张小洋');
INSERT INTO `sensitive_word_data` VALUES ('张工');
INSERT INTO `sensitive_word_data` VALUES ('张左己');
INSERT INTO `sensitive_word_data` VALUES ('张帆');
INSERT INTO `sensitive_word_data` VALUES ('张平');
INSERT INTO `sensitive_word_data` VALUES ('张庆黎');
INSERT INTO `sensitive_word_data` VALUES ('张廷翰');
INSERT INTO `sensitive_word_data` VALUES ('张开了嘴');
INSERT INTO `sensitive_word_data` VALUES ('张开双唇');
INSERT INTO `sensitive_word_data` VALUES ('张开双腿');
INSERT INTO `sensitive_word_data` VALUES ('张开小嘴');
INSERT INTO `sensitive_word_data` VALUES ('张开樱唇');
INSERT INTO `sensitive_word_data` VALUES ('张开红唇');
INSERT INTO `sensitive_word_data` VALUES ('张德江 ');
INSERT INTO `sensitive_word_data` VALUES ('张德邻');
INSERT INTO `sensitive_word_data` VALUES ('张志国');
INSERT INTO `sensitive_word_data` VALUES ('张志坚');
INSERT INTO `sensitive_word_data` VALUES ('张志新');
INSERT INTO `sensitive_word_data` VALUES ('张怀西');
INSERT INTO `sensitive_word_data` VALUES ('张思卿');
INSERT INTO `sensitive_word_data` VALUES ('张承芬');
INSERT INTO `sensitive_word_data` VALUES ('张振刚');
INSERT INTO `sensitive_word_data` VALUES ('张文中');
INSERT INTO `sensitive_word_data` VALUES ('张文岳');
INSERT INTO `sensitive_word_data` VALUES ('张新时');
INSERT INTO `sensitive_word_data` VALUES ('张春桥');
INSERT INTO `sensitive_word_data` VALUES ('张春贤');
INSERT INTO `sensitive_word_data` VALUES ('张晓平');
INSERT INTO `sensitive_word_data` VALUES ('张柏林');
INSERT INTO `sensitive_word_data` VALUES ('张梅颖');
INSERT INTO `sensitive_word_data` VALUES ('张榕明');
INSERT INTO `sensitive_word_data` VALUES ('张毓茂');
INSERT INTO `sensitive_word_data` VALUES ('张永珍');
INSERT INTO `sensitive_word_data` VALUES ('张洽');
INSERT INTO `sensitive_word_data` VALUES ('张涛');
INSERT INTO `sensitive_word_data` VALUES ('张祖桦');
INSERT INTO `sensitive_word_data` VALUES ('张立昌 ');
INSERT INTO `sensitive_word_data` VALUES ('张继禹');
INSERT INTO `sensitive_word_data` VALUES ('张绪武');
INSERT INTO `sensitive_word_data` VALUES ('张维庆');
INSERT INTO `sensitive_word_data` VALUES ('张美兰');
INSERT INTO `sensitive_word_data` VALUES ('张耕');
INSERT INTO `sensitive_word_data` VALUES ('张肖');
INSERT INTO `sensitive_word_data` VALUES ('张芝庭');
INSERT INTO `sensitive_word_data` VALUES ('张荣坤');
INSERT INTO `sensitive_word_data` VALUES ('张高丽');
INSERT INTO `sensitive_word_data` VALUES ('张龙俊');
INSERT INTO `sensitive_word_data` VALUES ('弹夹 ');
INSERT INTO `sensitive_word_data` VALUES ('弹种 ');
INSERT INTO `sensitive_word_data` VALUES ('弹药制造方法');
INSERT INTO `sensitive_word_data` VALUES ('强健');
INSERT INTO `sensitive_word_data` VALUES ('强卫');
INSERT INTO `sensitive_word_data` VALUES ('强奸');
INSERT INTO `sensitive_word_data` VALUES ('强效失忆药 ');
INSERT INTO `sensitive_word_data` VALUES ('强暴');
INSERT INTO `sensitive_word_data` VALUES ('强权政府');
INSERT INTO `sensitive_word_data` VALUES ('强硬');
INSERT INTO `sensitive_word_data` VALUES ('强硬发言');
INSERT INTO `sensitive_word_data` VALUES ('强精');
INSERT INTO `sensitive_word_data` VALUES ('当代七整');
INSERT INTO `sensitive_word_data` VALUES ('当官在于');
INSERT INTO `sensitive_word_data` VALUES ('当官要精');
INSERT INTO `sensitive_word_data` VALUES ('当官靠后台');
INSERT INTO `sensitive_word_data` VALUES ('形透视镜');
INSERT INTO `sensitive_word_data` VALUES ('彩宝');
INSERT INTO `sensitive_word_data` VALUES ('彩票 ');
INSERT INTO `sensitive_word_data` VALUES ('彩票选号机 ');
INSERT INTO `sensitive_word_data` VALUES ('彩票预测 ');
INSERT INTO `sensitive_word_data` VALUES ('彭小枫');
INSERT INTO `sensitive_word_data` VALUES ('彭钊');
INSERT INTO `sensitive_word_data` VALUES ('征鹏');
INSERT INTO `sensitive_word_data` VALUES ('径步枪');
INSERT INTO `sensitive_word_data` VALUES ('徐光春');
INSERT INTO `sensitive_word_data` VALUES ('徐冠华');
INSERT INTO `sensitive_word_data` VALUES ('徐匡迪');
INSERT INTO `sensitive_word_data` VALUES ('徐展堂');
INSERT INTO `sensitive_word_data` VALUES ('徐志纯');
INSERT INTO `sensitive_word_data` VALUES ('徐才厚 ');
INSERT INTO `sensitive_word_data` VALUES ('徐明');
INSERT INTO `sensitive_word_data` VALUES ('徐更生');
INSERT INTO `sensitive_word_data` VALUES ('徐永清');
INSERT INTO `sensitive_word_data` VALUES ('徐玉元');
INSERT INTO `sensitive_word_data` VALUES ('徐绍史');
INSERT INTO `sensitive_word_data` VALUES ('徐自强');
INSERT INTO `sensitive_word_data` VALUES ('徐至展');
INSERT INTO `sensitive_word_data` VALUES ('徐荣凯');
INSERT INTO `sensitive_word_data` VALUES ('徐金芝');
INSERT INTO `sensitive_word_data` VALUES ('徐鸿道');
INSERT INTO `sensitive_word_data` VALUES ('徐麟祥');
INSERT INTO `sensitive_word_data` VALUES ('得到');
INSERT INTO `sensitive_word_data` VALUES ('得财兼');
INSERT INTO `sensitive_word_data` VALUES ('微型摄像');
INSERT INTO `sensitive_word_data` VALUES ('德哇仓');
INSERT INTO `sensitive_word_data` VALUES ('志不愿跟');
INSERT INTO `sensitive_word_data` VALUES ('志洪李');
INSERT INTO `sensitive_word_data` VALUES ('忙爱国');
INSERT INTO `sensitive_word_data` VALUES ('快 感');
INSERT INTO `sensitive_word_data` VALUES ('快乐');
INSERT INTO `sensitive_word_data` VALUES ('快意');
INSERT INTO `sensitive_word_data` VALUES ('快感');
INSERT INTO `sensitive_word_data` VALUES ('快活');
INSERT INTO `sensitive_word_data` VALUES ('快速办');
INSERT INTO `sensitive_word_data` VALUES ('怎么用土办法做武器');
INSERT INTO `sensitive_word_data` VALUES ('怎么自制燃烧弹');
INSERT INTO `sensitive_word_data` VALUES ('怒张');
INSERT INTO `sensitive_word_data` VALUES ('怒涨');
INSERT INTO `sensitive_word_data` VALUES ('怒的志愿');
INSERT INTO `sensitive_word_data` VALUES ('怒胀');
INSERT INTO `sensitive_word_data` VALUES ('急喘');
INSERT INTO `sensitive_word_data` VALUES ('急性外阴炎');
INSERT INTO `sensitive_word_data` VALUES ('急性女阴溃疡');
INSERT INTO `sensitive_word_data` VALUES ('急性输卵管炎');
INSERT INTO `sensitive_word_data` VALUES ('急抽');
INSERT INTO `sensitive_word_data` VALUES ('急需嫖');
INSERT INTO `sensitive_word_data` VALUES ('性交 ');
INSERT INTO `sensitive_word_data` VALUES ('性交大赛');
INSERT INTO `sensitive_word_data` VALUES ('性伙伴');
INSERT INTO `sensitive_word_data` VALUES ('性伴侣 ');
INSERT INTO `sensitive_word_data` VALUES ('性奴 ');
INSERT INTO `sensitive_word_data` VALUES ('性奴集中营');
INSERT INTO `sensitive_word_data` VALUES ('性息 ');
INSERT INTO `sensitive_word_data` VALUES ('性感少');
INSERT INTO `sensitive_word_data` VALUES ('性推广歌');
INSERT INTO `sensitive_word_data` VALUES ('性服务 ');
INSERT INTO `sensitive_word_data` VALUES ('性爱');
INSERT INTO `sensitive_word_data` VALUES ('性爱日');
INSERT INTO `sensitive_word_data` VALUES ('性爱派对');
INSERT INTO `sensitive_word_data` VALUES ('性福情');
INSERT INTO `sensitive_word_data` VALUES ('性虐 ');
INSERT INTO `sensitive_word_data` VALUES ('总书记');
INSERT INTO `sensitive_word_data` VALUES ('总会美女');
INSERT INTO `sensitive_word_data` VALUES ('恋人');
INSERT INTO `sensitive_word_data` VALUES ('恋母');
INSERT INTO `sensitive_word_data` VALUES ('恋童');
INSERT INTO `sensitive_word_data` VALUES ('恐怖分子傻瓜手册');
INSERT INTO `sensitive_word_data` VALUES ('恐怖分子自己制造炸弹');
INSERT INTO `sensitive_word_data` VALUES ('恙虫病');
INSERT INTO `sensitive_word_data` VALUES ('恩氟烷');
INSERT INTO `sensitive_word_data` VALUES ('恶党');
INSERT INTO `sensitive_word_data` VALUES ('恶势力插');
INSERT INTO `sensitive_word_data` VALUES ('恶势力操');
INSERT INTO `sensitive_word_data` VALUES ('恶露');
INSERT INTO `sensitive_word_data` VALUES ('恽小华');
INSERT INTO `sensitive_word_data` VALUES ('悸动');
INSERT INTO `sensitive_word_data` VALUES ('情妹妹');
INSERT INTO `sensitive_word_data` VALUES ('情杀 ');
INSERT INTO `sensitive_word_data` VALUES ('情欲结');
INSERT INTO `sensitive_word_data` VALUES ('情聊天室');
INSERT INTO `sensitive_word_data` VALUES ('情自拍');
INSERT INTO `sensitive_word_data` VALUES ('情视频');
INSERT INTO `sensitive_word_data` VALUES ('惊暴双乳');
INSERT INTO `sensitive_word_data` VALUES ('惨奥');
INSERT INTO `sensitive_word_data` VALUES ('惩公安');
INSERT INTO `sensitive_word_data` VALUES ('惩贪难');
INSERT INTO `sensitive_word_data` VALUES ('惹的国');
INSERT INTO `sensitive_word_data` VALUES ('愈插愈快');
INSERT INTO `sensitive_word_data` VALUES ('愚民同');
INSERT INTO `sensitive_word_data` VALUES ('愚民政');
INSERT INTO `sensitive_word_data` VALUES ('感扑克');
INSERT INTO `sensitive_word_data` VALUES ('慈悲功 ');
INSERT INTO `sensitive_word_data` VALUES ('慈母');
INSERT INTO `sensitive_word_data` VALUES ('慕男症');
INSERT INTO `sensitive_word_data` VALUES ('慢性输卵管炎');
INSERT INTO `sensitive_word_data` VALUES ('慧网');
INSERT INTO `sensitive_word_data` VALUES ('慧聪 ');
INSERT INTO `sensitive_word_data` VALUES ('懊孕');
INSERT INTO `sensitive_word_data` VALUES ('懊运');
INSERT INTO `sensitive_word_data` VALUES ('戈博军刀');
INSERT INTO `sensitive_word_data` VALUES ('成人');
INSERT INTO `sensitive_word_data` VALUES ('成人卡通');
INSERT INTO `sensitive_word_data` VALUES ('成人图');
INSERT INTO `sensitive_word_data` VALUES ('成人小');
INSERT INTO `sensitive_word_data` VALUES ('成人小说 ');
INSERT INTO `sensitive_word_data` VALUES ('成人文');
INSERT INTO `sensitive_word_data` VALUES ('成人片');
INSERT INTO `sensitive_word_data` VALUES ('成人电');
INSERT INTO `sensitive_word_data` VALUES ('成人聊');
INSERT INTO `sensitive_word_data` VALUES ('成人视');
INSERT INTO `sensitive_word_data` VALUES ('成佛做主 ');
INSERT INTO `sensitive_word_data` VALUES ('成杰');
INSERT INTO `sensitive_word_data` VALUES ('我干');
INSERT INTO `sensitive_word_data` VALUES ('我搞台独');
INSERT INTO `sensitive_word_data` VALUES ('我操');
INSERT INTO `sensitive_word_data` VALUES ('我日你');
INSERT INTO `sensitive_word_data` VALUES ('我的西域');
INSERT INTO `sensitive_word_data` VALUES ('我草');
INSERT INTO `sensitive_word_data` VALUES ('战术军刀出售');
INSERT INTO `sensitive_word_data` VALUES ('戳入');
INSERT INTO `sensitive_word_data` VALUES ('戳穴');
INSERT INTO `sensitive_word_data` VALUES ('戳穿');
INSERT INTO `sensitive_word_data` VALUES ('戴海静');
INSERT INTO `sensitive_word_data` VALUES ('戴相龙');
INSERT INTO `sensitive_word_data` VALUES ('戴秉国');
INSERT INTO `sensitive_word_data` VALUES ('戴证良');
INSERT INTO `sensitive_word_data` VALUES ('户外军刀出售');
INSERT INTO `sensitive_word_data` VALUES ('户外军刀网');
INSERT INTO `sensitive_word_data` VALUES ('房中');
INSERT INTO `sensitive_word_data` VALUES ('房中七经');
INSERT INTO `sensitive_word_data` VALUES ('房中之术');
INSERT INTO `sensitive_word_data` VALUES ('房中术');
INSERT INTO `sensitive_word_data` VALUES ('房事');
INSERT INTO `sensitive_word_data` VALUES ('房事昏厥症');
INSERT INTO `sensitive_word_data` VALUES ('房室伤');
INSERT INTO `sensitive_word_data` VALUES ('房贷给废');
INSERT INTO `sensitive_word_data` VALUES ('扁平湿疣');
INSERT INTO `sensitive_word_data` VALUES ('手变牌');
INSERT INTO `sensitive_word_data` VALUES ('手工猎刀专卖');
INSERT INTO `sensitive_word_data` VALUES ('手工猎刀户外刀具');
INSERT INTO `sensitive_word_data` VALUES ('手把手教你做炸弹');
INSERT INTO `sensitive_word_data` VALUES ('手把手教你制作原子弹');
INSERT INTO `sensitive_word_data` VALUES ('手拉鸡');
INSERT INTO `sensitive_word_data` VALUES ('手教你做原子弹');
INSERT INTO `sensitive_word_data` VALUES ('手教你制作原子弹');
INSERT INTO `sensitive_word_data` VALUES ('手木仓');
INSERT INTO `sensitive_word_data` VALUES ('手机卡复制器');
INSERT INTO `sensitive_word_data` VALUES ('手机复制');
INSERT INTO `sensitive_word_data` VALUES ('手机监');
INSERT INTO `sensitive_word_data` VALUES ('手机监听');
INSERT INTO `sensitive_word_data` VALUES ('手机监听器 ');
INSERT INTO `sensitive_word_data` VALUES ('手机窃');
INSERT INTO `sensitive_word_data` VALUES ('手机窃听器 ');
INSERT INTO `sensitive_word_data` VALUES ('手机跟');
INSERT INTO `sensitive_word_data` VALUES ('手机跟踪定位器 ');
INSERT INTO `sensitive_word_data` VALUES ('手机追');
INSERT INTO `sensitive_word_data` VALUES ('手机魔卡 ');
INSERT INTO `sensitive_word_data` VALUES ('手枪 ');
INSERT INTO `sensitive_word_data` VALUES ('手枪|阻击枪');
INSERT INTO `sensitive_word_data` VALUES ('手枪式折叠三用弩');
INSERT INTO `sensitive_word_data` VALUES ('手枪气* 枪猎');
INSERT INTO `sensitive_word_data` VALUES ('手柄制作炸弹引爆器');
INSERT INTO `sensitive_word_data` VALUES ('手榴弹 ');
INSERT INTO `sensitive_word_data` VALUES ('手槍');
INSERT INTO `sensitive_word_data` VALUES ('手槍买卖自制手枪');
INSERT INTO `sensitive_word_data` VALUES ('手淫危害论');
INSERT INTO `sensitive_word_data` VALUES ('手狗');
INSERT INTO `sensitive_word_data` VALUES ('手答案');
INSERT INTO `sensitive_word_data` VALUES ('才知道只生');
INSERT INTO `sensitive_word_data` VALUES ('扎汗·俄马尔');
INSERT INTO `sensitive_word_data` VALUES ('扒开');
INSERT INTO `sensitive_word_data` VALUES ('打手枪');
INSERT INTO `sensitive_word_data` VALUES ('打死人');
INSERT INTO `sensitive_word_data` VALUES ('打死经过');
INSERT INTO `sensitive_word_data` VALUES ('打泡');
INSERT INTO `sensitive_word_data` VALUES ('打炮');
INSERT INTO `sensitive_word_data` VALUES ('打狗弓弩三步倒');
INSERT INTO `sensitive_word_data` VALUES ('打砸办公');
INSERT INTO `sensitive_word_data` VALUES ('打砸抢 ');
INSERT INTO `sensitive_word_data` VALUES ('打针');
INSERT INTO `sensitive_word_data` VALUES ('打飞机');
INSERT INTO `sensitive_word_data` VALUES ('打飞机专');
INSERT INTO `sensitive_word_data` VALUES ('扣弄');
INSERT INTO `sensitive_word_data` VALUES ('扫了爷爷');
INSERT INTO `sensitive_word_data` VALUES ('扭动');
INSERT INTO `sensitive_word_data` VALUES ('扭捏');
INSERT INTO `sensitive_word_data` VALUES ('扭腰');
INSERT INTO `sensitive_word_data` VALUES ('扭臀');
INSERT INTO `sensitive_word_data` VALUES ('扳开');
INSERT INTO `sensitive_word_data` VALUES ('找女 ');
INSERT INTO `sensitive_word_data` VALUES ('找援交');
INSERT INTO `sensitive_word_data` VALUES ('找政法委副');
INSERT INTO `sensitive_word_data` VALUES ('找枪手');
INSERT INTO `sensitive_word_data` VALUES ('找男 ');
INSERT INTO `sensitive_word_data` VALUES ('技巧');
INSERT INTO `sensitive_word_data` VALUES ('把学生整');
INSERT INTO `sensitive_word_data` VALUES ('把玩');
INSERT INTO `sensitive_word_data` VALUES ('把病人整');
INSERT INTO `sensitive_word_data` VALUES ('把邓小平');
INSERT INTO `sensitive_word_data` VALUES ('抓住');
INSERT INTO `sensitive_word_data` VALUES ('抓弄');
INSERT INTO `sensitive_word_data` VALUES ('抓捏');
INSERT INTO `sensitive_word_data` VALUES ('抓揉');
INSERT INTO `sensitive_word_data` VALUES ('抖颤');
INSERT INTO `sensitive_word_data` VALUES ('折刀专卖网');
INSERT INTO `sensitive_word_data` VALUES ('折刀砍刀专卖');
INSERT INTO `sensitive_word_data` VALUES ('折刀砍刀军品网');
INSERT INTO `sensitive_word_data` VALUES ('折叠狗QQ');
INSERT INTO `sensitive_word_data` VALUES ('抚弄');
INSERT INTO `sensitive_word_data` VALUES ('抚慰');
INSERT INTO `sensitive_word_data` VALUES ('抚捏');
INSERT INTO `sensitive_word_data` VALUES ('抚揉');
INSERT INTO `sensitive_word_data` VALUES ('抚摩');
INSERT INTO `sensitive_word_data` VALUES ('抚摸');
INSERT INTO `sensitive_word_data` VALUES ('抚模');
INSERT INTO `sensitive_word_data` VALUES ('抚爱');
INSERT INTO `sensitive_word_data` VALUES ('抚玩');
INSERT INTO `sensitive_word_data` VALUES ('抚着');
INSERT INTO `sensitive_word_data` VALUES ('抚著');
INSERT INTO `sensitive_word_data` VALUES ('抛浪');
INSERT INTO `sensitive_word_data` VALUES ('抠弄');
INSERT INTO `sensitive_word_data` VALUES ('抠挖');
INSERT INTO `sensitive_word_data` VALUES ('抠摸');
INSERT INTO `sensitive_word_data` VALUES ('抢其火炬');
INSERT INTO `sensitive_word_data` VALUES ('报复执法');
INSERT INTO `sensitive_word_data` VALUES ('报码');
INSERT INTO `sensitive_word_data` VALUES ('抱坐');
INSERT INTO `sensitive_word_data` VALUES ('抱抱');
INSERT INTO `sensitive_word_data` VALUES ('抱着');
INSERT INTO `sensitive_word_data` VALUES ('抱紧');
INSERT INTO `sensitive_word_data` VALUES ('抽');
INSERT INTO `sensitive_word_data` VALUES ('抽了');
INSERT INTO `sensitive_word_data` VALUES ('抽出');
INSERT INTO `sensitive_word_data` VALUES ('抽动');
INSERT INTO `sensitive_word_data` VALUES ('抽弄');
INSERT INTO `sensitive_word_data` VALUES ('抽打');
INSERT INTO `sensitive_word_data` VALUES ('抽捣');
INSERT INTO `sensitive_word_data` VALUES ('抽插');
INSERT INTO `sensitive_word_data` VALUES ('抽搐');
INSERT INTO `sensitive_word_data` VALUES ('抽擦');
INSERT INTO `sensitive_word_data` VALUES ('抽着大中');
INSERT INTO `sensitive_word_data` VALUES ('抽着芙蓉');
INSERT INTO `sensitive_word_data` VALUES ('抽离');
INSERT INTO `sensitive_word_data` VALUES ('抽缩');
INSERT INTO `sensitive_word_data` VALUES ('抽送');
INSERT INTO `sensitive_word_data` VALUES ('抽送着');
INSERT INTO `sensitive_word_data` VALUES ('拆迁灭');
INSERT INTO `sensitive_word_data` VALUES ('拇指');
INSERT INTO `sensitive_word_data` VALUES ('拉开水晶');
INSERT INTO `sensitive_word_data` VALUES ('拉登说');
INSERT INTO `sensitive_word_data` VALUES ('拉萨事件');
INSERT INTO `sensitive_word_data` VALUES ('拍肩型');
INSERT INTO `sensitive_word_data` VALUES ('拍肩神 ');
INSERT INTO `sensitive_word_data` VALUES ('拍肩神药');
INSERT INTO `sensitive_word_data` VALUES ('拍肩醉迷药');
INSERT INTO `sensitive_word_data` VALUES ('拔出');
INSERT INTO `sensitive_word_data` VALUES ('拟涛哥');
INSERT INTO `sensitive_word_data` VALUES ('拥吻');
INSERT INTO `sensitive_word_data` VALUES ('拥抱');
INSERT INTO `sensitive_word_data` VALUES ('拦截器');
INSERT INTO `sensitive_word_data` VALUES ('拨开');
INSERT INTO `sensitive_word_data` VALUES ('拨开阴毛');
INSERT INTO `sensitive_word_data` VALUES ('拨弄');
INSERT INTO `sensitive_word_data` VALUES ('择民');
INSERT INTO `sensitive_word_data` VALUES ('择油录');
INSERT INTO `sensitive_word_data` VALUES ('括约肌');
INSERT INTO `sensitive_word_data` VALUES ('括约肌间沟');
INSERT INTO `sensitive_word_data` VALUES ('拱铲');
INSERT INTO `sensitive_word_data` VALUES ('持续');
INSERT INTO `sensitive_word_data` VALUES ('指头');
INSERT INTO `sensitive_word_data` VALUES ('指技');
INSERT INTO `sensitive_word_data` VALUES ('指纹套');
INSERT INTO `sensitive_word_data` VALUES ('指纹考勤');
INSERT INTO `sensitive_word_data` VALUES ('指纹膜');
INSERT INTO `sensitive_word_data` VALUES ('按压');
INSERT INTO `sensitive_word_data` VALUES ('按揉');
INSERT INTO `sensitive_word_data` VALUES ('按摩');
INSERT INTO `sensitive_word_data` VALUES ('按柔');
INSERT INTO `sensitive_word_data` VALUES ('按照马雅历法');
INSERT INTO `sensitive_word_data` VALUES ('挡中央');
INSERT INTO `sensitive_word_data` VALUES ('挤乳汁');
INSERT INTO `sensitive_word_data` VALUES ('挤压');
INSERT INTO `sensitive_word_data` VALUES ('挤捏');
INSERT INTO `sensitive_word_data` VALUES ('挨了一炮');
INSERT INTO `sensitive_word_data` VALUES ('挫仑');
INSERT INTO `sensitive_word_data` VALUES ('捏');
INSERT INTO `sensitive_word_data` VALUES ('捏弄');
INSERT INTO `sensitive_word_data` VALUES ('捏挤');
INSERT INTO `sensitive_word_data` VALUES ('捏捏');
INSERT INTO `sensitive_word_data` VALUES ('捏掐');
INSERT INTO `sensitive_word_data` VALUES ('捏揉');
INSERT INTO `sensitive_word_data` VALUES ('捏揪');
INSERT INTO `sensitive_word_data` VALUES ('捏摸');
INSERT INTO `sensitive_word_data` VALUES ('换妻');
INSERT INTO `sensitive_word_data` VALUES ('据说全民');
INSERT INTO `sensitive_word_data` VALUES ('掌握');
INSERT INTO `sensitive_word_data` VALUES ('排入');
INSERT INTO `sensitive_word_data` VALUES ('排出');
INSERT INTO `sensitive_word_data` VALUES ('排卵');
INSERT INTO `sensitive_word_data` VALUES ('排卵日');
INSERT INTO `sensitive_word_data` VALUES ('排卵期');
INSERT INTO `sensitive_word_data` VALUES ('排射');
INSERT INTO `sensitive_word_data` VALUES ('排泄');
INSERT INTO `sensitive_word_data` VALUES ('排精');
INSERT INTO `sensitive_word_data` VALUES ('排过精');
INSERT INTO `sensitive_word_data` VALUES ('探测狗');
INSERT INTO `sensitive_word_data` VALUES ('接吻');
INSERT INTO `sensitive_word_data` VALUES ('接触');
INSERT INTO `sensitive_word_data` VALUES ('控制媒');
INSERT INTO `sensitive_word_data` VALUES ('控制射精');
INSERT INTO `sensitive_word_data` VALUES ('控诉世博');
INSERT INTO `sensitive_word_data` VALUES ('推bei图');
INSERT INTO `sensitive_word_data` VALUES ('推油按');
INSERT INTO `sensitive_word_data` VALUES ('推背图');
INSERT INTO `sensitive_word_data` VALUES ('提供专业及时的气狗');
INSERT INTO `sensitive_word_data` VALUES ('提供气狗专卖店');
INSERT INTO `sensitive_word_data` VALUES ('提前预测');
INSERT INTO `sensitive_word_data` VALUES ('插');
INSERT INTO `sensitive_word_data` VALUES ('插入');
INSERT INTO `sensitive_word_data` VALUES ('插奶');
INSERT INTO `sensitive_word_data` VALUES ('插她');
INSERT INTO `sensitive_word_data` VALUES ('插屁屁');
INSERT INTO `sensitive_word_data` VALUES ('插弄');
INSERT INTO `sensitive_word_data` VALUES ('插死你');
INSERT INTO `sensitive_word_data` VALUES ('插爆');
INSERT INTO `sensitive_word_data` VALUES ('插穴');
INSERT INTO `sensitive_word_data` VALUES ('插进');
INSERT INTO `sensitive_word_data` VALUES ('插进插出');
INSERT INTO `sensitive_word_data` VALUES ('插送');
INSERT INTO `sensitive_word_data` VALUES ('揩擦');
INSERT INTO `sensitive_word_data` VALUES ('揭贪难');
INSERT INTO `sensitive_word_data` VALUES ('援交 ');
INSERT INTO `sensitive_word_data` VALUES ('搂抱');
INSERT INTO `sensitive_word_data` VALUES ('搅弄');
INSERT INTO `sensitive_word_data` VALUES ('搓');
INSERT INTO `sensitive_word_data` VALUES ('搓弄');
INSERT INTO `sensitive_word_data` VALUES ('搓捏');
INSERT INTO `sensitive_word_data` VALUES ('搓揉');
INSERT INTO `sensitive_word_data` VALUES ('搓柔');
INSERT INTO `sensitive_word_data` VALUES ('搓玩');
INSERT INTO `sensitive_word_data` VALUES ('搓着');
INSERT INTO `sensitive_word_data` VALUES ('搓著');
INSERT INTO `sensitive_word_data` VALUES ('搓蹭');
INSERT INTO `sensitive_word_data` VALUES ('搜房');
INSERT INTO `sensitive_word_data` VALUES ('搞媛交');
INSERT INTO `sensitive_word_data` VALUES ('摆动');
INSERT INTO `sensitive_word_data` VALUES ('摆布');
INSERT INTO `sensitive_word_data` VALUES ('摆弄');
INSERT INTO `sensitive_word_data` VALUES ('摆脱');
INSERT INTO `sensitive_word_data` VALUES ('摇头丸');
INSERT INTO `sensitive_word_data` VALUES ('摧残');
INSERT INTO `sensitive_word_data` VALUES ('摩小姐');
INSERT INTO `sensitive_word_data` VALUES ('摩弄');
INSERT INTO `sensitive_word_data` VALUES ('摩擦');
INSERT INTO `sensitive_word_data` VALUES ('摩肝益肾法');
INSERT INTO `sensitive_word_data` VALUES ('摸');
INSERT INTO `sensitive_word_data` VALUES ('摸nai门');
INSERT INTO `sensitive_word_data` VALUES ('摸乳');
INSERT INTO `sensitive_word_data` VALUES ('摸他');
INSERT INTO `sensitive_word_data` VALUES ('摸到');
INSERT INTO `sensitive_word_data` VALUES ('摸向');
INSERT INTO `sensitive_word_data` VALUES ('摸弄');
INSERT INTO `sensitive_word_data` VALUES ('摸我');
INSERT INTO `sensitive_word_data` VALUES ('摸抠');
INSERT INTO `sensitive_word_data` VALUES ('摸捏');
INSERT INTO `sensitive_word_data` VALUES ('摸揉');
INSERT INTO `sensitive_word_data` VALUES ('摸摸');
INSERT INTO `sensitive_word_data` VALUES ('摸玩');
INSERT INTO `sensitive_word_data` VALUES ('摸着');
INSERT INTO `sensitive_word_data` VALUES ('摸鸡巴');
INSERT INTO `sensitive_word_data` VALUES ('撅着');
INSERT INTO `sensitive_word_data` VALUES ('撅起');
INSERT INTO `sensitive_word_data` VALUES ('撑涨');
INSERT INTO `sensitive_word_data` VALUES ('撑爆');
INSERT INTO `sensitive_word_data` VALUES ('撑破');
INSERT INTO `sensitive_word_data` VALUES ('撑胀感');
INSERT INTO `sensitive_word_data` VALUES ('撩乱');
INSERT INTO `sensitive_word_data` VALUES ('撩动');
INSERT INTO `sensitive_word_data` VALUES ('撩开');
INSERT INTO `sensitive_word_data` VALUES ('撩弄');
INSERT INTO `sensitive_word_data` VALUES ('撩拨');
INSERT INTO `sensitive_word_data` VALUES ('撩起');
INSERT INTO `sensitive_word_data` VALUES ('播散性淋菌感染');
INSERT INTO `sensitive_word_data` VALUES ('操了嫂');
INSERT INTO `sensitive_word_data` VALUES ('操他妈');
INSERT INTO `sensitive_word_data` VALUES ('操你');
INSERT INTO `sensitive_word_data` VALUES ('操你全家');
INSERT INTO `sensitive_word_data` VALUES ('操你大爷');
INSERT INTO `sensitive_word_data` VALUES ('操你妈');
INSERT INTO `sensitive_word_data` VALUES ('操你娘');
INSERT INTO `sensitive_word_data` VALUES ('操你祖宗');
INSERT INTO `sensitive_word_data` VALUES ('操嫂子');
INSERT INTO `sensitive_word_data` VALUES ('操弄');
INSERT INTO `sensitive_word_data` VALUES ('操我');
INSERT INTO `sensitive_word_data` VALUES ('操死');
INSERT INTO `sensitive_word_data` VALUES ('操穴');
INSERT INTO `sensitive_word_data` VALUES ('操起');
INSERT INTO `sensitive_word_data` VALUES ('操逼');
INSERT INTO `sensitive_word_data` VALUES ('擠乳汁');
INSERT INTO `sensitive_word_data` VALUES ('擦你妈');
INSERT INTO `sensitive_word_data` VALUES ('擦拭');
INSERT INTO `sensitive_word_data` VALUES ('改号软件');
INSERT INTO `sensitive_word_data` VALUES ('改善');
INSERT INTO `sensitive_word_data` VALUES ('改革历程');
INSERT INTO `sensitive_word_data` VALUES ('攻官小姐');
INSERT INTO `sensitive_word_data` VALUES ('放入春药');
INSERT INTO `sensitive_word_data` VALUES ('放在');
INSERT INTO `sensitive_word_data` VALUES ('放荡');
INSERT INTO `sensitive_word_data` VALUES ('政f');
INSERT INTO `sensitive_word_data` VALUES ('政zhi');
INSERT INTO `sensitive_word_data` VALUES ('政一府');
INSERT INTO `sensitive_word_data` VALUES ('政付');
INSERT INTO `sensitive_word_data` VALUES ('政俯');
INSERT INTO `sensitive_word_data` VALUES ('政府');
INSERT INTO `sensitive_word_data` VALUES ('政府操');
INSERT INTO `sensitive_word_data` VALUES ('政治局常委');
INSERT INTO `sensitive_word_data` VALUES ('政百度府');
INSERT INTO `sensitive_word_data` VALUES ('政腐');
INSERT INTO `sensitive_word_data` VALUES ('政论区');
INSERT INTO `sensitive_word_data` VALUES ('敏感');
INSERT INTO `sensitive_word_data` VALUES ('敏感带');
INSERT INTO `sensitive_word_data` VALUES ('救度众生说 ');
INSERT INTO `sensitive_word_data` VALUES ('教你制作原子弹');
INSERT INTO `sensitive_word_data` VALUES ('教你怎么用土办法做武器');
INSERT INTO `sensitive_word_data` VALUES ('敬请忍');
INSERT INTO `sensitive_word_data` VALUES ('整根');
INSERT INTO `sensitive_word_data` VALUES ('整根阴茎');
INSERT INTO `sensitive_word_data` VALUES ('文凭证');
INSERT INTO `sensitive_word_data` VALUES ('文化大革命');
INSERT INTO `sensitive_word_data` VALUES ('文强');
INSERT INTO `sensitive_word_data` VALUES ('文革');
INSERT INTO `sensitive_word_data` VALUES ('斑蝥 ');
INSERT INTO `sensitive_word_data` VALUES ('新唐人');
INSERT INTO `sensitive_word_data` VALUES ('新型毒品');
INSERT INTO `sensitive_word_data` VALUES ('新建户');
INSERT INTO `sensitive_word_data` VALUES ('新疆叛');
INSERT INTO `sensitive_word_data` VALUES ('新疆限');
INSERT INTO `sensitive_word_data` VALUES ('新疆骚乱');
INSERT INTO `sensitive_word_data` VALUES ('新金瓶');
INSERT INTO `sensitive_word_data` VALUES ('方兆本');
INSERT INTO `sensitive_word_data` VALUES ('方兆祥');
INSERT INTO `sensitive_word_data` VALUES ('方励之');
INSERT INTO `sensitive_word_data` VALUES ('方式');
INSERT INTO `sensitive_word_data` VALUES ('方新');
INSERT INTO `sensitive_word_data` VALUES ('方法');
INSERT INTO `sensitive_word_data` VALUES ('方祖岐');
INSERT INTO `sensitive_word_data` VALUES ('方觉');
INSERT INTO `sensitive_word_data` VALUES ('方迷香');
INSERT INTO `sensitive_word_data` VALUES ('施亮');
INSERT INTO `sensitive_word_data` VALUES ('无官正');
INSERT INTO `sensitive_word_data` VALUES ('无帮国');
INSERT INTO `sensitive_word_data` VALUES ('无码');
INSERT INTO `sensitive_word_data` VALUES ('无码专');
INSERT INTO `sensitive_word_data` VALUES ('无耻');
INSERT INTO `sensitive_word_data` VALUES ('无耻语录');
INSERT INTO `sensitive_word_data` VALUES ('日你妈');
INSERT INTO `sensitive_word_data` VALUES ('日月气功 ');
INSERT INTO `sensitive_word_data` VALUES ('早泄');
INSERT INTO `sensitive_word_data` VALUES ('昂奋');
INSERT INTO `sensitive_word_data` VALUES ('昆仑女神功 ');
INSERT INTO `sensitive_word_data` VALUES ('明Hui');
INSERT INTO `sensitive_word_data` VALUES ('明慧');
INSERT INTO `sensitive_word_data` VALUES ('明慧周报');
INSERT INTO `sensitive_word_data` VALUES ('明慧网');
INSERT INTO `sensitive_word_data` VALUES ('昏药');
INSERT INTO `sensitive_word_data` VALUES ('昝爱宗');
INSERT INTO `sensitive_word_data` VALUES ('星上门');
INSERT INTO `sensitive_word_data` VALUES ('春宫');
INSERT INTO `sensitive_word_data` VALUES ('春宫图 ');
INSERT INTO `sensitive_word_data` VALUES ('春心');
INSERT INTO `sensitive_word_data` VALUES ('春情');
INSERT INTO `sensitive_word_data` VALUES ('春水横溢');
INSERT INTO `sensitive_word_data` VALUES ('春洞');
INSERT INTO `sensitive_word_data` VALUES ('春药 ');
INSERT INTO `sensitive_word_data` VALUES ('是躲猫');
INSERT INTO `sensitive_word_data` VALUES ('昼将近');
INSERT INTO `sensitive_word_data` VALUES ('晕倒型');
INSERT INTO `sensitive_word_data` VALUES ('普提功');
INSERT INTO `sensitive_word_data` VALUES ('普萘洛尔');
INSERT INTO `sensitive_word_data` VALUES ('普通嘌');
INSERT INTO `sensitive_word_data` VALUES ('暗红');
INSERT INTO `sensitive_word_data` VALUES ('暗访包 ');
INSERT INTO `sensitive_word_data` VALUES ('暴乱');
INSERT INTO `sensitive_word_data` VALUES ('暴力虐待');
INSERT INTO `sensitive_word_data` VALUES ('暴力袭击');
INSERT INTO `sensitive_word_data` VALUES ('暴政');
INSERT INTO `sensitive_word_data` VALUES ('暴涨');
INSERT INTO `sensitive_word_data` VALUES ('暴胀');
INSERT INTO `sensitive_word_data` VALUES ('暴菊 ');
INSERT INTO `sensitive_word_data` VALUES ('暴露');
INSERT INTO `sensitive_word_data` VALUES ('曲钦岳');
INSERT INTO `sensitive_word_data` VALUES ('曹伯纯');
INSERT INTO `sensitive_word_data` VALUES ('曹其真');
INSERT INTO `sensitive_word_data` VALUES ('曹刚川');
INSERT INTO `sensitive_word_data` VALUES ('曹圣洁');
INSERT INTO `sensitive_word_data` VALUES ('曼妙');
INSERT INTO `sensitive_word_data` VALUES ('曾华');
INSERT INTO `sensitive_word_data` VALUES ('曾培炎 ');
INSERT INTO `sensitive_word_data` VALUES ('曾宪梓');
INSERT INTO `sensitive_word_data` VALUES ('曾庆红 ');
INSERT INTO `sensitive_word_data` VALUES ('曾荫权');
INSERT INTO `sensitive_word_data` VALUES ('曾道人');
INSERT INTO `sensitive_word_data` VALUES ('替人体');
INSERT INTO `sensitive_word_data` VALUES ('替考');
INSERT INTO `sensitive_word_data` VALUES ('最佳');
INSERT INTO `sensitive_word_data` VALUES ('最新销售汽狗');
INSERT INTO `sensitive_word_data` VALUES ('最牛公安');
INSERT INTO `sensitive_word_data` VALUES ('最高');
INSERT INTO `sensitive_word_data` VALUES ('月经');
INSERT INTO `sensitive_word_data` VALUES ('月经不调');
INSERT INTO `sensitive_word_data` VALUES ('月经初潮');
INSERT INTO `sensitive_word_data` VALUES ('月经周期');
INSERT INTO `sensitive_word_data` VALUES ('月经失调');
INSERT INTO `sensitive_word_data` VALUES ('月经异常');
INSERT INTO `sensitive_word_data` VALUES ('月经紊乱');
INSERT INTO `sensitive_word_data` VALUES ('有偿服务 ');
INSERT INTO `sensitive_word_data` VALUES ('有军用枪支出售');
INSERT INTO `sensitive_word_data` VALUES ('有力');
INSERT INTO `sensitive_word_data` VALUES ('有华龙');
INSERT INTO `sensitive_word_data` VALUES ('有奶不一');
INSERT INTO `sensitive_word_data` VALUES ('有码');
INSERT INTO `sensitive_word_data` VALUES ('有舒有缓');
INSERT INTO `sensitive_word_data` VALUES ('服囊肉膜');
INSERT INTO `sensitive_word_data` VALUES ('期出售手枪气枪猎');
INSERT INTO `sensitive_word_data` VALUES ('期货配');
INSERT INTO `sensitive_word_data` VALUES ('木牛流马的污染比汽车飞机大');
INSERT INTO `sensitive_word_data` VALUES ('木齐针');
INSERT INTO `sensitive_word_data` VALUES ('本公司担');
INSERT INTO `sensitive_word_data` VALUES ('本无码');
INSERT INTO `sensitive_word_data` VALUES ('术牌具');
INSERT INTO `sensitive_word_data` VALUES ('朱丽兰');
INSERT INTO `sensitive_word_data` VALUES ('朱云来');
INSERT INTO `sensitive_word_data` VALUES ('朱佩玲');
INSERT INTO `sensitive_word_data` VALUES ('朱兆良');
INSERT INTO `sensitive_word_data` VALUES ('朱启');
INSERT INTO `sensitive_word_data` VALUES ('朱唇');
INSERT INTO `sensitive_word_data` VALUES ('朱培康');
INSERT INTO `sensitive_word_data` VALUES ('朱增泉');
INSERT INTO `sensitive_word_data` VALUES ('朱容基');
INSERT INTO `sensitive_word_data` VALUES ('朱容鸡');
INSERT INTO `sensitive_word_data` VALUES ('朱成虎');
INSERT INTO `sensitive_word_data` VALUES ('朱振中');
INSERT INTO `sensitive_word_data` VALUES ('朱文泉');
INSERT INTO `sensitive_word_data` VALUES ('朱永新');
INSERT INTO `sensitive_word_data` VALUES ('朱海仑');
INSERT INTO `sensitive_word_data` VALUES ('朱瑟里诺');
INSERT INTO `sensitive_word_data` VALUES ('朱相远');
INSERT INTO `sensitive_word_data` VALUES ('朱铭');
INSERT INTO `sensitive_word_data` VALUES ('朱镕基');
INSERT INTO `sensitive_word_data` VALUES ('朱镕鸡');
INSERT INTO `sensitive_word_data` VALUES ('机卡密');
INSERT INTO `sensitive_word_data` VALUES ('机号卫');
INSERT INTO `sensitive_word_data` VALUES ('机号定');
INSERT INTO `sensitive_word_data` VALUES ('机定位器');
INSERT INTO `sensitive_word_data` VALUES ('机屏蔽器');
INSERT INTO `sensitive_word_data` VALUES ('杀b');
INSERT INTO `sensitive_word_data` VALUES ('杀指南');
INSERT INTO `sensitive_word_data` VALUES ('杀警');
INSERT INTO `sensitive_word_data` VALUES ('李peng');
INSERT INTO `sensitive_word_data` VALUES ('李世济');
INSERT INTO `sensitive_word_data` VALUES ('李乾元');
INSERT INTO `sensitive_word_data` VALUES ('李从军');
INSERT INTO `sensitive_word_data` VALUES ('李伙田');
INSERT INTO `sensitive_word_data` VALUES ('李伟信的笔供');
INSERT INTO `sensitive_word_data` VALUES ('李元正');
INSERT INTO `sensitive_word_data` VALUES ('李兆焯');
INSERT INTO `sensitive_word_data` VALUES ('李先念');
INSERT INTO `sensitive_word_data` VALUES ('李克强');
INSERT INTO `sensitive_word_data` VALUES ('李其炎');
INSERT INTO `sensitive_word_data` VALUES ('李勇武');
INSERT INTO `sensitive_word_data` VALUES ('李咏曰');
INSERT INTO `sensitive_word_data` VALUES ('李四光预测');
INSERT INTO `sensitive_word_data` VALUES ('李大同');
INSERT INTO `sensitive_word_data` VALUES ('李大师');
INSERT INTO `sensitive_word_data` VALUES ('李大轮子 ');
INSERT INTO `sensitive_word_data` VALUES ('李奇生');
INSERT INTO `sensitive_word_data` VALUES ('李学举');
INSERT INTO `sensitive_word_data` VALUES ('李宇静');
INSERT INTO `sensitive_word_data` VALUES ('李宏志');
INSERT INTO `sensitive_word_data` VALUES ('李宏治');
INSERT INTO `sensitive_word_data` VALUES ('李宝祥');
INSERT INTO `sensitive_word_data` VALUES ('李小琳');
INSERT INTO `sensitive_word_data` VALUES ('李小鹏');
INSERT INTO `sensitive_word_data` VALUES ('李岚清');
INSERT INTO `sensitive_word_data` VALUES ('李建国');
INSERT INTO `sensitive_word_data` VALUES ('李德洙');
INSERT INTO `sensitive_word_data` VALUES ('李志绥');
INSERT INTO `sensitive_word_data` VALUES ('李愚蠢');
INSERT INTO `sensitive_word_data` VALUES ('李慈君');
INSERT INTO `sensitive_word_data` VALUES ('李慎明');
INSERT INTO `sensitive_word_data` VALUES ('李慧珍');
INSERT INTO `sensitive_word_data` VALUES ('李成玉');
INSERT INTO `sensitive_word_data` VALUES ('李承淑');
INSERT INTO `sensitive_word_data` VALUES ('李敏宽');
INSERT INTO `sensitive_word_data` VALUES ('李新德');
INSERT INTO `sensitive_word_data` VALUES ('李新良');
INSERT INTO `sensitive_word_data` VALUES ('李旺阳');
INSERT INTO `sensitive_word_data` VALUES ('李昌鉴');
INSERT INTO `sensitive_word_data` VALUES ('李明豫');
INSERT INTO `sensitive_word_data` VALUES ('李春亭');
INSERT INTO `sensitive_word_data` VALUES ('李春城');
INSERT INTO `sensitive_word_data` VALUES ('李晓英 ');
INSERT INTO `sensitive_word_data` VALUES ('李月月鸟');
INSERT INTO `sensitive_word_data` VALUES ('李树文');
INSERT INTO `sensitive_word_data` VALUES ('李树菲');
INSERT INTO `sensitive_word_data` VALUES ('李汉田');
INSERT INTO `sensitive_word_data` VALUES ('李沛瑶');
INSERT INTO `sensitive_word_data` VALUES ('李泽钜');
INSERT INTO `sensitive_word_data` VALUES ('李洪X');
INSERT INTO `sensitive_word_data` VALUES ('李洪志');
INSERT INTO `sensitive_word_data` VALUES ('李源潮');
INSERT INTO `sensitive_word_data` VALUES ('李瑞环 ');
INSERT INTO `sensitive_word_data` VALUES ('李登辉 ');
INSERT INTO `sensitive_word_data` VALUES ('李盛霖');
INSERT INTO `sensitive_word_data` VALUES ('李禄');
INSERT INTO `sensitive_word_data` VALUES ('李秋田');
INSERT INTO `sensitive_word_data` VALUES ('李红痔 ');
INSERT INTO `sensitive_word_data` VALUES ('李继耐');
INSERT INTO `sensitive_word_data` VALUES ('李肇星');
INSERT INTO `sensitive_word_data` VALUES ('李至伦');
INSERT INTO `sensitive_word_data` VALUES ('李良辉');
INSERT INTO `sensitive_word_data` VALUES ('李荣融');
INSERT INTO `sensitive_word_data` VALUES ('李蒙');
INSERT INTO `sensitive_word_data` VALUES ('李贵鲜');
INSERT INTO `sensitive_word_data` VALUES ('李赣骝');
INSERT INTO `sensitive_word_data` VALUES ('李连宁');
INSERT INTO `sensitive_word_data` VALUES ('李连玉');
INSERT INTO `sensitive_word_data` VALUES ('李重庵');
INSERT INTO `sensitive_word_data` VALUES ('李金华');
INSERT INTO `sensitive_word_data` VALUES ('李金明');
INSERT INTO `sensitive_word_data` VALUES ('李铁映');
INSERT INTO `sensitive_word_data` VALUES ('李长春');
INSERT INTO `sensitive_word_data` VALUES ('李雅芳');
INSERT INTO `sensitive_word_data` VALUES ('李鹏');
INSERT INTO `sensitive_word_data` VALUES ('杜世成');
INSERT INTO `sensitive_word_data` VALUES ('杜冷丁');
INSERT INTO `sensitive_word_data` VALUES ('杜宜瑾');
INSERT INTO `sensitive_word_data` VALUES ('杜导斌');
INSERT INTO `sensitive_word_data` VALUES ('杜德印');
INSERT INTO `sensitive_word_data` VALUES ('杜湘成');
INSERT INTO `sensitive_word_data` VALUES ('杜铁环');
INSERT INTO `sensitive_word_data` VALUES ('杜青林');
INSERT INTO `sensitive_word_data` VALUES ('来搔抚');
INSERT INTO `sensitive_word_data` VALUES ('来潮');
INSERT INTO `sensitive_word_data` VALUES ('来福猎');
INSERT INTO `sensitive_word_data` VALUES ('来经');
INSERT INTO `sensitive_word_data` VALUES ('杨j');
INSERT INTO `sensitive_word_data` VALUES ('杨佳');
INSERT INTO `sensitive_word_data` VALUES ('杨俊文');
INSERT INTO `sensitive_word_data` VALUES ('杨兴富');
INSERT INTO `sensitive_word_data` VALUES ('杨国庆');
INSERT INTO `sensitive_word_data` VALUES ('杨国梁');
INSERT INTO `sensitive_word_data` VALUES ('杨子立');
INSERT INTO `sensitive_word_data` VALUES ('杨孙西');
INSERT INTO `sensitive_word_data` VALUES ('杨岐');
INSERT INTO `sensitive_word_data` VALUES ('杨崇汇');
INSERT INTO `sensitive_word_data` VALUES ('杨德清');
INSERT INTO `sensitive_word_data` VALUES ('杨振杰');
INSERT INTO `sensitive_word_data` VALUES ('杨文武');
INSERT INTO `sensitive_word_data` VALUES ('杨斌');
INSERT INTO `sensitive_word_data` VALUES ('杨春兴');
INSERT INTO `sensitive_word_data` VALUES ('杨景宇');
INSERT INTO `sensitive_word_data` VALUES ('杨晶');
INSERT INTO `sensitive_word_data` VALUES ('杨柏龄');
INSERT INTO `sensitive_word_data` VALUES ('杨树宽');
INSERT INTO `sensitive_word_data` VALUES ('杨永良');
INSERT INTO `sensitive_word_data` VALUES ('杨洁篪');
INSERT INTO `sensitive_word_data` VALUES ('杨茂东');
INSERT INTO `sensitive_word_data` VALUES ('杨邦杰');
INSERT INTO `sensitive_word_data` VALUES ('杨长槐');
INSERT INTO `sensitive_word_data` VALUES ('東京熱');
INSERT INTO `sensitive_word_data` VALUES ('极端武力军品网');
INSERT INTO `sensitive_word_data` VALUES ('极端武力折刀');
INSERT INTO `sensitive_word_data` VALUES ('极端武力直销网');
INSERT INTO `sensitive_word_data` VALUES ('林兆枢');
INSERT INTO `sensitive_word_data` VALUES ('林左鸣');
INSERT INTO `sensitive_word_data` VALUES ('林强');
INSERT INTO `sensitive_word_data` VALUES ('林彪');
INSERT INTO `sensitive_word_data` VALUES ('林文漪');
INSERT INTO `sensitive_word_data` VALUES ('林树森');
INSERT INTO `sensitive_word_data` VALUES ('林炎志');
INSERT INTO `sensitive_word_data` VALUES ('林醉');
INSERT INTO `sensitive_word_data` VALUES ('枪决女犯');
INSERT INTO `sensitive_word_data` VALUES ('枪决现场');
INSERT INTO `sensitive_word_data` VALUES ('枪出售');
INSERT INTO `sensitive_word_data` VALUES ('枪子弹');
INSERT INTO `sensitive_word_data` VALUES ('枪手网');
INSERT INTO `sensitive_word_data` VALUES ('枪手队');
INSERT INTO `sensitive_word_data` VALUES ('枪支');
INSERT INTO `sensitive_word_data` VALUES ('枪械制');
INSERT INTO `sensitive_word_data` VALUES ('枪模');
INSERT INTO `sensitive_word_data` VALUES ('枪的分');
INSERT INTO `sensitive_word_data` VALUES ('枪的制');
INSERT INTO `sensitive_word_data` VALUES ('枪的参');
INSERT INTO `sensitive_word_data` VALUES ('枪的结');
INSERT INTO `sensitive_word_data` VALUES ('枪货到');
INSERT INTO `sensitive_word_data` VALUES ('枪销售');
INSERT INTO `sensitive_word_data` VALUES ('柔胸粉');
INSERT INTO `sensitive_word_data` VALUES ('柳斌');
INSERT INTO `sensitive_word_data` VALUES ('柳斌杰');
INSERT INTO `sensitive_word_data` VALUES ('柴玲');
INSERT INTO `sensitive_word_data` VALUES ('标准');
INSERT INTO `sensitive_word_data` VALUES ('标准炸弹教程');
INSERT INTO `sensitive_word_data` VALUES ('栗智');
INSERT INTO `sensitive_word_data` VALUES ('校骚乱');
INSERT INTO `sensitive_word_data` VALUES ('核弹制作方法');
INSERT INTO `sensitive_word_data` VALUES ('核弹头的制造');
INSERT INTO `sensitive_word_data` VALUES ('根 部');
INSERT INTO `sensitive_word_data` VALUES ('根插');
INSERT INTO `sensitive_word_data` VALUES ('根毛');
INSERT INTO `sensitive_word_data` VALUES ('根浴 ');
INSERT INTO `sensitive_word_data` VALUES ('根达亚文明');
INSERT INTO `sensitive_word_data` VALUES ('根部');
INSERT INTO `sensitive_word_data` VALUES ('格证考试');
INSERT INTO `sensitive_word_data` VALUES ('栾恩杰');
INSERT INTO `sensitive_word_data` VALUES ('桂世镛');
INSERT INTO `sensitive_word_data` VALUES ('案的准确');
INSERT INTO `sensitive_word_data` VALUES ('档中央');
INSERT INTO `sensitive_word_data` VALUES ('档部');
INSERT INTO `sensitive_word_data` VALUES ('梁光烈');
INSERT INTO `sensitive_word_data` VALUES ('梁国扬');
INSERT INTO `sensitive_word_data` VALUES ('梁定邦');
INSERT INTO `sensitive_word_data` VALUES ('梁振英');
INSERT INTO `sensitive_word_data` VALUES ('梁绮萍');
INSERT INTO `sensitive_word_data` VALUES ('梁荣欣');
INSERT INTO `sensitive_word_data` VALUES ('梁金泉');
INSERT INTO `sensitive_word_data` VALUES ('梅毒疹');
INSERT INTO `sensitive_word_data` VALUES ('梅毒螺旋体');
INSERT INTO `sensitive_word_data` VALUES ('梓健特药');
INSERT INTO `sensitive_word_data` VALUES ('梦交');
INSERT INTO `sensitive_word_data` VALUES ('梦失精');
INSERT INTO `sensitive_word_data` VALUES ('梦泄精');
INSERT INTO `sensitive_word_data` VALUES ('梦遗');
INSERT INTO `sensitive_word_data` VALUES ('棒');
INSERT INTO `sensitive_word_data` VALUES ('棒棒');
INSERT INTO `sensitive_word_data` VALUES ('植物冰');
INSERT INTO `sensitive_word_data` VALUES ('植物性神经');
INSERT INTO `sensitive_word_data` VALUES ('椒乳');
INSERT INTO `sensitive_word_data` VALUES ('模式');
INSERT INTO `sensitive_word_data` VALUES ('横冲直撞');
INSERT INTO `sensitive_word_data` VALUES ('樱口');
INSERT INTO `sensitive_word_data` VALUES ('樱口之技');
INSERT INTO `sensitive_word_data` VALUES ('樱口之枝');
INSERT INTO `sensitive_word_data` VALUES ('欠干');
INSERT INTO `sensitive_word_data` VALUES ('次数');
INSERT INTO `sensitive_word_data` VALUES ('次通过考');
INSERT INTO `sensitive_word_data` VALUES ('欢吟');
INSERT INTO `sensitive_word_data` VALUES ('欢悦');
INSERT INTO `sensitive_word_data` VALUES ('欢愉');
INSERT INTO `sensitive_word_data` VALUES ('欢爱');
INSERT INTO `sensitive_word_data` VALUES ('欧广源');
INSERT INTO `sensitive_word_data` VALUES ('欧阳明高');
INSERT INTO `sensitive_word_data` VALUES ('欲感');
INSERT INTO `sensitive_word_data` VALUES ('欲望');
INSERT INTO `sensitive_word_data` VALUES ('欲火');
INSERT INTO `sensitive_word_data` VALUES ('欲焰');
INSERT INTO `sensitive_word_data` VALUES ('正品军刺出售');
INSERT INTO `sensitive_word_data` VALUES ('正府');
INSERT INTO `sensitive_word_data` VALUES ('正见网');
INSERT INTO `sensitive_word_data` VALUES ('武侯祠');
INSERT INTO `sensitive_word_data` VALUES ('武警已增');
INSERT INTO `sensitive_word_data` VALUES ('武警暴');
INSERT INTO `sensitive_word_data` VALUES ('武警殴');
INSERT INTO `sensitive_word_data` VALUES ('武连元');
INSERT INTO `sensitive_word_data` VALUES ('死亡笔记');
INSERT INTO `sensitive_word_data` VALUES ('死全家');
INSERT INTO `sensitive_word_data` VALUES ('死法分布');
INSERT INTO `sensitive_word_data` VALUES ('死要见毛');
INSERT INTO `sensitive_word_data` VALUES ('殖器护');
INSERT INTO `sensitive_word_data` VALUES ('段桂清');
INSERT INTO `sensitive_word_data` VALUES ('段桂青');
INSERT INTO `sensitive_word_data` VALUES ('母乳家');
INSERT INTO `sensitive_word_data` VALUES ('母子乱伦 ');
INSERT INTO `sensitive_word_data` VALUES ('母痔区');
INSERT INTO `sensitive_word_data` VALUES ('每周一死');
INSERT INTO `sensitive_word_data` VALUES ('毒蛇钻');
INSERT INTO `sensitive_word_data` VALUES ('毕业證');
INSERT INTO `sensitive_word_data` VALUES ('毛zd');
INSERT INTO `sensitive_word_data` VALUES ('毛zx');
INSERT INTO `sensitive_word_data` VALUES ('毛一鲜');
INSERT INTO `sensitive_word_data` VALUES ('毛主席的嫡孙 ');
INSERT INTO `sensitive_word_data` VALUES ('毛囊');
INSERT INTO `sensitive_word_data` VALUES ('毛增华');
INSERT INTO `sensitive_word_data` VALUES ('毛太祖');
INSERT INTO `sensitive_word_data` VALUES ('毛如柏');
INSERT INTO `sensitive_word_data` VALUES ('毛时代大饥荒揭秘');
INSERT INTO `sensitive_word_data` VALUES ('毛泽东');
INSERT INTO `sensitive_word_data` VALUES ('毛泽东侄子 ');
INSERT INTO `sensitive_word_data` VALUES ('毛泽东复活 ');
INSERT INTO `sensitive_word_data` VALUES ('毛片');
INSERT INTO `sensitive_word_data` VALUES ('毛相');
INSERT INTO `sensitive_word_data` VALUES ('毛茸茸');
INSERT INTO `sensitive_word_data` VALUES ('毛贼 ');
INSERT INTO `sensitive_word_data` VALUES ('民主墙');
INSERT INTO `sensitive_word_data` VALUES ('民主潮');
INSERT INTO `sensitive_word_data` VALUES ('民九亿商');
INSERT INTO `sensitive_word_data` VALUES ('民储害');
INSERT INTO `sensitive_word_data` VALUES ('民抗议');
INSERT INTO `sensitive_word_data` VALUES ('民泽江');
INSERT INTO `sensitive_word_data` VALUES ('民运');
INSERT INTO `sensitive_word_data` VALUES ('氓培训');
INSERT INTO `sensitive_word_data` VALUES ('气枪');
INSERT INTO `sensitive_word_data` VALUES ('气枪/出售/专卖网');
INSERT INTO `sensitive_word_data` VALUES ('气枪/猎枪/钢珠枪');
INSERT INTO `sensitive_word_data` VALUES ('气枪专卖网');
INSERT INTO `sensitive_word_data` VALUES ('气枪价格 仿真气枪');
INSERT INTO `sensitive_word_data` VALUES ('气枪价格，气枪专卖店');
INSERT INTO `sensitive_word_data` VALUES ('气枪仿真枪出售');
INSERT INTO `sensitive_word_data` VALUES ('气枪猎枪麻醉枪');
INSERT INTO `sensitive_word_data` VALUES ('气枪看货加QQ');
INSERT INTO `sensitive_word_data` VALUES ('气枪联系QQ');
INSERT INTO `sensitive_word_data` VALUES ('气枪货到付款,汽狗出售');
INSERT INTO `sensitive_word_data` VALUES ('气枪购买|气枪组装');
INSERT INTO `sensitive_word_data` VALUES ('气枪迷魂药');
INSERT INTO `sensitive_word_data` VALUES ('气氛');
INSERT INTO `sensitive_word_data` VALUES ('气淋');
INSERT INTO `sensitive_word_data` VALUES ('气狗');
INSERT INTO `sensitive_word_data` VALUES ('气狗 高压气枪');
INSERT INTO `sensitive_word_data` VALUES ('气狗出售 进口气狗');
INSERT INTO `sensitive_word_data` VALUES ('氢弹手');
INSERT INTO `sensitive_word_data` VALUES ('氢弹手工制作方法');
INSERT INTO `sensitive_word_data` VALUES ('氣槍');
INSERT INTO `sensitive_word_data` VALUES ('氯噻嗪');
INSERT INTO `sensitive_word_data` VALUES ('氯胺酮');
INSERT INTO `sensitive_word_data` VALUES ('氰化钠');
INSERT INTO `sensitive_word_data` VALUES ('氰化钾');
INSERT INTO `sensitive_word_data` VALUES ('水浴法制TNT');
INSERT INTO `sensitive_word_data` VALUES ('水阎王');
INSERT INTO `sensitive_word_data` VALUES ('氵去车仑');
INSERT INTO `sensitive_word_data` VALUES ('汉芯造假');
INSERT INTO `sensitive_word_data` VALUES ('江core');
INSERT INTO `sensitive_word_data` VALUES ('江x');
INSERT INTO `sensitive_word_data` VALUES ('江zm');
INSERT INTO `sensitive_word_data` VALUES ('江z民 ');
INSERT INTO `sensitive_word_data` VALUES ('江三条腿 ');
INSERT INTO `sensitive_word_data` VALUES ('江丑闻 ');
INSERT INTO `sensitive_word_data` VALUES ('江主席');
INSERT INTO `sensitive_word_data` VALUES ('江书记');
INSERT INTO `sensitive_word_data` VALUES ('江人马 ');
INSERT INTO `sensitive_word_data` VALUES ('江公子');
INSERT INTO `sensitive_word_data` VALUES ('江哥');
INSERT INTO `sensitive_word_data` VALUES ('江太上');
INSERT INTO `sensitive_word_data` VALUES ('江嫡系 ');
INSERT INTO `sensitive_word_data` VALUES ('江宰民 ');
INSERT INTO `sensitive_word_data` VALUES ('江家帮 ');
INSERT INTO `sensitive_word_data` VALUES ('江戏子 ');
INSERT INTO `sensitive_word_data` VALUES ('江某某');
INSERT INTO `sensitive_word_data` VALUES ('江核心 ');
INSERT INTO `sensitive_word_data` VALUES ('江梳头 ');
INSERT INTO `sensitive_word_data` VALUES ('江棋生');
INSERT INTO `sensitive_word_data` VALUES ('江毒 ');
INSERT INTO `sensitive_word_data` VALUES ('江氏家族 ');
INSERT INTO `sensitive_word_data` VALUES ('江氏政治委员 ');
INSERT INTO `sensitive_word_data` VALUES ('江氏政治局 ');
INSERT INTO `sensitive_word_data` VALUES ('江氏集团 ');
INSERT INTO `sensitive_word_data` VALUES ('江沢民');
INSERT INTO `sensitive_word_data` VALUES ('江泉集团 ');
INSERT INTO `sensitive_word_data` VALUES ('江泽慧');
INSERT INTO `sensitive_word_data` VALUES ('江泽民');
INSERT INTO `sensitive_word_data` VALUES ('江派 ');
INSERT INTO `sensitive_word_data` VALUES ('江派人马 ');
INSERT INTO `sensitive_word_data` VALUES ('江派和胡派 ');
INSERT INTO `sensitive_word_data` VALUES ('江浙民');
INSERT INTO `sensitive_word_data` VALUES ('江浙闽');
INSERT INTO `sensitive_word_data` VALUES ('江独裁 ');
INSERT INTO `sensitive_word_data` VALUES ('江猪 ');
INSERT INTO `sensitive_word_data` VALUES ('江祸心 ');
INSERT INTO `sensitive_word_data` VALUES ('江系人');
INSERT INTO `sensitive_word_data` VALUES ('江系人马 ');
INSERT INTO `sensitive_word_data` VALUES ('江绵康');
INSERT INTO `sensitive_word_data` VALUES ('江绵恒 ');
INSERT INTO `sensitive_word_data` VALUES ('江胡');
INSERT INTO `sensitive_word_data` VALUES ('江胡内斗');
INSERT INTO `sensitive_word_data` VALUES ('江蛤蟆 ');
INSERT INTO `sensitive_word_data` VALUES ('江责民');
INSERT INTO `sensitive_word_data` VALUES ('江贼 ');
INSERT INTO `sensitive_word_data` VALUES ('江贼民');
INSERT INTO `sensitive_word_data` VALUES ('江锦恒');
INSERT INTO `sensitive_word_data` VALUES ('江青');
INSERT INTO `sensitive_word_data` VALUES ('江驴 ');
INSERT INTO `sensitive_word_data` VALUES ('江黑心 ');
INSERT INTO `sensitive_word_data` VALUES ('汤洪高');
INSERT INTO `sensitive_word_data` VALUES ('汪东兴');
INSERT INTO `sensitive_word_data` VALUES ('汪光焘');
INSERT INTO `sensitive_word_data` VALUES ('汪啸风');
INSERT INTO `sensitive_word_data` VALUES ('汪恕诚');
INSERT INTO `sensitive_word_data` VALUES ('汪纪戎');
INSERT INTO `sensitive_word_data` VALUES ('汽枪');
INSERT INTO `sensitive_word_data` VALUES ('汽油三分之一白糖混合');
INSERT INTO `sensitive_word_data` VALUES ('汽狗');
INSERT INTO `sensitive_word_data` VALUES ('汽狗高压汽枪');
INSERT INTO `sensitive_word_data` VALUES ('汽车炸弹制作');
INSERT INTO `sensitive_word_data` VALUES ('汽车走表器 ');
INSERT INTO `sensitive_word_data` VALUES ('沈昌人体科技 ');
INSERT INTO `sensitive_word_data` VALUES ('沈昌功 ');
INSERT INTO `sensitive_word_data` VALUES ('沈春耀');
INSERT INTO `sensitive_word_data` VALUES ('沈跃跃');
INSERT INTO `sensitive_word_data` VALUES ('沈辛荪');
INSERT INTO `sensitive_word_data` VALUES ('沙比');
INSERT INTO `sensitive_word_data` VALUES ('沟缝');
INSERT INTO `sensitive_word_data` VALUES ('河殇');
INSERT INTO `sensitive_word_data` VALUES ('油黑');
INSERT INTO `sensitive_word_data` VALUES ('治疗乙肝 ');
INSERT INTO `sensitive_word_data` VALUES ('治疗性病 ');
INSERT INTO `sensitive_word_data` VALUES ('治疗红斑狼疮 ');
INSERT INTO `sensitive_word_data` VALUES ('治疗肿瘤 ');
INSERT INTO `sensitive_word_data` VALUES ('治荡');
INSERT INTO `sensitive_word_data` VALUES ('泄漏的内');
INSERT INTO `sensitive_word_data` VALUES ('泊泊');
INSERT INTO `sensitive_word_data` VALUES ('泌乳');
INSERT INTO `sensitive_word_data` VALUES ('泌出');
INSERT INTO `sensitive_word_data` VALUES ('泌尿生殖系统');
INSERT INTO `sensitive_word_data` VALUES ('泌尿系感染');
INSERT INTO `sensitive_word_data` VALUES ('泌尿系统');
INSERT INTO `sensitive_word_data` VALUES ('法0功');
INSERT INTO `sensitive_word_data` VALUES ('法L ');
INSERT INTO `sensitive_word_data` VALUES ('法lg');
INSERT INTO `sensitive_word_data` VALUES ('法lun');
INSERT INTO `sensitive_word_data` VALUES ('法o功');
INSERT INTO `sensitive_word_data` VALUES ('法x功');
INSERT INTO `sensitive_word_data` VALUES ('法一轮');
INSERT INTO `sensitive_word_data` VALUES ('法一轮一功');
INSERT INTO `sensitive_word_data` VALUES ('法仑功 ');
INSERT INTO `sensitive_word_data` VALUES ('法会');
INSERT INTO `sensitive_word_data` VALUES ('法伦 ');
INSERT INTO `sensitive_word_data` VALUES ('法伦功');
INSERT INTO `sensitive_word_data` VALUES ('法力像佛 ');
INSERT INTO `sensitive_word_data` VALUES ('法功 ');
INSERT INTO `sensitive_word_data` VALUES ('法拉盛 ');
INSERT INTO `sensitive_word_data` VALUES ('法拉盛缅街 ');
INSERT INTO `sensitive_word_data` VALUES ('法某功 ');
INSERT INTO `sensitive_word_data` VALUES ('法正乾');
INSERT INTO `sensitive_word_data` VALUES ('法维权');
INSERT INTO `sensitive_word_data` VALUES ('法能功 ');
INSERT INTO `sensitive_word_data` VALUES ('法西藏主义 ');
INSERT INTO `sensitive_word_data` VALUES ('法论功 ');
INSERT INTO `sensitive_word_data` VALUES ('法輪');
INSERT INTO `sensitive_word_data` VALUES ('法车仑');
INSERT INTO `sensitive_word_data` VALUES ('法轮');
INSERT INTO `sensitive_word_data` VALUES ('法轮义 ');
INSERT INTO `sensitive_word_data` VALUES ('法轮佛');
INSERT INTO `sensitive_word_data` VALUES ('法轮佛法 ');
INSERT INTO `sensitive_word_data` VALUES ('法轮公 ');
INSERT INTO `sensitive_word_data` VALUES ('法轮功 ');
INSERT INTO `sensitive_word_data` VALUES ('法轮大法 ');
INSERT INTO `sensitive_word_data` VALUES ('法轮大法好');
INSERT INTO `sensitive_word_data` VALUES ('法轮工 ');
INSERT INTO `sensitive_word_data` VALUES ('法院给废');
INSERT INTO `sensitive_word_data` VALUES ('泡友 ');
INSERT INTO `sensitive_word_data` VALUES ('泡彦');
INSERT INTO `sensitive_word_data` VALUES ('泡浴');
INSERT INTO `sensitive_word_data` VALUES ('泡疹性外阴炎');
INSERT INTO `sensitive_word_data` VALUES ('波推龙');
INSERT INTO `sensitive_word_data` VALUES ('波胆');
INSERT INTO `sensitive_word_data` VALUES ('泥马之歌');
INSERT INTO `sensitive_word_data` VALUES ('泰兴幼');
INSERT INTO `sensitive_word_data` VALUES ('泰兴镇中');
INSERT INTO `sensitive_word_data` VALUES ('泰州幼');
INSERT INTO `sensitive_word_data` VALUES ('泼尼松');
INSERT INTO `sensitive_word_data` VALUES ('泽d');
INSERT INTO `sensitive_word_data` VALUES ('洁阴法');
INSERT INTO `sensitive_word_data` VALUES ('洗澡死');
INSERT INTO `sensitive_word_data` VALUES ('洗脑班');
INSERT INTO `sensitive_word_data` VALUES ('洞口');
INSERT INTO `sensitive_word_data` VALUES ('洞小口紧');
INSERT INTO `sensitive_word_data` VALUES ('洞开');
INSERT INTO `sensitive_word_data` VALUES ('洞洞');
INSERT INTO `sensitive_word_data` VALUES ('洞眼');
INSERT INTO `sensitive_word_data` VALUES ('洞穴');
INSERT INTO `sensitive_word_data` VALUES ('津地震');
INSERT INTO `sensitive_word_data` VALUES ('津大地震');
INSERT INTO `sensitive_word_data` VALUES ('津液');
INSERT INTO `sensitive_word_data` VALUES ('洪吟');
INSERT INTO `sensitive_word_data` VALUES ('洪绂曾');
INSERT INTO `sensitive_word_data` VALUES ('活塞');
INSERT INTO `sensitive_word_data` VALUES ('派系斗争 ');
INSERT INTO `sensitive_word_data` VALUES ('流');
INSERT INTO `sensitive_word_data` VALUES ('流出');
INSERT INTO `sensitive_word_data` VALUES ('流到');
INSERT INTO `sensitive_word_data` VALUES ('流溢');
INSERT INTO `sensitive_word_data` VALUES ('流血事');
INSERT INTO `sensitive_word_data` VALUES ('浅会阴筋膜');
INSERT INTO `sensitive_word_data` VALUES ('浅出浅入');
INSERT INTO `sensitive_word_data` VALUES ('浆汁');
INSERT INTO `sensitive_word_data` VALUES ('济世灵文');
INSERT INTO `sensitive_word_data` VALUES ('浑圆');
INSERT INTO `sensitive_word_data` VALUES ('浑圆豪乳');
INSERT INTO `sensitive_word_data` VALUES ('浓密的阴毛');
INSERT INTO `sensitive_word_data` VALUES ('浓浊');
INSERT INTO `sensitive_word_data` VALUES ('浓热');
INSERT INTO `sensitive_word_data` VALUES ('浓稠');
INSERT INTO `sensitive_word_data` VALUES ('浓精');
INSERT INTO `sensitive_word_data` VALUES ('浓黑');
INSERT INTO `sensitive_word_data` VALUES ('浦志强');
INSERT INTO `sensitive_word_data` VALUES ('浪劲');
INSERT INTO `sensitive_word_data` VALUES ('浪叫');
INSERT INTO `sensitive_word_data` VALUES ('浪吟');
INSERT INTO `sensitive_word_data` VALUES ('浪哼');
INSERT INTO `sensitive_word_data` VALUES ('浪喘');
INSERT INTO `sensitive_word_data` VALUES ('浪女');
INSERT INTO `sensitive_word_data` VALUES ('浪妇');
INSERT INTO `sensitive_word_data` VALUES ('浪媚');
INSERT INTO `sensitive_word_data` VALUES ('浪态');
INSERT INTO `sensitive_word_data` VALUES ('浪样');
INSERT INTO `sensitive_word_data` VALUES ('浪漫邂逅');
INSERT INTO `sensitive_word_data` VALUES ('浪穴');
INSERT INTO `sensitive_word_data` VALUES ('浪语');
INSERT INTO `sensitive_word_data` VALUES ('浪货');
INSERT INTO `sensitive_word_data` VALUES ('浪逼');
INSERT INTO `sensitive_word_data` VALUES ('浴室');
INSERT INTO `sensitive_word_data` VALUES ('海luo因');
INSERT INTO `sensitive_word_data` VALUES ('海洛因');
INSERT INTO `sensitive_word_data` VALUES ('海访民');
INSERT INTO `sensitive_word_data` VALUES ('浸润');
INSERT INTO `sensitive_word_data` VALUES ('浸润癌');
INSERT INTO `sensitive_word_data` VALUES ('浸淫');
INSERT INTO `sensitive_word_data` VALUES ('浸湿');
INSERT INTO `sensitive_word_data` VALUES ('消声器 ');
INSERT INTO `sensitive_word_data` VALUES ('消防枪 ');
INSERT INTO `sensitive_word_data` VALUES ('消防灭火枪  ');
INSERT INTO `sensitive_word_data` VALUES ('涉嫌抄袭');
INSERT INTO `sensitive_word_data` VALUES ('涌入');
INSERT INTO `sensitive_word_data` VALUES ('涌出');
INSERT INTO `sensitive_word_data` VALUES ('涌向');
INSERT INTO `sensitive_word_data` VALUES ('涌泉');
INSERT INTO `sensitive_word_data` VALUES ('涛一样胡');
INSERT INTO `sensitive_word_data` VALUES ('涛共产');
INSERT INTO `sensitive_word_data` VALUES ('液体炸');
INSERT INTO `sensitive_word_data` VALUES ('液体炸药配方');
INSERT INTO `sensitive_word_data` VALUES ('淋巴管');
INSERT INTO `sensitive_word_data` VALUES ('淋巴结');
INSERT INTO `sensitive_word_data` VALUES ('淋漓');
INSERT INTO `sensitive_word_data` VALUES ('淋球菌');
INSERT INTO `sensitive_word_data` VALUES ('淋病');
INSERT INTO `sensitive_word_data` VALUES ('淋菌');
INSERT INTO `sensitive_word_data` VALUES ('淋证');
INSERT INTO `sensitive_word_data` VALUES ('淘宝气枪货到付款');
INSERT INTO `sensitive_word_data` VALUES ('淫兽');
INSERT INTO `sensitive_word_data` VALUES ('淫兽学');
INSERT INTO `sensitive_word_data` VALUES ('淫情女');
INSERT INTO `sensitive_word_data` VALUES ('淫水');
INSERT INTO `sensitive_word_data` VALUES ('淫穴');
INSERT INTO `sensitive_word_data` VALUES ('淫肉');
INSERT INTO `sensitive_word_data` VALUES ('淫靡 ');
INSERT INTO `sensitive_word_data` VALUES ('淫騷妹');
INSERT INTO `sensitive_word_data` VALUES ('淫魔舞');
INSERT INTO `sensitive_word_data` VALUES ('深喉冰');
INSERT INTO `sensitive_word_data` VALUES ('混圆');
INSERT INTO `sensitive_word_data` VALUES ('混蛋');
INSERT INTO `sensitive_word_data` VALUES ('清华帮 ');
INSERT INTO `sensitive_word_data` VALUES ('清明桥');
INSERT INTO `sensitive_word_data` VALUES ('清海无上师');
INSERT INTO `sensitive_word_data` VALUES ('清純壆');
INSERT INTO `sensitive_word_data` VALUES ('清除负面');
INSERT INTO `sensitive_word_data` VALUES ('渊盖苏文 ');
INSERT INTO `sensitive_word_data` VALUES ('温jb');
INSERT INTO `sensitive_word_data` VALUES ('温jia宝');
INSERT INTO `sensitive_word_data` VALUES ('温x');
INSERT INTO `sensitive_word_data` VALUES ('温下台 ');
INSERT INTO `sensitive_word_data` VALUES ('温云松');
INSERT INTO `sensitive_word_data` VALUES ('温休曾退 ');
INSERT INTO `sensitive_word_data` VALUES ('温元凯');
INSERT INTO `sensitive_word_data` VALUES ('温切斯特');
INSERT INTO `sensitive_word_data` VALUES ('温加保');
INSERT INTO `sensitive_word_data` VALUES ('温加宝');
INSERT INTO `sensitive_word_data` VALUES ('温加饱 ');
INSERT INTO `sensitive_word_data` VALUES ('温如春');
INSERT INTO `sensitive_word_data` VALUES ('温宝宝');
INSERT INTO `sensitive_word_data` VALUES ('温家保');
INSERT INTO `sensitive_word_data` VALUES ('温家堡');
INSERT INTO `sensitive_word_data` VALUES ('温家宝');
INSERT INTO `sensitive_word_data` VALUES ('温家饱');
INSERT INTO `sensitive_word_data` VALUES ('温影帝');
INSERT INTO `sensitive_word_data` VALUES ('温总');
INSERT INTO `sensitive_word_data` VALUES ('港澳博球');
INSERT INTO `sensitive_word_data` VALUES ('港鑫華');
INSERT INTO `sensitive_word_data` VALUES ('港馬會');
INSERT INTO `sensitive_word_data` VALUES ('渴望');
INSERT INTO `sensitive_word_data` VALUES ('游动');
INSERT INTO `sensitive_word_data` VALUES ('游戏机破解 ');
INSERT INTO `sensitive_word_data` VALUES ('游移');
INSERT INTO `sensitive_word_data` VALUES ('游精佑');
INSERT INTO `sensitive_word_data` VALUES ('游行');
INSERT INTO `sensitive_word_data` VALUES ('湖淫娘');
INSERT INTO `sensitive_word_data` VALUES ('湖紧掏');
INSERT INTO `sensitive_word_data` VALUES ('湾版假');
INSERT INTO `sensitive_word_data` VALUES ('溫家寶');
INSERT INTO `sensitive_word_data` VALUES ('滋润');
INSERT INTO `sensitive_word_data` VALUES ('滑下');
INSERT INTO `sensitive_word_data` VALUES ('滑入');
INSERT INTO `sensitive_word_data` VALUES ('滑出');
INSERT INTO `sensitive_word_data` VALUES ('滑到');
INSERT INTO `sensitive_word_data` VALUES ('滑动');
INSERT INTO `sensitive_word_data` VALUES ('滑向');
INSERT INTO `sensitive_word_data` VALUES ('滑嫩');
INSERT INTO `sensitive_word_data` VALUES ('滑抚');
INSERT INTO `sensitive_word_data` VALUES ('滑润');
INSERT INTO `sensitive_word_data` VALUES ('滑湿');
INSERT INTO `sensitive_word_data` VALUES ('滑溜');
INSERT INTO `sensitive_word_data` VALUES ('滑爽');
INSERT INTO `sensitive_word_data` VALUES ('滑粘');
INSERT INTO `sensitive_word_data` VALUES ('滑美');
INSERT INTO `sensitive_word_data` VALUES ('滑进');
INSERT INTO `sensitive_word_data` VALUES ('滑顺');
INSERT INTO `sensitive_word_data` VALUES ('滚动');
INSERT INTO `sensitive_word_data` VALUES ('滚圆');
INSERT INTO `sensitive_word_data` VALUES ('滚圆大乳');
INSERT INTO `sensitive_word_data` VALUES ('滚烫');
INSERT INTO `sensitive_word_data` VALUES ('滚热');
INSERT INTO `sensitive_word_data` VALUES ('满胀');
INSERT INTO `sensitive_word_data` VALUES ('满足');
INSERT INTO `sensitive_word_data` VALUES ('满面潮红');
INSERT INTO `sensitive_word_data` VALUES ('滴出');
INSERT INTO `sensitive_word_data` VALUES ('滴虫性阴道炎');
INSERT INTO `sensitive_word_data` VALUES ('漆黑的阴毛');
INSERT INTO `sensitive_word_data` VALUES ('漫步丝');
INSERT INTO `sensitive_word_data` VALUES ('潘蓓蕾');
INSERT INTO `sensitive_word_data` VALUES ('潘贵玉');
INSERT INTO `sensitive_word_data` VALUES ('潘霞');
INSERT INTO `sensitive_word_data` VALUES ('潜欲');
INSERT INTO `sensitive_word_data` VALUES ('潮湿');
INSERT INTO `sensitive_word_data` VALUES ('潮红');
INSERT INTO `sensitive_word_data` VALUES ('澎胀');
INSERT INTO `sensitive_word_data` VALUES ('澤民');
INSERT INTO `sensitive_word_data` VALUES ('激光气 ');
INSERT INTO `sensitive_word_data` VALUES ('激光汽 ');
INSERT INTO `sensitive_word_data` VALUES ('激发性地区');
INSERT INTO `sensitive_word_data` VALUES ('激射');
INSERT INTO `sensitive_word_data` VALUES ('激情');
INSERT INTO `sensitive_word_data` VALUES ('激情交友 ');
INSERT INTO `sensitive_word_data` VALUES ('激情妹');
INSERT INTO `sensitive_word_data` VALUES ('激情小电影 ');
INSERT INTO `sensitive_word_data` VALUES ('激情炮');
INSERT INTO `sensitive_word_data` VALUES ('激情电');
INSERT INTO `sensitive_word_data` VALUES ('激情电影');
INSERT INTO `sensitive_word_data` VALUES ('激情电话 ');
INSERT INTO `sensitive_word_data` VALUES ('激情短');
INSERT INTO `sensitive_word_data` VALUES ('激情视频 ');
INSERT INTO `sensitive_word_data` VALUES ('激流中国 ');
INSERT INTO `sensitive_word_data` VALUES ('激烈的性交');
INSERT INTO `sensitive_word_data` VALUES ('激素');
INSERT INTO `sensitive_word_data` VALUES ('火柱');
INSERT INTO `sensitive_word_data` VALUES ('火热');
INSERT INTO `sensitive_word_data` VALUES ('火热鸡巴');
INSERT INTO `sensitive_word_data` VALUES ('火药制作简易炸弹');
INSERT INTO `sensitive_word_data` VALUES ('火药原理');
INSERT INTO `sensitive_word_data` VALUES ('火药的配方');
INSERT INTO `sensitive_word_data` VALUES ('火药配方');
INSERT INTO `sensitive_word_data` VALUES ('火车也疯');
INSERT INTO `sensitive_word_data` VALUES ('火辣');
INSERT INTO `sensitive_word_data` VALUES ('灭绝罪');
INSERT INTO `sensitive_word_data` VALUES ('灯草和');
INSERT INTO `sensitive_word_data` VALUES ('灵肉');
INSERT INTO `sensitive_word_data` VALUES ('灼热');
INSERT INTO `sensitive_word_data` VALUES ('炮的小蜜');
INSERT INTO `sensitive_word_data` VALUES ('炸学校');
INSERT INTO `sensitive_word_data` VALUES ('炸广州');
INSERT INTO `sensitive_word_data` VALUES ('炸弹制作');
INSERT INTO `sensitive_word_data` VALUES ('炸弹制作从入门');
INSERT INTO `sensitive_word_data` VALUES ('炸弹制作从入门到精');
INSERT INTO `sensitive_word_data` VALUES ('炸弹制作入门');
INSERT INTO `sensitive_word_data` VALUES ('炸弹制作手册 硫酸');
INSERT INTO `sensitive_word_data` VALUES ('炸弹教');
INSERT INTO `sensitive_word_data` VALUES ('炸弹遥控');
INSERT INTO `sensitive_word_data` VALUES ('炸弹配方');
INSERT INTO `sensitive_word_data` VALUES ('炸立交');
INSERT INTO `sensitive_word_data` VALUES ('炸药');
INSERT INTO `sensitive_word_data` VALUES ('炸药出售 ');
INSERT INTO `sensitive_word_data` VALUES ('炸药制');
INSERT INTO `sensitive_word_data` VALUES ('炸药制造生产工艺');
INSERT INTO `sensitive_word_data` VALUES ('炸药及其制造方法');
INSERT INTO `sensitive_word_data` VALUES ('炸药的制');
INSERT INTO `sensitive_word_data` VALUES ('炸药的制作');
INSERT INTO `sensitive_word_data` VALUES ('炸药的制作配方');
INSERT INTO `sensitive_word_data` VALUES ('炸药的成分');
INSERT INTO `sensitive_word_data` VALUES ('炸药的配方');
INSERT INTO `sensitive_word_data` VALUES ('炸药硝酸甘油制法');
INSERT INTO `sensitive_word_data` VALUES ('炸药配');
INSERT INTO `sensitive_word_data` VALUES ('炸药配方与制作');
INSERT INTO `sensitive_word_data` VALUES ('点数优惠');
INSERT INTO `sensitive_word_data` VALUES ('点金商务 ');
INSERT INTO `sensitive_word_data` VALUES ('炼大法');
INSERT INTO `sensitive_word_data` VALUES ('炽热');
INSERT INTO `sensitive_word_data` VALUES ('烟感器');
INSERT INTO `sensitive_word_data` VALUES ('烧公安局');
INSERT INTO `sensitive_word_data` VALUES ('烧瓶的');
INSERT INTO `sensitive_word_data` VALUES ('热武器QQ');
INSERT INTO `sensitive_word_data` VALUES ('焚身');
INSERT INTO `sensitive_word_data` VALUES ('焦国标');
INSERT INTO `sensitive_word_data` VALUES ('煞笔');
INSERT INTO `sensitive_word_data` VALUES ('煞逼');
INSERT INTO `sensitive_word_data` VALUES ('煽动不明');
INSERT INTO `sensitive_word_data` VALUES ('煽动群众');
INSERT INTO `sensitive_word_data` VALUES ('熟妇');
INSERT INTO `sensitive_word_data` VALUES ('燃烧剂的简易制作');
INSERT INTO `sensitive_word_data` VALUES ('燃烧剂的简易制备');
INSERT INTO `sensitive_word_data` VALUES ('燃烧弹');
INSERT INTO `sensitive_word_data` VALUES ('燃烧弹 制作');
INSERT INTO `sensitive_word_data` VALUES ('燃烧弹制作');
INSERT INTO `sensitive_word_data` VALUES ('燃烧弹的制作方 ');
INSERT INTO `sensitive_word_data` VALUES ('燃烧弹的制作方法');
INSERT INTO `sensitive_word_data` VALUES ('燃烧瓶制作');
INSERT INTO `sensitive_word_data` VALUES ('燃烧瓶的制作方法');
INSERT INTO `sensitive_word_data` VALUES ('爆你菊');
INSERT INTO `sensitive_word_data` VALUES ('爆发骚');
INSERT INTO `sensitive_word_data` VALUES ('爆射');
INSERT INTO `sensitive_word_data` VALUES ('爆炸物 ');
INSERT INTO `sensitive_word_data` VALUES ('爆破 ');
INSERT INTO `sensitive_word_data` VALUES ('爆破和杀伤燃烧弹');
INSERT INTO `sensitive_word_data` VALUES ('爆菊 ');
INSERT INTO `sensitive_word_data` VALUES ('爱他死');
INSERT INTO `sensitive_word_data` VALUES ('爱侣');
INSERT INTO `sensitive_word_data` VALUES ('爱女');
INSERT INTO `sensitive_word_data` VALUES ('爱慰');
INSERT INTO `sensitive_word_data` VALUES ('爱抚');
INSERT INTO `sensitive_word_data` VALUES ('爱欲');
INSERT INTO `sensitive_word_data` VALUES ('爱液');
INSERT INTO `sensitive_word_data` VALUES ('爱液横流');
INSERT INTO `sensitive_word_data` VALUES ('父母下岗儿下地');
INSERT INTO `sensitive_word_data` VALUES ('爸 爸');
INSERT INTO `sensitive_word_data` VALUES ('爸爸');
INSERT INTO `sensitive_word_data` VALUES ('爹爹');
INSERT INTO `sensitive_word_data` VALUES ('牌分析');
INSERT INTO `sensitive_word_data` VALUES ('牌技网');
INSERT INTO `sensitive_word_data` VALUES ('特上门');
INSERT INTO `sensitive_word_data` VALUES ('特工资');
INSERT INTO `sensitive_word_data` VALUES ('特码');
INSERT INTO `sensitive_word_data` VALUES ('犬交 ');
INSERT INTO `sensitive_word_data` VALUES ('狂吻');
INSERT INTO `sensitive_word_data` VALUES ('狂干');
INSERT INTO `sensitive_word_data` VALUES ('狂抽');
INSERT INTO `sensitive_word_data` VALUES ('狂捣');
INSERT INTO `sensitive_word_data` VALUES ('狂插');
INSERT INTO `sensitive_word_data` VALUES ('狂操');
INSERT INTO `sensitive_word_data` VALUES ('狂暴');
INSERT INTO `sensitive_word_data` VALUES ('狂泄');
INSERT INTO `sensitive_word_data` VALUES ('狂热');
INSERT INTO `sensitive_word_data` VALUES ('狂舐');
INSERT INTO `sensitive_word_data` VALUES ('狗交');
INSERT INTO `sensitive_word_data` VALUES ('狗产蛋');
INSERT INTO `sensitive_word_data` VALUES ('狗友 ');
INSERT INTO `sensitive_word_data` VALUES ('狗娘养');
INSERT INTO `sensitive_word_data` VALUES ('狗屁专家');
INSERT INTO `sensitive_word_data` VALUES ('狗操');
INSERT INTO `sensitive_word_data` VALUES ('狗日的');
INSERT INTO `sensitive_word_data` VALUES ('狗杂种');
INSERT INTO `sensitive_word_data` VALUES ('狗爬');
INSERT INTO `sensitive_word_data` VALUES ('狗粮');
INSERT INTO `sensitive_word_data` VALUES ('狗草');
INSERT INTO `sensitive_word_data` VALUES ('狠干');
INSERT INTO `sensitive_word_data` VALUES ('狠插');
INSERT INTO `sensitive_word_data` VALUES ('狩猎器材弓弩');
INSERT INTO `sensitive_word_data` VALUES ('狩猎器材弓弩专卖');
INSERT INTO `sensitive_word_data` VALUES ('狩猎弓弩专卖');
INSERT INTO `sensitive_word_data` VALUES ('狩猎弓弩专卖网');
INSERT INTO `sensitive_word_data` VALUES ('狩猎弓弩麻醉箭');
INSERT INTO `sensitive_word_data` VALUES ('独生女');
INSERT INTO `sensitive_word_data` VALUES ('独立台湾');
INSERT INTO `sensitive_word_data` VALUES ('狮子旗');
INSERT INTO `sensitive_word_data` VALUES ('狼全部跪');
INSERT INTO `sensitive_word_data` VALUES ('猎好帮手');
INSERT INTO `sensitive_word_data` VALUES ('猎枪 ');
INSERT INTO `sensitive_word_data` VALUES ('猎枪|真枪');
INSERT INTO `sensitive_word_data` VALUES ('猎枪专卖店');
INSERT INTO `sensitive_word_data` VALUES ('猎枪仿真枪QQ');
INSERT INTO `sensitive_word_data` VALUES ('猎枪出售/枪');
INSERT INTO `sensitive_word_data` VALUES ('猎枪哪里有卖');
INSERT INTO `sensitive_word_data` VALUES ('猎枪子弹底火专卖');
INSERT INTO `sensitive_word_data` VALUES ('猎枪手枪买卖');
INSERT INTO `sensitive_word_data` VALUES ('猎枪销');
INSERT INTO `sensitive_word_data` VALUES ('猎枪销售手枪销售气枪');
INSERT INTO `sensitive_word_data` VALUES ('猎枪麻醉枪钢珠枪');
INSERT INTO `sensitive_word_data` VALUES ('猎槍');
INSERT INTO `sensitive_word_data` VALUES ('猛冲');
INSERT INTO `sensitive_word_data` VALUES ('猛刺');
INSERT INTO `sensitive_word_data` VALUES ('猛喘');
INSERT INTO `sensitive_word_data` VALUES ('猛干');
INSERT INTO `sensitive_word_data` VALUES ('猛抽');
INSERT INTO `sensitive_word_data` VALUES ('猛挺');
INSERT INTO `sensitive_word_data` VALUES ('猛插');
INSERT INTO `sensitive_word_data` VALUES ('猛搅');
INSERT INTO `sensitive_word_data` VALUES ('猛撞');
INSERT INTO `sensitive_word_data` VALUES ('猛操');
INSERT INTO `sensitive_word_data` VALUES ('猛男');
INSERT INTO `sensitive_word_data` VALUES ('猛舔');
INSERT INTO `sensitive_word_data` VALUES ('猛颤');
INSERT INTO `sensitive_word_data` VALUES ('猫则东');
INSERT INTO `sensitive_word_data` VALUES ('猫泽东');
INSERT INTO `sensitive_word_data` VALUES ('猫眼工具');
INSERT INTO `sensitive_word_data` VALUES ('猫贼洞');
INSERT INTO `sensitive_word_data` VALUES ('獵槍');
INSERT INTO `sensitive_word_data` VALUES ('玉 腿');
INSERT INTO `sensitive_word_data` VALUES ('玉乳');
INSERT INTO `sensitive_word_data` VALUES ('玉体');
INSERT INTO `sensitive_word_data` VALUES ('玉卿');
INSERT INTO `sensitive_word_data` VALUES ('玉娘');
INSERT INTO `sensitive_word_data` VALUES ('玉娟');
INSERT INTO `sensitive_word_data` VALUES ('玉峰');
INSERT INTO `sensitive_word_data` VALUES ('玉户');
INSERT INTO `sensitive_word_data` VALUES ('玉房');
INSERT INTO `sensitive_word_data` VALUES ('玉指');
INSERT INTO `sensitive_word_data` VALUES ('玉柱');
INSERT INTO `sensitive_word_data` VALUES ('玉棍');
INSERT INTO `sensitive_word_data` VALUES ('玉棒');
INSERT INTO `sensitive_word_data` VALUES ('玉洞');
INSERT INTO `sensitive_word_data` VALUES ('玉浆');
INSERT INTO `sensitive_word_data` VALUES ('玉液');
INSERT INTO `sensitive_word_data` VALUES ('玉液般');
INSERT INTO `sensitive_word_data` VALUES ('玉穴');
INSERT INTO `sensitive_word_data` VALUES ('玉缝');
INSERT INTO `sensitive_word_data` VALUES ('玉肌');
INSERT INTO `sensitive_word_data` VALUES ('玉肤');
INSERT INTO `sensitive_word_data` VALUES ('玉背');
INSERT INTO `sensitive_word_data` VALUES ('玉脚');
INSERT INTO `sensitive_word_data` VALUES ('玉腿');
INSERT INTO `sensitive_word_data` VALUES ('玉臀');
INSERT INTO `sensitive_word_data` VALUES ('玉臂');
INSERT INTO `sensitive_word_data` VALUES ('玉茎');
INSERT INTO `sensitive_word_data` VALUES ('玉蒲团');
INSERT INTO `sensitive_word_data` VALUES ('玉蕊');
INSERT INTO `sensitive_word_data` VALUES ('玉面');
INSERT INTO `sensitive_word_data` VALUES ('玉颈');
INSERT INTO `sensitive_word_data` VALUES ('玉麈');
INSERT INTO `sensitive_word_data` VALUES ('玉齿');
INSERT INTO `sensitive_word_data` VALUES ('王东');
INSERT INTO `sensitive_word_data` VALUES ('王东明');
INSERT INTO `sensitive_word_data` VALUES ('王东江');
INSERT INTO `sensitive_word_data` VALUES ('王丹');
INSERT INTO `sensitive_word_data` VALUES ('王乐泉 ');
INSERT INTO `sensitive_word_data` VALUES ('王云坤');
INSERT INTO `sensitive_word_data` VALUES ('王云龙');
INSERT INTO `sensitive_word_data` VALUES ('王以铭');
INSERT INTO `sensitive_word_data` VALUES ('王传平');
INSERT INTO `sensitive_word_data` VALUES ('王佐书');
INSERT INTO `sensitive_word_data` VALUES ('王兆国 ');
INSERT INTO `sensitive_word_data` VALUES ('王先琼');
INSERT INTO `sensitive_word_data` VALUES ('王光谦');
INSERT INTO `sensitive_word_data` VALUES ('王克勤');
INSERT INTO `sensitive_word_data` VALUES ('王全书');
INSERT INTO `sensitive_word_data` VALUES ('王冶坪');
INSERT INTO `sensitive_word_data` VALUES ('王刚 ');
INSERT INTO `sensitive_word_data` VALUES ('王力平');
INSERT INTO `sensitive_word_data` VALUES ('王占');
INSERT INTO `sensitive_word_data` VALUES ('王国发');
INSERT INTO `sensitive_word_data` VALUES ('王大中');
INSERT INTO `sensitive_word_data` VALUES ('王太华');
INSERT INTO `sensitive_word_data` VALUES ('王太岚');
INSERT INTO `sensitive_word_data` VALUES ('王奉友');
INSERT INTO `sensitive_word_data` VALUES ('王学军');
INSERT INTO `sensitive_word_data` VALUES ('王学永');
INSERT INTO `sensitive_word_data` VALUES ('王学萍');
INSERT INTO `sensitive_word_data` VALUES ('王宁生');
INSERT INTO `sensitive_word_data` VALUES ('王宋大');
INSERT INTO `sensitive_word_data` VALUES ('王少阶');
INSERT INTO `sensitive_word_data` VALUES ('王岐山');
INSERT INTO `sensitive_word_data` VALUES ('王巨禄');
INSERT INTO `sensitive_word_data` VALUES ('王广宪');
INSERT INTO `sensitive_word_data` VALUES ('王建军');
INSERT INTO `sensitive_word_data` VALUES ('王建民');
INSERT INTO `sensitive_word_data` VALUES ('王忍之');
INSERT INTO `sensitive_word_data` VALUES ('王忠禹');
INSERT INTO `sensitive_word_data` VALUES ('王怀远');
INSERT INTO `sensitive_word_data` VALUES ('王恒丰');
INSERT INTO `sensitive_word_data` VALUES ('王振华');
INSERT INTO `sensitive_word_data` VALUES ('王政 ');
INSERT INTO `sensitive_word_data` VALUES ('王旭东');
INSERT INTO `sensitive_word_data` VALUES ('王明明');
INSERT INTO `sensitive_word_data` VALUES ('王梅祥');
INSERT INTO `sensitive_word_data` VALUES ('王梦奎');
INSERT INTO `sensitive_word_data` VALUES ('王永炎');
INSERT INTO `sensitive_word_data` VALUES ('王沪宁');
INSERT INTO `sensitive_word_data` VALUES ('王洛林');
INSERT INTO `sensitive_word_data` VALUES ('王涛');
INSERT INTO `sensitive_word_data` VALUES ('王珉');
INSERT INTO `sensitive_word_data` VALUES ('王生铁');
INSERT INTO `sensitive_word_data` VALUES ('王益案');
INSERT INTO `sensitive_word_data` VALUES ('王祖训');
INSERT INTO `sensitive_word_data` VALUES ('王立军');
INSERT INTO `sensitive_word_data` VALUES ('王立平');
INSERT INTO `sensitive_word_data` VALUES ('王维城');
INSERT INTO `sensitive_word_data` VALUES ('王维民');
INSERT INTO `sensitive_word_data` VALUES ('王美茹');
INSERT INTO `sensitive_word_data` VALUES ('王耀华');
INSERT INTO `sensitive_word_data` VALUES ('王胜俊');
INSERT INTO `sensitive_word_data` VALUES ('王良漙');
INSERT INTO `sensitive_word_data` VALUES ('王英凡');
INSERT INTO `sensitive_word_data` VALUES ('王茂林');
INSERT INTO `sensitive_word_data` VALUES ('王茂润');
INSERT INTO `sensitive_word_data` VALUES ('王蒙');
INSERT INTO `sensitive_word_data` VALUES ('王选');
INSERT INTO `sensitive_word_data` VALUES ('王通智');
INSERT INTO `sensitive_word_data` VALUES ('王金山');
INSERT INTO `sensitive_word_data` VALUES ('王钦敏');
INSERT INTO `sensitive_word_data` VALUES ('王鸿举');
INSERT INTO `sensitive_word_data` VALUES ('王鹤龄');
INSERT INTO `sensitive_word_data` VALUES ('环境');
INSERT INTO `sensitive_word_data` VALUES ('环球证件');
INSERT INTO `sensitive_word_data` VALUES ('现大地震');
INSERT INTO `sensitive_word_data` VALUES ('现金投注');
INSERT INTO `sensitive_word_data` VALUES ('珍珠状阴茎丘疹');
INSERT INTO `sensitive_word_data` VALUES ('珐(工力)学 ');
INSERT INTO `sensitive_word_data` VALUES ('理做帐报');
INSERT INTO `sensitive_word_data` VALUES ('理各种证');
INSERT INTO `sensitive_word_data` VALUES ('理想');
INSERT INTO `sensitive_word_data` VALUES ('理是影帝');
INSERT INTO `sensitive_word_data` VALUES ('理证件');
INSERT INTO `sensitive_word_data` VALUES ('琼花问');
INSERT INTO `sensitive_word_data` VALUES ('瓦哈甫·苏来曼');
INSERT INTO `sensitive_word_data` VALUES ('瓦斯手');
INSERT INTO `sensitive_word_data` VALUES ('瓮安');
INSERT INTO `sensitive_word_data` VALUES ('瓮安事件');
INSERT INTO `sensitive_word_data` VALUES ('甘子钊');
INSERT INTO `sensitive_word_data` VALUES ('甘宇平');
INSERT INTO `sensitive_word_data` VALUES ('生命树的分叉 ');
INSERT INTO `sensitive_word_data` VALUES ('生意宝 ');
INSERT INTO `sensitive_word_data` VALUES ('生肖中特');
INSERT INTO `sensitive_word_data` VALUES ('生被砍');
INSERT INTO `sensitive_word_data` VALUES ('生踩踏');
INSERT INTO `sensitive_word_data` VALUES ('用力');
INSERT INTO `sensitive_word_data` VALUES ('用力一顶');
INSERT INTO `sensitive_word_data` VALUES ('用手枪');
INSERT INTO `sensitive_word_data` VALUES ('用火药混合塑料');
INSERT INTO `sensitive_word_data` VALUES ('用药');
INSERT INTO `sensitive_word_data` VALUES ('田停工');
INSERT INTO `sensitive_word_data` VALUES ('田岚');
INSERT INTO `sensitive_word_data` VALUES ('田成平');
INSERT INTO `sensitive_word_data` VALUES ('田期玉');
INSERT INTO `sensitive_word_data` VALUES ('田玉科');
INSERT INTO `sensitive_word_data` VALUES ('田田桑');
INSERT INTO `sensitive_word_data` VALUES ('田罢工');
INSERT INTO `sensitive_word_data` VALUES ('由喜贵');
INSERT INTO `sensitive_word_data` VALUES ('甲基安非他明');
INSERT INTO `sensitive_word_data` VALUES ('甲流了');
INSERT INTO `sensitive_word_data` VALUES ('甲睾酮');
INSERT INTO `sensitive_word_data` VALUES ('甲虫跳');
INSERT INTO `sensitive_word_data` VALUES ('电信路');
INSERT INTO `sensitive_word_data` VALUES ('电击枪 ');
INSERT INTO `sensitive_word_data` VALUES ('电子狗导航手机 ');
INSERT INTO `sensitive_word_data` VALUES ('电狗');
INSERT INTO `sensitive_word_data` VALUES ('电警棒');
INSERT INTO `sensitive_word_data` VALUES ('电话交友 ');
INSERT INTO `sensitive_word_data` VALUES ('电话监');
INSERT INTO `sensitive_word_data` VALUES ('电话追杀系统 ');
INSERT INTO `sensitive_word_data` VALUES ('电鸡');
INSERT INTO `sensitive_word_data` VALUES ('男上式');
INSERT INTO `sensitive_word_data` VALUES ('男公关');
INSERT INTO `sensitive_word_data` VALUES ('男女');
INSERT INTO `sensitive_word_data` VALUES ('男奴 ');
INSERT INTO `sensitive_word_data` VALUES ('男子');
INSERT INTO `sensitive_word_data` VALUES ('男性');
INSERT INTO `sensitive_word_data` VALUES ('男性不育症');
INSERT INTO `sensitive_word_data` VALUES ('男性外生殖器官');
INSERT INTO `sensitive_word_data` VALUES ('男性尿道唇');
INSERT INTO `sensitive_word_data` VALUES ('男性生殖器');
INSERT INTO `sensitive_word_data` VALUES ('男方');
INSERT INTO `sensitive_word_data` VALUES ('男方膝立位');
INSERT INTO `sensitive_word_data` VALUES ('男根');
INSERT INTO `sensitive_word_data` VALUES ('男欢女爱');
INSERT INTO `sensitive_word_data` VALUES ('男精女血');
INSERT INTO `sensitive_word_data` VALUES ('男茎');
INSERT INTO `sensitive_word_data` VALUES ('男跪女後');
INSERT INTO `sensitive_word_data` VALUES ('甸果敢');
INSERT INTO `sensitive_word_data` VALUES ('疆独');
INSERT INTO `sensitive_word_data` VALUES ('疆獨');
INSERT INTO `sensitive_word_data` VALUES ('疲软');
INSERT INTO `sensitive_word_data` VALUES ('疾病业债说 ');
INSERT INTO `sensitive_word_data` VALUES ('病毒性睾丸炎');
INSERT INTO `sensitive_word_data` VALUES ('症腐');
INSERT INTO `sensitive_word_data` VALUES ('痉挛');
INSERT INTO `sensitive_word_data` VALUES ('痉脔');
INSERT INTO `sensitive_word_data` VALUES ('痔内静脉丛');
INSERT INTO `sensitive_word_data` VALUES ('痔外静脉丛');
INSERT INTO `sensitive_word_data` VALUES ('瘟假饱');
INSERT INTO `sensitive_word_data` VALUES ('瘟加饱');
INSERT INTO `sensitive_word_data` VALUES ('瘟家宝 ');
INSERT INTO `sensitive_word_data` VALUES ('瘟疫爆发');
INSERT INTO `sensitive_word_data` VALUES ('登床');
INSERT INTO `sensitive_word_data` VALUES ('發票');
INSERT INTO `sensitive_word_data` VALUES ('白克明');
INSERT INTO `sensitive_word_data` VALUES ('白塞氏病');
INSERT INTO `sensitive_word_data` VALUES ('白嫩');
INSERT INTO `sensitive_word_data` VALUES ('白小姐');
INSERT INTO `sensitive_word_data` VALUES ('白带恶臭');
INSERT INTO `sensitive_word_data` VALUES ('白恩培');
INSERT INTO `sensitive_word_data` VALUES ('白春礼');
INSERT INTO `sensitive_word_data` VALUES ('白浊');
INSERT INTO `sensitive_word_data` VALUES ('白痴');
INSERT INTO `sensitive_word_data` VALUES ('白立忱');
INSERT INTO `sensitive_word_data` VALUES ('白里透红');
INSERT INTO `sensitive_word_data` VALUES ('白黄牙签');
INSERT INTO `sensitive_word_data` VALUES ('百乐二呓 ');
INSERT INTO `sensitive_word_data` VALUES ('百家乐');
INSERT INTO `sensitive_word_data` VALUES ('的同修');
INSERT INTO `sensitive_word_data` VALUES ('皇冠投注');
INSERT INTO `sensitive_word_data` VALUES ('皇家轮盘 ');
INSERT INTO `sensitive_word_data` VALUES ('皮角');
INSERT INTO `sensitive_word_data` VALUES ('盆腔');
INSERT INTO `sensitive_word_data` VALUES ('盆腔放线菌病');
INSERT INTO `sensitive_word_data` VALUES ('盆腔炎');
INSERT INTO `sensitive_word_data` VALUES ('盆腔腹膜');
INSERT INTO `sensitive_word_data` VALUES ('盆腔腹膜炎');
INSERT INTO `sensitive_word_data` VALUES ('盆膈下筋膜');
INSERT INTO `sensitive_word_data` VALUES ('盈满');
INSERT INTO `sensitive_word_data` VALUES ('益关注组');
INSERT INTO `sensitive_word_data` VALUES ('益受贿');
INSERT INTO `sensitive_word_data` VALUES ('盐酸曲');
INSERT INTO `sensitive_word_data` VALUES ('监听器');
INSERT INTO `sensitive_word_data` VALUES ('监听头');
INSERT INTO `sensitive_word_data` VALUES ('监听王');
INSERT INTO `sensitive_word_data` VALUES ('盘口|3');
INSERT INTO `sensitive_word_data` VALUES ('盘古');
INSERT INTO `sensitive_word_data` VALUES ('盘古乐队 ');
INSERT INTO `sensitive_word_data` VALUES ('盛华仁');
INSERT INTO `sensitive_word_data` VALUES ('盛行在舞');
INSERT INTO `sensitive_word_data` VALUES ('直刀匕首直销网');
INSERT INTO `sensitive_word_data` VALUES ('直挺挺');
INSERT INTO `sensitive_word_data` VALUES ('直插');
INSERT INTO `sensitive_word_data` VALUES ('直精小管');
INSERT INTO `sensitive_word_data` VALUES ('直肠');
INSERT INTO `sensitive_word_data` VALUES ('直肠壶腹');
INSERT INTO `sensitive_word_data` VALUES ('直肠柱');
INSERT INTO `sensitive_word_data` VALUES ('直肠瓣');
INSERT INTO `sensitive_word_data` VALUES ('直肠阴道瘘');
INSERT INTO `sensitive_word_data` VALUES ('相自首');
INSERT INTO `sensitive_word_data` VALUES ('看JJ ');
INSERT INTO `sensitive_word_data` VALUES ('真他妈');
INSERT INTO `sensitive_word_data` VALUES ('真善忍');
INSERT INTO `sensitive_word_data` VALUES ('真实文凭');
INSERT INTO `sensitive_word_data` VALUES ('真实资格');
INSERT INTO `sensitive_word_data` VALUES ('真琴');
INSERT INTO `sensitive_word_data` VALUES ('真钱投注');
INSERT INTO `sensitive_word_data` VALUES ('真钱斗地');
INSERT INTO `sensitive_word_data` VALUES ('着护士的胸');
INSERT INTO `sensitive_word_data` VALUES ('着涛哥');
INSERT INTO `sensitive_word_data` VALUES ('睾丸');
INSERT INTO `sensitive_word_data` VALUES ('睾丸动脉');
INSERT INTO `sensitive_word_data` VALUES ('睾丸固有鞘膜');
INSERT INTO `sensitive_word_data` VALUES ('睾丸坠痛');
INSERT INTO `sensitive_word_data` VALUES ('睾丸增生');
INSERT INTO `sensitive_word_data` VALUES ('睾丸小叶');
INSERT INTO `sensitive_word_data` VALUES ('睾丸小隔');
INSERT INTO `sensitive_word_data` VALUES ('睾丸损伤');
INSERT INTO `sensitive_word_data` VALUES ('睾丸液');
INSERT INTO `sensitive_word_data` VALUES ('睾丸激素');
INSERT INTO `sensitive_word_data` VALUES ('睾丸炎');
INSERT INTO `sensitive_word_data` VALUES ('睾丸生精功能障碍');
INSERT INTO `sensitive_word_data` VALUES ('睾丸移植');
INSERT INTO `sensitive_word_data` VALUES ('睾丸精索鞘膜');
INSERT INTO `sensitive_word_data` VALUES ('睾丸系带');
INSERT INTO `sensitive_word_data` VALUES ('睾丸系膜');
INSERT INTO `sensitive_word_data` VALUES ('睾丸素');
INSERT INTO `sensitive_word_data` VALUES ('睾丸结核');
INSERT INTO `sensitive_word_data` VALUES ('睾丸网');
INSERT INTO `sensitive_word_data` VALUES ('睾丸酮');
INSERT INTO `sensitive_word_data` VALUES ('睾丸间质');
INSERT INTO `sensitive_word_data` VALUES ('睾丸鞘膜');
INSERT INTO `sensitive_word_data` VALUES ('睾酮');
INSERT INTO `sensitive_word_data` VALUES ('矗立');
INSERT INTO `sensitive_word_data` VALUES ('短信截');
INSERT INTO `sensitive_word_data` VALUES ('短信投票业务 ');
INSERT INTO `sensitive_word_data` VALUES ('短信猫');
INSERT INTO `sensitive_word_data` VALUES ('短信群发 ');
INSERT INTO `sensitive_word_data` VALUES ('短粗');
INSERT INTO `sensitive_word_data` VALUES ('石万鹏');
INSERT INTO `sensitive_word_data` VALUES ('石四箴');
INSERT INTO `sensitive_word_data` VALUES ('石宗源');
INSERT INTO `sensitive_word_data` VALUES ('石广生');
INSERT INTO `sensitive_word_data` VALUES ('石秀诗');
INSERT INTO `sensitive_word_data` VALUES ('矿难不公');
INSERT INTO `sensitive_word_data` VALUES ('砍伤儿');
INSERT INTO `sensitive_word_data` VALUES ('砍刀精品折刀专卖');
INSERT INTO `sensitive_word_data` VALUES ('砍杀幼');
INSERT INTO `sensitive_word_data` VALUES ('破产');
INSERT INTO `sensitive_word_data` VALUES ('破坏');
INSERT INTO `sensitive_word_data` VALUES ('破处');
INSERT INTO `sensitive_word_data` VALUES ('破瓜');
INSERT INTO `sensitive_word_data` VALUES ('破贞');
INSERT INTO `sensitive_word_data` VALUES ('破身');
INSERT INTO `sensitive_word_data` VALUES ('硝化甘');
INSERT INTO `sensitive_word_data` VALUES ('硝酸甘油');
INSERT INTO `sensitive_word_data` VALUES ('硝酸甘油制作');
INSERT INTO `sensitive_word_data` VALUES ('硝酸甘油炸弹制作');
INSERT INTO `sensitive_word_data` VALUES ('硝酸甘油炸弹制作方法');
INSERT INTO `sensitive_word_data` VALUES ('硝酸甘油炸药制作');
INSERT INTO `sensitive_word_data` VALUES ('硝酸铵的简便制取方法');
INSERT INTO `sensitive_word_data` VALUES ('硝铵炸药配方');
INSERT INTO `sensitive_word_data` VALUES ('硬下疳');
INSERT INTO `sensitive_word_data` VALUES ('硬挺');
INSERT INTO `sensitive_word_data` VALUES ('硬梆梆');
INSERT INTO `sensitive_word_data` VALUES ('硬涨');
INSERT INTO `sensitive_word_data` VALUES ('硬热');
INSERT INTO `sensitive_word_data` VALUES ('硬物');
INSERT INTO `sensitive_word_data` VALUES ('硬硬');
INSERT INTO `sensitive_word_data` VALUES ('硬立');
INSERT INTO `sensitive_word_data` VALUES ('硬绑绑');
INSERT INTO `sensitive_word_data` VALUES ('硬胀');
INSERT INTO `sensitive_word_data` VALUES ('硬茎');
INSERT INTO `sensitive_word_data` VALUES ('硬邦邦');
INSERT INTO `sensitive_word_data` VALUES ('碰触');
INSERT INTO `sensitive_word_data` VALUES ('磅解码器');
INSERT INTO `sensitive_word_data` VALUES ('磅遥控器');
INSERT INTO `sensitive_word_data` VALUES ('磕彰');
INSERT INTO `sensitive_word_data` VALUES ('磨弄');
INSERT INTO `sensitive_word_data` VALUES ('磨搓');
INSERT INTO `sensitive_word_data` VALUES ('磨搽');
INSERT INTO `sensitive_word_data` VALUES ('磨擦');
INSERT INTO `sensitive_word_data` VALUES ('磨穴');
INSERT INTO `sensitive_word_data` VALUES ('社会黑暗');
INSERT INTO `sensitive_word_data` VALUES ('神七假');
INSERT INTO `sensitive_word_data` VALUES ('神州忏悔录 ');
INSERT INTO `sensitive_word_data` VALUES ('神的教会');
INSERT INTO `sensitive_word_data` VALUES ('神韵艺术');
INSERT INTO `sensitive_word_data` VALUES ('禁网禁片 ');
INSERT INTO `sensitive_word_data` VALUES ('禅密功');
INSERT INTO `sensitive_word_data` VALUES ('福娃的預');
INSERT INTO `sensitive_word_data` VALUES ('福娃頭上');
INSERT INTO `sensitive_word_data` VALUES ('福尔马林');
INSERT INTO `sensitive_word_data` VALUES ('福音会');
INSERT INTO `sensitive_word_data` VALUES ('福香巴');
INSERT INTO `sensitive_word_data` VALUES ('禽流感了');
INSERT INTO `sensitive_word_data` VALUES ('私家侦探服务 ');
INSERT INTO `sensitive_word_data` VALUES ('私房写真');
INSERT INTO `sensitive_word_data` VALUES ('秃鹰气枪货到付款');
INSERT INTO `sensitive_word_data` VALUES ('秃鹰汽');
INSERT INTO `sensitive_word_data` VALUES ('种公务员');
INSERT INTO `sensitive_word_data` VALUES ('种学历证');
INSERT INTO `sensitive_word_data` VALUES ('秘唇');
INSERT INTO `sensitive_word_data` VALUES ('秘处');
INSERT INTO `sensitive_word_data` VALUES ('秘洞');
INSERT INTO `sensitive_word_data` VALUES ('秘穴');
INSERT INTO `sensitive_word_data` VALUES ('秘缝');
INSERT INTO `sensitive_word_data` VALUES ('秘肉');
INSERT INTO `sensitive_word_data` VALUES ('秘贝');
INSERT INTO `sensitive_word_data` VALUES ('秘部');
INSERT INTO `sensitive_word_data` VALUES ('秦玉琴');
INSERT INTO `sensitive_word_data` VALUES ('秦高潮');
INSERT INTO `sensitive_word_data` VALUES ('积克馆');
INSERT INTO `sensitive_word_data` VALUES ('秽物');
INSERT INTO `sensitive_word_data` VALUES ('秽疮');
INSERT INTO `sensitive_word_data` VALUES ('程安东');
INSERT INTO `sensitive_word_data` VALUES ('程津培');
INSERT INTO `sensitive_word_data` VALUES ('程誌青');
INSERT INTO `sensitive_word_data` VALUES ('程贻举');
INSERT INTO `sensitive_word_data` VALUES ('稚嫩');
INSERT INTO `sensitive_word_data` VALUES ('穴海');
INSERT INTO `sensitive_word_data` VALUES ('究生答案');
INSERT INTO `sensitive_word_data` VALUES ('空和雅典');
INSERT INTO `sensitive_word_data` VALUES ('穿插');
INSERT INTO `sensitive_word_data` VALUES ('穿透仪器');
INSERT INTO `sensitive_word_data` VALUES ('突破封锁');
INSERT INTO `sensitive_word_data` VALUES ('突破网路');
INSERT INTO `sensitive_word_data` VALUES ('窃听器');
INSERT INTO `sensitive_word_data` VALUES ('窄窄');
INSERT INTO `sensitive_word_data` VALUES ('窜动');
INSERT INTO `sensitive_word_data` VALUES ('窥探');
INSERT INTO `sensitive_word_data` VALUES ('窦瑞华');
INSERT INTO `sensitive_word_data` VALUES ('站位性交');
INSERT INTO `sensitive_word_data` VALUES ('站立');
INSERT INTO `sensitive_word_data` VALUES ('站立式性交');
INSERT INTO `sensitive_word_data` VALUES ('章祥荪');
INSERT INTO `sensitive_word_data` VALUES ('章虹');
INSERT INTO `sensitive_word_data` VALUES ('童傅');
INSERT INTO `sensitive_word_data` VALUES ('符廷贵');
INSERT INTO `sensitive_word_data` VALUES ('第一夫人');
INSERT INTO `sensitive_word_data` VALUES ('第二性征');
INSERT INTO `sensitive_word_data` VALUES ('第五性病');
INSERT INTO `sensitive_word_data` VALUES ('第四性病');
INSERT INTO `sensitive_word_data` VALUES ('等人手术');
INSERT INTO `sensitive_word_data` VALUES ('等人是老');
INSERT INTO `sensitive_word_data` VALUES ('等人老百');
INSERT INTO `sensitive_word_data` VALUES ('等屁民');
INSERT INTO `sensitive_word_data` VALUES ('等级證');
INSERT INTO `sensitive_word_data` VALUES ('筋肉');
INSERT INTO `sensitive_word_data` VALUES ('答案包');
INSERT INTO `sensitive_word_data` VALUES ('答案提供');
INSERT INTO `sensitive_word_data` VALUES ('策没有不');
INSERT INTO `sensitive_word_data` VALUES ('简易原子弹教程');
INSERT INTO `sensitive_word_data` VALUES ('简易大威力炸弹');
INSERT INTO `sensitive_word_data` VALUES ('简易导弹制作');
INSERT INTO `sensitive_word_data` VALUES ('简易核弹头的制造');
INSERT INTO `sensitive_word_data` VALUES ('简易炸');
INSERT INTO `sensitive_word_data` VALUES ('简易炸弹');
INSERT INTO `sensitive_word_data` VALUES ('简易炸药');
INSERT INTO `sensitive_word_data` VALUES ('简易燃烧弹');
INSERT INTO `sensitive_word_data` VALUES ('箍住');
INSERT INTO `sensitive_word_data` VALUES ('类准确答');
INSERT INTO `sensitive_word_data` VALUES ('类菌质体');
INSERT INTO `sensitive_word_data` VALUES ('粉 嫩');
INSERT INTO `sensitive_word_data` VALUES ('粉 颊');
INSERT INTO `sensitive_word_data` VALUES ('粉乳');
INSERT INTO `sensitive_word_data` VALUES ('粉头');
INSERT INTO `sensitive_word_data` VALUES ('粉嫩');
INSERT INTO `sensitive_word_data` VALUES ('粉嫩小洞');
INSERT INTO `sensitive_word_data` VALUES ('粉汗微出');
INSERT INTO `sensitive_word_data` VALUES ('粉白');
INSERT INTO `sensitive_word_data` VALUES ('粉红');
INSERT INTO `sensitive_word_data` VALUES ('粉红阴唇');
INSERT INTO `sensitive_word_data` VALUES ('粉腮');
INSERT INTO `sensitive_word_data` VALUES ('粉腰');
INSERT INTO `sensitive_word_data` VALUES ('粉腿');
INSERT INTO `sensitive_word_data` VALUES ('粉臀');
INSERT INTO `sensitive_word_data` VALUES ('粉臂');
INSERT INTO `sensitive_word_data` VALUES ('粉舌');
INSERT INTO `sensitive_word_data` VALUES ('粉颊');
INSERT INTO `sensitive_word_data` VALUES ('粉额');
INSERT INTO `sensitive_word_data` VALUES ('粗壮');
INSERT INTO `sensitive_word_data` VALUES ('粗大');
INSERT INTO `sensitive_word_data` VALUES ('粗大的玩意儿');
INSERT INTO `sensitive_word_data` VALUES ('粗暴');
INSERT INTO `sensitive_word_data` VALUES ('粗涨');
INSERT INTO `sensitive_word_data` VALUES ('粗硬');
INSERT INTO `sensitive_word_data` VALUES ('粗粗');
INSERT INTO `sensitive_word_data` VALUES ('粗红');
INSERT INTO `sensitive_word_data` VALUES ('粗长');
INSERT INTO `sensitive_word_data` VALUES ('粗鲁');
INSERT INTO `sensitive_word_data` VALUES ('粗黑');
INSERT INTO `sensitive_word_data` VALUES ('粘乎乎');
INSERT INTO `sensitive_word_data` VALUES ('粘液');
INSERT INTO `sensitive_word_data` VALUES ('粘滑');
INSERT INTO `sensitive_word_data` VALUES ('粘稠');
INSERT INTO `sensitive_word_data` VALUES ('粘膜');
INSERT INTO `sensitive_word_data` VALUES ('粟戎生');
INSERT INTO `sensitive_word_data` VALUES ('粮荒');
INSERT INTO `sensitive_word_data` VALUES ('精元');
INSERT INTO `sensitive_word_data` VALUES ('精关失固');
INSERT INTO `sensitive_word_data` VALUES ('精原');
INSERT INTO `sensitive_word_data` VALUES ('精原核');
INSERT INTO `sensitive_word_data` VALUES ('精原细胞');
INSERT INTO `sensitive_word_data` VALUES ('精囊');
INSERT INTO `sensitive_word_data` VALUES ('精囊囊肿');
INSERT INTO `sensitive_word_data` VALUES ('精囊炎');
INSERT INTO `sensitive_word_data` VALUES ('精囊腺');
INSERT INTO `sensitive_word_data` VALUES ('精囊良性肿瘤');
INSERT INTO `sensitive_word_data` VALUES ('精失固摄');
INSERT INTO `sensitive_word_data` VALUES ('精子');
INSERT INTO `sensitive_word_data` VALUES ('精子射在');
INSERT INTO `sensitive_word_data` VALUES ('精子成活率');
INSERT INTO `sensitive_word_data` VALUES ('精子抗原');
INSERT INTO `sensitive_word_data` VALUES ('精子膜');
INSERT INTO `sensitive_word_data` VALUES ('精子鞭毛');
INSERT INTO `sensitive_word_data` VALUES ('精巢');
INSERT INTO `sensitive_word_data` VALUES ('精母');
INSERT INTO `sensitive_word_data` VALUES ('精母细胞');
INSERT INTO `sensitive_word_data` VALUES ('精水');
INSERT INTO `sensitive_word_data` VALUES ('精浆');
INSERT INTO `sensitive_word_data` VALUES ('精液');
INSERT INTO `sensitive_word_data` VALUES ('精满自溢');
INSERT INTO `sensitive_word_data` VALUES ('精神药品 ');
INSERT INTO `sensitive_word_data` VALUES ('精种');
INSERT INTO `sensitive_word_data` VALUES ('精索');
INSERT INTO `sensitive_word_data` VALUES ('精索内筋膜');
INSERT INTO `sensitive_word_data` VALUES ('精索外筋膜');
INSERT INTO `sensitive_word_data` VALUES ('精索静脉曲张');
INSERT INTO `sensitive_word_data` VALUES ('精索鞘韧带');
INSERT INTO `sensitive_word_data` VALUES ('精细胞');
INSERT INTO `sensitive_word_data` VALUES ('精脱');
INSERT INTO `sensitive_word_data` VALUES ('精虫');
INSERT INTO `sensitive_word_data` VALUES ('精门');
INSERT INTO `sensitive_word_data` VALUES ('精门开');
INSERT INTO `sensitive_word_data` VALUES ('精阜');
INSERT INTO `sensitive_word_data` VALUES ('紅色恐');
INSERT INTO `sensitive_word_data` VALUES ('素女心');
INSERT INTO `sensitive_word_data` VALUES ('索丽生');
INSERT INTO `sensitive_word_data` VALUES ('紧合');
INSERT INTO `sensitive_word_data` VALUES ('紧咬');
INSERT INTO `sensitive_word_data` VALUES ('紧夹');
INSERT INTO `sensitive_word_data` VALUES ('紧套');
INSERT INTO `sensitive_word_data` VALUES ('紧小');
INSERT INTO `sensitive_word_data` VALUES ('紧抓');
INSERT INTO `sensitive_word_data` VALUES ('紧掏');
INSERT INTO `sensitive_word_data` VALUES ('紧握');
INSERT INTO `sensitive_word_data` VALUES ('紧淘');
INSERT INTO `sensitive_word_data` VALUES ('紧窄');
INSERT INTO `sensitive_word_data` VALUES ('紧贴');
INSERT INTO `sensitive_word_data` VALUES ('紧靠');
INSERT INTO `sensitive_word_data` VALUES ('紫红色');
INSERT INTO `sensitive_word_data` VALUES ('紫阳');
INSERT INTO `sensitive_word_data` VALUES ('红唇');
INSERT INTO `sensitive_word_data` VALUES ('红外透视');
INSERT INTO `sensitive_word_data` VALUES ('红床 ');
INSERT INTO `sensitive_word_data` VALUES ('红润');
INSERT INTO `sensitive_word_data` VALUES ('红满堂');
INSERT INTO `sensitive_word_data` VALUES ('红病历 ');
INSERT INTO `sensitive_word_data` VALUES ('红色恐怖');
INSERT INTO `sensitive_word_data` VALUES ('红颊');
INSERT INTO `sensitive_word_data` VALUES ('红魂网站');
INSERT INTO `sensitive_word_data` VALUES ('级办理');
INSERT INTO `sensitive_word_data` VALUES ('级答案');
INSERT INTO `sensitive_word_data` VALUES ('纯度白');
INSERT INTO `sensitive_word_data` VALUES ('纯度黄');
INSERT INTO `sensitive_word_data` VALUES ('纯熟');
INSERT INTO `sensitive_word_data` VALUES ('纳米比亚');
INSERT INTO `sensitive_word_data` VALUES ('纵欲');
INSERT INTO `sensitive_word_data` VALUES ('纹了毛');
INSERT INTO `sensitive_word_data` VALUES ('线透视镜');
INSERT INTO `sensitive_word_data` VALUES ('练功群众 ');
INSERT INTO `sensitive_word_data` VALUES ('组装手枪  ');
INSERT INTO `sensitive_word_data` VALUES ('经典谎言');
INSERT INTO `sensitive_word_data` VALUES ('经前痤疮');
INSERT INTO `sensitive_word_data` VALUES ('经期');
INSERT INTO `sensitive_word_data` VALUES ('经期紊乱');
INSERT INTO `sensitive_word_data` VALUES ('经痛');
INSERT INTO `sensitive_word_data` VALUES ('经血');
INSERT INTO `sensitive_word_data` VALUES ('经血来潮');
INSERT INTO `sensitive_word_data` VALUES ('经质粘稠');
INSERT INTO `sensitive_word_data` VALUES ('经验');
INSERT INTO `sensitive_word_data` VALUES ('结缔组织');
INSERT INTO `sensitive_word_data` VALUES ('绕过封锁');
INSERT INTO `sensitive_word_data` VALUES ('绝经');
INSERT INTO `sensitive_word_data` VALUES ('绝食');
INSERT INTO `sensitive_word_data` VALUES ('绝食声');
INSERT INTO `sensitive_word_data` VALUES ('统一教');
INSERT INTO `sensitive_word_data` VALUES ('继父');
INSERT INTO `sensitive_word_data` VALUES ('绩过后付');
INSERT INTO `sensitive_word_data` VALUES ('维权人');
INSERT INTO `sensitive_word_data` VALUES ('维权基');
INSERT INTO `sensitive_word_data` VALUES ('维权谈');
INSERT INTO `sensitive_word_data` VALUES ('维汉员');
INSERT INTO `sensitive_word_data` VALUES ('绵软');
INSERT INTO `sensitive_word_data` VALUES ('缓慢');
INSERT INTO `sensitive_word_data` VALUES ('缓进速出');
INSERT INTO `sensitive_word_data` VALUES ('缠抱');
INSERT INTO `sensitive_word_data` VALUES ('缠绵');
INSERT INTO `sensitive_word_data` VALUES ('缩阴 ');
INSERT INTO `sensitive_word_data` VALUES ('缸交');
INSERT INTO `sensitive_word_data` VALUES ('网民案');
INSERT INTO `sensitive_word_data` VALUES ('网民获刑');
INSERT INTO `sensitive_word_data` VALUES ('网民诬');
INSERT INTO `sensitive_word_data` VALUES ('网站推广软件 ');
INSERT INTO `sensitive_word_data` VALUES ('网络推广扩散器 ');
INSERT INTO `sensitive_word_data` VALUES ('罗干 ');
INSERT INTO `sensitive_word_data` VALUES ('罗斯小姐');
INSERT INTO `sensitive_word_data` VALUES ('罗清泉');
INSERT INTO `sensitive_word_data` VALUES ('罗箭');
INSERT INTO `sensitive_word_data` VALUES ('罗豪才');
INSERT INTO `sensitive_word_data` VALUES ('罢工门');
INSERT INTO `sensitive_word_data` VALUES ('美乳');
INSERT INTO `sensitive_word_data` VALUES ('美人');
INSERT INTO `sensitive_word_data` VALUES ('美伶');
INSERT INTO `sensitive_word_data` VALUES ('美体');
INSERT INTO `sensitive_word_data` VALUES ('美唇');
INSERT INTO `sensitive_word_data` VALUES ('美国军刀出售');
INSERT INTO `sensitive_word_data` VALUES ('美妇');
INSERT INTO `sensitive_word_data` VALUES ('美感');
INSERT INTO `sensitive_word_data` VALUES ('美沙酮');
INSERT INTO `sensitive_word_data` VALUES ('美满');
INSERT INTO `sensitive_word_data` VALUES ('美目');
INSERT INTO `sensitive_word_data` VALUES ('美穴');
INSERT INTO `sensitive_word_data` VALUES ('美肉');
INSERT INTO `sensitive_word_data` VALUES ('美脚');
INSERT INTO `sensitive_word_data` VALUES ('美腿');
INSERT INTO `sensitive_word_data` VALUES ('美臀');
INSERT INTO `sensitive_word_data` VALUES ('美艳少妇');
INSERT INTO `sensitive_word_data` VALUES ('美香');
INSERT INTO `sensitive_word_data` VALUES ('群交 ');
INSERT INTO `sensitive_word_data` VALUES ('群体性事');
INSERT INTO `sensitive_word_data` VALUES ('群体灭绝');
INSERT INTO `sensitive_word_data` VALUES ('群发广告机 ');
INSERT INTO `sensitive_word_data` VALUES ('群发软件 ');
INSERT INTO `sensitive_word_data` VALUES ('群奸暴');
INSERT INTO `sensitive_word_data` VALUES ('群起抗暴');
INSERT INTO `sensitive_word_data` VALUES ('翁安');
INSERT INTO `sensitive_word_data` VALUES ('翘臀');
INSERT INTO `sensitive_word_data` VALUES ('翘起');
INSERT INTO `sensitive_word_data` VALUES ('翟泰丰');
INSERT INTO `sensitive_word_data` VALUES ('翻动');
INSERT INTO `sensitive_word_data` VALUES ('翻弄');
INSERT INTO `sensitive_word_data` VALUES ('翻搅');
INSERT INTO `sensitive_word_data` VALUES ('耀邦 ');
INSERT INTO `sensitive_word_data` VALUES ('老j');
INSERT INTO `sensitive_word_data` VALUES ('老习');
INSERT INTO `sensitive_word_data` VALUES ('老二');
INSERT INTO `sensitive_word_data` VALUES ('老共');
INSERT INTO `sensitive_word_data` VALUES ('老套');
INSERT INTO `sensitive_word_data` VALUES ('老姐');
INSERT INTO `sensitive_word_data` VALUES ('老江');
INSERT INTO `sensitive_word_data` VALUES ('老爸');
INSERT INTO `sensitive_word_data` VALUES ('老虎机');
INSERT INTO `sensitive_word_data` VALUES ('考中答案');
INSERT INTO `sensitive_word_data` VALUES ('考前付');
INSERT INTO `sensitive_word_data` VALUES ('考前答');
INSERT INTO `sensitive_word_data` VALUES ('考前答案');
INSERT INTO `sensitive_word_data` VALUES ('考后付款');
INSERT INTO `sensitive_word_data` VALUES ('考机构');
INSERT INTO `sensitive_word_data` VALUES ('考研考中');
INSERT INTO `sensitive_word_data` VALUES ('考答案');
INSERT INTO `sensitive_word_data` VALUES ('考考邓');
INSERT INTO `sensitive_word_data` VALUES ('考联盟');
INSERT INTO `sensitive_word_data` VALUES ('考设备');
INSERT INTO `sensitive_word_data` VALUES ('考试保');
INSERT INTO `sensitive_word_data` VALUES ('考试包过');
INSERT INTO `sensitive_word_data` VALUES ('考试机构');
INSERT INTO `sensitive_word_data` VALUES ('考试枪');
INSERT INTO `sensitive_word_data` VALUES ('考试答案');
INSERT INTO `sensitive_word_data` VALUES ('考试联盟');
INSERT INTO `sensitive_word_data` VALUES ('耗精伤气');
INSERT INTO `sensitive_word_data` VALUES ('耳垂');
INSERT INTO `sensitive_word_data` VALUES ('耳磨鬓擦');
INSERT INTO `sensitive_word_data` VALUES ('耶稣基督血水圣灵全备福音布道团 ');
INSERT INTO `sensitive_word_data` VALUES ('耻丘');
INSERT INTO `sensitive_word_data` VALUES ('耻毛');
INSERT INTO `sensitive_word_data` VALUES ('耻部');
INSERT INTO `sensitive_word_data` VALUES ('耻骨尾骨肌');
INSERT INTO `sensitive_word_data` VALUES ('耻骨直肠肌');
INSERT INTO `sensitive_word_data` VALUES ('聂树斌');
INSERT INTO `sensitive_word_data` VALUES ('聊斋艳');
INSERT INTO `sensitive_word_data` VALUES ('聊视频');
INSERT INTO `sensitive_word_data` VALUES ('聯繫電');
INSERT INTO `sensitive_word_data` VALUES ('肉棍');
INSERT INTO `sensitive_word_data` VALUES ('肉棒 ');
INSERT INTO `sensitive_word_data` VALUES ('肉洞');
INSERT INTO `sensitive_word_data` VALUES ('肉穴');
INSERT INTO `sensitive_word_data` VALUES ('肌肉');
INSERT INTO `sensitive_word_data` VALUES ('肏');
INSERT INTO `sensitive_word_data` VALUES ('肏人');
INSERT INTO `sensitive_word_data` VALUES ('肏干');
INSERT INTO `sensitive_word_data` VALUES ('肏我');
INSERT INTO `sensitive_word_data` VALUES ('肏死');
INSERT INTO `sensitive_word_data` VALUES ('肖爱玲');
INSERT INTO `sensitive_word_data` VALUES ('肚脐');
INSERT INTO `sensitive_word_data` VALUES ('肛乳头炎');
INSERT INTO `sensitive_word_data` VALUES ('肛交');
INSERT INTO `sensitive_word_data` VALUES ('肛尾韧带');
INSERT INTO `sensitive_word_data` VALUES ('肛柱');
INSERT INTO `sensitive_word_data` VALUES ('肛窦炎');
INSERT INTO `sensitive_word_data` VALUES ('肛管');
INSERT INTO `sensitive_word_data` VALUES ('肛管内括约肌');
INSERT INTO `sensitive_word_data` VALUES ('肛管外括约肌');
INSERT INTO `sensitive_word_data` VALUES ('肛管直肠环');
INSERT INTO `sensitive_word_data` VALUES ('肛肉');
INSERT INTO `sensitive_word_data` VALUES ('肛腺');
INSERT INTO `sensitive_word_data` VALUES ('肛部');
INSERT INTO `sensitive_word_data` VALUES ('肛门');
INSERT INTO `sensitive_word_data` VALUES ('肛门交');
INSERT INTO `sensitive_word_data` VALUES ('肛门是邻');
INSERT INTO `sensitive_word_data` VALUES ('肠壁');
INSERT INTO `sensitive_word_data` VALUES ('肠梨形鞭毛虫病');
INSERT INTO `sensitive_word_data` VALUES ('肠源性紫绀');
INSERT INTO `sensitive_word_data` VALUES ('股沟');
INSERT INTO `sensitive_word_data` VALUES ('肢体');
INSERT INTO `sensitive_word_data` VALUES ('肥乳');
INSERT INTO `sensitive_word_data` VALUES ('肥大');
INSERT INTO `sensitive_word_data` VALUES ('肥奶');
INSERT INTO `sensitive_word_data` VALUES ('肥尻');
INSERT INTO `sensitive_word_data` VALUES ('肥润');
INSERT INTO `sensitive_word_data` VALUES ('肥穴');
INSERT INTO `sensitive_word_data` VALUES ('肥美');
INSERT INTO `sensitive_word_data` VALUES ('肥翘');
INSERT INTO `sensitive_word_data` VALUES ('肥臀');
INSERT INTO `sensitive_word_data` VALUES ('育部女官');
INSERT INTO `sensitive_word_data` VALUES ('肿涨');
INSERT INTO `sensitive_word_data` VALUES ('肿胀的东西');
INSERT INTO `sensitive_word_data` VALUES ('胀大');
INSERT INTO `sensitive_word_data` VALUES ('胀得难受');
INSERT INTO `sensitive_word_data` VALUES ('胀疼');
INSERT INTO `sensitive_word_data` VALUES ('胀破');
INSERT INTO `sensitive_word_data` VALUES ('胀硬');
INSERT INTO `sensitive_word_data` VALUES ('胀红');
INSERT INTO `sensitive_word_data` VALUES ('胀胀');
INSERT INTO `sensitive_word_data` VALUES ('背飞凫');
INSERT INTO `sensitive_word_data` VALUES ('胎盘');
INSERT INTO `sensitive_word_data` VALUES ('胞漏疮');
INSERT INTO `sensitive_word_data` VALUES ('胡boss');
INSERT INTO `sensitive_word_data` VALUES ('胡jintao');
INSERT INTO `sensitive_word_data` VALUES ('胡jt');
INSERT INTO `sensitive_word_data` VALUES ('胡j涛');
INSERT INTO `sensitive_word_data` VALUES ('胡x');
INSERT INTO `sensitive_word_data` VALUES ('胡下台 ');
INSERT INTO `sensitive_word_data` VALUES ('胡主席');
INSERT INTO `sensitive_word_data` VALUES ('胡光宝');
INSERT INTO `sensitive_word_data` VALUES ('胡富国');
INSERT INTO `sensitive_word_data` VALUES ('胡康生');
INSERT INTO `sensitive_word_data` VALUES ('胡彦林');
INSERT INTO `sensitive_word_data` VALUES ('胡彪');
INSERT INTO `sensitive_word_data` VALUES ('胡德平');
INSERT INTO `sensitive_word_data` VALUES ('胡总');
INSERT INTO `sensitive_word_data` VALUES ('胡惊涛');
INSERT INTO `sensitive_word_data` VALUES ('胡春华');
INSERT INTO `sensitive_word_data` VALUES ('胡景涛');
INSERT INTO `sensitive_word_data` VALUES ('胡江 ');
INSERT INTO `sensitive_word_data` VALUES ('胡江争斗 ');
INSERT INTO `sensitive_word_data` VALUES ('胡江关系 ');
INSERT INTO `sensitive_word_data` VALUES ('胡江内斗');
INSERT INTO `sensitive_word_data` VALUES ('胡江曾 ');
INSERT INTO `sensitive_word_data` VALUES ('胡江风云 ');
INSERT INTO `sensitive_word_data` VALUES ('胡派 ');
INSERT INTO `sensitive_word_data` VALUES ('胡派人马 ');
INSERT INTO `sensitive_word_data` VALUES ('胡派军委 ');
INSERT INTO `sensitive_word_data` VALUES ('胡海峰');
INSERT INTO `sensitive_word_data` VALUES ('胡海清');
INSERT INTO `sensitive_word_data` VALUES ('胡温 ');
INSERT INTO `sensitive_word_data` VALUES ('胡王八');
INSERT INTO `sensitive_word_data` VALUES ('胡的接班人');
INSERT INTO `sensitive_word_data` VALUES ('胡紧套');
INSERT INTO `sensitive_word_data` VALUES ('胡紧掏 ');
INSERT INTO `sensitive_word_data` VALUES ('胡绩伟');
INSERT INTO `sensitive_word_data` VALUES ('胡耀邦');
INSERT INTO `sensitive_word_data` VALUES ('胡贤生');
INSERT INTO `sensitive_word_data` VALUES ('胡进套');
INSERT INTO `sensitive_word_data` VALUES ('胡进涛 ');
INSERT INTO `sensitive_word_data` VALUES ('胡适眼');
INSERT INTO `sensitive_word_data` VALUES ('胡錦濤');
INSERT INTO `sensitive_word_data` VALUES ('胡锦 ');
INSERT INTO `sensitive_word_data` VALUES ('胡锦涛');
INSERT INTO `sensitive_word_data` VALUES ('胯下');
INSERT INTO `sensitive_word_data` VALUES ('胯股');
INSERT INTO `sensitive_word_data` VALUES ('胰岛素样生长因子');
INSERT INTO `sensitive_word_data` VALUES ('胸主席');
INSERT INTO `sensitive_word_data` VALUES ('脏病');
INSERT INTO `sensitive_word_data` VALUES ('脓尿');
INSERT INTO `sensitive_word_data` VALUES ('脚交');
INSERT INTO `sensitive_word_data` VALUES ('脱光');
INSERT INTO `sensitive_word_data` VALUES ('脱衣艳');
INSERT INTO `sensitive_word_data` VALUES ('脱裤门');
INSERT INTO `sensitive_word_data` VALUES ('腔内');
INSERT INTO `sensitive_word_data` VALUES ('腔肉');
INSERT INTO `sensitive_word_data` VALUES ('腹股沟淋巴结肿大');
INSERT INTO `sensitive_word_data` VALUES ('腹股沟淋巴肉芽肿');
INSERT INTO `sensitive_word_data` VALUES ('腹股沟疝');
INSERT INTO `sensitive_word_data` VALUES ('腹股沟管');
INSERT INTO `sensitive_word_data` VALUES ('腹股沟肉芽肿');
INSERT INTO `sensitive_word_data` VALUES ('腻滑');
INSERT INTO `sensitive_word_data` VALUES ('膀胱');
INSERT INTO `sensitive_word_data` VALUES ('膀胱三角');
INSERT INTO `sensitive_word_data` VALUES ('膀胱肿瘤');
INSERT INTO `sensitive_word_data` VALUES ('膀胱阴道瘘');
INSERT INTO `sensitive_word_data` VALUES ('膏淋');
INSERT INTO `sensitive_word_data` VALUES ('膨大');
INSERT INTO `sensitive_word_data` VALUES ('膨涨');
INSERT INTO `sensitive_word_data` VALUES ('膨胀');
INSERT INTO `sensitive_word_data` VALUES ('臂部');
INSERT INTO `sensitive_word_data` VALUES ('自fen');
INSERT INTO `sensitive_word_data` VALUES ('自sha');
INSERT INTO `sensitive_word_data` VALUES ('自制手弩');
INSERT INTO `sensitive_word_data` VALUES ('自制手枪哪里买  ');
INSERT INTO `sensitive_word_data` VALUES ('自制手枪哪里有卖');
INSERT INTO `sensitive_word_data` VALUES ('自制炸药方法');
INSERT INTO `sensitive_word_data` VALUES ('自制炸药配方');
INSERT INTO `sensitive_word_data` VALUES ('自制燃烧弹');
INSERT INTO `sensitive_word_data` VALUES ('自动群发');
INSERT INTO `sensitive_word_data` VALUES ('自动跳刀专卖');
INSERT INTO `sensitive_word_data` VALUES ('自己找枪');
INSERT INTO `sensitive_word_data` VALUES ('自慰');
INSERT INTO `sensitive_word_data` VALUES ('自慰用');
INSERT INTO `sensitive_word_data` VALUES ('自杀手册 ');
INSERT INTO `sensitive_word_data` VALUES ('自淫');
INSERT INTO `sensitive_word_data` VALUES ('自渎');
INSERT INTO `sensitive_word_data` VALUES ('自烧 ');
INSERT INTO `sensitive_word_data` VALUES ('自焚 ');
INSERT INTO `sensitive_word_data` VALUES ('自由亚');
INSERT INTO `sensitive_word_data` VALUES ('自由圣');
INSERT INTO `sensitive_word_data` VALUES ('自由门 ');
INSERT INTO `sensitive_word_data` VALUES ('至国家高');
INSERT INTO `sensitive_word_data` VALUES ('舅妈');
INSERT INTO `sensitive_word_data` VALUES ('舅父');
INSERT INTO `sensitive_word_data` VALUES ('舅舅');
INSERT INTO `sensitive_word_data` VALUES ('舒圣佑');
INSERT INTO `sensitive_word_data` VALUES ('舒惠国');
INSERT INTO `sensitive_word_data` VALUES ('色妹妹');
INSERT INTO `sensitive_word_data` VALUES ('色小说');
INSERT INTO `sensitive_word_data` VALUES ('色情片 ');
INSERT INTO `sensitive_word_data` VALUES ('色情电影 ');
INSERT INTO `sensitive_word_data` VALUES ('色电影');
INSERT INTO `sensitive_word_data` VALUES ('色视频');
INSERT INTO `sensitive_word_data` VALUES ('艹你');
INSERT INTO `sensitive_word_data` VALUES ('艾丕善');
INSERT INTO `sensitive_word_data` VALUES ('艾仰华');
INSERT INTO `sensitive_word_data` VALUES ('花唇');
INSERT INTO `sensitive_word_data` VALUES ('花心');
INSERT INTO `sensitive_word_data` VALUES ('花瓣');
INSERT INTO `sensitive_word_data` VALUES ('花穴');
INSERT INTO `sensitive_word_data` VALUES ('花芯');
INSERT INTO `sensitive_word_data` VALUES ('花苞');
INSERT INTO `sensitive_word_data` VALUES ('花蕊');
INSERT INTO `sensitive_word_data` VALUES ('花蕾');
INSERT INTO `sensitive_word_data` VALUES ('花蜜');
INSERT INTO `sensitive_word_data` VALUES ('芳香');
INSERT INTO `sensitive_word_data` VALUES ('芳香型智悟气功 ');
INSERT INTO `sensitive_word_data` VALUES ('苍山兰');
INSERT INTO `sensitive_word_data` VALUES ('苍白螺旋体');
INSERT INTO `sensitive_word_data` VALUES ('苍蝇水');
INSERT INTO `sensitive_word_data` VALUES ('苏家屯集');
INSERT INTO `sensitive_word_data` VALUES ('苏晓康');
INSERT INTO `sensitive_word_data` VALUES ('苏树林');
INSERT INTO `sensitive_word_data` VALUES ('苏纪兰');
INSERT INTO `sensitive_word_data` VALUES ('苏荣');
INSERT INTO `sensitive_word_data` VALUES ('苟建丽');
INSERT INTO `sensitive_word_data` VALUES ('苯丙胺');
INSERT INTO `sensitive_word_data` VALUES ('苯巴比妥');
INSERT INTO `sensitive_word_data` VALUES ('茁壮');
INSERT INTO `sensitive_word_data` VALUES ('茂密');
INSERT INTO `sensitive_word_data` VALUES ('茂盛');
INSERT INTO `sensitive_word_data` VALUES ('范宝俊');
INSERT INTO `sensitive_word_data` VALUES ('范徐丽泰');
INSERT INTO `sensitive_word_data` VALUES ('范燕琼');
INSERT INTO `sensitive_word_data` VALUES ('范长龙');
INSERT INTO `sensitive_word_data` VALUES ('茎头');
INSERT INTO `sensitive_word_data` VALUES ('茎底');
INSERT INTO `sensitive_word_data` VALUES ('茳泽民');
INSERT INTO `sensitive_word_data` VALUES ('茳澤民');
INSERT INTO `sensitive_word_data` VALUES ('草你丫');
INSERT INTO `sensitive_word_data` VALUES ('草你吗');
INSERT INTO `sensitive_word_data` VALUES ('荡叫');
INSERT INTO `sensitive_word_data` VALUES ('荡声');
INSERT INTO `sensitive_word_data` VALUES ('荡妇');
INSERT INTO `sensitive_word_data` VALUES ('荡尽天下');
INSERT INTO `sensitive_word_data` VALUES ('莫时仁');
INSERT INTO `sensitive_word_data` VALUES ('莫洛托夫燃烧弹');
INSERT INTO `sensitive_word_data` VALUES ('莫洛托夫鸡尾酒的配方');
INSERT INTO `sensitive_word_data` VALUES ('莫达非尼');
INSERT INTO `sensitive_word_data` VALUES ('获得');
INSERT INTO `sensitive_word_data` VALUES ('菊孔');
INSERT INTO `sensitive_word_data` VALUES ('菊暴 ');
INSERT INTO `sensitive_word_data` VALUES ('菊爆 ');
INSERT INTO `sensitive_word_data` VALUES ('菊花洞');
INSERT INTO `sensitive_word_data` VALUES ('营造');
INSERT INTO `sensitive_word_data` VALUES ('萧灼基');
INSERT INTO `sensitive_word_data` VALUES ('落霞缀');
INSERT INTO `sensitive_word_data` VALUES ('著名精品折刀出售');
INSERT INTO `sensitive_word_data` VALUES ('葡萄胎');
INSERT INTO `sensitive_word_data` VALUES ('董元辰');
INSERT INTO `sensitive_word_data` VALUES ('董建华');
INSERT INTO `sensitive_word_data` VALUES ('董昕');
INSERT INTO `sensitive_word_data` VALUES ('蒋以任');
INSERT INTO `sensitive_word_data` VALUES ('蒋公');
INSERT INTO `sensitive_word_data` VALUES ('蒋彦永');
INSERT INTO `sensitive_word_data` VALUES ('蒋树声');
INSERT INTO `sensitive_word_data` VALUES ('蒋正华');
INSERT INTO `sensitive_word_data` VALUES ('蒙汗药');
INSERT INTO `sensitive_word_data` VALUES ('蒙汗药粉 ');
INSERT INTO `sensitive_word_data` VALUES ('蔡武');
INSERT INTO `sensitive_word_data` VALUES ('蔡赴朝');
INSERT INTO `sensitive_word_data` VALUES ('蔣彥永');
INSERT INTO `sensitive_word_data` VALUES ('蕾苞');
INSERT INTO `sensitive_word_data` VALUES ('薄一波');
INSERT INTO `sensitive_word_data` VALUES ('薄熙来');
INSERT INTO `sensitive_word_data` VALUES ('藏字石');
INSERT INTO `sensitive_word_data` VALUES ('藏春阁');
INSERT INTO `sensitive_word_data` VALUES ('藏独');
INSERT INTO `sensitive_word_data` VALUES ('藏獨');
INSERT INTO `sensitive_word_data` VALUES ('虎头猎');
INSERT INTO `sensitive_word_data` VALUES ('虐待');
INSERT INTO `sensitive_word_data` VALUES ('蚁力神');
INSERT INTO `sensitive_word_data` VALUES ('蚌唇');
INSERT INTO `sensitive_word_data` VALUES ('蛋');
INSERT INTO `sensitive_word_data` VALUES ('蛋子');
INSERT INTO `sensitive_word_data` VALUES ('蛋蛋');
INSERT INTO `sensitive_word_data` VALUES ('蛤蟆转世 ');
INSERT INTO `sensitive_word_data` VALUES ('蛮腰');
INSERT INTO `sensitive_word_data` VALUES ('蜜唇');
INSERT INTO `sensitive_word_data` VALUES ('蜜壶');
INSERT INTO `sensitive_word_data` VALUES ('蜜意');
INSERT INTO `sensitive_word_data` VALUES ('蜜桃');
INSERT INTO `sensitive_word_data` VALUES ('蜜汁');
INSERT INTO `sensitive_word_data` VALUES ('蜜洞');
INSERT INTO `sensitive_word_data` VALUES ('蜜液');
INSERT INTO `sensitive_word_data` VALUES ('蜜穴');
INSERT INTO `sensitive_word_data` VALUES ('蜜肉');
INSERT INTO `sensitive_word_data` VALUES ('蝶舞按');
INSERT INTO `sensitive_word_data` VALUES ('蟆叫专家');
INSERT INTO `sensitive_word_data` VALUES ('蟾蜍迁徙');
INSERT INTO `sensitive_word_data` VALUES ('蠢蠢欲动');
INSERT INTO `sensitive_word_data` VALUES ('行长王益');
INSERT INTO `sensitive_word_data` VALUES ('表兄');
INSERT INTO `sensitive_word_data` VALUES ('表哥');
INSERT INTO `sensitive_word_data` VALUES ('表妹');
INSERT INTO `sensitive_word_data` VALUES ('表姊');
INSERT INTO `sensitive_word_data` VALUES ('表姐');
INSERT INTO `sensitive_word_data` VALUES ('表嫂');
INSERT INTO `sensitive_word_data` VALUES ('表弟');
INSERT INTO `sensitive_word_data` VALUES ('表现');
INSERT INTO `sensitive_word_data` VALUES ('袁伟时');
INSERT INTO `sensitive_word_data` VALUES ('袁汉民');
INSERT INTO `sensitive_word_data` VALUES ('袁纯清');
INSERT INTO `sensitive_word_data` VALUES ('袁腾飞');
INSERT INTO `sensitive_word_data` VALUES ('袁行霈');
INSERT INTO `sensitive_word_data` VALUES ('袁驷');
INSERT INTO `sensitive_word_data` VALUES ('袜按摩');
INSERT INTO `sensitive_word_data` VALUES ('被中共');
INSERT INTO `sensitive_word_data` VALUES ('被打死');
INSERT INTO `sensitive_word_data` VALUES ('被指抄袭');
INSERT INTO `sensitive_word_data` VALUES ('袭警');
INSERT INTO `sensitive_word_data` VALUES ('装b');
INSERT INTO `sensitive_word_data` VALUES ('装弹甲');
INSERT INTO `sensitive_word_data` VALUES ('装枪套');
INSERT INTO `sensitive_word_data` VALUES ('装消音');
INSERT INTO `sensitive_word_data` VALUES ('裆中央');
INSERT INTO `sensitive_word_data` VALUES ('裙中性运动');
INSERT INTO `sensitive_word_data` VALUES ('裸体');
INSERT INTO `sensitive_word_data` VALUES ('裸体男女');
INSERT INTO `sensitive_word_data` VALUES ('裸女');
INSERT INTO `sensitive_word_data` VALUES ('裸照 ');
INSERT INTO `sensitive_word_data` VALUES ('裸着');
INSERT INTO `sensitive_word_data` VALUES ('裸睡');
INSERT INTO `sensitive_word_data` VALUES ('裸聊 ');
INSERT INTO `sensitive_word_data` VALUES ('裸聊网');
INSERT INTO `sensitive_word_data` VALUES ('裸背');
INSERT INTO `sensitive_word_data` VALUES ('裸胸');
INSERT INTO `sensitive_word_data` VALUES ('裸臀');
INSERT INTO `sensitive_word_data` VALUES ('裸舞视');
INSERT INTO `sensitive_word_data` VALUES ('裸身');
INSERT INTO `sensitive_word_data` VALUES ('裸躯');
INSERT INTO `sensitive_word_data` VALUES ('裸露');
INSERT INTO `sensitive_word_data` VALUES ('裹住');
INSERT INTO `sensitive_word_data` VALUES ('裹着');
INSERT INTO `sensitive_word_data` VALUES ('褚玉光');
INSERT INTO `sensitive_word_data` VALUES ('西布曲明');
INSERT INTO `sensitive_word_data` VALUES ('西服进去');
INSERT INTO `sensitive_word_data` VALUES ('西点军刀网');
INSERT INTO `sensitive_word_data` VALUES ('西点军品军刀网');
INSERT INTO `sensitive_word_data` VALUES ('西纳');
INSERT INTO `sensitive_word_data` VALUES ('西藏限');
INSERT INTO `sensitive_word_data` VALUES ('要人权');
INSERT INTO `sensitive_word_data` VALUES ('要射了');
INSERT INTO `sensitive_word_data` VALUES ('要射精了');
INSERT INTO `sensitive_word_data` VALUES ('要泄了');
INSERT INTO `sensitive_word_data` VALUES ('覃志刚');
INSERT INTO `sensitive_word_data` VALUES ('观淫癖');
INSERT INTO `sensitive_word_data` VALUES ('观音法门');
INSERT INTO `sensitive_word_data` VALUES ('视解密');
INSERT INTO `sensitive_word_data` VALUES ('解密软件 ');
INSERT INTO `sensitive_word_data` VALUES ('解带');
INSERT INTO `sensitive_word_data` VALUES ('解开');
INSERT INTO `sensitive_word_data` VALUES ('解脲脲原体');
INSERT INTO `sensitive_word_data` VALUES ('解衣');
INSERT INTO `sensitive_word_data` VALUES ('触动');
INSERT INTO `sensitive_word_data` VALUES ('触摸');
INSERT INTO `sensitive_word_data` VALUES ('触淫');
INSERT INTO `sensitive_word_data` VALUES ('触碰');
INSERT INTO `sensitive_word_data` VALUES ('言被劳教');
INSERT INTO `sensitive_word_data` VALUES ('言论罪');
INSERT INTO `sensitive_word_data` VALUES ('證件');
INSERT INTO `sensitive_word_data` VALUES ('警察我们是为人民服务的');
INSERT INTO `sensitive_word_data` VALUES ('警察殴打');
INSERT INTO `sensitive_word_data` VALUES ('警察的幌');
INSERT INTO `sensitive_word_data` VALUES ('警察被');
INSERT INTO `sensitive_word_data` VALUES ('警察说保');
INSERT INTO `sensitive_word_data` VALUES ('警徽 ');
INSERT INTO `sensitive_word_data` VALUES ('警方包庇');
INSERT INTO `sensitive_word_data` VALUES ('警用刀具出售');
INSERT INTO `sensitive_word_data` VALUES ('警用品');
INSERT INTO `sensitive_word_data` VALUES ('警车雷达');
INSERT INTO `sensitive_word_data` VALUES ('认牌绝');
INSERT INTO `sensitive_word_data` VALUES ('讨回工资');
INSERT INTO `sensitive_word_data` VALUES ('记号扑克 ');
INSERT INTO `sensitive_word_data` VALUES ('讲法传功');
INSERT INTO `sensitive_word_data` VALUES ('许万平');
INSERT INTO `sensitive_word_data` VALUES ('许仲林');
INSERT INTO `sensitive_word_data` VALUES ('许克敏');
INSERT INTO `sensitive_word_data` VALUES ('许其亮');
INSERT INTO `sensitive_word_data` VALUES ('许嘉璐');
INSERT INTO `sensitive_word_data` VALUES ('许志琴');
INSERT INTO `sensitive_word_data` VALUES ('许智宏');
INSERT INTO `sensitive_word_data` VALUES ('许柏年');
INSERT INTO `sensitive_word_data` VALUES ('许永跃');
INSERT INTO `sensitive_word_data` VALUES ('论文代');
INSERT INTO `sensitive_word_data` VALUES ('证一次性');
INSERT INTO `sensitive_word_data` VALUES ('证书办');
INSERT INTO `sensitive_word_data` VALUES ('证件');
INSERT INTO `sensitive_word_data` VALUES ('证件公司');
INSERT INTO `sensitive_word_data` VALUES ('证件办');
INSERT INTO `sensitive_word_data` VALUES ('证件集团');
INSERT INTO `sensitive_word_data` VALUES ('证到付款');
INSERT INTO `sensitive_word_data` VALUES ('证生成器');
INSERT INTO `sensitive_word_data` VALUES ('诉讼集团');
INSERT INTO `sensitive_word_data` VALUES ('话在肉身显现 ');
INSERT INTO `sensitive_word_data` VALUES ('诱奸');
INSERT INTO `sensitive_word_data` VALUES ('诱惑');
INSERT INTO `sensitive_word_data` VALUES ('请愿');
INSERT INTO `sensitive_word_data` VALUES ('请示威');
INSERT INTO `sensitive_word_data` VALUES ('请集会');
INSERT INTO `sensitive_word_data` VALUES ('诸世纪');
INSERT INTO `sensitive_word_data` VALUES ('诺查丹玛斯');
INSERT INTO `sensitive_word_data` VALUES ('读不起选个学校三万起');
INSERT INTO `sensitive_word_data` VALUES ('诽谤罪');
INSERT INTO `sensitive_word_data` VALUES ('调情');
INSERT INTO `sensitive_word_data` VALUES ('调戏');
INSERT INTO `sensitive_word_data` VALUES ('调整');
INSERT INTO `sensitive_word_data` VALUES ('调经');
INSERT INTO `sensitive_word_data` VALUES ('调逗');
INSERT INTO `sensitive_word_data` VALUES ('谓的和谐');
INSERT INTO `sensitive_word_data` VALUES ('谜奸药');
INSERT INTO `sensitive_word_data` VALUES ('谢丽娟');
INSERT INTO `sensitive_word_data` VALUES ('谢佑卿');
INSERT INTO `sensitive_word_data` VALUES ('谢克昌');
INSERT INTO `sensitive_word_data` VALUES ('谢小庆');
INSERT INTO `sensitive_word_data` VALUES ('谢生林');
INSERT INTO `sensitive_word_data` VALUES ('谤罪获刑');
INSERT INTO `sensitive_word_data` VALUES ('谷建芬');
INSERT INTO `sensitive_word_data` VALUES ('豆腐渣');
INSERT INTO `sensitive_word_data` VALUES ('豪乳');
INSERT INTO `sensitive_word_data` VALUES ('豪乳型');
INSERT INTO `sensitive_word_data` VALUES ('豪圈钱');
INSERT INTO `sensitive_word_data` VALUES ('贝肉');
INSERT INTO `sensitive_word_data` VALUES ('贝领');
INSERT INTO `sensitive_word_data` VALUES ('贡挡');
INSERT INTO `sensitive_word_data` VALUES ('财众科技');
INSERT INTO `sensitive_word_data` VALUES ('败培训');
INSERT INTO `sensitive_word_data` VALUES ('质量');
INSERT INTO `sensitive_word_data` VALUES ('贪官也辛');
INSERT INTO `sensitive_word_data` VALUES ('购买枪支QQ');
INSERT INTO `sensitive_word_data` VALUES ('购买自爆材料');
INSERT INTO `sensitive_word_data` VALUES ('贱b');
INSERT INTO `sensitive_word_data` VALUES ('贱人');
INSERT INTO `sensitive_word_data` VALUES ('贱比');
INSERT INTO `sensitive_word_data` VALUES ('贱货');
INSERT INTO `sensitive_word_data` VALUES ('贷开');
INSERT INTO `sensitive_word_data` VALUES ('费私服');
INSERT INTO `sensitive_word_data` VALUES ('费良勇');
INSERT INTO `sensitive_word_data` VALUES ('贺一诚');
INSERT INTO `sensitive_word_data` VALUES ('贺国强 ');
INSERT INTO `sensitive_word_data` VALUES ('贺旻');
INSERT INTO `sensitive_word_data` VALUES ('贺立旗');
INSERT INTO `sensitive_word_data` VALUES ('贺铿');
INSERT INTO `sensitive_word_data` VALUES ('贾军');
INSERT INTO `sensitive_word_data` VALUES ('贾庆林 ');
INSERT INTO `sensitive_word_data` VALUES ('贾廷安');
INSERT INTO `sensitive_word_data` VALUES ('贾志杰');
INSERT INTO `sensitive_word_data` VALUES ('资料泄');
INSERT INTO `sensitive_word_data` VALUES ('资格證');
INSERT INTO `sensitive_word_data` VALUES ('赌具 ');
INSERT INTO `sensitive_word_data` VALUES ('赌博机');
INSERT INTO `sensitive_word_data` VALUES ('赌恒指');
INSERT INTO `sensitive_word_data` VALUES ('赌球');
INSERT INTO `sensitive_word_data` VALUES ('赌球网');
INSERT INTO `sensitive_word_data` VALUES ('赔率|3');
INSERT INTO `sensitive_word_data` VALUES ('赚钱资料');
INSERT INTO `sensitive_word_data` VALUES ('赛后骚');
INSERT INTO `sensitive_word_data` VALUES ('赤匪 ');
INSERT INTO `sensitive_word_data` VALUES ('赤裸');
INSERT INTO `sensitive_word_data` VALUES ('赤裸裸');
INSERT INTO `sensitive_word_data` VALUES ('赴港生子');
INSERT INTO `sensitive_word_data` VALUES ('赵乐际');
INSERT INTO `sensitive_word_data` VALUES ('赵勇');
INSERT INTO `sensitive_word_data` VALUES ('赵喜明');
INSERT INTO `sensitive_word_data` VALUES ('赵地');
INSERT INTO `sensitive_word_data` VALUES ('赵展岳');
INSERT INTO `sensitive_word_data` VALUES ('赵氏弓弩专卖');
INSERT INTO `sensitive_word_data` VALUES ('赵氏弓弩专卖店');
INSERT INTO `sensitive_word_data` VALUES ('赵氏弓弩专卖网');
INSERT INTO `sensitive_word_data` VALUES ('赵氏弓弩销售');
INSERT INTO `sensitive_word_data` VALUES ('赵洪祝');
INSERT INTO `sensitive_word_data` VALUES ('赵燕');
INSERT INTO `sensitive_word_data` VALUES ('赵紫阳');
INSERT INTO `sensitive_word_data` VALUES ('赵金铎');
INSERT INTO `sensitive_word_data` VALUES ('赵铁斌');
INSERT INTO `sensitive_word_data` VALUES ('赵龙');
INSERT INTO `sensitive_word_data` VALUES ('起性');
INSERT INTO `sensitive_word_data` VALUES ('起爆器');
INSERT INTO `sensitive_word_data` VALUES ('超越红墙');
INSERT INTO `sensitive_word_data` VALUES ('足交 ');
INSERT INTO `sensitive_word_data` VALUES ('足球玩法');
INSERT INTO `sensitive_word_data` VALUES ('跟帖器');
INSERT INTO `sensitive_word_data` VALUES ('跟踪器');
INSERT INTO `sensitive_word_data` VALUES ('跨坐');
INSERT INTO `sensitive_word_data` VALUES ('跨跪');
INSERT INTO `sensitive_word_data` VALUES ('跨骑');
INSERT INTO `sensitive_word_data` VALUES ('跪姿');
INSERT INTO `sensitive_word_data` VALUES ('跪真相');
INSERT INTO `sensitive_word_data` VALUES ('跪臀位');
INSERT INTO `sensitive_word_data` VALUES ('跪骑于');
INSERT INTO `sensitive_word_data` VALUES ('路明');
INSERT INTO `sensitive_word_data` VALUES ('路甬祥');
INSERT INTO `sensitive_word_data` VALUES ('踩踏事');
INSERT INTO `sensitive_word_data` VALUES ('踩踏事故');
INSERT INTO `sensitive_word_data` VALUES ('蹋纳税');
INSERT INTO `sensitive_word_data` VALUES ('身份证生成器');
INSERT INTO `sensitive_word_data` VALUES ('躲猫猫');
INSERT INTO `sensitive_word_data` VALUES ('车仑大法 ');
INSERT INTO `sensitive_word_data` VALUES ('车仑工力');
INSERT INTO `sensitive_word_data` VALUES ('车殿光');
INSERT INTO `sensitive_word_data` VALUES ('车牌隐');
INSERT INTO `sensitive_word_data` VALUES ('车用电子狗');
INSERT INTO `sensitive_word_data` VALUES ('转是政府');
INSERT INTO `sensitive_word_data` VALUES ('转法论 ');
INSERT INTO `sensitive_word_data` VALUES ('转法轮 ');
INSERT INTO `sensitive_word_data` VALUES ('转让 猎枪');
INSERT INTO `sensitive_word_data` VALUES ('轮功');
INSERT INTO `sensitive_word_data` VALUES ('轮奸');
INSERT INTO `sensitive_word_data` VALUES ('轮子功');
INSERT INTO `sensitive_word_data` VALUES ('轮手枪');
INSERT INTO `sensitive_word_data` VALUES ('轮暴');
INSERT INTO `sensitive_word_data` VALUES ('轮法功');
INSERT INTO `sensitive_word_data` VALUES ('软弱的国');
INSERT INTO `sensitive_word_data` VALUES ('轻喘');
INSERT INTO `sensitive_word_data` VALUES ('轻型巡航导弹的制作');
INSERT INTO `sensitive_word_data` VALUES ('轻按');
INSERT INTO `sensitive_word_data` VALUES ('轻搓');
INSERT INTO `sensitive_word_data` VALUES ('轻触');
INSERT INTO `sensitive_word_data` VALUES ('轻颤');
INSERT INTO `sensitive_word_data` VALUES ('较好');
INSERT INTO `sensitive_word_data` VALUES ('辛灏年');
INSERT INTO `sensitive_word_data` VALUES ('辦毕业');
INSERT INTO `sensitive_word_data` VALUES ('辦證');
INSERT INTO `sensitive_word_data` VALUES ('辩词与梦');
INSERT INTO `sensitive_word_data` VALUES ('达毕业证');
INSERT INTO `sensitive_word_data` VALUES ('达米宣教会 ');
INSERT INTO `sensitive_word_data` VALUES ('达赖');
INSERT INTO `sensitive_word_data` VALUES ('过度');
INSERT INTO `sensitive_word_data` VALUES ('过程');
INSERT INTO `sensitive_word_data` VALUES ('迎合');
INSERT INTO `sensitive_word_data` VALUES ('还会吹萧');
INSERT INTO `sensitive_word_data` VALUES ('还看锦涛');
INSERT INTO `sensitive_word_data` VALUES ('进入');
INSERT INTO `sensitive_word_data` VALUES ('进化不完全的生命体');
INSERT INTO `sensitive_word_data` VALUES ('进口气枪,气枪子弹');
INSERT INTO `sensitive_word_data` VALUES ('进来的罪');
INSERT INTO `sensitive_word_data` VALUES ('进程');
INSERT INTO `sensitive_word_data` VALUES ('连炮几炮');
INSERT INTO `sensitive_word_data` VALUES ('迫进');
INSERT INTO `sensitive_word_data` VALUES ('迷奸药');
INSERT INTO `sensitive_word_data` VALUES ('迷幻型');
INSERT INTO `sensitive_word_data` VALUES ('迷幻药');
INSERT INTO `sensitive_word_data` VALUES ('迷幻藥');
INSERT INTO `sensitive_word_data` VALUES ('迷情');
INSERT INTO `sensitive_word_data` VALUES ('迷情水');
INSERT INTO `sensitive_word_data` VALUES ('迷情粉  ');
INSERT INTO `sensitive_word_data` VALUES ('迷情药');
INSERT INTO `sensitive_word_data` VALUES ('迷昏口');
INSERT INTO `sensitive_word_data` VALUES ('迷昏药');
INSERT INTO `sensitive_word_data` VALUES ('迷昏藥');
INSERT INTO `sensitive_word_data` VALUES ('迷药');
INSERT INTO `sensitive_word_data` VALUES ('迷藥');
INSERT INTO `sensitive_word_data` VALUES ('迷魂药');
INSERT INTO `sensitive_word_data` VALUES ('迷魂藥');
INSERT INTO `sensitive_word_data` VALUES ('迷魂香');
INSERT INTO `sensitive_word_data` VALUES ('追债公司 ');
INSERT INTO `sensitive_word_data` VALUES ('追风弓弩麻醉箭专卖');
INSERT INTO `sensitive_word_data` VALUES ('退dang');
INSERT INTO `sensitive_word_data` VALUES ('退党');
INSERT INTO `sensitive_word_data` VALUES ('逆行射精');
INSERT INTO `sensitive_word_data` VALUES ('透视仪');
INSERT INTO `sensitive_word_data` VALUES ('透视功能');
INSERT INTO `sensitive_word_data` VALUES ('透视器');
INSERT INTO `sensitive_word_data` VALUES ('透视扑');
INSERT INTO `sensitive_word_data` VALUES ('透视眼睛');
INSERT INTO `sensitive_word_data` VALUES ('透视眼镜');
INSERT INTO `sensitive_word_data` VALUES ('透视药');
INSERT INTO `sensitive_word_data` VALUES ('透视镜');
INSERT INTO `sensitive_word_data` VALUES ('递纸死');
INSERT INTO `sensitive_word_data` VALUES ('逗弄');
INSERT INTO `sensitive_word_data` VALUES ('通钢总经');
INSERT INTO `sensitive_word_data` VALUES ('速代办');
INSERT INTO `sensitive_word_data` VALUES ('速取证');
INSERT INTO `sensitive_word_data` VALUES ('造反');
INSERT INTO `sensitive_word_data` VALUES ('造爱');
INSERT INTO `sensitive_word_data` VALUES ('逢8必灾');
INSERT INTO `sensitive_word_data` VALUES ('逢9必乱');
INSERT INTO `sensitive_word_data` VALUES ('逢九必乱');
INSERT INTO `sensitive_word_data` VALUES ('逢八必灾');
INSERT INTO `sensitive_word_data` VALUES ('逼');
INSERT INTO `sensitive_word_data` VALUES ('逼迫');
INSERT INTO `sensitive_word_data` VALUES ('逼里');
INSERT INTO `sensitive_word_data` VALUES ('遗情书');
INSERT INTO `sensitive_word_data` VALUES ('遥控信号拦截器 ');
INSERT INTO `sensitive_word_data` VALUES ('遭便衣');
INSERT INTO `sensitive_word_data` VALUES ('遭到警');
INSERT INTO `sensitive_word_data` VALUES ('遭武警');
INSERT INTO `sensitive_word_data` VALUES ('遭警察');
INSERT INTO `sensitive_word_data` VALUES ('避孕膜');
INSERT INTO `sensitive_word_data` VALUES ('邓xp');
INSERT INTO `sensitive_word_data` VALUES ('邓伟志');
INSERT INTO `sensitive_word_data` VALUES ('邓可人');
INSERT INTO `sensitive_word_data` VALUES ('邓小平');
INSERT INTO `sensitive_word_data` VALUES ('邓成城');
INSERT INTO `sensitive_word_data` VALUES ('邓昌友');
INSERT INTO `sensitive_word_data` VALUES ('邓晓平');
INSERT INTO `sensitive_word_data` VALUES ('邓朴方');
INSERT INTO `sensitive_word_data` VALUES ('邓榕');
INSERT INTO `sensitive_word_data` VALUES ('邓爷爷转');
INSERT INTO `sensitive_word_data` VALUES ('邓狗 ');
INSERT INTO `sensitive_word_data` VALUES ('邓玉娇');
INSERT INTO `sensitive_word_data` VALUES ('邓矮子 ');
INSERT INTO `sensitive_word_data` VALUES ('邓质方');
INSERT INTO `sensitive_word_data` VALUES ('邢世忠');
INSERT INTO `sensitive_word_data` VALUES ('邢军');
INSERT INTO `sensitive_word_data` VALUES ('那话');
INSERT INTO `sensitive_word_data` VALUES ('那话儿');
INSERT INTO `sensitive_word_data` VALUES ('邪党');
INSERT INTO `sensitive_word_data` VALUES ('邳州');
INSERT INTO `sensitive_word_data` VALUES ('邵华泽');
INSERT INTO `sensitive_word_data` VALUES ('邵奇惠');
INSERT INTO `sensitive_word_data` VALUES ('邵长良');
INSERT INTO `sensitive_word_data` VALUES ('邵鸿');
INSERT INTO `sensitive_word_data` VALUES ('郑州弓弩专卖');
INSERT INTO `sensitive_word_data` VALUES ('郝建秀');
INSERT INTO `sensitive_word_data` VALUES ('部忙组阁');
INSERT INTO `sensitive_word_data` VALUES ('部是这样');
INSERT INTO `sensitive_word_data` VALUES ('郭东坡');
INSERT INTO `sensitive_word_data` VALUES ('郭伯雄');
INSERT INTO `sensitive_word_data` VALUES ('郭凤莲');
INSERT INTO `sensitive_word_data` VALUES ('郭廷标');
INSERT INTO `sensitive_word_data` VALUES ('郭树言');
INSERT INTO `sensitive_word_data` VALUES ('郭炳湘');
INSERT INTO `sensitive_word_data` VALUES ('郭玉闪');
INSERT INTO `sensitive_word_data` VALUES ('郭金龙');
INSERT INTO `sensitive_word_data` VALUES ('郭飞熊');
INSERT INTO `sensitive_word_data` VALUES ('郭飞雄');
INSERT INTO `sensitive_word_data` VALUES ('都当小姐');
INSERT INTO `sensitive_word_data` VALUES ('都当警');
INSERT INTO `sensitive_word_data` VALUES ('都进中央');
INSERT INTO `sensitive_word_data` VALUES ('配偶');
INSERT INTO `sensitive_word_data` VALUES ('配有消');
INSERT INTO `sensitive_word_data` VALUES ('酒像喝汤');
INSERT INTO `sensitive_word_data` VALUES ('酒瓶门');
INSERT INTO `sensitive_word_data` VALUES ('酒象喝汤');
INSERT INTO `sensitive_word_data` VALUES ('酸甘油炸药');
INSERT INTO `sensitive_word_data` VALUES ('酸羟亚胺');
INSERT INTO `sensitive_word_data` VALUES ('醉乙醚');
INSERT INTO `sensitive_word_data` VALUES ('醉迷药');
INSERT INTO `sensitive_word_data` VALUES ('醉钢枪');
INSERT INTO `sensitive_word_data` VALUES ('采精');
INSERT INTO `sensitive_word_data` VALUES ('采花堂');
INSERT INTO `sensitive_word_data` VALUES ('采阴补阳');
INSERT INTO `sensitive_word_data` VALUES ('里有汽枪出售');
INSERT INTO `sensitive_word_data` VALUES ('里鹏');
INSERT INTO `sensitive_word_data` VALUES ('重视');
INSERT INTO `sensitive_word_data` VALUES ('野营军刀出售');
INSERT INTO `sensitive_word_data` VALUES ('野营刀专卖');
INSERT INTO `sensitive_word_data` VALUES ('野营刀具专卖');
INSERT INTO `sensitive_word_data` VALUES ('野营刀具军品网');
INSERT INTO `sensitive_word_data` VALUES ('野营开山刀军刺');
INSERT INTO `sensitive_word_data` VALUES ('野营砍刀户外军刀');
INSERT INTO `sensitive_word_data` VALUES ('金人庆');
INSERT INTO `sensitive_word_data` VALUES ('金基鹏');
INSERT INTO `sensitive_word_data` VALUES ('金开诚');
INSERT INTO `sensitive_word_data` VALUES ('金异');
INSERT INTO `sensitive_word_data` VALUES ('金扎金');
INSERT INTO `sensitive_word_data` VALUES ('金日光');
INSERT INTO `sensitive_word_data` VALUES ('金炳华');
INSERT INTO `sensitive_word_data` VALUES ('金烈');
INSERT INTO `sensitive_word_data` VALUES ('金钟气');
INSERT INTO `sensitive_word_data` VALUES ('金鲁贤');
INSERT INTO `sensitive_word_data` VALUES ('针刺');
INSERT INTO `sensitive_word_data` VALUES ('针刺事');
INSERT INTO `sensitive_word_data` VALUES ('针刺伤');
INSERT INTO `sensitive_word_data` VALUES ('针刺案');
INSERT INTO `sensitive_word_data` VALUES ('针刺死');
INSERT INTO `sensitive_word_data` VALUES ('针孔摄像机 ');
INSERT INTO `sensitive_word_data` VALUES ('钟志根');
INSERT INTO `sensitive_word_data` VALUES ('钟起煌');
INSERT INTO `sensitive_word_data` VALUES ('钢珠弓弩专卖店');
INSERT INTO `sensitive_word_data` VALUES ('钢珠弓弩专卖网');
INSERT INTO `sensitive_word_data` VALUES ('钢珠枪');
INSERT INTO `sensitive_word_data` VALUES ('钢珠枪小口径步枪');
INSERT INTO `sensitive_word_data` VALUES ('钢针狗');
INSERT INTO `sensitive_word_data` VALUES ('钦定接班人');
INSERT INTO `sensitive_word_data` VALUES ('钮茂生');
INSERT INTO `sensitive_word_data` VALUES ('钱三字经');
INSERT INTO `sensitive_word_data` VALUES ('钱景仁');
INSERT INTO `sensitive_word_data` VALUES ('钱运录');
INSERT INTO `sensitive_word_data` VALUES ('铁凝');
INSERT INTO `sensitive_word_data` VALUES ('铃声');
INSERT INTO `sensitive_word_data` VALUES ('铅弹');
INSERT INTO `sensitive_word_data` VALUES ('铅弹 上海工字气枪');
INSERT INTO `sensitive_word_data` VALUES ('铭记印尼');
INSERT INTO `sensitive_word_data` VALUES ('银行卡复制设备 ');
INSERT INTO `sensitive_word_data` VALUES ('销售/专卖/买卖77式手枪');
INSERT INTO `sensitive_word_data` VALUES ('销售小口径步枪');
INSERT INTO `sensitive_word_data` VALUES ('销售气手狗');
INSERT INTO `sensitive_word_data` VALUES ('销售电手狗');
INSERT INTO `sensitive_word_data` VALUES ('销售运动步枪');
INSERT INTO `sensitive_word_data` VALUES ('锋同志');
INSERT INTO `sensitive_word_data` VALUES ('锡峰气枪出售');
INSERT INTO `sensitive_word_data` VALUES ('锦掏');
INSERT INTO `sensitive_word_data` VALUES ('锦涛');
INSERT INTO `sensitive_word_data` VALUES ('锦淘');
INSERT INTO `sensitive_word_data` VALUES ('镇压');
INSERT INTO `sensitive_word_data` VALUES ('长');
INSERT INTO `sensitive_word_data` VALUES ('长兄');
INSERT INTO `sensitive_word_data` VALUES ('长期出 售手枪');
INSERT INTO `sensitive_word_data` VALUES ('长狗 ');
INSERT INTO `sensitive_word_data` VALUES ('长腿');
INSERT INTO `sensitive_word_data` VALUES ('长驱直入');
INSERT INTO `sensitive_word_data` VALUES ('門服務');
INSERT INTO `sensitive_word_data` VALUES ('開碼');
INSERT INTO `sensitive_word_data` VALUES ('開票');
INSERT INTO `sensitive_word_data` VALUES ('门保健');
INSERT INTO `sensitive_word_data` VALUES ('门按摩');
INSERT INTO `sensitive_word_data` VALUES ('闭经');
INSERT INTO `sensitive_word_data` VALUES ('间质细胞');
INSERT INTO `sensitive_word_data` VALUES ('间质细胞刺激素');
INSERT INTO `sensitive_word_data` VALUES ('间质部');
INSERT INTO `sensitive_word_data` VALUES ('闵乃本');
INSERT INTO `sensitive_word_data` VALUES ('闵智亭');
INSERT INTO `sensitive_word_data` VALUES ('闷哼');
INSERT INTO `sensitive_word_data` VALUES ('闻封锁');
INSERT INTO `sensitive_word_data` VALUES ('闻被控制');
INSERT INTO `sensitive_word_data` VALUES ('阉割');
INSERT INTO `sensitive_word_data` VALUES ('阎洪臣');
INSERT INTO `sensitive_word_data` VALUES ('防卫刀具专卖');
INSERT INTO `sensitive_word_data` VALUES ('防卫刀具军品网');
INSERT INTO `sensitive_word_data` VALUES ('防卫刀具直销网');
INSERT INTO `sensitive_word_data` VALUES ('防卫棍刀出售');
INSERT INTO `sensitive_word_data` VALUES ('防卫棍刀户外刀具');
INSERT INTO `sensitive_word_data` VALUES ('防卫甩棍出售');
INSERT INTO `sensitive_word_data` VALUES ('防卫电棍出售');
INSERT INTO `sensitive_word_data` VALUES ('防卫著名军刀出售');
INSERT INTO `sensitive_word_data` VALUES ('防卫野营砍刀出售');
INSERT INTO `sensitive_word_data` VALUES ('防电子眼');
INSERT INTO `sensitive_word_data` VALUES ('防身手枪QQ');
INSERT INTO `sensitive_word_data` VALUES ('防身枪 ');
INSERT INTO `sensitive_word_data` VALUES ('防身武器手枪');
INSERT INTO `sensitive_word_data` VALUES ('防身药水');
INSERT INTO `sensitive_word_data` VALUES ('阳安江');
INSERT INTO `sensitive_word_data` VALUES ('阳江军品军刀网');
INSERT INTO `sensitive_word_data` VALUES ('阳江刀具专卖');
INSERT INTO `sensitive_word_data` VALUES ('阳江刀具军品网');
INSERT INTO `sensitive_word_data` VALUES ('阳江刀具批发网');
INSERT INTO `sensitive_word_data` VALUES ('阳江刀具直销网');
INSERT INTO `sensitive_word_data` VALUES ('阴唇 ');
INSERT INTO `sensitive_word_data` VALUES ('阴户 ');
INSERT INTO `sensitive_word_data` VALUES ('阴毛 ');
INSERT INTO `sensitive_word_data` VALUES ('阴茎');
INSERT INTO `sensitive_word_data` VALUES ('阴蒂 ');
INSERT INTO `sensitive_word_data` VALUES ('阴道 ');
INSERT INTO `sensitive_word_data` VALUES ('阴道被捅');
INSERT INTO `sensitive_word_data` VALUES ('阴间来电');
INSERT INTO `sensitive_word_data` VALUES ('阵阵快感');
INSERT INTO `sensitive_word_data` VALUES ('阻击枪/汽枪/高压气枪');
INSERT INTO `sensitive_word_data` VALUES ('阿兰得龙野营刀具网');
INSERT INTO `sensitive_word_data` VALUES ('阿兰德龙户外');
INSERT INTO `sensitive_word_data` VALUES ('阿兰德龙野营刀');
INSERT INTO `sensitive_word_data` VALUES ('阿共');
INSERT INTO `sensitive_word_data` VALUES ('阿凡提机 ');
INSERT INTO `sensitive_word_data` VALUES ('阿姨');
INSERT INTO `sensitive_word_data` VALUES ('阿宾');
INSERT INTO `sensitive_word_data` VALUES ('阿扁推翻');
INSERT INTO `sensitive_word_data` VALUES ('阿芙蓉');
INSERT INTO `sensitive_word_data` VALUES ('阿賓');
INSERT INTO `sensitive_word_data` VALUES ('附 睾');
INSERT INTO `sensitive_word_data` VALUES ('附件炎');
INSERT INTO `sensitive_word_data` VALUES ('附属性腺');
INSERT INTO `sensitive_word_data` VALUES ('附性腺分泌液');
INSERT INTO `sensitive_word_data` VALUES ('附睾');
INSERT INTO `sensitive_word_data` VALUES ('附睾丸');
INSERT INTO `sensitive_word_data` VALUES ('附睾小叶');
INSERT INTO `sensitive_word_data` VALUES ('附睾液');
INSERT INTO `sensitive_word_data` VALUES ('附睾炎');
INSERT INTO `sensitive_word_data` VALUES ('附睾管');
INSERT INTO `sensitive_word_data` VALUES ('附睾结核');
INSERT INTO `sensitive_word_data` VALUES ('附送枪');
INSERT INTO `sensitive_word_data` VALUES ('陆兵');
INSERT INTO `sensitive_word_data` VALUES ('陆同修');
INSERT INTO `sensitive_word_data` VALUES ('陆封锁');
INSERT INTO `sensitive_word_data` VALUES ('陆浩');
INSERT INTO `sensitive_word_data` VALUES ('陆玄霜');
INSERT INTO `sensitive_word_data` VALUES ('陆锡蕾');
INSERT INTO `sensitive_word_data` VALUES ('陈佳洱');
INSERT INTO `sensitive_word_data` VALUES ('陈佳贵');
INSERT INTO `sensitive_word_data` VALUES ('陈俊亮');
INSERT INTO `sensitive_word_data` VALUES ('陈军');
INSERT INTO `sensitive_word_data` VALUES ('陈凌孚');
INSERT INTO `sensitive_word_data` VALUES ('陈勋儒');
INSERT INTO `sensitive_word_data` VALUES ('陈同海');
INSERT INTO `sensitive_word_data` VALUES ('陈士能');
INSERT INTO `sensitive_word_data` VALUES ('陈奎元');
INSERT INTO `sensitive_word_data` VALUES ('陈子明');
INSERT INTO `sensitive_word_data` VALUES ('陈宗兴');
INSERT INTO `sensitive_word_data` VALUES ('陈宜瑜');
INSERT INTO `sensitive_word_data` VALUES ('陈小雅');
INSERT INTO `sensitive_word_data` VALUES ('陈广元');
INSERT INTO `sensitive_word_data` VALUES ('陈广文');
INSERT INTO `sensitive_word_data` VALUES ('陈建国');
INSERT INTO `sensitive_word_data` VALUES ('陈建生');
INSERT INTO `sensitive_word_data` VALUES ('陈德敏');
INSERT INTO `sensitive_word_data` VALUES ('陈德铭');
INSERT INTO `sensitive_word_data` VALUES ('陈心昭');
INSERT INTO `sensitive_word_data` VALUES ('陈抗甫');
INSERT INTO `sensitive_word_data` VALUES ('陈政立');
INSERT INTO `sensitive_word_data` VALUES ('陈昊苏');
INSERT INTO `sensitive_word_data` VALUES ('陈昌智');
INSERT INTO `sensitive_word_data` VALUES ('陈明德');
INSERT INTO `sensitive_word_data` VALUES ('陈晓铭');
INSERT INTO `sensitive_word_data` VALUES ('陈永棋');
INSERT INTO `sensitive_word_data` VALUES ('陈清华');
INSERT INTO `sensitive_word_data` VALUES ('陈清泰');
INSERT INTO `sensitive_word_data` VALUES ('陈炳德');
INSERT INTO `sensitive_word_data` VALUES ('陈益群');
INSERT INTO `sensitive_word_data` VALUES ('陈相贵');
INSERT INTO `sensitive_word_data` VALUES ('陈章良');
INSERT INTO `sensitive_word_data` VALUES ('陈绍基');
INSERT INTO `sensitive_word_data` VALUES ('陈耀邦');
INSERT INTO `sensitive_word_data` VALUES ('陈胜');
INSERT INTO `sensitive_word_data` VALUES ('陈至立');
INSERT INTO `sensitive_word_data` VALUES ('陈良宇');
INSERT INTO `sensitive_word_data` VALUES ('陈虹');
INSERT INTO `sensitive_word_data` VALUES ('陈辉光');
INSERT INTO `sensitive_word_data` VALUES ('陈邦柱');
INSERT INTO `sensitive_word_data` VALUES ('陈难先');
INSERT INTO `sensitive_word_data` VALUES ('陈高华');
INSERT INTO `sensitive_word_data` VALUES ('降低');
INSERT INTO `sensitive_word_data` VALUES ('限制言');
INSERT INTO `sensitive_word_data` VALUES ('限量');
INSERT INTO `sensitive_word_data` VALUES ('陪考枪');
INSERT INTO `sensitive_word_data` VALUES ('陪聊 ');
INSERT INTO `sensitive_word_data` VALUES ('陰唇');
INSERT INTO `sensitive_word_data` VALUES ('陰戶');
INSERT INTO `sensitive_word_data` VALUES ('陰道');
INSERT INTO `sensitive_word_data` VALUES ('陶伯钧');
INSERT INTO `sensitive_word_data` VALUES ('陶驷驹');
INSERT INTO `sensitive_word_data` VALUES ('陷害案');
INSERT INTO `sensitive_word_data` VALUES ('陷害罪');
INSERT INTO `sensitive_word_data` VALUES ('隆手指');
INSERT INTO `sensitive_word_data` VALUES ('隆起');
INSERT INTO `sensitive_word_data` VALUES ('隋明太');
INSERT INTO `sensitive_word_data` VALUES ('隐形喷剂');
INSERT INTO `sensitive_word_data` VALUES ('隐形摄像机');
INSERT INTO `sensitive_word_data` VALUES ('隐形耳');
INSERT INTO `sensitive_word_data` VALUES ('隐形耳机 ');
INSERT INTO `sensitive_word_data` VALUES ('隐瞒地震');
INSERT INTO `sensitive_word_data` VALUES ('隐蔽式摄像机 ');
INSERT INTO `sensitive_word_data` VALUES ('障碍');
INSERT INTO `sensitive_word_data` VALUES ('雄烯二醇');
INSERT INTO `sensitive_word_data` VALUES ('集体上访');
INSERT INTO `sensitive_word_data` VALUES ('集体打砸');
INSERT INTO `sensitive_word_data` VALUES ('集体腐');
INSERT INTO `sensitive_word_data` VALUES ('集体自杀');
INSERT INTO `sensitive_word_data` VALUES ('集团出售手枪');
INSERT INTO `sensitive_word_data` VALUES ('集团出售手枪气枪');
INSERT INTO `sensitive_word_data` VALUES ('集束炸弹制作');
INSERT INTO `sensitive_word_data` VALUES ('雌二醇');
INSERT INTO `sensitive_word_data` VALUES ('雌性激素');
INSERT INTO `sensitive_word_data` VALUES ('雌激素');
INSERT INTO `sensitive_word_data` VALUES ('零八奥运艰');
INSERT INTO `sensitive_word_data` VALUES ('雷人女官');
INSERT INTO `sensitive_word_data` VALUES ('雷管 ');
INSERT INTO `sensitive_word_data` VALUES ('雷管出售');
INSERT INTO `sensitive_word_data` VALUES ('雷蕾');
INSERT INTO `sensitive_word_data` VALUES ('雷鸣球');
INSERT INTO `sensitive_word_data` VALUES ('雾型迷');
INSERT INTO `sensitive_word_data` VALUES ('震其国土');
INSERT INTO `sensitive_word_data` VALUES ('震惊一个民');
INSERT INTO `sensitive_word_data` VALUES ('震惊全球');
INSERT INTO `sensitive_word_data` VALUES ('震死他们');
INSERT INTO `sensitive_word_data` VALUES ('霉疮');
INSERT INTO `sensitive_word_data` VALUES ('霉菌性阴道炎');
INSERT INTO `sensitive_word_data` VALUES ('霍英东');
INSERT INTO `sensitive_word_data` VALUES ('霍达');
INSERT INTO `sensitive_word_data` VALUES ('霰弹 ');
INSERT INTO `sensitive_word_data` VALUES ('露出');
INSERT INTO `sensitive_word_data` VALUES ('露阴和窥阴');
INSERT INTO `sensitive_word_data` VALUES ('露阴癖');
INSERT INTO `sensitive_word_data` VALUES ('青春期');
INSERT INTO `sensitive_word_data` VALUES ('靖志远');
INSERT INTO `sensitive_word_data` VALUES ('静坐');
INSERT INTO `sensitive_word_data` VALUES ('静香');
INSERT INTO `sensitive_word_data` VALUES ('非淋菌性尿道炎');
INSERT INTO `sensitive_word_data` VALUES ('革兰氏阳性细菌');
INSERT INTO `sensitive_word_data` VALUES ('靳尚谊');
INSERT INTO `sensitive_word_data` VALUES ('鞘膜腔');
INSERT INTO `sensitive_word_data` VALUES ('鞭满 ');
INSERT INTO `sensitive_word_data` VALUES ('韦家能');
INSERT INTO `sensitive_word_data` VALUES ('韩启德');
INSERT INTO `sensitive_word_data` VALUES ('韩喜凯');
INSERT INTO `sensitive_word_data` VALUES ('韩大建');
INSERT INTO `sensitive_word_data` VALUES ('韩寓群');
INSERT INTO `sensitive_word_data` VALUES ('韩忠朝');
INSERT INTO `sensitive_word_data` VALUES ('韩正');
INSERT INTO `sensitive_word_data` VALUES ('韩汝琦');
INSERT INTO `sensitive_word_data` VALUES ('韩生贵');
INSERT INTO `sensitive_word_data` VALUES ('韵律');
INSERT INTO `sensitive_word_data` VALUES ('韵徐娘');
INSERT INTO `sensitive_word_data` VALUES ('韶关斗');
INSERT INTO `sensitive_word_data` VALUES ('韶关旭');
INSERT INTO `sensitive_word_data` VALUES ('韶关玩');
INSERT INTO `sensitive_word_data` VALUES ('顶住');
INSERT INTO `sensitive_word_data` VALUES ('顶体素');
INSERT INTO `sensitive_word_data` VALUES ('顶体膜');
INSERT INTO `sensitive_word_data` VALUES ('顶体蛋白酶');
INSERT INTO `sensitive_word_data` VALUES ('顶体酶');
INSERT INTO `sensitive_word_data` VALUES ('顶帖器 ');
INSERT INTO `sensitive_word_data` VALUES ('顶弄');
INSERT INTO `sensitive_word_data` VALUES ('顶我');
INSERT INTO `sensitive_word_data` VALUES ('顶破');
INSERT INTO `sensitive_word_data` VALUES ('顶紧');
INSERT INTO `sensitive_word_data` VALUES ('顶花心');
INSERT INTO `sensitive_word_data` VALUES ('顶贴机 ');
INSERT INTO `sensitive_word_data` VALUES ('顶进');
INSERT INTO `sensitive_word_data` VALUES ('顶送');
INSERT INTO `sensitive_word_data` VALUES ('顾秀莲');
INSERT INTO `sensitive_word_data` VALUES ('领土拿');
INSERT INTO `sensitive_word_data` VALUES ('领导干部吃王八');
INSERT INTO `sensitive_word_data` VALUES ('频度');
INSERT INTO `sensitive_word_data` VALUES ('频率');
INSERT INTO `sensitive_word_data` VALUES ('频繁');
INSERT INTO `sensitive_word_data` VALUES ('颜射');
INSERT INTO `sensitive_word_data` VALUES ('颠鸾倒凤');
INSERT INTO `sensitive_word_data` VALUES ('颤动');
INSERT INTO `sensitive_word_data` VALUES ('颤抖');
INSERT INTO `sensitive_word_data` VALUES ('风流');
INSERT INTO `sensitive_word_data` VALUES ('风骚');
INSERT INTO `sensitive_word_data` VALUES ('飞溅');
INSERT INTO `sensitive_word_data` VALUES ('飞燕');
INSERT INTO `sensitive_word_data` VALUES ('食堂涨价');
INSERT INTO `sensitive_word_data` VALUES ('饥渴');
INSERT INTO `sensitive_word_data` VALUES ('饭菜涨价');
INSERT INTO `sensitive_word_data` VALUES ('饱胀');
INSERT INTO `sensitive_word_data` VALUES ('香港一类');
INSERT INTO `sensitive_word_data` VALUES ('香港彩');
INSERT INTO `sensitive_word_data` VALUES ('香港总彩');
INSERT INTO `sensitive_word_data` VALUES ('香港生子');
INSERT INTO `sensitive_word_data` VALUES ('香港论坛');
INSERT INTO `sensitive_word_data` VALUES ('香港马会');
INSERT INTO `sensitive_word_data` VALUES ('马万祺');
INSERT INTO `sensitive_word_data` VALUES ('马凯');
INSERT INTO `sensitive_word_data` VALUES ('马勒');
INSERT INTO `sensitive_word_data` VALUES ('马启智');
INSERT INTO `sensitive_word_data` VALUES ('马子');
INSERT INTO `sensitive_word_data` VALUES ('马庆生');
INSERT INTO `sensitive_word_data` VALUES ('马强');
INSERT INTO `sensitive_word_data` VALUES ('马志伟');
INSERT INTO `sensitive_word_data` VALUES ('马忠臣');
INSERT INTO `sensitive_word_data` VALUES ('马恺');
INSERT INTO `sensitive_word_data` VALUES ('马明哲');
INSERT INTO `sensitive_word_data` VALUES ('马杀鸡');
INSERT INTO `sensitive_word_data` VALUES ('马永伟');
INSERT INTO `sensitive_word_data` VALUES ('马眼');
INSERT INTO `sensitive_word_data` VALUES ('马英九 ');
INSERT INTO `sensitive_word_data` VALUES ('马飚');
INSERT INTO `sensitive_word_data` VALUES ('骂四川');
INSERT INTO `sensitive_word_data` VALUES ('骄穴');
INSERT INTO `sensitive_word_data` VALUES ('骄躯');
INSERT INTO `sensitive_word_data` VALUES ('骑乘位');
INSERT INTO `sensitive_word_data` VALUES ('骑单车出');
INSERT INTO `sensitive_word_data` VALUES ('骚乱');
INSERT INTO `sensitive_word_data` VALUES ('骚嘴');
INSERT INTO `sensitive_word_data` VALUES ('骚女');
INSERT INTO `sensitive_word_data` VALUES ('骚妇');
INSERT INTO `sensitive_word_data` VALUES ('骚浪');
INSERT INTO `sensitive_word_data` VALUES ('骚穴');
INSERT INTO `sensitive_word_data` VALUES ('骚逼');
INSERT INTO `sensitive_word_data` VALUES ('骨感');
INSERT INTO `sensitive_word_data` VALUES ('骨盆');
INSERT INTO `sensitive_word_data` VALUES ('骨盆腔');
INSERT INTO `sensitive_word_data` VALUES ('骷髅死');
INSERT INTO `sensitive_word_data` VALUES ('高亢');
INSERT INTO `sensitive_word_data` VALUES ('高凸');
INSERT INTO `sensitive_word_data` VALUES ('高占祥');
INSERT INTO `sensitive_word_data` VALUES ('高压气枪,气枪子弹');
INSERT INTO `sensitive_word_data` VALUES ('高压气枪|气枪货到付款');
INSERT INTO `sensitive_word_data` VALUES ('高压气枪|电狗专卖');
INSERT INTO `sensitive_word_data` VALUES ('高压气枪专卖店');
INSERT INTO `sensitive_word_data` VALUES ('高压狗QQ');
INSERT INTO `sensitive_word_data` VALUES ('高国才');
INSERT INTO `sensitive_word_data` VALUES ('高就在政');
INSERT INTO `sensitive_word_data` VALUES ('高强');
INSERT INTO `sensitive_word_data` VALUES ('高性能爆炸物质混合');
INSERT INTO `sensitive_word_data` VALUES ('高挺');
INSERT INTO `sensitive_word_data` VALUES ('高洪');
INSERT INTO `sensitive_word_data` VALUES ('高潮');
INSERT INTO `sensitive_word_data` VALUES ('高爆炸药基本配方');
INSERT INTO `sensitive_word_data` VALUES ('高爆炸药的基本配方');
INSERT INTO `sensitive_word_data` VALUES ('高考黑');
INSERT INTO `sensitive_word_data` VALUES ('高耸');
INSERT INTO `sensitive_word_data` VALUES ('高胀');
INSERT INTO `sensitive_word_data` VALUES ('高自联');
INSERT INTO `sensitive_word_data` VALUES ('高莺莺');
INSERT INTO `sensitive_word_data` VALUES ('鬼交');
INSERT INTO `sensitive_word_data` VALUES ('魂飞魄散');
INSERT INTO `sensitive_word_data` VALUES ('魏京生');
INSERT INTO `sensitive_word_data` VALUES ('魏复盛');
INSERT INTO `sensitive_word_data` VALUES ('鱼比目');
INSERT INTO `sensitive_word_data` VALUES ('鱼水');
INSERT INTO `sensitive_word_data` VALUES ('鲍彤');
INSERT INTO `sensitive_word_data` VALUES ('鲍筒');
INSERT INTO `sensitive_word_data` VALUES ('鸡吧');
INSERT INTO `sensitive_word_data` VALUES ('鸡奸');
INSERT INTO `sensitive_word_data` VALUES ('鸡尾酒炸弹制作');
INSERT INTO `sensitive_word_data` VALUES ('鸡巴');
INSERT INTO `sensitive_word_data` VALUES ('鸡巴顶住');
INSERT INTO `sensitive_word_data` VALUES ('鸡把');
INSERT INTO `sensitive_word_data` VALUES ('鸡鸡');
INSERT INTO `sensitive_word_data` VALUES ('鸥之歌');
INSERT INTO `sensitive_word_data` VALUES ('鸦片');
INSERT INTO `sensitive_word_data` VALUES ('鸳鸯洗');
INSERT INTO `sensitive_word_data` VALUES ('麦当劳被砸');
INSERT INTO `sensitive_word_data` VALUES ('麻古');
INSERT INTO `sensitive_word_data` VALUES ('麻将透');
INSERT INTO `sensitive_word_data` VALUES ('麻果丸');
INSERT INTO `sensitive_word_data` VALUES ('麻果配');
INSERT INTO `sensitive_word_data` VALUES ('麻痒');
INSERT INTO `sensitive_word_data` VALUES ('麻痹的');
INSERT INTO `sensitive_word_data` VALUES ('麻酥');
INSERT INTO `sensitive_word_data` VALUES ('麻酥酥');
INSERT INTO `sensitive_word_data` VALUES ('麻醉乙醚');
INSERT INTO `sensitive_word_data` VALUES ('麻醉弓弩专卖店');
INSERT INTO `sensitive_word_data` VALUES ('麻醉弹 ');
INSERT INTO `sensitive_word_data` VALUES ('麻醉枪');
INSERT INTO `sensitive_word_data` VALUES ('麻醉枪出售可验货');
INSERT INTO `sensitive_word_data` VALUES ('麻醉枪猎枪军用');
INSERT INTO `sensitive_word_data` VALUES ('麻醉槍');
INSERT INTO `sensitive_word_data` VALUES ('麻醉狗');
INSERT INTO `sensitive_word_data` VALUES ('麻醉药');
INSERT INTO `sensitive_word_data` VALUES ('麻醉藥');
INSERT INTO `sensitive_word_data` VALUES ('麻黄草');
INSERT INTO `sensitive_word_data` VALUES ('黄JU');
INSERT INTO `sensitive_word_data` VALUES ('黄丽满');
INSERT INTO `sensitive_word_data` VALUES ('黄体生成素');
INSERT INTO `sensitive_word_data` VALUES ('黄体酮');
INSERT INTO `sensitive_word_data` VALUES ('黄光汉');
INSERT INTO `sensitive_word_data` VALUES ('黄关从');
INSERT INTO `sensitive_word_data` VALUES ('黄兴国');
INSERT INTO `sensitive_word_data` VALUES ('黄冰');
INSERT INTO `sensitive_word_data` VALUES ('黄华华');
INSERT INTO `sensitive_word_data` VALUES ('黄孟复');
INSERT INTO `sensitive_word_data` VALUES ('黄小晶');
INSERT INTO `sensitive_word_data` VALUES ('黄巨');
INSERT INTO `sensitive_word_data` VALUES ('黄康生');
INSERT INTO `sensitive_word_data` VALUES ('黄智权');
INSERT INTO `sensitive_word_data` VALUES ('黄格胜');
INSERT INTO `sensitive_word_data` VALUES ('黄璜');
INSERT INTO `sensitive_word_data` VALUES ('黄疽');
INSERT INTO `sensitive_word_data` VALUES ('黄色电影 ');
INSERT INTO `sensitive_word_data` VALUES ('黄菊');
INSERT INTO `sensitive_word_data` VALUES ('黎乐民');
INSERT INTO `sensitive_word_data` VALUES ('黎阳平');
INSERT INTO `sensitive_word_data` VALUES ('黑毛');
INSERT INTO `sensitive_word_data` VALUES ('黑洞');
INSERT INTO `sensitive_word_data` VALUES ('黑火药和硝酸钾农药混合');
INSERT INTO `sensitive_word_data` VALUES ('黑火药比例');
INSERT INTO `sensitive_word_data` VALUES ('黑火药的');
INSERT INTO `sensitive_word_data` VALUES ('黑火药的制作');
INSERT INTO `sensitive_word_data` VALUES ('黑火药的成分');
INSERT INTO `sensitive_word_data` VALUES ('黑火药的配方');
INSERT INTO `sensitive_word_data` VALUES ('黑火药配方');
INSERT INTO `sensitive_word_data` VALUES ('黑火药配方比例');
INSERT INTO `sensitive_word_data` VALUES ('黑索金的制造过程');
INSERT INTO `sensitive_word_data` VALUES ('黑色的阴毛');
INSERT INTO `sensitive_word_data` VALUES ('黑黑的阴毛');
INSERT INTO `sensitive_word_data` VALUES ('鼓动一些');
INSERT INTO `sensitive_word_data` VALUES ('鼓胀');
INSERT INTO `sensitive_word_data` VALUES ('齐续春');
INSERT INTO `sensitive_word_data` VALUES ('龙小霞');
INSERT INTO `sensitive_word_data` VALUES ('龙新民');
INSERT INTO `sensitive_word_data` VALUES ('龙根');
INSERT INTO `sensitive_word_data` VALUES ('龙湾事件');
INSERT INTO `sensitive_word_data` VALUES ('龚世萍');
INSERT INTO `sensitive_word_data` VALUES ('龚学平');
INSERT INTO `sensitive_word_data` VALUES ('龚谷成');
INSERT INTO `sensitive_word_data` VALUES ('龟 头');
INSERT INTO `sensitive_word_data` VALUES ('龟头');
INSERT INTO `sensitive_word_data` VALUES ('龟头冠状沟');
INSERT INTO `sensitive_word_data` VALUES ('龟头固定药疹');
INSERT INTO `sensitive_word_data` VALUES ('龟头炎');
INSERT INTO `sensitive_word_data` VALUES ('龟头珍珠垢');
INSERT INTO `sensitive_word_data` VALUES ('龟头结核疹');
INSERT INTO `sensitive_word_data` VALUES ('龟腾');

-- ----------------------------
-- Table structure for server_state
-- ----------------------------
DROP TABLE IF EXISTS `server_state`;
CREATE TABLE `server_state`  (
  `key_name` varchar(100) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL DEFAULT '' COMMENT 'KEY名称',
  `int_value` int(11) UNSIGNED NOT NULL DEFAULT 0 COMMENT '数字值',
  `list_value` varchar(200) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL DEFAULT '[]' COMMENT '列表值',
  `string_value` varchar(100) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL DEFAULT '' COMMENT '字符串值',
  PRIMARY KEY (`key_name`) USING BTREE
) ENGINE = InnoDB CHARACTER SET = utf8mb4 COLLATE = utf8mb4_general_ci COMMENT = '服务器状态表' ROW_FORMAT = Dynamic;

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
) ENGINE = InnoDB CHARACTER SET = utf8mb4 COLLATE = utf8mb4_general_ci COMMENT = '角色商店表' ROW_FORMAT = Compact;

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
  `bind` tinyint(1) UNSIGNED NOT NULL DEFAULT 0 COMMENT '是否绑定',
  `level` int(10) UNSIGNED NOT NULL DEFAULT 0 COMMENT '等级限制',
  `limit` int(10) UNSIGNED NOT NULL DEFAULT 0 COMMENT '购买上限',
  `vip_level` int(10) UNSIGNED NOT NULL DEFAULT 0 COMMENT 'vip等级限购',
  `vip_limit` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL DEFAULT '' COMMENT 'vip等级购买上限',
  `description` char(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL DEFAULT '' COMMENT '描述',
  PRIMARY KEY (`shop_id`) USING BTREE
) ENGINE = InnoDB CHARACTER SET = utf8mb4 COLLATE = utf8mb4_general_ci COMMENT = '商店配置表' ROW_FORMAT = Compact;

-- ----------------------------
-- Records of shop_data
-- ----------------------------
INSERT INTO `shop_data` VALUES (1, 1, 1, 'gold', 10, 1, 0, 0, 0, 0, '', '');

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
  `daily_time` int(10) UNSIGNED NOT NULL DEFAULT 0 COMMENT '零点时间',
  PRIMARY KEY (`id`) USING BTREE
) ENGINE = InnoDB AUTO_INCREMENT = 1 CHARACTER SET = utf8mb4 COLLATE = utf8mb4_general_ci COMMENT = '商店日志表' ROW_FORMAT = Dynamic;

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

-- ----------------------------
-- Table structure for skill_data
-- ----------------------------
DROP TABLE IF EXISTS `skill_data`;
CREATE TABLE `skill_data`  (
  `skill_id` int(10) UNSIGNED NOT NULL DEFAULT 0 COMMENT '技能ID',
  `group_id` int(10) UNSIGNED NOT NULL DEFAULT 0 COMMENT '组ID',
  `type` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL DEFAULT '' COMMENT '类型(validate(skill_type))',
  `name` char(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL DEFAULT '' COMMENT '名字',
  `condition` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL DEFAULT '' COMMENT '学习条件',
  `stuff` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL DEFAULT '' COMMENT '升级材料',
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
INSERT INTO `skill_data` VALUES (1, 1, 'active', '普攻技能', '', '', '[1]', 1, 100, 100, 1, '', '', '', '', '对目标造成180%的伤害');
INSERT INTO `skill_data` VALUES (2, 2, 'active', '群攻技能', '', '', '[2]', 1, 100, 100, 3, '', '', '', '', '对3个目标造成150%的伤害');
INSERT INTO `skill_data` VALUES (3, 3, 'passive', '增益', '', '', '[8]', 10, 100, 100, 1, '', '', '', '', '每秒扣血，总血量万分之50');
INSERT INTO `skill_data` VALUES (5, 5, 'active', '普攻技能', '', '', '', 1, 100, 100, 1, '', '', '', '', '普通技能');

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
-- Table structure for validity_data
-- ----------------------------
DROP TABLE IF EXISTS `validity_data`;
CREATE TABLE `validity_data`  (
  `type` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL DEFAULT '' COMMENT '类型',
  `key` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL DEFAULT '' COMMENT '键',
  `value` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL DEFAULT '' COMMENT '值',
  `description` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL DEFAULT '' COMMENT '描述',
  PRIMARY KEY (`type`, `key`) USING BTREE
) ENGINE = InnoDB CHARACTER SET = utf8mb4 COLLATE = utf8mb4_general_ci COMMENT = '数据校验配置表' ROW_FORMAT = Dynamic;

-- ----------------------------
-- Records of validity_data
-- ----------------------------
INSERT INTO `validity_data` VALUES ('activity_service', '', '无', '无');
INSERT INTO `validity_data` VALUES ('act_script', 'enemy', '敌人', '敌人');
INSERT INTO `validity_data` VALUES ('act_script', 'location', '位置', '位置');
INSERT INTO `validity_data` VALUES ('act_script', 'monster', '怪物', '怪物');
INSERT INTO `validity_data` VALUES ('act_script', 'role', '玩家', '玩家');
INSERT INTO `validity_data` VALUES ('act_type', 'active', '主动', '主动');
INSERT INTO `validity_data` VALUES ('act_type', 'fix', '固定', '固定');
INSERT INTO `validity_data` VALUES ('act_type', 'movable', '移动', '移动');
INSERT INTO `validity_data` VALUES ('act_type', 'passive', '被动', '被动');
INSERT INTO `validity_data` VALUES ('classes', '0', '无限制', '职业');
INSERT INTO `validity_data` VALUES ('classes', '1', '七杀', '职业');
INSERT INTO `validity_data` VALUES ('classes', '2', '天师', '职业');
INSERT INTO `validity_data` VALUES ('classes', '3', '飞羽', '职业');
INSERT INTO `validity_data` VALUES ('classes', '4', '御灵', '职业');
INSERT INTO `validity_data` VALUES ('classes', '5', '妙音', '职业');
INSERT INTO `validity_data` VALUES ('classes', '6', '星术', '职业');
INSERT INTO `validity_data` VALUES ('compare', 'eq', '等于', '比较模式');
INSERT INTO `validity_data` VALUES ('compare', 'ge', '大于等于', '比较模式');
INSERT INTO `validity_data` VALUES ('compare', 'gt', '大于', '比较模式');
INSERT INTO `validity_data` VALUES ('compare', 'le', '小于等于', '比较模式');
INSERT INTO `validity_data` VALUES ('compare', 'lt', '小于', '比较模式');
INSERT INTO `validity_data` VALUES ('compare', 'nc', '不比较', '比较模式');
INSERT INTO `validity_data` VALUES ('compare', 'ne', '不等于', '比较模式');
INSERT INTO `validity_data` VALUES ('effect_attribute', 'Asset', '资产', '效果属性');
INSERT INTO `validity_data` VALUES ('effect_attribute', 'Attribute', '属性', '效果属性');
INSERT INTO `validity_data` VALUES ('effect_attribute', 'Buff', 'Buff', '效果属性');
INSERT INTO `validity_data` VALUES ('effect_attribute', 'Hurt', '伤害', '效果属性');
INSERT INTO `validity_data` VALUES ('effect_attribute', 'Skill', '技能', '效果属性');
INSERT INTO `validity_data` VALUES ('effect_field', '', '无', '效果属性字段');
INSERT INTO `validity_data` VALUES ('effect_field', 'accuracy', '命中', '效果属性字段');
INSERT INTO `validity_data` VALUES ('effect_field', 'ack_elements', '元素攻击', '效果属性字段');
INSERT INTO `validity_data` VALUES ('effect_field', 'ack_jewelry', '圣器(首饰)攻击', '效果属性字段');
INSERT INTO `validity_data` VALUES ('effect_field', 'ack_weapon', '武器攻击', '效果属性字段');
INSERT INTO `validity_data` VALUES ('effect_field', 'act_hurt_max', '伤害上限', '效果属性字段');
INSERT INTO `validity_data` VALUES ('effect_field', 'act_hurt_min', '伤害下限', '效果属性字段');
INSERT INTO `validity_data` VALUES ('effect_field', 'add_att_per_1', '每1级攻击+n整数', '效果属性字段');
INSERT INTO `validity_data` VALUES ('effect_field', 'add_att_per_2', '每2级攻击+n整数', '效果属性字段');
INSERT INTO `validity_data` VALUES ('effect_field', 'add_att_per_3', '每3级攻击+n整数', '效果属性字段');
INSERT INTO `validity_data` VALUES ('effect_field', 'add_def_per_1', '每1级防御+n整数', '效果属性字段');
INSERT INTO `validity_data` VALUES ('effect_field', 'add_def_per_2', '每2级防御+n整数', '效果属性字段');
INSERT INTO `validity_data` VALUES ('effect_field', 'add_def_per_3', '每3级防御+n整数', '效果属性字段');
INSERT INTO `validity_data` VALUES ('effect_field', 'add_hp_per_1', '每1级生命+n整数', '效果属性字段');
INSERT INTO `validity_data` VALUES ('effect_field', 'add_hp_per_2', '每2级生命+n整数', '效果属性字段');
INSERT INTO `validity_data` VALUES ('effect_field', 'add_hp_per_3', '每3级生命+n整数', '效果属性字段');
INSERT INTO `validity_data` VALUES ('effect_field', 'agility', '智力', '效果属性字段');
INSERT INTO `validity_data` VALUES ('effect_field', 'anti_control', '控制抵抗', '效果属性字段');
INSERT INTO `validity_data` VALUES ('effect_field', 'anti_escape', '抗逃跑率', '效果属性字段');
INSERT INTO `validity_data` VALUES ('effect_field', 'atk_speed', '攻击速度', '效果属性字段');
INSERT INTO `validity_data` VALUES ('effect_field', 'attack', '攻击', '效果属性字段');
INSERT INTO `validity_data` VALUES ('effect_field', 'attack_add_hp_fixed', '每一击回血', '效果属性字段');
INSERT INTO `validity_data` VALUES ('effect_field', 'attack_add_hp_fixed_by_level', '根据等级的每一击回血(整数)', '效果属性字段');
INSERT INTO `validity_data` VALUES ('effect_field', 'attack_add_hp_fixed_only_pvp', '每一击回血PVP', '效果属性字段');
INSERT INTO `validity_data` VALUES ('effect_field', 'attack_add_hp_per', '攻击自身回血百分比', '效果属性字段');
INSERT INTO `validity_data` VALUES ('effect_field', 'attack_fixed', '固定加伤(整数)绝对攻击', '效果属性字段');
INSERT INTO `validity_data` VALUES ('effect_field', 'attack_fixed_by_level', '根据等级的固定加伤(整数)', '效果属性字段');
INSERT INTO `validity_data` VALUES ('effect_field', 'attack_max', '最大攻击固定值', '效果属性字段');
INSERT INTO `validity_data` VALUES ('effect_field', 'attack_min', '最小攻击固定值', '效果属性字段');
INSERT INTO `validity_data` VALUES ('effect_field', 'attack_speed', '攻速', '效果属性字段');
INSERT INTO `validity_data` VALUES ('effect_field', 'base_hp', '基础生命', '效果属性字段');
INSERT INTO `validity_data` VALUES ('effect_field', 'be_attack_add_hp_per', '被击者回血百分比', '效果属性字段');
INSERT INTO `validity_data` VALUES ('effect_field', 'capture', '抓捕概率', '效果属性字段');
INSERT INTO `validity_data` VALUES ('effect_field', 'combo_attack_rate', '连击几率', '效果属性字段');
INSERT INTO `validity_data` VALUES ('effect_field', 'constitution', '体质', '效果属性字段');
INSERT INTO `validity_data` VALUES ('effect_field', 'copper_rate', '铜币倍率', '效果属性字段');
INSERT INTO `validity_data` VALUES ('effect_field', 'counter_ack_fixed', '反射伤害值(固定)', '效果属性字段');
INSERT INTO `validity_data` VALUES ('effect_field', 'counter_ack_per', '反射伤害值(万分比)', '效果属性字段');
INSERT INTO `validity_data` VALUES ('effect_field', 'critical', '暴击', '效果属性字段');
INSERT INTO `validity_data` VALUES ('effect_field', 'critical_dmg', '暴击伤害', '效果属性字段');
INSERT INTO `validity_data` VALUES ('effect_field', 'critical_hit_add_fixed', '会心伤害加成(固定值)', '效果属性字段');
INSERT INTO `validity_data` VALUES ('effect_field', 'critical_hit_add_per', '会心伤害加成(百分数)', '效果属性字段');
INSERT INTO `validity_data` VALUES ('effect_field', 'critical_hit_dec_fixed', '会心伤害减免(固定值)', '效果属性字段');
INSERT INTO `validity_data` VALUES ('effect_field', 'critical_hit_dec_per', '会心伤害减免(百分数)', '效果属性字段');
INSERT INTO `validity_data` VALUES ('effect_field', 'critical_hit_rate', '会心几率百分比', '效果属性字段');
INSERT INTO `validity_data` VALUES ('effect_field', 'defense', '防御', '效果属性字段');
INSERT INTO `validity_data` VALUES ('effect_field', 'defense_fixed', '固定免伤(整数)绝对防御', '效果属性字段');
INSERT INTO `validity_data` VALUES ('effect_field', 'def_armor', '防具防御', '效果属性字段');
INSERT INTO `validity_data` VALUES ('effect_field', 'def_elements', '元素防御', '效果属性字段');
INSERT INTO `validity_data` VALUES ('effect_field', 'def_ratio', '伤害减免', '效果属性字段');
INSERT INTO `validity_data` VALUES ('effect_field', 'destroy', '毁灭', '效果属性字段');
INSERT INTO `validity_data` VALUES ('effect_field', 'dexterity', '敏捷', '效果属性字段');
INSERT INTO `validity_data` VALUES ('effect_field', 'diligence_rate', '抗暴率(百分数)', '效果属性字段');
INSERT INTO `validity_data` VALUES ('effect_field', 'dmg_ratio', '伤害加成', '效果属性字段');
INSERT INTO `validity_data` VALUES ('effect_field', 'duck', '闪避', '效果属性字段');
INSERT INTO `validity_data` VALUES ('effect_field', 'duck_rate', '闪避率', '效果属性字段');
INSERT INTO `validity_data` VALUES ('effect_field', 'eff_heal_ratio', '被治疗效果', '效果属性字段');
INSERT INTO `validity_data` VALUES ('effect_field', 'endurance', '耐力', '效果属性字段');
INSERT INTO `validity_data` VALUES ('effect_field', 'enhance_control', '控制加强', '效果属性字段');
INSERT INTO `validity_data` VALUES ('effect_field', 'escape', '逃跑率', '效果属性字段');
INSERT INTO `validity_data` VALUES ('effect_field', 'evasion', '闪避', '效果属性字段');
INSERT INTO `validity_data` VALUES ('effect_field', 'exp_rate', '经验倍率', '效果属性字段');
INSERT INTO `validity_data` VALUES ('effect_field', 'exp_ratio', '经验加成', '效果属性字段');
INSERT INTO `validity_data` VALUES ('effect_field', 'fc', '战力', '效果属性字段');
INSERT INTO `validity_data` VALUES ('effect_field', 'freeze', '冰冻', '效果属性字段');
INSERT INTO `validity_data` VALUES ('effect_field', 'heal_ratio', '治疗效果', '效果属性字段');
INSERT INTO `validity_data` VALUES ('effect_field', 'heal_val', '治疗加强', '效果属性字段');
INSERT INTO `validity_data` VALUES ('effect_field', 'hit', '命中', '效果属性字段');
INSERT INTO `validity_data` VALUES ('effect_field', 'hit_rate', '命中率', '效果属性字段');
INSERT INTO `validity_data` VALUES ('effect_field', 'holy_dmg', '神圣伤害', '效果属性字段');
INSERT INTO `validity_data` VALUES ('effect_field', 'hp', '血量', '效果属性字段');
INSERT INTO `validity_data` VALUES ('effect_field', 'hp_armor', '防具生命', '效果属性字段');
INSERT INTO `validity_data` VALUES ('effect_field', 'hp_fastening', '不能回血', '效果属性字段');
INSERT INTO `validity_data` VALUES ('effect_field', 'hp_max', '生命', '效果属性字段');
INSERT INTO `validity_data` VALUES ('effect_field', 'hurt_add_per', '伤害加成(百分数)', '效果属性字段');
INSERT INTO `validity_data` VALUES ('effect_field', 'hurt_add_per_4_show', '显示用的额外的伤害加成(百分数)', '效果属性字段');
INSERT INTO `validity_data` VALUES ('effect_field', 'hurt_dec_per', '伤害减免(百分数)', '效果属性字段');
INSERT INTO `validity_data` VALUES ('effect_field', 'ignore_def_rate', '无视防御比例(百分数)', '效果属性字段');
INSERT INTO `validity_data` VALUES ('effect_field', 'ignore_strike_hurt_add_per', '无视一击伤害加成(百分比)', '效果属性字段');
INSERT INTO `validity_data` VALUES ('effect_field', 'ignore_strike_hurt_dec_per', '无视一击伤害减免(百分比)', '效果属性字段');
INSERT INTO `validity_data` VALUES ('effect_field', 'ignore_strike_rate', '无视一击几率', '效果属性字段');
INSERT INTO `validity_data` VALUES ('effect_field', 'intellect', '智力', '效果属性字段');
INSERT INTO `validity_data` VALUES ('effect_field', 'invincibility', '无敌(不会受伤)', '效果属性字段');
INSERT INTO `validity_data` VALUES ('effect_field', 'is_multiple_hurt', '2倍伤害被动技能,数值为伤害倍数', '效果属性字段');
INSERT INTO `validity_data` VALUES ('effect_field', 'kill_mon_copper', '杀怪加铜币比例', '效果属性字段');
INSERT INTO `validity_data` VALUES ('effect_field', 'kill_mon_exp', '杀怪加经验比例', '效果属性字段');
INSERT INTO `validity_data` VALUES ('effect_field', 'magic', '魔法', '效果属性字段');
INSERT INTO `validity_data` VALUES ('effect_field', 'magic_accuracy', '魔法命中', '效果属性字段');
INSERT INTO `validity_data` VALUES ('effect_field', 'magic_critical', '魔法暴击', '效果属性字段');
INSERT INTO `validity_data` VALUES ('effect_field', 'magic_def', '魔防', '效果属性字段');
INSERT INTO `validity_data` VALUES ('effect_field', 'magic_defense', '法术防御', '效果属性字段');
INSERT INTO `validity_data` VALUES ('effect_field', 'magic_def_ratio', '魔法伤害减免', '效果属性字段');
INSERT INTO `validity_data` VALUES ('effect_field', 'magic_dmg', '魔攻', '效果属性字段');
INSERT INTO `validity_data` VALUES ('effect_field', 'magic_dmg_ratio', '魔法伤害加成', '效果属性字段');
INSERT INTO `validity_data` VALUES ('effect_field', 'magic_evasion', '魔法闪避', '效果属性字段');
INSERT INTO `validity_data` VALUES ('effect_field', 'magic_tenacity', '魔法坚韧', '效果属性字段');
INSERT INTO `validity_data` VALUES ('effect_field', 'move_speed', '移动速度固定值', '效果属性字段');
INSERT INTO `validity_data` VALUES ('effect_field', 'mp_max', '魔法', '效果属性字段');
INSERT INTO `validity_data` VALUES ('effect_field', 'paralysis', '麻痹几率', '效果属性字段');
INSERT INTO `validity_data` VALUES ('effect_field', 'parry_per', '格挡几率', '效果属性字段');
INSERT INTO `validity_data` VALUES ('effect_field', 'passive_add_attack_by_dex', '被动按基础加敏捷', '效果属性字段');
INSERT INTO `validity_data` VALUES ('effect_field', 'passive_add_buff_when_low_hp', '被动：生命值低于30时自动触发buff', '效果属性字段');
INSERT INTO `validity_data` VALUES ('effect_field', 'passive_add_counter_ack_by_pow', '被动：反射伤害值=自身力量*2', '效果属性字段');
INSERT INTO `validity_data` VALUES ('effect_field', 'passive_add_def_by_pow', '被动按基础加力量', '效果属性字段');
INSERT INTO `validity_data` VALUES ('effect_field', 'passive_add_duck_by_dex', '被动按基础闪避加敏捷', '效果属性字段');
INSERT INTO `validity_data` VALUES ('effect_field', 'passive_add_hp_by_int', '被动按基础加值', '效果属性字段');
INSERT INTO `validity_data` VALUES ('effect_field', 'passive_add_hp_by_per', '自身(X)生命上限', '效果属性字段');
INSERT INTO `validity_data` VALUES ('effect_field', 'passive_add_min_attack', '被动按基础属性加攻击', '效果属性字段');
INSERT INTO `validity_data` VALUES ('effect_field', 'passive_add_skill_hurt_when_duck', '被动：闪避一次后，下一次攻击技能伤害提高200。(PVP生效),值存{伤害例,冷却时间},非0生效', '效果属性字段');
INSERT INTO `validity_data` VALUES ('effect_field', 'passive_fan_recover_be_hit', '扇子的受击满血被动', '效果属性字段');
INSERT INTO `validity_data` VALUES ('effect_field', 'passive_power_hit_must_next', '触发暴击时，下一次攻击必触发暴击,值存冷却时间,非-1生效', '效果属性字段');
INSERT INTO `validity_data` VALUES ('effect_field', 'passive_protect', '被动：队友血量低于20时，可以代替他承受伤害(值存{CD,要求血量比,持续时间,免伤比例,技能冷却时间})', '效果属性字段');
INSERT INTO `validity_data` VALUES ('effect_field', 'pet_dead_boom', '侍女死亡释放技能', '效果属性字段');
INSERT INTO `validity_data` VALUES ('effect_field', 'pet_protect_per', '侍女分担伤害', '效果属性字段');
INSERT INTO `validity_data` VALUES ('effect_field', 'physic_accuracy', '物理命中', '效果属性字段');
INSERT INTO `validity_data` VALUES ('effect_field', 'physic_critical', '物理暴击', '效果属性字段');
INSERT INTO `validity_data` VALUES ('effect_field', 'physic_def', '物防', '效果属性字段');
INSERT INTO `validity_data` VALUES ('effect_field', 'physic_def_ratio', '物理伤害减免', '效果属性字段');
INSERT INTO `validity_data` VALUES ('effect_field', 'physic_dmg', '物攻', '效果属性字段');
INSERT INTO `validity_data` VALUES ('effect_field', 'physic_dmg_ratio', '物理伤害加成', '效果属性字段');
INSERT INTO `validity_data` VALUES ('effect_field', 'physic_evasion', '物理闪避', '效果属性字段');
INSERT INTO `validity_data` VALUES ('effect_field', 'physic_tenacity', '物理坚韧', '效果属性字段');
INSERT INTO `validity_data` VALUES ('effect_field', 'power', '力量', '效果属性字段');
INSERT INTO `validity_data` VALUES ('effect_field', 'power_hit_add_fixed', '暴伤加成(固定值)', '效果属性字段');
INSERT INTO `validity_data` VALUES ('effect_field', 'power_hit_add_per', '暴伤加成(百分数)', '效果属性字段');
INSERT INTO `validity_data` VALUES ('effect_field', 'power_hit_dec_fixed', '暴伤减免(固定值)', '效果属性字段');
INSERT INTO `validity_data` VALUES ('effect_field', 'power_hit_dec_per', '暴伤减免(百分数)', '效果属性字段');
INSERT INTO `validity_data` VALUES ('effect_field', 'power_hit_rate', '暴击几率百分比', '效果属性字段');
INSERT INTO `validity_data` VALUES ('effect_field', 'reborn', '重生,值存冷却时间,非-1生效', '效果属性字段');
INSERT INTO `validity_data` VALUES ('effect_field', 'reduce_speed', '减速几率', '效果属性字段');
INSERT INTO `validity_data` VALUES ('effect_field', 'resist_control', '控制抵抗', '效果属性字段');
INSERT INTO `validity_data` VALUES ('effect_field', 'resist_critical_hit', '会心抵抗百分比', '效果属性字段');
INSERT INTO `validity_data` VALUES ('effect_field', 'resist_ignore_def', '无视防御抵抗(百分数)', '效果属性字段');
INSERT INTO `validity_data` VALUES ('effect_field', 'resist_paralysis', '麻痹抵抗', '效果属性字段');
INSERT INTO `validity_data` VALUES ('effect_field', 'resist_reduce_speed', '减速抵抗', '效果属性字段');
INSERT INTO `validity_data` VALUES ('effect_field', 'resist_silence', '沉默抵抗', '效果属性字段');
INSERT INTO `validity_data` VALUES ('effect_field', 'resist_vertigo', '眩晕抵抗', '效果属性字段');
INSERT INTO `validity_data` VALUES ('effect_field', 'shield_can_boom', '满值后爆炸的盾,非0生效,值存{技能组id,吸收系数}', '效果属性字段');
INSERT INTO `validity_data` VALUES ('effect_field', 'silence', '沉默几率', '效果属性字段');
INSERT INTO `validity_data` VALUES ('effect_field', 'skill_hurt', '技能固定伤害(整数)', '效果属性字段');
INSERT INTO `validity_data` VALUES ('effect_field', 'skill_hurt_add_per', '技能伤害', '效果属性字段');
INSERT INTO `validity_data` VALUES ('effect_field', 'skill_hurt_per', '技能伤害比例(百分数)', '效果属性字段');
INSERT INTO `validity_data` VALUES ('effect_field', 'speed', '移动速度', '效果属性字段');
INSERT INTO `validity_data` VALUES ('effect_field', 'strength', '力量', '效果属性字段');
INSERT INTO `validity_data` VALUES ('effect_field', 'suck_hp', '吸血', '效果属性字段');
INSERT INTO `validity_data` VALUES ('effect_field', 'target_hurt_max', '损害上限', '效果属性字段');
INSERT INTO `validity_data` VALUES ('effect_field', 'target_hurt_min', '损害下限', '效果属性字段');
INSERT INTO `validity_data` VALUES ('effect_field', 'tenacity', '坚韧', '效果属性字段');
INSERT INTO `validity_data` VALUES ('effect_field', 'total_hp', '生命', '效果属性字段');
INSERT INTO `validity_data` VALUES ('effect_field', 'total_mp', '总法力值', '效果属性字段');
INSERT INTO `validity_data` VALUES ('effect_field', 'use_skill_when_dead', '自身死亡后释放技能，仅对玩家有效,值存{技能id,冷却时间},非0生效', '效果属性字段');
INSERT INTO `validity_data` VALUES ('effect_field', 'vertigo', '眩晕几率', '效果属性字段');
INSERT INTO `validity_data` VALUES ('effect_field', 'vitality', '体力', '效果属性字段');
INSERT INTO `validity_data` VALUES ('effect_object', 'Mate', '队友', '效果对象');
INSERT INTO `validity_data` VALUES ('effect_object', 'Rival', '对方', '效果对象');
INSERT INTO `validity_data` VALUES ('effect_object', 'Self', '自己', '效果对象');
INSERT INTO `validity_data` VALUES ('effect_operation', 'add', '增加', '效果操作');
INSERT INTO `validity_data` VALUES ('effect_operation', 'clear', '清除', '效果操作');
INSERT INTO `validity_data` VALUES ('effect_operation', 'reduce', '减少', '效果操作');
INSERT INTO `validity_data` VALUES ('effect_operation', 'set', '设置', '效果操作');
INSERT INTO `validity_data` VALUES ('effect_scope', 'battle', '战斗', '效果范围');
INSERT INTO `validity_data` VALUES ('effect_scope', 'user', '玩家', '效果范围');
INSERT INTO `validity_data` VALUES ('effect_type', 'active', '主动', '效果类型');
INSERT INTO `validity_data` VALUES ('effect_type', 'buff', 'Buff', '效果类型');
INSERT INTO `validity_data` VALUES ('effect_type', 'passive', '被动', '效果类型');
INSERT INTO `validity_data` VALUES ('event', 'event_add_friend', '添加好友', '添加好友');
INSERT INTO `validity_data` VALUES ('event', 'event_guild_join', '加入公会', '事件');
INSERT INTO `validity_data` VALUES ('event', 'event_kill_monster', '杀怪', '事件');
INSERT INTO `validity_data` VALUES ('event', 'event_level_upgrade', '升级', '事件');
INSERT INTO `validity_data` VALUES ('event', 'event_pass_dungeon', '通关副本', '事件');
INSERT INTO `validity_data` VALUES ('event', 'event_shop_buy', '商店购买', '事件');
INSERT INTO `validity_data` VALUES ('map_rank_key', 'camp', '阵营', '阵营');
INSERT INTO `validity_data` VALUES ('map_rank_key', 'guild', '公会', '公会');
INSERT INTO `validity_data` VALUES ('map_rank_key', 'self', '个人', '个人');
INSERT INTO `validity_data` VALUES ('map_rank_key', 'team', '队伍', '队伍');
INSERT INTO `validity_data` VALUES ('map_rank_mode', 'global', '全局', '全局');
INSERT INTO `validity_data` VALUES ('map_rank_mode', 'local', '不共享', '不共享');
INSERT INTO `validity_data` VALUES ('map_rank_mode', 'none', '不用排行', '不用排行');
INSERT INTO `validity_data` VALUES ('map_rank_mode', 'share', '共享', '共享');
INSERT INTO `validity_data` VALUES ('map_rank_value', 'hurt', '伤害', '伤害');
INSERT INTO `validity_data` VALUES ('map_type', 'full', '全图', '全图');
INSERT INTO `validity_data` VALUES ('map_type', 'slice', '九宫格', '九宫格');
INSERT INTO `validity_data` VALUES ('node_type_atom', 'center', '跨服', '跨服');
INSERT INTO `validity_data` VALUES ('node_type_atom', 'center_world', '跨服和大世界', '跨服和大世界');
INSERT INTO `validity_data` VALUES ('node_type_atom', 'local', '本地', '本地');
INSERT INTO `validity_data` VALUES ('node_type_atom', 'local_center', '本地和跨服', '本地和跨服');
INSERT INTO `validity_data` VALUES ('node_type_atom', 'local_center_world', '全部', '全部');
INSERT INTO `validity_data` VALUES ('node_type_atom', 'local_world', '本地和大世界', '本地和大世界');
INSERT INTO `validity_data` VALUES ('node_type_atom', 'world', '大世界', '大世界');
INSERT INTO `validity_data` VALUES ('node_type_integer', '1', '本地', '本地');
INSERT INTO `validity_data` VALUES ('node_type_integer', '2', '跨服', '跨服');
INSERT INTO `validity_data` VALUES ('node_type_integer', '3', '本地和跨服', '本地和跨服');
INSERT INTO `validity_data` VALUES ('node_type_integer', '4', '大世界', '大世界');
INSERT INTO `validity_data` VALUES ('node_type_integer', '5', '本地和大世界', '本地和大世界');
INSERT INTO `validity_data` VALUES ('node_type_integer', '6', '跨服和大世界', '跨服和大世界');
INSERT INTO `validity_data` VALUES ('node_type_integer', '7', '全部', '全部');
INSERT INTO `validity_data` VALUES ('sex', '0', '无限制', '性别');
INSERT INTO `validity_data` VALUES ('sex', '1', '男性', '性别');
INSERT INTO `validity_data` VALUES ('sex', '2', '女性', '性别');
INSERT INTO `validity_data` VALUES ('skill_type', 'active', '主动', '主动技能');
INSERT INTO `validity_data` VALUES ('skill_type', 'passive', '被动', '被动技能');
INSERT INTO `validity_data` VALUES ('use_effect', '', '无', '无');
INSERT INTO `validity_data` VALUES ('use_effect', 'copper', '铜币', '铜币');
INSERT INTO `validity_data` VALUES ('use_effect', 'exp', '经验', '经验');

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

/*
 Navicat Premium Data Transfer

 Source Server         : ubuntu
 Source Server Type    : MariaDB
 Source Server Version : 100411
 Source Host           : 192.168.1.77:3306
 Source Schema         : main

 Target Server Type    : MariaDB
 Target Server Version : 100411
 File Encoding         : 65001

 Date: 06/01/2020 20:44:48
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
  PRIMARY KEY (`role_id`) USING BTREE
) ENGINE = InnoDB CHARACTER SET = utf8mb4 COLLATE = utf8mb4_general_ci COMMENT = '角色资产表' ROW_FORMAT = Dynamic;

-- ----------------------------
-- Records of asset
-- ----------------------------
INSERT INTO `asset` VALUES (1, 0, 0, 0, 0, 0);

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
  `role_id` bigint(20) UNSIGNED NOT NULL DEFAULT 0 COMMENT '获得者ID',
  `role_name` char(16) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL DEFAULT '' COMMENT '获得者名字',
  `server_id` int(10) UNSIGNED NOT NULL DEFAULT 0 COMMENT '获得者服ID',
  `time` int(10) UNSIGNED NOT NULL DEFAULT 0 COMMENT '时间',
  PRIMARY KEY (`id`) USING BTREE,
  INDEX `role_id`(`role_id`) USING BTREE,
  INDEX `time`(`time`) USING BTREE
) ENGINE = InnoDB AUTO_INCREMENT = 1 CHARACTER SET = utf8mb4 COLLATE = utf8mb4_general_ci COMMENT = '拍卖日志表' ROW_FORMAT = Compressed;

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
INSERT INTO `buff` VALUES (1, 1, 1577588400, 0, 1, '');
INSERT INTO `buff` VALUES (1, 2, 1577588400, 0, 1, '');

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
INSERT INTO `buff_data` VALUES (1, 1, 1, 0, '铜币', '[9]', 0, 1, '', '');
INSERT INTO `buff_data` VALUES (2, 2, 1, 0, '经验', '[10]', 0, 1, '', '');

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
INSERT INTO `effect_data` VALUES (1, 'active', 'battle', '', '10000', 'add', 'self', 'hurt', '', 'Hurt * 1.8', 0, '', '增加80%伤害');
INSERT INTO `effect_data` VALUES (2, 'active', 'battle', '', '10000', 'add', 'self', 'hurt', '', 'Hurt * 1.5', 0, '', '增加50%伤害');
INSERT INTO `effect_data` VALUES (3, 'active', 'battle', 'SelfAttribute.hp == 0', '10000', 'add', 'self', 'attribute', 'hp', 'Self.Attribute.total_hp', 0, '', '死亡立即复活');
INSERT INTO `effect_data` VALUES (4, 'active', 'battle', '', '10000', 'set', 'self', 'attribute', 'vertigo', '0', 0, '', '清除眩晕');
INSERT INTO `effect_data` VALUES (5, 'active', 'battle', '', '10000', 'reduce', 'rival', 'attribute', 'hp', 'Rival.Attribute.total_hp * (50 / 10000)', 5, '', '每秒扣血，总血量万分之50');
INSERT INTO `effect_data` VALUES (6, 'active', 'battle', '', '10000', 'add', 'mate', 'attribute', 'attack', 'Mate.Attribute.attack * 1.5', 3, '', '增加队友攻击150%');
INSERT INTO `effect_data` VALUES (7, 'active', 'battle', '', '10000', 'add', 'mate', 'attribute', 'defense', 'Mate.Attribute.defense * 1.5', 3, '', '增加队友防御150%');
INSERT INTO `effect_data` VALUES (8, 'active', 'battle', '', '10000', 'add', 'self', 'buff', '', '[1]', 0, '', '添加Buff');
INSERT INTO `effect_data` VALUES (9, 'active', 'user', '', '10000', 'add', 'self', 'asset', 'copper', '1.5', 0, '', '增加150%铜币');
INSERT INTO `effect_data` VALUES (10, 'active', 'user', '', '10000', 'add', 'self', 'asset', 'exp', '2', 0, '', '增加200%经验');

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
  `guild_id` int(20) UNSIGNED NOT NULL DEFAULT 0 COMMENT '公会ID(join(`guild`.`guild_id`)/(delete_guild_id))',
  `role_id` int(20) UNSIGNED NOT NULL DEFAULT 0 COMMENT '角色ID(join(`role`.`role_id`)/join(`vip`.`role_id`)/(delete_role_id))',
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
INSERT INTO `increment` VALUES ('monster', 10003);

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
) ENGINE = InnoDB AUTO_INCREMENT = 28 CHARACTER SET = utf8mb4 COLLATE = utf8mb4_general_ci COMMENT = '角色物品表' ROW_FORMAT = Dynamic;

-- ----------------------------
-- Records of item
-- ----------------------------
INSERT INTO `item` VALUES (1, 1, 1, 1, 1000, 0, 0, '');
INSERT INTO `item` VALUES (2, 1, 1, 1, 666, 0, 0, '');
INSERT INTO `item` VALUES (3, 1, 2, 1, 7, 0, 0, '');
INSERT INTO `item` VALUES (4, 1, 3, 1, 10, 0, 0, '');
INSERT INTO `item` VALUES (5, 1, 4, 2, 1, 0, 0, '');
INSERT INTO `item` VALUES (6, 1, 5, 3, 1, 0, 0, '');

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
) ENGINE = InnoDB AUTO_INCREMENT = 1 CHARACTER SET = utf8mb4 COLLATE = utf8mb4_general_ci COMMENT = '物品消费日志表' ROW_FORMAT = Compressed;

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
  PRIMARY KEY (`id`) USING BTREE,
  INDEX `role_id`(`role_id`) USING BTREE,
  INDEX `time`(`time`) USING BTREE
) ENGINE = InnoDB AUTO_INCREMENT = 1 CHARACTER SET = utf8mb4 COLLATE = utf8mb4_general_ci COMMENT = '物品产出日志表' ROW_FORMAT = Compressed;

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
INSERT INTO `map_data` VALUES (100000, 'full', 'false', '', 'self', 'hurt', 'global', '[{10,10},{20,10},{30,10},{40,10},{50,10},{60,10},{70,10},{10,10},{90,10},{100,10}]', '', '', '', '');

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
INSERT INTO `monster_data` VALUES (1, 10, 'active', 'monster', 1, 100, 0, 1, 1, 300, 0, 'active', '[role]', '[5]', '[{10,10},{20,10},{30,10},{40,10},{50,10},{60,10},{70,10},{10,10},{90,10},{100,10}]', '[{100005,100}]');
INSERT INTO `monster_data` VALUES (2, 20, 'passive', 'guard', 1, 200, 0, 1, 2, 300, 0, 'passive', '[enemy]', '', '[{40,10}]', '[{100005,200}]');
INSERT INTO `monster_data` VALUES (3, 30, 'movable', 'boom', 1, 300, 0, 1, 3, 300, 0, 'movable', '', '', '[{60,10}]', '[{100005,300}]');
INSERT INTO `monster_data` VALUES (4, 40, 'fix', 'statue', 1, 400, 0, 1, 4, 300, 0, 'fix', '', '', '[{80,10}]', '');
INSERT INTO `monster_data` VALUES (5, 50, 'act', 'boom', 1, 500, 0, 1, 5, 300, 0, 'fix', '[enemy]', '', '[{100,10}]', '');
INSERT INTO `monster_data` VALUES (6, 60, 'boom', 'boss', 1, 600, 0, 1, 6, 300, 0, 'active', '[{monster, 20}, {monster, 50}, role]', '', '[{120,10}]', '[{100005,600}]');

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
) ENGINE = InnoDB AUTO_INCREMENT = 138 CHARACTER SET = utf8mb4 COLLATE = utf8mb4_general_ci COMMENT = '在线统计日志' ROW_FORMAT = Compressed;

-- ----------------------------
-- Records of online_log
-- ----------------------------
INSERT INTO `online_log` VALUES (1, 1, 1, 0, 16, 1578040166);
INSERT INTO `online_log` VALUES (2, 1, 0, 1, 0, 1578040106);
INSERT INTO `online_log` VALUES (3, 2, 1, 1, 0, 1578040166);
INSERT INTO `online_log` VALUES (4, 3, 0, 3, 0, 1578040226);
INSERT INTO `online_log` VALUES (5, 4, 2, 2, 0, 1578040286);
INSERT INTO `online_log` VALUES (6, 5, 3, 2, 0, 1578040346);
INSERT INTO `online_log` VALUES (7, 6, 6, 0, 0, 1578040406);
INSERT INTO `online_log` VALUES (8, 7, 5, 2, 0, 1578040466);
INSERT INTO `online_log` VALUES (9, 8, 5, 3, 0, 1578040526);
INSERT INTO `online_log` VALUES (10, 9, 3, 6, 0, 1578040586);
INSERT INTO `online_log` VALUES (11, 10, 5, 5, 0, 1578040646);
INSERT INTO `online_log` VALUES (12, 11, 8, 3, 0, 1578040706);
INSERT INTO `online_log` VALUES (13, 12, 9, 3, 0, 1578040766);
INSERT INTO `online_log` VALUES (14, 13, 10, 3, 0, 1578040806);
INSERT INTO `online_log` VALUES (15, 14, 5, 9, 0, 1578040866);
INSERT INTO `online_log` VALUES (16, 15, 10, 5, 0, 1578040826);
INSERT INTO `online_log` VALUES (17, 0, 0, 0, 11, 1578193482);
INSERT INTO `online_log` VALUES (18, 0, 0, 0, 11, 1578193542);
INSERT INTO `online_log` VALUES (19, 0, 0, 0, 11, 1578193602);
INSERT INTO `online_log` VALUES (20, 0, 0, 0, 11, 1578193662);
INSERT INTO `online_log` VALUES (21, 0, 0, 0, 11, 1578193722);
INSERT INTO `online_log` VALUES (22, 0, 0, 0, 11, 1578193782);
INSERT INTO `online_log` VALUES (23, 0, 0, 0, 11, 1578193842);
INSERT INTO `online_log` VALUES (24, 0, 0, 0, 11, 1578193902);
INSERT INTO `online_log` VALUES (25, 0, 0, 0, 11, 1578193962);
INSERT INTO `online_log` VALUES (26, 0, 0, 0, 11, 1578194022);
INSERT INTO `online_log` VALUES (27, 0, 0, 0, 11, 1578194082);
INSERT INTO `online_log` VALUES (28, 0, 0, 0, 11, 1578194142);
INSERT INTO `online_log` VALUES (29, 0, 0, 0, 11, 1578194202);
INSERT INTO `online_log` VALUES (30, 0, 0, 0, 11, 1578194262);
INSERT INTO `online_log` VALUES (31, 0, 0, 0, 11, 1578194322);
INSERT INTO `online_log` VALUES (32, 0, 0, 0, 11, 1578194382);
INSERT INTO `online_log` VALUES (33, 0, 0, 0, 11, 1578194532);
INSERT INTO `online_log` VALUES (34, 0, 0, 0, 11, 1578194592);
INSERT INTO `online_log` VALUES (35, 0, 0, 0, 11, 1578194787);
INSERT INTO `online_log` VALUES (36, 0, 0, 0, 11, 1578194847);
INSERT INTO `online_log` VALUES (37, 0, 0, 0, 11, 1578194907);
INSERT INTO `online_log` VALUES (38, 0, 0, 0, 11, 1578194967);
INSERT INTO `online_log` VALUES (39, 0, 0, 0, 11, 1578195027);
INSERT INTO `online_log` VALUES (40, 0, 0, 0, 11, 1578195087);
INSERT INTO `online_log` VALUES (41, 0, 0, 0, 11, 1578195147);
INSERT INTO `online_log` VALUES (42, 0, 0, 0, 11, 1578195207);
INSERT INTO `online_log` VALUES (43, 0, 0, 0, 11, 1578195267);
INSERT INTO `online_log` VALUES (44, 0, 0, 0, 11, 1578195540);
INSERT INTO `online_log` VALUES (45, 0, 0, 0, 11, 1578195600);
INSERT INTO `online_log` VALUES (46, 0, 0, 0, 11, 1578195660);
INSERT INTO `online_log` VALUES (47, 0, 0, 0, 11, 1578195720);
INSERT INTO `online_log` VALUES (48, 0, 0, 0, 11, 1578195780);
INSERT INTO `online_log` VALUES (49, 0, 0, 0, 11, 1578195840);
INSERT INTO `online_log` VALUES (50, 0, 0, 0, 11, 1578195900);
INSERT INTO `online_log` VALUES (51, 0, 0, 0, 11, 1578195960);
INSERT INTO `online_log` VALUES (52, 0, 0, 0, 11, 1578196020);
INSERT INTO `online_log` VALUES (53, 0, 0, 0, 11, 1578196080);
INSERT INTO `online_log` VALUES (54, 0, 0, 0, 11, 1578196140);
INSERT INTO `online_log` VALUES (55, 0, 0, 0, 11, 1578196200);
INSERT INTO `online_log` VALUES (56, 0, 0, 0, 11, 1578196260);
INSERT INTO `online_log` VALUES (57, 0, 0, 0, 12, 1578196930);
INSERT INTO `online_log` VALUES (58, 0, 0, 0, 12, 1578196990);
INSERT INTO `online_log` VALUES (59, 0, 0, 0, 12, 1578197050);
INSERT INTO `online_log` VALUES (60, 0, 0, 0, 12, 1578197110);
INSERT INTO `online_log` VALUES (61, 0, 0, 0, 9, 1578274449);
INSERT INTO `online_log` VALUES (62, 0, 0, 0, 9, 1578274509);
INSERT INTO `online_log` VALUES (63, 0, 0, 0, 9, 1578274569);
INSERT INTO `online_log` VALUES (64, 0, 0, 0, 9, 1578274856);
INSERT INTO `online_log` VALUES (65, 0, 0, 0, 9, 1578274916);
INSERT INTO `online_log` VALUES (66, 0, 0, 0, 9, 1578274976);
INSERT INTO `online_log` VALUES (67, 0, 0, 0, 9, 1578275108);
INSERT INTO `online_log` VALUES (68, 0, 0, 0, 9, 1578275419);
INSERT INTO `online_log` VALUES (69, 0, 0, 0, 9, 1578275479);
INSERT INTO `online_log` VALUES (70, 0, 0, 0, 9, 1578275641);
INSERT INTO `online_log` VALUES (71, 0, 0, 0, 9, 1578275702);
INSERT INTO `online_log` VALUES (72, 0, 0, 0, 9, 1578275762);
INSERT INTO `online_log` VALUES (73, 0, 0, 0, 10, 1578276904);
INSERT INTO `online_log` VALUES (74, 0, 0, 0, 10, 1578276964);
INSERT INTO `online_log` VALUES (75, 0, 0, 0, 10, 1578277024);
INSERT INTO `online_log` VALUES (76, 0, 0, 0, 10, 1578277084);
INSERT INTO `online_log` VALUES (77, 0, 0, 0, 10, 1578277144);
INSERT INTO `online_log` VALUES (78, 0, 0, 0, 10, 1578277204);
INSERT INTO `online_log` VALUES (79, 0, 0, 0, 10, 1578277264);
INSERT INTO `online_log` VALUES (80, 0, 0, 0, 10, 1578277324);
INSERT INTO `online_log` VALUES (81, 0, 0, 0, 10, 1578277464);
INSERT INTO `online_log` VALUES (82, 0, 0, 0, 10, 1578277524);
INSERT INTO `online_log` VALUES (83, 0, 0, 0, 10, 1578277584);
INSERT INTO `online_log` VALUES (84, 0, 0, 0, 10, 1578277644);
INSERT INTO `online_log` VALUES (85, 0, 0, 0, 10, 1578277704);
INSERT INTO `online_log` VALUES (86, 0, 0, 0, 10, 1578277764);
INSERT INTO `online_log` VALUES (87, 0, 0, 0, 10, 1578277904);
INSERT INTO `online_log` VALUES (88, 0, 0, 0, 10, 1578279040);
INSERT INTO `online_log` VALUES (89, 0, 0, 0, 10, 1578279100);
INSERT INTO `online_log` VALUES (90, 0, 0, 0, 10, 1578279160);
INSERT INTO `online_log` VALUES (91, 0, 0, 0, 10, 1578279220);
INSERT INTO `online_log` VALUES (92, 0, 0, 0, 10, 1578279280);
INSERT INTO `online_log` VALUES (93, 0, 0, 0, 10, 1578279340);
INSERT INTO `online_log` VALUES (94, 0, 0, 0, 10, 1578279400);
INSERT INTO `online_log` VALUES (95, 0, 0, 0, 10, 1578279461);
INSERT INTO `online_log` VALUES (96, 0, 0, 0, 10, 1578279521);
INSERT INTO `online_log` VALUES (97, 0, 0, 0, 10, 1578279581);
INSERT INTO `online_log` VALUES (98, 0, 0, 0, 11, 1578279641);
INSERT INTO `online_log` VALUES (99, 0, 0, 0, 11, 1578279701);
INSERT INTO `online_log` VALUES (100, 0, 0, 0, 11, 1578279761);
INSERT INTO `online_log` VALUES (101, 0, 0, 0, 11, 1578279821);
INSERT INTO `online_log` VALUES (102, 0, 0, 0, 11, 1578279881);
INSERT INTO `online_log` VALUES (103, 0, 0, 0, 11, 1578279941);
INSERT INTO `online_log` VALUES (104, 0, 0, 0, 11, 1578280001);
INSERT INTO `online_log` VALUES (105, 0, 0, 0, 11, 1578280061);
INSERT INTO `online_log` VALUES (106, 0, 0, 0, 11, 1578280121);
INSERT INTO `online_log` VALUES (107, 0, 0, 0, 11, 1578280181);
INSERT INTO `online_log` VALUES (108, 0, 0, 0, 11, 1578280241);
INSERT INTO `online_log` VALUES (109, 0, 0, 0, 11, 1578280301);
INSERT INTO `online_log` VALUES (110, 0, 0, 0, 11, 1578280361);
INSERT INTO `online_log` VALUES (111, 0, 0, 0, 11, 1578280542);
INSERT INTO `online_log` VALUES (112, 0, 0, 0, 11, 1578280602);
INSERT INTO `online_log` VALUES (113, 0, 0, 0, 11, 1578280662);
INSERT INTO `online_log` VALUES (114, 0, 0, 0, 11, 1578280722);
INSERT INTO `online_log` VALUES (115, 0, 0, 0, 11, 1578280782);
INSERT INTO `online_log` VALUES (116, 0, 0, 0, 11, 1578280842);
INSERT INTO `online_log` VALUES (117, 0, 0, 0, 11, 1578280902);
INSERT INTO `online_log` VALUES (118, 0, 0, 0, 11, 1578280962);
INSERT INTO `online_log` VALUES (119, 0, 0, 0, 11, 1578281022);
INSERT INTO `online_log` VALUES (120, 0, 0, 0, 11, 1578281082);
INSERT INTO `online_log` VALUES (121, 0, 0, 0, 11, 1578281142);
INSERT INTO `online_log` VALUES (122, 0, 0, 0, 11, 1578281202);
INSERT INTO `online_log` VALUES (123, 0, 0, 0, 14, 1578291414);
INSERT INTO `online_log` VALUES (124, 0, 0, 0, 14, 1578291474);
INSERT INTO `online_log` VALUES (125, 0, 0, 0, 14, 1578291534);
INSERT INTO `online_log` VALUES (126, 0, 0, 0, 14, 1578291594);
INSERT INTO `online_log` VALUES (127, 0, 0, 0, 14, 1578291654);
INSERT INTO `online_log` VALUES (128, 0, 0, 0, 14, 1578291714);
INSERT INTO `online_log` VALUES (129, 0, 0, 0, 14, 1578291774);
INSERT INTO `online_log` VALUES (130, 0, 0, 0, 14, 1578291834);
INSERT INTO `online_log` VALUES (131, 0, 0, 0, 14, 1578291894);
INSERT INTO `online_log` VALUES (132, 0, 0, 0, 14, 1578291954);
INSERT INTO `online_log` VALUES (133, 0, 0, 0, 14, 1578292014);
INSERT INTO `online_log` VALUES (134, 0, 0, 0, 14, 1578292074);
INSERT INTO `online_log` VALUES (135, 0, 0, 0, 14, 1578292134);
INSERT INTO `online_log` VALUES (136, 0, 0, 0, 14, 1578292194);
INSERT INTO `online_log` VALUES (137, 0, 0, 0, 14, 1578292254);

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
INSERT INTO `parameter_data` VALUES ('area', 'cn', '语言区域');
INSERT INTO `parameter_data` VALUES ('chat_cd', '30', '聊天冷却时间');
INSERT INTO `parameter_data` VALUES ('chat_level', '10', '聊天开放等级');
INSERT INTO `parameter_data` VALUES ('friend_level', '30', '好友开放等级');
INSERT INTO `parameter_data` VALUES ('friend_number', '50', '好友上限');
INSERT INTO `parameter_data` VALUES ('guild_create_cd', '86400', '公会创建冷却时间');
INSERT INTO `parameter_data` VALUES ('guild_join_cd', '86400', '公会加入冷却时间');
INSERT INTO `parameter_data` VALUES ('login_cd', '180', '登录时间间隔');
INSERT INTO `parameter_data` VALUES ('time_zone', '+8', '时区');
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
INSERT INTO `quest` VALUES (1, 1, 1, 'event_kill_monster', 0, 0, 'nc', 1, '');
INSERT INTO `quest` VALUES (1, 1001, 2, 'event_pass_dungeon', 100, 1, 'ge', 0, '');
INSERT INTO `quest` VALUES (1, 100001, 3, 'event_shop_buy', 1, 1, 'eq', 0, '');

-- ----------------------------
-- Table structure for quest_data
-- ----------------------------
DROP TABLE IF EXISTS `quest_data`;
CREATE TABLE `quest_data`  (
  `quest_id` int(10) UNSIGNED NOT NULL DEFAULT 0 COMMENT '任务ID',
  `group_id` int(10) UNSIGNED NOT NULL DEFAULT 0 COMMENT '组ID',
  `pre_id` int(10) UNSIGNED NOT NULL DEFAULT 0 COMMENT '前置任务',
  `next_id` int(10) UNSIGNED NOT NULL DEFAULT 0 COMMENT '后置任务',
  `module` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL DEFAULT '' COMMENT '模块(validate(module))',
  `function` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL DEFAULT '' COMMENT '函数(validate(function))',
  `event` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL DEFAULT '' COMMENT '事件(validate(event))',
  `compare` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL DEFAULT '' COMMENT '比较模式(validate(compare))',
  `target` int(10) UNSIGNED NOT NULL DEFAULT 0 COMMENT '目标',
  `number` int(10) UNSIGNED NOT NULL DEFAULT 0 COMMENT '数量',
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
INSERT INTO `quest_data` VALUES (1, 1, 0, 2, '', '', 'event_kill_monster', 'nc', 0, 3, '', '[{1,1}]', '', '', '');
INSERT INTO `quest_data` VALUES (2, 1, 1, 3, 'role', 'check_quest', 'event_level_upgrade', 'ge', 5, 1, '[{copper, 100}]', '[{1,10}]', '', '', '');
INSERT INTO `quest_data` VALUES (3, 1, 2, 4, '', '', 'event_pass_dungeon', 'ge', 100001, 1, '[{level, 10}]', '[{1,100}]', '', '', '');
INSERT INTO `quest_data` VALUES (4, 1, 3, 5, '', '', 'event_shop_buy', 'eq', 1, 1, '', '[{1,1000}]', '', '', '');
INSERT INTO `quest_data` VALUES (5, 1, 4, 0, '', '', 'event_guild_join', 'nc', 0, 1, '', '[{1,1000}]', '', '', '');
INSERT INTO `quest_data` VALUES (6, 1, 5, 0, 'friend', 'check_quest', 'event_friend_add', 'nc', 0, 5, '', '[{1,10}]', '', '', '');
INSERT INTO `quest_data` VALUES (1001, 2, 0, 1002, '', '', 'event_pass_dungeon', 'ge', 100, 1, '', '[{1,10}]', '', '', '');
INSERT INTO `quest_data` VALUES (1002, 2, 1001, 0, '', '', 'event_friend_add', 'eq', 1, 1, '', '[{1,10}]', '', '', '');
INSERT INTO `quest_data` VALUES (100001, 3, 0, 100002, 'shop', 'check_quest', 'event_shop_buy', 'eq', 1, 1, '', '[{1,10}]', '', '', '');
INSERT INTO `quest_data` VALUES (100002, 3, 100001, 0, '', '', 'event_guild_join', 'nc', 0, 1, '', '[{1,10}]', '', '', '');

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
  INDEX `order_id`(`order_id`) USING BTREE,
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
  `online_time` int(10) UNSIGNED NOT NULL DEFAULT 0 COMMENT '在线时间',
  `server_id` smallint(5) UNSIGNED NOT NULL DEFAULT 0 COMMENT '服ID',
  `channel_id` int(10) UNSIGNED NOT NULL DEFAULT 0 COMMENT '渠道ID',
  `map` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL DEFAULT '' COMMENT '地图',
  `device_id` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL DEFAULT '' COMMENT '设备ID',
  `device_type` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL DEFAULT '' COMMENT '设备类型',
  `mac` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL DEFAULT '' COMMENT 'Mac地址',
  PRIMARY KEY (`role_id`) USING BTREE,
  INDEX `account`(`account`) USING BTREE
) ENGINE = InnoDB AUTO_INCREMENT = 8 CHARACTER SET = utf8mb4 COLLATE = utf8mb4_general_ci COMMENT = '角色信息表' ROW_FORMAT = Dynamic;

-- ----------------------------
-- Records of role
-- ----------------------------
INSERT INTO `role` VALUES (1, '1', '1', 3, 100, 1, 1, 100, 100, 100, 0, 1578040094, 1, 1, '{map,1000000000000000,100000,undefined,30,30}', '', '', '');
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
-- Table structure for sensitive_word_data
-- ----------------------------
DROP TABLE IF EXISTS `sensitive_word_data`;
CREATE TABLE `sensitive_word_data`  (
  `word` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL DEFAULT '' COMMENT '敏感词',
  PRIMARY KEY (`word`) USING BTREE
) ENGINE = InnoDB CHARACTER SET = utf8mb4 COLLATE = utf8mb4_general_ci COMMENT = '敏感词配置表' ROW_FORMAT = Dynamic;

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
  PRIMARY KEY (`id`) USING BTREE,
  INDEX `role_id`(`role_id`) USING BTREE,
  INDEX `time`(`time`) USING BTREE
) ENGINE = InnoDB AUTO_INCREMENT = 1 CHARACTER SET = utf8mb4 COLLATE = utf8mb4_general_ci COMMENT = '商店日志表' ROW_FORMAT = Compressed;

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
INSERT INTO `skill_data` VALUES (1, 1, 'active', '普攻技能', '', '', '[1]', 1, 1, 1, 1, '', '', '', '', '对目标造成180%的伤害');
INSERT INTO `skill_data` VALUES (2, 2, 'active', '群攻技能', '', '', '[2]', 1, 1, 1, 3, '', '', '', '', '对3个目标造成150%的伤害');
INSERT INTO `skill_data` VALUES (3, 3, 'passive', '增益', '', '', '[8]', 10, 1, 1, 1, '', '', '', '', '每秒扣血，总血量万分之50');
INSERT INTO `skill_data` VALUES (5, 5, 'active', '普攻技能', '', '', '', 1, 1, 1, 1, '', '', '', '', '普通技能');

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

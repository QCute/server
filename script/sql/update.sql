-- ------------------------------------------------------------------
-- :tag:
-- ------------------------------------------------------------------
-- 2015-07-10


-- ------------------------------------------------------------------
-- :tag:
-- ------------------------------------------------------------------
REPLACE INTO `validity_data` SET `type` = 'act_type', `key` = 'active', `value` = '主动', `description` = '主动';
REPLACE INTO `validity_data` SET `type` = 'act_type', `key` = 'passive', `value` = '被动', `description` = '被动';
REPLACE INTO `validity_data` SET `type` = 'act_type', `key` = 'movable', `value` = '移动', `description` = '移动';
REPLACE INTO `validity_data` SET `type` = 'act_type', `key` = 'fix', `value` = '固定', `description` = '固定';

REPLACE INTO `validity_data` SET `type` = 'act_script', `key` = 'role', `value` = '玩家', `description` = '玩家';
REPLACE INTO `validity_data` SET `type` = 'act_script', `key` = 'monster', `value` = '怪物', `description` = '怪物';
REPLACE INTO `validity_data` SET `type` = 'act_script', `key` = 'enemy', `value` = '敌人', `description` = '敌人';
REPLACE INTO `validity_data` SET `type` = 'act_script', `key` = 'location', `value` = '位置', `description` = '位置';



-- 2017-03-11
DROP TABLE IF EXISTS `monster_data`;
CREATE TABLE IF NOT EXISTS `monster_data` (
  `monster_id` INT(10) UNSIGNED NOT NULL DEFAULT 0 COMMENT '怪物ID',
  `monster_name` CHAR(255) NOT NULL DEFAULT '' COMMENT '怪物名称',
  `type` INT(10) UNSIGNED NOT NULL DEFAULT 0 COMMENT '怪物类型',
  `level` INT(10) UNSIGNED NOT NULL DEFAULT 0 COMMENT '等级',
  `hp` BIGINT(20) UNSIGNED NOT NULL DEFAULT 0 COMMENT '血量',
  `camp` TINYINT(1) UNSIGNED NOT NULL DEFAULT 0 COMMENT '阵营',
  `range` INT(10) UNSIGNED NOT NULL DEFAULT 0 COMMENT '攻击距离',
  `act_type` VARCHAR(255) NOT NULL DEFAULT '' COMMENT '动作类型',
  `act_script` VARCHAR(255) NOT NULL DEFAULT '' COMMENT '动作脚本',
  `skill` VARCHAR(255) NOT NULL DEFAULT '' COMMENT '技能',
  `born_points` VARCHAR(255) NOT NULL DEFAULT '' COMMENT '出生点',
  `award` VARCHAR(255) NOT NULL DEFAULT '' COMMENT '奖励',
  PRIMARY KEY(`monster_id`)
) ENGINE = InnoDB CHARACTER SET = utf8mb4 COLLATE = utf8mb4_unicode_ci COMMENT = '怪物配置表' ROW_FORMAT = Dynamic;

DROP TABLE IF EXISTS `map_data`;
CREATE TABLE IF NOT EXISTS `map_data` (
  `map_id` INT(11) UNSIGNED NOT NULL DEFAULT 0 COMMENT '',
  `type` VARCHAR(255) NOT NULL DEFAULT '' COMMENT '广播类型(validate(`map_type`))',
  `reconnect` TINYINT(1) UNSIGNED NOT NULL DEFAULT 0 COMMENT '是否重连',
  `monster` VARCHAR(255) NOT NULL DEFAULT '' COMMENT '随地图启动的怪物',
  `rank_key` VARCHAR(255) NOT NULL DEFAULT '' COMMENT '榜键类型(validate(`map_rank_key`))',
  `rank_value` VARCHAR(255) NOT NULL DEFAULT '' COMMENT '榜值类型(validate(`map_rank_value`))',
  `rank_mode` VARCHAR(255) NOT NULL DEFAULT '' COMMENT '榜模式(validate(`map_rank_mode`))',
  `enter_point` VARCHAR(255) NOT NULL DEFAULT '' COMMENT '进入点',
  `pk_mode` VARCHAR(255) NOT NULL DEFAULT '' COMMENT 'PK模式',
  `enter_script` VARCHAR(255) NOT NULL DEFAULT '' COMMENT '进入脚本',
  `relive_script` VARCHAR(255) NOT NULL DEFAULT '' COMMENT '复活脚本',
  `leave_script` VARCHAR(255) NOT NULL DEFAULT '' COMMENT '离开脚本',
  PRIMARY KEY (`map_id`)
) ENGINE = InnoDB CHARACTER SET = utf8mb4 COLLATE = utf8mb4_unicode_ci COMMENT = '地图配置表' ROW_FORMAT = Dynamic;


REPLACE INTO `validity_data` SET `type` = 'map_type', `key` = 'slice', `value` = '九宫格', `description` = '九宫格';
REPLACE INTO `validity_data` SET `type` = 'map_type', `key` = 'full', `value` = '全图', `description` = '全图';

REPLACE INTO `validity_data` SET `type` = 'map_rank_key', `key` = 'self', `value` = '个人', `description` = '个人';
REPLACE INTO `validity_data` SET `type` = 'map_rank_key', `key` = 'guild', `value` = '公会', `description` = '公会';
REPLACE INTO `validity_data` SET `type` = 'map_rank_key', `key` = 'team', `value` = '队伍', `description` = '队伍';
REPLACE INTO `validity_data` SET `type` = 'map_rank_key', `key` = 'camp', `value` = '阵营', `description` = '阵营';

REPLACE INTO `validity_data` SET `type` = 'map_rank_value', `key` = 'hurt', `value` = '伤害', `description` = '伤害';

REPLACE INTO `validity_data` SET `type` = 'map_rank_mode', `key` = 'none', `value` = '不用排行', `description` = '不用排行';
REPLACE INTO `validity_data` SET `type` = 'map_rank_mode', `key` = 'global', `value` = '全局', `description` = '全局';
REPLACE INTO `validity_data` SET `type` = 'map_rank_mode', `key` = 'share', `value` = '共享', `description` = '共享';
REPLACE INTO `validity_data` SET `type` = 'map_rank_mode', `key` = 'local', `value` = '不共享', `description` = '不共享';


-- 2017-06-22
REPLACE INTO `validity_data` SET `type` = 'node_type_integer', `key` = '1', `value` = '本地', `description` = '本地';
REPLACE INTO `validity_data` SET `type` = 'node_type_integer', `key` = '2', `value` = '跨服', `description` = '跨服';
REPLACE INTO `validity_data` SET `type` = 'node_type_integer', `key` = '4', `value` = '大世界', `description` = '大世界';
REPLACE INTO `validity_data` SET `type` = 'node_type_integer', `key` = '3', `value` = '本地和跨服', `description` = '本地和跨服';
REPLACE INTO `validity_data` SET `type` = 'node_type_integer', `key` = '5', `value` = '本地和大世界', `description` = '本地和大世界';
REPLACE INTO `validity_data` SET `type` = 'node_type_integer', `key` = '6', `value` = '跨服和大世界', `description` = '跨服和大世界';
REPLACE INTO `validity_data` SET `type` = 'node_type_integer', `key` = '7', `value` = '全部', `description` = '全部';

REPLACE INTO `validity_data` SET `type` = 'node_type_atom', `key` = 'local', `value` = '本地', `description` = '本地';
REPLACE INTO `validity_data` SET `type` = 'node_type_atom', `key` = 'center', `value` = '跨服', `description` = '跨服';
REPLACE INTO `validity_data` SET `type` = 'node_type_atom', `key` = 'world', `value` = '大世界', `description` = '大世界';
REPLACE INTO `validity_data` SET `type` = 'node_type_atom', `key` = 'local_center', `value` = '本地和跨服', `description` = '本地和跨服';
REPLACE INTO `validity_data` SET `type` = 'node_type_atom', `key` = 'local_world', `value` = '本地和大世界', `description` = '本地和大世界';
REPLACE INTO `validity_data` SET `type` = 'node_type_atom', `key` = 'center_world', `value` = '跨服和大世界', `description` = '跨服和大世界';
REPLACE INTO `validity_data` SET `type` = 'node_type_atom', `key` = 'local_center_world', `value` = '全部', `description` = '全部';

-- ------------------------------------------------------------------
-- :tag:
-- ------------------------------------------------------------------
-- 2019-03-31
REPLACE INTO `parameter_data` SET `key` = 'login_cd', `value` = '180', `description` = '登录时间间隔';
REPLACE INTO `validity_data` SET `type` = 'event', `key` = 'event_add_friend', `value` = '添加好友', `description` = '添加好友';

-- 2019-04-15
REPLACE INTO `validity_data` SET `type` = 'skill_type', `key` = 'active', `value` = '主动', `description` = '主动技能';
REPLACE INTO `validity_data` SET `type` = 'skill_type', `key` = 'passive', `value` = '被动', `description` = '被动技能';


-- 2019-09-01
DROP TABLE IF EXISTS `test`;
CREATE TABLE IF NOT EXISTS `test` (
  `tinyint` TINYINT(3) UNSIGNED NOT NULL DEFAULT 0 COMMENT 'TinyInt',
  `smallint` SMALLINT(3) UNSIGNED NOT NULL DEFAULT 0 COMMENT 'SmallInt',
  `int` INT(3) UNSIGNED NOT NULL DEFAULT 0 COMMENT 'Int',
  `bigint` BIGINT(3) UNSIGNED NOT NULL DEFAULT 0 COMMENT 'BigInt',
  `char` CHAR(255) NOT NULL DEFAULT 0 COMMENT 'Char',
  `varchar` VARCHAR(255) NOT NULL DEFAULT 0 COMMENT 'VarChar',
  PRIMARY KEY (`tinyint`),
  KEY `int` (`int`)
) ENGINE = InnoDB CHARACTER SET = utf8mb4 COLLATE = utf8mb4_unicode_ci COMMENT = '类型测试表' ROW_FORMAT = Dynamic;

-- ------------------------------------------------------------------
-- :tag:
-- ------------------------------------------------------------------
-- 2019-10-18
REPLACE INTO `validity_data` SET `type` = 'use_effect', `key` = '', `value` = '无', `description` = '无';
REPLACE INTO `validity_data` SET `type` = 'use_effect', `key` = 'exp', `value` = '经验', `description` = '经验';
REPLACE INTO `validity_data` SET `type` = 'use_effect', `key` = 'copper', `value` = '铜币', `description` = '铜币';
-- ------------------------------------------------------------------
-- :tag:
-- ------------------------------------------------------------------
-- 2019-12-12
DROP TABLE IF EXISTS `increment`;
CREATE TABLE IF NOT EXISTS `increment` (
  `name` CHAR(255) NOT NULL DEFAULT 0 COMMENT '名字',
  `value` BIGINT(20) UNSIGNED NOT NULL DEFAULT 0 COMMENT '数值',
  PRIMARY KEY (`name`)
) ENGINE = InnoDB CHARACTER SET = utf8mb4 COLLATE = utf8mb4_unicode_ci COMMENT = '自增表' ROW_FORMAT = Dynamic;

-- ------------------------------------------------------------------
-- :tag:
-- ------------------------------------------------------------------

-- 2020-07-01
INSERT IGNORE `text_date` VALUES ('test', '😂', '测试');

-- 2020-10-01
INSERT INTO `validate_data` VALUES ('activity_service', 'boss', 'BOSS');
INSERT INTO `validate_data` VALUES ('activity_service', 'auction', '拍卖');


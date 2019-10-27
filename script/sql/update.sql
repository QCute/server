-- -------------------------------------------------------------------
-- :tag:
-- -------------------------------------------------------------------
-- 2015-07-10


-- -------------------------------------------------------------------
-- :tag:
-- -------------------------------------------------------------------
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
) ENGINE = InnoDB CHARACTER SET = utf8mb4 COLLATE = utf8mb4_general_ci COMMENT = '怪物配置表' ROW_FORMAT = Dynamic;

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

-- -------------------------------------------------------------------
-- :tag:
-- -------------------------------------------------------------------
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
) ENGINE = InnoDB CHARACTER SET = utf8mb4 COLLATE = utf8mb4_general_ci COMMENT = '类型测试表' ROW_FORMAT = Dynamic;

-- -------------------------------------------------------------------
-- :tag:
-- -------------------------------------------------------------------
-- 2019-10-18
REPLACE INTO `validity_data` SET `type` = 'use_effect', `key` = '', `value` = '无', `description` = '无';
REPLACE INTO `validity_data` SET `type` = 'use_effect', `key` = 'exp', `value` = '经验', `description` = '经验';
REPLACE INTO `validity_data` SET `type` = 'use_effect', `key` = 'copper', `value` = '铜币', `description` = '铜币';

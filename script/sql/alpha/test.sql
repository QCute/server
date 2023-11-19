
-- ------------------------------------------------------------------
-- :tag:
-- ------------------------------------------------------------------
-- 2019-03-31
REPLACE INTO `parameter_data` SET `key` = 'login_cd', `value` = '180', `description` = '登录时间间隔';
REPLACE INTO `validation_data` SET `type` = 'event', `key` = 'event_add_friend', `value` = '添加好友', `description` = '添加好友';

-- 2019-04-15
REPLACE INTO `validation_data` SET `type` = 'skill_type', `key` = 'active', `value` = '主动', `description` = '主动技能';
REPLACE INTO `validation_data` SET `type` = 'skill_type', `key` = 'passive', `value` = '被动', `description` = '被动技能';


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
REPLACE INTO `validation_data` SET `type` = 'use_effect', `key` = '', `value` = '无', `description` = '无';
REPLACE INTO `validation_data` SET `type` = 'use_effect', `key` = 'exp', `value` = '经验', `description` = '经验';
REPLACE INTO `validation_data` SET `type` = 'use_effect', `key` = 'copper', `value` = '铜币', `description` = '铜币';

-- ------------------------------------------------------------------
-- :tag:
-- ------------------------------------------------------------------

-- 2020-07-01
INSERT IGNORE `text_date` VALUES ('test', '😂', '测试');

-- 2020-10-01
INSERT INTO `validation_data` VALUES ('activity_service', 'boss', 'BOSS');
INSERT INTO `validation_data` VALUES ('activity_service', 'auction', '拍卖');

INSERT INTO `validation_data` VALUES ('condition', 'classes', '职业');
INSERT INTO `validation_data` VALUES ('condition', 'level', '等级');
INSERT INTO `validation_data` VALUES ('condition', 'sex', '性别');
INSERT INTO `validation_data` VALUES ('condition', 'vip', 'VIP等级');

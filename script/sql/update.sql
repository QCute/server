-- -------------------------------------------------------------------
-- :tag:
-- -------------------------------------------------------------------
-- 2015-07-10


-- -------------------------------------------------------------------
-- :tag:
-- -------------------------------------------------------------------
-- 2017-03-11


-- 2017-06-22

-- -------------------------------------------------------------------
-- :tag:
-- -------------------------------------------------------------------
-- 2019-03-31
REPLACE INTO `parameter_data` SET `key` = 'login_cd', `value` = '180', `description` = '登录时间间隔';
REPLACE INTO `validity_data` SET `type` = 'event', `key` = 'event_add_friend', `value` = '添加好友', `description` = '添加好友';

-- 2019-04-15


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

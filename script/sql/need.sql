-- ------------------------------------------------------------------
-- :tag:
-- ------------------------------------------------------------------
-- 2019-12-12
DROP TABLE IF EXISTS `increment`;
CREATE TABLE IF NOT EXISTS `increment` (
    `name` CHAR(255) NOT NULL DEFAULT 0 COMMENT '名字',
    `value` BIGINT(20) UNSIGNED NOT NULL DEFAULT 0 COMMENT '数值',
    PRIMARY KEY (`name`)
) ENGINE = InnoDB CHARACTER SET = utf8mb4 COLLATE = utf8mb4_general_ci COMMENT = '自增表' ROW_FORMAT = Dynamic;


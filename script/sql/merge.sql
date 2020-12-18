-- ------------------------------------------------------------------
-- server merge sql set
-- 1. role name conflict resolve
-- 2. guild name conflict resolve
-- 3. rank order conflict reorder
-- n. etc...
-- 
-- ------------------------------------------------------------------


-- ------------------------------------------------------------------
-- role table
-- ------------------------------------------------------------------

-- query conflict set
-- SELECT 
-- {{src}}.`role`.`role_id` AS `src_role_id`, {{dst}}.`role`.`role_id` AS `dst_role_id`
-- FROM {{src}}.`role`
-- INNER JOIN {{dst}}.`role` 
-- ON {{src}}.`role`.`role_name` = {{dst}}.`role`.`role_name`;


-- insert mail compensate
-- src 
INSERT INTO {{src}}.`mail` 
( `role_id`, `receive_time`, `expire_time`, `title`, `content`, `attachment`, `from` )
( 
  SELECT {{src}}.`role`.`role_id`, UNIX_TIMESTAMP(), UNIX_TIMESTAMP() + (15 * 86400), '', '', '', 'merge_server' 
  FROM {{src}}.`role` 
  INNER JOIN {{dst}}.`role` 
  ON {{src}}.`role`.`role_name` = {{dst}}.`role`.`role_name` 
);

-- dst
INSERT INTO {{dst}}.`mail` 
( `role_id`, `receive_time`, `expire_time`, `title`, `content`, `attachment`, `from` )
( 
  SELECT {{dst}}.`role`.`role_id`, UNIX_TIMESTAMP(), UNIX_TIMESTAMP() + (15 * 86400), '', '', '', 'merge_server' 
  FROM {{dst}}.`role` 
  INNER JOIN {{src}}.`role` 
  ON {{dst}}.`role`.`role_name` = {{src}}.`role`.`role_name` 
);

-- update role name
UPDATE
  {{src}}.`role`
INNER JOIN
  {{dst}}.`role`
ON
  {{src}}.`role`.`role_name` = {{dst}}.`role`.`role_name` 
SET
  {{src}}.`role`.`role_name` = CONCAT({{src}}.`role`.`role_name`, '(', {{src_server_id}}, ')'),
  {{dst}}.`role`.`role_name` = CONCAT({{dst}}.`role`.`role_name`, '(', {{dst_server_id}}, ')');

-- the server id and the brackets 6 chars 
-- avoid update name conflict again

-- ------------------------------------------------------------------
-- guild table
-- ------------------------------------------------------------------

-- query conflict set
-- SELECT {{src}}.`guild`.`guild_id` FROM {{src}}.`guild`
-- UNION ALL
-- SELECT {{dst}}.`guild`.`guild_id` FROM {{dst}}.`guild`
-- INNER JOIN {{src}}.`guild` 
-- ON {{src}}.`guild`.`guild_name` = {{dst}}.`guild`.`guild_name`;

-- insert mail compensate
-- src 
INSERT INTO {{src}}.`mail` 
( `role_id`, `receive_time`, `expire_time`, `title`, `content`, `attachment`, `from` )
( 
  SELECT {{src}}.`guild`.`leader_id`, UNIX_TIMESTAMP(), UNIX_TIMESTAMP() + (15 * 86400), '', '', '', 'merge_server' 
  FROM {{src}}.`guild` 
  INNER JOIN {{dst}}.`guild` 
  ON {{src}}.`guild`.`guild_name` = {{dst}}.`guild`.`guild_name` 
);

-- dst
INSERT INTO {{dst}}.`mail` 
( `role_id`, `receive_time`, `expire_time`, `title`, `content`, `attachment`, `from` )
( 
  SELECT {{dst}}.`guild`.`leader_id`, UNIX_TIMESTAMP(), UNIX_TIMESTAMP() + (15 * 86400), '', '', '', 'merge_server' 
  FROM {{dst}}.`guild` 
  INNER JOIN {{src}}.`guild` 
  ON {{dst}}.`guild`.`guild_name` = {{src}}.`guild`.`guild_name` 
);

-- update guild name
UPDATE
  {{src}}.`guild`
INNER JOIN
  {{dst}}.`guild`
ON
  {{src}}.`guild`.`guild_name` = {{dst}}.`guild`.`guild_name` 
SET
  {{src}}.`guild`.`guild_name` = CONCAT({{src}}.`guild`.`guild_name`, '(', {{src_server_id}}, ')'),
  {{dst}}.`guild`.`guild_name` = CONCAT({{dst}}.`guild`.`guild_name`, '(', {{dst_server_id}}, ')');

-- the server id and the brackets 6 chars 
-- avoid update name conflict again

-- ------------------------------------------------------------------
-- rank table
-- ------------------------------------------------------------------

-- backup src
RENAME TABLE {{src}}.`rank` TO {{src}}.`rank_merge_backup`;
CREATE TABLE {{src}}.`rank` LIKE {{src}}.`rank_merge_backup`;
-- backup dst
RENAME TABLE {{dst}}.`rank` TO {{dst}}.`rank_merge_backup`;
CREATE TABLE {{dst}}.`rank` LIKE {{dst}}.`rank_merge_backup`;

-- @doc merge rank by type
-- migrate data

-- INSERT INTO {{dst}}.`rank`
--   ( `type`, `order`, `key`, `value`, `time`, `name`, `server_id`, `digest`, `extra`, `other` )
-- SELECT
--   `type`, `rank` AS `order`, `key`, `value`, `time`, `name`, `server_id`, `digest`, `extra`, `other`
-- FROM
--   (
-- 	SELECT *, @rank := IF(@type != `type`, 1, @rank + 1) AS `rank`, @type := `type`
-- 	FROM
-- 	  ( SELECT * FROM {{src}}.`rank` UNION ALL SELECT * FROM {{dst}}.`rank_merge_backup` ) AS r,
-- 	  ( SELECT @rank := 0, @type := NULL ) AS t
-- 	ORDER BY `type` ASC, `value` DESC, `time` ASC, `key` ASC
--   ) AS s
-- WHERE `order` <= 100;

-- use ROW_NUMBER OVER ( PARTITION BY )
-- MySQL 8.0/MariaDB 10.2 or later
-- INSERT INTO {{dst}}.`rank`
--   ( `type`, `order`, `key`, `value`, `time`, `name`, `server_id`, `digest`, `extra`, `other` )
-- SELECT * FROM
--   (
--     SELECT `type`, ROW_NUMBER() OVER ( PARTITION BY `type` ORDER BY `value` DESC, `time` ASC, `key` ASC) AS `order`, `key`, `value`, `time`, `name`, `server_id`, `digest`, `extra`, `other`
--     FROM ( SELECT * FROM {{src}}.`rank` UNION ALL SELECT * FROM {{dst}}.`rank_merge_backup` ) AS `rank`
--   ) AS `rank`
-- WHERE `order` <= 100;

-- resort rank and set to src 
INSERT INTO {{src}}.`rank` ( `type`, `order`, `key`, `value`, `time`, `name`, `server_id`, `digest`, `extra`, `other` )
SELECT * FROM
(
  SELECT `type`, ROW_NUMBER() OVER ( PARTITION BY `type` ORDER BY `value` DESC, `time` ASC, `key` ASC) AS `order`, `key`, `value`, `time`, `name`, `server_id`, `digest`, `extra`, `other`
  FROM ( SELECT * FROM {{src}}.`rank` UNION ALL SELECT * FROM {{dst}}.`rank` ) AS `rank`
) `rank` WHERE `server_id` = {{src_server_id}} AND `order` <= 100;

-- resort rank and set to dst 
INSERT INTO {{dst}}.`rank` ( `type`, `order`, `key`, `value`, `time`, `name`, `server_id`, `digest`, `extra`, `other` )
SELECT * FROM
(
  SELECT `type`, ROW_NUMBER() OVER ( PARTITION BY `type` ORDER BY `value` DESC, `time` ASC, `key` ASC) AS `order`, `key`, `value`, `time`, `name`, `server_id`, `digest`, `extra`, `other`
  FROM ( SELECT * FROM {{src}}.`rank_merge_backup` UNION ALL SELECT * FROM {{dst}}.`rank_merge_backup` ) AS `rank`
) `rank` WHERE `server_id` = {{dst_server_id}} AND `order` <= 100;

-- remove backup
DROP TABLE IF EXISTS {{src}}.`rank_merge_backup`;
DROP TABLE IF EXISTS {{dst}}.`rank_merge_backup`;

-- ------------------------------------------------------------------
-- @doc 
-- update all auto increment and ref field offset
-- use sql to make update server id sql
-- @make_update_sql_start 
-- SELECT 
-- IF
-- (
--   `SQL` IS NOT NULL,
--   CONCAT('UPDATE {{src}}.`', `TABLE_NAME`, '` SET', GROUP_CONCAT(`SQL` ORDER BY `ORDINAL_POSITION`), ';'),
--   CONCAT('Unknown Reference Field: ', `TABLE_FIELD`, ', set join(`table`.`field`) or ref(`table`.`field`) in comment to fix it.')
-- ) AS `SQL`
-- FROM
-- (
-- 	SELECT 
-- 		`TABLE_NAME`, 
-- 		`COLUMN_NAME`, 
-- 		`ORDINAL_POSITION`, 
-- 		CONCAT( ' `', `COLUMN_NAME`, '` = `', `COLUMN_NAME`, '` - ({{src_server_id}} * 1000000000) + IFNULL((SELECT MAX({{dst}}.`', `TARGET_TABLE_NAME`, '`.`', `TARGET_COLUMN_NAME`, '`) FROM {{dst}}.`', `TARGET_TABLE_NAME`, '`), 0)') AS `SQL`,
--    CONCAT('`', `TABLE_NAME`, '`.`', `COLUMN_NAME`, '`') AS `TABLE_FIELD`
-- 	FROM
-- 	(
-- 		SELECT 
-- 			`TABLE_NAME`,
-- 			`COLUMN_NAME`,
-- 			IF(
-- 				REGEXP_SUBSTR(SUBSTRING_INDEX(REGEXP_SUBSTR(`COLUMN_COMMENT`, '(?<=join\\()`?\\w+`?\\s*\\.\\s*`?\\w+`?(?=\\))|(?<=ref\\()`?\\w+`?\\s*\\.\\s*`?\\w+`?(?=\\))'), '.', -1), '\\w+') != '',
-- 				REGEXP_SUBSTR(SUBSTRING_INDEX(REGEXP_SUBSTR(`COLUMN_COMMENT`, '(?<=join\\()`?\\w+`?\\s*\\.\\s*`?\\w+`?(?=\\))|(?<=ref\\()`?\\w+`?\\s*\\.\\s*`?\\w+`?(?=\\))'), '.', -1), '\\w+'),
-- 				IF(`COLUMN_NAME` = 'id', CONCAT(`TABLE_NAME`, '_', `COLUMN_NAME`), `COLUMN_NAME`)
-- 			) AS `FIX_COLUMN_NAME`,
-- 			`ORDINAL_POSITION`
-- 		FROM information_schema.`COLUMNS` 
-- 		WHERE `TABLE_SCHEMA` = DATABASE() AND `TABLE_NAME` NOT LIKE '%data' AND `IS_GENERATED` = 'NEVER' AND `DATA_TYPE` = 'bigint' AND ( `COLUMN_NAME` LIKE '%id' OR `COLUMN_NAME` LIKE '%no' )
-- 	) AS `TABLE_COLUMNS`
-- 	LEFT JOIN 
-- 	( 
-- 		SELECT `TABLE_NAME` AS `TARGET_TABLE_NAME`, `COLUMN_NAME` AS `TARGET_COLUMN_NAME`, IF(`COLUMN_NAME` = 'id', CONCAT(`TABLE_NAME`, '_', `COLUMN_NAME`), `COLUMN_NAME`) AS `TARGET_FIX_COLUMN_NAME` 
-- 		FROM information_schema.`COLUMNS` 
-- 		WHERE `TABLE_SCHEMA` = DATABASE() AND `EXTRA` = 'auto_increment' 
-- 	) AS `TARGET_TABLE_COLUMNS`
-- 	ON `FIX_COLUMN_NAME` = `TARGET_FIX_COLUMN_NAME` 
-- ) AS `TABLE_SQL`
-- GROUP BY `TABLE_NAME` ORDER BY `TABLE_NAME` DESC
-- @make_update_sql_end
-- ------------------------------------------------------------------
-- @update_sql_start
UPDATE {{src}}.`asset` SET `role_id` = `role_id` - ({{src_server_id}} * 1000000000) + IFNULL((SELECT MAX({{dst}}.`role`.`role_id`) FROM {{dst}}.`role`), 0);
UPDATE {{src}}.`auction` SET `auction_no` = `auction_no` - ({{src_server_id}} * 1000000000) + IFNULL((SELECT MAX({{dst}}.`auction`.`auction_no`) FROM {{dst}}.`auction`), 0), `guild_id` = `guild_id` - ({{src_server_id}} * 1000000000) + IFNULL((SELECT MAX({{dst}}.`guild`.`guild_id`) FROM {{dst}}.`guild`), 0);
UPDATE {{src}}.`auction_log` SET `id` = `id` - ({{src_server_id}} * 1000000000) + IFNULL((SELECT MAX({{dst}}.`auction_log`.`id`) FROM {{dst}}.`auction_log`), 0), `role_id` = `role_id` - ({{src_server_id}} * 1000000000) + IFNULL((SELECT MAX({{dst}}.`role`.`role_id`) FROM {{dst}}.`role`), 0);
UPDATE {{src}}.`auction_role` SET `auction_no` = `auction_no` - ({{src_server_id}} * 1000000000) + IFNULL((SELECT MAX({{dst}}.`auction`.`auction_no`) FROM {{dst}}.`auction`), 0), `role_id` = `role_id` - ({{src_server_id}} * 1000000000) + IFNULL((SELECT MAX({{dst}}.`role`.`role_id`) FROM {{dst}}.`role`), 0), `guild_id` = `guild_id` - ({{src_server_id}} * 1000000000) + IFNULL((SELECT MAX({{dst}}.`guild`.`guild_id`) FROM {{dst}}.`guild`), 0);
UPDATE {{src}}.`buff` SET `role_id` = `role_id` - ({{src_server_id}} * 1000000000) + IFNULL((SELECT MAX({{dst}}.`role`.`role_id`) FROM {{dst}}.`role`), 0);
UPDATE {{src}}.`count` SET `role_id` = `role_id` - ({{src_server_id}} * 1000000000) + IFNULL((SELECT MAX({{dst}}.`role`.`role_id`) FROM {{dst}}.`role`), 0);
UPDATE {{src}}.`dungeon` SET `role_id` = `role_id` - ({{src_server_id}} * 1000000000) + IFNULL((SELECT MAX({{dst}}.`role`.`role_id`) FROM {{dst}}.`role`), 0);
UPDATE {{src}}.`friend` SET `role_id` = `role_id` - ({{src_server_id}} * 1000000000) + IFNULL((SELECT MAX({{dst}}.`role`.`role_id`) FROM {{dst}}.`role`), 0), `friend_id` = `friend_id` - ({{src_server_id}} * 1000000000) + IFNULL((SELECT MAX({{dst}}.`role`.`role_id`) FROM {{dst}}.`role`), 0);
UPDATE {{src}}.`guild` SET `guild_id` = `guild_id` - ({{src_server_id}} * 1000000000) + IFNULL((SELECT MAX({{dst}}.`guild`.`guild_id`) FROM {{dst}}.`guild`), 0), `leader_id` = `leader_id` - ({{src_server_id}} * 1000000000) + IFNULL((SELECT MAX({{dst}}.`role`.`role_id`) FROM {{dst}}.`role`), 0);
UPDATE {{src}}.`guild_apply` SET `guild_id` = `guild_id` - ({{src_server_id}} * 1000000000) + IFNULL((SELECT MAX({{dst}}.`guild`.`guild_id`) FROM {{dst}}.`guild`), 0), `role_id` = `role_id` - ({{src_server_id}} * 1000000000) + IFNULL((SELECT MAX({{dst}}.`role`.`role_id`) FROM {{dst}}.`role`), 0);
UPDATE {{src}}.`guild_role` SET `guild_id` = `guild_id` - ({{src_server_id}} * 1000000000) + IFNULL((SELECT MAX({{dst}}.`guild`.`guild_id`) FROM {{dst}}.`guild`), 0), `role_id` = `role_id` - ({{src_server_id}} * 1000000000) + IFNULL((SELECT MAX({{dst}}.`role`.`role_id`) FROM {{dst}}.`role`), 0);
UPDATE {{src}}.`item` SET `item_no` = `item_no` - ({{src_server_id}} * 1000000000) + IFNULL((SELECT MAX({{dst}}.`item`.`item_no`) FROM {{dst}}.`item`), 0), `role_id` = `role_id` - ({{src_server_id}} * 1000000000) + IFNULL((SELECT MAX({{dst}}.`role`.`role_id`) FROM {{dst}}.`role`), 0);
UPDATE {{src}}.`item_consume_log` SET `id` = `id` - ({{src_server_id}} * 1000000000) + IFNULL((SELECT MAX({{dst}}.`item_consume_log`.`id`) FROM {{dst}}.`item_consume_log`), 0), `role_id` = `role_id` - ({{src_server_id}} * 1000000000) + IFNULL((SELECT MAX({{dst}}.`role`.`role_id`) FROM {{dst}}.`role`), 0);
UPDATE {{src}}.`item_produce_log` SET `id` = `id` - ({{src_server_id}} * 1000000000) + IFNULL((SELECT MAX({{dst}}.`item_produce_log`.`id`) FROM {{dst}}.`item_produce_log`), 0), `role_id` = `role_id` - ({{src_server_id}} * 1000000000) + IFNULL((SELECT MAX({{dst}}.`role`.`role_id`) FROM {{dst}}.`role`), 0);
UPDATE {{src}}.`key` SET `role_id` = `role_id` - ({{src_server_id}} * 1000000000) + IFNULL((SELECT MAX({{dst}}.`role`.`role_id`) FROM {{dst}}.`role`), 0);
UPDATE {{src}}.`login_log` SET `id` = `id` - ({{src_server_id}} * 1000000000) + IFNULL((SELECT MAX({{dst}}.`login_log`.`id`) FROM {{dst}}.`login_log`), 0), `role_id` = `role_id` - ({{src_server_id}} * 1000000000) + IFNULL((SELECT MAX({{dst}}.`role`.`role_id`) FROM {{dst}}.`role`), 0);
UPDATE {{src}}.`lucky_money` SET `lucky_money_id` = `lucky_money_id` - ({{src_server_id}} * 1000000000) + IFNULL((SELECT MAX({{dst}}.`lucky_money`.`lucky_money_id`) FROM {{dst}}.`lucky_money`), 0), `role_id` = `role_id` - ({{src_server_id}} * 1000000000) + IFNULL((SELECT MAX({{dst}}.`role`.`role_id`) FROM {{dst}}.`role`), 0), `guild_id` = `guild_id` - ({{src_server_id}} * 1000000000) + IFNULL((SELECT MAX({{dst}}.`guild`.`guild_id`) FROM {{dst}}.`guild`), 0);
UPDATE {{src}}.`lucky_money_role` SET `lucky_money_id` = `lucky_money_id` - ({{src_server_id}} * 1000000000) + IFNULL((SELECT MAX({{dst}}.`lucky_money`.`lucky_money_id`) FROM {{dst}}.`lucky_money`), 0), `role_id` = `role_id` - ({{src_server_id}} * 1000000000) + IFNULL((SELECT MAX({{dst}}.`role`.`role_id`) FROM {{dst}}.`role`), 0), `guild_id` = `guild_id` - ({{src_server_id}} * 1000000000) + IFNULL((SELECT MAX({{dst}}.`guild`.`guild_id`) FROM {{dst}}.`guild`), 0);
UPDATE {{src}}.`mail` SET `mail_id` = `mail_id` - ({{src_server_id}} * 1000000000) + IFNULL((SELECT MAX({{dst}}.`mail`.`mail_id`) FROM {{dst}}.`mail`), 0), `role_id` = `role_id` - ({{src_server_id}} * 1000000000) + IFNULL((SELECT MAX({{dst}}.`role`.`role_id`) FROM {{dst}}.`role`), 0);
UPDATE {{src}}.`online_log` SET `id` = `id` - ({{src_server_id}} * 1000000000) + IFNULL((SELECT MAX({{dst}}.`online_log`.`id`) FROM {{dst}}.`online_log`), 0);
UPDATE {{src}}.`quest` SET `role_id` = `role_id` - ({{src_server_id}} * 1000000000) + IFNULL((SELECT MAX({{dst}}.`role`.`role_id`) FROM {{dst}}.`role`), 0);
UPDATE {{src}}.`quest_log` SET `id` = `id` - ({{src_server_id}} * 1000000000) + IFNULL((SELECT MAX({{dst}}.`quest_log`.`id`) FROM {{dst}}.`quest_log`), 0), `role_id` = `role_id` - ({{src_server_id}} * 1000000000) + IFNULL((SELECT MAX({{dst}}.`role`.`role_id`) FROM {{dst}}.`role`), 0);
UPDATE {{src}}.`recharge` SET `recharge_no` = `recharge_no` - ({{src_server_id}} * 1000000000) + IFNULL((SELECT MAX({{dst}}.`recharge`.`recharge_no`) FROM {{dst}}.`recharge`), 0), `role_id` = `role_id` - ({{src_server_id}} * 1000000000) + IFNULL((SELECT MAX({{dst}}.`role`.`role_id`) FROM {{dst}}.`role`), 0);
UPDATE {{src}}.`role` SET `role_id` = `role_id` - ({{src_server_id}} * 1000000000) + IFNULL((SELECT MAX({{dst}}.`role`.`role_id`) FROM {{dst}}.`role`), 0);
UPDATE {{src}}.`role_log` SET `id` = `id` - ({{src_server_id}} * 1000000000) + IFNULL((SELECT MAX({{dst}}.`role_log`.`id`) FROM {{dst}}.`role_log`), 0), `role_id` = `role_id` - ({{src_server_id}} * 1000000000) + IFNULL((SELECT MAX({{dst}}.`role`.`role_id`) FROM {{dst}}.`role`), 0);
UPDATE {{src}}.`shop` SET `role_id` = `role_id` - ({{src_server_id}} * 1000000000) + IFNULL((SELECT MAX({{dst}}.`role`.`role_id`) FROM {{dst}}.`role`), 0);
UPDATE {{src}}.`shop_log` SET `id` = `id` - ({{src_server_id}} * 1000000000) + IFNULL((SELECT MAX({{dst}}.`shop_log`.`id`) FROM {{dst}}.`shop_log`), 0), `role_id` = `role_id` - ({{src_server_id}} * 1000000000) + IFNULL((SELECT MAX({{dst}}.`role`.`role_id`) FROM {{dst}}.`role`), 0);
UPDATE {{src}}.`sign` SET `role_id` = `role_id` - ({{src_server_id}} * 1000000000) + IFNULL((SELECT MAX({{dst}}.`role`.`role_id`) FROM {{dst}}.`role`), 0);
UPDATE {{src}}.`skill` SET `role_id` = `role_id` - ({{src_server_id}} * 1000000000) + IFNULL((SELECT MAX({{dst}}.`role`.`role_id`) FROM {{dst}}.`role`), 0);
UPDATE {{src}}.`title` SET `role_id` = `role_id` - ({{src_server_id}} * 1000000000) + IFNULL((SELECT MAX({{dst}}.`role`.`role_id`) FROM {{dst}}.`role`), 0);
UPDATE {{src}}.`title_log` SET `id` = `id` - ({{src_server_id}} * 1000000000) + IFNULL((SELECT MAX({{dst}}.`title_log`.`id`) FROM {{dst}}.`title_log`), 0), `role_id` = `role_id` - ({{src_server_id}} * 1000000000) + IFNULL((SELECT MAX({{dst}}.`role`.`role_id`) FROM {{dst}}.`role`), 0);
UPDATE {{src}}.`vip` SET `role_id` = `role_id` - ({{src_server_id}} * 1000000000) + IFNULL((SELECT MAX({{dst}}.`role`.`role_id`) FROM {{dst}}.`role`), 0);
-- @update_sql_end


-- other update sql write here



-- ------------------------------------------------------------------
-- @doc 
-- update src server data server id
-- use sql to make update server id sql
-- @make_update_server_id_sql_start 
-- SELECT CONCAT('UPDATE {{src}}.`', `TABLE_NAME`, '` SET ', GROUP_CONCAT(CONCAT('`', `COLUMN_NAME`, '` = {{dst_server_id}};'))) AS `SQL` 
-- FROM information_schema.`COLUMNS` 
-- WHERE `TABLE_SCHEMA` = DATABASE() AND `TABLE_NAME` NOT LIKE '%data' AND `COLUMN_NAME` LIKE '%server_id' AND `IS_GENERATED` = 'NEVER' AND 
-- NOT ( `TABLE_NAME` = 'role' AND `COLUMN_NAME` = 'origin_server_id' ) 
-- GROUP BY `TABLE_NAME`
-- ORDER BY `TABLE_NAME` DESC
-- @make_update_server_id_sql_end
-- ------------------------------------------------------------------
-- @update_server_id_sql_start
UPDATE {{src}}.`auction_log` SET `server_id` = {{dst_server_id}};
UPDATE {{src}}.`auction_role` SET `server_id` = {{dst_server_id}};
UPDATE {{src}}.`lucky_money` SET `server_id` = {{dst_server_id}};
UPDATE {{src}}.`lucky_money_role` SET `server_id` = {{dst_server_id}};
UPDATE {{src}}.`rank` SET `server_id` = {{dst_server_id}};
UPDATE {{src}}.`recharge` SET `server_id` = {{dst_server_id}};
UPDATE {{src}}.`role` SET `server_id` = {{dst_server_id}};
-- @update_server_id_sql_end


-- other server id update sql write here



-- ------------------------------------------------------------------
-- @doc
-- the migrate table data sql
-- merge user's and log's table
-- merge some except table
-- except generated field
-- use this sql make merge sql
-- @make_merge_sql_start 
-- SELECT CONCAT('INSERT INTO {{dst}}.`', `TABLE_NAME`, '` (', GROUP_CONCAT( `COLUMN_NAME` ORDER BY `ORDINAL_POSITION` ASC ), ') SELECT ', GROUP_CONCAT( `COLUMN_NAME` ORDER BY `ORDINAL_POSITION` ASC ), ' FROM {{src}}.`', `TABLE_NAME`, '`;' ) AS `merge_sql` 
-- FROM 
-- ( 
--   SELECT `TABLE_NAME`, CONCAT('`', `COLUMN_NAME`, '`') AS `COLUMN_NAME`, `ORDINAL_POSITION` 
--   FROM information_schema.`COLUMNS` 
--   WHERE `TABLE_SCHEMA` = DATABASE() AND `TABLE_NAME` NOT LIKE '%_data' AND `TABLE_NAME` NOT IN ( '' ) AND `IS_GENERATED` = 'NEVER' 
-- ) AS `table` 
-- WHERE `TABLE_NAME` NOT IN ( 'increment' )
-- GROUP BY `TABLE_NAME` 
-- ORDER BY `TABLE_NAME` DESC
-- @make_merge_sql_end 
-- ------------------------------------------------------------------
-- @merge_sql_start
INSERT INTO {{dst}}.`asset` (`role_id`,`gold`,`silver`,`copper`,`coin`,`exp`) SELECT `role_id`,`gold`,`silver`,`copper`,`coin`,`exp` FROM {{src}}.`asset`;
INSERT INTO {{dst}}.`auction` (`auction_no`,`auction_id`,`number`,`type`,`bid_type`,`start_time`,`end_time`,`from`,`bid_number`,`now_price`,`next_price`,`guild_id`) SELECT `auction_no`,`auction_id`,`number`,`type`,`bid_type`,`start_time`,`end_time`,`from`,`bid_number`,`now_price`,`next_price`,`guild_id` FROM {{src}}.`auction`;
INSERT INTO {{dst}}.`auction_log` (`id`,`auction_id`,`number`,`bid_number`,`price`,`role_id`,`role_name`,`server_id`,`time`) SELECT `id`,`auction_id`,`number`,`bid_number`,`price`,`role_id`,`role_name`,`server_id`,`time` FROM {{src}}.`auction_log`;
INSERT INTO {{dst}}.`auction_role` (`auction_no`,`server_id`,`role_id`,`role_name`,`guild_id`,`guild_name`,`type`,`price`,`time`) SELECT `auction_no`,`server_id`,`role_id`,`role_name`,`guild_id`,`guild_name`,`type`,`price`,`time` FROM {{src}}.`auction_role`;
INSERT INTO {{dst}}.`buff` (`role_id`,`buff_id`,`expire_time`,`overlap`) SELECT `role_id`,`buff_id`,`expire_time`,`overlap` FROM {{src}}.`buff`;
INSERT INTO {{dst}}.`count` (`role_id`,`type`,`today_number`,`week_number`,`total_number`,`time`) SELECT `role_id`,`type`,`today_number`,`week_number`,`total_number`,`time` FROM {{src}}.`count`;
INSERT INTO {{dst}}.`dungeon` (`role_id`,`dungeon_id`,`type`,`today_number`,`total_number`,`is_pass`) SELECT `role_id`,`dungeon_id`,`type`,`today_number`,`total_number`,`is_pass` FROM {{src}}.`dungeon`;
INSERT INTO {{dst}}.`friend` (`role_id`,`friend_id`,`relation`,`time`) SELECT `role_id`,`friend_id`,`relation`,`time` FROM {{src}}.`friend`;
INSERT INTO {{dst}}.`guild` (`guild_id`,`exp`,`wealth`,`level`,`create_time`,`guild_name`,`notice`,`leader_id`) SELECT `guild_id`,`exp`,`wealth`,`level`,`create_time`,`guild_name`,`notice`,`leader_id` FROM {{src}}.`guild`;
INSERT INTO {{dst}}.`guild_apply` (`guild_id`,`role_id`,`apply_time`) SELECT `guild_id`,`role_id`,`apply_time` FROM {{src}}.`guild_apply`;
INSERT INTO {{dst}}.`guild_role` (`guild_id`,`role_id`,`job`,`wealth`,`join_time`,`leave_time`) SELECT `guild_id`,`role_id`,`job`,`wealth`,`join_time`,`leave_time` FROM {{src}}.`guild_role`;
INSERT INTO {{dst}}.`item` (`item_no`,`role_id`,`item_id`,`type`,`number`,`expire_time`) SELECT `item_no`,`role_id`,`item_id`,`type`,`number`,`expire_time` FROM {{src}}.`item`;
INSERT INTO {{dst}}.`item_consume_log` (`id`,`role_id`,`item_id`,`operation`,`from`,`time`) SELECT `id`,`role_id`,`item_id`,`operation`,`from`,`time` FROM {{src}}.`item_consume_log`;
INSERT INTO {{dst}}.`item_produce_log` (`id`,`role_id`,`item_id`,`operation`,`from`,`time`) SELECT `id`,`role_id`,`item_id`,`operation`,`from`,`time` FROM {{src}}.`item_produce_log`;
INSERT INTO {{dst}}.`key` (`role_id`,`key`) SELECT `role_id`,`key` FROM {{src}}.`key`;
INSERT INTO {{dst}}.`login_log` (`id`,`role_id`,`ip`,`device_id`,`login_time`,`online_time`,`logout_time`,`time`) SELECT `id`,`role_id`,`ip`,`device_id`,`login_time`,`online_time`,`logout_time`,`time` FROM {{src}}.`login_log`;
INSERT INTO {{dst}}.`lucky_money` (`lucky_money_id`,`server_id`,`role_id`,`role_name`,`guild_id`,`guild_name`,`total_gold`,`remain_gold`,`total_number`,`receive_number`,`time`) SELECT `lucky_money_id`,`server_id`,`role_id`,`role_name`,`guild_id`,`guild_name`,`total_gold`,`remain_gold`,`total_number`,`receive_number`,`time` FROM {{src}}.`lucky_money`;
INSERT INTO {{dst}}.`lucky_money_role` (`lucky_money_id`,`server_id`,`role_id`,`role_name`,`guild_id`,`guild_name`,`gold`,`time`) SELECT `lucky_money_id`,`server_id`,`role_id`,`role_name`,`guild_id`,`guild_name`,`gold`,`time` FROM {{src}}.`lucky_money_role`;
INSERT INTO {{dst}}.`mail` (`mail_id`,`role_id`,`receive_time`,`is_read`,`read_time`,`expire_time`,`is_receive_attachment`,`receive_attachment_time`,`title`,`content`,`attachment`,`from`) SELECT `mail_id`,`role_id`,`receive_time`,`is_read`,`read_time`,`expire_time`,`is_receive_attachment`,`receive_attachment_time`,`title`,`content`,`attachment`,`from` FROM {{src}}.`mail`;
INSERT INTO {{dst}}.`online_log` (`id`,`all`,`online`,`hosting`,`hour`,`time`) SELECT `id`,`all`,`online`,`hosting`,`hour`,`time` FROM {{src}}.`online_log`;
INSERT INTO {{dst}}.`quest` (`role_id`,`quest_id`,`type`,`target`,`number`,`is_award`) SELECT `role_id`,`quest_id`,`type`,`target`,`number`,`is_award` FROM {{src}}.`quest`;
INSERT INTO {{dst}}.`quest_log` (`id`,`role_id`,`quest_id`,`time`) SELECT `id`,`role_id`,`quest_id`,`time` FROM {{src}}.`quest_log`;
INSERT INTO {{dst}}.`rank` (`type`,`order`,`key`,`value`,`time`,`name`,`server_id`,`digest`,`extra`,`other`) SELECT `type`,`order`,`key`,`value`,`time`,`name`,`server_id`,`digest`,`extra`,`other` FROM {{src}}.`rank`;
INSERT INTO {{dst}}.`recharge` (`recharge_no`,`recharge_id`,`order_id`,`channel`,`role_id`,`role_name`,`server_id`,`account_name`,`money`,`status`,`time`) SELECT `recharge_no`,`recharge_id`,`order_id`,`channel`,`role_id`,`role_name`,`server_id`,`account_name`,`money`,`status`,`time` FROM {{src}}.`recharge`;
INSERT INTO {{dst}}.`role` (`role_id`,`role_name`,`server_id`,`account_name`,`origin_server_id`,`level`,`sex`,`classes`,`type`,`status`,`is_online`,`register_time`,`login_time`,`logout_time`,`first_recharge_time`,`last_recharge_time`,`recharge_total`,`item_size`,`bag_size`,`store_size`,`map`,`channel`,`device_id`,`device_type`,`mac`,`ip`) SELECT `role_id`,`role_name`,`server_id`,`account_name`,`origin_server_id`,`level`,`sex`,`classes`,`type`,`status`,`is_online`,`register_time`,`login_time`,`logout_time`,`first_recharge_time`,`last_recharge_time`,`recharge_total`,`item_size`,`bag_size`,`store_size`,`map`,`channel`,`device_id`,`device_type`,`mac`,`ip` FROM {{src}}.`role`;
INSERT INTO {{dst}}.`role_log` (`id`,`role_id`,`exp`,`time`) SELECT `id`,`role_id`,`exp`,`time` FROM {{src}}.`role_log`;
INSERT INTO {{dst}}.`shop` (`role_id`,`shop_id`,`number`) SELECT `role_id`,`shop_id`,`number` FROM {{src}}.`shop`;
INSERT INTO {{dst}}.`shop_log` (`id`,`role_id`,`shop_id`,`number`,`time`) SELECT `id`,`role_id`,`shop_id`,`number`,`time` FROM {{src}}.`shop_log`;
INSERT INTO {{dst}}.`sign` (`role_id`,`login_day`,`sign_total`,`is_sign_today`) SELECT `role_id`,`login_day`,`sign_total`,`is_sign_today` FROM {{src}}.`sign`;
INSERT INTO {{dst}}.`skill` (`role_id`,`skill_id`,`level`) SELECT `role_id`,`skill_id`,`level` FROM {{src}}.`skill`;
INSERT INTO {{dst}}.`title` (`role_id`,`title_id`,`type`,`expire_time`) SELECT `role_id`,`title_id`,`type`,`expire_time` FROM {{src}}.`title`;
INSERT INTO {{dst}}.`title_log` (`id`,`role_id`,`title_id`,`from`,`time`) SELECT `id`,`role_id`,`title_id`,`from`,`time` FROM {{src}}.`title_log`;
INSERT INTO {{dst}}.`vip` (`role_id`,`vip_level`,`exp`,`expire_time`) SELECT `role_id`,`vip_level`,`exp`,`expire_time` FROM {{src}}.`vip`;
-- @merge_sql_end

-- other merge sql write here



-- ------------------------------------------------------------------
-- after merge
-- ------------------------------------------------------------------


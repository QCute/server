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
  SELECT {{src}}.`guild`.`leader_role_id`, UNIX_TIMESTAMP(), UNIX_TIMESTAMP() + (15 * 86400), '', '', '', 'merge_server'
  FROM {{src}}.`guild`
  INNER JOIN {{dst}}.`guild`
  ON {{src}}.`guild`.`guild_name` = {{dst}}.`guild`.`guild_name`
);

-- dst
INSERT INTO {{dst}}.`mail`
( `role_id`, `receive_time`, `expire_time`, `title`, `content`, `attachment`, `from` )
(
  SELECT {{dst}}.`guild`.`leader_role_id`, UNIX_TIMESTAMP(), UNIX_TIMESTAMP() + (15 * 86400), '', '', '', 'merge_server'
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

-- merge rank by type
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
INSERT INTO {{dst}}.`achievement` (`role_id`,`achievement_id`,`type`) SELECT `role_id`,`achievement_id`,`type` FROM {{src}}.`achievement`;
INSERT INTO {{dst}}.`achievement_log` (`id`,`role_id`,`achievement_id`,`time`) SELECT `id`,`role_id`,`achievement_id`,`time` FROM {{src}}.`achievement_log`;
INSERT INTO {{dst}}.`asset` (`role_id`,`gold`,`silver`,`copper`,`coin`,`exp`) SELECT `role_id`,`gold`,`silver`,`copper`,`coin`,`exp` FROM {{src}}.`asset`;
INSERT INTO {{dst}}.`auction` (`auction_no`,`auction_id`,`number`,`type`,`bid_type`,`start_time`,`end_time`,`from`,`bid_number`,`now_price`,`next_price`,`guild_id`) SELECT `auction_no`,`auction_id`,`number`,`type`,`bid_type`,`start_time`,`end_time`,`from`,`bid_number`,`now_price`,`next_price`,`guild_id` FROM {{src}}.`auction`;
INSERT INTO {{dst}}.`auction_log` (`id`,`auction_id`,`number`,`bid_number`,`price`,`role_id`,`role_name`,`server_id`,`time`) SELECT `id`,`auction_id`,`number`,`bid_number`,`price`,`role_id`,`role_name`,`server_id`,`time` FROM {{src}}.`auction_log`;
INSERT INTO {{dst}}.`auction_role` (`auction_no`,`server_id`,`role_id`,`role_name`,`guild_id`,`guild_name`,`type`,`price`,`time`) SELECT `auction_no`,`server_id`,`role_id`,`role_name`,`guild_id`,`guild_name`,`type`,`price`,`time` FROM {{src}}.`auction_role`;
INSERT INTO {{dst}}.`bubble` (`role_id`,`bubble_id`,`type`,`expire_time`) SELECT `role_id`,`bubble_id`,`type`,`expire_time` FROM {{src}}.`bubble`;
INSERT INTO {{dst}}.`bubble_log` (`id`,`role_id`,`bubble_id`,`from`,`time`) SELECT `id`,`role_id`,`bubble_id`,`from`,`time` FROM {{src}}.`bubble_log`;
INSERT INTO {{dst}}.`buff` (`role_id`,`buff_id`,`expire_time`,`overlap`) SELECT `role_id`,`buff_id`,`expire_time`,`overlap` FROM {{src}}.`buff`;
INSERT INTO {{dst}}.`count` (`role_id`,`type`,`today_number`,`week_number`,`total_number`,`time`) SELECT `role_id`,`type`,`today_number`,`week_number`,`total_number`,`time` FROM {{src}}.`count`;
INSERT INTO {{dst}}.`daily` (`role_id`,`daily_id`,`is_award`) SELECT `role_id`,`daily_id`,`is_award` FROM {{src}}.`daily`;
INSERT INTO {{dst}}.`daily_active` (`role_id`,`stage_id`,`score`) SELECT `role_id`,`stage_id`,`score` FROM {{src}}.`daily_active`;
INSERT INTO {{dst}}.`dungeon` (`role_id`,`dungeon_id`,`type`,`today_number`,`total_number`,`is_pass`) SELECT `role_id`,`dungeon_id`,`type`,`today_number`,`total_number`,`is_pass` FROM {{src}}.`dungeon`;
INSERT INTO {{dst}}.`fashion` (`role_id`,`fashion_id`,`type`,`expire_time`) SELECT `role_id`,`fashion_id`,`type`,`expire_time` FROM {{src}}.`fashion`;
INSERT INTO {{dst}}.`fashion_log` (`id`,`role_id`,`fashion_id`,`from`,`time`) SELECT `id`,`role_id`,`fashion_id`,`from`,`time` FROM {{src}}.`fashion_log`;
INSERT INTO {{dst}}.`friend` (`role_id`,`friend_role_id`,`relation`,`time`) SELECT `role_id`,`friend_role_id`,`relation`,`time` FROM {{src}}.`friend`;
INSERT INTO {{dst}}.`guild` (`guild_id`,`guild_name`,`exp`,`wealth`,`level`,`create_time`,`notice`,`leader_role_id`) SELECT `guild_id`,`guild_name`,`exp`,`wealth`,`level`,`create_time`,`notice`,`leader_role_id` FROM {{src}}.`guild`;
INSERT INTO {{dst}}.`guild_apply` (`guild_id`,`role_id`,`apply_time`) SELECT `guild_id`,`role_id`,`apply_time` FROM {{src}}.`guild_apply`;
INSERT INTO {{dst}}.`guild_role` (`guild_id`,`role_id`,`job`,`wealth`,`join_time`,`leave_time`) SELECT `guild_id`,`role_id`,`job`,`wealth`,`join_time`,`leave_time` FROM {{src}}.`guild_role`;
INSERT INTO {{dst}}.`item` (`item_no`,`role_id`,`item_id`,`type`,`number`,`expire_time`) SELECT `item_no`,`role_id`,`item_id`,`type`,`number`,`expire_time` FROM {{src}}.`item`;
INSERT INTO {{dst}}.`item_consume_log` (`id`,`role_id`,`item_id`,`operation`,`from`,`time`) SELECT `id`,`role_id`,`item_id`,`operation`,`from`,`time` FROM {{src}}.`item_consume_log`;
INSERT INTO {{dst}}.`item_produce_log` (`id`,`role_id`,`item_id`,`operation`,`from`,`time`) SELECT `id`,`role_id`,`item_id`,`operation`,`from`,`time` FROM {{src}}.`item_produce_log`;
INSERT INTO {{dst}}.`key` (`role_id`,`key`) SELECT `role_id`,`key` FROM {{src}}.`key`;
INSERT INTO {{dst}}.`login_log` (`id`,`role_id`,`ip`,`device_id`,`login_time`,`online_time`,`logout_time`,`time`) SELECT `id`,`role_id`,`ip`,`device_id`,`login_time`,`online_time`,`logout_time`,`time` FROM {{src}}.`login_log`;
INSERT INTO {{dst}}.`lucky_money` (`lucky_money_no`,`server_id`,`role_id`,`role_name`,`guild_id`,`guild_name`,`total_gold`,`remain_gold`,`total_number`,`receive_number`,`scope`,`restrict`,`skin`,`message`,`time`) SELECT `lucky_money_no`,`server_id`,`role_id`,`role_name`,`guild_id`,`guild_name`,`total_gold`,`remain_gold`,`total_number`,`receive_number`,`scope`,`restrict`,`skin`,`message`,`time` FROM {{src}}.`lucky_money`;
INSERT INTO {{dst}}.`lucky_money_role` (`lucky_money_no`,`server_id`,`role_id`,`role_name`,`guild_id`,`guild_name`,`gold`,`time`) SELECT `lucky_money_no`,`server_id`,`role_id`,`role_name`,`guild_id`,`guild_name`,`gold`,`time` FROM {{src}}.`lucky_money_role`;
INSERT INTO {{dst}}.`mail` (`mail_id`,`role_id`,`receive_time`,`expire_time`,`read_time`,`receive_attachment_time`,`title`,`content`,`attachment`,`from`) SELECT `mail_id`,`role_id`,`receive_time`,`expire_time`,`read_time`,`receive_attachment_time`,`title`,`content`,`attachment`,`from` FROM {{src}}.`mail`;
INSERT INTO {{dst}}.`online_log` (`id`,`total`,`online`,`hosting`,`hour`,`time`) SELECT `id`,`total`,`online`,`hosting`,`hour`,`time` FROM {{src}}.`online_log`;
INSERT INTO {{dst}}.`rank` (`type`,`order`,`key`,`value`,`time`,`name`,`server_id`,`digest`,`extra`,`other`) SELECT `type`,`order`,`key`,`value`,`time`,`name`,`server_id`,`digest`,`extra`,`other` FROM {{src}}.`rank`;
INSERT INTO {{dst}}.`recharge` (`recharge_no`,`recharge_id`,`order_id`,`channel`,`role_id`,`role_name`,`money`,`status`,`time`) SELECT `recharge_no`,`recharge_id`,`order_id`,`channel`,`role_id`,`role_name`,`money`,`status`,`time` FROM {{src}}.`recharge`;
INSERT INTO {{dst}}.`role` (`role_id`,`role_name`,`server_id`,`account_name`,`origin_server_id`,`type`,`status`,`sex`,`avatar`,`classes`,`level`,`is_online`,`register_time`,`login_time`,`logout_time`,`first_recharge_time`,`last_recharge_time`,`recharge_total`,`item_size`,`bag_size`,`store_size`,`map`,`channel`,`device_id`,`device_type`,`mac`,`ip`) SELECT `role_id`,`role_name`,`server_id`,`account_name`,`origin_server_id`,`type`,`status`,`sex`,`avatar`,`classes`,`level`,`is_online`,`register_time`,`login_time`,`logout_time`,`first_recharge_time`,`last_recharge_time`,`recharge_total`,`item_size`,`bag_size`,`store_size`,`map`,`channel`,`device_id`,`device_type`,`mac`,`ip` FROM {{src}}.`role`;
INSERT INTO {{dst}}.`role_log` (`id`,`role_id`,`exp`,`time`) SELECT `id`,`role_id`,`exp`,`time` FROM {{src}}.`role_log`;
INSERT INTO {{dst}}.`shop` (`role_id`,`shop_id`,`number`) SELECT `role_id`,`shop_id`,`number` FROM {{src}}.`shop`;
INSERT INTO {{dst}}.`shop_log` (`id`,`role_id`,`shop_id`,`number`,`time`) SELECT `id`,`role_id`,`shop_id`,`number`,`time` FROM {{src}}.`shop_log`;
INSERT INTO {{dst}}.`sign` (`role_id`,`login_day`,`sign_total`,`is_sign_today`) SELECT `role_id`,`login_day`,`sign_total`,`is_sign_today` FROM {{src}}.`sign`;
INSERT INTO {{dst}}.`skill` (`role_id`,`skill_id`,`level`) SELECT `role_id`,`skill_id`,`level` FROM {{src}}.`skill`;
INSERT INTO {{dst}}.`state` (`name`,`value`) SELECT `name`,`value` FROM {{src}}.`state`;
INSERT INTO {{dst}}.`task` (`role_id`,`task_id`,`type`,`number`,`is_award`) SELECT `role_id`,`task_id`,`type`,`number`,`is_award` FROM {{src}}.`task`;
INSERT INTO {{dst}}.`task_log` (`id`,`role_id`,`task_id`,`time`) SELECT `id`,`role_id`,`task_id`,`time` FROM {{src}}.`task_log`;
INSERT INTO {{dst}}.`title` (`role_id`,`title_id`,`type`,`expire_time`) SELECT `role_id`,`title_id`,`type`,`expire_time` FROM {{src}}.`title`;
INSERT INTO {{dst}}.`title_log` (`id`,`role_id`,`title_id`,`from`,`time`) SELECT `id`,`role_id`,`title_id`,`from`,`time` FROM {{src}}.`title_log`;
INSERT INTO {{dst}}.`vip` (`role_id`,`vip_level`,`exp`,`expire_time`) SELECT `role_id`,`vip_level`,`exp`,`expire_time` FROM {{src}}.`vip`;
-- @merge_sql_end

-- other merge sql write here



-- ------------------------------------------------------------------
-- after merge
-- ------------------------------------------------------------------


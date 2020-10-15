-- ------------------------------------------------------------------
-- server merge sql set
-- ✔ ✖ ✘ ➦ ⚡
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
-- SELECT {{src}}.`role`.`role_id` FROM {{src}}.`role`
-- UNION ALL
-- SELECT {{dst}}.`role`.`role_id` FROM {{dst}}.`role`
-- INNER JOIN {{src}}.`role` 
-- ON {{src}}.`role`.`role_name` = {{dst}}.`role`.`role_name`;

-- insert mail compensate
INSERT INTO {{dst}}.`mail` (`sender_id`, `sender_nick`, `receiver_id`, `receiver_nick`, `receive_time`, `from`, `title`, `content`, `attachment`) 
(
  SELECT '0', 'system', {{src}}.`role`.`role_id`, {{src}}.`role`.`role_name`, UNIX_TIMESTAMP(), 'system', 'rename card', 'rename card', '[{600001, 1}]' FROM {{src}}.`role`
  UNION ALL 
  SELECT '0', 'system', {{dst}}.`role`.`role_id`, {{dst}}.`role`.`role_name`, UNIX_TIMESTAMP(), 'system', 'rename card', 'rename card award', '[{600001, 1}]' FROM {{dst}}.`role`
  INNER JOIN {{src}}.`role` ON {{src}}.`role`.`role_name` = {{dst}}.`role`.`role_name`
);

-- update role name to role id
UPDATE
  {{dst}}.`role`
LEFT JOIN
  {{src}}.`role`
ON
  {{dst}}.`role`.`role_name` = {{src}}.`role`.`role_name` 
SET
  {{dst}}.`role`.`role_name` = {{dst}}.`role`.`role_id`, {{src}}.`role`.`role_name` = {{src}}.`role`.`role_id`;

-- update server id
UPDATE {{dst}}.`role` SET `server_id` = {{server_id}};


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
INSERT INTO {{dst}}.`mail` (`sender_id`, `sender_nick`, `receiver_id`, `receiver_nick`, `receive_time`, `from`, `title`, `content`, `attachment`) 
(
  SELECT '0', 'system', {{src}}.`guild`.`guild_id`, {{src}}.`guild`.`guild_name`, UNIX_TIMESTAMP(), 'system', 'rename card', 'rename card', '[{600001, 1}]' FROM {{src}}.`guild`
  UNION ALL
  SELECT '0', 'system', {{dst}}.`guild`.`guild_id`, {{dst}}.`guild`.`guild_name`, UNIX_TIMESTAMP(), 'system', 'rename card', 'rename card', '[{600001, 1}]' FROM {{dst}}.`guild`
  INNER JOIN {{src}}.`guild` ON {{src}}.`guild`.`guild_name` = {{dst}}.`guild`.`guild_name`
);

-- update guild name to guild id
UPDATE
  {{dst}}.`guild`
LEFT JOIN
  {{src}}.`guild`
ON
  {{dst}}.`guild`.`guild_name` = {{src}}.`guild`.`guild_name` 
SET
  {{dst}}.`guild`.`guild_name` = {{dst}}.`guild`.`guild_id`, {{src}}.`guild`.`guild_name` = {{src}}.`guild`.`guild_id`;

-- ------------------------------------------------------------------
-- rank table
-- ------------------------------------------------------------------

-- backup
RENAME TABLE {{dst}}.`rank` TO {{dst}}.`rank_merge_backup`;
CREATE TABLE {{dst}}.`rank` LIKE {{dst}}.`rank_merge_backup`;

-- @doc merge rank by type
-- migrate data

-- INSERT INTO {{dst}}.`rank`
-- (
--   SELECT `type`, row_number() OVER (ORDER BY r.`type`, r.`value` desc, r.`time` ASC, r.`key` ASC) AS `order`, `key`, `value`, `time`, `name`, `server_id`, `digest`, `extra`, `other`, '' FROM
--     (SELECT * FROM {{src}}.`rank` UNION ALL SELECT * FROM {{dst}}.`rank_merge_backup`) AS r,
-- 	  ( SELECT GROUP_CONCAT( CONCAT('{', `type`, '-', `key`, '}') ORDER BY `value` DESC ) AS `key_set` FROM (SELECT * FROM {{src}}.`rank` UNION ALL SELECT * FROM {{dst}}.`rank_merge_backup`) AS mr GROUP BY `type` ) AS t
--  WHERE
--	  FIND_IN_SET( CONCAT('{', r.`type`, '-', r.`key`, '}'), t.`key_set` ) BETWEEN 1 AND 100
--	ORDER by `type` ASC, `value` DESC, `time` ASC, `key` ASC
-- );

INSERT INTO {{dst}}.`rank`
  ( `type`, `order`, `key`, `value`, `time`, `name`, `server_id`, `digest`, `extra`, `other` )
SELECT
  `type`, `rank` AS `order`, `key`, `value`, `time`, `name`, `server_id`, `digest`, `extra`, `other`
FROM
  (
	SELECT *, @rank := IF(@type != `type`, 1, @rank + 1) AS `rank`, @type := `type`
	FROM
	  ( SELECT * FROM {{src}}.`rank` UNION ALL SELECT * FROM {{dst}}.`rank_merge_backup` ) AS r,
	  ( SELECT @rank := 0, @type := NULL ) AS t
	ORDER BY `type` ASC, `value` DESC, `time` ASC, `key` ASC
  ) AS s
WHERE `order` <= 100;

-- use ROW_NUMBER and PARTITION BY
-- INSERT INTO {{dst}}.`rank`
--   ( `type`, `order`, `key`, `value`, `time`, `name`, `server_id`, `digest`, `extra`, `other` )
-- SELECT * FROM
--   (
--     SELECT `type`, ROW_NUMBER() OVER ( PARTITION BY `type` ORDER BY `value` DESC, `time` ASC, `key` ASC) AS `order`, `key`, `value`, `time`, `name`, `server_id`, `digest`, `extra`, `other`
--     FROM ( SELECT * FROM {{src}}.`rank` UNION ALL SELECT * FROM {{dst}}.`rank_merge_backup` ) AS `rank`
--   ) AS `rank`
-- WHERE `order` <= 100;

-- update server_id
UPDATE {{dst}}.`rank` SET `server_id` = {{server_id}};
-- remove backup
DROP TABLE IF EXISTS {{dst}}.`rank_merge_backup`;


-- ------------------------------------------------------------------
-- other table
-- merge user's and log's table
-- merge some except table
-- @doc
-- SELECT CONCAT('INSERT INTO {{dst}}.`', `TABLE_NAME`, '` SELECT * FROM {{src}}.`', `TABLE_NAME`, '`;') FROM information_schema.`TABLES` WHERE `TABLE_SCHEMA` IN ('local') AND `TABLE_NAME` NOT LIKE '%_data' AND `TABLE_NAME` NOT IN ('role', 'guild', 'rank') ORDER BY `TABLE_NAME` ASC
-- @doc
-- EXCEPT GENERATE FIELD
-- USE THIS SQL MAKE MERGE SQL
-- @merge_sql SELECT CONCAT('INSERT INTO {{dst}}.`', `TABLE_NAME`, '` (', GROUP_CONCAT( `COLUMN_NAME` ORDER BY `ORDINAL_POSITION` ASC ), ') SELECT ', GROUP_CONCAT( `COLUMN_NAME` ORDER BY `ORDINAL_POSITION` ASC ), ' FROM {{src}}.`', `TABLE_NAME`, '`;' ) AS `merge_sql` FROM ( SELECT `TABLE_NAME`, CONCAT('`', `COLUMN_NAME`, '`') AS `COLUMN_NAME`, `ORDINAL_POSITION` FROM information_schema.`COLUMNS` WHERE `TABLE_SCHEMA` IN ( 'local' ) AND `TABLE_NAME` NOT LIKE '%_data' AND `TABLE_NAME` NOT IN ( 'rank' ) AND `IS_GENERATED` = 'NEVER' ) AS `table` GROUP BY `TABLE_NAME` ORDER BY `TABLE_NAME` ASC
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
INSERT INTO {{dst}}.`increment` (`name`,`value`) SELECT `name`,`value` FROM {{src}}.`increment`;
INSERT INTO {{dst}}.`item` (`item_no`,`role_id`,`item_id`,`type`,`number`,`expire_time`) SELECT `item_no`,`role_id`,`item_id`,`type`,`number`,`expire_time` FROM {{src}}.`item`;
INSERT INTO {{dst}}.`item_consume_log` (`id`,`role_id`,`item_id`,`operation`,`source`,`time`) SELECT `id`,`role_id`,`item_id`,`operation`,`source`,`time` FROM {{src}}.`item_consume_log`;
INSERT INTO {{dst}}.`item_produce_log` (`id`,`role_id`,`item_id`,`operation`,`source`,`time`) SELECT `id`,`role_id`,`item_id`,`operation`,`source`,`time` FROM {{src}}.`item_produce_log`;
INSERT INTO {{dst}}.`key` (`role_id`,`key`) SELECT `role_id`,`key` FROM {{src}}.`key`;
INSERT INTO {{dst}}.`login_log` (`id`,`role_id`,`ip`,`device_id`,`login_time`,`online_time`,`time`) SELECT `id`,`role_id`,`ip`,`device_id`,`login_time`,`online_time`,`time` FROM {{src}}.`login_log`;
INSERT INTO {{dst}}.`lucky_money` (`lucky_money_id`,`server_id`,`role_id`,`role_name`,`guild_id`,`guild_name`,`total_gold`,`remain_gold`,`total_number`,`receive_number`,`time`) SELECT `lucky_money_id`,`server_id`,`role_id`,`role_name`,`guild_id`,`guild_name`,`total_gold`,`remain_gold`,`total_number`,`receive_number`,`time` FROM {{src}}.`lucky_money`;
INSERT INTO {{dst}}.`lucky_money_role` (`lucky_money_id`,`server_id`,`role_id`,`role_name`,`guild_id`,`guild_name`,`gold`,`time`) SELECT `lucky_money_id`,`server_id`,`role_id`,`role_name`,`guild_id`,`guild_name`,`gold`,`time` FROM {{src}}.`lucky_money_role`;
INSERT INTO {{dst}}.`mail` (`mail_id`,`sender_id`,`sender_nick`,`receiver_id`,`receiver_nick`,`receive_time`,`is_read`,`read_time`,`expire_time`,`is_receive_attachment`,`receive_attachment_time`,`from`,`title`,`content`,`attachment`) SELECT `mail_id`,`sender_id`,`sender_nick`,`receiver_id`,`receiver_nick`,`receive_time`,`is_read`,`read_time`,`expire_time`,`is_receive_attachment`,`receive_attachment_time`,`from`,`title`,`content`,`attachment` FROM {{src}}.`mail`;
INSERT INTO {{dst}}.`online_log` (`id`,`all`,`online`,`hosting`,`hour`,`time`) SELECT `id`,`all`,`online`,`hosting`,`hour`,`time` FROM {{src}}.`online_log`;
INSERT INTO {{dst}}.`quest` (`role_id`,`quest_id`,`type`,`event`,`target`,`number`,`compare`,`award`) SELECT `role_id`,`quest_id`,`type`,`event`,`target`,`number`,`compare`,`award` FROM {{src}}.`quest`;
INSERT INTO {{dst}}.`quest_log` (`id`,`role_id`,`quest_id`,`time`) SELECT `id`,`role_id`,`quest_id`,`time` FROM {{src}}.`quest_log`;
INSERT INTO {{dst}}.`recharge` (`recharge_no`,`recharge_id`,`channel`,`server_id`,`role_id`,`role_name`,`account`,`money`,`status`,`time`,`receive_time`) SELECT `recharge_no`,`recharge_id`,`channel`,`server_id`,`role_id`,`role_name`,`account`,`money`,`status`,`time`,`receive_time` FROM {{src}}.`recharge`;
INSERT INTO {{dst}}.`role` (`role_id`,`role_name`,`server_id`,`account`,`type`,`level`,`sex`,`classes`,`item_size`,`bag_size`,`store_size`,`online`,`online_time`,`register_time`,`first_recharge_time`,`channel`,`map`,`device_id`,`device_type`,`mac`,`ip`) SELECT `role_id`,`role_name`,`server_id`,`account`,`type`,`level`,`sex`,`classes`,`item_size`,`bag_size`,`store_size`,`online`,`online_time`,`register_time`,`first_recharge_time`,`channel`,`map`,`device_id`,`device_type`,`mac`,`ip` FROM {{src}}.`role`;
INSERT INTO {{dst}}.`role_log` (`id`,`role_id`,`exp`,`time`) SELECT `id`,`role_id`,`exp`,`time` FROM {{src}}.`role_log`;
INSERT INTO {{dst}}.`shop` (`role_id`,`shop_id`,`number`) SELECT `role_id`,`shop_id`,`number` FROM {{src}}.`shop`;
INSERT INTO {{dst}}.`shop_log` (`id`,`role_id`,`shop_id`,`number`,`time`) SELECT `id`,`role_id`,`shop_id`,`number`,`time` FROM {{src}}.`shop_log`;
INSERT INTO {{dst}}.`sign` (`role_id`,`login_day`,`sign_total`,`is_sign_today`) SELECT `role_id`,`login_day`,`sign_total`,`is_sign_today` FROM {{src}}.`sign`;
INSERT INTO {{dst}}.`skill` (`role_id`,`skill_id`,`level`) SELECT `role_id`,`skill_id`,`level` FROM {{src}}.`skill`;
INSERT INTO {{dst}}.`title` (`role_id`,`title_id`,`type`,`expire_time`) SELECT `role_id`,`title_id`,`type`,`expire_time` FROM {{src}}.`title`;
INSERT INTO {{dst}}.`title_log` (`id`,`role_id`,`title_id`,`from`,`time`) SELECT `id`,`role_id`,`title_id`,`from`,`time` FROM {{src}}.`title_log`;
INSERT INTO {{dst}}.`total_login_log` (`id`,`total`,`hour_list`,`time`) SELECT `id`,`total`,`hour_list`,`time` FROM {{src}}.`total_login_log`;
INSERT INTO {{dst}}.`vip` (`role_id`,`vip_level`,`exp`,`expire_time`) SELECT `role_id`,`vip_level`,`exp`,`expire_time` FROM {{src}}.`vip`;
-- @merge_sql_end

-- ------------------------------------------------------------------
-- after merge
-- ------------------------------------------------------------------


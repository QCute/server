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
INSERT INTO `{{dst}}`.`mail` (`sender_id`, `sender_nick`, `receiver_id`, `receiver_nick`, `receive_time`, `from`, `title`, `content`, `attachment`) 
(
  SELECT '0', 'system', {{src}}.`role`.`role_id`, {{src}}.`role`.`role_name`, UNIX_TIMESTAMP(), 'system', 'rename card', 'rename card award', '[{600001, 1}]' FROM {{src}}.`role`
  UNION ALL 
  SELECT '0', 'system', {{dst}}.`role`.`role_id`, {{dst}}.`role`.`role_name`, UNIX_TIMESTAMP(), 'system', 'rename card', 'rename card award', '[{600001, 1}]' FROM {{dst}}.`role`
  INNER JOIN {{src}}.`role` ON {{src}}.`role`.`role_name` = {{dst}}.`role`.`role_name`
);

-- backup
-- RENAME TABLE `{{dst}}`.`role` TO `{{dst}}`.`role_merge_backup`;
-- CREATE TABLE `{{dst}}`.`role` AS `{{dst}}`.`role_merge_backup`;

-- update role name to role id
UPDATE
  {{dst}}.`role`
LEFT JOIN
  {{src}}.`role`
ON
  {{dst}}.`role`.`role_name` = {{src}}.`role`.`role_name` 
SET
  {{dst}}.`role`.`role_name` = {{dst}}.`role`.`role_id`, {{src}}.`role`.`role_name` = {{src}}.`role`.`role_id`;

-- migrate data
INSERT INTO {{dst}}.`role` SELECT * FROM {{src}}.`role`;
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
INSERT INTO `{{dst}}`.`mail` (`sender_id`, `sender_nick`, `receiver_id`, `receiver_nick`, `receive_time`, `from`, `title`, `content`, `attachment`) 
(
  SELECT '0', 'system', {{src}}.`guild`.`guild_id`, {{src}}.`guild`.`guild_name`, UNIX_TIMESTAMP(), 'system', 'rename card', 'rename card award', '[{600001, 1}]' FROM {{src}}.`guild`
  UNION ALL
  SELECT '0', 'system', {{dst}}.`guild`.`guild_id`, {{dst}}.`guild`.`guild_name`, UNIX_TIMESTAMP(), 'system', 'rename card', 'rename card award', '[{600001, 1}]' FROM {{dst}}.`guild`
  INNER JOIN {{src}}.`guild` ON {{src}}.`guild`.`guild_name` = {{dst}}.`guild`.`guild_name`
);

-- backup
-- RENAME TABLE `{{dst}}`.`guild` TO `{{dst}}`.`guild_merge_backup`;
-- CREATE TABLE `{{dst}}`.`guild` AS `{{dst}}`.`guild_merge_backup`;

-- update guild name to guild id
UPDATE
  {{dst}}.`guild`
LEFT JOIN
  {{src}}.`guild`
ON
  {{dst}}.`guild`.`guild_name` = {{src}}.`guild`.`guild_name` 
SET
  {{dst}}.`guild`.`guild_name` = {{dst}}.`guild`.`guild_id`, {{src}}.`guild`.`guild_name` = {{src}}.`guild`.`guild_id`;

-- migrate data
INSERT INTO {{dst}}.`guild` SELECT * FROM {{src}}.`guild` ;


-- ------------------------------------------------------------------
-- rank table
-- ------------------------------------------------------------------

-- backup
RENAME TABLE `{{dst}}`.`rank` TO `{{dst}}`.`rank_merge_backup`;
CREATE TABLE `{{dst}}`.`rank` LIKE `{{dst}}`.`rank_merge_backup`;

-- query all type set
-- select `type` from `rank` group by `type`;

-- merge rank and reorder 
-- SET @row_number = 0;
-- migrate/merge data
-- INSERT INTO {{dst}}.`rank` (`type`, `key`, `value`, `time`, `rank`, `name`, `extra`, `other`) 
-- (
     -- reorder merge set
--   SELECT `type`, `key`, `value`, `time`, @row_number := @row_number + 1, `name`, `extra`, `other` FROM 
--   (
--     SELECT * FROM {{src}}.`rank` WHERE type = '1'
--     UNION ALL
--     SELECT * FROM {{dst}}.`rank` WHERE type = '1'
--   ) AS r
--   ORDER BY r.`value` desc, r.`time` ASC, r.`key` ASC 
   -- rank number
-- LIMIT 100
-- ) 
-- duplicate key 
-- ON DUPLICATE KEY UPDATE `key` = VALUES(`key`), `value` = VALUES(`value`), `time` = VALUES(`time`), `name` = VALUES(`name`), `extra` = VALUES(`extra`), `other` = VALUES(`other`)


-- or MySQL 8.0 ~ MariaDB 10.2 ~
-- migrate/merge data
-- INSERT INTO {{dst}}.`rank` (`type`, `key`, `value`, `time`, `rank`, `name`, `extra`, `other`) 
-- (
  -- use window function row_number() reorder data set
--   SELECT `type`, row_number() OVER (ORDER BY r.`value` desc, r.`time` ASC, r.`key` ASC) AS `rank`, `key`, `value`, `time`, `name`, `extra`, `other` FROM 
--   (
--     SELECT * FROM {{src}}.`rank` WHERE type = {{rank_type}}
--     UNION ALL
--     SELECT * FROM {{dst}}.`rank` WHERE type = {{rank_type}}
--   ) AS r
  -- rank number
--  LIMIT 100
-- ) 
   -- duplicate key 
-- ON DUPLICATE KEY UPDATE `key` = VALUES(`key`), `value` = VALUES(`value`), `time` = VALUES(`time`), `name` = VALUES(`name`), `extra` = VALUES(`extra`), `other` = VALUES(`other`);


INSERT INTO {{dst}}.`rank`
(
  SELECT `type`, row_number() OVER (ORDER BY r.`type`, r.`value` desc, r.`time` ASC, r.`key` ASC) AS `rank` , `key`, `value`, `time`, `name`, `digest`, `extra`, `other`, '' FROM
    (SELECT * FROM {{src}}.`rank` UNION ALL SELECT * FROM {{dst}}.`rank_merge_backup`) AS r,
	  ( SELECT GROUP_CONCAT( CONCAT('{', `type`, '-', `key`, '}') ORDER BY `value` DESC ) AS `key_set` FROM (SELECT * FROM {{src}}.`rank` UNION ALL SELECT * FROM {{dst}}.`rank_merge_backup`) AS mr GROUP BY `type` ) AS t 
  WHERE
	  FIND_IN_SET( CONCAT('{', r.`type`, '-', r.`key`, '}'), t.`key_set` ) BETWEEN 1 AND 100
	ORDER by `type` ASC, `value` DESC, `time` ASC, `key` ASC
);

-- remove backup
DROP TABLE IF EXISTS {{dst}}.`rank_merge_backup`;


-- ------------------------------------------------------------------
-- other table
-- merge user's and log's table
-- merge some except table
-- SELECT CONCAT('INSERT INTO {{dst}}.`', `TABLE_NAME`, '` SELECT * FROM {{src}}.`', `TABLE_NAME`, '`;') FROM information_schema.`TABLES` WHERE `TABLE_SCHEMA` IN ('main') AND `TABLE_NAME` NOT LIKE '%_data' AND `TABLE_NAME` NOT IN ('role', 'guild', 'rank')
-- ------------------------------------------------------------------
INSERT INTO {{dst}}.`asset` SELECT * FROM {{src}}.`asset`;
INSERT INTO {{dst}}.`auction` SELECT * FROM {{src}}.`auction`;
INSERT INTO {{dst}}.`auction_log` SELECT * FROM {{src}}.`auction_log`;
INSERT INTO {{dst}}.`auction_role` SELECT * FROM {{src}}.`auction_role`;
INSERT INTO {{dst}}.`buff` SELECT * FROM {{src}}.`buff`;
INSERT INTO {{dst}}.`count` SELECT * FROM {{src}}.`count`;
INSERT INTO {{dst}}.`dungeon` SELECT * FROM {{src}}.`dungeon`;
INSERT INTO {{dst}}.`friend` SELECT * FROM {{src}}.`friend`;
INSERT INTO {{dst}}.`guild_apply` SELECT * FROM {{src}}.`guild_apply`;
INSERT INTO {{dst}}.`guild_role` SELECT * FROM {{src}}.`guild_role`;
INSERT INTO {{dst}}.`increment` SELECT * FROM {{src}}.`increment`;
INSERT INTO {{dst}}.`item` SELECT * FROM {{src}}.`item`;
INSERT INTO {{dst}}.`item_consume_log` SELECT * FROM {{src}}.`item_consume_log`;
INSERT INTO {{dst}}.`item_produce_log` SELECT * FROM {{src}}.`item_produce_log`;
INSERT INTO {{dst}}.`key` SELECT * FROM {{src}}.`key`;
INSERT INTO {{dst}}.`login_log` SELECT * FROM {{src}}.`login_log`;
INSERT INTO {{dst}}.`lucky_money` SELECT * FROM {{src}}.`lucky_money`;
INSERT INTO {{dst}}.`lucky_money_role` SELECT * FROM {{src}}.`lucky_money_role`;
INSERT INTO {{dst}}.`mail` SELECT * FROM {{src}}.`mail`;
INSERT INTO {{dst}}.`online_log` SELECT * FROM {{src}}.`online_log`;
INSERT INTO {{dst}}.`quest` SELECT * FROM {{src}}.`quest`;
INSERT INTO {{dst}}.`quest_log` SELECT * FROM {{src}}.`quest_log`;
INSERT INTO {{dst}}.`recharge` SELECT * FROM {{src}}.`recharge`;
INSERT INTO {{dst}}.`role_log` SELECT * FROM {{src}}.`role_log`;
INSERT INTO {{dst}}.`shop` SELECT * FROM {{src}}.`shop`;
INSERT INTO {{dst}}.`shop_log` SELECT * FROM {{src}}.`shop_log`;
INSERT INTO {{dst}}.`skill` SELECT * FROM {{src}}.`skill`;
INSERT INTO {{dst}}.`title` SELECT * FROM {{src}}.`title`;
INSERT INTO {{dst}}.`title_log` SELECT * FROM {{src}}.`title_log`;
INSERT INTO {{dst}}.`vip` SELECT * FROM {{src}}.`vip`;

-- ------------------------------------------------------------------
-- after merge
-- ------------------------------------------------------------------


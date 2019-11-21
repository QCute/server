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
SELECT game_src.`role`.`role_id` FROM game_src.`role`
UNION ALL
SELECT game_dst.`role`.`role_id` FROM game_dst.`role`
INNER JOIN game_src.`role` 
ON game_src.`role`.`role_name` = game_dst.`role`.`role_name`;

-- insert mail compensate
INSERT INTO `game_dst`.`mail` (`sender_id`, `sender_nick`, `receiver_id`, `receiver_nick`, `is_read`, `read_time`, `valid_time`, `receive_time`, `from`, `title`, `content`, `attachment`) 
(
  SELECT '0', '', game_src.`role`.`role_id`, game_src.`role`.`role_name`, '0', '0', '0', 'UNIX_TIMESTAMP()', 'system', 'rename card', 'rename card award', '[{600001, 1}]' FROM game_src.`role`
  UNION ALL
  SELECT '0', '', game_dst.`role`.`role_id`, game_dst.`role`.`role_name`, '0', '0', '0', 'UNIX_TIMESTAMP()', 'system', 'rename card', 'rename card award', '[{600001, 1}]' FROM game_dst.`role`
  INNER JOIN game_src.`role` ON game_src.`role`.`role_name` = game_dst.`role`.`role_name`
);

-- backup
RENAME TABLE `game_dst`.`role` TO `game_dst`.`role_merge_backup`;
CREATE TABLE `game_dst`.`role` AS `game_dst`.`role_merge_backup`;

-- update role name to role id
UPDATE
  game_dst.`role`
LEFT JOIN
  game_src.`role`
ON
  game_dst.`role`.`role_name` = game_src.`role`.`role_name` 
SET
  game_dst.`role`.`role_name` = game_dst.`role`.`role_id`, game_src.`role`.`role_name` = game_src.`role`.`role_id`;

-- migrate data
INSERT INTO game_dst.`role` ( SELECT * FROM game_src.`role` )
-- update server id
UPDATE game_dst.`role` SET `server_id` = '1'


-- ------------------------------------------------------------------
-- guild table
-- ------------------------------------------------------------------

-- query conflict set
SELECT game_src.`guild`.`guild_id` FROM game_src.`guild`
UNION ALL
SELECT game_dst.`guild`.`guild_id` FROM game_dst.`guild`
INNER JOIN game_src.`guild` 
ON game_src.`guild`.`guild_name` = game_dst.`guild`.`guild_name`;

-- insert mail compensate
INSERT INTO `game_dst`.`mail` (`sender_id`, `sender_nick`, `receiver_id`, `receiver_nick`, `is_read`, `read_time`, `valid_time`, `receive_time`, `from`, `title`, `content`, `attachment`) 
(
  SELECT '0', '', game_src.`guild`.`guild_id`, game_src.`guild`.`guild_name`, '0', '0', '0', 'UNIX_TIMESTAMP()', 'system', 'rename card', 'rename card award', '[{600001, 1}]' FROM game_src.`guild`
  UNION ALL
  SELECT '0', '', game_dst.`guild`.`guild_id`, game_dst.`guild`.`guild_name`, '0', '0', '0', 'UNIX_TIMESTAMP()', 'system', 'rename card', 'rename card award', '[{600001, 1}]' FROM game_dst.`guild`
  INNER JOIN game_src.`guild` ON game_src.`guild`.`guild_name` = game_dst.`guild`.`guild_name`
);

-- backup
RENAME TABLE `game_dst`.`guild` TO `game_dst`.`guild_merge_backup`;
CREATE TABLE `game_dst`.`guild` AS `game_dst`.`guild_merge_backup`;

-- update guild name to guild id
UPDATE
  game_dst.`guild`
LEFT JOIN
  game_src.`guild`
ON
  game_dst.`guild`.`guild_name` = game_src.`guild`.`guild_name` 
SET
  game_dst.`guild`.`guild_name` = game_dst.`guild`.`guild_id`, game_src.`guild`.`guild_name` = game_src.`guild`.`guild_id`;

-- migrate data
INSERT INTO game_dst.`guild` ( SELECT * FROM game_src.`guild` )


-- ------------------------------------------------------------------
-- rank table
-- ------------------------------------------------------------------

-- backup
RENAME TABLE `game_dst`.`rank` TO `game_dst`.`rank_merge_backup`;
CREATE TABLE `game_dst`.`rank` AS `game_dst`.`rank_merge_backup`;

-- query all type set
select `type` from `rank` group by `type`;

-- merge rank and reorder 
SET @row_number = 0;
-- migrate/merge data
INSERT INTO game_dst.`rank` (`type`, `key`, `value`, `time`, `rank`, `name`, `extra`, `other`) 
(
  -- reorder merge set
  SELECT `type`, `key`, `value`, `time`, @row_number := @row_number + 1, `name`, `extra`, `other` FROM 
  (
    SELECT * FROM game_src.`rank` WHERE type = '1'
    UNION ALL
    SELECT * FROM game_dst.`rank` WHERE type = '1'
  ) AS r
  ORDER BY r.`value` desc, r.`time` ASC, r.`key` ASC 
  -- rank number
  LIMIT 100
) 
-- duplicate key 
ON DUPLICATE KEY UPDATE `key` = VALUES(`key`), `value` = VALUES(`value`), `time` = VALUES(`time`), `name` = VALUES(`name`), `extra` = VALUES(`extra`), `other` = VALUES(`other`)


-- or MySQL 8.0 ~ MariaDB 10.2 ~
-- migrate/merge data
INSERT INTO game_dst.`rank` (`type`, `key`, `value`, `time`, `rank`, `name`, `extra`, `other`) 
(
  -- use window function row_number() reorder data set
  SELECT `type`, row_number() OVER (ORDER BY r.`value` desc, r.`time` ASC, r.`key` ASC) AS `rank`, `key`, `value`, `time`, `name`, `extra`, `other` FROM 
  (
    SELECT * FROM game_src.`rank` WHERE type = '1'
    UNION ALL
    SELECT * FROM game_dst.`rank` WHERE type = '1'
  ) AS r
  -- rank number
  LIMIT 100
) 
-- duplicate key 
ON DUPLICATE KEY UPDATE `key` = VALUES(`key`), `value` = VALUES(`value`), `time` = VALUES(`time`), `name` = VALUES(`name`), `extra` = VALUES(`extra`), `other` = VALUES(`other`)


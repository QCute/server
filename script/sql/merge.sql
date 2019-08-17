-- -------------------------------------------------------------------
-- server merge sql set
-- ✔ ✖
-- ✘ ➦ ⚡
-- role name conflict
-- rank reorder
-- etc...
-- 
-- -------------------------------------------------------------------

-- query conflict set
SELECT game_src.`role`.`role_id`, '', '', '', '0', '0' FROM game_src.`role`
UNION ALL
SELECT game_dst.`role`.`role_id`, '', '', '', '0', '0' FROM game_dst.`role`
INNER JOIN game_src.`role` ON game_src.`role`.`role_name` = game_dst.`role`.`role_name`

-- insert mail compensate
INSERT INTO `game_dst`.`mail` (`sender_id`, `sender_nick`, `receiver_id`, `receiver_nick`, `is_read`, `read_time`, `valid_time`, `receive_time`, `from`, `title`, `content`, `attachment`) 
(
  SELECT '0', '', game_src.`role`.`role_id`, game_src.`role`.`role_name`, '0', '0', '0', '1565971200', 'system', 'rename card', 'rename card award', '[{600001, 1}]' FROM game_src.`role`
  UNION ALL
  SELECT '0', '', game_dst.`role`.`role_id`, game_dst.`role`.`role_name`, '0', '0', '0', '1565971200', 'system', 'rename card', 'rename card award', '[{600001, 1}]' FROM game_dst.`role`
  INNER JOIN game_src.`role` ON game_src.`role`.`role_name` = game_dst.`role`.`role_name`
);

-- update account to user id
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


-- merge rank and reorder 
SET @row_number = 0;
-- migrate/merge data
INSERT INTO game_dst.`rank` (`type`, `key`, `value`, `time`, `rank`, `name`, `other`) 
(
  -- reorder merge set
  SELECT `type`, `key`, `value`, `time`, @row_number := @row_number + 1, `name`, `other` FROM 
  (
    SELECT * FROM game_src.`rank` WHERE type = '1'
    UNION ALL
    SELECT * FROM game_dst.`rank` WHERE type = '1'
  ) AS r
  ORDER BY r.`rank` desc, r.`time` ASC, r.`key` ASC 
  -- rank number
  LIMIT 100
) 
-- duplicate key 
ON DUPLICATE KEY UPDATE `key` = VALUES(`key`), `value` = VALUES(`value`), `time` = VALUES(`time`), `name` = VALUES(`name`), `other` = VALUES(`other`)


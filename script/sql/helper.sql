
-- @doc make truncate table sentence
SELECT CONCAT('TRUNCATE TABLE `', `TABLE_SCHEMA`, '`.`', `TABLE_NAME`, '`;') FROM information_schema.`TABLES` WHERE `TABLE_SCHEMA` IN (DATABASE())

-- @doc make truncate table sentence without data table
SELECT CONCAT('TRUNCATE TABLE `', `TABLE_SCHEMA`, '`.`', `TABLE_NAME`, '`;') FROM information_schema.`TABLES` WHERE `TABLE_SCHEMA` IN (DATABASE()) AND `TABLE_NAME` NOT LIKE '%_data'

-- @doc query without primary key table
SELECT `TABLE_NAME`, COUNT(IF(`COLUMN_KEY` = 'PRI', `COLUMN_KEY`, NULL)) AS `KEY_NUMBER` FROM information_schema.`COLUMNS` WHERE `TABLE_SCHEMA` = DATABASE () GROUP BY `TABLE_NAME` HAVING `KEY_NUMBER` = 0

-- @doc query auto_increment not bigint
SELECT `TABLE_NAME`, `COLUMN_NAME` FROM information_schema.`COLUMNS` WHERE `TABLE_SCHEMA` = DATABASE () AND `EXTRA` = 'auto_increment' AND `DATA_TYPE` != 'bigint'

-- @doc query non bigint auto increment ref field
SELECT `TABLE_NAME`, `COLUMN_NAME`, `DATA_TYPE` FROM information_schema.`COLUMNS` WHERE `TABLE_SCHEMA` = DATABASE() AND `TABLE_NAME` NOT LIKE '%_data' AND `COLUMN_NAME` IN (SELECT `COLUMN_NAME` AS `NAME` FROM information_schema.`COLUMNS` WHERE `TABLE_SCHEMA` = DATABASE() AND `TABLE_NAME` NOT LIKE '%_data' AND `EXTRA` = 'auto_increment' GROUP BY `TABLE_NAME`) AND `DATA_TYPE` != 'bigint' AND `IS_GENERATED` = 'NEVER' ORDER BY `TABLE_NAME`, `ORDINAL_POSITION`

-- @doc query non compressed log table
SELECT `TABLE_NAME`, `TABLE_COMMENT`, `ROW_FORMAT` FROM information_schema.`TABLES` WHERE `TABLE_SCHEMA` = DATABASE() AND `TABLE_NAME` LIKE '%_log' AND `ROW_FORMAT` != 'Compressed'

-- @doc query non log table with compressed
SELECT `TABLE_NAME`, `TABLE_COMMENT`, `ROW_FORMAT` FROM information_schema.`TABLES` WHERE `TABLE_SCHEMA` = DATABASE() AND `ROW_FORMAT` = 'Compressed' AND `TABLE_NAME` NOT LIKE '%_log'

-- @doc duplicate comment table
SELECT `TABLE_NAME`, `TABLE_COMMENT` FROM information_schema.`TABLES` WHERE `TABLE_SCHEMA` = DATABASE() AND `TABLE_COMMENT` IN ( SELECT `TABLE_COMMENT` FROM information_schema.`TABLES` WHERE `TABLE_SCHEMA` = DATABASE() GROUP BY `TABLE_COMMENT` HAVING COUNT(*) > 1 ) ORDER BY `TABLE_COMMENT`

-- @doc query non virtual field default
SELECT `TABLE_NAME`, `COLUMN_NAME` FROM information_schema.`COLUMNS` WHERE `TABLE_SCHEMA` = DATABASE () AND `COLUMN_DEFAULT` != 'NULL' AND `IS_GENERATED` != 'NEVER'

-- @doc auto increment column ref
SELECT
*,
CONCAT('ALTER TABLE `', `TABLE`, '` ADD CONSTRAINT `', `TABLE`, '-', `COLUMN`, '` FOREIGN KEY (`', `COLUMN`, '`) REFERENCES `', `REF_TABLE`, '` (`', `REF_COLUMN`, '`) ON DELETE NO ACTION ON UPDATE CASCADE;') AS `ADD_SQL`,
CONCAT('ALTER TABLE `', `TABLE`, '` DROP FOREIGN KEY `', `TABLE`, '-', `COLUMN`, '`;') AS `DROP_SQL`
FROM
(
  SELECT `TABLE_NAME` AS `TABLE`, `COLUMN_NAME` AS `COLUMN`
  FROM information_schema.`COLUMNS`
  WHERE `TABLE_SCHEMA` = DATABASE()
  AND `TABLE_NAME` NOT LIKE '%_data'                                       -- not data table
  AND ( `COLUMN_NAME` LIKE '%_id' OR `COLUMN_NAME` LIKE '%_no' )           -- like id or number
  AND `DATA_TYPE` = 'bigint'                                               -- not data id
  AND `EXTRA` = ''                                                         -- not auto increment
  AND `IS_GENERATED` = 'NEVER'                                             -- not virtual
) AS `SRC`
LEFT JOIN
(
  SELECT `TABLE_NAME` AS `REF_TABLE`, `COLUMN_NAME` AS `REF_COLUMN`
  FROM information_schema.`COLUMNS`
  WHERE `TABLE_SCHEMA` = DATABASE()
  AND `COLUMN_NAME` != 'id'                                                -- not log id
  AND `EXTRA` = 'auto_increment'                                           -- auto increment
) AS `REF`
ON `SRC`.`COLUMN` = `REF`.`REF_COLUMN`
OR `SRC`.`COLUMN` LIKE CONCAT('%', `REF`.`REF_COLUMN`)
ORDER BY `SRC`.`TABLE`;

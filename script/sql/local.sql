/*!999999\- enable the sandbox mode */ 
-- MariaDB dump 10.19  Distrib 10.11.8-MariaDB, for Linux (x86_64)
--
-- Host: localhost    Database: local
-- ------------------------------------------------------
-- Server version	10.6.18-MariaDB-log

/*!40101 SET @OLD_CHARACTER_SET_CLIENT=@@CHARACTER_SET_CLIENT */;
/*!40101 SET @OLD_CHARACTER_SET_RESULTS=@@CHARACTER_SET_RESULTS */;
/*!40101 SET @OLD_COLLATION_CONNECTION=@@COLLATION_CONNECTION */;
/*!40101 SET NAMES utf8mb4 */;
/*!40103 SET @OLD_TIME_ZONE=@@TIME_ZONE */;
/*!40103 SET TIME_ZONE='+00:00' */;
/*!40014 SET @OLD_UNIQUE_CHECKS=@@UNIQUE_CHECKS, UNIQUE_CHECKS=0 */;
/*!40014 SET @OLD_FOREIGN_KEY_CHECKS=@@FOREIGN_KEY_CHECKS, FOREIGN_KEY_CHECKS=0 */;
/*!40101 SET @OLD_SQL_MODE=@@SQL_MODE, SQL_MODE='NO_AUTO_VALUE_ON_ZERO' */;
/*!40111 SET @OLD_SQL_NOTES=@@SQL_NOTES, SQL_NOTES=0 */;

--
-- Table structure for table `achievement`
--

DROP TABLE IF EXISTS `achievement`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `achievement` (
  `role_id` bigint(20) unsigned NOT NULL DEFAULT 0 COMMENT '角色ID(select_by_role_id)',
  `achievement_id` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '成就ID',
  `type` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '类型',
  `flag` tinyint(3) unsigned GENERATED ALWAYS AS (0) VIRTUAL COMMENT '标识(flag)',
  PRIMARY KEY (`role_id`,`type`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci ROW_FORMAT=DYNAMIC COMMENT='角色成就表';
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `achievement`
--

LOCK TABLES `achievement` WRITE;
/*!40000 ALTER TABLE `achievement` DISABLE KEYS */;
/*!40000 ALTER TABLE `achievement` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `achievement_data`
--

DROP TABLE IF EXISTS `achievement_data`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `achievement_data` (
  `achievement_id` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '成就ID',
  `type` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '类型',
  `count_type` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '统计类型',
  `pre_id` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '前置成就',
  `next_id` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '后置成就',
  `event` enum('event_add_friend','event_dungeon_passed','event_friend_add','event_guild_join','event_kill_monster','event_level_upgrade','event_shop_buy') NOT NULL COMMENT '事件(validate(event))',
  `target` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '目标',
  `number` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '数量',
  `award` varchar(255) NOT NULL DEFAULT '' COMMENT '奖励',
  `title` char(255) NOT NULL DEFAULT '' COMMENT '标题',
  `content` char(255) NOT NULL DEFAULT '' COMMENT '内容',
  `description` char(255) NOT NULL DEFAULT '' COMMENT '描述',
  PRIMARY KEY (`achievement_id`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci ROW_FORMAT=DYNAMIC COMMENT='成就配置表';
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `achievement_data`
--

LOCK TABLES `achievement_data` WRITE;
/*!40000 ALTER TABLE `achievement_data` DISABLE KEYS */;
INSERT INTO `achievement_data` VALUES
(1,1,1,0,2,'event_level_upgrade',0,3,'[{1,1}]','','',''),
(2,1,2,1,3,'event_level_upgrade',5,1,'[{1,10}]','','',''),
(3,1,3,2,0,'event_level_upgrade',2,1,'[{1,100}]','','',''),
(4,2,4,0,4,'event_shop_buy',1,1,'[{1,1000}]','','',''),
(5,2,5,4,5,'event_shop_buy',0,1,'[{1,1000}]','','',''),
(6,2,6,5,0,'event_shop_buy',0,5,'[{1,10}]','','',''),
(7,3,7,0,8,'event_dungeon_passed',3,1,'[{1,10}]','','',''),
(8,3,8,8,9,'event_dungeon_passed',1,1,'[{1,10}]','','',''),
(9,3,9,9,0,'event_dungeon_passed',1,1,'[{1,10}]','','','');
/*!40000 ALTER TABLE `achievement_data` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `achievement_log`
--

DROP TABLE IF EXISTS `achievement_log`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `achievement_log` (
  `id` bigint(20) unsigned NOT NULL AUTO_INCREMENT COMMENT 'ID',
  `role_id` bigint(20) unsigned NOT NULL DEFAULT 0 COMMENT '角色ID',
  `achievement_id` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '成就ID',
  `time` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '时间',
  PRIMARY KEY (`id`) USING BTREE,
  KEY `role_id` (`role_id`) USING BTREE,
  KEY `time` (`time`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci ROW_FORMAT=DYNAMIC COMMENT='成就日志表';
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `achievement_log`
--

LOCK TABLES `achievement_log` WRITE;
/*!40000 ALTER TABLE `achievement_log` DISABLE KEYS */;
/*!40000 ALTER TABLE `achievement_log` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `activity_data`
--

DROP TABLE IF EXISTS `activity_data`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `activity_data` (
  `activity_id` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '活动ID',
  `mode` tinyint(3) unsigned NOT NULL DEFAULT 0 COMMENT '活动模式(validate(node_type_integer))',
  `service` enum('auction_server','boss_server','none') NOT NULL COMMENT '服务进程模块(validate(module))',
  `type` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '类型',
  `subtype` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '子类型',
  `award_type` enum('auto','manual') NOT NULL COMMENT '领奖类型(validate(receive_type))',
  `show_time` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '图标展示时间(时间戳)',
  `start_time` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '开始时间(时间戳)',
  `over_time` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '结束时间(时间戳)',
  `award_time` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '领奖时间(时间戳)',
  `stop_time` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '图标消失时间(时间戳)',
  `show_hour` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '每天展示小时',
  `start_hour` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '每天开始小时',
  `over_hour` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '每天结束小时',
  `start_award_hour` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '每天领奖开始小时',
  `over_award_hour` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '每天领奖结束小时',
  `min_open_days` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '最小开服天数',
  `max_open_days` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '最大开服天数',
  `name` char(255) NOT NULL DEFAULT '' COMMENT '活动名',
  `icon` char(255) NOT NULL DEFAULT '' COMMENT '活动图标',
  `entrance` char(255) NOT NULL DEFAULT '' COMMENT '活动入口',
  `description` char(255) NOT NULL DEFAULT '' COMMENT '活动描述',
  PRIMARY KEY (`activity_id`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci ROW_FORMAT=DYNAMIC COMMENT='活动配置表';
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `activity_data`
--

LOCK TABLES `activity_data` WRITE;
/*!40000 ALTER TABLE `activity_data` DISABLE KEYS */;
INSERT INTO `activity_data` VALUES
(1,1,'auction_server',1,1,'manual',1577808000,1577808000,1577808000,1577808000,1577808000,9,10,22,22,23,3,7,'活动名','activity.icon','activity','活动描述'),
(2,2,'boss_server',1,1,'manual',1577808000,1577808000,1577808000,1577808000,1577808000,9,10,22,22,23,3,7,'活动名','activity.icon','activity','活动描述'),
(3,4,'none',1,1,'manual',1577808000,1577808000,1577808000,1577808000,1577808000,9,10,22,22,23,3,7,'活动名','activity.icon','activity','活动描述');
/*!40000 ALTER TABLE `activity_data` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `asset`
--

DROP TABLE IF EXISTS `asset`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `asset` (
  `role_id` bigint(20) unsigned NOT NULL DEFAULT 0 COMMENT '角色ID',
  `gold` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '金币',
  `silver` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '银币',
  `copper` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '铜币',
  `coin` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '硬币',
  `exp` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '经验',
  PRIMARY KEY (`role_id`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci ROW_FORMAT=DYNAMIC COMMENT='角色资产表';
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `asset`
--

LOCK TABLES `asset` WRITE;
/*!40000 ALTER TABLE `asset` DISABLE KEYS */;
/*!40000 ALTER TABLE `asset` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `asset_consume_count`
--

DROP TABLE IF EXISTS `asset_consume_count`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `asset_consume_count` (
  `role_id` bigint(20) unsigned NOT NULL DEFAULT 0 COMMENT '角色ID(select_by_role_id)',
  `asset` varchar(10) NOT NULL DEFAULT '' COMMENT '资产类型',
  `asset_name` char(10) NOT NULL DEFAULT '' COMMENT '资产类型名字',
  `to` varchar(255) NOT NULL DEFAULT '' COMMENT '去向',
  `to_name` char(255) NOT NULL DEFAULT '' COMMENT '去向名字',
  `number` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '数量',
  `time` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '时间',
  `flag` tinyint(3) unsigned GENERATED ALWAYS AS (0) VIRTUAL COMMENT '标识(flag)',
  PRIMARY KEY (`role_id`,`asset`,`to`) USING BTREE,
  KEY `time` (`time`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci ROW_FORMAT=DYNAMIC COMMENT='资产消耗统计';
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `asset_consume_count`
--

LOCK TABLES `asset_consume_count` WRITE;
/*!40000 ALTER TABLE `asset_consume_count` DISABLE KEYS */;
/*!40000 ALTER TABLE `asset_consume_count` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `asset_consume_log`
--

DROP TABLE IF EXISTS `asset_consume_log`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `asset_consume_log` (
  `id` bigint(20) unsigned NOT NULL AUTO_INCREMENT COMMENT 'ID',
  `role_id` bigint(20) unsigned NOT NULL DEFAULT 0 COMMENT '角色ID',
  `asset` varchar(32) NOT NULL DEFAULT '0' COMMENT '资产',
  `asset_id` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '资产ID',
  `number` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '数量',
  `operation` varchar(32) NOT NULL DEFAULT '' COMMENT '操作',
  `from` varchar(32) NOT NULL DEFAULT '' COMMENT '来源',
  `time` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '时间',
  PRIMARY KEY (`id`) USING BTREE,
  KEY `role_id` (`role_id`) USING BTREE,
  KEY `asset` (`asset`) USING BTREE,
  KEY `asset_id` (`asset_id`) USING BTREE,
  KEY `from` (`from`) USING BTREE,
  KEY `time` (`time`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci ROW_FORMAT=DYNAMIC COMMENT='资产消费日志表';
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `asset_consume_log`
--

LOCK TABLES `asset_consume_log` WRITE;
/*!40000 ALTER TABLE `asset_consume_log` DISABLE KEYS */;
/*!40000 ALTER TABLE `asset_consume_log` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `asset_data`
--

DROP TABLE IF EXISTS `asset_data`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `asset_data` (
  `asset` varchar(255) NOT NULL DEFAULT '' COMMENT '资产类型',
  `item_id` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '物品配置ID',
  PRIMARY KEY (`asset`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci ROW_FORMAT=DYNAMIC COMMENT='资产物品映射配置表';
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `asset_data`
--

LOCK TABLES `asset_data` WRITE;
/*!40000 ALTER TABLE `asset_data` DISABLE KEYS */;
INSERT INTO `asset_data` VALUES
('coin',100004),
('copper',100003),
('exp',100005),
('gold',100001),
('silver',100002);
/*!40000 ALTER TABLE `asset_data` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `asset_produce_count`
--

DROP TABLE IF EXISTS `asset_produce_count`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `asset_produce_count` (
  `role_id` bigint(20) unsigned NOT NULL DEFAULT 0 COMMENT '角色ID(select_by_role_id)',
  `asset` varchar(10) NOT NULL DEFAULT '' COMMENT '资产类型',
  `asset_name` char(10) NOT NULL DEFAULT '' COMMENT '资产类型名字',
  `from` varchar(255) NOT NULL DEFAULT '' COMMENT '来源',
  `from_name` char(255) NOT NULL DEFAULT '' COMMENT '来源名字',
  `number` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '数量',
  `time` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '时间',
  `flag` tinyint(3) unsigned GENERATED ALWAYS AS (0) VIRTUAL COMMENT '标识(flag)',
  PRIMARY KEY (`role_id`,`asset`,`from`) USING BTREE,
  KEY `time` (`time`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci ROW_FORMAT=DYNAMIC COMMENT='资产产出统计';
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `asset_produce_count`
--

LOCK TABLES `asset_produce_count` WRITE;
/*!40000 ALTER TABLE `asset_produce_count` DISABLE KEYS */;
/*!40000 ALTER TABLE `asset_produce_count` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `asset_produce_log`
--

DROP TABLE IF EXISTS `asset_produce_log`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `asset_produce_log` (
  `id` bigint(20) unsigned NOT NULL AUTO_INCREMENT COMMENT 'ID',
  `role_id` bigint(20) unsigned NOT NULL DEFAULT 0 COMMENT '角色ID',
  `asset` varchar(32) NOT NULL DEFAULT '0' COMMENT '资产',
  `asset_id` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '资产ID',
  `number` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '数量',
  `operation` varchar(32) NOT NULL DEFAULT '' COMMENT '操作',
  `from` varchar(32) NOT NULL DEFAULT '' COMMENT '来源',
  `time` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '时间',
  PRIMARY KEY (`id`) USING BTREE,
  KEY `role_id` (`role_id`) USING BTREE,
  KEY `asset` (`asset`) USING BTREE,
  KEY `asset_id` (`asset_id`) USING BTREE,
  KEY `from` (`from`) USING BTREE,
  KEY `time` (`time`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci ROW_FORMAT=DYNAMIC COMMENT='资产产出日志表';
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `asset_produce_log`
--

LOCK TABLES `asset_produce_log` WRITE;
/*!40000 ALTER TABLE `asset_produce_log` DISABLE KEYS */;
/*!40000 ALTER TABLE `asset_produce_log` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `attribute_data`
--

DROP TABLE IF EXISTS `attribute_data`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `attribute_data` (
  `attribute_id` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '属性ID',
  `attribute` varchar(255) NOT NULL DEFAULT '' COMMENT '属性',
  `type` varchar(255) NOT NULL DEFAULT '' COMMENT '类型(固定值/万分比)',
  `merge` varchar(255) NOT NULL DEFAULT '' COMMENT '合并计算公式',
  `effect` varchar(255) NOT NULL DEFAULT '' COMMENT '效果',
  `name` char(255) NOT NULL DEFAULT '' COMMENT '名字',
  `description` char(255) NOT NULL DEFAULT '' COMMENT '描述',
  PRIMARY KEY (`attribute_id`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci ROW_FORMAT=DYNAMIC COMMENT='属性配置表';
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `attribute_data`
--

LOCK TABLES `attribute_data` WRITE;
/*!40000 ALTER TABLE `attribute_data` DISABLE KEYS */;
INSERT INTO `attribute_data` VALUES
(2,'hp','fix','','','血量','血量'),
(3,'attack','fix','attack','','攻击','攻击'),
(4,'defense','fix','defense','','防御','防御'),
(5,'health','fix','health','','生命','生命'),
(6,'hit','fix','hit','','命中','命中'),
(7,'duck','fix','duck','','闪避','闪避'),
(8,'freeze','fix','','cannot_be_attack','冰冻','冰冻'),
(9,'destroy','fix','','','毁灭','毁灭'),
(10,'vertigo','fix','','','眩晕','眩晕');
/*!40000 ALTER TABLE `attribute_data` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `auction`
--

DROP TABLE IF EXISTS `auction`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `auction` (
  `auction_no` bigint(20) unsigned NOT NULL AUTO_INCREMENT COMMENT '拍品编号',
  `auction_id` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '拍品ID',
  `number` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '拍品数量',
  `type` tinyint(3) unsigned NOT NULL DEFAULT 0 COMMENT '拍卖类型(1:全服/2:公会)',
  `bid_type` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '竞拍类型(1:竞价/2:一口价)',
  `start_time` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '开始时间',
  `end_time` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '结束时间',
  `from` varchar(32) NOT NULL DEFAULT '' COMMENT '物品来源',
  `bid_number` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '加价次数',
  `now_price` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '当前价格',
  `next_price` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '下次出价的价格',
  `seller_list` varchar(0) GENERATED ALWAYS AS ('') VIRTUAL COMMENT '卖家列表',
  `bidder_list` varchar(0) GENERATED ALWAYS AS ('') VIRTUAL COMMENT '买家列表',
  `guild_id` bigint(20) unsigned NOT NULL DEFAULT 0 COMMENT '公会ID',
  `timer` tinyint(3) unsigned GENERATED ALWAYS AS (0) VIRTUAL COMMENT '定时器',
  `flag` tinyint(3) unsigned GENERATED ALWAYS AS (0) VIRTUAL COMMENT '标识(flag)',
  PRIMARY KEY (`auction_no`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci ROW_FORMAT=DYNAMIC COMMENT='拍卖信息表';
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `auction`
--

LOCK TABLES `auction` WRITE;
/*!40000 ALTER TABLE `auction` DISABLE KEYS */;
/*!40000 ALTER TABLE `auction` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `auction_data`
--

DROP TABLE IF EXISTS `auction_data`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `auction_data` (
  `auction_id` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '拍品ID',
  `bid_type` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '竞拍类型(1:竞价/2:一口价)',
  `begin_price` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '底价',
  `add_price` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '每次加价',
  `tax` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '税收',
  `show_time` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '预览时间',
  `auction_time` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '拍卖时间',
  `critical_time` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '临界时间(出价加时的临界时间)',
  `overtime` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '延迟时间(出价加时的时间)',
  PRIMARY KEY (`auction_id`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci ROW_FORMAT=DYNAMIC COMMENT='拍卖配置表';
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `auction_data`
--

LOCK TABLES `auction_data` WRITE;
/*!40000 ALTER TABLE `auction_data` DISABLE KEYS */;
INSERT INTO `auction_data` VALUES
(1,1,1,1,0,0,0,0,0);
/*!40000 ALTER TABLE `auction_data` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `auction_log`
--

DROP TABLE IF EXISTS `auction_log`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `auction_log` (
  `id` bigint(20) unsigned NOT NULL AUTO_INCREMENT COMMENT 'ID',
  `auction_id` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '拍品ID',
  `number` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '拍品数量',
  `bid_number` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '加价次数',
  `price` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '成交价',
  `role_id` bigint(20) unsigned NOT NULL DEFAULT 0 COMMENT '获得者ID',
  `role_name` char(16) NOT NULL DEFAULT '' COMMENT '获得者名字',
  `server_id` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '获得者服务器ID',
  `time` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '时间',
  PRIMARY KEY (`id`) USING BTREE,
  KEY `role_id` (`role_id`) USING BTREE,
  KEY `time` (`time`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci ROW_FORMAT=DYNAMIC COMMENT='拍卖日志表';
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `auction_log`
--

LOCK TABLES `auction_log` WRITE;
/*!40000 ALTER TABLE `auction_log` DISABLE KEYS */;
/*!40000 ALTER TABLE `auction_log` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `auction_role`
--

DROP TABLE IF EXISTS `auction_role`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `auction_role` (
  `auction_no` bigint(20) unsigned NOT NULL DEFAULT 0 COMMENT '拍品编号(delete_by_no)',
  `server_id` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '服务器ID',
  `role_id` bigint(20) unsigned NOT NULL DEFAULT 0 COMMENT '出价者ID',
  `role_name` char(16) NOT NULL DEFAULT '' COMMENT '出价者名字',
  `guild_id` bigint(20) unsigned NOT NULL DEFAULT 0 COMMENT '出价者公会ID',
  `guild_name` char(16) NOT NULL DEFAULT '' COMMENT '出价者公会名字',
  `type` tinyint(3) unsigned NOT NULL DEFAULT 0 COMMENT '角色类型(1:卖家/2:买家)',
  `price` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '当前价格',
  `time` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '时间',
  `flag` tinyint(3) unsigned GENERATED ALWAYS AS (0) VIRTUAL COMMENT '标识(flag)',
  PRIMARY KEY (`auction_no`,`role_id`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci ROW_FORMAT=DYNAMIC COMMENT='拍卖角色表';
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `auction_role`
--

LOCK TABLES `auction_role` WRITE;
/*!40000 ALTER TABLE `auction_role` DISABLE KEYS */;
/*!40000 ALTER TABLE `auction_role` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `bubble`
--

DROP TABLE IF EXISTS `bubble`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `bubble` (
  `role_id` bigint(20) unsigned NOT NULL DEFAULT 0 COMMENT '角色ID(select_by_role_id)',
  `bubble_id` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '气泡ID',
  `type` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '类型',
  `expire_time` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '过期时间',
  `flag` tinyint(3) unsigned GENERATED ALWAYS AS (0) VIRTUAL COMMENT '标识(flag)',
  PRIMARY KEY (`role_id`,`bubble_id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb3 COLLATE=utf8mb3_general_ci COMMENT='聊天气泡数据';
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `bubble`
--

LOCK TABLES `bubble` WRITE;
/*!40000 ALTER TABLE `bubble` DISABLE KEYS */;
/*!40000 ALTER TABLE `bubble` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `bubble_data`
--

DROP TABLE IF EXISTS `bubble_data`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `bubble_data` (
  `bubble_id` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '气泡ID',
  `type` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '类型',
  `tag` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '标签',
  `expire_time` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '过期时间',
  `name` char(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '气泡名称',
  `description` char(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '气泡描述',
  PRIMARY KEY (`bubble_id`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb3 COLLATE=utf8mb3_general_ci COMMENT='聊天气泡配置';
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `bubble_data`
--

LOCK TABLES `bubble_data` WRITE;
/*!40000 ALTER TABLE `bubble_data` DISABLE KEYS */;
INSERT INTO `bubble_data` VALUES
(101,1,0,0,'VIP1可获得','小试牛刀'),
(102,1,0,0,'VIP2可获得','有钱任性'),
(103,1,0,0,'VIP3可获得','一掷千金'),
(104,1,0,0,'VIP4可获得','腰缠万贯'),
(105,1,0,0,'VIP5可获得','挥金如土'),
(106,1,0,0,'VIP6可获得','富甲天下'),
(107,1,0,0,'VIP7可获得','富可敌国'),
(108,1,0,0,'VIP8可获得','人生巅峰'),
(109,1,0,0,'VIP9可获得','至尊王者'),
(110,1,0,0,'VIP0可获得','高手对决'),
(201,2,0,0,'开服冲榜活动获取','武艺超群'),
(202,2,0,0,'开服冲榜活动获取','出神入化'),
(203,2,0,0,'开服冲榜活动获取','仙武主宰'),
(204,2,0,0,'开服冲榜活动获取','锻造大师'),
(205,2,0,0,'开服冲榜活动获取','黑暗主宰'),
(206,2,0,0,'开服冲榜活动获取','聚魂先锋'),
(207,2,0,0,'开服冲榜活动获取','全职高手'),
(208,2,0,0,'开服冲榜活动获取','人中之龙'),
(209,2,0,0,'开服冲榜活动获取','勇者无畏'),
(210,2,0,0,'开服冲榜活动获取','称霸天下'),
(10010,3,0,0,'充值获取','归隐山林');
/*!40000 ALTER TABLE `bubble_data` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `bubble_log`
--

DROP TABLE IF EXISTS `bubble_log`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `bubble_log` (
  `id` bigint(20) unsigned NOT NULL AUTO_INCREMENT COMMENT 'ID',
  `role_id` bigint(20) unsigned NOT NULL DEFAULT 0 COMMENT '角色ID',
  `bubble_id` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '气泡ID',
  `from` varchar(32) NOT NULL DEFAULT '' COMMENT '来源',
  `time` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '时间',
  PRIMARY KEY (`id`) USING BTREE,
  KEY `role_id` (`role_id`) USING BTREE,
  KEY `time` (`time`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci ROW_FORMAT=DYNAMIC COMMENT='气泡日志表';
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `bubble_log`
--

LOCK TABLES `bubble_log` WRITE;
/*!40000 ALTER TABLE `bubble_log` DISABLE KEYS */;
/*!40000 ALTER TABLE `bubble_log` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `buff`
--

DROP TABLE IF EXISTS `buff`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `buff` (
  `role_id` bigint(20) unsigned NOT NULL DEFAULT 0 COMMENT '角色ID(select_by_role_id)',
  `buff_id` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '状态增益ID',
  `expire_time` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '过期时间',
  `overlap` int(10) unsigned NOT NULL DEFAULT 1 COMMENT '叠加数',
  `flag` tinyint(3) unsigned GENERATED ALWAYS AS (0) VIRTUAL COMMENT '标识(flag)',
  PRIMARY KEY (`role_id`,`buff_id`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci ROW_FORMAT=DYNAMIC COMMENT='角色buff表';
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `buff`
--

LOCK TABLES `buff` WRITE;
/*!40000 ALTER TABLE `buff` DISABLE KEYS */;
/*!40000 ALTER TABLE `buff` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `buff_data`
--

DROP TABLE IF EXISTS `buff_data`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `buff_data` (
  `buff_id` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '增益状态(Buff)ID',
  `type` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '类型',
  `expire_time` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '过期时间',
  `attribute` varchar(255) NOT NULL DEFAULT '' COMMENT '属性',
  `effect` varchar(255) NOT NULL DEFAULT '' COMMENT '效果',
  `is_temporary` enum('false','true') NOT NULL COMMENT '是否临时的(切地图失效)(validate(boolean))',
  `overlap_type` tinyint(3) unsigned NOT NULL DEFAULT 0 COMMENT '叠加类型(validate(overlap_type))',
  `name` char(255) NOT NULL DEFAULT '' COMMENT '名字',
  `description` char(255) NOT NULL DEFAULT '' COMMENT '描述',
  PRIMARY KEY (`buff_id`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci ROW_FORMAT=DYNAMIC COMMENT='buff配置表';
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `buff_data`
--

LOCK TABLES `buff_data` WRITE;
/*!40000 ALTER TABLE `buff_data` DISABLE KEYS */;
INSERT INTO `buff_data` VALUES
(1,1,1800,'[]','[9]','false',3,'铜币',''),
(2,1,3600,'[]','[10]','false',3,'经验',''),
(3,2,0,'[{3,100}]','[]','false',2,'攻击',''),
(4,2,0,'[{4,100}]','[]','false',2,'防御',''),
(5,2,60,'[]','[3]','false',1,'眩晕',''),
(6,3,60,'[]','[5]','false',0,'扣血','');
/*!40000 ALTER TABLE `buff_data` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `charge`
--

DROP TABLE IF EXISTS `charge`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `charge` (
  `charge_no` bigint(20) unsigned NOT NULL AUTO_INCREMENT COMMENT '充值编号',
  `charge_id` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '充值ID',
  `order_id` char(255) NOT NULL DEFAULT '' COMMENT '订单ID',
  `channel` char(255) NOT NULL DEFAULT '' COMMENT '渠道',
  `role_id` bigint(20) unsigned NOT NULL DEFAULT 0 COMMENT '玩家ID',
  `role_name` char(16) NOT NULL DEFAULT '' COMMENT '玩家名称',
  `money` decimal(10,2) unsigned NOT NULL DEFAULT 0.00 COMMENT '充值金额',
  `status` tinyint(3) unsigned NOT NULL DEFAULT 0 COMMENT '状态(update_status)',
  `time` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '订单时间',
  PRIMARY KEY (`charge_no`) USING BTREE,
  UNIQUE KEY `order_id` (`order_id`) USING BTREE,
  KEY `channel` (`channel`) USING BTREE,
  KEY `time` (`time`) USING BTREE,
  KEY `role_id` (`role_id`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci ROW_FORMAT=DYNAMIC COMMENT='角色充值订单表';
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `charge`
--

LOCK TABLES `charge` WRITE;
/*!40000 ALTER TABLE `charge` DISABLE KEYS */;
/*!40000 ALTER TABLE `charge` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `charge_data`
--

DROP TABLE IF EXISTS `charge_data`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `charge_data` (
  `charge_id` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '充值ID',
  `type` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '类型(普通充值:0/购买月卡:1)',
  `limit` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '限制数量',
  `exp` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '经验',
  `original_price` decimal(10,2) unsigned NOT NULL DEFAULT 0.00 COMMENT '原价',
  `now_price` decimal(10,2) unsigned NOT NULL DEFAULT 0.00 COMMENT '现价',
  `gold` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '金币',
  `gift_gold` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '赠送金币',
  `begin_open_days` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '结束时间，跟开服相关，填天数',
  `end_open_days` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '结束时间，跟开服相关，填天数',
  `sort` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '排序',
  `icon` char(255) NOT NULL DEFAULT '' COMMENT '图片',
  `name` char(255) NOT NULL DEFAULT '' COMMENT '名字',
  `description` char(255) NOT NULL DEFAULT '' COMMENT '描述',
  PRIMARY KEY (`charge_id`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci ROW_FORMAT=DYNAMIC COMMENT='充值配置表';
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `charge_data`
--

LOCK TABLES `charge_data` WRITE;
/*!40000 ALTER TABLE `charge_data` DISABLE KEYS */;
INSERT INTO `charge_data` VALUES
(1,3,1,6,6.00,6.00,6,0,1,9999,1,'0','至尊神兵宝箱',''),
(2,1,1,18,18.00,18.00,18,5,1,9999,2,'1','元宝',''),
(3,1,1,68,68.00,68.00,68,40,1,9999,3,'2','元宝',''),
(4,1,1,128,128.00,128.00,128,90,1,9999,4,'3','元宝',''),
(5,1,1,268,268.00,268.00,268,190,1,9999,5,'4','元宝',''),
(6,1,1,588,588.00,588.00,588,330,1,9999,6,'5','元宝',''),
(7,1,1,688,688.00,688.00,688,590,1,9999,7,'6','元宝',''),
(8,1,1,888,888.00,888.00,888,1300,1,9999,8,'7','元宝',''),
(9,2,1,1288,1288.00,1288.00,1288,0,1,9999,0,'','周卡',''),
(10,6,1,8888,8888.00,8888.00,8888,0,1,9999,0,'','月卡','');
/*!40000 ALTER TABLE `charge_data` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `classes_data`
--

DROP TABLE IF EXISTS `classes_data`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `classes_data` (
  `classes` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '物品配置ID',
  `name` char(255) NOT NULL DEFAULT '' COMMENT '资产类型',
  PRIMARY KEY (`classes`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci ROW_FORMAT=DYNAMIC COMMENT='职业配置表';
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `classes_data`
--

LOCK TABLES `classes_data` WRITE;
/*!40000 ALTER TABLE `classes_data` DISABLE KEYS */;
INSERT INTO `classes_data` VALUES
(1,'七杀'),
(2,'天师'),
(3,'飞羽'),
(4,'御灵'),
(5,'妙音'),
(6,'星术');
/*!40000 ALTER TABLE `classes_data` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `count`
--

DROP TABLE IF EXISTS `count`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `count` (
  `role_id` bigint(20) unsigned NOT NULL DEFAULT 0 COMMENT '角色ID(select_by_role_id)',
  `type` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '计数类型',
  `today_number` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '今天数量',
  `week_number` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '今周数量',
  `total_number` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '总数',
  `time` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '时间',
  `flag` tinyint(3) unsigned GENERATED ALWAYS AS (0) VIRTUAL COMMENT '标识(flag)',
  PRIMARY KEY (`role_id`,`type`) USING BTREE,
  KEY `type` (`type`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci ROW_FORMAT=DYNAMIC COMMENT='角色计数表';
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `count`
--

LOCK TABLES `count` WRITE;
/*!40000 ALTER TABLE `count` DISABLE KEYS */;
/*!40000 ALTER TABLE `count` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `daily`
--

DROP TABLE IF EXISTS `daily`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `daily` (
  `role_id` bigint(20) unsigned NOT NULL DEFAULT 0 COMMENT '角色ID(select_by_role_id)',
  `daily_id` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '日常ID',
  `is_award` tinyint(3) unsigned NOT NULL DEFAULT 0 COMMENT '是否领取奖励',
  `flag` tinyint(3) unsigned GENERATED ALWAYS AS (0) VIRTUAL COMMENT '标识(flag)',
  PRIMARY KEY (`role_id`,`daily_id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci COMMENT='角色日常表';
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `daily`
--

LOCK TABLES `daily` WRITE;
/*!40000 ALTER TABLE `daily` DISABLE KEYS */;
/*!40000 ALTER TABLE `daily` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `daily_active`
--

DROP TABLE IF EXISTS `daily_active`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `daily_active` (
  `role_id` bigint(20) unsigned NOT NULL DEFAULT 0 COMMENT '角色ID',
  `stage_id` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '奖励阶段ID',
  `score` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '活跃度',
  PRIMARY KEY (`role_id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci COMMENT='角色日常活跃表';
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `daily_active`
--

LOCK TABLES `daily_active` WRITE;
/*!40000 ALTER TABLE `daily_active` DISABLE KEYS */;
/*!40000 ALTER TABLE `daily_active` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `daily_active_data`
--

DROP TABLE IF EXISTS `daily_active_data`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `daily_active_data` (
  `stage_id` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '阶段ID',
  `pre_id` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '前一个阶段ID',
  `next_id` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '下一个阶段ID',
  `score` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '所需活跃度',
  `award` varchar(255) NOT NULL DEFAULT '' COMMENT '奖励',
  PRIMARY KEY (`stage_id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci COMMENT='日常活跃奖励配置';
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `daily_active_data`
--

LOCK TABLES `daily_active_data` WRITE;
/*!40000 ALTER TABLE `daily_active_data` DISABLE KEYS */;
INSERT INTO `daily_active_data` VALUES
(1,0,2,30,'[{1,1000}]'),
(2,1,3,50,'[{1,1000}]'),
(3,2,4,80,'[{1,1000}]'),
(4,3,5,100,'[{1,1000}]'),
(5,4,6,120,'[{1,1000}]'),
(6,5,0,150,'[{1,1000}]');
/*!40000 ALTER TABLE `daily_active_data` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `daily_data`
--

DROP TABLE IF EXISTS `daily_data`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `daily_data` (
  `daily_id` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '日常ID',
  `type` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '类型',
  `count_type` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '统计类型',
  `number` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '目标数量',
  `score` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '活跃度',
  `award` varchar(255) NOT NULL DEFAULT '' COMMENT '奖励',
  PRIMARY KEY (`daily_id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci COMMENT='日常配置表';
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `daily_data`
--

LOCK TABLES `daily_data` WRITE;
/*!40000 ALTER TABLE `daily_data` DISABLE KEYS */;
INSERT INTO `daily_data` VALUES
(1,1,1,1,1,'[{1,1000}]'),
(2,1,2,2,2,'[{1,1000}]'),
(3,1,3,3,3,'[{1,1000}]'),
(4,1,4,4,4,'[{1,1000}]'),
(5,1,5,5,5,'[{1,1000}]'),
(6,1,6,6,6,'[{1,1000}]'),
(7,1,7,7,7,'[{1,1000}]'),
(8,1,8,8,8,'[{1,1000}]'),
(9,1,9,9,9,'[{1,1000}]');
/*!40000 ALTER TABLE `daily_data` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `dungeon`
--

DROP TABLE IF EXISTS `dungeon`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `dungeon` (
  `role_id` bigint(20) unsigned NOT NULL DEFAULT 0 COMMENT '玩家ID(select_by_role_id)',
  `dungeon_id` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '副本ID',
  `type` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '类型',
  `today_number` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '今天次数',
  `total_number` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '历史总次数',
  `is_pass` tinyint(3) unsigned NOT NULL DEFAULT 0 COMMENT '是否通关',
  `flag` tinyint(3) unsigned GENERATED ALWAYS AS (0) VIRTUAL COMMENT '标识(flag)',
  PRIMARY KEY (`role_id`,`type`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci ROW_FORMAT=DYNAMIC COMMENT='角色副本表';
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `dungeon`
--

LOCK TABLES `dungeon` WRITE;
/*!40000 ALTER TABLE `dungeon` DISABLE KEYS */;
/*!40000 ALTER TABLE `dungeon` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `dungeon_data`
--

DROP TABLE IF EXISTS `dungeon_data`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `dungeon_data` (
  `dungeon_id` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '副本ID',
  `type` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '类型(validate(dungeon_type))',
  `condition` varchar(255) NOT NULL DEFAULT '' COMMENT '条件',
  `cost` varchar(255) NOT NULL DEFAULT '' COMMENT '消耗',
  `day_number` varchar(255) NOT NULL DEFAULT '' COMMENT '每日次数',
  `buy_number` varchar(255) NOT NULL DEFAULT '' COMMENT '购买次数',
  `map_id` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '地图ID',
  `monsters` varchar(255) NOT NULL DEFAULT '' COMMENT '怪物',
  `boss` varchar(255) NOT NULL DEFAULT '' COMMENT 'Boss',
  `time` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '时间',
  `award` varchar(255) NOT NULL DEFAULT '' COMMENT '奖励',
  `name` char(255) NOT NULL DEFAULT '' COMMENT '名字',
  `description` char(255) NOT NULL DEFAULT '' COMMENT '描述',
  PRIMARY KEY (`dungeon_id`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci ROW_FORMAT=DYNAMIC COMMENT='副本配置表';
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `dungeon_data`
--

LOCK TABLES `dungeon_data` WRITE;
/*!40000 ALTER TABLE `dungeon_data` DISABLE KEYS */;
INSERT INTO `dungeon_data` VALUES
(1,1,'[{level,10}]','[{100005,100}]','[{0,1},{1,2},{2,3},{3,4},{4,5},{5,6}]','[{0,1,100},{1,2,200},{2,3,300},{3,4,400},{4,5,500},{5,6,600}]',110001,'[{1,10},{1,20},{1,10},{1,20},{2,1}]','[]',600,'[{100005,100}]','经验副本','经验副本'),
(2,1,'[{level,20}]','[{100005,200}]','[{0,1},{1,2},{2,3},{3,4},{4,5},{5,6}]','[{0,1,100},{1,2,200},{2,3,300},{3,4,400},{4,5,500},{5,6,600}]',110002,'[{1,10},{1,20},{1,10},{1,20},{2,1}]','[]',600,'[{100005,200}]','经验副本','经验副本'),
(3,1,'[{level,30}]','[{100005,300}]','[{0,1},{1,2},{2,3},{3,4},{4,5},{5,6}]','[{0,1,100},{1,2,200},{2,3,300},{3,4,400},{4,5,500},{5,6,600}]',110003,'[{1,10},{1,20},{1,10},{1,20},{2,1}]','[]',600,'[{100005,300}]','经验副本','经验副本'),
(4,2,'[{level,10}]','[{100005,100}]','[{0,1},{1,2},{2,3},{3,4},{4,5},{5,6}]','[{0,1,100},{1,2,200},{2,3,300},{3,4,400},{4,5,500},{5,6,600}]',120001,'[{1,10},{1,20},{1,10},{1,20},{2,1}]','[]',600,'[{100003,100}]','铜币副本','铜币副本'),
(5,2,'[{level,20}]','[{100005,200}]','[{0,1},{1,2},{2,3},{3,4},{4,5},{5,6}]','[{0,1,100},{1,2,200},{2,3,300},{3,4,400},{4,5,500},{5,6,600}]',120002,'[{1,10},{1,20},{1,10},{1,20},{2,1}]','[]',600,'[{100003,200}]','铜币副本','铜币副本'),
(6,2,'[{level,30}]','[{100005,300}]','[{0,1},{1,2},{2,3},{3,4},{4,5},{5,6}]','[{0,1,100},{1,2,200},{2,3,300},{3,4,400},{4,5,500},{5,6,600}]',120003,'[{1,10},{1,20},{1,10},{1,20},{2,1}]','[]',600,'[{100003,300}]','铜币副本','铜币副本');
/*!40000 ALTER TABLE `dungeon_data` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `effect_data`
--

DROP TABLE IF EXISTS `effect_data`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `effect_data` (
  `effect_id` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '效果ID',
  `type` enum('active','buff','passive') NOT NULL COMMENT '类型(validate(effect_type))',
  `scope` enum('battle','user') NOT NULL COMMENT '作用范围(validate(effect_scope))',
  `condition` varchar(255) NOT NULL DEFAULT '' COMMENT '条件',
  `ratio` varchar(255) NOT NULL DEFAULT '' COMMENT '概率',
  `restrict` varchar(255) NOT NULL DEFAULT '' COMMENT '约束',
  `operation` enum('add','clear','reduce','set') NOT NULL COMMENT '操作(validate(effect_operation))',
  `object` enum('mate','rival','self') NOT NULL COMMENT '作用对象(validate(effect_object))',
  `attribute` enum('asset','attribute','buff','hurt','skill') NOT NULL COMMENT '操作属性(validate(effect_attribute))',
  `field` enum('none','attack','copper','defense','destroy','duck','exp','fc','freeze','health','hit','hp','vertigo') NOT NULL COMMENT '操作属性字段(validate(effect_field))',
  `value` varchar(255) NOT NULL DEFAULT '' COMMENT '属性值',
  `time` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '效果时间',
  `extra` varchar(255) NOT NULL DEFAULT '' COMMENT '额外',
  `description` varchar(255) NOT NULL DEFAULT '' COMMENT '描述',
  PRIMARY KEY (`effect_id`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci ROW_FORMAT=DYNAMIC COMMENT='作用效果配置表';
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `effect_data`
--

LOCK TABLES `effect_data` WRITE;
/*!40000 ALTER TABLE `effect_data` DISABLE KEYS */;
INSERT INTO `effect_data` VALUES
(1,'active','battle','[]','10000','_','add','self','hurt','none','Hurt',0,'','伤害'),
(2,'active','battle','[]','10000','_','add','self','hurt','none','Hurt * 1.5',0,'','增加50%伤害'),
(3,'active','battle','[]','10000','_','add','self','attribute','vertigo','1',0,'','眩晕'),
(4,'active','battle','[]','10000','_','reduce','self','attribute','vertigo','0',0,'','清除眩晕'),
(5,'active','battle','[]','10000','_','reduce','self','attribute','hp','Rival.Attribute.health * 0.01',3600,'','每秒扣血，总血量百分之1'),
(6,'active','battle','[]','10000','_','add','mate','attribute','attack','Mate.Attribute.attack * 1.5',3,'','增加队友攻击150%'),
(7,'active','battle','[]','10000','_','add','mate','attribute','defense','Mate.Attribute.defense * 1.5',3,'','增加队友防御150%'),
(8,'active','battle','[]','10000','_','add','self','buff','none','[1]',0,'','添加Buff'),
(9,'active','user','[]','10000','_','add','self','asset','copper','1.5',0,'','增加150%铜币'),
(10,'active','user','[]','10000','_','add','self','asset','exp','2',0,'','增加200%经验');
/*!40000 ALTER TABLE `effect_data` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `fashion`
--

DROP TABLE IF EXISTS `fashion`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `fashion` (
  `role_id` bigint(20) unsigned NOT NULL DEFAULT 0 COMMENT '玩家ID(select_by_role_id)(update_role_id)',
  `fashion_id` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '时装ID(select_by_fashion_id)',
  `type` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '类型',
  `expire_time` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '过期时间',
  `flag` tinyint(3) unsigned GENERATED ALWAYS AS (0) VIRTUAL COMMENT '标识(flag)',
  PRIMARY KEY (`role_id`,`fashion_id`),
  KEY `fashion_id` (`fashion_id`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb3 COLLATE=utf8mb3_general_ci COMMENT='玩家时装表';
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `fashion`
--

LOCK TABLES `fashion` WRITE;
/*!40000 ALTER TABLE `fashion` DISABLE KEYS */;
INSERT INTO `fashion` VALUES
(1,1,0,0,0),
(2,2,0,0,0),
(3,3,0,0,0);
/*!40000 ALTER TABLE `fashion` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `fashion_data`
--

DROP TABLE IF EXISTS `fashion_data`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `fashion_data` (
  `fashion_id` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '时装ID',
  `type` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '时装类型',
  `is_unique` enum('false','true') CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci NOT NULL COMMENT '是否全局唯一(validate(boolean))',
  `expire_time` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '过期时间',
  `attribute` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '属性',
  `name` char(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '时装名字',
  `description` char(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '时装描述',
  PRIMARY KEY (`fashion_id`),
  KEY `fashion_type` (`type`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb3 COLLATE=utf8mb3_general_ci COMMENT='时装配置表';
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `fashion_data`
--

LOCK TABLES `fashion_data` WRITE;
/*!40000 ALTER TABLE `fashion_data` DISABLE KEYS */;
INSERT INTO `fashion_data` VALUES
(101,1,'false',0,'[{3,30},{4,40}]','小试牛刀','VIP1可获得'),
(102,1,'false',0,'[{3,30},{4,40}]','有钱任性','VIP2可获得'),
(103,1,'false',0,'[{3,30},{4,40}]','一掷千金','VIP3可获得'),
(104,1,'false',0,'[{3,30},{4,40}]','腰缠万贯','VIP4可获得'),
(105,1,'false',0,'[{3,30},{4,40}]','挥金如土','VIP5可获得'),
(106,1,'false',0,'[{3,30},{4,40}]','富甲天下','VIP6可获得'),
(107,1,'false',0,'[{3,30},{4,40}]','富可敌国','VIP7可获得'),
(108,1,'false',0,'[{3,30},{4,40}]','人生巅峰','VIP8可获得'),
(109,1,'false',0,'[{3,30},{4,40}]','至尊王者','VIP9可获得'),
(110,1,'false',0,'[{3,30},{4,40}]','高手对决','VIP0可获得'),
(201,2,'false',0,'[{6,60},{7,70}]','武艺超群','开服冲榜活动获取'),
(202,2,'false',0,'[{6,60},{7,70}]','出神入化','开服冲榜活动获取'),
(203,2,'false',0,'[{6,60},{7,70}]','仙武主宰','开服冲榜活动获取'),
(204,2,'false',0,'[{6,60},{7,70}]','锻造大师','开服冲榜活动获取'),
(205,2,'false',0,'[{6,60},{7,70}]','黑暗主宰','开服冲榜活动获取'),
(206,2,'false',0,'[{6,60},{7,70}]','聚魂先锋','开服冲榜活动获取'),
(207,2,'false',0,'[{6,60},{7,70}]','全职高手','开服冲榜活动获取'),
(208,2,'false',0,'[{6,60},{7,70}]','人中之龙','开服冲榜活动获取'),
(209,2,'false',0,'[{6,60},{7,70}]','勇者无畏','开服冲榜活动获取'),
(210,2,'false',0,'[{6,60},{7,70}]','称霸天下','开服冲榜活动获取'),
(10010,3,'true',604800,'[{5,50}]','归隐山林','充值获取');
/*!40000 ALTER TABLE `fashion_data` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `fashion_log`
--

DROP TABLE IF EXISTS `fashion_log`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `fashion_log` (
  `id` bigint(20) unsigned NOT NULL AUTO_INCREMENT COMMENT 'ID',
  `role_id` bigint(20) unsigned NOT NULL DEFAULT 0 COMMENT '角色ID',
  `fashion_id` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '时装ID',
  `from` varchar(32) NOT NULL DEFAULT '' COMMENT '来源',
  `time` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '时间',
  PRIMARY KEY (`id`) USING BTREE,
  KEY `role_id` (`role_id`) USING BTREE,
  KEY `time` (`time`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci ROW_FORMAT=DYNAMIC COMMENT='时装日志表';
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `fashion_log`
--

LOCK TABLES `fashion_log` WRITE;
/*!40000 ALTER TABLE `fashion_log` DISABLE KEYS */;
/*!40000 ALTER TABLE `fashion_log` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `friend`
--

DROP TABLE IF EXISTS `friend`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `friend` (
  `role_id` bigint(20) unsigned NOT NULL DEFAULT 0 COMMENT '用户ID(select_by_role_id)',
  `friend_role_id` bigint(20) unsigned NOT NULL DEFAULT 0 COMMENT '好友角色ID(join_on(`role`.`role_id`)/join_on(`vip`.`role_id`))',
  `friend_name` char(0) GENERATED ALWAYS AS ('') VIRTUAL COMMENT '好友名字(join(`role`.`role_name`))',
  `sex` tinyint(3) unsigned GENERATED ALWAYS AS (0) VIRTUAL COMMENT '好友性别(join(`role`.`sex`))',
  `avatar` tinyint(3) unsigned GENERATED ALWAYS AS (0) VIRTUAL COMMENT '头像(join(`role`.`avatar`))',
  `classes` tinyint(3) unsigned GENERATED ALWAYS AS (0) VIRTUAL COMMENT '好友职业(join(`role`.`classes`))',
  `level` int(10) unsigned GENERATED ALWAYS AS (0) VIRTUAL COMMENT '等级(join(`role`.`level`))',
  `vip_level` int(10) unsigned GENERATED ALWAYS AS (0) VIRTUAL COMMENT 'VIP等级(join(`vip`.`vip_level`))',
  `is_online` tinyint(3) unsigned GENERATED ALWAYS AS (0) VIRTUAL COMMENT '好友在线状态(join(`role`.`is_online`))',
  `relation` tinyint(3) unsigned NOT NULL DEFAULT 0 COMMENT '友好状态(update_relation)',
  `time` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '时间',
  `flag` tinyint(3) unsigned GENERATED ALWAYS AS (0) VIRTUAL COMMENT '标识(flag)',
  PRIMARY KEY (`role_id`,`friend_role_id`) USING BTREE,
  KEY `friend_role_id` (`friend_role_id`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci ROW_FORMAT=DYNAMIC COMMENT='角色好友表';
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `friend`
--

LOCK TABLES `friend` WRITE;
/*!40000 ALTER TABLE `friend` DISABLE KEYS */;
/*!40000 ALTER TABLE `friend` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `guild`
--

DROP TABLE IF EXISTS `guild`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `guild` (
  `guild_id` bigint(20) unsigned NOT NULL AUTO_INCREMENT COMMENT '公会ID',
  `guild_name` char(16) NOT NULL DEFAULT '' COMMENT '名字(update_name)',
  `exp` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '经验',
  `wealth` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '财富',
  `level` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '等级',
  `create_time` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '时间',
  `notice` char(255) NOT NULL DEFAULT '' COMMENT '公告(update_notice)',
  `leader_role_id` bigint(20) unsigned NOT NULL DEFAULT 0 COMMENT '会长角色ID(join_on(`role`.`role_id`)/join_on(`vip`.`role_id`))',
  `leader_name` char(0) GENERATED ALWAYS AS ('') VIRTUAL COMMENT '会长名字(join(`role`.`role_name`))',
  `leader_sex` tinyint(3) unsigned GENERATED ALWAYS AS (0) VIRTUAL COMMENT '性别(join(`role`.`sex`))',
  `leader_avatar` tinyint(3) unsigned GENERATED ALWAYS AS (0) VIRTUAL COMMENT '头像(join(`role`.`avatar`))',
  `leader_class` tinyint(3) unsigned GENERATED ALWAYS AS (0) VIRTUAL COMMENT '会长名字(join(`role`.`classes`))',
  `leader_level` int(10) unsigned GENERATED ALWAYS AS (0) VIRTUAL COMMENT '职业(join(`role`.`level`))',
  `leader_vip_level` int(10) unsigned GENERATED ALWAYS AS (0) VIRTUAL COMMENT '会长名字(join(`vip`.`vip_level`))',
  `flag` tinyint(3) unsigned GENERATED ALWAYS AS (0) VIRTUAL COMMENT '标识(flag)',
  PRIMARY KEY (`guild_id`) USING BTREE,
  UNIQUE KEY `guild_name` (`guild_name`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci ROW_FORMAT=DYNAMIC COMMENT='公会表';
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `guild`
--

LOCK TABLES `guild` WRITE;
/*!40000 ALTER TABLE `guild` DISABLE KEYS */;
/*!40000 ALTER TABLE `guild` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `guild_apply`
--

DROP TABLE IF EXISTS `guild_apply`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `guild_apply` (
  `guild_id` bigint(20) unsigned NOT NULL DEFAULT 0 COMMENT '公会ID(join_on(`guild`.`guild_id`)/(delete_by_guild_id))',
  `role_id` bigint(20) unsigned NOT NULL DEFAULT 0 COMMENT '角色ID(join_on(`role`.`role_id`)/join_on(`vip`.`role_id`)/(delete_by_role_id))',
  `apply_time` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '时间',
  `guild_name` char(0) GENERATED ALWAYS AS ('') VIRTUAL COMMENT '帮派名(join(`guild`.`guild_name`))',
  `role_name` char(0) GENERATED ALWAYS AS ('') VIRTUAL COMMENT '角色名(join(`role`.`role_name`))',
  `sex` tinyint(3) unsigned GENERATED ALWAYS AS (0) VIRTUAL COMMENT '性别(join(`role`.`sex`))',
  `avatar` tinyint(3) unsigned GENERATED ALWAYS AS (0) VIRTUAL COMMENT '头像(join(`role`.`avatar`))',
  `classes` tinyint(3) unsigned GENERATED ALWAYS AS (0) VIRTUAL COMMENT '职业(join(`role`.`classes`))',
  `level` int(10) unsigned GENERATED ALWAYS AS (0) VIRTUAL COMMENT '职业(join(`role`.`level`))',
  `vip_level` int(10) unsigned GENERATED ALWAYS AS (0) VIRTUAL COMMENT 'VIP等级(join(`vip`.`vip_level`))',
  `flag` tinyint(3) unsigned GENERATED ALWAYS AS (0) VIRTUAL COMMENT '标识(flag)',
  PRIMARY KEY (`guild_id`,`role_id`) USING BTREE,
  KEY `role_id` (`role_id`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci ROW_FORMAT=DYNAMIC COMMENT='公会申请表';
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `guild_apply`
--

LOCK TABLES `guild_apply` WRITE;
/*!40000 ALTER TABLE `guild_apply` DISABLE KEYS */;
/*!40000 ALTER TABLE `guild_apply` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `guild_create_data`
--

DROP TABLE IF EXISTS `guild_create_data`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `guild_create_data` (
  `type` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '等级',
  `condition` varchar(255) NOT NULL DEFAULT '' COMMENT '条件',
  `cost` varchar(255) NOT NULL DEFAULT '' COMMENT '消耗',
  PRIMARY KEY (`type`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci COMMENT='公会创建配置表';
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `guild_create_data`
--

LOCK TABLES `guild_create_data` WRITE;
/*!40000 ALTER TABLE `guild_create_data` DISABLE KEYS */;
INSERT INTO `guild_create_data` VALUES
(0,'[]','[]'),
(1,'[{level, 1}, {vip, 1}]','[{100001, 1}]'),
(2,'[{level, 2}, {vip, 2}]','[{100001, 2}]'),
(3,'[{level, 3}, {vip, 3}]','[{100001, 3}]');
/*!40000 ALTER TABLE `guild_create_data` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `guild_level_data`
--

DROP TABLE IF EXISTS `guild_level_data`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `guild_level_data` (
  `level` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '等级',
  `exp` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '经验',
  PRIMARY KEY (`level`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci ROW_FORMAT=DYNAMIC COMMENT='公会等级配置表';
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `guild_level_data`
--

LOCK TABLES `guild_level_data` WRITE;
/*!40000 ALTER TABLE `guild_level_data` DISABLE KEYS */;
INSERT INTO `guild_level_data` VALUES
(0,100),
(1,200),
(2,300),
(3,400),
(4,500),
(5,600),
(6,700),
(7,800),
(8,900),
(9,1000);
/*!40000 ALTER TABLE `guild_level_data` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `guild_role`
--

DROP TABLE IF EXISTS `guild_role`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `guild_role` (
  `guild_id` bigint(20) unsigned NOT NULL DEFAULT 0 COMMENT '公会ID(join_on(`guild`.`guild_id`))',
  `role_id` bigint(20) unsigned NOT NULL DEFAULT 0 COMMENT '角色ID(join_on(`role`.`role_id`)/join_on(`vip`.`role_id`))',
  `job` tinyint(3) unsigned NOT NULL DEFAULT 0 COMMENT '职位',
  `wealth` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '财富',
  `join_time` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '加入时间',
  `leave_time` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '离开时间',
  `guild_name` char(0) GENERATED ALWAYS AS ('') VIRTUAL COMMENT '帮派名(join(`guild`.`guild_name`))',
  `role_name` char(0) GENERATED ALWAYS AS ('') VIRTUAL COMMENT '角色名(join(`role`.`role_name`))',
  `sex` tinyint(3) unsigned GENERATED ALWAYS AS (0) VIRTUAL COMMENT '性别(join(`role`.`sex`))',
  `avatar` tinyint(3) unsigned GENERATED ALWAYS AS (0) VIRTUAL COMMENT '头像(join(`role`.`avatar`))',
  `classes` tinyint(3) unsigned GENERATED ALWAYS AS (0) VIRTUAL COMMENT '职业(join(`role`.`classes`))',
  `level` int(10) unsigned GENERATED ALWAYS AS (0) VIRTUAL COMMENT '职业(join(`role`.`level`))',
  `vip_level` int(10) unsigned GENERATED ALWAYS AS (0) VIRTUAL COMMENT 'VIP等级(join(`vip`.`vip_level`))',
  `flag` tinyint(3) unsigned GENERATED ALWAYS AS (0) VIRTUAL COMMENT '标识(flag)',
  PRIMARY KEY (`role_id`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci ROW_FORMAT=DYNAMIC COMMENT='公会角色表';
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `guild_role`
--

LOCK TABLES `guild_role` WRITE;
/*!40000 ALTER TABLE `guild_role` DISABLE KEYS */;
/*!40000 ALTER TABLE `guild_role` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `increment`
--

DROP TABLE IF EXISTS `increment`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `increment` (
  `name` char(255) NOT NULL DEFAULT '' COMMENT '名字',
  `value` bigint(20) unsigned NOT NULL DEFAULT 0 COMMENT '数值',
  PRIMARY KEY (`name`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci ROW_FORMAT=DYNAMIC COMMENT='自增表';
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `increment`
--

LOCK TABLES `increment` WRITE;
/*!40000 ALTER TABLE `increment` DISABLE KEYS */;
/*!40000 ALTER TABLE `increment` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `item`
--

DROP TABLE IF EXISTS `item`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `item` (
  `item_no` bigint(20) unsigned NOT NULL AUTO_INCREMENT COMMENT '物品编号',
  `role_id` bigint(20) unsigned NOT NULL DEFAULT 0 COMMENT '角色ID(select_by_role_id)',
  `item_id` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '物品ID',
  `type` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '类型',
  `number` int(10) unsigned NOT NULL DEFAULT 1 COMMENT '数量',
  `expire_time` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '过期时间',
  `flag` tinyint(3) unsigned GENERATED ALWAYS AS (0) VIRTUAL COMMENT '标识(flag)',
  PRIMARY KEY (`item_no`) USING BTREE,
  KEY `role_id` (`role_id`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci ROW_FORMAT=DYNAMIC COMMENT='角色物品表';
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `item`
--

LOCK TABLES `item` WRITE;
/*!40000 ALTER TABLE `item` DISABLE KEYS */;
/*!40000 ALTER TABLE `item` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `item_consume_log`
--

DROP TABLE IF EXISTS `item_consume_log`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `item_consume_log` (
  `id` bigint(20) unsigned NOT NULL AUTO_INCREMENT COMMENT 'ID',
  `role_id` bigint(20) unsigned NOT NULL DEFAULT 0 COMMENT '角色ID',
  `item_id` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '物品ID',
  `operation` varchar(32) NOT NULL DEFAULT '' COMMENT '操作',
  `from` varchar(32) NOT NULL DEFAULT '' COMMENT '来源',
  `time` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '时间',
  PRIMARY KEY (`id`) USING BTREE,
  KEY `role_id` (`role_id`) USING BTREE,
  KEY `time` (`time`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci ROW_FORMAT=DYNAMIC COMMENT='物品消费日志表';
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `item_consume_log`
--

LOCK TABLES `item_consume_log` WRITE;
/*!40000 ALTER TABLE `item_consume_log` DISABLE KEYS */;
/*!40000 ALTER TABLE `item_consume_log` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `item_data`
--

DROP TABLE IF EXISTS `item_data`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `item_data` (
  `item_id` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '物品id',
  `type` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '类型(validate(item_type))',
  `overlap` int(10) unsigned NOT NULL DEFAULT 1 COMMENT '叠加数',
  `category` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '分类ID',
  `expire_time` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '过期时间',
  `use_number` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '使用数量(0:不能直接使用/1:一个/N:N个)',
  `use_effect` enum('none','gold','silver','copper','exp','coin') NOT NULL COMMENT '使用效果(validate(use_effect))',
  `use_value` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '使用效果数值',
  `name` char(255) NOT NULL DEFAULT '' COMMENT '名字',
  `icon` char(255) NOT NULL DEFAULT '' COMMENT '图标',
  `description` char(255) NOT NULL DEFAULT '' COMMENT '描述',
  PRIMARY KEY (`item_id`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci ROW_FORMAT=DYNAMIC COMMENT='物品配置表';
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `item_data`
--

LOCK TABLES `item_data` WRITE;
/*!40000 ALTER TABLE `item_data` DISABLE KEYS */;
INSERT INTO `item_data` VALUES
(1,1,1000,0,0,0,'none',0,'rust','file_type_rust.svg',''),
(2,1,100,0,0,0,'none',0,'erlang','file_type_erlang.svg',''),
(3,1,10,0,0,0,'none',0,'php','file_type_php.svg',''),
(4,2,1,0,0,0,'none',0,'lua','file_type_lua.svg',''),
(5,2,1,0,0,0,'none',0,'js','file_type_js.svg',''),
(6,2,1,0,0,0,'none',0,'html','file_type_html.svg',''),
(7,2,1,0,604800,100,'none',0,'css','file_type_css.svg',''),
(100001,10,1,0,0,0,'gold',0,'gold','file_type_gold.svg',''),
(100002,10,1,0,0,0,'silver',0,'silver','file_type_silver.svg',''),
(100003,10,1,0,0,0,'copper',0,'copper','file_type_copper.svg',''),
(100004,10,1,0,0,0,'exp',0,'exp','file_type_exp.svg',''),
(100005,10,1,0,0,0,'coin',0,'coin','file_type_coin.svg','');
/*!40000 ALTER TABLE `item_data` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `item_produce_log`
--

DROP TABLE IF EXISTS `item_produce_log`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `item_produce_log` (
  `id` bigint(20) unsigned NOT NULL AUTO_INCREMENT COMMENT 'ID',
  `role_id` bigint(20) unsigned NOT NULL DEFAULT 0 COMMENT '角色ID',
  `item_id` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '物品ID',
  `operation` varchar(32) NOT NULL DEFAULT '' COMMENT '操作',
  `from` varchar(32) NOT NULL DEFAULT '' COMMENT '来源',
  `time` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '时间',
  PRIMARY KEY (`id`) USING BTREE,
  KEY `role_id` (`role_id`) USING BTREE,
  KEY `time` (`time`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci ROW_FORMAT=DYNAMIC COMMENT='物品产出日志表';
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `item_produce_log`
--

LOCK TABLES `item_produce_log` WRITE;
/*!40000 ALTER TABLE `item_produce_log` DISABLE KEYS */;
/*!40000 ALTER TABLE `item_produce_log` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `key`
--

DROP TABLE IF EXISTS `key`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `key` (
  `role_id` bigint(20) unsigned NOT NULL DEFAULT 0 COMMENT '角色ID',
  `key` char(255) NOT NULL DEFAULT '' COMMENT '码(select_by_key)',
  PRIMARY KEY (`role_id`,`key`) USING BTREE,
  KEY `key` (`key`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci ROW_FORMAT=DYNAMIC COMMENT='角色兑换码表';
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `key`
--

LOCK TABLES `key` WRITE;
/*!40000 ALTER TABLE `key` DISABLE KEYS */;
/*!40000 ALTER TABLE `key` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `key_award_data`
--

DROP TABLE IF EXISTS `key_award_data`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `key_award_data` (
  `type` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '类型',
  `is_unique` enum('false','true') NOT NULL COMMENT '是否唯一(validate(boolean))',
  `award` varchar(255) NOT NULL DEFAULT '' COMMENT '奖励',
  PRIMARY KEY (`type`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci ROW_FORMAT=DYNAMIC COMMENT='兑换码奖励配置表';
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `key_award_data`
--

LOCK TABLES `key_award_data` WRITE;
/*!40000 ALTER TABLE `key_award_data` DISABLE KEYS */;
INSERT INTO `key_award_data` VALUES
(1,'false','[{700001,1},{700002,2},{700003,3}]'),
(2,'true','[{700001,1},{700002,2},{700003,3}]');
/*!40000 ALTER TABLE `key_award_data` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `key_data`
--

DROP TABLE IF EXISTS `key_data`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `key_data` (
  `key` char(255) NOT NULL DEFAULT '' COMMENT '码',
  `type` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '类型',
  PRIMARY KEY (`key`) USING BTREE,
  KEY `key` (`key`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci ROW_FORMAT=DYNAMIC COMMENT='兑换码配置表';
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `key_data`
--

LOCK TABLES `key_data` WRITE;
/*!40000 ALTER TABLE `key_data` DISABLE KEYS */;
INSERT INTO `key_data` VALUES
('fake',2),
('test',1);
/*!40000 ALTER TABLE `key_data` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `level_data`
--

DROP TABLE IF EXISTS `level_data`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `level_data` (
  `level` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '等级',
  `exp` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '经验',
  PRIMARY KEY (`level`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci ROW_FORMAT=DYNAMIC COMMENT='等级配置表';
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `level_data`
--

LOCK TABLES `level_data` WRITE;
/*!40000 ALTER TABLE `level_data` DISABLE KEYS */;
INSERT INTO `level_data` VALUES
(0,100),
(1,200),
(2,300),
(3,400),
(4,500),
(5,600),
(6,700),
(7,800),
(8,900),
(9,1000);
/*!40000 ALTER TABLE `level_data` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `login_log`
--

DROP TABLE IF EXISTS `login_log`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `login_log` (
  `id` bigint(20) unsigned NOT NULL AUTO_INCREMENT COMMENT 'ID',
  `role_id` bigint(20) unsigned NOT NULL DEFAULT 0 COMMENT '角色ID',
  `ip` char(255) NOT NULL DEFAULT '' COMMENT '登录IP',
  `device_id` char(255) NOT NULL DEFAULT '' COMMENT '设备ID',
  `login_time` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '登录时间',
  `online_time` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '在线时间',
  `logout_time` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '登出时间',
  `time` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '时间',
  PRIMARY KEY (`id`) USING BTREE,
  KEY `role_id` (`role_id`) USING BTREE,
  KEY `time` (`time`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci ROW_FORMAT=DYNAMIC COMMENT='登录日志';
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `login_log`
--

LOCK TABLES `login_log` WRITE;
/*!40000 ALTER TABLE `login_log` DISABLE KEYS */;
/*!40000 ALTER TABLE `login_log` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `lucky_money`
--

DROP TABLE IF EXISTS `lucky_money`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `lucky_money` (
  `lucky_money_no` bigint(20) unsigned NOT NULL AUTO_INCREMENT COMMENT '红包编号',
  `server_id` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '服务器ID',
  `role_id` bigint(20) unsigned NOT NULL DEFAULT 0 COMMENT '角色ID',
  `role_name` char(16) NOT NULL DEFAULT '' COMMENT '角色名',
  `guild_id` bigint(20) unsigned NOT NULL DEFAULT 0 COMMENT '公会ID',
  `guild_name` char(16) NOT NULL DEFAULT '' COMMENT '公会名',
  `total_gold` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '总金币',
  `remain_gold` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '剩余金币',
  `total_number` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '总人数',
  `receive_number` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '已领取人数',
  `receive_list` varchar(0) GENERATED ALWAYS AS ('') VIRTUAL COMMENT '领取列表',
  `scope` varchar(255) NOT NULL DEFAULT '' COMMENT '范围',
  `restrict` bigint(20) unsigned NOT NULL DEFAULT 0 COMMENT '限制',
  `skin` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '皮肤',
  `message` char(255) NOT NULL DEFAULT '' COMMENT '消息',
  `time` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '发送时间',
  `flag` tinyint(3) unsigned GENERATED ALWAYS AS (0) VIRTUAL COMMENT '标识(flag)',
  PRIMARY KEY (`lucky_money_no`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci ROW_FORMAT=DYNAMIC COMMENT='红包信息表';
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `lucky_money`
--

LOCK TABLES `lucky_money` WRITE;
/*!40000 ALTER TABLE `lucky_money` DISABLE KEYS */;
/*!40000 ALTER TABLE `lucky_money` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `lucky_money_role`
--

DROP TABLE IF EXISTS `lucky_money_role`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `lucky_money_role` (
  `lucky_money_no` bigint(20) unsigned NOT NULL DEFAULT 0 COMMENT '红包编号(delete_by_lucky_money_no)',
  `server_id` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '服务器ID',
  `role_id` bigint(20) unsigned NOT NULL DEFAULT 0 COMMENT '角色ID',
  `role_name` char(16) NOT NULL DEFAULT '' COMMENT '角色名',
  `guild_id` bigint(20) unsigned NOT NULL DEFAULT 0 COMMENT '公会ID',
  `guild_name` char(16) NOT NULL DEFAULT '' COMMENT '公会名',
  `gold` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '领取金币数',
  `time` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '领取时间',
  `flag` tinyint(3) unsigned GENERATED ALWAYS AS (0) VIRTUAL COMMENT '标识(flag)',
  PRIMARY KEY (`lucky_money_no`,`role_id`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci ROW_FORMAT=DYNAMIC COMMENT='红包角色表';
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `lucky_money_role`
--

LOCK TABLES `lucky_money_role` WRITE;
/*!40000 ALTER TABLE `lucky_money_role` DISABLE KEYS */;
/*!40000 ALTER TABLE `lucky_money_role` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `mail`
--

DROP TABLE IF EXISTS `mail`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `mail` (
  `mail_id` bigint(20) unsigned NOT NULL AUTO_INCREMENT COMMENT '邮件ID',
  `role_id` bigint(20) unsigned NOT NULL DEFAULT 0 COMMENT '角色ID(select_by_role_id)',
  `receive_time` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '接收时间',
  `expire_time` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '过期时间',
  `read_time` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '读取时间(update_read)',
  `receive_attachment_time` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '领取附件时间(update_receive)',
  `title` char(255) NOT NULL DEFAULT '' COMMENT '标题',
  `content` char(255) NOT NULL DEFAULT '' COMMENT '内容',
  `attachment` varchar(255) NOT NULL DEFAULT '' COMMENT '附件',
  `from` varchar(32) NOT NULL DEFAULT '' COMMENT '来源',
  `flag` tinyint(3) unsigned GENERATED ALWAYS AS (0) VIRTUAL COMMENT '标识(flag)',
  PRIMARY KEY (`mail_id`) USING BTREE,
  KEY `role_id` (`role_id`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci ROW_FORMAT=DYNAMIC COMMENT='角色邮件表';
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `mail`
--

LOCK TABLES `mail` WRITE;
/*!40000 ALTER TABLE `mail` DISABLE KEYS */;
/*!40000 ALTER TABLE `mail` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `map_data`
--

DROP TABLE IF EXISTS `map_data`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `map_data` (
  `map_id` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '地图ID',
  `type` enum('full','slice') NOT NULL COMMENT '广播类型(validate(map_type))',
  `reconnect` enum('false','true') NOT NULL COMMENT '是否重连(validate(boolean))',
  `monsters` varchar(255) NOT NULL DEFAULT '' COMMENT '随地图启动的怪物',
  `rank_key` enum('none','camp','guild','role','team') NOT NULL COMMENT '榜键类型(validate(map_rank_key))',
  `rank_value` enum('none','hurt') NOT NULL COMMENT '榜值类型(validate(map_rank_value))',
  `rank_mode` enum('none','global','local','share') NOT NULL COMMENT '榜模式(validate(map_rank_mode))',
  `enter_points` varchar(255) NOT NULL DEFAULT '' COMMENT '进入点',
  `pk_mode` varchar(255) NOT NULL DEFAULT '' COMMENT 'PK模式',
  `enter_script` varchar(255) NOT NULL DEFAULT '' COMMENT '进入脚本',
  `relive_script` varchar(255) NOT NULL DEFAULT '' COMMENT '复活脚本',
  `leave_script` varchar(255) NOT NULL DEFAULT '' COMMENT '离开脚本',
  PRIMARY KEY (`map_id`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci ROW_FORMAT=DYNAMIC COMMENT='地图配置表';
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `map_data`
--

LOCK TABLES `map_data` WRITE;
/*!40000 ALTER TABLE `map_data` DISABLE KEYS */;
INSERT INTO `map_data` VALUES
(100000,'slice','false','[]','none','hurt','share','[]','[]','[]','[]','[]'),
(110001,'full','false','[]','camp','hurt','share','[]','[]','[]','[]','[]'),
(110002,'full','false','[]','camp','hurt','share','[]','[]','[]','[]','[]'),
(110003,'full','false','[]','camp','hurt','share','[]','[]','[]','[]','[]'),
(120001,'full','false','[]','team','hurt','share','[]','[]','[]','[]','[]'),
(120002,'full','false','[]','team','hurt','share','[]','[]','[]','[]','[]'),
(120003,'full','false','[]','team','hurt','share','[]','[]','[]','[]','[]'),
(200001,'slice','true','[]','guild','hurt','share','[]','[]','[]','[]','[]'),
(200002,'slice','true','[]','guild','hurt','share','[]','[]','[]','[]','[]'),
(200003,'slice','true','[]','guild','hurt','share','[]','[]','[]','[]','[]'),
(300001,'slice','true','[]','role','hurt','share','[]','[]','[]','[]','[]'),
(300002,'slice','true','[]','role','hurt','share','[]','[]','[]','[]','[]'),
(300003,'slice','true','[]','role','hurt','share','[]','[]','[]','[]','[]');
/*!40000 ALTER TABLE `map_data` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `monster_data`
--

DROP TABLE IF EXISTS `monster_data`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `monster_data` (
  `monster_id` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '怪物ID',
  `type` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '类型ID',
  `name` char(255) NOT NULL DEFAULT '' COMMENT '怪物名称',
  `description` char(255) NOT NULL DEFAULT '' COMMENT '怪物描述',
  `level` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '等级',
  `hp` bigint(20) unsigned NOT NULL DEFAULT 0 COMMENT '血量',
  `map_id` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '地图ID',
  `camp` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '阵营',
  `range` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '攻击距离',
  `distance` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '搜索距离',
  `relive_time` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '复活时间',
  `act_type` enum('active','fix','movable','passive') NOT NULL COMMENT '动作类型(validate(act_type))',
  `act_script` varchar(255) NOT NULL DEFAULT '' COMMENT '动作脚本(enemy:敌人/role:玩家/monster:怪物/{monster,组ID}:特定怪物)',
  `skills` varchar(255) NOT NULL DEFAULT '' COMMENT '技能',
  `born_points` varchar(255) NOT NULL DEFAULT '' COMMENT '出生点',
  `award` varchar(255) NOT NULL DEFAULT '' COMMENT '奖励',
  PRIMARY KEY (`monster_id`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci ROW_FORMAT=DYNAMIC COMMENT='怪物配置表';
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `monster_data`
--

LOCK TABLES `monster_data` WRITE;
/*!40000 ALTER TABLE `monster_data` DISABLE KEYS */;
INSERT INTO `monster_data` VALUES
(1,1,'active','主动',1,100,200001,1,1,300,0,'active','[role]','[1]','[{10,10},{20,10},{30,10},{40,10},{50,10},{60,10},{70,10},{10,10},{90,10},{100,10}]','[{100005,100}]'),
(2,2,'passive','被动',1,200,200002,1,2,300,0,'passive','[enemy]','[1]','[{40,10}]','[{100005,200}]'),
(3,3,'movable','移动',1,300,200003,1,3,300,0,'movable','[]','[]','[{60,10}]','[{100005,300}]'),
(4,4,'fix','固定',1,400,0,1,4,300,0,'fix','[]','[]','[{80,10}]','[]'),
(5,5,'act','行为',1,500,0,1,5,300,0,'fix','[enemy]','[]','[{100,10}]','[]'),
(6,6,'boom','爆炸',1,600,0,1,6,300,0,'active','[{monster, 20}, {monster, 50}, role]','[]','[{120,10}]','[{100005,600}]'),
(7,5,'act','行为',1,700,0,1,7,300,0,'fix','[enemy]','[]','[{140,10}]','[]'),
(8,6,'boom','爆炸',1,800,0,1,8,300,0,'fix','[{monster, 20}, {monster, 50}, role]','[]','[{160,10}]','[]');
/*!40000 ALTER TABLE `monster_data` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `notice`
--

DROP TABLE IF EXISTS `notice`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `notice` (
  `notice_id` bigint(20) unsigned NOT NULL AUTO_INCREMENT COMMENT '公告ID',
  `type` tinyint(3) unsigned NOT NULL DEFAULT 0 COMMENT '类型',
  `receive_time` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '接收时间',
  `expire_time` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '过期时间',
  `title` char(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '标题',
  `content` char(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '内容',
  `attachment` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '附件',
  `from` varchar(32) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '来源',
  PRIMARY KEY (`notice_id`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_general_ci COMMENT='公告表';
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `notice`
--

LOCK TABLES `notice` WRITE;
/*!40000 ALTER TABLE `notice` DISABLE KEYS */;
/*!40000 ALTER TABLE `notice` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `online_log`
--

DROP TABLE IF EXISTS `online_log`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `online_log` (
  `id` bigint(20) unsigned NOT NULL AUTO_INCREMENT COMMENT 'ID',
  `total` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '总计',
  `online` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '在线',
  `hosting` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '挂机',
  `hour` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '当前小时',
  `time` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '当前时间',
  PRIMARY KEY (`id`) USING BTREE,
  KEY `time` (`time`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci ROW_FORMAT=DYNAMIC COMMENT='在线统计日志';
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `online_log`
--

LOCK TABLES `online_log` WRITE;
/*!40000 ALTER TABLE `online_log` DISABLE KEYS */;
/*!40000 ALTER TABLE `online_log` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `parameter_data`
--

DROP TABLE IF EXISTS `parameter_data`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `parameter_data` (
  `key` varchar(255) NOT NULL DEFAULT '' COMMENT '参数键',
  `value` varchar(255) NOT NULL DEFAULT '' COMMENT '参数值',
  `description` char(255) NOT NULL DEFAULT '' COMMENT '参数名称',
  PRIMARY KEY (`key`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci ROW_FORMAT=DYNAMIC COMMENT='游戏参数配置表';
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `parameter_data`
--

LOCK TABLES `parameter_data` WRITE;
/*!40000 ALTER TABLE `parameter_data` DISABLE KEYS */;
INSERT INTO `parameter_data` VALUES
('bag_size','100','装备背包大小'),
('chat_cd','0','聊天冷却时间'),
('chat_guild_size_limit','100','公会聊天保留条数'),
('chat_level','0','聊天开放等级'),
('chat_private_size_limit','100','私聊保留条数'),
('chat_system_size_limit','100','系统信息条数'),
('chat_world_size_limit','100','世界聊天保留条数'),
('dungeon_inspire_buff_id','3','副本鼓舞BuffID'),
('friend_level','0','好友开放等级'),
('friend_number','50','好友上限'),
('guild_create_cd','86400','公会创建冷却时间'),
('guild_join_cd','86400','公会加入冷却时间'),
('guild_member_limit','[{0, 50}, {1, 60}, {2, 70}, {3, 80}, {4, 90}, {5, 100}]','公会人员数'),
('item_size','100','道具背包大小'),
('language','zhCN','默认语言'),
('login_cd','180','登录时间间隔'),
('mail_expire_time','604800','邮件过期时间'),
('mail_max_item','10','单封邮件最大物品数'),
('store_size','100','仓库大小'),
('time_zone','8','时区');
/*!40000 ALTER TABLE `parameter_data` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `rank`
--

DROP TABLE IF EXISTS `rank`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `rank` (
  `type` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '类型(select_by_type)(delete_by_type)',
  `order` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '排名',
  `key` bigint(20) unsigned NOT NULL DEFAULT 0 COMMENT '键',
  `value` bigint(20) unsigned NOT NULL DEFAULT 0 COMMENT '值',
  `time` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '时间',
  `name` char(16) NOT NULL DEFAULT '' COMMENT '名字',
  `server_id` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '服务器ID',
  `digest` varchar(255) NOT NULL DEFAULT '' COMMENT '摘要数据',
  `extra` varchar(255) NOT NULL DEFAULT '' COMMENT '额外数据',
  `other` varchar(255) NOT NULL DEFAULT '' COMMENT '其他数据',
  `flag` tinyint(3) unsigned GENERATED ALWAYS AS (1) VIRTUAL COMMENT '标识(flag)',
  PRIMARY KEY (`type`,`order`) USING BTREE,
  KEY `order` (`order`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci ROW_FORMAT=DYNAMIC COMMENT='角色排行表';
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `rank`
--

LOCK TABLES `rank` WRITE;
/*!40000 ALTER TABLE `rank` DISABLE KEYS */;
/*!40000 ALTER TABLE `rank` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `reference_data`
--

DROP TABLE IF EXISTS `reference_data`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `reference_data` (
  `key` varchar(255) NOT NULL DEFAULT '' COMMENT '键',
  `value` char(255) NOT NULL DEFAULT '' COMMENT '值',
  `description` char(255) NOT NULL DEFAULT '' COMMENT '描述',
  PRIMARY KEY (`key`,`value`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci COMMENT='数据参考配置表';
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `reference_data`
--

LOCK TABLES `reference_data` WRITE;
/*!40000 ALTER TABLE `reference_data` DISABLE KEYS */;
INSERT INTO `reference_data` VALUES
('condition','{classes, n}','职业为n'),
('condition','{level, n}','等级n级'),
('condition','{sex, n}','性别为n'),
('condition','{vip, n}','VIP等级n级');
/*!40000 ALTER TABLE `reference_data` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `role`
--

DROP TABLE IF EXISTS `role`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `role` (
  `role_id` bigint(20) unsigned NOT NULL AUTO_INCREMENT COMMENT '角色ID',
  `role_name` char(16) NOT NULL DEFAULT '' COMMENT '角色名(update_name)',
  `server_id` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '服务器ID',
  `account_name` char(16) NOT NULL DEFAULT '' COMMENT '账户',
  `origin_server_id` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '原服务器ID',
  `type` tinyint(3) unsigned NOT NULL DEFAULT 0 COMMENT '账户类型',
  `status` tinyint(3) unsigned NOT NULL DEFAULT 0 COMMENT '账户状态',
  `sex` tinyint(3) unsigned NOT NULL DEFAULT 0 COMMENT '性别',
  `avatar` tinyint(3) unsigned NOT NULL DEFAULT 0 COMMENT '头像',
  `classes` tinyint(3) unsigned NOT NULL DEFAULT 0 COMMENT '职业',
  `level` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '等级',
  `is_online` tinyint(3) unsigned NOT NULL DEFAULT 0 COMMENT '是否在线',
  `register_time` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '注册时间',
  `login_time` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '登录时间',
  `logout_time` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '登出时间',
  `world_chat_time` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '世界聊天时间',
  `guild_chat_time` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '公会聊天时间',
  `first_charge_time` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '首充时间',
  `last_charge_time` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '最后充值时间',
  `charge_total` decimal(10,2) unsigned NOT NULL DEFAULT 0.00 COMMENT '总充值',
  `item_size` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '普通背包大小',
  `bag_size` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '装备背包大小',
  `store_size` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '仓库背包大小',
  `map` varchar(255) NOT NULL DEFAULT '' COMMENT '地图',
  `channel` char(255) NOT NULL DEFAULT '' COMMENT '渠道',
  `device_id` char(255) NOT NULL DEFAULT '' COMMENT '设备ID',
  `device_type` char(255) NOT NULL DEFAULT '' COMMENT '设备类型',
  `mac` char(255) NOT NULL DEFAULT '' COMMENT 'Mac地址',
  `ip` char(255) NOT NULL DEFAULT '' COMMENT 'IP地址',
  PRIMARY KEY (`role_id`) USING BTREE,
  UNIQUE KEY `role_name` (`role_name`) USING BTREE,
  KEY `account_name` (`account_name`) USING BTREE,
  KEY `register_time` (`register_time`) USING BTREE,
  KEY `first_charge_time` (`first_charge_time`) USING BTREE
) ENGINE=InnoDB AUTO_INCREMENT=364 DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci ROW_FORMAT=DYNAMIC COMMENT='角色信息表';
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `role`
--

LOCK TABLES `role` WRITE;
/*!40000 ALTER TABLE `role` DISABLE KEYS */;
INSERT INTO `role` VALUES
(1,'1',1,'1',0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0.00,0,0,0,'','','','','',''),
(2,'2',1,'2',0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0.00,0,0,0,'','','','','',''),
(3,'3',1,'3',0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0.00,0,0,0,'','','','','',''),
(65,'伊藤蓮',10,'伊藤蓮',198,225,101,9,172,89,716,119,196,865,186,764,892,888,282,948.79,38,312,705,'nT8TZtbuaE','ImzlWtUweY','wSwl8z6TiE','v5zz6x7aoF','lGYSLKMqt9','253.88.191.255'),
(66,'市川凛',592,'市川凛',939,81,140,203,113,220,614,255,303,344,430,720,945,425,339,343.82,413,674,2,'AhRoGCBopM','hyOe5UlKv9','6aF045blFf','OE4w7Y1mCf','0TmASVDy6y','115.3.214.255'),
(67,'毛安琪',186,'毛安琪',196,169,229,35,69,81,69,239,871,332,10,884,964,809,226,471.03,800,167,541,'08VzigBmLp','JcYq3de7kL','a0GNB6PjBx','SPMNEuyoyu','pGN9kOBHsL','4.250.4.209'),
(68,'山本凛',83,'山本凛',553,170,104,201,96,49,971,60,940,612,949,860,653,672,669,574.94,904,365,158,'FEAWVTOz5P','F4043X5To9','5Qctu4VSYg','tc68VIiV9E','KG5f2LN5Uv','238.251.235.252'),
(69,'陶睿',90,'陶睿',388,30,139,168,68,70,860,25,691,986,206,711,243,188,543,490.68,79,155,177,'QkkMWem9Za','dEaCo83ztR','Aj9vg1W6Ya','5lNDowp11R','OqZto8fduD','180.80.0.1'),
(70,'雷慧敏',24,'雷慧敏',492,94,178,173,117,233,313,184,354,155,97,772,499,400,245,57.92,895,42,617,'9CzTpMoDQ8','2BwEuRPvqt','MSWcsNdHdY','icMci2h53h','SX07U5WRZT','252.5.72.2'),
(71,'赵宇宁',668,'赵宇宁',875,144,43,70,78,174,190,247,350,968,15,942,32,955,94,211.78,32,725,114,'51AN4tJR7u','h9EYzSyO30','dhMwm7OKvM','Q22WuK56VO','JwHGzEfvBA','229.229.227.207'),
(72,'彭玲玲',878,'彭玲玲',239,173,38,107,34,234,722,140,76,232,968,874,434,525,549,767.49,114,196,150,'1538qkbIqj','5potlbFlx0','hiG98dCUfo','sODO2OUwdg','OLD8ageX83','205.255.244.214'),
(73,'工藤愛梨',109,'工藤愛梨',721,56,129,25,233,207,365,227,228,920,515,739,958,150,427,132.35,696,461,258,'zOavph6ztb','SI6s3ZNKAY','nE6SetqeTS','axBxmkqp0j','Ui2WnIlDBI','38.194.228.2'),
(74,'江云熙',100,'江云熙',168,178,67,144,159,245,846,181,174,727,919,646,979,255,371,794.90,967,548,144,'BF9LhjDtW5','ZlOAAlJ952','Amjgc7878A','JsST5sOS6w','RU9ay90BL0','38.212.8.117'),
(75,'丸山詩乃',422,'丸山詩乃',584,24,78,189,92,33,861,209,93,144,770,846,377,751,519,961.62,410,116,374,'sB0PN1jFPa','pqB04n8YR9','e4eF2aulNB','MS0TdbynSF','bqS6t2PbbL','8.209.181.254'),
(76,'戴秀英',608,'戴秀英',190,234,227,58,119,41,921,88,718,401,602,594,452,92,681,46.47,626,559,107,'XuaiD4ockW','dZadMIkO0O','AYWSSFTWcJ','Om2tz2WiIQ','VB4pT9V2Qz','84.228.253.224'),
(77,'戴安琪',268,'戴安琪',699,86,215,226,5,131,846,86,75,978,448,241,136,160,475,324.71,903,60,810,'RzWMgUuIro','f1Y60YDiyY','hx4R3EFRdQ','VAkPPQPqqS','6lzR0j11HG','226.159.254.250'),
(78,'小山彩乃',496,'小山彩乃',748,176,3,28,44,121,726,31,160,567,769,580,505,187,503,534.24,255,996,737,'aQcq7tAGNy','JdvAi229wq','JzUZlPyhVJ','EUjdifPAny','QnfwE3XGSx','39.75.212.252'),
(79,'井上葉月',722,'井上葉月',444,94,253,199,13,253,995,184,95,749,294,763,408,668,704,208.77,313,99,896,'2DuX6u0two','G2tPIPnhfQ','LYEEHcjK2d','CXfmZo9vFS','GXJ514fTY4','1.142.46.5'),
(80,'安藤拓哉',955,'安藤拓哉',589,10,155,2,206,246,647,168,979,625,182,755,228,140,710,389.57,970,187,301,'TPpJtBls70','JY3IBc4ZA5','Y8agfu4toN','P7bErxBFp9','DiqKxwdNjz','0.143.215.58'),
(81,'田中樹',688,'田中樹',482,96,122,254,210,181,644,201,263,696,811,585,384,306,271,853.23,533,837,505,'Y8ZtIe5MIt','0NBBkWIfWO','e5MFrakvp8','u0eWNxKh8V','JaxX82fmdo','2.75.251.8'),
(82,'卢杰宏',323,'卢杰宏',32,32,57,226,202,208,502,141,295,395,514,43,568,10,502,565.32,442,533,454,'ShaFTe5FuX','dMNh034Rf8','d22VyHpRzE','ZBVicU4YWq','z9ytcto6Mp','7.61.250.251'),
(83,'史宇宁',610,'史宇宁',957,243,111,44,74,162,139,237,627,967,53,187,879,624,686,52.75,475,8,307,'0vqyWHlorX','y9MLCvCdFR','lgoM3qQ1t9','pMih2xepAk','30KEe9HgiG','7.0.1.251'),
(84,'蔣裕玲',862,'蔣裕玲',970,249,131,73,140,191,846,114,112,630,269,381,174,512,898,803.25,990,550,747,'e8D0kvgzUF','CgQ2pcry6J','R223EZnaDJ','WYHjD7dUuV','fZphUglFDb','255.0.1.1'),
(85,'山口詩乃',167,'山口詩乃',789,20,177,71,232,2,596,4,227,609,676,689,944,195,521,989.05,248,229,159,'G8wEn6sRIR','P80tFOHw8t','CRbaYt6fX4','WQseGEiAma','EkSh4CvkOU','132.237.90.241'),
(86,'黎睿',944,'黎睿',667,240,81,41,161,189,623,39,179,691,221,229,193,376,375,5.54,894,522,627,'uaPi4Uc2GL','cfaIv5kZ1w','XPsru4DiOd','SjCFUbL7g4','90JxAB1PQU','3.6.218.200'),
(87,'中森葵',621,'中森葵',654,37,134,69,14,42,974,115,702,316,890,976,189,788,569,964.34,929,944,178,'3YHjKt7XHS','uBBygdfhac','lFEz4CivW6','SrAPU441k8','uXlpBJwX8d','222.78.240.237'),
(88,'唐安琪',659,'唐安琪',314,63,19,60,160,203,529,39,143,895,940,963,955,325,303,974.53,375,396,227,'brrvKcXl18','L00ibDEBS9','pqCtSWK4s3','j3od9MGqD2','ckADch1HD2','230.11.254.5'),
(89,'劉玲玲',955,'劉玲玲',991,73,250,59,99,92,94,73,493,660,868,424,300,402,821,183.54,22,707,596,'Z6IElAuozX','PhIfPtDZMt','6sMeowMQ3n','YlIIyhxO2t','MaAHFRO4hO','250.246.71.127'),
(90,'關惠敏',579,'關惠敏',195,112,82,249,103,76,265,5,749,196,95,148,685,503,348,55.87,740,31,22,'ak25MjWWwi','wxTxeF8AMP','S2njEYdGYI','Hf6EOMKbkL','dhUFhdtgiy','191.252.220.199'),
(91,'藤田百花',383,'藤田百花',87,77,180,93,188,128,921,232,445,956,542,482,583,689,125,863.29,547,223,596,'vjrqbJHaQa','OBYDeblCKn','MDUsfqxCB9','VWxSDROotA','L9E1KP1Pce','211.255.3.87'),
(92,'關德華',553,'關德華',240,33,159,168,238,164,315,186,3,306,375,241,190,732,403,510.95,616,710,724,'Y6fp74dQon','0gk9OjgQo0','faMVf4Djqq','OI9khdDLOh','wIocRZajcn','244.39.8.251'),
(93,'陆岚',252,'陆岚',816,124,253,239,26,206,724,158,83,822,788,488,592,460,739,544.42,960,962,260,'Su49eyUQsc','tg8DQKwni7','5dsIOBWHEy','lJsBJeE8kd','WMe5zD6HVV','252.219.1.252'),
(94,'森田蓮',966,'森田蓮',353,45,92,57,18,61,756,157,893,840,196,975,429,54,514,246.67,924,144,722,'lJ0MR7ZSYo','qJPAMxfe80','rghjoGgWaK','bs7GebxRBW','7PpLFT6dBM','76.230.217.26'),
(95,'房惠敏',28,'房惠敏',472,167,232,172,147,195,846,0,966,260,775,762,714,179,512,372.07,705,523,26,'S0gN85rBaB','sVsu1rf8uw','4DcsjVKgWx','LMO4lsPaAd','AVmlSQXcPr','1.209.72.5'),
(96,'刘安琪',924,'刘安琪',529,94,39,76,118,2,907,39,564,661,887,129,714,971,460,314.32,567,311,223,'QpWac3kaa1','mKuIBUfSWL','GYgnUOQsHB','PyGW3x9HON','LvG4ckLSdA','64.7.224.136'),
(97,'菊地聖子',145,'菊地聖子',251,44,86,101,160,231,973,233,209,744,167,22,334,688,279,408.11,739,965,801,'nNIeTrBphR','mrKCGsYGuc','ULC3oE7wuL','IWdrfjWfGL','RDMFRP0hRr','35.252.254.100'),
(98,'梅國榮',780,'梅國榮',611,237,171,177,116,234,270,118,587,932,119,434,918,967,513,10.02,101,241,7,'N71sUilFBf','FsQNkU76t2','nZGmEM7riv','du2UIw3MQA','0T0lxCjWJi','219.0.228.217'),
(99,'梅永發',504,'梅永發',481,31,247,251,79,5,208,121,857,38,158,936,903,547,210,12.82,281,399,274,'a2fDSJ2xaM','DpEP2U1f5r','0SG2gWNujA','wNCbOG8Wx7','dMCkmJppHW','4.255.225.49'),
(100,'河野舞',284,'河野舞',485,78,167,54,160,147,672,34,650,516,880,187,703,368,583,42.46,114,449,468,'zhyy1HaTCN','WgRaLVV4h6','o7laKJQBkq','Nq8LhV2BYR','8BRrcsbZoA','153.9.115.250'),
(101,'平野一輝',359,'平野一輝',268,208,28,36,33,177,939,185,751,985,571,482,623,231,464,883.10,511,231,584,'x9nLrWKO8t','s4gexaovBq','4uN8RWRsqQ','2PqbOJ2r2e','rqjUU14izE','255.8.202.251'),
(102,'謝富城',149,'謝富城',475,70,131,221,121,60,20,109,724,576,185,187,544,433,101,256.92,665,916,863,'idXzssidwj','ZnwnKq8HGv','uaLb2UJtnq','pOszOcGwDx','lnn5ZH7Bcn','6.207.9.227'),
(103,'山田美咲',328,'山田美咲',171,220,206,156,149,195,290,151,258,674,957,174,163,380,994,676.50,989,245,369,'CM2BulmhtA','pdrhutOLTz','Iopu4rrf4e','eo1kUIchbp','RTXEXzRLBY','4.51.8.252'),
(104,'吕宇宁',424,'吕宇宁',143,180,225,79,26,116,445,177,757,326,612,944,736,888,434,364.39,138,775,186,'En55JU2YrY','PuA8dcSuHW','65kmqvbXXx','xjLPqgElYZ','Efpl7I9pRT','229.68.83.205'),
(105,'今井蒼士',467,'今井蒼士',107,127,104,222,108,49,308,224,253,239,70,497,33,713,53,733.40,574,589,361,'s8EGDCNzUr','F0Hkdau6wR','jwLWB19zXW','syYWSWdjoU','0vBXqTRRn4','28.41.171.70'),
(106,'田璐',407,'田璐',457,26,67,87,16,226,516,193,303,321,140,258,653,84,218,919.32,773,836,12,'rWw9KO1Uxw','2V4m03DhHd','tXDxGQZIma','E7jzNmiMM7','o9l7N601yt','21.41.201.211'),
(107,'小山悠人',591,'小山悠人',203,11,13,27,150,65,790,134,687,920,669,257,949,988,297,182.27,3,256,55,'6WTBHVnnAL','4zPaLX0BqP','jJVk8NLzSF','8HuSVKAccC','vvuFD9I2gK','12.252.133.184'),
(108,'竹内玲奈',331,'竹内玲奈',453,145,217,229,134,104,417,86,829,673,151,681,410,430,143,586.72,803,6,581,'fNl5hvQK5m','7JLUQBz2jR','y0ytaNOxuK','U8VGwYvw5n','vssjM0rQnl','129.114.252.72'),
(109,'野村七海',952,'野村七海',168,139,49,19,117,37,652,133,431,731,746,302,310,391,615,172.71,494,935,410,'WBC3yxsHUr','ya4vCv3Zpf','nLYt0SJFQP','xcm2WPsYQI','zYMwGl67oU','9.123.220.227'),
(110,'段致远',66,'段致远',289,224,217,232,67,151,582,46,391,241,937,967,811,897,804,717.88,90,358,437,'dLkppIUqlq','Yupkw7TzJI','cUJ0PYDPU2','xx4dOgJPpz','CzoBj54YQF','83.75.5.38'),
(111,'苏震南',681,'苏震南',737,30,195,239,229,25,267,119,101,535,21,722,179,358,615,75.76,205,647,829,'PEmWcp9kis','bPJKtpdJZ6','vISmrtOxFs','i1xBfYDmj4','JIkRK5JSSd','255.255.253.13'),
(112,'贾嘉伦',339,'贾嘉伦',952,142,240,236,49,224,867,225,230,490,522,409,82,968,933,222.25,353,871,452,'Ol6U98sXNS','y8d5FVMQcL','I3oi7IwQFo','LGgsVsNb7X','YFXedKPfO4','250.252.1.52'),
(113,'加藤陸',13,'加藤陸',862,216,215,55,140,228,971,53,848,915,532,349,519,502,637,308.07,575,6,284,'GbBYCiguHq','p8SnAQWap1','WhrA633Aoj','kSh3FaVpjP','gzhyVIaVow','187.9.71.251'),
(114,'郑震南',331,'郑震南',279,89,203,129,22,196,154,155,412,372,37,348,590,328,943,641.76,78,311,637,'mVr3GhsU7o','gh80szBTu2','qZnfpOOA8n','UUdOSxhryY','QZVv0D2YCh','170.34.0.213'),
(115,'河野悠人',53,'河野悠人',460,104,219,140,238,45,115,205,416,806,861,687,490,929,525,247.95,896,240,466,'q7iJYmAqAk','A18rTLkK1E','m854MyBChz','Dkk2tae8ex','a32HKEWjJG','237.213.105.255'),
(116,'今井拓哉',763,'今井拓哉',501,28,249,235,239,252,311,235,330,884,429,619,459,938,50,647.55,151,141,519,'4xo4KRE2R8','MfWZ3gkbzY','9KfaUvwfEF','frAFUsg2U0','Y5Yji095xv','195.241.252.85'),
(117,'罗云熙',413,'罗云熙',700,181,198,227,127,197,20,235,729,44,362,909,396,684,970,45.30,99,145,230,'MDsnaQaFyS','tPXJpK1djW','G1miqh6jE2','IusqXSYmY2','PFSX5wClZn','93.230.59.39'),
(118,'山下七海',928,'山下七海',209,35,180,149,213,138,76,160,452,716,831,236,269,150,950,338.38,847,340,799,'O8YdhlSgpY','SUIbT8Q78v','RzjAeiWCKT','ub5blpPNwH','xQ1WhjSvlr','153.22.206.27'),
(119,'段睿',808,'段睿',570,250,31,1,136,211,383,109,248,417,92,465,910,40,850,135.63,624,456,275,'JeQIzBdj6U','oHhTQhxRKT','zTUUFE5PQW','1vs4QNGDqe','MwZDbHpQqO','128.252.255.5'),
(120,'武致远',821,'武致远',845,173,127,96,111,235,412,175,903,98,752,369,306,356,537,999.81,389,895,306,'0HB3rRP7ni','YwNGgzoDul','THyTo8XgkG','Bd761leGuy','9XbkuwLukH','252.181.218.168'),
(121,'中川花',979,'中川花',435,246,204,10,202,44,572,155,242,584,238,339,803,137,936,521.11,121,348,956,'PkrHqgyq3x','Qd3FkHaSLi','05Ykt7zdIb','JcBzhJszke','O6ZM6bEmBF','0.179.216.38'),
(122,'唐潤發',326,'唐潤發',351,185,80,131,95,18,708,99,365,633,112,263,51,423,325,885.95,971,874,850,'6pKgX8b7Iw','g6XL7hWZDj','FMt9HTnztY','y9EenHTn0P','HZb7xx3R7f','254.249.8.255'),
(123,'藤原美緒',210,'藤原美緒',814,146,213,60,27,5,86,116,63,732,48,800,834,938,254,638.57,596,94,518,'PFj7M7ejOS','56nu0hHfmV','dRw1hv6Erl','DlQY6SMpxV','LtropAKZR4','4.255.54.186'),
(124,'陆睿',408,'陆睿',820,202,96,225,142,57,997,162,520,988,455,291,327,270,873,105.63,698,76,582,'nwI3APOV9S','zQhy39jRvN','viwnZ7Oixg','pCH5hB8hz4','YsRt2BKyOp','34.5.250.203'),
(125,'清水陽太',841,'清水陽太',411,22,100,220,243,224,597,243,248,269,329,50,681,604,175,628.79,698,162,859,'5xkn98K8TW','u0OdwQJs28','L24iVdMNt3','Fpf6ZNO7j3','TDMvG6n9Kq','72.7.233.110'),
(126,'张璐',556,'张璐',512,121,14,168,178,96,322,206,43,512,693,174,412,86,82,365.91,499,117,2,'aOy3ELNjsj','0wQuuhRGI8','SUBQuGL56X','k093FN8Dom','xWyDgvyTAm','101.2.146.250'),
(127,'宋杰宏',429,'宋杰宏',22,216,191,177,144,37,145,103,614,387,870,710,640,337,590,641.67,384,826,814,'viuIiE1aql','JTSUcFWtm9','w3qN05MpIN','5kKXi7wVg1','Ui9ti0H2kv','7.158.52.255'),
(128,'韩晓明',314,'韩晓明',412,249,169,32,45,186,424,144,986,916,845,670,317,467,14,397.02,385,80,672,'QRZ1R4JUBu','O7U57SOMU2','GoC0zLGeUy','iSP32q7qcP','3WXEnPnRz6','251.89.179.2'),
(129,'谷浩然',144,'谷浩然',386,192,138,200,160,111,511,74,21,44,266,518,944,451,614,517.84,687,67,738,'uInlMT4fvR','wKxcaqDVE9','Y6WAQSO0R3','ep17FwOjjV','LgX3NNIzsu','122.212.231.8'),
(130,'河野聖子',535,'河野聖子',756,29,111,154,151,33,779,0,459,594,555,442,541,927,643,796.50,392,925,70,'NcF0aLwlIm','MZMSdR4KwJ','rK7IF986Jk','ZXnAJgRm3S','nMx8kKunn8','205.4.250.54'),
(131,'严岚',112,'严岚',642,217,242,38,106,2,470,99,902,148,617,683,331,273,895,463.57,89,251,223,'SUfgmxdcoD','Kys6eMsxjK','n9uPRykZP9','wj65IxrJwm','nHDx11UcQ9','3.222.87.252'),
(132,'中野美羽',12,'中野美羽',154,31,223,91,139,253,470,101,392,355,747,449,812,186,718,347.31,970,821,812,'4MkCs0tyiI','gL8Kl6uTVN','sTvSgAasW3','qxIy2qwffk','lTLORrGO3Z','169.251.120.207'),
(133,'官世榮',365,'官世榮',990,71,182,177,205,190,101,237,25,52,862,243,994,761,582,697.51,414,145,546,'sIfJlRRzVq','0c1luhdoq3','pEpXZHa0LE','gy1zKvpRD9','dfRPskbiMf','244.254.195.7'),
(134,'江梓軒',998,'江梓軒',573,15,229,37,67,162,767,72,628,497,685,154,807,910,691,304.21,202,197,726,'6ADv7iKwOU','95ukhw7LAq','LN04dOU5xw','6hu55mGM24','wASVBdprUv','235.222.253.253'),
(135,'中村悠人',196,'中村悠人',230,231,171,124,107,248,627,112,650,261,866,822,314,751,868,476.97,64,946,109,'DeD1QE7kjf','Lnbr1wFSKs','fAgpNhYzS3','p5iiU5YxvV','2XDgaGw7RW','3.220.7.219'),
(136,'藤原大地',786,'藤原大地',252,207,80,186,187,88,684,146,114,950,784,113,742,201,603,933.39,984,969,345,'WOOC9jiVrY','CXdawRX0T9','7Mq8kfdHkZ','v7Id873FbL','4GGyL73gZz','241.252.211.41'),
(137,'中野和真',648,'中野和真',715,146,182,70,158,11,342,54,667,604,755,962,68,358,795,132.57,573,412,220,'7WnY14g4bH','iHVHjaIXqN','9Y5giE6NQH','KjK2BjWXPd','FgZrAbHrHC','241.222.255.192'),
(138,'盧家玲',760,'盧家玲',798,52,102,251,242,239,979,187,786,483,252,413,527,573,600,775.41,405,71,140,'1rwTuKakdd','0iXvzoRY30','lLXrOllIEb','Jo4VqgGefZ','wyqGCb1r7H','201.151.52.8'),
(139,'江嘉伦',649,'江嘉伦',508,232,238,114,109,49,897,92,119,795,512,814,563,161,408,130.65,345,754,991,'VwL7CavX7O','VhDq0TZCm2','VdBwqBN5gg','36AUHvMfmF','gYlrHy78qs','201.252.50.4'),
(140,'近藤詩乃',198,'近藤詩乃',378,84,76,8,124,207,231,75,680,436,160,699,297,29,578,219.38,356,606,67,'H3bSZvxXi9','a6eptvcq1i','4dhMeOV2IV','EvVvQozNUl','L5M7gMS7Gu','8.250.7.40'),
(141,'中森舞',304,'中森舞',35,116,121,34,37,56,963,86,831,177,792,537,340,678,345,404.73,442,582,488,'yiPy0Ak5iB','3Dw4cY2dqy','w3j7vw9RY3','dYyJWH9WuA','nYFqfsbIyE','54.70.114.185'),
(142,'宋致远',36,'宋致远',201,183,72,118,174,57,564,128,430,207,265,193,648,168,165,302.95,782,534,557,'BLUquGlkEW','C55VUX96cw','Eq2SdMwlPP','3UT0QHM9yG','WEQnPJ8Rou','53.240.5.7'),
(143,'谷口美羽',0,'谷口美羽',309,19,84,90,164,195,469,116,741,243,763,216,796,278,241,775.63,660,713,923,'HYcHmZsX6J','ZN4OVeEqTq','es1nKfsp1k','PZINxGafez','f0nqGC5Rk4','155.9.234.1'),
(144,'曾震南',137,'曾震南',607,108,120,150,132,165,355,184,505,930,435,104,636,367,626,163.86,542,214,375,'I992eAlvZ4','VPaQd9YlLn','0fNmtYOP9c','YntOjUzUpa','47XfseShjr','1.132.246.14'),
(145,'苗明',624,'苗明',608,50,198,230,137,111,900,67,48,422,610,859,841,886,622,762.65,486,331,721,'ZKZuboPcbK','EYnnbKxasX','XYt0dsDvQM','5oxCxHCEGD','8wzzyMFi0p','200.131.71.239'),
(146,'戴杰宏',556,'戴杰宏',614,100,230,215,150,165,330,208,306,738,602,505,714,93,507,156.18,703,977,650,'4rgKc8ifF2','IDWulnUWJ0','uUicgq8uPy','6BKPpmymir','Oh7xCUXCMo','219.151.63.249'),
(147,'河野蒼士',799,'河野蒼士',701,109,144,55,37,124,903,140,705,106,554,185,946,289,808,149.22,997,226,762,'7R4g71vv41','z6O4OW03HX','IsC712IqnQ','ULduiJshCH','ffcAanA6HG','9.192.221.6'),
(148,'區秀文',589,'區秀文',972,46,143,127,34,59,88,84,704,861,120,756,770,479,782,97.96,104,861,261,'TCXhJqdgov','ZSGymoUXmP','NXkOLgg3Gf','zKDuZHA1po','IjzyjSRihR','1.147.4.58'),
(149,'常宇宁',611,'常宇宁',776,188,124,53,71,242,97,157,774,613,676,263,686,308,1,109.27,774,924,179,'WxyClw8Kqy','xm9CaAE206','vfXGwEJ4T6','AVqVkMnxir','oMPmYQ7TRf','70.252.1.7'),
(150,'魏杰宏',450,'魏杰宏',316,183,101,236,116,207,730,230,834,92,238,698,825,16,98,832.23,403,100,38,'vIhBWTCJkY','eXdlRNTfMg','FAuNvvnORV','NSHvfifQAu','kMm6bpu5Hi','135.102.171.252'),
(151,'苑安琪',355,'苑安琪',808,102,123,222,229,17,27,201,895,995,202,956,584,3,88,809.31,519,114,626,'FAbI09bFOm','niLuobNyeJ','uXJAA6dM22','OCBxK7Q1wo','5dijQuOnWD','88.0.189.230'),
(152,'萧睿',653,'萧睿',301,179,72,237,195,14,165,223,190,34,472,725,763,385,709,378.57,501,630,551,'EJDktk5ID3','oQ8Cx71DGX','wOTgHczvOw','bLbY3P5cHE','AeTPRZUMJf','106.129.248.167'),
(153,'石川蓮',20,'石川蓮',460,137,175,29,119,169,571,235,262,786,474,729,270,130,829,191.37,284,368,403,'EFeVH0siAo','ERsGMQZY34','oGNn7R5gKj','AJuAaFptvI','TkflCY5sWC','202.254.252.5'),
(154,'廖詩涵',361,'廖詩涵',487,107,136,138,10,189,747,37,558,971,939,287,914,386,749,643.36,478,13,837,'uy7ymvwCI9','wcaQ7rtwMW','ktkdcsO0EL','tyJJyvFWD4','s8MJbNa5Cc','76.215.47.252'),
(155,'駱國榮',740,'駱國榮',219,138,131,33,180,62,36,238,655,674,612,764,129,266,92,662.16,590,953,595,'6w3nxPuOMB','vUc9ZJV3oE','vNPTtpG5si','mqetBGv6Jz','dzCC0hkL8g','2.7.9.6'),
(156,'梁睿',819,'梁睿',612,253,66,198,163,226,351,246,800,942,806,931,253,514,185,502.06,323,193,126,'et1H7L4FbU','glMrCbaWmd','JycfNUlRLp','swioY3KFlR','DZTK7QsW1E','9.26.149.213'),
(157,'熊嘉伦',352,'熊嘉伦',266,81,5,55,4,118,29,90,585,46,498,770,666,980,255,861.63,500,577,350,'2B0xxRFPKS','TpXXDShzWw','ylBYTfC52r','KRQQDK142v','qtD3PnSqPp','186.5.244.172'),
(158,'狄頴思',546,'狄頴思',711,163,62,228,126,66,425,149,381,768,100,213,749,60,305,438.64,241,123,42,'yZGSoaEGNb','fyteVizmRn','AzsVwWBQil','lWg6uJUwRT','i7qS0TzWhM','9.62.19.229'),
(159,'孙睿',550,'孙睿',955,216,188,221,24,51,838,190,476,765,690,821,346,567,287,580.70,696,955,150,'ncD4IL6yes','7OplalbtHK','HbmYVKfcqp','zQa8vpVAOY','FuizK7oaDc','253.252.7.221'),
(160,'姜震南',730,'姜震南',101,43,46,139,99,206,487,81,121,895,751,416,950,810,857,838.21,569,680,831,'AezMejmHHC','wy1Zueqvjz','8it4LnQTlo','aWypIna7PN','uQNzYbuWRt','251.0.199.5'),
(161,'邵睿',470,'邵睿',259,81,199,221,209,234,621,20,202,828,236,538,457,755,427,438.24,553,472,215,'MKeptLnckr','2y6IXdOZBc','4Ui0UgDAld','6gKchourK1','W8gKROz80E','250.255.203.2'),
(162,'李宇宁',28,'李宇宁',174,179,110,232,152,162,186,117,983,304,586,547,516,71,347,545.23,369,510,386,'OybqZjS1Bl','ouiK5bQN3S','JzYVEfMouP','3WRspC9Xoo','cu8bC6MmNh','254.40.2.96'),
(163,'曹德華',767,'曹德華',622,18,55,134,246,2,773,246,277,169,426,267,666,551,714,49.37,446,107,971,'pDP7RXHLkP','o7UeU2zgRM','hXgxY6sXCH','WqrQPC032N','5MzCVx87JD','192.43.203.9'),
(164,'戴國榮',77,'戴國榮',555,112,55,119,63,173,86,169,839,147,354,873,306,522,18,209.68,59,596,710,'poY0siQD5p','tdwP3BWeEz','1HNXqA30lI','YbY3r097xz','90b6vGCqGc','252.252.72.255'),
(203,'馬天榮',277,'馬天榮',478,11,10,218,38,98,310,96,481,994,369,567,172,713,74,914.00,113,463,339,'jEZsGYs9RF','kkFkcrPm31','MVOH0ntDSo','KSYuNkx2ZY','Y6XMaoZdIR','139.171.255.7'),
(204,'區慧琳',558,'區慧琳',623,248,35,246,233,165,333,113,768,270,528,597,748,991,104,717.54,195,653,266,'rjFVmibDW0','50pQS6bQnU','wUomZICgOK','pCUZEYEz1c','dg5o6Y3lnu','230.210.34.254'),
(205,'和田明菜',330,'和田明菜',553,227,52,40,63,179,485,141,435,752,735,322,354,320,137,19.78,910,655,42,'A5i1UVlSC8','uybDBYKHku','POEyIouU4X','7p0Kk0BOkb','kfFOGUEfmn','12.249.9.151'),
(206,'錢天榮',314,'錢天榮',265,112,74,204,114,188,801,28,891,681,585,316,308,487,426,579.95,35,122,632,'tu4Ztz7Y9y','l30MHPViRa','fQdgEsKqY7','B20gQ3me9B','8Zs4cjCEoF','255.121.232.230'),
(207,'文梓晴',943,'文梓晴',649,184,229,128,15,232,774,93,284,389,921,374,822,483,686,820.73,53,78,669,'JPVHpFVWlO','hSrTjRWb3s','lGufEGSUgZ','uDaSbkaeeK','59IrWotFRt','177.241.138.250'),
(208,'小山光莉',510,'小山光莉',879,91,66,95,104,6,477,248,689,188,21,746,459,260,993,85.46,521,510,794,'cGykug66Ap','190SfAkfJn','CydqfrrD0e','ExjoOK8cPJ','QjPKVQK2LK','6.248.178.192'),
(209,'萬淑怡',378,'萬淑怡',920,143,88,2,37,247,674,209,918,325,618,289,898,923,882,253.44,715,637,908,'glBVpQRWDn','xqigUuMFhw','okMCHkeXJa','I83DsKj8vz','UT4xpiUcaF','124.255.8.89'),
(210,'曾子异',12,'曾子异',349,12,246,202,118,33,323,37,963,891,563,877,36,713,103,672.17,242,839,373,'5obfe2DLEQ','3R8t8Uzl4G','92YjOxsfpK','HGZ75VVjl6','vlNuFP4Ilf','83.255.103.224'),
(211,'林子异',757,'林子异',317,152,22,218,136,183,257,94,370,260,956,412,263,6,997,379.63,995,420,66,'c4yfvEGyzu','C38PylOOtk','gns0Ec88Dm','YpRvI6C7em','O0GCy14xkK','59.252.3.216'),
(212,'岩崎陽太',347,'岩崎陽太',393,201,45,48,204,50,329,16,978,727,496,901,766,104,395,111.17,977,455,465,'cRUsEIc5ZO','cG6YGRGcID','DIiOqxrlW1','GuOW1geCYj','VWEL8ynrTq','2.254.43.10'),
(213,'郭宇宁',394,'郭宇宁',477,68,132,208,73,23,339,12,448,348,400,540,387,967,137,440.90,883,250,402,'DkotrIl7Cq','JwyXjZbxYL','Utjvv83p49','UQpghgqtHn','ld17DFHPWa','55.255.189.211'),
(214,'渡辺陸',740,'渡辺陸',247,67,166,16,21,196,620,77,982,844,204,452,28,82,807,833.40,970,75,285,'edZnLdvD5H','7qo45X4W0G','Wr68tjrfwq','QIST9PT1mU','Qrit94cmBn','9.188.128.212'),
(215,'村上湊',794,'村上湊',970,5,254,169,129,205,294,156,792,383,573,185,716,709,641,485.34,591,985,46,'wYLLUs6kIf','aLIpfugquI','EMVUCZSnIi','3AURGqHWJu','E3VGqJnKIO','231.255.250.255'),
(216,'阮祖兒',635,'阮祖兒',512,230,40,30,202,202,491,51,167,229,956,794,116,565,876,339.86,740,99,527,'GlMhIOhyUQ','9uVIITqQB0','K474C9TrG5','ivjHvDpmaf','BluJZZ16Mi','32.215.3.254'),
(217,'河野彩乃',27,'河野彩乃',138,94,69,66,72,11,376,20,682,722,534,616,746,957,308,636.36,21,279,248,'INCTJ0NyxY','Aj5HLrk2HQ','yHhiKGf2aN','6b0OTMPhWB','fD41FnZq8q','0.252.250.162'),
(218,'任浩然',413,'任浩然',116,111,88,128,14,158,152,39,613,220,335,747,510,100,673,90.05,370,509,762,'tffLDgl7PM','WQAmsbjrom','whuz66k149','k9fyR8Ivto','Kz9Nj3ygUb','251.205.234.250'),
(219,'増田蓮',474,'増田蓮',528,185,208,98,207,71,308,247,7,1,210,268,420,899,487,829.45,384,704,25,'Ue54I8ABLC','zkTfBmZ25G','2P6yyaaThZ','CmRJoRY3OK','TTIFVta8RI','5.136.6.180'),
(220,'徐子韬',169,'徐子韬',597,197,37,117,43,137,426,47,220,500,385,658,802,683,68,500.27,234,521,911,'lFLfYlRB93','Hc6t7a1KtK','twWcgsasUC','xLMWHmDQ6E','zUAVvBK3tI','198.2.234.25'),
(221,'杜心穎',651,'杜心穎',894,160,223,80,84,121,99,195,790,683,669,190,108,944,982,733.31,509,505,142,'U6SkHfA80z','ojM4ySQVZz','iSaOCvanIZ','8MsPmqDnx4','lEEgtuldRy','104.0.166.6'),
(222,'孫天樂',115,'孫天樂',153,128,49,163,202,104,735,41,176,509,710,268,832,71,706,83.33,828,408,650,'kAscOnk9vq','RrzJRTeCFE','QAIcgDBpL2','yRhNqZbkmp','d1V5xLDAiF','86.253.125.8'),
(223,'太田彩乃',985,'太田彩乃',716,135,221,141,197,91,697,37,25,48,947,673,409,206,771,3.14,680,439,534,'k8aUyfKXGC','okApB4mEFK','g6b1AZLPzx','EJXFaQAvY8','w5lwhJ8y8R','60.3.118.255'),
(224,'熊杰宏',779,'熊杰宏',879,33,36,185,154,101,60,45,397,714,853,575,666,694,47,763.06,918,671,848,'EiOaAMztyV','0yFJXCfzNa','2p8KGDX3WU','0WpTX8HDNg','wWfZQ37xy2','48.36.228.255'),
(225,'戴致远',464,'戴致远',119,164,173,30,53,121,628,34,281,538,507,736,680,683,212,703.23,987,626,653,'ewp9WimGH2','noIx6hJ42k','lphsJ4U522','mBJg2yZrsJ','5JMoMI7KWC','101.1.3.22'),
(226,'李璐',194,'李璐',477,141,155,123,51,191,620,109,202,150,662,109,361,647,350,886.27,803,859,111,'onRX6wNI2G','mIDlzqw4QR','X0itrUz42L','maxUrlsVSO','xpXnFkuFqV','165.6.155.11'),
(227,'梅仲賢',102,'梅仲賢',728,185,207,9,164,81,592,151,281,342,848,620,558,36,592,7.28,899,388,487,'KmJyuomGqK','Jo8xLa2xWY','vBr0bYuPkm','SPFM1XqK0E','HYGGGtdO04','56.59.228.251'),
(228,'大塚優奈',59,'大塚優奈',903,72,35,159,71,16,176,85,666,434,2,221,404,514,59,401.75,857,176,124,'lJrrhmrDOY','4EG08FMNau','Nf44FVXFNM','cjUcwQH7GV','L6JM808vhw','9.251.18.254'),
(229,'李國賢',882,'李國賢',255,24,252,228,77,246,548,51,325,519,21,763,671,747,295,166.64,198,92,255,'347TTDiB3l','rKPlGFblpC','sUSWPkrOOG','AIiLJPcYsF','nHStFBnnYX','222.242.253.99'),
(230,'邱致远',663,'邱致远',331,211,77,115,88,39,399,106,455,313,471,166,433,616,537,91.86,910,316,421,'usIhEHXpja','lJ1uWS8Xhh','2pwmhb7RN0','mP7TrWtc65','SJh0Mjqjrh','197.66.181.192'),
(231,'梅安娜',618,'梅安娜',915,181,161,174,165,222,995,224,948,652,618,94,932,704,71,538.72,699,408,567,'3kH1rSfrJM','ZVmcnKkL0N','5yLO4fi6hL','4V0RVPE8yt','DcFLFBcu9L','2.254.0.252'),
(232,'程云熙',874,'程云熙',788,73,37,200,73,235,709,225,926,780,883,60,84,704,207,188.38,759,561,167,'HPgFBVRf01','wMZ8ofxl94','4EsQhF65ZH','LNw3CzJ07E','2Pbdshxlk0','214.6.132.4'),
(233,'新井花',241,'新井花',202,189,128,241,102,122,23,183,77,892,54,411,477,541,425,336.83,452,203,200,'9yuSNBFa1P','RS9ZEf6s6l','o1aSro6x6q','r04mhZcJHp','bTHaKn1Zl0','254.62.227.65'),
(234,'毛心穎',702,'毛心穎',58,78,250,231,4,184,43,186,608,991,658,851,555,972,169,896.16,233,461,289,'Fwb6lVudKj','qtXZ2x3fiu','xW2HAMvPmE','B7hBreqLXQ','SiXxhCUtqr','55.8.3.236'),
(235,'罗子韬',439,'罗子韬',370,91,99,205,81,149,6,252,105,768,623,492,731,786,523,873.08,567,988,788,'vbziExj2WD','G1IBGcGnIW','RXm0KGySkE','ZLXIi62C1x','7T4pdRJ5K5','236.230.4.9'),
(236,'竹内彩乃',517,'竹内彩乃',334,202,101,83,116,138,349,130,700,447,770,38,707,621,639,311.78,457,379,428,'D9ILzBdzsI','d6iAlF688a','I6kXyf1Mgo','dw820xD5Oo','RjV0UrE0NN','228.9.228.203'),
(237,'秦睿',694,'秦睿',500,15,219,108,213,180,1,65,592,82,25,564,708,450,175,14.94,222,59,177,'zrav2xwl6P','GRcklHmRmf','VaRHIJAumI','vsIOGh3qDp','614UWWbji8','216.251.4.77'),
(238,'木下愛梨',63,'木下愛梨',44,183,238,2,122,224,143,224,86,302,755,442,277,511,495,657.75,2,436,10,'Bd2FLwuj8s','IGJqyDrYbR','UKPxE0tIep','u9ymZZw8Ce','amYK04c2jh','221.252.160.4'),
(239,'桜井大輔',587,'桜井大輔',944,172,144,20,248,7,36,161,455,340,157,443,479,659,585,562.29,361,283,867,'iRQA7VzzLW','8e9ib3h2CT','fhjsVV2Mx7','EKmznjyUut','erfbeoidEl','75.0.30.250'),
(240,'崔富城',770,'崔富城',171,48,83,12,44,132,690,216,429,722,336,613,356,136,494,601.00,966,994,241,'Fh77MMklAl','gfA1oqL0W7','Muz4pY138n','ySJAHoEvv9','hCBEA6dxxK','5.109.24.245'),
(241,'岡本百恵',760,'岡本百恵',755,116,237,29,102,89,452,17,303,280,314,935,167,474,589,696.94,324,245,356,'PX6jRXdzZ6','axaQNIcI7U','ErD3Bb4M5U','fWm8auy8uv','KuHGe5HDmX','208.97.89.251'),
(242,'曹嘉伦',867,'曹嘉伦',507,40,125,235,156,61,402,39,579,724,621,531,556,505,343,423.15,169,181,512,'4l4rndnbx9','IwAOKfKPe7','v0yNTdS3Zp','UvVzNoRg1L','uQiPRHGl7B','86.197.116.49'),
(243,'冯璐',776,'冯璐',172,183,193,52,127,231,198,213,782,133,187,187,40,271,799,830.33,930,518,407,'7DJq6mnWiO','PFYtKMh7F0','8sl2lqeeDy','8i04ZbJaiW','eDYb0UYWtK','213.206.231.75'),
(244,'井上聖子',550,'井上聖子',996,243,67,158,47,173,911,22,994,280,780,437,123,399,673,723.02,342,20,352,'yawuQN6vpN','FiZHIo2BFc','4Xz8pMoSy4','b7PhizIdjX','752grrKbnE','141.254.144.53'),
(245,'桜井結翔',369,'桜井結翔',399,76,174,220,17,115,686,196,326,180,625,12,129,588,100,62.85,794,30,313,'3vtMA5LxDZ','iTqj9U5V4G','d3Xq1OnHQw','NzsZp8xxk4','XzVYtlZd7j','102.61.151.191'),
(246,'伊藤玲奈',668,'伊藤玲奈',702,91,66,163,48,15,867,56,712,211,332,819,829,132,584,610.80,780,246,139,'GTp4GsVvxK','CwiPdqCVW5','IuKM014rsj','2aEBcsV7iz','dxSCimnjDs','254.60.0.201'),
(247,'邹岚',961,'邹岚',759,90,70,133,108,199,821,224,36,622,350,884,585,195,983,794.84,824,285,980,'4905gUGiUO','505kbKba1W','a9gXthdbnY','7lJUHOe1k5','i6f0cH2uYA','236.255.253.1'),
(248,'青木美咲',397,'青木美咲',983,6,20,23,40,64,10,41,288,560,66,115,313,154,661,719.97,616,869,939,'akmm6oo5lu','gADyp2we4M','HsLvQuydoG','MjrN6ZFTvN','rIjVPcawIw','11.136.1.103'),
(249,'甘明詩',221,'甘明詩',310,43,145,106,201,30,360,253,542,987,346,237,24,59,543,233.21,750,498,51,'cQZyIEg8m7','BkhTTqxIr1','S5LKprgFNi','4jXQ4EdRDw','Kk9Fv2dRqZ','14.251.254.224'),
(250,'藤井彩乃',28,'藤井彩乃',878,135,141,188,47,172,379,230,43,970,294,11,134,888,613,50.88,557,141,781,'9vAEz7uxEK','UpwthqCtKE','QMBuXRbyuh','rCF4wpJF9k','AasrWROmdo','129.173.246.2'),
(251,'原和真',686,'原和真',264,139,42,15,11,61,657,89,194,931,589,938,306,749,323,55.99,285,192,132,'yoyBtepgwB','4IHL3FSirc','zoiUVb08iR','4uIFiFDuuN','Vq45Ga7e4G','47.150.0.252'),
(252,'阎子韬',671,'阎子韬',196,213,33,105,180,117,329,25,196,313,87,834,503,769,200,776.18,151,807,886,'4yGiBBTQdz','e3c2pisAyq','oWrvdRAFAA','by1G8lJeq1','LeUPPKHqyU','251.197.5.139'),
(253,'佘學友',510,'佘學友',49,210,211,62,186,107,212,93,203,749,226,434,176,139,312,940.85,525,713,479,'7g0qTaVtL7','nXRbsGHTfN','vN49jSvIvG','7MInHTce6a','GdEPyhGE4x','108.254.139.57'),
(254,'高橋光',611,'高橋光',122,184,157,74,12,120,670,66,646,712,180,292,78,183,147,291.53,812,196,376,'D12tph7UVV','3gQQ7pSvPO','NMCkzDDpOn','E313f8e2Gk','CbAmcBS5tg','205.198.146.3'),
(255,'曹頴思',725,'曹頴思',579,83,1,129,178,147,151,235,29,176,765,429,952,937,680,117.56,194,615,370,'p38z4bQYfZ','rrzguIoNgE','zKIyCiSFx2','aq2N7GrFcb','aANthaHxMQ','210.254.112.32'),
(256,'黎惠妹',861,'黎惠妹',345,41,102,70,32,121,355,178,371,830,309,54,519,756,652,933.18,318,601,733,'Nvg84kotXC','ZaEraVlFNL','aZbjCLXvJQ','p7aN6bayng','pewsqPDqFi','236.8.251.255'),
(257,'應心穎',791,'應心穎',458,114,6,139,20,112,802,233,764,700,467,958,175,346,193,103.40,712,657,942,'osltewU2BD','XZdzzbhlHG','fwK4oGZmuR','ctm1e89Hx5','iPhT025MQc','8.75.239.220'),
(258,'邓嘉伦',781,'邓嘉伦',941,254,2,168,21,229,74,1,872,684,266,230,626,258,510,212.96,596,918,621,'oP9ob58d0a','N2djhD15Sf','YDcOwhN2Mf','gxfA2bBKl1','Ju299b1clf','250.193.148.186'),
(259,'莫杰宏',855,'莫杰宏',518,69,178,94,231,118,830,162,238,108,582,729,516,293,9,129.15,747,964,540,'52S1jyx208','bikwkNG7kW','NSAMEI5c5M','jJ0SYBscXN','9KDiD3BV7Q','216.251.255.166'),
(260,'謝霆鋒',626,'謝霆鋒',132,26,57,121,249,67,464,80,264,911,526,456,702,232,595,988.92,617,817,886,'9MqzkYvRFS','p8lkECeBVE','OvXpu8qTYf','G8Syhod9Ga','bAsM3Ad3cA','192.7.251.216'),
(261,'古永權',330,'古永權',850,61,251,194,0,174,814,208,519,750,136,124,761,227,794,890.98,495,377,232,'ShyD76g6Uo','1Et2XIIYCh','riFWZGPcf5','4Mtp2Y0tPJ','2DjDUhWPOk','19.254.162.79'),
(262,'岡田健太',63,'岡田健太',973,129,74,38,227,203,808,223,619,42,747,997,128,715,793,69.34,736,988,106,'NVKuk0sImD','LcMoWJEt9q','dcMjJkAPLT','TNkHYJDMRC','E8q1qQb0q9','1.46.251.5'),
(263,'小島葵',617,'小島葵',800,166,122,221,170,202,686,102,28,935,804,211,658,888,209,625.57,24,109,272,'2SxTnpf6IP','pPa7941hrT','69WBZUZne9','fFRrRXYPMt','vdUsBhvgtc','253.250.5.90'),
(264,'龙杰宏',348,'龙杰宏',766,151,23,154,170,228,252,19,959,668,983,376,27,349,861,675.70,149,990,813,'8iW0Ck0bA9','cZwDLPD7tL','SejBq0uRqc','1kKCtjsPzs','36RlXw2RzH','7.1.25.222'),
(265,'太田悠人',197,'太田悠人',135,163,84,243,93,14,945,199,760,694,401,963,9,584,923,50.78,656,834,686,'bnvops6jwH','pONHin9BcL','W5r7AvdBgt','ul0qr4mJj2','W0JfzXz3RJ','252.234.16.52'),
(266,'菊地拓哉',40,'菊地拓哉',704,122,211,1,22,200,938,168,505,702,590,385,640,213,293,921.90,669,121,816,'GlaY8DgK9c','4SrRa8DQtl','0f6V5O42gh','7O2piqJvKB','E0iA8D6YOi','126.238.252.255'),
(267,'成家文',538,'成家文',514,145,61,49,1,63,971,11,40,162,625,388,397,964,36,778.93,870,265,486,'YuThesXqJ6','zGdRZrTV2M','2YTbyzRsmq','kkXmGj5XkH','DcQDJa8DUq','223.239.207.239'),
(268,'伍力申',864,'伍力申',474,132,35,92,169,167,908,133,987,57,624,506,880,7,998,826.15,401,750,699,'mNTGghsRtW','LP5UioQkoK','OCbavCgcuR','CPlLiKz6mX','sscLUF7Iq1','61.206.3.99'),
(269,'陈子异',98,'陈子异',230,128,28,253,247,127,31,169,770,82,268,431,948,264,758,813.23,964,780,789,'zXwShAcyPz','FDLfOygSdA','ExOEWMd8Ch','TFcCXn3J4k','xVEAmtf8nl','28.138.26.255'),
(270,'高橋光莉',253,'高橋光莉',7,172,183,246,42,20,562,216,361,492,123,802,423,491,638,955.52,63,259,504,'rELNOBxuJT','gpaowZTVEE','ccQf1ViOUi','jgMjlF44GA','xW7coxcODY','152.207.125.88'),
(271,'廖國賢',608,'廖國賢',305,55,136,138,196,136,148,166,938,779,741,350,407,416,161,234.70,910,331,969,'T4tDQkPFT7','i8328kG8Il','A9xsLCTyte','QbunUcNZnP','OdUygnyoxn','34.242.211.253'),
(272,'千葉明菜',429,'千葉明菜',557,245,92,213,154,236,113,12,852,855,375,808,821,538,441,873.77,525,980,368,'uGkrentsIv','vwxTPtU71g','PyLCC7D8f0','ks09Zznhd2','tAbfiS1OIE','241.217.4.1'),
(273,'莫學友',892,'莫學友',360,52,238,244,11,181,410,163,893,199,607,249,398,212,350,372.53,480,956,183,'VldqvWINrO','lD8c9JLg1L','4ZJZu2Jf9P','R6VigbGCCa','94JSixokU0','0.37.7.224'),
(274,'冯安琪',374,'冯安琪',326,128,2,215,141,123,807,71,418,551,917,548,462,236,537,802.11,893,281,442,'Y2xvlC0JRW','sGzDtA3iG9','WtJkG5MTmt','sYwFikXp7y','94QHzF9746','3.109.252.162'),
(275,'藤井海斗',243,'藤井海斗',854,107,186,32,209,252,376,79,908,657,394,674,733,879,531,430.28,181,234,795,'6pDMDIVVbP','iUopgkemmj','e8MXKhcdiw','L2kUjDZt9R','qtQzzWTl2v','12.31.7.0'),
(276,'孟嘉伦',542,'孟嘉伦',153,85,83,147,244,207,390,63,21,526,260,577,874,506,980,603.66,44,343,362,'HXlHP2S5s7','id533O4uXX','QSe7nATr4K','g8Km9oBRdI','OVRL3UCGLk','131.127.83.206'),
(277,'尹致远',246,'尹致远',831,189,121,96,77,191,21,91,789,628,686,23,117,162,972,929.50,738,483,932,'rQ0oZOqa1m','sshARXgubj','Qaf7skEjqt','0Nc91FRYTT','ujBp6KUeBP','145.0.220.156'),
(278,'沈晓明',763,'沈晓明',975,176,154,194,57,225,796,12,675,319,955,81,173,483,290,927.85,175,5,89,'6m7i79knUl','AZHRTBi0yK','TYFrFsUVn9','cKacquiLdn','NuHeGVuaCl','216.247.168.146'),
(279,'樊祖兒',49,'樊祖兒',574,179,116,103,51,73,757,155,473,666,226,241,838,238,701,784.08,54,999,361,'s6q81C7V6F','UsybmUC2Ux','fQUuJcmhYn','y4N9689Jpt','J2q1fN5yBG','68.130.33.253'),
(280,'段宇宁',685,'段宇宁',125,173,158,194,100,147,819,74,89,804,152,172,154,364,157,195.32,696,364,359,'ZOL9RWuxxj','DRUisMcppU','BBWRa7GJvu','23cn5meTX5','ehskXhi4Yj','137.252.236.6'),
(281,'伍明',533,'伍明',145,81,95,178,82,154,351,176,230,151,530,793,952,288,940,90.86,965,887,641,'hz1VunyNed','J6LvG8vrEp','i3PUHNOhKf','84NHZHDcna','UQTBHuffVz','244.163.0.255'),
(282,'大野紗良',912,'大野紗良',414,62,173,198,14,207,953,47,21,754,188,851,943,782,772,145.93,204,191,965,'BSWl2n4zQb','20UtYm7RR8','lSF1XiNuUY','92ggrDgoRK','mv7qnWAR00','170.253.82.3'),
(283,'内田陽太',813,'内田陽太',945,171,213,93,59,14,296,233,371,765,468,680,944,353,462,676.00,245,448,73,'3wYHudqkfn','jyhOWEZnJR','YIay7F6SvA','zTuKDdEN1W','fQtDTxVdmQ','0.78.13.5'),
(284,'宋子韬',511,'宋子韬',299,30,184,138,19,85,112,45,876,289,691,429,179,490,242,361.44,266,475,614,'UNsjFaTijf','t6SPOwDlk2','5dYaHlEnvl','BAYpQsqE8G','1nB7tFSdDU','99.204.5.6'),
(285,'黎朝偉',596,'黎朝偉',875,139,220,53,55,40,161,112,638,162,196,533,198,644,584,700.31,393,723,957,'2jphRy22mH','wbUg2rNVWv','g9sGohdRyE','kj8yx98F5W','TNm3wDfoFJ','251.164.56.25'),
(286,'田俊宇',295,'田俊宇',210,75,233,37,187,159,96,48,808,965,561,341,679,468,848,583.18,411,90,480,'ICOgUzEWHu','OHAWjnEAyW','Kf1XRHYEiU','TlPxt4gDNc','wCzi3yXzX7','20.205.76.60'),
(287,'龚嘉伦',95,'龚嘉伦',98,115,134,115,72,101,362,2,838,80,175,523,135,655,629,239.05,490,223,139,'EwG2DUoaCd','EH1FqWJ657','CTOtITOxiJ','JkXiTQrhHJ','dblrtZPtK5','221.112.78.84'),
(288,'湯發',160,'湯發',305,233,122,133,143,186,485,178,991,332,53,881,741,952,977,800.13,367,163,340,'vKkhGG1BrB','Wq3ZrDnczX','VjkPbqvx6E','3jBFTcrGFd','HzmWlrMcO7','133.250.219.250'),
(289,'潘秀英',894,'潘秀英',788,193,36,174,154,53,541,155,555,306,527,319,550,424,15,177.79,401,258,546,'uYNDbsr0Zd','luu0A35ZJR','7zkr5fk9MJ','hFqqKFItY7','UEWeHVOxFT','83.32.201.164'),
(290,'杜永權',110,'杜永權',485,96,186,34,49,102,609,171,174,521,957,331,808,753,212,713.88,430,668,966,'278J8cX00j','fJMqyYqQAM','gYeT1c7RyP','8bARgbHzNW','MaKP7NLmjw','252.255.91.115'),
(291,'洪慧嫻',220,'洪慧嫻',884,20,211,20,176,76,54,210,920,839,334,425,929,826,983,503.74,32,229,49,'4rtFuI7yEa','QjgVmVVagZ','18hjzXjBHS','BGoIii4wHw','huzdr3eb9l','1.145.152.228'),
(292,'官嘉欣',242,'官嘉欣',786,227,174,71,212,18,727,138,847,288,93,772,703,625,461,161.61,907,400,336,'WFn0MNJq0O','geCMekTkKR','mMeWURRKWS','S9P25UXaDI','sH3IyYDMkP','4.254.133.52'),
(293,'小山大輔',790,'小山大輔',377,225,211,240,49,161,931,57,599,124,793,789,589,390,456,188.70,89,649,457,'bJJ8WZT0pi','2fErk3Xj8M','uvgPziwwtq','xdlvRhTkKG','bs6OQXBoLO','251.109.4.222'),
(294,'鄧潤發',746,'鄧潤發',705,88,26,248,153,224,609,99,327,727,722,752,569,97,357,296.25,410,308,114,'Dzot35jxg1','YrJCzU8oBa','KE5h1sH0Cv','VFvJBRNqj1','IOMhu56SMI','67.255.64.17'),
(295,'福田陽菜',534,'福田陽菜',617,166,38,182,36,113,53,100,279,812,853,632,35,90,176,642.14,734,721,629,'w3VNRn13UB','DpzciOjqQp','KfqI71CN5t','ZCsTkNDIPD','TqZkZl3u8Y','41.5.118.5'),
(296,'渡部美咲',734,'渡部美咲',790,33,179,240,55,14,820,251,524,985,171,765,314,940,905,367.27,298,704,515,'YVwQ4Ao429','OTl2MVM92V','po8dYeiKHl','WNKXCE7YCJ','Jzth9zOw6W','155.87.73.51'),
(297,'宣麗欣',54,'宣麗欣',543,181,110,89,253,9,408,137,32,886,680,703,461,228,666,939.06,177,637,289,'1EpvKUK8rK','psSmClkocN','hmSAF00JHG','drC0BTBJ6h','skinsDU9QG','3.200.7.81'),
(298,'吉田悠人',474,'吉田悠人',818,100,190,128,218,181,447,88,246,522,150,266,898,565,963,510.79,984,698,750,'0H2LLUl2Yn','npnWmuZ2EV','Nib1oRRhJJ','DkFMLz2RU9','wVrdpP1IPy','114.133.4.199'),
(299,'松田光莉',339,'松田光莉',304,218,221,135,90,212,902,249,399,549,236,340,754,177,435,454.29,952,655,919,'5mEgUFgdos','cBeCwbd8wa','Ojj4kr09gf','hfoucinn1y','7IO4XrxzTP','118.8.222.171'),
(300,'岡田花',529,'岡田花',850,203,79,217,106,131,22,33,304,477,881,747,219,268,235,10.06,371,501,585,'cuwdGsvDaA','smEX4BtBaP','L5wBKTUkNh','anFJZ7yyhH','BuCtxt7X6C','126.103.9.130'),
(301,'石子异',636,'石子异',713,103,82,245,168,200,936,179,795,883,492,499,154,621,742,568.65,293,237,384,'7dM7BUYqCV','vCkAhoMziv','PBjqVmC1XE','cTsws8o1KN','K471ZXQFo1','165.80.1.252'),
(302,'安藤架純',901,'安藤架純',692,84,114,167,18,245,224,207,187,126,707,407,495,481,656,850.67,206,732,977,'67yb6EWZ6z','NvUDKwmkQW','ZYhR3SP334','o8KClakkbv','jzMQb04jp1','8.201.103.229');
/*!40000 ALTER TABLE `role` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `role_log`
--

DROP TABLE IF EXISTS `role_log`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `role_log` (
  `id` bigint(20) unsigned NOT NULL AUTO_INCREMENT COMMENT 'ID',
  `role_id` bigint(20) unsigned NOT NULL DEFAULT 0 COMMENT '角色ID',
  `exp` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '经验',
  `time` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '时间',
  PRIMARY KEY (`id`) USING BTREE,
  KEY `role_id` (`role_id`) USING BTREE,
  KEY `time` (`time`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci ROW_FORMAT=DYNAMIC COMMENT='角色日志表';
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `role_log`
--

LOCK TABLES `role_log` WRITE;
/*!40000 ALTER TABLE `role_log` DISABLE KEYS */;
/*!40000 ALTER TABLE `role_log` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `role_notice`
--

DROP TABLE IF EXISTS `role_notice`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `role_notice` (
  `role_id` bigint(20) unsigned NOT NULL DEFAULT 0 COMMENT '角色ID(select_by_role_id)',
  `notice_id` bigint(20) unsigned NOT NULL DEFAULT 0 COMMENT '公告ID(join_on(`notice`.`notice_id`))',
  `receive_time` int(10) unsigned GENERATED ALWAYS AS (0) VIRTUAL COMMENT '接收时间(join(`notice`.`receive_time`))',
  `expire_time` int(10) unsigned GENERATED ALWAYS AS (0) VIRTUAL COMMENT '过期时间(join(`notice`.`expire_time`))',
  `read_time` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '读取时间(update_read)',
  `title` char(255) GENERATED ALWAYS AS ('') VIRTUAL COMMENT '标题',
  `content` char(255) GENERATED ALWAYS AS ('') VIRTUAL COMMENT '内容',
  `flag` tinyint(3) unsigned GENERATED ALWAYS AS (0) VIRTUAL COMMENT '标识(flag)',
  PRIMARY KEY (`role_id`,`notice_id`) USING BTREE,
  KEY `notice_id` (`notice_id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_general_ci COMMENT='角色公告表';
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `role_notice`
--

LOCK TABLES `role_notice` WRITE;
/*!40000 ALTER TABLE `role_notice` DISABLE KEYS */;
/*!40000 ALTER TABLE `role_notice` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `sensitive_word_data`
--

DROP TABLE IF EXISTS `sensitive_word_data`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `sensitive_word_data` (
  `word` varchar(255) NOT NULL DEFAULT '' COMMENT '敏感词',
  PRIMARY KEY (`word`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci ROW_FORMAT=DYNAMIC COMMENT='敏感词配置表';
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `sensitive_word_data`
--

LOCK TABLES `sensitive_word_data` WRITE;
/*!40000 ALTER TABLE `sensitive_word_data` DISABLE KEYS */;
/*!40000 ALTER TABLE `sensitive_word_data` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `sex_data`
--

DROP TABLE IF EXISTS `sex_data`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `sex_data` (
  `sex` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '物品配置ID',
  `name` char(255) NOT NULL DEFAULT '' COMMENT '资产类型',
  PRIMARY KEY (`sex`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci ROW_FORMAT=DYNAMIC COMMENT='性别配置表';
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `sex_data`
--

LOCK TABLES `sex_data` WRITE;
/*!40000 ALTER TABLE `sex_data` DISABLE KEYS */;
INSERT INTO `sex_data` VALUES
(1,'男'),
(2,'女');
/*!40000 ALTER TABLE `sex_data` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `shop`
--

DROP TABLE IF EXISTS `shop`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `shop` (
  `role_id` bigint(20) unsigned NOT NULL DEFAULT 0 COMMENT '角色ID(select_by_role_id)',
  `shop_id` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '商店ID',
  `number` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '数量',
  `flag` tinyint(3) unsigned GENERATED ALWAYS AS (0) VIRTUAL COMMENT '标识(flag)',
  PRIMARY KEY (`role_id`,`shop_id`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci ROW_FORMAT=DYNAMIC COMMENT='角色商店表';
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `shop`
--

LOCK TABLES `shop` WRITE;
/*!40000 ALTER TABLE `shop` DISABLE KEYS */;
/*!40000 ALTER TABLE `shop` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `shop_data`
--

DROP TABLE IF EXISTS `shop_data`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `shop_data` (
  `shop_id` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '商店ID',
  `item_id` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '物品配置ID',
  `type` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '商店类型',
  `pay_asset` enum('coin','copper','exp','gold','silver') NOT NULL COMMENT '货币类型(validate(asset))',
  `price` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '价格',
  `number` int(10) unsigned NOT NULL DEFAULT 1 COMMENT '数量',
  `level` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '等级限制',
  `limit` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '购买上限',
  `vip_level` int(10) unsigned NOT NULL DEFAULT 0 COMMENT 'vip等级限购',
  `vip_limit` varchar(255) NOT NULL DEFAULT '' COMMENT 'vip等级购买上限',
  `description` char(255) NOT NULL DEFAULT '' COMMENT '描述',
  PRIMARY KEY (`shop_id`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci ROW_FORMAT=DYNAMIC COMMENT='商店配置表';
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `shop_data`
--

LOCK TABLES `shop_data` WRITE;
/*!40000 ALTER TABLE `shop_data` DISABLE KEYS */;
INSERT INTO `shop_data` VALUES
(1,1,1,'gold',10,1,0,10,0,'[]','');
/*!40000 ALTER TABLE `shop_data` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `shop_log`
--

DROP TABLE IF EXISTS `shop_log`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `shop_log` (
  `id` bigint(20) unsigned NOT NULL AUTO_INCREMENT COMMENT 'ID',
  `role_id` bigint(20) unsigned NOT NULL DEFAULT 0 COMMENT '角色ID',
  `shop_id` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '商店ID',
  `number` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '购买数量',
  `time` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '时间',
  PRIMARY KEY (`id`) USING BTREE,
  KEY `role_id` (`role_id`) USING BTREE,
  KEY `time` (`time`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci ROW_FORMAT=DYNAMIC COMMENT='商店日志表';
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `shop_log`
--

LOCK TABLES `shop_log` WRITE;
/*!40000 ALTER TABLE `shop_log` DISABLE KEYS */;
/*!40000 ALTER TABLE `shop_log` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `sign`
--

DROP TABLE IF EXISTS `sign`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `sign` (
  `role_id` bigint(20) unsigned NOT NULL DEFAULT 0 COMMENT '角色ID',
  `login_day` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '登录天数',
  `sign_total` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '签到总数',
  `is_sign_today` tinyint(3) unsigned NOT NULL DEFAULT 0 COMMENT '今天是否签到',
  PRIMARY KEY (`role_id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci COMMENT='角色签到表';
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `sign`
--

LOCK TABLES `sign` WRITE;
/*!40000 ALTER TABLE `sign` DISABLE KEYS */;
/*!40000 ALTER TABLE `sign` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `sign_active_data`
--

DROP TABLE IF EXISTS `sign_active_data`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `sign_active_data` (
  `day` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '累计天数',
  `pre_day` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '前一个阶段ID',
  `next_day` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '下一个阶段ID',
  `score` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '积分',
  PRIMARY KEY (`day`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci COMMENT='签到活跃奖励配置';
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `sign_active_data`
--

LOCK TABLES `sign_active_data` WRITE;
/*!40000 ALTER TABLE `sign_active_data` DISABLE KEYS */;
INSERT INTO `sign_active_data` VALUES
(2,0,5,1),
(5,2,7,2),
(7,5,10,3),
(10,7,15,4),
(15,10,20,5),
(20,15,22,6),
(22,20,25,7),
(25,22,28,8),
(28,25,30,9),
(30,28,0,10);
/*!40000 ALTER TABLE `sign_active_data` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `sign_data`
--

DROP TABLE IF EXISTS `sign_data`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `sign_data` (
  `day` tinyint(3) unsigned NOT NULL DEFAULT 0 COMMENT '签到天数',
  `award` varchar(255) NOT NULL DEFAULT '' COMMENT '奖励',
  PRIMARY KEY (`day`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci COMMENT='签到配置表';
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `sign_data`
--

LOCK TABLES `sign_data` WRITE;
/*!40000 ALTER TABLE `sign_data` DISABLE KEYS */;
INSERT INTO `sign_data` VALUES
(1,'[{1,1}]'),
(2,'[{2,1}]'),
(3,'[{3,1}]'),
(4,'[{4,1}]'),
(5,'[{5,1}]'),
(6,'[{6,1}]'),
(7,'[{7,1}]');
/*!40000 ALTER TABLE `sign_data` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `skill`
--

DROP TABLE IF EXISTS `skill`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `skill` (
  `role_id` bigint(20) unsigned NOT NULL DEFAULT 0 COMMENT '角色ID(select_by_role_id)',
  `skill_id` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '技能ID',
  `level` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '等级',
  `flag` tinyint(3) unsigned GENERATED ALWAYS AS (0) VIRTUAL COMMENT '标识(flag)',
  PRIMARY KEY (`role_id`,`skill_id`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci ROW_FORMAT=DYNAMIC COMMENT='角色技能表';
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `skill`
--

LOCK TABLES `skill` WRITE;
/*!40000 ALTER TABLE `skill` DISABLE KEYS */;
/*!40000 ALTER TABLE `skill` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `skill_data`
--

DROP TABLE IF EXISTS `skill_data`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `skill_data` (
  `skill_id` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '技能ID',
  `type` enum('active','passive') NOT NULL COMMENT '类型(validate(skill_type))',
  `classes` tinyint(3) unsigned NOT NULL DEFAULT 0 COMMENT '职业(validate(classes))',
  `name` char(255) NOT NULL DEFAULT '' COMMENT '名字',
  `condition` varchar(255) NOT NULL DEFAULT '' COMMENT '学习条件',
  `cost` varchar(255) NOT NULL DEFAULT '' COMMENT '升级消耗',
  `attribute` varchar(255) NOT NULL DEFAULT '' COMMENT '属性',
  `effect` varchar(255) NOT NULL DEFAULT '' COMMENT '作用效果',
  `cd` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '冷却时间',
  `radius` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '作用半径',
  `distance` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '作用距离',
  `number` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '作用对象数',
  `buffs` varchar(255) NOT NULL DEFAULT '' COMMENT '作用Buff',
  `before_effects` varchar(255) NOT NULL DEFAULT '' COMMENT '效果前',
  `hit_effects` varchar(255) NOT NULL DEFAULT '' COMMENT '击中效果',
  `after_effects` varchar(255) NOT NULL DEFAULT '' COMMENT '效果后',
  `description` char(255) NOT NULL DEFAULT '' COMMENT '描述',
  PRIMARY KEY (`skill_id`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci ROW_FORMAT=DYNAMIC COMMENT='技能配置表';
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `skill_data`
--

LOCK TABLES `skill_data` WRITE;
/*!40000 ALTER TABLE `skill_data` DISABLE KEYS */;
INSERT INTO `skill_data` VALUES
(1,'active',0,'普攻技能','[]','[]','[]','[1]',1,1000,1000,1,'[]','[]','[]','[]','对目标造成180%的伤害'),
(2,'active',0,'群攻技能','[]','[]','[]','[2]',1,1000,1000,30,'[]','[]','[]','[]','对3个目标造成150%的伤害'),
(3,'passive',0,'增益','[]','[]','[]','[8]',10,1,1,1,'[]','[]','[]','[]','每秒扣血，总血量万分之50'),
(5,'active',0,'普攻技能','[]','[]','[]','[1]',1,1,1,1,'[]','[]','[]','[]','普通技能');
/*!40000 ALTER TABLE `skill_data` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `state`
--

DROP TABLE IF EXISTS `state`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `state` (
  `name` char(255) NOT NULL DEFAULT '' COMMENT '名字',
  `value` bigint(20) unsigned NOT NULL DEFAULT 0 COMMENT '数值',
  PRIMARY KEY (`name`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci ROW_FORMAT=DYNAMIC COMMENT='状态表';
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `state`
--

LOCK TABLES `state` WRITE;
/*!40000 ALTER TABLE `state` DISABLE KEYS */;
/*!40000 ALTER TABLE `state` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `task`
--

DROP TABLE IF EXISTS `task`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `task` (
  `role_id` bigint(20) unsigned NOT NULL DEFAULT 0 COMMENT '角色ID(select_by_role_id)',
  `task_id` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '任务ID',
  `type` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '类型',
  `number` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '数量',
  `is_award` tinyint(3) unsigned NOT NULL DEFAULT 0 COMMENT '是否领取奖励',
  `flag` tinyint(3) unsigned GENERATED ALWAYS AS (0) VIRTUAL COMMENT '标识(flag)',
  PRIMARY KEY (`role_id`,`type`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci ROW_FORMAT=DYNAMIC COMMENT='角色任务表';
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `task`
--

LOCK TABLES `task` WRITE;
/*!40000 ALTER TABLE `task` DISABLE KEYS */;
/*!40000 ALTER TABLE `task` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `task_data`
--

DROP TABLE IF EXISTS `task_data`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `task_data` (
  `task_id` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '任务ID',
  `type` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '类型',
  `pre_id` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '前置任务',
  `next_id` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '后置任务',
  `event` enum('event_add_friend','event_dungeon_copper_passed','event_dungeon_exp_passed','event_dungeon_passed','event_friend_add','event_guild_join','event_kill_monster','event_level_upgrade','event_shop_buy') NOT NULL COMMENT '事件(validate(event))',
  `compare` enum('eq','ge','gt','le','lt','nc','ne') NOT NULL COMMENT '比较模式(validate(compare))',
  `target` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '目标',
  `number` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '数量',
  `condition` varchar(255) NOT NULL DEFAULT '' COMMENT '条件(ref(condition))',
  `cost` varchar(255) NOT NULL DEFAULT '' COMMENT '消耗',
  `award` varchar(255) NOT NULL DEFAULT '' COMMENT '奖励',
  `title` char(255) NOT NULL DEFAULT '' COMMENT '标题',
  `content` char(255) NOT NULL DEFAULT '' COMMENT '内容',
  `description` char(255) NOT NULL DEFAULT '' COMMENT '描述',
  PRIMARY KEY (`task_id`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci ROW_FORMAT=DYNAMIC COMMENT='任务配置表';
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `task_data`
--

LOCK TABLES `task_data` WRITE;
/*!40000 ALTER TABLE `task_data` DISABLE KEYS */;
INSERT INTO `task_data` VALUES
(1,1,0,2,'event_kill_monster','nc',0,3,'[{level, 10}]','[]','[{1,1}]','','',''),
(2,1,1,3,'event_level_upgrade','ge',5,1,'[{sex, 1}]','[{100003, 100}]','[{1,10}]','','',''),
(3,1,2,4,'event_dungeon_passed','gt',2,1,'[{level, 10},{classes,2}]','[]','[{1,100}]','','',''),
(4,1,3,5,'event_shop_buy','eq',1,1,'[{vip, 3}]','[]','[{1,1000}]','','',''),
(5,1,4,0,'event_guild_join','nc',0,1,'[{classes, 1},{level, 2},{sex, 3},{vip, 4}]','[]','[{1,1000}]','','',''),
(6,1,5,0,'event_add_friend','nc',0,5,'[]','[]','[{1,10}]','','',''),
(1001,2,0,1002,'event_dungeon_exp_passed','ge',3,1,'[]','[]','[{1,10}]','','',''),
(1002,2,1001,0,'event_friend_add','eq',1,1,'[]','[]','[{1,10}]','','',''),
(100001,3,0,100002,'event_dungeon_copper_passed','eq',1,1,'[]','[]','[{1,10}]','','',''),
(100002,3,100001,0,'event_guild_join','nc',0,1,'[]','[]','[{1,10}]','','','');
/*!40000 ALTER TABLE `task_data` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `task_log`
--

DROP TABLE IF EXISTS `task_log`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `task_log` (
  `id` bigint(20) unsigned NOT NULL AUTO_INCREMENT COMMENT 'ID',
  `role_id` bigint(20) unsigned NOT NULL DEFAULT 0 COMMENT '角色ID',
  `task_id` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '任务ID',
  `time` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '时间',
  PRIMARY KEY (`id`) USING BTREE,
  KEY `role_id` (`role_id`) USING BTREE,
  KEY `time` (`time`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci ROW_FORMAT=DYNAMIC COMMENT='任务日志表';
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `task_log`
--

LOCK TABLES `task_log` WRITE;
/*!40000 ALTER TABLE `task_log` DISABLE KEYS */;
/*!40000 ALTER TABLE `task_log` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `text_data`
--

DROP TABLE IF EXISTS `text_data`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `text_data` (
  `key` varchar(255) NOT NULL DEFAULT '' COMMENT '键',
  `zhCN` char(255) NOT NULL DEFAULT '' COMMENT '简体中文',
  `description` char(255) NOT NULL DEFAULT '' COMMENT '描述',
  PRIMARY KEY (`key`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci ROW_FORMAT=DYNAMIC COMMENT='游戏文本配置表';
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `text_data`
--

LOCK TABLES `text_data` WRITE;
/*!40000 ALTER TABLE `text_data` DISABLE KEYS */;
INSERT INTO `text_data` VALUES
('account_create_max','服务器角色数量已达到上限','文本'),
('account_login_forbidden','账号禁止登录','文本'),
('account_logout','登出','文本'),
('account_not_found','没有找到此账号','文本'),
('account_permission_denied','账号权限不足','文本'),
('achievement_not_completed','成就未完成','文本'),
('achievement_not_found','没有找到此成就','文本'),
('asset_copper_not_enough','铜币不足','文本'),
('asset_gold_not_enough','金币不足','文本'),
('asset_not_enough','资产不足','文本'),
('asset_silver_not_enough','银币不足','文本'),
('auction_not_found','没有找到此拍品','文本'),
('auction_price_changed','拍品价格已发生变化','文本'),
('award_already_received','奖励已经领取过了','文本'),
('award_error','奖励领取错误','文本'),
('award_pre_not_received','前置奖励未领取','文本'),
('boss_dead','BOSS已经死亡','文本'),
('boss_not_found','没有找到此Boss','文本'),
('bubble_duplicated','气泡重复','文本'),
('buff_duplicated','Buff重复','文本'),
('chat_cannot_with_self','不能和自己聊天','文本'),
('chat_too_frequently','发言太频繁','文本'),
('cheat_command_not_found','没有找到此命令','文本'),
('condition_not_met','条件不满足','文本'),
('configure_not_found','没有找到此配置','文本'),
('daily_not_completed','日常任务未完成','文本'),
('daily_score_not_enough','日常活跃度不足','文本'),
('dungeon_not_found','没有找到此副本','文本'),
('dungeon_today_number_limit','今日进入次数已达到上限','文本'),
('fashion_duplicated','时装重复','文本'),
('friend_apply_not_found','没有找到此好友的申请','文本'),
('friend_in_apply','对方已在申请列表中','文本'),
('friend_in_be_block','你已被对方拉黑','文本'),
('friend_in_block','对方已在黑名单中','文本'),
('friend_in_list','对方已在好友列表中','文本'),
('friend_level_not_met','对方好友等级不满足','文本'),
('friend_not_found','没有找到此好友','文本'),
('friend_number_max','好友数量达到上限','文本'),
('guild_already_joined','你已经加入过公会了','文本'),
('guild_apply_frequently','公会申请太频繁','文本'),
('guild_apply_not_found','没有找到此申请','文本'),
('guild_cannot_kick_self','不可剔除自己','文本'),
('guild_cannot_update_self','不可升级自己','文本'),
('guild_create_frequently','公会创建太频繁','文本'),
('guild_member_not_found','没有找到此成员','文本'),
('guild_member_number_limit','公会成员数量已达到上限','文本'),
('guild_not_found','没有找到此商会','文本'),
('guild_not_joined','没有加入公会','文本'),
('guild_permission_denied','公会权限不足','文本'),
('invalid_classes','无效职业','文本'),
('invalid_item','无效物品','文本'),
('invalid_number','无效数量','文本'),
('invalid_sex','无效性别','文本'),
('invalid_type','无效类型','文本'),
('item_bag_full','背包已满','文本'),
('item_cannot_use_directly','物品不能直接使用','文本'),
('item_not_enough','物品不足','文本'),
('item_use_number_max','使用个数超过单次使用上限','文本'),
('key_already_activated','激活码已激活过','文本'),
('key_already_active','此兑换码已经兑换过了','文本'),
('level_not_met','等级不满足','文本'),
('lucky_money_already_received','红包已领取过','文本'),
('lucky_money_expired','红包已过期','文本'),
('lucky_money_not_found','没有找到此红包','文本'),
('mail_already_read','邮件已阅读过','文本'),
('mail_attachment_empty','附件为空','文本'),
('mail_not_found','没有找到此邮件','文本'),
('mail_text_add_item_content','您的背包已满，新增的道具已经放到了邮件里，请注意查收。','背包满内容'),
('mail_text_add_item_title','背包已满','背包满标题'),
('mail_text_auction_income_content','您的拍卖收入分成。','拍卖分红内容'),
('mail_text_auction_income_title','拍卖收入','拍卖分红标题'),
('mail_text_auction_success_content','您的拍卖物品，请注意查收。','拍卖成功内容'),
('mail_text_auction_success_title','拍卖成功','拍卖成功标题'),
('name_duplicate','名字重复','文本'),
('name_duplicated','名字重复','文本'),
('name_length','名字长度不对','文本'),
('name_length_invalid','名字长度无效','文本'),
('name_not_utf8_charset','名字非UTF8字符','文本'),
('name_sensitive','名字敏感','文本'),
('notice_text_guild_create','<id>~w</id>~s创建公会<id>~w</id>~s','创建公会公告'),
('notice_text_level_upgrade','恭喜<id>~w</id>~s升到~w级','升级公告'),
('notice_text_vip_upgrade','恭喜<id>~w</id>~sVip升到~w级','Vip升级公告'),
('packet_heartbeat_too_fast','心跳包速度过快','文本'),
('packet_too_fast','包速度过快','文本'),
('role_cannot_change_same_classes','职业不能相同','文本'),
('role_cannot_change_same_name','名字不能相同','文本'),
('role_cannot_change_same_sex','性别不能相同','文本'),
('server_create_forbidden','服务器禁止创建角色','文本'),
('server_id_mismatch','服务器ID不匹配','文本'),
('server_login_forbidden','服务器禁止登录','文本'),
('server_update','服务器更新','文本'),
('shop_buy_num_max','已达到购买数量上限','文本'),
('signed_already','已经签到过了','文本'),
('task_already_submitted','任务已提交','文本'),
('task_not_completed','任务还没完成','文本'),
('task_not_found','没有找到此任务','文本'),
('task_not_next','请按顺序完成','文本'),
('task_pre_not_completed','前置任务还没完成','文本'),
('timeout','请求超时','文本'),
('title_duplicated','称号重复','文本'),
('user_offline','对方不在线','文本'),
('vip_level_not_met','Vip等级不满足','文本');
/*!40000 ALTER TABLE `text_data` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `title`
--

DROP TABLE IF EXISTS `title`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `title` (
  `role_id` bigint(20) unsigned NOT NULL DEFAULT 0 COMMENT '角色ID(select_by_role_id)(update_role_id)',
  `title_id` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '称号ID(select_by_title_id)',
  `type` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '类型',
  `expire_time` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '过期时间',
  `flag` tinyint(3) unsigned GENERATED ALWAYS AS (0) VIRTUAL COMMENT '标识(flag)',
  PRIMARY KEY (`role_id`,`title_id`) USING BTREE,
  KEY `title_id` (`title_id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci ROW_FORMAT=DYNAMIC COMMENT='角色称号表';
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `title`
--

LOCK TABLES `title` WRITE;
/*!40000 ALTER TABLE `title` DISABLE KEYS */;
/*!40000 ALTER TABLE `title` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `title_data`
--

DROP TABLE IF EXISTS `title_data`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `title_data` (
  `title_id` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '称号ID',
  `type` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '类型',
  `multi` enum('false','true') NOT NULL COMMENT '同类型可否拥有多个(validate(boolean))',
  `is_unique` enum('false','true') NOT NULL COMMENT '是否全服唯一(validate(boolean))',
  `expire_time` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '过期时间',
  `attribute` varchar(255) NOT NULL DEFAULT '' COMMENT '属性',
  `name` char(255) NOT NULL DEFAULT '' COMMENT '称号名字',
  `description` char(255) NOT NULL DEFAULT '' COMMENT '称号描述',
  PRIMARY KEY (`title_id`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci ROW_FORMAT=DYNAMIC COMMENT='称号配置表';
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `title_data`
--

LOCK TABLES `title_data` WRITE;
/*!40000 ALTER TABLE `title_data` DISABLE KEYS */;
INSERT INTO `title_data` VALUES
(101,1,'false','false',0,'[{3,30},{4,40}]','小试牛刀','VIP1可获得'),
(102,1,'false','false',0,'[{3,30},{4,40}]','有钱任性','VIP2可获得'),
(103,1,'false','false',0,'[{3,30},{4,40}]','一掷千金','VIP3可获得'),
(104,1,'false','false',0,'[{3,30},{4,40}]','腰缠万贯','VIP4可获得'),
(105,1,'false','false',0,'[{3,30},{4,40}]','挥金如土','VIP5可获得'),
(106,1,'false','false',0,'[{3,30},{4,40}]','富甲天下','VIP6可获得'),
(107,1,'false','false',0,'[{3,30},{4,40}]','富可敌国','VIP7可获得'),
(108,1,'false','false',0,'[{3,30},{4,40}]','人生巅峰','VIP8可获得'),
(109,1,'false','false',0,'[{3,30},{4,40}]','至尊王者','VIP9可获得'),
(110,1,'false','false',0,'[{3,30},{4,40}]','高手对决','VIP0可获得'),
(201,2,'true','false',0,'[{6,60},{7,70}]','武艺超群','开服冲榜活动获取'),
(202,2,'true','false',0,'[{6,60},{7,70}]','出神入化','开服冲榜活动获取'),
(203,2,'true','false',0,'[{6,60},{7,70}]','仙武主宰','开服冲榜活动获取'),
(204,2,'true','false',0,'[{6,60},{7,70}]','锻造大师','开服冲榜活动获取'),
(205,2,'true','false',0,'[{6,60},{7,70}]','黑暗主宰','开服冲榜活动获取'),
(206,2,'true','false',0,'[{6,60},{7,70}]','聚魂先锋','开服冲榜活动获取'),
(207,2,'true','false',0,'[{6,60},{7,70}]','全职高手','开服冲榜活动获取'),
(208,2,'true','false',0,'[{6,60},{7,70}]','人中之龙','开服冲榜活动获取'),
(209,2,'true','false',0,'[{6,60},{7,70}]','勇者无畏','开服冲榜活动获取'),
(210,2,'true','false',0,'[{6,60},{7,70}]','称霸天下','开服冲榜活动获取'),
(10010,3,'false','true',604800,'[{5,50}]','归隐山林','充值获取');
/*!40000 ALTER TABLE `title_data` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `title_log`
--

DROP TABLE IF EXISTS `title_log`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `title_log` (
  `id` bigint(20) unsigned NOT NULL AUTO_INCREMENT COMMENT 'ID',
  `role_id` bigint(20) unsigned NOT NULL DEFAULT 0 COMMENT '角色ID',
  `title_id` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '称号ID',
  `from` varchar(32) NOT NULL DEFAULT '' COMMENT '来源',
  `time` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '时间',
  PRIMARY KEY (`id`) USING BTREE,
  KEY `role_id` (`role_id`) USING BTREE,
  KEY `time` (`time`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci ROW_FORMAT=DYNAMIC COMMENT='称号日志表';
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `title_log`
--

LOCK TABLES `title_log` WRITE;
/*!40000 ALTER TABLE `title_log` DISABLE KEYS */;
/*!40000 ALTER TABLE `title_log` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `total_login_log`
--

DROP TABLE IF EXISTS `total_login_log`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `total_login_log` (
  `id` bigint(20) unsigned NOT NULL AUTO_INCREMENT COMMENT 'ID',
  `number` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '数量',
  `time` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '时间',
  PRIMARY KEY (`id`) USING BTREE,
  KEY `time` (`time`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci ROW_FORMAT=DYNAMIC COMMENT='总登录日志';
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `total_login_log`
--

LOCK TABLES `total_login_log` WRITE;
/*!40000 ALTER TABLE `total_login_log` DISABLE KEYS */;
/*!40000 ALTER TABLE `total_login_log` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `total_online_log`
--

DROP TABLE IF EXISTS `total_online_log`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `total_online_log` (
  `id` bigint(20) unsigned NOT NULL AUTO_INCREMENT COMMENT 'ID',
  `role_id` bigint(20) unsigned NOT NULL DEFAULT 0 COMMENT '角色ID',
  `online_time` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '在线时长',
  `time` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '时间',
  PRIMARY KEY (`id`) USING BTREE,
  KEY `time` (`time`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci ROW_FORMAT=DYNAMIC COMMENT='总在线时长日志';
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `total_online_log`
--

LOCK TABLES `total_online_log` WRITE;
/*!40000 ALTER TABLE `total_online_log` DISABLE KEYS */;
/*!40000 ALTER TABLE `total_online_log` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `validation_data`
--

DROP TABLE IF EXISTS `validation_data`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `validation_data` (
  `type` varchar(255) NOT NULL DEFAULT '' COMMENT '类型',
  `key` varchar(255) NOT NULL DEFAULT '' COMMENT '键',
  `value` char(255) NOT NULL DEFAULT '' COMMENT '值',
  `description` char(255) NOT NULL DEFAULT '' COMMENT '描述',
  PRIMARY KEY (`type`,`key`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci COMMENT='数据校验配置表';
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `validation_data`
--

LOCK TABLES `validation_data` WRITE;
/*!40000 ALTER TABLE `validation_data` DISABLE KEYS */;
INSERT INTO `validation_data` VALUES
('act_script','enemy','敌人','动作脚本'),
('act_script','location','位置','动作脚本'),
('act_script','monster','怪物','动作脚本'),
('act_script','role','玩家','动作脚本'),
('act_type','active','主动','动作类型'),
('act_type','fix','固定','动作类型'),
('act_type','movable','移动','动作类型'),
('act_type','passive','被动','动作类型'),
('activity_service','','无','活动类型'),
('activity_service','auction','拍卖','活动类型'),
('activity_service','boss','BOSS','活动类型'),
('asset','','无','资产'),
('asset','coin','硬币','资产'),
('asset','copper','铜币','资产'),
('asset','exp','经验','资产'),
('asset','gold','金币','资产'),
('asset','silver','银币','资产'),
('bool','0','否','数字型布尔值'),
('bool','1','是','数字型布尔值'),
('boolean','false','否','布尔值'),
('boolean','true','是','布尔值'),
('classes','0','无限制','职业'),
('classes','1','七杀','职业'),
('classes','2','天师','职业'),
('classes','3','飞羽','职业'),
('classes','4','御灵','职业'),
('classes','5','妙音','职业'),
('classes','6','星术','职业'),
('compare','eq','等于','比较'),
('compare','ge','大于等于','比较'),
('compare','gt','大于','比较'),
('compare','le','小于等于','比较'),
('compare','lt','小于','比较'),
('compare','nc','不比较','比较'),
('compare','ne','不等于','比较'),
('condition','classes','职业','条件'),
('condition','level','等级','条件'),
('condition','sex','性别','条件'),
('condition','vip','VIP等级','条件'),
('dungeon_type','0','无','副本类型'),
('dungeon_type','1','经验副本','副本类型'),
('dungeon_type','2','铜币副本','副本类型'),
('effect_attribute','asset','资产','效果属性'),
('effect_attribute','attribute','属性','效果属性'),
('effect_attribute','buff','Buff','效果属性'),
('effect_attribute','hurt','伤害','效果属性'),
('effect_attribute','skill','技能','效果属性'),
('effect_field','','无','效果字段'),
('effect_field','attack','攻击','效果字段'),
('effect_field','copper','铜币','效果字段'),
('effect_field','defense','防御','效果字段'),
('effect_field','destroy','毁灭','效果字段'),
('effect_field','duck','闪避','效果字段'),
('effect_field','exp','经验','效果字段'),
('effect_field','fc','战力','效果字段'),
('effect_field','freeze','冰冻','效果字段'),
('effect_field','health','生命','效果字段'),
('effect_field','hit','命中','效果字段'),
('effect_field','hp','血量','效果字段'),
('effect_field','vertigo','眩晕','效果字段'),
('effect_object','mate','队友','效果对象'),
('effect_object','rival','对方','效果对象'),
('effect_object','self','自己','效果对象'),
('effect_operation','add','增加','效果操作'),
('effect_operation','clear','清除','效果操作'),
('effect_operation','reduce','减少','效果操作'),
('effect_operation','set','设置','效果操作'),
('effect_scope','battle','战斗','效果范围'),
('effect_scope','user','玩家','效果范围'),
('effect_type','active','主动','效果类型'),
('effect_type','buff','Buff','效果类型'),
('effect_type','passive','被动','效果类型'),
('event','','无','事件'),
('event','event_add_friend','添加好友','事件'),
('event','event_dungeon_passed','通关副本','事件'),
('event','event_friend_add','添加好友','事件'),
('event','event_guild_join','加入公会','事件'),
('event','event_kill_monster','杀怪','事件'),
('event','event_level_upgrade','升级','事件'),
('event','event_shop_buy','商店购买','事件'),
('function','','无','功能'),
('function','check_task','检查任务','功能'),
('function','start','开始','功能'),
('item_type','1','道具','物品类型'),
('item_type','10','资产','物品类型'),
('item_type','2','装备','物品类型'),
('item_type','3','身上','物品类型'),
('item_type','4','仓库','物品类型'),
('item_type','5','符文','物品类型'),
('item_type','6','寻宝','物品类型'),
('item_type','7','神兽','物品类型'),
('item_type','8','聚魂','物品类型'),
('item_type','9','饕餮','物品类型'),
('map_rank_key','','无','地图排行榜类型'),
('map_rank_key','camp','阵营','地图排行榜类型'),
('map_rank_key','guild','公会','地图排行榜类型'),
('map_rank_key','role','个人','地图排行榜类型'),
('map_rank_key','team','队伍','地图排行榜类型'),
('map_rank_mode','','不用排行','地图排行榜模式'),
('map_rank_mode','global','全局','地图排行榜模式'),
('map_rank_mode','local','不共享','地图排行榜模式'),
('map_rank_mode','share','共享','地图排行榜模式'),
('map_rank_value','','无','地图排行榜数值'),
('map_rank_value','hurt','伤害','地图排行榜数值'),
('map_type','full','全图','地图类型'),
('map_type','slice','九宫格','地图类型'),
('module','','无','模块'),
('module','auction_server','拍卖','模块'),
('module','boss_server','BOSS','模块'),
('module','dungeon_map','通用副本','模块'),
('module','friend','好友','模块'),
('module','role','角色','模块'),
('module','shop','商店','模块'),
('node_type_atom','center','跨服','节点'),
('node_type_atom','center_world','跨服和大世界','节点'),
('node_type_atom','local','本地','节点'),
('node_type_atom','local_center','本地和跨服','节点'),
('node_type_atom','local_center_world','本地和跨服和大世界','节点'),
('node_type_atom','local_world','本地和大世界','节点'),
('node_type_atom','world','大世界','节点'),
('node_type_integer','1','本地','数字型节点'),
('node_type_integer','2','跨服','数字型节点'),
('node_type_integer','3','本地和跨服','数字型节点'),
('node_type_integer','4','大世界','数字型节点'),
('node_type_integer','5','本地和大世界','数字型节点'),
('node_type_integer','6','跨服和大世界','数字型节点'),
('node_type_integer','7','本地和跨服和大世界','数字型节点'),
('receive_type','auto','自动','领取类型'),
('receive_type','manual','手动','领取类型'),
('sex','0','无限制','性别'),
('sex','1','男','性别'),
('sex','2','女','性别'),
('skill_type','active','主动','技能类型'),
('skill_type','passive','被动','技能类型'),
('use_effect','','无','使用效果'),
('use_effect','coin','硬币','使用效果'),
('use_effect','copper','铜币','使用效果'),
('use_effect','exp','经验','使用效果'),
('use_effect','gold','金币','使用效果'),
('use_effect','silver','银币','使用效果');
/*!40000 ALTER TABLE `validation_data` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `vip`
--

DROP TABLE IF EXISTS `vip`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `vip` (
  `role_id` bigint(20) unsigned NOT NULL DEFAULT 0 COMMENT '角色id',
  `vip_level` int(10) unsigned NOT NULL DEFAULT 0 COMMENT 'vip等级',
  `exp` int(10) unsigned NOT NULL DEFAULT 0 COMMENT 'vip经验',
  `expire_time` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '过期时间',
  PRIMARY KEY (`role_id`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci ROW_FORMAT=DYNAMIC COMMENT='角色vip表';
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `vip`
--

LOCK TABLES `vip` WRITE;
/*!40000 ALTER TABLE `vip` DISABLE KEYS */;
/*!40000 ALTER TABLE `vip` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `vip_data`
--

DROP TABLE IF EXISTS `vip_data`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `vip_data` (
  `vip_level` int(10) unsigned NOT NULL DEFAULT 0 COMMENT 'VIP等级',
  `exp` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '经验',
  PRIMARY KEY (`vip_level`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci ROW_FORMAT=DYNAMIC COMMENT='vip配置表';
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `vip_data`
--

LOCK TABLES `vip_data` WRITE;
/*!40000 ALTER TABLE `vip_data` DISABLE KEYS */;
INSERT INTO `vip_data` VALUES
(1,6),
(2,30),
(3,100),
(4,150),
(5,300),
(6,600),
(7,1000),
(8,2000),
(9,3000),
(10,5000),
(11,10000),
(12,30000),
(13,60000),
(14,100000),
(15,200000);
/*!40000 ALTER TABLE `vip_data` ENABLE KEYS */;
UNLOCK TABLES;
/*!40103 SET TIME_ZONE=@OLD_TIME_ZONE */;

/*!40101 SET SQL_MODE=@OLD_SQL_MODE */;
/*!40014 SET FOREIGN_KEY_CHECKS=@OLD_FOREIGN_KEY_CHECKS */;
/*!40014 SET UNIQUE_CHECKS=@OLD_UNIQUE_CHECKS */;
/*!40101 SET CHARACTER_SET_CLIENT=@OLD_CHARACTER_SET_CLIENT */;
/*!40101 SET CHARACTER_SET_RESULTS=@OLD_CHARACTER_SET_RESULTS */;
/*!40101 SET COLLATION_CONNECTION=@OLD_COLLATION_CONNECTION */;
/*!40111 SET SQL_NOTES=@OLD_SQL_NOTES */;

-- Dump completed on 2024-06-28 14:50:32

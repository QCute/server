-- MariaDB dump 10.19  Distrib 10.6.16-MariaDB, for Linux (x86_64)
--
-- Host: localhost    Database: local
-- ------------------------------------------------------
-- Server version	10.6.16-MariaDB-log

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
  `event` varchar(255) NOT NULL DEFAULT '' COMMENT '事件(validate(event))',
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
INSERT INTO `achievement_data` VALUES (1,1,1,0,2,'event_level_upgrade',0,3,'[{1,1}]','','',''),(2,1,2,1,3,'event_level_upgrade',5,1,'[{1,10}]','','',''),(3,1,3,2,0,'event_level_upgrade',2,1,'[{1,100}]','','',''),(4,2,4,0,4,'event_shop_buy',1,1,'[{1,1000}]','','',''),(5,2,5,4,5,'event_shop_buy',0,1,'[{1,1000}]','','',''),(6,2,6,5,0,'event_shop_buy',0,5,'[{1,10}]','','',''),(7,3,7,0,8,'event_dungeon_passed',3,1,'[{1,10}]','','',''),(8,3,8,8,9,'event_dungeon_passed',1,1,'[{1,10}]','','',''),(9,3,9,9,0,'event_dungeon_passed',1,1,'[{1,10}]','','','');
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
  `service` varchar(255) NOT NULL DEFAULT '' COMMENT '服务进程模块(validate(module))',
  `type` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '类型',
  `subtype` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '子类型',
  `award_type` varchar(255) NOT NULL DEFAULT '' COMMENT '领奖类型(validate(receive_type))',
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
INSERT INTO `activity_data` VALUES (1,1,'auction_server',1,1,'manual',1577808000,1577808000,1577808000,1577808000,1577808000,9,10,22,22,23,3,7,'活动名','activity.icon','activity','活动描述'),(2,2,'boss_server',1,1,'manual',1577808000,1577808000,1577808000,1577808000,1577808000,9,10,22,22,23,3,7,'活动名','activity.icon','activity','活动描述'),(3,4,'',1,1,'manual',1577808000,1577808000,1577808000,1577808000,1577808000,9,10,22,22,23,3,7,'活动名','activity.icon','activity','活动描述');
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
INSERT INTO `asset_data` VALUES ('coin',100004),('copper',100003),('exp',100005),('gold',100001),('silver',100002);
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
INSERT INTO `attribute_data` VALUES (2,'hp','fix','','','血量','血量'),(3,'attack','fix','attack','','攻击','攻击'),(4,'defense','fix','defense','','防御','防御'),(5,'health','fix','health','','生命','生命'),(6,'hit','fix','hit','','命中','命中'),(7,'duck','fix','duck','','闪避','闪避'),(8,'freeze','fix','','cannot_be_attack','冰冻','冰冻'),(9,'destroy','fix','','','毁灭','毁灭'),(10,'vertigo','fix','','','眩晕','眩晕');
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
INSERT INTO `auction_data` VALUES (1,1,1,1,0,0,0,0,0);
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
INSERT INTO `bubble_data` VALUES (101,1,0,0,'VIP1可获得','小试牛刀'),(102,1,0,0,'VIP2可获得','有钱任性'),(103,1,0,0,'VIP3可获得','一掷千金'),(104,1,0,0,'VIP4可获得','腰缠万贯'),(105,1,0,0,'VIP5可获得','挥金如土'),(106,1,0,0,'VIP6可获得','富甲天下'),(107,1,0,0,'VIP7可获得','富可敌国'),(108,1,0,0,'VIP8可获得','人生巅峰'),(109,1,0,0,'VIP9可获得','至尊王者'),(110,1,0,0,'VIP0可获得','高手对决'),(201,2,0,0,'开服冲榜活动获取','武艺超群'),(202,2,0,0,'开服冲榜活动获取','出神入化'),(203,2,0,0,'开服冲榜活动获取','仙武主宰'),(204,2,0,0,'开服冲榜活动获取','锻造大师'),(205,2,0,0,'开服冲榜活动获取','黑暗主宰'),(206,2,0,0,'开服冲榜活动获取','聚魂先锋'),(207,2,0,0,'开服冲榜活动获取','全职高手'),(208,2,0,0,'开服冲榜活动获取','人中之龙'),(209,2,0,0,'开服冲榜活动获取','勇者无畏'),(210,2,0,0,'开服冲榜活动获取','称霸天下'),(10010,3,0,0,'充值获取','归隐山林');
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
  `is_temporary` varchar(255) NOT NULL DEFAULT '' COMMENT '是否临时的(切地图失效)(validate(boolean))',
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
INSERT INTO `buff_data` VALUES (1,1,1800,'','[9]','false',3,'铜币',''),(2,1,3600,'','[10]','false',3,'经验',''),(3,2,0,'[{3,100}]','','false',2,'攻击',''),(4,2,0,'[{4,100}]','','false',2,'防御',''),(5,2,60,'','[3]','false',1,'眩晕',''),(6,3,60,'','[5]','false',0,'扣血','');
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
INSERT INTO `charge_data` VALUES (1,3,1,6,6.00,6.00,6,0,1,9999,1,'0','至尊神兵宝箱',''),(2,1,1,18,18.00,18.00,18,5,1,9999,2,'1','元宝',''),(3,1,1,68,68.00,68.00,68,40,1,9999,3,'2','元宝',''),(4,1,1,128,128.00,128.00,128,90,1,9999,4,'3','元宝',''),(5,1,1,268,268.00,268.00,268,190,1,9999,5,'4','元宝',''),(6,1,1,588,588.00,588.00,588,330,1,9999,6,'5','元宝',''),(7,1,1,688,688.00,688.00,688,590,1,9999,7,'6','元宝',''),(8,1,1,888,888.00,888.00,888,1300,1,9999,8,'7','元宝',''),(9,2,1,1288,1288.00,1288.00,1288,0,1,9999,0,'','周卡',''),(10,6,1,8888,8888.00,8888.00,8888,0,1,9999,0,'','月卡','');
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
INSERT INTO `classes_data` VALUES (1,'七杀'),(2,'天师'),(3,'飞羽'),(4,'御灵'),(5,'妙音'),(6,'星术');
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
INSERT INTO `daily_active_data` VALUES (1,0,2,30,'[{1,1000}]'),(2,1,3,50,'[{1,1000}]'),(3,2,4,80,'[{1,1000}]'),(4,3,5,100,'[{1,1000}]'),(5,4,6,120,'[{1,1000}]'),(6,5,0,150,'[{1,1000}]');
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
INSERT INTO `daily_data` VALUES (1,1,1,1,1,'[{1,1000}]'),(2,1,2,2,2,'[{1,1000}]'),(3,1,3,3,3,'[{1,1000}]'),(4,1,4,4,4,'[{1,1000}]'),(5,1,5,5,5,'[{1,1000}]'),(6,1,6,6,6,'[{1,1000}]'),(7,1,7,7,7,'[{1,1000}]'),(8,1,8,8,8,'[{1,1000}]'),(9,1,9,9,9,'[{1,1000}]');
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
INSERT INTO `dungeon_data` VALUES (1,1,'[{level,10}]','[{100005,100}]','[{0,1},{1,2},{2,3},{3,4},{4,5},{5,6}]','[{0,1,100},{1,2,200},{2,3,300},{3,4,400},{4,5,500},{5,6,600}]',110001,'[{1,10},{1,20},{1,10},{1,20},{2,1}]','',600,'[{100005,100}]','经验副本','经验副本'),(2,1,'[{level,20}]','[{100005,200}]','[{0,1},{1,2},{2,3},{3,4},{4,5},{5,6}]','[{0,1,100},{1,2,200},{2,3,300},{3,4,400},{4,5,500},{5,6,600}]',110002,'[{1,10},{1,20},{1,10},{1,20},{2,1}]','',600,'[{100005,200}]','经验副本','经验副本'),(3,1,'[{level,30}]','[{100005,300}]','[{0,1},{1,2},{2,3},{3,4},{4,5},{5,6}]','[{0,1,100},{1,2,200},{2,3,300},{3,4,400},{4,5,500},{5,6,600}]',110003,'[{1,10},{1,20},{1,10},{1,20},{2,1}]','',600,'[{100005,300}]','经验副本','经验副本'),(4,2,'[{level,10}]','[{100005,100}]','[{0,1},{1,2},{2,3},{3,4},{4,5},{5,6}]','[{0,1,100},{1,2,200},{2,3,300},{3,4,400},{4,5,500},{5,6,600}]',120001,'[{1,10},{1,20},{1,10},{1,20},{2,1}]','',600,'[{100003,100}]','铜币副本','铜币副本'),(5,2,'[{level,20}]','[{100005,200}]','[{0,1},{1,2},{2,3},{3,4},{4,5},{5,6}]','[{0,1,100},{1,2,200},{2,3,300},{3,4,400},{4,5,500},{5,6,600}]',120002,'[{1,10},{1,20},{1,10},{1,20},{2,1}]','',600,'[{100003,200}]','铜币副本','铜币副本'),(6,2,'[{level,30}]','[{100005,300}]','[{0,1},{1,2},{2,3},{3,4},{4,5},{5,6}]','[{0,1,100},{1,2,200},{2,3,300},{3,4,400},{4,5,500},{5,6,600}]',120003,'[{1,10},{1,20},{1,10},{1,20},{2,1}]','',600,'[{100003,300}]','铜币副本','铜币副本');
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
  `type` varchar(255) NOT NULL DEFAULT '' COMMENT '类型(validate(effect_type))',
  `scope` varchar(255) NOT NULL DEFAULT '' COMMENT '作用范围(validate(effect_scope))',
  `condition` varchar(255) NOT NULL DEFAULT '' COMMENT '条件',
  `ratio` varchar(255) NOT NULL DEFAULT '' COMMENT '概率',
  `restrict` varchar(255) NOT NULL DEFAULT '' COMMENT '约束',
  `operation` varchar(255) NOT NULL DEFAULT '' COMMENT '操作(validate(effect_operation))',
  `object` varchar(255) NOT NULL DEFAULT '' COMMENT '作用对象(validate(effect_object))',
  `attribute` varchar(255) NOT NULL DEFAULT '' COMMENT '操作属性(validate(effect_attribute))',
  `field` varchar(255) NOT NULL DEFAULT '' COMMENT '操作属性字段(validate(effect_field))',
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
INSERT INTO `effect_data` VALUES (1,'active','battle','','10000','_','add','self','hurt','','Hurt',0,'','伤害'),(2,'active','battle','','10000','_','add','self','hurt','','Hurt * 1.5',0,'','增加50%伤害'),(3,'active','battle','','10000','_','add','self','attribute','vertigo','1',0,'','眩晕'),(4,'active','battle','','10000','_','reduce','self','attribute','vertigo','0',0,'','清除眩晕'),(5,'active','battle','','10000','_','reduce','self','attribute','hp','Rival.Attribute.health * 0.01',3600,'','每秒扣血，总血量百分之1'),(6,'active','battle','','10000','_','add','mate','attribute','attack','Mate.Attribute.attack * 1.5',3,'','增加队友攻击150%'),(7,'active','battle','','10000','_','add','mate','attribute','defense','Mate.Attribute.defense * 1.5',3,'','增加队友防御150%'),(8,'active','battle','','10000','_','add','self','buff','','[1]',0,'','添加Buff'),(9,'active','user','','10000','_','add','self','asset','copper','1.5',0,'','增加150%铜币'),(10,'active','user','','10000','_','add','self','asset','exp','2',0,'','增加200%经验');
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
  `is_unique` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '是否全局唯一(validate(boolean))',
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
INSERT INTO `fashion_data` VALUES (101,1,'false',0,'[{3,30},{4,40}]','小试牛刀','VIP1可获得'),(102,1,'false',0,'[{3,30},{4,40}]','有钱任性','VIP2可获得'),(103,1,'false',0,'[{3,30},{4,40}]','一掷千金','VIP3可获得'),(104,1,'false',0,'[{3,30},{4,40}]','腰缠万贯','VIP4可获得'),(105,1,'false',0,'[{3,30},{4,40}]','挥金如土','VIP5可获得'),(106,1,'false',0,'[{3,30},{4,40}]','富甲天下','VIP6可获得'),(107,1,'false',0,'[{3,30},{4,40}]','富可敌国','VIP7可获得'),(108,1,'false',0,'[{3,30},{4,40}]','人生巅峰','VIP8可获得'),(109,1,'false',0,'[{3,30},{4,40}]','至尊王者','VIP9可获得'),(110,1,'false',0,'[{3,30},{4,40}]','高手对决','VIP0可获得'),(201,2,'false',0,'[{6,60},{7,70}]','武艺超群','开服冲榜活动获取'),(202,2,'false',0,'[{6,60},{7,70}]','出神入化','开服冲榜活动获取'),(203,2,'false',0,'[{6,60},{7,70}]','仙武主宰','开服冲榜活动获取'),(204,2,'false',0,'[{6,60},{7,70}]','锻造大师','开服冲榜活动获取'),(205,2,'false',0,'[{6,60},{7,70}]','黑暗主宰','开服冲榜活动获取'),(206,2,'false',0,'[{6,60},{7,70}]','聚魂先锋','开服冲榜活动获取'),(207,2,'false',0,'[{6,60},{7,70}]','全职高手','开服冲榜活动获取'),(208,2,'false',0,'[{6,60},{7,70}]','人中之龙','开服冲榜活动获取'),(209,2,'false',0,'[{6,60},{7,70}]','勇者无畏','开服冲榜活动获取'),(210,2,'false',0,'[{6,60},{7,70}]','称霸天下','开服冲榜活动获取'),(10010,3,'true',604800,'[{5,50}]','归隐山林','充值获取');
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
  `friend_role_id` bigint(20) unsigned NOT NULL DEFAULT 0 COMMENT '好友角色ID', -- ON role.role_id, (join_on(`role`.`role_id`)/join_on(`vip`.`role_id`))
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
INSERT INTO `guild_create_data` VALUES (0,'',''),(1,'[{level, 1}, {vip, 1}]','[{100001, 1}]'),(2,'[{level, 2}, {vip, 2}]','[{100001, 2}]'),(3,'[{level, 3}, {vip, 3}]','[{100001, 3}]');
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
INSERT INTO `guild_level_data` VALUES (0,100),(1,200),(2,300),(3,400),(4,500),(5,600),(6,700),(7,800),(8,900),(9,1000);
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
  `use_effect` varchar(255) NOT NULL DEFAULT '' COMMENT '使用效果(validate(use_effect))',
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
INSERT INTO `item_data` VALUES (1,1,1000,0,0,0,'',0,'rust','file_type_rust.svg',''),(2,1,100,0,0,0,'',0,'erlang','file_type_erlang.svg',''),(3,1,10,0,0,0,'',0,'php','file_type_php.svg',''),(4,2,1,0,0,0,'',0,'lua','file_type_lua.svg',''),(5,2,1,0,0,0,'',0,'js','file_type_js.svg',''),(6,2,1,0,0,0,'',0,'html','file_type_html.svg',''),(7,2,1,0,604800,100,'',0,'css','file_type_css.svg',''),(100001,10,1,0,0,0,'gold',0,'gold','file_type_gold.svg',''),(100002,10,1,0,0,0,'silver',0,'silver','file_type_silver.svg',''),(100003,10,1,0,0,0,'copper',0,'copper','file_type_copper.svg',''),(100004,10,1,0,0,0,'exp',0,'exp','file_type_exp.svg',''),(100005,10,1,0,0,0,'coin',0,'coin','file_type_coin.svg','');
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
  `is_unique` varchar(255) NOT NULL DEFAULT '' COMMENT '是否唯一(validate(boolean))',
  `award` varchar(255) NOT NULL DEFAULT '' COMMENT '奖励',
  PRIMARY KEY (`type`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci ROW_FORMAT=DYNAMIC COMMENT='兑换码奖励配置表';
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `key_award_data`
--

LOCK TABLES `key_award_data` WRITE;
/*!40000 ALTER TABLE `key_award_data` DISABLE KEYS */;
INSERT INTO `key_award_data` VALUES (1,'false','[{700001,1},{700002,2},{700003,3}]'),(2,'true','[{700001,1},{700002,2},{700003,3}]');
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
INSERT INTO `key_data` VALUES ('fake',2),('test',1);
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
INSERT INTO `level_data` VALUES (0,100),(1,200),(2,300),(3,400),(4,500),(5,600),(6,700),(7,800),(8,900),(9,1000);
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
  `type` varchar(255) NOT NULL DEFAULT '' COMMENT '广播类型(validate(map_type))',
  `reconnect` varchar(255) NOT NULL DEFAULT '' COMMENT '是否重连(validate(boolean))',
  `monsters` varchar(255) NOT NULL DEFAULT '' COMMENT '随地图启动的怪物',
  `rank_key` varchar(255) NOT NULL DEFAULT '' COMMENT '榜键类型(validate(map_rank_key))',
  `rank_value` varchar(255) NOT NULL DEFAULT '' COMMENT '榜值类型(validate(map_rank_value))',
  `rank_mode` varchar(255) NOT NULL DEFAULT '' COMMENT '榜模式(validate(map_rank_mode))',
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
INSERT INTO `map_data` VALUES (100000,'slice','false','','','hurt','share','','','','',''),(110001,'full','false','','camp','hurt','share','','','','',''),(110002,'full','false','','camp','hurt','share','','','','',''),(110003,'full','false','','camp','hurt','share','','','','',''),(120001,'full','false','','team','hurt','share','','','','',''),(120002,'full','false','','team','hurt','share','','','','',''),(120003,'full','false','','team','hurt','share','','','','',''),(200001,'slice','true','','guild','hurt','share','','','','',''),(200002,'slice','true','','guild','hurt','share','','','','',''),(200003,'slice','true','','guild','hurt','share','','','','',''),(300001,'slice','true','','role','hurt','share','','','','',''),(300002,'slice','true','','role','hurt','share','','','','',''),(300003,'slice','true','','role','hurt','share','','','','','');
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
  `act_type` varchar(255) NOT NULL DEFAULT '' COMMENT '动作类型(validate(act_type))',
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
INSERT INTO `monster_data` VALUES (1,1,'active','active',1,100,200001,1,1,300,0,'active','[role]','[1]','[{10,10},{20,10},{30,10},{40,10},{50,10},{60,10},{70,10},{10,10},{90,10},{100,10}]','[{100005,100}]'),(2,2,'passive','passive',1,200,200002,1,2,300,0,'passive','[enemy]','[1]','[{40,10}]','[{100005,200}]'),(3,3,'movable','movable',1,300,200003,1,3,300,0,'movable','','','[{60,10}]','[{100005,300}]'),(4,4,'fix','fix',1,400,0,1,4,300,0,'fix','','','[{80,10}]',''),(5,5,'act','act',1,500,0,1,5,300,0,'fix','[enemy]','','[{100,10}]',''),(6,6,'boom','boom',1,600,0,1,6,300,0,'active','[{monster, 20}, {monster, 50}, role]','','[{120,10}]','[{100005,600}]'),(7,5,'act','act',1,700,0,1,7,300,0,'fix','[enemy]','','[{140,10}]',''),(8,6,'boom','boom',1,800,0,1,8,300,0,'fix','[{monster, 20}, {monster, 50}, role]','','[{160,10}]','');
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
INSERT INTO `parameter_data` VALUES ('bag_size','100','装备背包大小'),('chat_cd','0','聊天冷却时间'),('chat_guild_size_limit','100','公会聊天保留条数'),('chat_level','0','聊天开放等级'),('chat_private_size_limit','100','私聊保留条数'),('chat_system_size_limit','100','系统信息条数'),('chat_world_size_limit','100','世界聊天保留条数'),('dungeon_inspire_buff_id','3','副本鼓舞BuffID'),('friend_level','0','好友开放等级'),('friend_number','50','好友上限'),('guild_create_cd','86400','公会创建冷却时间'),('guild_join_cd','86400','公会加入冷却时间'),('guild_member_limit','[{0, 50}, {1, 60}, {2, 70}, {3, 80}, {4, 90}, {5, 100}]','公会人员数'),('item_size','100','道具背包大小'),('language','zhCN','默认语言'),('login_cd','180','登录时间间隔'),('mail_expire_time','604800','邮件过期时间'),('mail_max_item','10','单封邮件最大物品数'),('store_size','100','仓库大小'),('time_zone','8','时区');
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
INSERT INTO `charge_data` VALUES (1,3,1,6,6.00,6.00,6,0,1,9999,1,'0','至尊神兵宝箱',''),(2,1,1,18,18.00,18.00,18,5,1,9999,2,'1','元宝',''),(3,1,1,68,68.00,68.00,68,40,1,9999,3,'2','元宝',''),(4,1,1,128,128.00,128.00,128,90,1,9999,4,'3','元宝',''),(5,1,1,268,268.00,268.00,268,190,1,9999,5,'4','元宝',''),(6,1,1,588,588.00,588.00,588,330,1,9999,6,'5','元宝',''),(7,1,1,688,688.00,688.00,688,590,1,9999,7,'6','元宝',''),(8,1,1,888,888.00,888.00,888,1300,1,9999,8,'7','元宝',''),(9,2,1,1288,1288.00,1288.00,1288,0,1,9999,0,'','周卡',''),(10,6,1,8888,8888.00,8888.00,8888,0,1,9999,0,'','月卡','');
/*!40000 ALTER TABLE `charge_data` ENABLE KEYS */;
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
INSERT INTO `reference_data` VALUES ('condition','{classes, n}','职业为n'),('condition','{level, n}','等级n级'),('condition','{sex, n}','性别为n'),('condition','{vip, n}','VIP等级n级');
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
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci ROW_FORMAT=DYNAMIC COMMENT='角色信息表';
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `role`
--

LOCK TABLES `role` WRITE;
/*!40000 ALTER TABLE `role` DISABLE KEYS */;
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
INSERT INTO `sex_data` VALUES (1,'男'),(2,'女');
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
  `pay_asset` varchar(255) NOT NULL DEFAULT '' COMMENT '货币类型(validate(asset))',
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
INSERT INTO `shop_data` VALUES (1,1,1,'gold',10,1,0,10,0,'','');
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
INSERT INTO `sign_active_data` VALUES (2,0,5,1),(5,2,7,2),(7,5,10,3),(10,7,15,4),(15,10,20,5),(20,15,22,6),(22,20,25,7),(25,22,28,8),(28,25,30,9),(30,28,0,10);
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
INSERT INTO `sign_data` VALUES (1,'[{1,1}]'),(2,'[{2,1}]'),(3,'[{3,1}]'),(4,'[{4,1}]'),(5,'[{5,1}]'),(6,'[{6,1}]'),(7,'[{7,1}]');
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
  `type` varchar(255) NOT NULL DEFAULT '' COMMENT '类型(validate(skill_type))',
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
INSERT INTO `skill_data` VALUES (1,'active',0,'普攻技能','','','','[1]',1,1000,1000,1,'','','','','对目标造成180%的伤害'),(2,'active',0,'群攻技能','','','','[2]',1,1000,1000,30,'','','','','对3个目标造成150%的伤害'),(3,'passive',0,'增益','','','','[8]',10,1,1,1,'','','','','每秒扣血，总血量万分之50'),(5,'active',0,'普攻技能','','','','[1]',1,1,1,1,'','','','','普通技能');
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
  `event` varchar(255) NOT NULL DEFAULT '' COMMENT '事件(validate(event))',
  `compare` varchar(255) NOT NULL DEFAULT '' COMMENT '比较模式(validate(compare))',
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
INSERT INTO `task_data` VALUES (1,1,0,2,'event_kill_monster','nc',0,3,'[{level, 10}]','','[{1,1}]','','',''),(2,1,1,3,'event_level_upgrade','ge',5,1,'[{sex, 1}]','[{100003, 100}]','[{1,10}]','','',''),(3,1,2,4,'event_dungeon_passed','gt',2,1,'[{level, 10},{classes,2}]','','[{1,100}]','','',''),(4,1,3,5,'event_shop_buy','eq',1,1,'[{vip, 3}]','','[{1,1000}]','','',''),(5,1,4,0,'event_guild_join','nc',0,1,'[{classes, 1},{level, 2},{sex, 3},{vip, 4}]','','[{1,1000}]','','',''),(6,1,5,0,'event_add_friend','nc',0,5,'','','[{1,10}]','','',''),(1001,2,0,1002,'event_dungeon_exp_passed','ge',3,1,'','','[{1,10}]','','',''),(1002,2,1001,0,'event_friend_add','eq',1,1,'','','[{1,10}]','','',''),(100001,3,0,100002,'event_dungeon_copper_passed','eq',1,1,'','','[{1,10}]','','',''),(100002,3,100001,0,'event_guild_join','nc',0,1,'','','[{1,10}]','','','');
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
INSERT INTO `text_data` VALUES ('account_create_max','服务器角色数量已达到上限','文本'),('account_login_forbidden','账号禁止登录','文本'),('account_logout','登出','文本'),('account_not_found','没有找到此账号','文本'),('account_permission_denied','账号权限不足','文本'),('achievement_not_completed','成就未完成','文本'),('achievement_not_found','没有找到此成就','文本'),('asset_copper_not_enough','铜币不足','文本'),('asset_gold_not_enough','金币不足','文本'),('asset_not_enough','资产不足','文本'),('asset_silver_not_enough','银币不足','文本'),('auction_not_found','没有找到此拍品','文本'),('auction_price_changed','拍品价格已发生变化','文本'),('award_already_received','奖励已经领取过了','文本'),('award_error','奖励领取错误','文本'),('award_pre_not_received','前置奖励未领取','文本'),('boss_dead','BOSS已经死亡','文本'),('boss_not_found','没有找到此Boss','文本'),('bubble_duplicated','气泡重复','文本'),('buff_duplicated','Buff重复','文本'),('chat_cannot_with_self','不能和自己聊天','文本'),('chat_too_frequently','发言太频繁','文本'),('cheat_command_not_found','没有找到此命令','文本'),('condition_not_met','条件不满足','文本'),('configure_not_found','没有找到此配置','文本'),('daily_not_completed','日常任务未完成','文本'),('daily_score_not_enough','日常活跃度不足','文本'),('dungeon_not_found','没有找到此副本','文本'),('dungeon_today_number_limit','今日进入次数已达到上限','文本'),('fashion_duplicated','时装重复','文本'),('friend_apply_not_found','没有找到此好友的申请','文本'),('friend_in_apply','对方已在申请列表中','文本'),('friend_in_be_block','你已被对方拉黑','文本'),('friend_in_block','对方已在黑名单中','文本'),('friend_in_list','对方已在好友列表中','文本'),('friend_level_not_met','对方好友等级不满足','文本'),('friend_not_found','没有找到此好友','文本'),('friend_number_max','好友数量达到上限','文本'),('guild_already_joined','你已经加入过公会了','文本'),('guild_apply_frequently','公会申请太频繁','文本'),('guild_apply_not_found','没有找到此申请','文本'),('guild_cannot_kick_self','不可剔除自己','文本'),('guild_cannot_update_self','不可升级自己','文本'),('guild_create_frequently','公会创建太频繁','文本'),('guild_member_not_found','没有找到此成员','文本'),('guild_member_number_limit','公会成员数量已达到上限','文本'),('guild_not_found','没有找到此商会','文本'),('guild_not_joined','没有加入公会','文本'),('guild_permission_denied','公会权限不足','文本'),('invalid_classes','无效职业','文本'),('invalid_item','无效物品','文本'),('invalid_number','无效数量','文本'),('invalid_sex','无效性别','文本'),('invalid_type','无效类型','文本'),('item_bag_full','背包已满','文本'),('item_cannot_use_directly','物品不能直接使用','文本'),('item_not_enough','物品不足','文本'),('item_use_number_max','使用个数超过单次使用上限','文本'),('key_already_activated','激活码已激活过','文本'),('key_already_active','此兑换码已经兑换过了','文本'),('level_not_met','等级不满足','文本'),('lucky_money_already_received','红包已领取过','文本'),('lucky_money_expired','红包已过期','文本'),('lucky_money_not_found','没有找到此红包','文本'),('mail_already_read','邮件已阅读过','文本'),('mail_attachment_empty','附件为空','文本'),('mail_not_found','没有找到此邮件','文本'),('mail_text_add_item_content','您的背包已满，新增的道具已经放到了邮件里，请注意查收。','背包满内容'),('mail_text_add_item_title','背包已满','背包满标题'),('mail_text_auction_income_content','您的拍卖收入分成。','拍卖分红内容'),('mail_text_auction_income_title','拍卖收入','拍卖分红标题'),('mail_text_auction_success_content','您的拍卖物品，请注意查收。','拍卖成功内容'),('mail_text_auction_success_title','拍卖成功','拍卖成功标题'),('name_duplicate','名字重复','文本'),('name_duplicated','名字重复','文本'),('name_length','名字长度不对','文本'),('name_length_invalid','名字长度无效','文本'),('name_not_utf8_charset','名字非UTF8字符','文本'),('name_sensitive','名字敏感','文本'),('notice_text_guild_create','<id>~w</id>~s创建公会<id>~w</id>~s','创建公会公告'),('notice_text_level_upgrade','恭喜<id>~w</id>~s升到~w级','升级公告'),('notice_text_vip_upgrade','恭喜<id>~w</id>~sVip升到~w级','Vip升级公告'),('packet_heartbeat_too_fast','心跳包速度过快','文本'),('packet_too_fast','包速度过快','文本'),('role_cannot_change_same_classes','职业不能相同','文本'),('role_cannot_change_same_name','名字不能相同','文本'),('role_cannot_change_same_sex','性别不能相同','文本'),('server_create_forbidden','服务器禁止创建角色','文本'),('server_id_mismatch','服务器ID不匹配','文本'),('server_login_forbidden','服务器禁止登录','文本'),('server_update','服务器更新','文本'),('shop_buy_num_max','已达到购买数量上限','文本'),('signed_already','已经签到过了','文本'),('task_already_submitted','任务已提交','文本'),('task_not_completed','任务还没完成','文本'),('task_not_found','没有找到此任务','文本'),('task_not_next','请按顺序完成','文本'),('task_pre_not_completed','前置任务还没完成','文本'),('timeout','请求超时','文本'),('title_duplicated','称号重复','文本'),('user_offline','对方不在线','文本'),('vip_level_not_met','Vip等级不满足','文本');
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
  `multi` varchar(255) NOT NULL DEFAULT '' COMMENT '同类型可否拥有多个(validate(boolean))',
  `is_unique` varchar(255) NOT NULL DEFAULT '' COMMENT '是否全服唯一(validate(boolean))',
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
INSERT INTO `title_data` VALUES (101,1,'false','false',0,'[{3,30},{4,40}]','小试牛刀','VIP1可获得'),(102,1,'false','false',0,'[{3,30},{4,40}]','有钱任性','VIP2可获得'),(103,1,'false','false',0,'[{3,30},{4,40}]','一掷千金','VIP3可获得'),(104,1,'false','false',0,'[{3,30},{4,40}]','腰缠万贯','VIP4可获得'),(105,1,'false','false',0,'[{3,30},{4,40}]','挥金如土','VIP5可获得'),(106,1,'false','false',0,'[{3,30},{4,40}]','富甲天下','VIP6可获得'),(107,1,'false','false',0,'[{3,30},{4,40}]','富可敌国','VIP7可获得'),(108,1,'false','false',0,'[{3,30},{4,40}]','人生巅峰','VIP8可获得'),(109,1,'false','false',0,'[{3,30},{4,40}]','至尊王者','VIP9可获得'),(110,1,'false','false',0,'[{3,30},{4,40}]','高手对决','VIP0可获得'),(201,2,'true','false',0,'[{6,60},{7,70}]','武艺超群','开服冲榜活动获取'),(202,2,'true','false',0,'[{6,60},{7,70}]','出神入化','开服冲榜活动获取'),(203,2,'true','false',0,'[{6,60},{7,70}]','仙武主宰','开服冲榜活动获取'),(204,2,'true','false',0,'[{6,60},{7,70}]','锻造大师','开服冲榜活动获取'),(205,2,'true','false',0,'[{6,60},{7,70}]','黑暗主宰','开服冲榜活动获取'),(206,2,'true','false',0,'[{6,60},{7,70}]','聚魂先锋','开服冲榜活动获取'),(207,2,'true','false',0,'[{6,60},{7,70}]','全职高手','开服冲榜活动获取'),(208,2,'true','false',0,'[{6,60},{7,70}]','人中之龙','开服冲榜活动获取'),(209,2,'true','false',0,'[{6,60},{7,70}]','勇者无畏','开服冲榜活动获取'),(210,2,'true','false',0,'[{6,60},{7,70}]','称霸天下','开服冲榜活动获取'),(10010,3,'false','true',604800,'[{5,50}]','归隐山林','充值获取');
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
INSERT INTO `validation_data` VALUES ('act_script','enemy','敌人','动作脚本'),('act_script','location','位置','动作脚本'),('act_script','monster','怪物','动作脚本'),('act_script','role','玩家','动作脚本'),('act_type','active','主动','动作类型'),('act_type','fix','固定','动作类型'),('act_type','movable','移动','动作类型'),('act_type','passive','被动','动作类型'),('activity_service','','无','活动类型'),('activity_service','auction','拍卖','活动类型'),('activity_service','boss','BOSS','活动类型'),('asset','','无','资产'),('asset','coin','硬币','资产'),('asset','copper','铜币','资产'),('asset','exp','经验','资产'),('asset','gold','金币','资产'),('asset','silver','银币','资产'),('bool','0','否','数字型布尔值'),('bool','1','是','数字型布尔值'),('boolean','false','否','布尔值'),('boolean','true','是','布尔值'),('classes','0','无限制','职业'),('classes','1','七杀','职业'),('classes','2','天师','职业'),('classes','3','飞羽','职业'),('classes','4','御灵','职业'),('classes','5','妙音','职业'),('classes','6','星术','职业'),('compare','eq','等于','比较'),('compare','ge','大于等于','比较'),('compare','gt','大于','比较'),('compare','le','小于等于','比较'),('compare','lt','小于','比较'),('compare','nc','不比较','比较'),('compare','ne','不等于','比较'),('condition','classes','职业','条件'),('condition','level','等级','条件'),('condition','sex','性别','条件'),('condition','vip','VIP等级','条件'),('dungeon_type','0','无','副本类型'),('dungeon_type','1','经验副本','副本类型'),('dungeon_type','2','铜币副本','副本类型'),('effect_attribute','asset','资产','效果属性'),('effect_attribute','attribute','属性','效果属性'),('effect_attribute','buff','Buff','效果属性'),('effect_attribute','hurt','伤害','效果属性'),('effect_attribute','skill','技能','效果属性'),('effect_field','','无','效果字段'),('effect_field','attack','攻击','效果字段'),('effect_field','copper','铜币','效果字段'),('effect_field','defense','防御','效果字段'),('effect_field','destroy','毁灭','效果字段'),('effect_field','duck','闪避','效果字段'),('effect_field','exp','经验','效果字段'),('effect_field','fc','战力','效果字段'),('effect_field','freeze','冰冻','效果字段'),('effect_field','health','生命','效果字段'),('effect_field','hit','命中','效果字段'),('effect_field','hp','血量','效果字段'),('effect_field','vertigo','眩晕','效果字段'),('effect_object','mate','队友','效果对象'),('effect_object','rival','对方','效果对象'),('effect_object','self','自己','效果对象'),('effect_operation','add','增加','效果操作'),('effect_operation','clear','清除','效果操作'),('effect_operation','reduce','减少','效果操作'),('effect_operation','set','设置','效果操作'),('effect_scope','battle','战斗','效果范围'),('effect_scope','user','玩家','效果范围'),('effect_type','active','主动','效果类型'),('effect_type','buff','Buff','效果类型'),('effect_type','passive','被动','效果类型'),('event','','无','事件'),('event','event_add_friend','添加好友','事件'),('event','event_dungeon_passed','通关副本','事件'),('event','event_friend_add','添加好友','事件'),('event','event_guild_join','加入公会','事件'),('event','event_kill_monster','杀怪','事件'),('event','event_level_upgrade','升级','事件'),('event','event_shop_buy','商店购买','事件'),('function','','无','功能'),('function','check_task','检查任务','功能'),('function','start','开始','功能'),('item_type','1','道具','物品类型'),('item_type','10','资产','物品类型'),('item_type','2','装备','物品类型'),('item_type','3','身上','物品类型'),('item_type','4','仓库','物品类型'),('item_type','5','符文','物品类型'),('item_type','6','寻宝','物品类型'),('item_type','7','神兽','物品类型'),('item_type','8','聚魂','物品类型'),('item_type','9','饕餮','物品类型'),('map_rank_key','','无','地图排行榜类型'),('map_rank_key','camp','阵营','地图排行榜类型'),('map_rank_key','guild','公会','地图排行榜类型'),('map_rank_key','role','个人','地图排行榜类型'),('map_rank_key','team','队伍','地图排行榜类型'),('map_rank_mode','','不用排行','地图排行榜模式'),('map_rank_mode','global','全局','地图排行榜模式'),('map_rank_mode','local','不共享','地图排行榜模式'),('map_rank_mode','share','共享','地图排行榜模式'),('map_rank_value','','无','地图排行榜数值'),('map_rank_value','hurt','伤害','地图排行榜数值'),('map_type','full','全图','地图类型'),('map_type','slice','九宫格','地图类型'),('module','','无','模块'),('module','auction_server','拍卖','模块'),('module','boss_server','BOSS','模块'),('module','dungeon_map','通用副本','模块'),('module','friend','好友','模块'),('module','role','角色','模块'),('module','shop','商店','模块'),('node_type_atom','center','跨服','节点'),('node_type_atom','center_world','跨服和大世界','节点'),('node_type_atom','local','本地','节点'),('node_type_atom','local_center','本地和跨服','节点'),('node_type_atom','local_center_world','本地和跨服和大世界','节点'),('node_type_atom','local_world','本地和大世界','节点'),('node_type_atom','world','大世界','节点'),('node_type_integer','1','本地','数字型节点'),('node_type_integer','2','跨服','数字型节点'),('node_type_integer','3','本地和跨服','数字型节点'),('node_type_integer','4','大世界','数字型节点'),('node_type_integer','5','本地和大世界','数字型节点'),('node_type_integer','6','跨服和大世界','数字型节点'),('node_type_integer','7','本地和跨服和大世界','数字型节点'),('receive_type','auto','自动','领取类型'),('receive_type','manual','手动','领取类型'),('sex','0','无限制','性别'),('sex','1','男','性别'),('sex','2','女','性别'),('skill_type','active','主动','技能类型'),('skill_type','passive','被动','技能类型'),('use_effect','','无','使用效果'),('use_effect','coin','硬币','使用效果'),('use_effect','copper','铜币','使用效果'),('use_effect','exp','经验','使用效果'),('use_effect','gold','金币','使用效果'),('use_effect','silver','银币','使用效果');
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
INSERT INTO `vip_data` VALUES (1,6),(2,30),(3,100),(4,150),(5,300),(6,600),(7,1000),(8,2000),(9,3000),(10,5000),(11,10000),(12,30000),(13,60000),(14,100000),(15,200000);
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

-- Dump completed on 2024-01-30 17:41:20

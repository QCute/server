-- MariaDB dump 10.17  Distrib 10.5.2-MariaDB, for debian-linux-gnu (x86_64)
--
-- Host: localhost    Database: local
-- ------------------------------------------------------
-- Server version	10.5.2-MariaDB-1:10.5.2+maria~focal-log

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
-- Table structure for table `activity_data`
--

DROP TABLE IF EXISTS `activity_data`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `activity_data` (
  `activity_id` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '活动ID',
  `mode` tinyint(1) unsigned NOT NULL DEFAULT 0 COMMENT '活动模式(validate(node_type_integer))',
  `service` varchar(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '服务进程模块(validate(activity_service))',
  `type` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '类型',
  `subtype` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '子类型',
  `award_type` tinyint(1) unsigned NOT NULL DEFAULT 0 COMMENT '领奖类型(自动:0/手动:1)',
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
  `name` char(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '活动名',
  `icon` char(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '活动图标',
  `entrance` char(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '活动入口',
  `description` char(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '活动描述',
  PRIMARY KEY (`activity_id`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci ROW_FORMAT=DYNAMIC COMMENT='活动配置表';
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `activity_data`
--

LOCK TABLES `activity_data` WRITE;
/*!40000 ALTER TABLE `activity_data` DISABLE KEYS */;
INSERT INTO `activity_data` VALUES (1,1,'auction_server',1,1,0,1577808000,1577808000,1577808000,1577808000,1577808000,9,10,22,22,23,3,7,'活动名','activity.icon','activity','活动描述'),(2,2,'boss_server',1,1,0,1577808000,1577808000,1577808000,1577808000,1577808000,9,10,22,22,23,3,7,'活动名','activity.icon','activity','活动描述'),(3,4,'',1,1,0,1577808000,1577808000,1577808000,1577808000,1577808000,9,10,22,22,23,3,7,'活动名','activity.icon','activity','活动描述');
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
  `gold` bigint(20) unsigned NOT NULL DEFAULT 0 COMMENT '金币',
  `silver` bigint(20) unsigned NOT NULL DEFAULT 0 COMMENT '银币',
  `copper` bigint(20) unsigned NOT NULL DEFAULT 0 COMMENT '铜币',
  `coin` bigint(20) unsigned NOT NULL DEFAULT 0 COMMENT '硬币',
  `exp` bigint(20) unsigned NOT NULL DEFAULT 0 COMMENT '经验',
  PRIMARY KEY (`role_id`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci ROW_FORMAT=DYNAMIC COMMENT='角色资产表';
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `asset`
--

LOCK TABLES `asset` WRITE;
/*!40000 ALTER TABLE `asset` DISABLE KEYS */;
INSERT INTO `asset` VALUES (1,1000000,1000000,1000000,1011200,1000000),(2,0,0,0,0,0),(3,0,0,0,0,0),(4,0,0,0,0,0),(5,0,0,0,0,0),(6,0,0,0,0,0),(7,0,0,0,0,0),(8,0,0,0,0,0),(9,0,0,0,0,0),(10,0,0,0,0,0),(11,0,0,0,0,0),(12,0,0,0,0,0),(13,0,0,0,0,0),(14,0,0,0,0,0),(15,0,0,0,0,0),(16,0,0,0,0,0),(17,0,0,0,0,0),(18,0,0,0,0,0),(19,0,0,0,0,0),(20,0,0,0,0,0),(21,0,0,0,0,0),(22,0,0,0,0,0),(23,0,0,0,0,0),(24,0,0,0,0,0),(25,0,0,0,0,0),(26,0,0,0,0,0),(27,0,0,0,0,0),(28,0,0,0,0,0),(29,0,0,0,0,0),(30,0,0,0,0,0),(31,0,0,0,0,0),(32,0,0,0,0,0),(33,0,0,0,0,0),(34,0,0,0,0,0),(35,0,0,0,0,0),(36,0,0,0,0,0),(37,0,0,0,0,0),(38,0,0,0,0,0),(39,0,0,0,0,0),(40,0,0,0,0,0),(41,0,0,0,0,0),(42,0,0,0,0,0),(43,0,0,0,0,0),(44,0,0,0,0,0),(45,0,0,0,0,0),(46,0,0,0,0,0),(47,0,0,0,0,0),(48,0,0,0,0,0),(49,0,0,0,0,0),(50,0,0,0,0,0),(51,0,0,0,0,0),(52,0,0,0,0,0),(53,0,0,0,0,0),(54,0,0,0,0,0),(55,0,0,0,0,0),(56,0,0,0,0,0),(57,0,0,0,0,0),(58,0,0,0,0,0),(59,0,0,0,0,0),(60,0,0,0,0,0),(61,0,0,0,0,0),(62,0,0,0,0,0),(63,0,0,0,0,0),(64,0,0,0,0,0),(65,0,0,0,0,0),(66,0,0,0,0,0),(67,0,0,0,0,0),(68,0,0,0,0,0),(69,0,0,0,0,0),(70,0,0,0,0,0),(71,0,0,0,0,0),(72,0,0,0,0,0),(73,0,0,0,0,0),(74,0,0,0,0,0),(75,0,0,0,0,0),(76,0,0,0,0,0),(77,0,0,0,0,0),(78,0,0,0,0,0),(79,0,0,0,0,0),(80,0,0,0,0,0),(81,0,0,0,0,0),(82,0,0,0,0,0),(83,0,0,0,0,0),(84,0,0,0,0,0),(85,0,0,0,0,0),(86,0,0,0,0,0),(87,0,0,0,0,0),(88,0,0,0,0,0),(89,0,0,0,0,0),(90,0,0,0,0,0),(91,0,0,0,0,0),(92,0,0,0,0,0),(93,0,0,0,0,0),(94,0,0,0,0,0),(95,0,0,0,0,0),(96,0,0,0,0,0),(97,0,0,0,0,0),(98,0,0,0,0,0),(99,0,0,0,0,0),(100,0,0,0,0,0);
/*!40000 ALTER TABLE `asset` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `asset_data`
--

DROP TABLE IF EXISTS `asset_data`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `asset_data` (
  `asset` varchar(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '资产类型',
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
-- Table structure for table `attribute_data`
--

DROP TABLE IF EXISTS `attribute_data`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `attribute_data` (
  `attribute_id` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '属性ID',
  `attribute` varchar(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '属性',
  `type` varchar(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '类型(固定值/万分比)',
  `merge` varchar(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '合并计算公式',
  `effect` varchar(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '效果',
  `name` char(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '名字',
  `description` char(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '描述',
  PRIMARY KEY (`attribute_id`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci ROW_FORMAT=DYNAMIC COMMENT='属性配置表';
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `attribute_data`
--

LOCK TABLES `attribute_data` WRITE;
/*!40000 ALTER TABLE `attribute_data` DISABLE KEYS */;
INSERT INTO `attribute_data` VALUES (1,'fc','fix','fc','','战力','战力'),(2,'hp','fix','','','血量','血量'),(3,'attack','fix','attack','','攻击','攻击'),(4,'defense','fix','defense','','防御','防御'),(5,'health','fix','health','','生命','生命'),(6,'hit','fix','hit','','命中','命中'),(7,'duck','fix','duck','','闪避','闪避'),(8,'freeze','fix','','cannot_be_attack','冰冻','冰冻'),(9,'destroy','fix','','','毁灭','毁灭'),(10,'vertigo','fix','','','眩晕','眩晕');
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
  `type` tinyint(1) unsigned NOT NULL DEFAULT 0 COMMENT '拍卖类型(1:全服/2:公会)',
  `bid_type` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '竞拍类型(1:竞价/2:一口价)',
  `start_time` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '开始时间',
  `end_time` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '结束时间',
  `from` varchar(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '物品来源',
  `bid_number` smallint(5) unsigned NOT NULL DEFAULT 0 COMMENT '加价次数',
  `now_price` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '当前价格',
  `next_price` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '下次出价的价格',
  `seller_list` varchar(0) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '卖家列表(default([]))',
  `bidder_list` varchar(0) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '买家列表(default([]))',
  `guild_id` bigint(20) unsigned NOT NULL DEFAULT 0 COMMENT '公会ID',
  `timer` varchar(0) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '定时器',
  `flag` varchar(0) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '标识(flag)',
  PRIMARY KEY (`auction_no`) USING BTREE
) ENGINE=InnoDB AUTO_INCREMENT=3 DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci ROW_FORMAT=DYNAMIC COMMENT='拍卖信息表';
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `auction`
--

LOCK TABLES `auction` WRITE;
/*!40000 ALTER TABLE `auction` DISABLE KEYS */;
INSERT INTO `auction` VALUES (2,0,0,0,0,0,0,'',0,0,0,'','',0,'','');
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
  `role_name` char(16) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '获得者名字',
  `server_id` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '获得者服ID',
  `time` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '时间',
  PRIMARY KEY (`id`) USING BTREE,
  KEY `role_id` (`role_id`) USING BTREE,
  KEY `time` (`time`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci ROW_FORMAT=COMPRESSED COMMENT='拍卖日志表';
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
  `auction_no` bigint(20) unsigned NOT NULL DEFAULT 0 COMMENT '拍品编号(delete_no)',
  `server_id` smallint(5) unsigned NOT NULL DEFAULT 0 COMMENT '服务器ID',
  `role_id` int(20) unsigned NOT NULL DEFAULT 0 COMMENT '出价者ID',
  `role_name` char(16) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '出价者名字',
  `guild_id` int(20) unsigned NOT NULL DEFAULT 0 COMMENT '出价者公会ID',
  `guild_name` char(16) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '出价者公会名字',
  `type` tinyint(1) unsigned NOT NULL DEFAULT 0 COMMENT '角色类型(1:卖家/2:买家)',
  `price` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '当前价格',
  `time` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '时间',
  `flag` varchar(0) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '标识(flag)',
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
-- Table structure for table `buff`
--

DROP TABLE IF EXISTS `buff`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `buff` (
  `role_id` bigint(20) unsigned NOT NULL DEFAULT 0 COMMENT '角色ID(select)',
  `buff_id` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '状态增益ID',
  `expire_time` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '结束时间',
  `overlap` int(10) unsigned NOT NULL DEFAULT 1 COMMENT '叠加数',
  `flag` varchar(0) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '标识(flag)',
  PRIMARY KEY (`role_id`,`buff_id`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci ROW_FORMAT=DYNAMIC COMMENT='角色buff表';
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `buff`
--

LOCK TABLES `buff` WRITE;
/*!40000 ALTER TABLE `buff` DISABLE KEYS */;
INSERT INTO `buff` VALUES (1,1,0,1,''),(1,2,0,1,'');
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
  `time` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '有效时间',
  `effect` varchar(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '效果',
  `temporary` tinyint(1) unsigned NOT NULL DEFAULT 0 COMMENT '是否临时的(切地图失效)',
  `overlap_type` tinyint(1) unsigned NOT NULL DEFAULT 0 COMMENT '叠加类型(0:不叠加/1:时间/2:数值/3:都叠加)',
  `name` char(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '名字',
  `description` varchar(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '描述',
  PRIMARY KEY (`buff_id`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci ROW_FORMAT=DYNAMIC COMMENT='buff配置表';
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `buff_data`
--

LOCK TABLES `buff_data` WRITE;
/*!40000 ALTER TABLE `buff_data` DISABLE KEYS */;
INSERT INTO `buff_data` VALUES (1,1,0,'[9]',0,1,'铜币',''),(2,2,60,'[10]',0,2,'经验',''),(3,3,120,'[9]',0,3,'经验',''),(4,4,0,'[10]',0,2,'经验',''),(5,5,0,'[10]',0,1,'经验',''),(6,6,0,'[9]',0,0,'铜币','');
/*!40000 ALTER TABLE `buff_data` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `client_error_log`
--

DROP TABLE IF EXISTS `client_error_log`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `client_error_log` (
  `id` bigint(20) unsigned NOT NULL AUTO_INCREMENT COMMENT '编号',
  `server_id` smallint(5) unsigned NOT NULL DEFAULT 0 COMMENT '服务器ID',
  `account` char(16) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '账号',
  `role_id` bigint(20) unsigned NOT NULL DEFAULT 0 COMMENT '玩家ID',
  `role_name` char(16) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '玩家名',
  `env` varchar(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '环境',
  `title` varchar(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '标题',
  `content` varchar(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '内容',
  `content_kernel` varchar(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '内核内容',
  `ip` varchar(16) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT 'IP地址',
  `time` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '时间',
  PRIMARY KEY (`id`) USING BTREE,
  KEY `role_id` (`role_id`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci ROW_FORMAT=COMPRESSED COMMENT='客户端错误日志表';
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `client_error_log`
--

LOCK TABLES `client_error_log` WRITE;
/*!40000 ALTER TABLE `client_error_log` DISABLE KEYS */;
/*!40000 ALTER TABLE `client_error_log` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `count`
--

DROP TABLE IF EXISTS `count`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `count` (
  `role_id` bigint(20) unsigned NOT NULL DEFAULT 0 COMMENT '角色ID(select)',
  `type` int(64) unsigned NOT NULL DEFAULT 0 COMMENT '计数类型',
  `today_number` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '今天数量',
  `week_number` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '今周数量',
  `total_number` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '总数',
  `time` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '时间',
  `flag` varchar(0) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '标识(flag)',
  PRIMARY KEY (`role_id`,`type`) USING BTREE,
  KEY `type` (`type`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci ROW_FORMAT=DYNAMIC COMMENT='角色计数表';
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `count`
--

LOCK TABLES `count` WRITE;
/*!40000 ALTER TABLE `count` DISABLE KEYS */;
INSERT INTO `count` VALUES (1,1,0,0,1,1578540442,''),(1,2,0,0,1,1578540442,''),(1,3,0,0,1,1578540442,'');
/*!40000 ALTER TABLE `count` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `dungeon`
--

DROP TABLE IF EXISTS `dungeon`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `dungeon` (
  `role_id` bigint(20) unsigned NOT NULL DEFAULT 0 COMMENT '玩家ID(select)',
  `dungeon_id` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '副本ID',
  `type` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '类型',
  `today_number` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '今天次数',
  `total_number` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '历史总次数',
  `flag` varchar(0) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '标识(flag)',
  PRIMARY KEY (`role_id`,`type`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci ROW_FORMAT=DYNAMIC COMMENT='角色副本表';
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `dungeon`
--

LOCK TABLES `dungeon` WRITE;
/*!40000 ALTER TABLE `dungeon` DISABLE KEYS */;
INSERT INTO `dungeon` VALUES (1,1,1,0,1,'');
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
  `event` varchar(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '事件(validate(event))',
  `condition` varchar(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '条件',
  `cost` varchar(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '消耗',
  `day_number` varchar(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '每日次数',
  `buy_number` varchar(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '购买次数',
  `module` varchar(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '代码模块(validate(module))',
  `function` varchar(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '入口函数(validate(function))',
  `map_id` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '地图ID',
  `monsters` varchar(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '怪物',
  `boss` varchar(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT 'Boss',
  `time` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '时间',
  `award` varchar(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '奖励',
  `name` char(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '名字',
  `description` char(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '描述',
  PRIMARY KEY (`dungeon_id`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci ROW_FORMAT=DYNAMIC COMMENT='副本配置表';
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `dungeon_data`
--

LOCK TABLES `dungeon_data` WRITE;
/*!40000 ALTER TABLE `dungeon_data` DISABLE KEYS */;
INSERT INTO `dungeon_data` VALUES (1,1,'event_dungeon_passed','[{level,10}]','[{100004,100}]','[{0,1},{1,2},{2,3},{3,4},{4,5},{5,6}]','[{0,1,100},{1,2,200},{2,3,300},{3,4,400},{4,5,500},{5,6,600}]','dungeon_map','start',100001,'[{1,10},{1,20},{1,10},{1,20},{2,1}]','',600,'[{100005,100}]','经验副本','经验副本'),(2,1,'event_dungeon_passed','[{level,20}]','[{100004,200}]','[{0,1},{1,2},{2,3},{3,4},{4,5},{5,6}]','[{0,1,100},{1,2,200},{2,3,300},{3,4,400},{4,5,500},{5,6,600}]','dungeon_map','start',100002,'[{1,10},{1,20},{1,10},{1,20},{2,1}]','',600,'[{100005,200}]','经验副本','经验副本'),(3,1,'event_dungeon_passed','[{level,30}]','[{100004,300}]','[{0,1},{1,2},{2,3},{3,4},{4,5},{5,6}]','[{0,1,100},{1,2,200},{2,3,300},{3,4,400},{4,5,500},{5,6,600}]','dungeon_map','start',100003,'[{1,10},{1,20},{1,10},{1,20},{2,1}]','',600,'[{100005,300}]','经验副本','经验副本'),(4,2,'event_dungeon_passed','[{level,10}]','[{100004,100}]','[{0,1},{1,2},{2,3},{3,4},{4,5},{5,6}]','[{0,1,100},{1,2,200},{2,3,300},{3,4,400},{4,5,500},{5,6,600}]','dungeon_map','start',200001,'[{1,10},{1,20},{1,10},{1,20},{2,1}]','',600,'[{100003,100}]','铜币副本','铜币副本'),(5,2,'event_dungeon_passed','[{level,20}]','[{100004,200}]','[{0,1},{1,2},{2,3},{3,4},{4,5},{5,6}]','[{0,1,100},{1,2,200},{2,3,300},{3,4,400},{4,5,500},{5,6,600}]','dungeon_map','start',200002,'[{1,10},{1,20},{1,10},{1,20},{2,1}]','',600,'[{100003,200}]','铜币副本','铜币副本'),(6,2,'event_dungeon_passed','[{level,30}]','[{100004,300}]','[{0,1},{1,2},{2,3},{3,4},{4,5},{5,6}]','[{0,1,100},{1,2,200},{2,3,300},{3,4,400},{4,5,500},{5,6,600}]','dungeon_map','start',200003,'[{1,10},{1,20},{1,10},{1,20},{2,1}]','',600,'[{100003,300}]','铜币副本','铜币副本');
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
  `type` varchar(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '类型(validate(effect_type))',
  `scope` varchar(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '作用范围(validate(effect_scope))',
  `condition` varchar(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '条件',
  `ratio` varchar(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '概率',
  `restrict` varchar(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '约束',
  `operation` varchar(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '操作(validate(effect_operation))',
  `object` varchar(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '作用对象(validate(effect_object))',
  `attribute` varchar(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '操作属性(validate(effect_attribute))',
  `field` varchar(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '操作属性字段(validate(effect_field))',
  `value` varchar(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '属性值',
  `time` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '效果时间',
  `extra` varchar(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '额外',
  `description` varchar(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '描述',
  PRIMARY KEY (`effect_id`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci ROW_FORMAT=DYNAMIC COMMENT='作用效果配置表';
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `effect_data`
--

LOCK TABLES `effect_data` WRITE;
/*!40000 ALTER TABLE `effect_data` DISABLE KEYS */;
INSERT INTO `effect_data` VALUES (1,'active','battle','','10000','_','add','self','hurt','','Hurt * 1.8',0,'','增加80%伤害'),(2,'active','battle','','10000','_','add','self','hurt','','Hurt * 1.5',0,'','增加50%伤害'),(3,'active','battle','SelfAttribute.hp == 0','10000','_','add','self','attribute','hp','Self.Attribute.total_hp',0,'','死亡立即复活'),(4,'active','battle','','10000','_','set','self','attribute','vertigo','0',0,'','清除眩晕'),(5,'active','battle','','10000','_','reduce','rival','attribute','hp','Rival.Attribute.total_hp * (50 / 10000)',5,'','每秒扣血，总血量万分之50'),(6,'active','battle','','10000','_','add','mate','attribute','attack','Mate.Attribute.attack * 1.5',3,'','增加队友攻击150%'),(7,'active','battle','','10000','_','add','mate','attribute','defense','Mate.Attribute.defense * 1.5',3,'','增加队友防御150%'),(8,'active','battle','','10000','_','add','self','buff','','[1]',0,'','添加Buff'),(9,'active','user','','10000','_','add','self','asset','copper','1.5',0,'','增加150%铜币'),(10,'active','user','','10000','_','add','self','asset','exp','2',0,'','增加200%经验');
/*!40000 ALTER TABLE `effect_data` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `friend`
--

DROP TABLE IF EXISTS `friend`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `friend` (
  `role_id` bigint(20) unsigned NOT NULL DEFAULT 0 COMMENT '用户ID(select)',
  `friend_id` bigint(20) unsigned NOT NULL DEFAULT 0 COMMENT '好友ID(join(`role`.`role_id`)/join(`vip`.`role_id`))',
  `friend_name` char(0) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '好友名字(join(`role`.`role_name`))',
  `sex` varchar(0) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '好友性别(join(`role`.`sex`)/default(0))',
  `classes` varchar(0) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '好友职业(join(`role`.`classes`)/default(0))',
  `vip_level` varchar(0) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT 'VIP等级(join(`vip`.`vip_level`)/default(0))',
  `online` varchar(0) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '好友在线状态(join(`role`.`online`)/default(0))',
  `relation` tinyint(1) unsigned NOT NULL DEFAULT 0 COMMENT '友好状态(0:申请/1:好友/2:黑名单)',
  `time` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '时间',
  `flag` varchar(0) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '标识(flag)',
  PRIMARY KEY (`role_id`,`friend_id`) USING BTREE,
  KEY `friend_id` (`friend_id`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci ROW_FORMAT=DYNAMIC COMMENT='角色好友表';
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `friend`
--

LOCK TABLES `friend` WRITE;
/*!40000 ALTER TABLE `friend` DISABLE KEYS */;
INSERT INTO `friend` VALUES (1,2,'','','','','',1,0,''),(2,1,'','','','','',1,0,'');
/*!40000 ALTER TABLE `friend` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `guild`
--

DROP TABLE IF EXISTS `guild`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `guild` (
  `guild_id` bigint(20) unsigned NOT NULL AUTO_INCREMENT COMMENT '公会id',
  `exp` bigint(20) unsigned NOT NULL DEFAULT 0 COMMENT '经验',
  `wealth` bigint(20) unsigned NOT NULL DEFAULT 0 COMMENT '财富',
  `level` tinyint(1) unsigned NOT NULL DEFAULT 0 COMMENT '等级',
  `create_time` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '时间(once)',
  `guild_name` char(16) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '名字((once)/(update_name))',
  `notice` char(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '公告((once)/(update_notice))',
  `leader_id` bigint(20) unsigned NOT NULL DEFAULT 0 COMMENT '会长id(join(`role`.`role_id`)/join(`vip`.`role_id`))',
  `leader_name` char(0) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '会长名字(join(`role`.`role_name`))',
  `leader_sex` varchar(0) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '性别(join(`role`.`sex`)/default(0))',
  `leader_class` varchar(0) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '会长名字(join(`role`.`classes`))',
  `leader_level` varchar(0) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '职业(join(`role`.`level`)/default(0))',
  `leader_vip_level` varchar(0) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '会长名字(join(`vip`.`vip_level`))',
  `flag` varchar(0) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '标识(flag)',
  PRIMARY KEY (`guild_id`) USING BTREE
) ENGINE=InnoDB AUTO_INCREMENT=6 DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci ROW_FORMAT=DYNAMIC COMMENT='公会表';
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `guild`
--

LOCK TABLES `guild` WRITE;
/*!40000 ALTER TABLE `guild` DISABLE KEYS */;
INSERT INTO `guild` VALUES (1,1,100,1,1577808000,'1','1',1,'','','','','',''),(2,2,200,2,1577894400,'2','2',2,'','','','','',''),(3,3,300,3,1577980800,'3','3',3,'','','','','','');
/*!40000 ALTER TABLE `guild` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `guild_apply`
--

DROP TABLE IF EXISTS `guild_apply`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `guild_apply` (
  `guild_id` bigint(20) unsigned NOT NULL DEFAULT 0 COMMENT '公会ID(join(`guild`.`guild_id`)/(delete_guild_id))',
  `role_id` bigint(20) unsigned NOT NULL DEFAULT 0 COMMENT '角色ID(join(`role`.`role_id`)/join(`vip`.`role_id`)/(delete_role_id))',
  `apply_time` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '时间',
  `guild_name` char(0) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '帮派名(join(`guild`.`guild_name`))',
  `role_name` char(0) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '角色名(join(`role`.`role_name`))',
  `sex` varchar(0) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '性别(join(`role`.`sex`)/default(0))',
  `classes` varchar(0) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '职业(join(`role`.`classes`)/default(0))',
  `level` varchar(0) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '职业(join(`role`.`level`)/default(0))',
  `vip_level` varchar(0) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT 'VIP等级(join(`vip`.`vip_level`)/default(0))',
  `flag` varchar(0) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '标识(flag)',
  PRIMARY KEY (`guild_id`,`role_id`) USING BTREE,
  KEY `role_id` (`role_id`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci ROW_FORMAT=DYNAMIC COMMENT='公会申请表';
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `guild_apply`
--

LOCK TABLES `guild_apply` WRITE;
/*!40000 ALTER TABLE `guild_apply` DISABLE KEYS */;
INSERT INTO `guild_apply` VALUES (1,7,1578326400,'','','','','','',''),(1,8,1578412800,'','','','','','',''),(2,9,1578499200,'','','','','','',''),(3,10,1578585600,'','','','','','','');
/*!40000 ALTER TABLE `guild_apply` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `guild_level_data`
--

DROP TABLE IF EXISTS `guild_level_data`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `guild_level_data` (
  `level` bigint(20) unsigned NOT NULL DEFAULT 0 COMMENT '等级',
  `exp` bigint(20) unsigned NOT NULL DEFAULT 0 COMMENT '经验'
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci ROW_FORMAT=DYNAMIC COMMENT='等级配置表';
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
  `guild_id` int(20) unsigned NOT NULL DEFAULT 0 COMMENT '公会ID(join(`guild`.`guild_id`))',
  `role_id` int(20) unsigned NOT NULL DEFAULT 0 COMMENT '角色ID(join(`role`.`role_id`)/join(`vip`.`role_id`))',
  `job` tinyint(1) unsigned NOT NULL DEFAULT 0 COMMENT '职位',
  `wealth` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '财富',
  `join_time` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '加入时间',
  `leave_time` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '离开时间',
  `guild_name` char(0) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '帮派名(join(`guild`.`guild_name`))',
  `role_name` char(0) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '角色名(join(`role`.`role_name`))',
  `sex` varchar(0) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '性别(join(`role`.`sex`)/default(0))',
  `classes` varchar(0) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '职业(join(`role`.`classes`)/default(0))',
  `level` varchar(0) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '职业(join(`role`.`level`)/default(0))',
  `vip_level` varchar(0) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT 'VIP等级(join(`vip`.`vip_level`)/default(0))',
  `flag` varchar(0) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '标识(flag)',
  PRIMARY KEY (`role_id`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci ROW_FORMAT=DYNAMIC COMMENT='公会角色表';
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `guild_role`
--

LOCK TABLES `guild_role` WRITE;
/*!40000 ALTER TABLE `guild_role` DISABLE KEYS */;
INSERT INTO `guild_role` VALUES (1,1,1,100,1577808000,0,'','','','','','',''),(2,2,1,200,1577894400,0,'','','','','','',''),(3,3,1,300,1577980800,0,'','','','','','',''),(1,4,2,400,1578067200,0,'','','','','','',''),(1,5,5,500,1578153600,0,'','','','','','',''),(2,6,2,600,1578240000,0,'','','','','','','');
/*!40000 ALTER TABLE `guild_role` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `impeach`
--

DROP TABLE IF EXISTS `impeach`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `impeach` (
  `id` bigint(20) unsigned NOT NULL AUTO_INCREMENT COMMENT '编号',
  `server_id` smallint(5) unsigned NOT NULL DEFAULT 0 COMMENT '举报方玩家服号',
  `role_id` bigint(20) unsigned NOT NULL DEFAULT 0 COMMENT '举报方玩家ID',
  `role_name` char(16) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '举报方玩家名字',
  `impeach_server_id` smallint(5) unsigned NOT NULL DEFAULT 0 COMMENT '被举报玩家服号',
  `impeach_role_id` bigint(20) unsigned NOT NULL DEFAULT 0 COMMENT '被举报玩家ID',
  `impeach_role_name` char(16) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '被举报玩家名字',
  `type` tinyint(1) unsigned NOT NULL DEFAULT 0 COMMENT '举报类型(1:言语辱骂他人/2:盗取他人账号/3:非正规充值交易/4:其他)',
  `content` varchar(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '举报内容',
  `time` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '时间',
  PRIMARY KEY (`id`) USING BTREE,
  KEY `impeach_role_server` (`impeach_role_id`,`impeach_server_id`) USING BTREE,
  KEY `role_server` (`role_id`,`server_id`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci ROW_FORMAT=DYNAMIC COMMENT='举报信息表';
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `impeach`
--

LOCK TABLES `impeach` WRITE;
/*!40000 ALTER TABLE `impeach` DISABLE KEYS */;
/*!40000 ALTER TABLE `impeach` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `increment`
--

DROP TABLE IF EXISTS `increment`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `increment` (
  `name` char(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '0' COMMENT '名字',
  `value` bigint(20) unsigned NOT NULL DEFAULT 0 COMMENT '数值',
  PRIMARY KEY (`name`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci ROW_FORMAT=DYNAMIC COMMENT='自增表';
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `increment`
--

LOCK TABLES `increment` WRITE;
/*!40000 ALTER TABLE `increment` DISABLE KEYS */;
INSERT INTO `increment` VALUES ('increment_server',0),('map',10),('monster',10010);
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
  `role_id` bigint(20) unsigned NOT NULL DEFAULT 0 COMMENT '角色ID((select)/(once))',
  `item_id` int(20) unsigned NOT NULL DEFAULT 0 COMMENT '物品ID(once)',
  `type` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '类型',
  `number` int(20) unsigned NOT NULL DEFAULT 1 COMMENT '数量',
  `expire_time` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '过期时间',
  `flag` varchar(0) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '标识(flag)',
  PRIMARY KEY (`item_no`) USING BTREE,
  KEY `role_id` (`role_id`) USING BTREE
) ENGINE=InnoDB AUTO_INCREMENT=28 DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci ROW_FORMAT=DYNAMIC COMMENT='角色物品表';
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `item`
--

LOCK TABLES `item` WRITE;
/*!40000 ALTER TABLE `item` DISABLE KEYS */;
INSERT INTO `item` VALUES (3,1,2,1,7,0,''),(4,1,3,1,10,0,''),(5,1,4,2,1,0,''),(6,1,5,3,1,0,'');
/*!40000 ALTER TABLE `item` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `item_consume_log`
--

DROP TABLE IF EXISTS `item_consume_log`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `item_consume_log` (
  `id` int(10) unsigned NOT NULL AUTO_INCREMENT COMMENT 'ID',
  `role_id` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '角色ID',
  `item_id` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '物品ID',
  `operation` varchar(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '操作',
  `source` varchar(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '来源',
  `time` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '时间',
  PRIMARY KEY (`id`) USING BTREE,
  KEY `role_id` (`role_id`) USING BTREE,
  KEY `time` (`time`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci ROW_FORMAT=COMPRESSED COMMENT='物品消费日志表';
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
  `type` tinyint(3) unsigned NOT NULL DEFAULT 0 COMMENT '类型(validate(item_type))',
  `overlap` int(10) unsigned NOT NULL DEFAULT 1 COMMENT '叠加数',
  `category` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '分类ID',
  `time` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '有效时间',
  `use_number` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '使用数量(0:不能直接使用/1:一个/N:N个)',
  `use_effect` varchar(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '使用效果(validate(use_effect))',
  `use_value` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '使用效果数值',
  `name` char(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '名字',
  `icon` char(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '图标',
  `description` char(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '描述',
  PRIMARY KEY (`item_id`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci ROW_FORMAT=DYNAMIC COMMENT='物品配置表';
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `item_data`
--

LOCK TABLES `item_data` WRITE;
/*!40000 ALTER TABLE `item_data` DISABLE KEYS */;
INSERT INTO `item_data` VALUES (1,1,1000,0,0,0,'',0,'rust','file_type_rust.svg',''),(2,1,100,0,0,0,'',0,'erlang','file_type_erlang.svg',''),(3,1,10,0,0,0,'',0,'php','file_type_php.svg',''),(4,2,1,0,0,0,'',0,'lua','file_type_lua.svg',''),(5,2,1,0,0,0,'',0,'js','file_type_js.svg',''),(6,2,1,0,0,0,'',0,'html','file_type_html.svg',''),(7,2,1,0,0,0,'',0,'css','file_type_css.svg',''),(100001,10,1,0,0,0,'gold',0,'gold','file_type_gold.svg',''),(100002,10,1,0,0,0,'sliver',0,'silver','file_type_sliver.svg',''),(100003,10,1,0,0,0,'copper',0,'copper','file_type_copper.svg',''),(100004,10,1,0,0,0,'exp',0,'exp','file_type_exp.svg',''),(100005,10,1,0,0,0,'coin',0,'coin','file_type_coin.svg','');
/*!40000 ALTER TABLE `item_data` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `item_produce_log`
--

DROP TABLE IF EXISTS `item_produce_log`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `item_produce_log` (
  `id` int(10) unsigned NOT NULL AUTO_INCREMENT COMMENT 'ID',
  `role_id` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '角色ID',
  `item_id` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '物品ID',
  `operation` varchar(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '操作',
  `source` varchar(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '来源',
  `time` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '时间',
  PRIMARY KEY (`id`) USING BTREE,
  KEY `role_id` (`role_id`) USING BTREE,
  KEY `time` (`time`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci ROW_FORMAT=COMPRESSED COMMENT='物品产出日志表';
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
  `key` varchar(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '码',
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
  `unique` varchar(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '是否唯一(validate(boolean))',
  `award` varchar(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '奖励',
  PRIMARY KEY (`type`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci ROW_FORMAT=DYNAMIC COMMENT='兑换码奖励配置表';
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `key_award_data`
--

LOCK TABLES `key_award_data` WRITE;
/*!40000 ALTER TABLE `key_award_data` DISABLE KEYS */;
INSERT INTO `key_award_data` VALUES (1,'0','[{700001,1},{700002,2},{700003,3}]'),(2,'0','[{700001,1},{700002,2},{700003,3}]');
/*!40000 ALTER TABLE `key_award_data` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `key_data`
--

DROP TABLE IF EXISTS `key_data`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `key_data` (
  `key` char(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '码',
  `type` tinyint(1) unsigned NOT NULL DEFAULT 0 COMMENT '类型',
  PRIMARY KEY (`key`) USING BTREE,
  KEY `key` (`key`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci ROW_FORMAT=DYNAMIC COMMENT='兑换码配置表';
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `key_data`
--

LOCK TABLES `key_data` WRITE;
/*!40000 ALTER TABLE `key_data` DISABLE KEYS */;
/*!40000 ALTER TABLE `key_data` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `level_data`
--

DROP TABLE IF EXISTS `level_data`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `level_data` (
  `level` bigint(20) unsigned NOT NULL DEFAULT 0 COMMENT '等级',
  `exp` bigint(20) unsigned NOT NULL DEFAULT 0 COMMENT '经验'
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
  `ip` varchar(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '登录IP',
  `device_id` varchar(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '设备ID',
  `login_time` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '登录时间',
  `online_time` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '在线时间',
  `time` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '登出时间',
  PRIMARY KEY (`id`) USING BTREE,
  KEY `role_id` (`role_id`) USING BTREE,
  KEY `time` (`time`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci ROW_FORMAT=COMPRESSED COMMENT='登录日志';
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
  `lucky_money_id` bigint(20) unsigned NOT NULL AUTO_INCREMENT COMMENT '红包ID',
  `server_id` smallint(5) unsigned NOT NULL DEFAULT 0 COMMENT '服务器ID',
  `role_id` bigint(20) unsigned NOT NULL DEFAULT 0 COMMENT '角色ID',
  `role_name` char(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '角色名',
  `guild_id` bigint(20) unsigned NOT NULL DEFAULT 0 COMMENT '公会ID',
  `guild_name` char(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '公会名',
  `total_gold` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '总金币',
  `remain_gold` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '剩余金币',
  `total_number` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '总人数',
  `receive_number` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '已领取人数',
  `receive_list` varchar(0) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '领取列表',
  `time` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '发送时间',
  `flag` varchar(0) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '标识(flag)',
  PRIMARY KEY (`lucky_money_id`) USING BTREE
) ENGINE=InnoDB AUTO_INCREMENT=2 DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci ROW_FORMAT=DYNAMIC COMMENT='红包信息表';
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `lucky_money`
--

LOCK TABLES `lucky_money` WRITE;
/*!40000 ALTER TABLE `lucky_money` DISABLE KEYS */;
INSERT INTO `lucky_money` VALUES (1,1,1,'1',1,'1',100,50,2,1,'',1583829641,'');
/*!40000 ALTER TABLE `lucky_money` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `lucky_money_role`
--

DROP TABLE IF EXISTS `lucky_money_role`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `lucky_money_role` (
  `lucky_money_id` bigint(20) unsigned NOT NULL DEFAULT 0 COMMENT '红包ID',
  `server_id` smallint(5) unsigned NOT NULL DEFAULT 0 COMMENT '服务器ID',
  `role_id` bigint(20) unsigned NOT NULL DEFAULT 0 COMMENT '角色ID',
  `role_name` char(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '角色名',
  `guild_id` bigint(20) unsigned NOT NULL DEFAULT 0 COMMENT '公会ID',
  `guild_name` char(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '公会名',
  `gold` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '领取金币数',
  `time` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '领取时间',
  `flag` varchar(0) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '标识(flag)',
  PRIMARY KEY (`lucky_money_id`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci ROW_FORMAT=DYNAMIC COMMENT='红包角色表';
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `lucky_money_role`
--

LOCK TABLES `lucky_money_role` WRITE;
/*!40000 ALTER TABLE `lucky_money_role` DISABLE KEYS */;
INSERT INTO `lucky_money_role` VALUES (1,1,1,'1',1,'1',50,1583829662,'');
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
  `sender_id` bigint(20) unsigned NOT NULL DEFAULT 0 COMMENT '发送者',
  `sender_nick` char(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '发送者昵称',
  `receiver_id` bigint(20) unsigned NOT NULL DEFAULT 0 COMMENT '接收者(select)',
  `receiver_nick` char(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '接受者昵称',
  `receive_time` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '接收时间',
  `is_read` tinyint(1) unsigned NOT NULL DEFAULT 0 COMMENT '是否已经读取(update_read)',
  `read_time` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '读取时间(update_read)',
  `expire_time` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '过期时间',
  `is_receive_attachment` tinyint(1) unsigned NOT NULL DEFAULT 0 COMMENT '是否领取附件(update_receive)',
  `receive_attachment_time` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '领取附件时间(update_receive)',
  `from` varchar(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '来源',
  `title` char(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '标题',
  `content` char(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '内容',
  `attachment` varchar(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '附件',
  `flag` varchar(0) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '标识(flag)',
  PRIMARY KEY (`mail_id`) USING BTREE,
  KEY `receiver_id` (`receiver_id`) USING BTREE
) ENGINE=InnoDB AUTO_INCREMENT=2 DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci ROW_FORMAT=DYNAMIC COMMENT='角色邮件表';
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `mail`
--

LOCK TABLES `mail` WRITE;
/*!40000 ALTER TABLE `mail` DISABLE KEYS */;
INSERT INTO `mail` VALUES (1,0,'',1,'1',0,0,0,0,0,0,'','标题','内容','[{1,1},{2,2},{3,3}]','');
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
  `type` varchar(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '广播类型(validate(map_type))',
  `reconnect` varchar(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '是否重连(validate(boolean))',
  `monsters` varchar(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '随地图启动的怪物',
  `rank_key` varchar(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '榜键类型(validate(map_rank_key))',
  `rank_value` varchar(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '榜值类型(validate(map_rank_value))',
  `rank_mode` varchar(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '榜模式(validate(map_rank_mode))',
  `enter_points` varchar(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '进入点',
  `pk_mode` varchar(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT 'PK模式',
  `enter_script` varchar(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '进入脚本',
  `relive_script` varchar(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '复活脚本',
  `leave_script` varchar(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '离开脚本',
  PRIMARY KEY (`map_id`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci ROW_FORMAT=DYNAMIC COMMENT='地图配置表';
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `map_data`
--

LOCK TABLES `map_data` WRITE;
/*!40000 ALTER TABLE `map_data` DISABLE KEYS */;
INSERT INTO `map_data` VALUES (100000,'slice','false','','role','hurt','share','','','','',''),(100001,'full','false','','guild','hurt','share','','','','',''),(100002,'full','false','','team','hurt','share','','','','',''),(100003,'full','false','','camp','hurt','share','','','','',''),(200001,'full','false','','role','hurt','share','','','','',''),(200002,'full','false','','guild','hurt','share','','','','',''),(200003,'full','false','','team','hurt','share','','','','',''),(200004,'slice','true','','camp','hurt','share','','','','','');
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
  `type` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '组ID',
  `name` char(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '怪物名称',
  `description` char(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '怪物描述',
  `level` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '等级',
  `hp` bigint(20) unsigned NOT NULL DEFAULT 0 COMMENT '血量',
  `map_id` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '地图ID',
  `camp` tinyint(1) unsigned NOT NULL DEFAULT 0 COMMENT '阵营',
  `range` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '攻击距离',
  `distance` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '搜索距离',
  `relive_time` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '复活时间',
  `act_type` varchar(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '动作类型(validate(act_type))',
  `act_script` varchar(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '动作脚本(enemy:敌人/role:玩家/monster:怪物/{monster,组ID}:特定怪物)',
  `skills` varchar(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '技能',
  `born_points` varchar(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '出生点',
  `award` varchar(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '奖励',
  PRIMARY KEY (`monster_id`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci ROW_FORMAT=DYNAMIC COMMENT='怪物配置表';
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `monster_data`
--

LOCK TABLES `monster_data` WRITE;
/*!40000 ALTER TABLE `monster_data` DISABLE KEYS */;
INSERT INTO `monster_data` VALUES (1,1,'active','active',1,100,100001,1,1,300,0,'active','[role]','[5]','[{10,10},{20,10},{30,10},{40,10},{50,10},{60,10},{70,10},{10,10},{90,10},{100,10}]','[{100005,100}]'),(2,2,'passive','passive',1,200,100001,1,2,300,0,'passive','[enemy]','','[{40,10}]','[{100005,200}]'),(3,3,'movable','movable',1,300,0,1,3,300,0,'movable','','','[{60,10}]','[{100005,300}]'),(4,4,'fix','fix',1,400,0,1,4,300,0,'fix','','','[{80,10}]',''),(5,5,'act','act',1,500,0,1,5,300,0,'fix','[enemy]','','[{100,10}]',''),(6,6,'boom','boom',1,600,0,1,6,300,0,'active','[{monster, 20}, {monster, 50}, role]','','[{120,10}]','[{100005,600}]');
/*!40000 ALTER TABLE `monster_data` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `node_data`
--

DROP TABLE IF EXISTS `node_data`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `node_data` (
  `server_node` varchar(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '游戏服节点',
  `server_name` varchar(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '游戏服名',
  `server_host` varchar(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '游戏服域名',
  `server_ip` varchar(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '游戏服IP',
  `server_port` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '游戏服端口',
  `server_id` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '游戏服编号',
  `server_type` varchar(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '服务器类型',
  `center_node` varchar(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '中央服节点',
  `center_name` varchar(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '中央服名',
  `center_host` varchar(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '中央服域名',
  `center_ip` varchar(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '中央服IP',
  `center_port` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '中央服端口',
  `center_id` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '中央服编号',
  PRIMARY KEY (`server_node`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci ROW_FORMAT=DYNAMIC COMMENT='节点配置表';
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `node_data`
--

LOCK TABLES `node_data` WRITE;
/*!40000 ALTER TABLE `node_data` DISABLE KEYS */;
INSERT INTO `node_data` VALUES ('center','小跨服','','',0,10001,'center','','','','',0,0),('dev','开发服','','',10004,4,'local','center','','','',0,0),('local','本地服','','',10001,1,'local','center','','','',0,0),('publish','版署服','','',10005,5,'local','center','','','',0,0),('stable','稳定服','','',10002,2,'local','center','','','',0,0),('test','测试服','','',10003,3,'local','center','','','',0,0),('world','大世界','','',0,0,'world','','','','',0,0);
/*!40000 ALTER TABLE `node_data` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `online_log`
--

DROP TABLE IF EXISTS `online_log`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `online_log` (
  `id` bigint(20) unsigned NOT NULL AUTO_INCREMENT COMMENT 'ID',
  `all` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '全部',
  `online` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '在线',
  `hosting` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '挂机',
  `hour` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '当前小时',
  `time` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '当前时间',
  PRIMARY KEY (`id`) USING BTREE,
  KEY `time` (`time`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci ROW_FORMAT=COMPRESSED COMMENT='在线统计日志';
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
  `key` varchar(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '参数键',
  `value` varchar(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '参数值',
  `description` char(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '参数名称',
  PRIMARY KEY (`key`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci ROW_FORMAT=DYNAMIC COMMENT='游戏参数配置表';
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `parameter_data`
--

LOCK TABLES `parameter_data` WRITE;
/*!40000 ALTER TABLE `parameter_data` DISABLE KEYS */;
INSERT INTO `parameter_data` VALUES ('chat_cd','30','聊天冷却时间'),('chat_level','10','聊天开放等级'),('friend_level','30','好友开放等级'),('friend_number','50','好友上限'),('guild_create','[{1, [{level, 10}, {vip, 0}, {gold, 0}]}, {2, [{level, 50}, {vip, 1}, {gold, 100}]},{3, [{level, 100}, {vip, 3}, {gold, 500}]}]','创建一级公会条件'),('guild_create_cd','86400','公会创建冷却时间'),('guild_join_cd','86400','公会加入冷却时间'),('guild_member_limit','[{0, 50}, {1, 60}, {2, 70}, {3, 80}, {4, 90}, {5, 100}]','公会人员数'),('language','sc','默认语言'),('language_set','[{1, sc}, {2, tc}, {3, en}, {4, kr}, {5, vi}]','支持语言'),('login_cd','180','登录时间间隔'),('time_zone','+8','时区');
/*!40000 ALTER TABLE `parameter_data` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `quest`
--

DROP TABLE IF EXISTS `quest`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `quest` (
  `role_id` bigint(20) unsigned NOT NULL DEFAULT 0 COMMENT '角色ID(select)',
  `quest_id` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '任务ID',
  `type` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '类型',
  `event` varchar(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '事件',
  `target` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '目标',
  `number` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '数量',
  `compare` varchar(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '比较',
  `award` tinyint(1) unsigned NOT NULL DEFAULT 0 COMMENT '是否领取奖励',
  `flag` varchar(0) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '标识(flag)',
  PRIMARY KEY (`role_id`,`type`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci ROW_FORMAT=DYNAMIC COMMENT='角色任务表';
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `quest`
--

LOCK TABLES `quest` WRITE;
/*!40000 ALTER TABLE `quest` DISABLE KEYS */;
INSERT INTO `quest` VALUES (1,1,1,'event_kill_monster',0,0,'nc',1,''),(1,1001,2,'event_dungeon_passed',100,1,'ge',0,''),(1,100001,3,'event_shop_buy',1,1,'eq',0,'');
/*!40000 ALTER TABLE `quest` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `quest_data`
--

DROP TABLE IF EXISTS `quest_data`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `quest_data` (
  `quest_id` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '任务ID',
  `type` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '类型',
  `pre_id` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '前置任务',
  `next_id` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '后置任务',
  `module` varchar(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '模块(validate(module))',
  `function` varchar(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '函数(validate(function))',
  `event` varchar(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '事件(validate(event))',
  `compare` varchar(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '比较模式(validate(compare))',
  `target` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '目标',
  `number` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '数量',
  `condition` varchar(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '条件',
  `cost` varchar(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '消耗',
  `award` varchar(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '奖励',
  `title` char(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '标题',
  `content` char(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '内容',
  `description` char(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '描述',
  PRIMARY KEY (`quest_id`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci ROW_FORMAT=DYNAMIC COMMENT='任务配置表';
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `quest_data`
--

LOCK TABLES `quest_data` WRITE;
/*!40000 ALTER TABLE `quest_data` DISABLE KEYS */;
INSERT INTO `quest_data` VALUES (1,1,0,2,'','','event_kill_monster','nc',0,3,'','','[{1,1}]','','',''),(2,1,1,3,'role','check_quest','event_level_upgrade','ge',5,1,'','[{100003, 100}]','[{1,10}]','','',''),(3,1,2,4,'','','event_dungeon_passed','ge',100001,1,'[{level, 10}]','','[{1,100}]','','',''),(4,1,3,5,'','','event_shop_buy','eq',1,1,'','','[{1,1000}]','','',''),(5,1,4,0,'','','event_guild_join','nc',0,1,'','','[{1,1000}]','','',''),(6,1,5,0,'friend','check_quest','event_friend_add','nc',0,5,'','','[{1,10}]','','',''),(1001,2,0,1002,'','','event_dungeon_passed','ge',100,1,'','','[{1,10}]','','',''),(1002,2,1001,0,'','','event_friend_add','eq',1,1,'','','[{1,10}]','','',''),(100001,3,0,100002,'shop','check_quest','event_shop_buy','eq',1,1,'','','[{1,10}]','','',''),(100002,3,100001,0,'','','event_guild_join','nc',0,1,'','','[{1,10}]','','','');
/*!40000 ALTER TABLE `quest_data` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `quest_log`
--

DROP TABLE IF EXISTS `quest_log`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `quest_log` (
  `id` int(10) unsigned NOT NULL AUTO_INCREMENT COMMENT 'ID',
  `role_id` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '角色ID',
  `quest_id` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '任务ID',
  `time` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '时间',
  PRIMARY KEY (`id`) USING BTREE,
  KEY `role_id` (`role_id`) USING BTREE,
  KEY `time` (`time`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci ROW_FORMAT=COMPRESSED COMMENT='任务日志表';
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `quest_log`
--

LOCK TABLES `quest_log` WRITE;
/*!40000 ALTER TABLE `quest_log` DISABLE KEYS */;
/*!40000 ALTER TABLE `quest_log` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `rank`
--

DROP TABLE IF EXISTS `rank`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `rank` (
  `type` tinyint(3) unsigned NOT NULL DEFAULT 0 COMMENT '类型(select)(delete_type)',
  `order` bigint(20) unsigned NOT NULL DEFAULT 0 COMMENT '排名',
  `key` bigint(20) unsigned NOT NULL DEFAULT 0 COMMENT '键',
  `value` bigint(20) unsigned NOT NULL DEFAULT 0 COMMENT '值',
  `time` int(20) unsigned NOT NULL DEFAULT 0 COMMENT '时间',
  `name` char(16) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '名字',
  `server_id` smallint(5) unsigned NOT NULL DEFAULT 0 COMMENT '服务器ID',
  `digest` varchar(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '摘要数据',
  `extra` varchar(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '额外数据',
  `other` varchar(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '其他数据',
  `flag` varchar(0) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '标识((flag)/default(1))',
  PRIMARY KEY (`type`,`order`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci ROW_FORMAT=DYNAMIC COMMENT='角色排行表';
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `rank`
--

LOCK TABLES `rank` WRITE;
/*!40000 ALTER TABLE `rank` DISABLE KEYS */;
INSERT INTO `rank` VALUES (1,1,1,1,1,'1',1,'','','',''),(1,2,2,2,2,'2',1,'','','',''),(1,3,3,3,3,'3',1,'','','',''),(1,4,4,4,4,'4',1,'','','',''),(1,5,5,5,5,'5',1,'','','',''),(1,6,6,6,6,'6',1,'','','',''),(1,7,7,7,7,'7',1,'','','','');
/*!40000 ALTER TABLE `rank` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `recharge`
--

DROP TABLE IF EXISTS `recharge`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `recharge` (
  `recharge_no` bigint(11) unsigned NOT NULL AUTO_INCREMENT COMMENT '充值编号',
  `recharge_id` int(11) unsigned NOT NULL DEFAULT 0 COMMENT '充值ID',
  `channel_id` smallint(5) unsigned NOT NULL DEFAULT 0 COMMENT '渠道ID',
  `server_id` smallint(5) unsigned NOT NULL DEFAULT 0 COMMENT '区服ID',
  `role_id` bigint(20) unsigned NOT NULL DEFAULT 0 COMMENT '玩家ID',
  `role_name` char(16) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '玩家名称',
  `account` char(16) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '平台账号ID',
  `money` decimal(10,2) unsigned NOT NULL DEFAULT 0.00 COMMENT '充值金额',
  `gold` int(11) unsigned NOT NULL DEFAULT 0 COMMENT '金币',
  `status` tinyint(1) unsigned NOT NULL DEFAULT 0 COMMENT '状态(0:未发放/1:已发放)',
  `time` int(11) unsigned NOT NULL DEFAULT 0 COMMENT '订单时间',
  `receive_time` int(11) unsigned NOT NULL DEFAULT 0 COMMENT '发放时间',
  PRIMARY KEY (`recharge_no`) USING BTREE,
  KEY `role_id` (`role_id`,`status`) USING BTREE,
  KEY `channel_id` (`channel_id`) USING BTREE,
  KEY `time` (`time`) USING BTREE
) ENGINE=InnoDB AUTO_INCREMENT=11 DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci ROW_FORMAT=DYNAMIC COMMENT='角色充值订单表';
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `recharge`
--

LOCK TABLES `recharge` WRITE;
/*!40000 ALTER TABLE `recharge` DISABLE KEYS */;
INSERT INTO `recharge` VALUES (1,1,1,1,1,'1','1',6.00,0,0,1577808000,0),(2,2,1,1,1,'1','1',18.00,0,0,1577894400,0),(3,3,1,1,1,'1','1',68.00,0,0,1577980800,0),(4,4,1,1,1,'1','1',128.00,0,0,1578067200,0),(5,5,1,1,2,'2','2',268.00,0,0,1578153600,0),(6,6,1,1,2,'2','2',588.00,0,0,1577808000,0),(7,7,1,1,2,'2','2',688.00,0,0,1577808000,0),(8,8,1,1,3,'3','3',888.00,0,0,1577894400,0),(9,9,1,1,3,'3','3',1288.00,0,0,1577980800,0),(10,10,1,1,4,'4','4',8888.00,0,0,1577808000,0);
/*!40000 ALTER TABLE `recharge` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `recharge_data`
--

DROP TABLE IF EXISTS `recharge_data`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `recharge_data` (
  `recharge_id` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '充值ID',
  `type` tinyint(3) unsigned NOT NULL DEFAULT 0 COMMENT '类型(普通充值:0/购买月卡:1)',
  `channel_id` tinyint(3) unsigned NOT NULL DEFAULT 0 COMMENT '渠道ID',
  `limit` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '限制数量',
  `original_price` decimal(10,2) unsigned NOT NULL DEFAULT 0.00 COMMENT '原价',
  `now_price` decimal(10,2) unsigned NOT NULL DEFAULT 0.00 COMMENT '现价',
  `gold` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '金币',
  `gift_gold` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '赠送金币',
  `begin_open_days` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '结束时间，跟开服相关，填天数',
  `end_open_days` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '结束时间，跟开服相关，填天数',
  `sort` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '排序',
  `icon` char(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '图片',
  `name` char(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '名字',
  `description` char(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '描述',
  PRIMARY KEY (`recharge_id`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci ROW_FORMAT=DYNAMIC COMMENT='充值配置表';
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `recharge_data`
--

LOCK TABLES `recharge_data` WRITE;
/*!40000 ALTER TABLE `recharge_data` DISABLE KEYS */;
INSERT INTO `recharge_data` VALUES (1,3,0,1,6.00,6.00,6,0,1,9999,1,'0','至尊神兵宝箱',''),(2,1,0,1,18.00,18.00,18,5,1,9999,2,'1','元宝',''),(3,1,0,1,68.00,68.00,68,40,1,9999,3,'2','元宝',''),(4,1,0,1,128.00,128.00,128,90,1,9999,4,'3','元宝',''),(5,1,0,1,268.00,268.00,268,190,1,9999,5,'4','元宝',''),(6,1,0,1,588.00,588.00,588,330,1,9999,6,'5','元宝',''),(7,1,0,1,688.00,688.00,688,590,1,9999,7,'6','元宝',''),(8,1,0,1,888.00,888.00,888,1300,1,9999,8,'7','元宝',''),(9,2,0,1,1288.00,1288.00,1288,0,1,9999,0,'','周卡',''),(10,6,0,1,8888.00,8888.00,8888,0,1,9999,0,'','月卡','');
/*!40000 ALTER TABLE `recharge_data` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `role`
--

DROP TABLE IF EXISTS `role`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `role` (
  `role_id` bigint(20) unsigned NOT NULL AUTO_INCREMENT COMMENT '角色ID',
  `role_name` char(16) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '角色名((once)/(update_name))',
  `account` char(16) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '账户(once)',
  `type` tinyint(255) unsigned NOT NULL DEFAULT 0 COMMENT '账户类型',
  `level` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '等级',
  `sex` tinyint(1) unsigned NOT NULL DEFAULT 0 COMMENT '性别',
  `classes` tinyint(1) unsigned NOT NULL DEFAULT 0 COMMENT '职业',
  `item_size` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '普通背包大小',
  `bag_size` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '装备背包大小',
  `store_size` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '仓库背包大小',
  `online` tinyint(1) unsigned NOT NULL DEFAULT 0 COMMENT '是否在线',
  `online_time` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '在线时间',
  `register_time` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '注册时间',
  `server_id` smallint(5) unsigned NOT NULL DEFAULT 0 COMMENT '服ID',
  `channel_id` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '渠道ID',
  `map` varchar(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '地图',
  `device_id` char(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '设备ID',
  `device_type` char(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '设备类型',
  `mac` char(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT 'Mac地址',
  PRIMARY KEY (`role_id`) USING BTREE,
  KEY `account` (`account`) USING BTREE,
  KEY `online_time` (`online_time`) USING BTREE,
  KEY `register_time` (`register_time`) USING BTREE
) ENGINE=InnoDB AUTO_INCREMENT=1001 DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci ROW_FORMAT=DYNAMIC COMMENT='角色信息表';
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `role`
--

LOCK TABLES `role` WRITE;
/*!40000 ALTER TABLE `role` DISABLE KEYS */;
INSERT INTO `role` VALUES (1,'1','1',3,100,1,1,100,100,100,0,1590198510,1577808000,1,1,'','','',''),(2,'2','2',2,200,2,2,100,100,100,0,1590198496,1577894400,1,1,'','','',''),(3,'3','3',2,300,1,3,100,100,100,0,1590198508,1577980800,1,1,'','','',''),(4,'4','4',1,400,2,4,100,100,100,0,1590198508,1578067200,1,1,'','','',''),(5,'5','5',1,500,1,5,100,100,100,0,1590198510,1578153600,1,1,'','','',''),(6,'6','6',2,600,2,1,100,100,100,0,1590198508,1578240000,1,1,'','','',''),(7,'7','7',3,700,2,3,100,100,100,0,1590198501,1578326400,1,1,'','','',''),(8,'8','8',1,800,2,4,100,100,100,0,1590198508,1578412800,1,1,'','','',''),(9,'9','9',3,900,1,5,100,100,100,0,1590198508,1578499200,1,1,'','','',''),(10,'10','10',2,1000,2,2,100,100,100,0,1590198507,1578585600,1,1,'','','',''),(11,'11','11',2,100,1,1,100,100,100,0,1590198508,1578672000,1,1,'','','',''),(12,'12','12',2,200,2,2,100,100,100,0,1590198508,1578758400,1,1,'','','',''),(13,'13','13',2,300,1,3,100,100,100,0,1590198508,1578844800,1,1,'','','',''),(14,'14','14',2,400,2,4,100,100,100,0,1590198509,1578931200,1,1,'','','',''),(15,'15','15',2,500,1,5,100,100,100,0,1590198508,1579017600,1,1,'','','',''),(16,'16','16',2,600,2,1,100,100,100,0,1590198508,1579104000,1,1,'','','',''),(17,'17','17',2,700,2,3,100,100,100,0,1590198508,1579190400,1,1,'','','',''),(18,'18','18',2,800,2,4,100,100,100,0,1590198510,1579276800,1,1,'','','',''),(19,'19','19',2,900,1,5,100,100,100,0,1590198515,1579363200,1,1,'','','',''),(20,'20','20',2,1000,2,2,100,100,100,0,1590198519,1579449600,1,1,'','','',''),(21,'21','21',2,100,1,1,100,100,100,0,1590198510,1579536000,1,1,'','','',''),(22,'22','22',2,200,2,2,100,100,100,0,1590198521,1579622400,1,1,'','','',''),(23,'23','23',2,300,1,3,100,100,100,0,1590198515,1579708800,1,1,'','','',''),(24,'24','24',2,400,2,4,100,100,100,0,1590198510,1579795200,1,1,'','','',''),(25,'25','25',2,500,1,5,100,100,100,0,1590198518,1579881600,1,1,'','','',''),(26,'26','26',2,600,2,1,100,100,100,0,1590198508,1579968000,1,1,'','','',''),(27,'27','27',2,700,2,3,100,100,100,0,1590198518,1580054400,1,1,'','','',''),(28,'28','28',2,800,2,4,100,100,100,0,1590198519,1580140800,1,1,'','','',''),(29,'29','29',2,900,1,5,100,100,100,0,1590198515,1580227200,1,1,'','','',''),(30,'30','30',2,1000,2,2,100,100,100,0,1590198521,1580313600,1,1,'','','',''),(31,'31','31',2,100,1,1,100,100,100,0,1590198521,1580400000,1,1,'','','',''),(32,'32','32',2,200,2,2,100,100,100,0,1590198519,1580486400,1,1,'','','',''),(33,'33','33',2,300,1,3,100,100,100,0,1590198515,1580572800,1,1,'','','',''),(34,'34','34',2,400,2,4,100,100,100,0,1590198508,1580659200,1,1,'','','',''),(35,'35','35',2,500,1,5,100,100,100,0,1590198518,1580745600,1,1,'','','',''),(36,'36','36',2,600,2,1,100,100,100,0,1590198519,1580832000,1,1,'','','',''),(37,'37','37',2,700,2,3,100,100,100,0,1590198515,1580918400,1,1,'','','',''),(38,'38','38',2,800,2,4,100,100,100,0,1590198519,1581004800,1,1,'','','',''),(39,'39','39',2,900,1,5,100,100,100,0,1590198519,1581091200,1,1,'','','',''),(40,'40','40',2,1000,2,2,100,100,100,0,1590198521,1581177600,1,1,'','','',''),(41,'41','41',2,100,1,1,100,100,100,0,1590198516,1581264000,1,1,'','','',''),(42,'42','42',2,200,2,2,100,100,100,0,1590198515,1581350400,1,1,'','','',''),(43,'43','43',2,300,1,3,100,100,100,0,1590198521,1581436800,1,1,'','','',''),(44,'44','44',2,400,2,4,100,100,100,0,1590198512,1581523200,1,1,'','','',''),(45,'45','45',2,500,1,5,100,100,100,0,1590198520,1581609600,1,1,'','','',''),(46,'46','46',2,600,2,1,100,100,100,0,1590198515,1581696000,1,1,'','','',''),(47,'47','47',2,700,2,3,100,100,100,0,1590198508,1581782400,1,1,'','','',''),(48,'48','48',2,800,2,4,100,100,100,0,1590198521,1581868800,1,1,'','','',''),(49,'49','49',2,900,1,5,100,100,100,0,1590198515,1581955200,1,1,'','','',''),(50,'50','50',2,1000,2,2,100,100,100,0,1590198515,1582041600,1,1,'','','',''),(51,'51','51',2,100,1,1,100,100,100,0,1590198521,1582128000,1,1,'','','',''),(52,'52','52',2,200,2,2,100,100,100,0,1590198519,1582214400,1,1,'','','',''),(53,'53','53',2,300,1,3,100,100,100,0,1590198515,1582300800,1,1,'','','',''),(54,'54','54',2,400,2,4,100,100,100,0,1590198521,1582387200,1,1,'','','',''),(55,'55','55',2,500,1,5,100,100,100,0,1590198518,1582473600,1,1,'','','',''),(56,'56','56',2,600,2,1,100,100,100,0,1590198521,1582560000,1,1,'','','',''),(57,'57','57',2,700,2,3,100,100,100,0,1590198510,1582646400,1,1,'','','',''),(58,'58','58',2,800,2,4,100,100,100,0,1590198517,1582732800,1,1,'','','',''),(59,'59','59',2,900,1,5,100,100,100,0,1590198510,1582819200,1,1,'','','',''),(60,'60','60',2,1000,2,2,100,100,100,0,1590198518,1582905600,1,1,'','','',''),(61,'61','61',2,100,1,1,100,100,100,0,1590198518,1582992000,1,1,'','','',''),(62,'62','62',2,200,2,2,100,100,100,0,1590198508,1583078400,1,1,'','','',''),(63,'63','63',2,300,1,3,100,100,100,0,1590198509,1583164800,1,1,'','','',''),(64,'64','64',2,400,2,4,100,100,100,0,1590198510,1583251200,1,1,'','','',''),(65,'65','65',2,500,1,5,100,100,100,0,1590198515,1583337600,1,1,'','','',''),(66,'66','66',2,600,2,1,100,100,100,0,1590198515,1583424000,1,1,'','','',''),(67,'67','67',2,700,2,3,100,100,100,0,1590198515,1583510400,1,1,'','','',''),(68,'68','68',2,800,2,4,100,100,100,0,1590198519,1583596800,1,1,'','','',''),(69,'69','69',2,900,1,5,100,100,100,0,1590198520,1583683200,1,1,'','','',''),(70,'70','70',2,1000,2,2,100,100,100,0,1590198507,1583769600,1,1,'','','',''),(71,'71','71',2,100,1,1,100,100,100,0,1590198518,1583856000,1,1,'','','',''),(72,'72','72',2,200,2,2,100,100,100,0,1590198508,1583942400,1,1,'','','',''),(73,'73','73',2,300,1,3,100,100,100,0,1590198508,1584028800,1,1,'','','',''),(74,'74','74',2,400,2,4,100,100,100,0,1590198516,1584115200,1,1,'','','',''),(75,'75','75',2,500,1,5,100,100,100,0,1590198518,1584201600,1,1,'','','',''),(76,'76','76',2,600,2,1,100,100,100,0,1590198508,1584288000,1,1,'','','',''),(77,'77','77',2,700,2,3,100,100,100,0,1590198515,1584374400,1,1,'','','',''),(78,'78','78',2,800,2,4,100,100,100,0,1590198518,1584460800,1,1,'','','',''),(79,'79','79',2,900,1,5,100,100,100,0,1590198520,1584547200,1,1,'','','',''),(80,'80','80',2,1000,2,2,100,100,100,0,1590198510,1584633600,1,1,'','','',''),(81,'81','81',2,100,1,1,100,100,100,0,1590198519,1584720000,1,1,'','','',''),(82,'82','82',2,200,2,2,100,100,100,0,1590198508,1584806400,1,1,'','','',''),(83,'83','83',2,300,1,3,100,100,100,0,1590198515,1584892800,1,1,'','','',''),(84,'84','84',2,400,2,4,100,100,100,0,1590198519,1584979200,1,1,'','','',''),(85,'85','85',2,500,1,5,100,100,100,0,1590198521,1585065600,1,1,'','','',''),(86,'86','86',2,600,2,1,100,100,100,0,1590198519,1585152000,1,1,'','','',''),(87,'87','87',2,700,1,3,100,100,100,0,1590198519,1585238400,1,1,'','','',''),(88,'88','88',2,800,2,4,100,100,100,0,1590198508,1585324800,1,1,'','','',''),(89,'89','89',2,900,2,5,100,100,100,0,1590198508,1585411200,1,1,'','','',''),(90,'90','90',2,1000,2,2,100,100,100,0,1590198510,1585497600,1,1,'','','',''),(91,'91','91',2,100,1,1,100,100,100,0,1590198515,1585584000,1,1,'','','',''),(92,'92','92',2,200,2,2,100,100,100,0,1590198518,1585670400,1,1,'','','',''),(93,'93','93',2,300,2,3,100,100,100,0,1590198515,1585756800,1,1,'','','',''),(94,'94','94',2,400,1,4,100,100,100,0,1590198521,1585843200,1,1,'','','',''),(95,'95','95',2,500,2,5,100,100,100,0,1590198520,1585929600,1,1,'','','',''),(96,'96','96',2,600,1,1,100,100,100,0,1590198515,1586016000,1,1,'','','',''),(97,'97','97',2,700,2,3,100,100,100,0,1590198508,1586102400,1,1,'','','',''),(98,'98','98',2,800,1,4,100,100,100,0,1590198518,1586188800,1,1,'','','',''),(99,'99','99',2,900,2,5,100,100,100,0,1590198510,1586275200,1,1,'','','',''),(100,'100','100',2,1000,1,2,100,100,100,0,1590198508,1586361600,1,1,'','','',''),(101,'101','101',3,100,1,1,100,100,100,0,1590198510,1577808000,1,1,'','','',''),(102,'102','102',2,200,2,2,100,100,100,0,1590198496,1577894400,1,1,'','','',''),(103,'103','103',2,300,1,3,100,100,100,0,1590198508,1577980800,1,1,'','','',''),(104,'104','104',1,400,2,4,100,100,100,0,1590198508,1578067200,1,1,'','','',''),(105,'105','105',1,500,1,5,100,100,100,0,1590198510,1578153600,1,1,'','','',''),(106,'106','106',2,600,2,1,100,100,100,0,1590198508,1578240000,1,1,'','','',''),(107,'107','107',3,700,2,3,100,100,100,0,1590198501,1578326400,1,1,'','','',''),(108,'108','108',1,800,2,4,100,100,100,0,1590198508,1578412800,1,1,'','','',''),(109,'109','109',3,900,1,5,100,100,100,0,1590198508,1578499200,1,1,'','','',''),(110,'110','110',2,1000,2,2,100,100,100,0,1590198507,1578585600,1,1,'','','',''),(111,'111','111',2,100,1,1,100,100,100,0,1590198508,1578672000,1,1,'','','',''),(112,'112','112',2,200,2,2,100,100,100,0,1590198508,1578758400,1,1,'','','',''),(113,'113','113',2,300,1,3,100,100,100,0,1590198508,1578844800,1,1,'','','',''),(114,'114','114',2,400,2,4,100,100,100,0,1590198509,1578931200,1,1,'','','',''),(115,'115','115',2,500,1,5,100,100,100,0,1590198508,1579017600,1,1,'','','',''),(116,'116','116',2,600,2,1,100,100,100,0,1590198508,1579104000,1,1,'','','',''),(117,'117','117',2,700,2,3,100,100,100,0,1590198508,1579190400,1,1,'','','',''),(118,'118','118',2,800,2,4,100,100,100,0,1590198510,1579276800,1,1,'','','',''),(119,'119','119',2,900,1,5,100,100,100,0,1590198515,1579363200,1,1,'','','',''),(120,'120','120',2,1000,2,2,100,100,100,0,1590198519,1579449600,1,1,'','','',''),(121,'121','121',2,100,1,1,100,100,100,0,1590198510,1579536000,1,1,'','','',''),(122,'122','122',2,200,2,2,100,100,100,0,1590198521,1579622400,1,1,'','','',''),(123,'123','123',2,300,1,3,100,100,100,0,1590198515,1579708800,1,1,'','','',''),(124,'124','124',2,400,2,4,100,100,100,0,1590198510,1579795200,1,1,'','','',''),(125,'125','125',2,500,1,5,100,100,100,0,1590198518,1579881600,1,1,'','','',''),(126,'126','126',2,600,2,1,100,100,100,0,1590198508,1579968000,1,1,'','','',''),(127,'127','127',2,700,2,3,100,100,100,0,1590198518,1580054400,1,1,'','','',''),(128,'128','128',2,800,2,4,100,100,100,0,1590198519,1580140800,1,1,'','','',''),(129,'129','129',2,900,1,5,100,100,100,0,1590198515,1580227200,1,1,'','','',''),(130,'130','130',2,1000,2,2,100,100,100,0,1590198521,1580313600,1,1,'','','',''),(131,'131','131',2,100,1,1,100,100,100,0,1590198521,1580400000,1,1,'','','',''),(132,'132','132',2,200,2,2,100,100,100,0,1590198519,1580486400,1,1,'','','',''),(133,'133','133',2,300,1,3,100,100,100,0,1590198515,1580572800,1,1,'','','',''),(134,'134','134',2,400,2,4,100,100,100,0,1590198508,1580659200,1,1,'','','',''),(135,'135','135',2,500,1,5,100,100,100,0,1590198518,1580745600,1,1,'','','',''),(136,'136','136',2,600,2,1,100,100,100,0,1590198519,1580832000,1,1,'','','',''),(137,'137','137',2,700,2,3,100,100,100,0,1590198515,1580918400,1,1,'','','',''),(138,'138','138',2,800,2,4,100,100,100,0,1590198519,1581004800,1,1,'','','',''),(139,'139','139',2,900,1,5,100,100,100,0,1590198519,1581091200,1,1,'','','',''),(140,'140','140',2,1000,2,2,100,100,100,0,1590198521,1581177600,1,1,'','','',''),(141,'141','141',2,100,1,1,100,100,100,0,1590198516,1581264000,1,1,'','','',''),(142,'142','142',2,200,2,2,100,100,100,0,1590198515,1581350400,1,1,'','','',''),(143,'143','143',2,300,1,3,100,100,100,0,1590198521,1581436800,1,1,'','','',''),(144,'144','144',2,400,2,4,100,100,100,0,1590198512,1581523200,1,1,'','','',''),(145,'145','145',2,500,1,5,100,100,100,0,1590198520,1581609600,1,1,'','','',''),(146,'146','146',2,600,2,1,100,100,100,0,1590198515,1581696000,1,1,'','','',''),(147,'147','147',2,700,2,3,100,100,100,0,1590198508,1581782400,1,1,'','','',''),(148,'148','148',2,800,2,4,100,100,100,0,1590198521,1581868800,1,1,'','','',''),(149,'149','149',2,900,1,5,100,100,100,0,1590198515,1581955200,1,1,'','','',''),(150,'150','150',2,1000,2,2,100,100,100,0,1590198515,1582041600,1,1,'','','',''),(151,'151','151',2,100,1,1,100,100,100,0,1590198521,1582128000,1,1,'','','',''),(152,'152','152',2,200,2,2,100,100,100,0,1590198519,1582214400,1,1,'','','',''),(153,'153','153',2,300,1,3,100,100,100,0,1590198515,1582300800,1,1,'','','',''),(154,'154','154',2,400,2,4,100,100,100,0,1590198521,1582387200,1,1,'','','',''),(155,'155','155',2,500,1,5,100,100,100,0,1590198518,1582473600,1,1,'','','',''),(156,'156','156',2,600,2,1,100,100,100,0,1590198521,1582560000,1,1,'','','',''),(157,'157','157',2,700,2,3,100,100,100,0,1590198510,1582646400,1,1,'','','',''),(158,'158','158',2,800,2,4,100,100,100,0,1590198517,1582732800,1,1,'','','',''),(159,'159','159',2,900,1,5,100,100,100,0,1590198510,1582819200,1,1,'','','',''),(160,'160','160',2,1000,2,2,100,100,100,0,1590198518,1582905600,1,1,'','','',''),(161,'161','161',2,100,1,1,100,100,100,0,1590198518,1582992000,1,1,'','','',''),(162,'162','162',2,200,2,2,100,100,100,0,1590198508,1583078400,1,1,'','','',''),(163,'163','163',2,300,1,3,100,100,100,0,1590198509,1583164800,1,1,'','','',''),(164,'164','164',2,400,2,4,100,100,100,0,1590198510,1583251200,1,1,'','','',''),(165,'165','165',2,500,1,5,100,100,100,0,1590198515,1583337600,1,1,'','','',''),(166,'166','166',2,600,2,1,100,100,100,0,1590198515,1583424000,1,1,'','','',''),(167,'167','167',2,700,2,3,100,100,100,0,1590198515,1583510400,1,1,'','','',''),(168,'168','168',2,800,2,4,100,100,100,0,1590198519,1583596800,1,1,'','','',''),(169,'169','169',2,900,1,5,100,100,100,0,1590198520,1583683200,1,1,'','','',''),(170,'170','170',2,1000,2,2,100,100,100,0,1590198507,1583769600,1,1,'','','',''),(171,'171','171',2,100,1,1,100,100,100,0,1590198518,1583856000,1,1,'','','',''),(172,'172','172',2,200,2,2,100,100,100,0,1590198508,1583942400,1,1,'','','',''),(173,'173','173',2,300,1,3,100,100,100,0,1590198508,1584028800,1,1,'','','',''),(174,'174','174',2,400,2,4,100,100,100,0,1590198516,1584115200,1,1,'','','',''),(175,'175','175',2,500,1,5,100,100,100,0,1590198518,1584201600,1,1,'','','',''),(176,'176','176',2,600,2,1,100,100,100,0,1590198508,1584288000,1,1,'','','',''),(177,'177','177',2,700,2,3,100,100,100,0,1590198515,1584374400,1,1,'','','',''),(178,'178','178',2,800,2,4,100,100,100,0,1590198518,1584460800,1,1,'','','',''),(179,'179','179',2,900,1,5,100,100,100,0,1590198520,1584547200,1,1,'','','',''),(180,'180','180',2,1000,2,2,100,100,100,0,1590198510,1584633600,1,1,'','','',''),(181,'181','181',2,100,1,1,100,100,100,0,1590198519,1584720000,1,1,'','','',''),(182,'182','182',2,200,2,2,100,100,100,0,1590198508,1584806400,1,1,'','','',''),(183,'183','183',2,300,1,3,100,100,100,0,1590198515,1584892800,1,1,'','','',''),(184,'184','184',2,400,2,4,100,100,100,0,1590198519,1584979200,1,1,'','','',''),(185,'185','185',2,500,1,5,100,100,100,0,1590198521,1585065600,1,1,'','','',''),(186,'186','186',2,600,2,1,100,100,100,0,1590198519,1585152000,1,1,'','','',''),(187,'187','187',2,700,1,3,100,100,100,0,1590198519,1585238400,1,1,'','','',''),(188,'188','188',2,800,2,4,100,100,100,0,1590198508,1585324800,1,1,'','','',''),(189,'189','189',2,900,2,5,100,100,100,0,1590198508,1585411200,1,1,'','','',''),(190,'190','190',2,1000,2,2,100,100,100,0,1590198510,1585497600,1,1,'','','',''),(191,'191','191',2,100,1,1,100,100,100,0,1590198515,1585584000,1,1,'','','',''),(192,'192','192',2,200,2,2,100,100,100,0,1590198518,1585670400,1,1,'','','',''),(193,'193','193',2,300,2,3,100,100,100,0,1590198515,1585756800,1,1,'','','',''),(194,'194','194',2,400,1,4,100,100,100,0,1590198521,1585843200,1,1,'','','',''),(195,'195','195',2,500,2,5,100,100,100,0,1590198520,1585929600,1,1,'','','',''),(196,'196','196',2,600,1,1,100,100,100,0,1590198515,1586016000,1,1,'','','',''),(197,'197','197',2,700,2,3,100,100,100,0,1590198508,1586102400,1,1,'','','',''),(198,'198','198',2,800,1,4,100,100,100,0,1590198518,1586188800,1,1,'','','',''),(199,'199','199',2,900,2,5,100,100,100,0,1590198510,1586275200,1,1,'','','',''),(200,'200','200',2,1000,1,2,100,100,100,0,1590198508,1586361600,1,1,'','','',''),(201,'201','201',3,100,1,1,100,100,100,0,1590198510,1577808000,1,1,'','','',''),(202,'202','202',2,200,2,2,100,100,100,0,1590198496,1577894400,1,1,'','','',''),(203,'203','203',2,300,1,3,100,100,100,0,1590198508,1577980800,1,1,'','','',''),(204,'204','204',1,400,2,4,100,100,100,0,1590198508,1578067200,1,1,'','','',''),(205,'205','205',1,500,1,5,100,100,100,0,1590198510,1578153600,1,1,'','','',''),(206,'206','206',2,600,2,1,100,100,100,0,1590198508,1578240000,1,1,'','','',''),(207,'207','207',3,700,2,3,100,100,100,0,1590198501,1578326400,1,1,'','','',''),(208,'208','208',1,800,2,4,100,100,100,0,1590198508,1578412800,1,1,'','','',''),(209,'209','209',3,900,1,5,100,100,100,0,1590198508,1578499200,1,1,'','','',''),(210,'210','210',2,1000,2,2,100,100,100,0,1590198507,1578585600,1,1,'','','',''),(211,'211','211',2,100,1,1,100,100,100,0,1590198508,1578672000,1,1,'','','',''),(212,'212','212',2,200,2,2,100,100,100,0,1590198508,1578758400,1,1,'','','',''),(213,'213','213',2,300,1,3,100,100,100,0,1590198508,1578844800,1,1,'','','',''),(214,'214','214',2,400,2,4,100,100,100,0,1590198509,1578931200,1,1,'','','',''),(215,'215','215',2,500,1,5,100,100,100,0,1590198508,1579017600,1,1,'','','',''),(216,'216','216',2,600,2,1,100,100,100,0,1590198508,1579104000,1,1,'','','',''),(217,'217','217',2,700,2,3,100,100,100,0,1590198508,1579190400,1,1,'','','',''),(218,'218','218',2,800,2,4,100,100,100,0,1590198510,1579276800,1,1,'','','',''),(219,'219','219',2,900,1,5,100,100,100,0,1590198515,1579363200,1,1,'','','',''),(220,'220','220',2,1000,2,2,100,100,100,0,1590198519,1579449600,1,1,'','','',''),(221,'221','221',2,100,1,1,100,100,100,0,1590198510,1579536000,1,1,'','','',''),(222,'222','222',2,200,2,2,100,100,100,0,1590198521,1579622400,1,1,'','','',''),(223,'223','223',2,300,1,3,100,100,100,0,1590198515,1579708800,1,1,'','','',''),(224,'224','224',2,400,2,4,100,100,100,0,1590198510,1579795200,1,1,'','','',''),(225,'225','225',2,500,1,5,100,100,100,0,1590198518,1579881600,1,1,'','','',''),(226,'226','226',2,600,2,1,100,100,100,0,1590198508,1579968000,1,1,'','','',''),(227,'227','227',2,700,2,3,100,100,100,0,1590198518,1580054400,1,1,'','','',''),(228,'228','228',2,800,2,4,100,100,100,0,1590198519,1580140800,1,1,'','','',''),(229,'229','229',2,900,1,5,100,100,100,0,1590198515,1580227200,1,1,'','','',''),(230,'230','230',2,1000,2,2,100,100,100,0,1590198521,1580313600,1,1,'','','',''),(231,'231','231',2,100,1,1,100,100,100,0,1590198521,1580400000,1,1,'','','',''),(232,'232','232',2,200,2,2,100,100,100,0,1590198519,1580486400,1,1,'','','',''),(233,'233','233',2,300,1,3,100,100,100,0,1590198515,1580572800,1,1,'','','',''),(234,'234','234',2,400,2,4,100,100,100,0,1590198508,1580659200,1,1,'','','',''),(235,'235','235',2,500,1,5,100,100,100,0,1590198518,1580745600,1,1,'','','',''),(236,'236','236',2,600,2,1,100,100,100,0,1590198519,1580832000,1,1,'','','',''),(237,'237','237',2,700,2,3,100,100,100,0,1590198515,1580918400,1,1,'','','',''),(238,'238','238',2,800,2,4,100,100,100,0,1590198519,1581004800,1,1,'','','',''),(239,'239','239',2,900,1,5,100,100,100,0,1590198519,1581091200,1,1,'','','',''),(240,'240','240',2,1000,2,2,100,100,100,0,1590198521,1581177600,1,1,'','','',''),(241,'241','241',2,100,1,1,100,100,100,0,1590198516,1581264000,1,1,'','','',''),(242,'242','242',2,200,2,2,100,100,100,0,1590198515,1581350400,1,1,'','','',''),(243,'243','243',2,300,1,3,100,100,100,0,1590198521,1581436800,1,1,'','','',''),(244,'244','244',2,400,2,4,100,100,100,0,1590198512,1581523200,1,1,'','','',''),(245,'245','245',2,500,1,5,100,100,100,0,1590198520,1581609600,1,1,'','','',''),(246,'246','246',2,600,2,1,100,100,100,0,1590198515,1581696000,1,1,'','','',''),(247,'247','247',2,700,2,3,100,100,100,0,1590198508,1581782400,1,1,'','','',''),(248,'248','248',2,800,2,4,100,100,100,0,1590198521,1581868800,1,1,'','','',''),(249,'249','249',2,900,1,5,100,100,100,0,1590198515,1581955200,1,1,'','','',''),(250,'250','250',2,1000,2,2,100,100,100,0,1590198515,1582041600,1,1,'','','',''),(251,'251','251',2,100,1,1,100,100,100,0,1590198521,1582128000,1,1,'','','',''),(252,'252','252',2,200,2,2,100,100,100,0,1590198519,1582214400,1,1,'','','',''),(253,'253','253',2,300,1,3,100,100,100,0,1590198515,1582300800,1,1,'','','',''),(254,'254','254',2,400,2,4,100,100,100,0,1590198521,1582387200,1,1,'','','',''),(255,'255','255',2,500,1,5,100,100,100,0,1590198518,1582473600,1,1,'','','',''),(256,'256','256',2,600,2,1,100,100,100,0,1590198521,1582560000,1,1,'','','',''),(257,'257','257',2,700,2,3,100,100,100,0,1590198510,1582646400,1,1,'','','',''),(258,'258','258',2,800,2,4,100,100,100,0,1590198517,1582732800,1,1,'','','',''),(259,'259','259',2,900,1,5,100,100,100,0,1590198510,1582819200,1,1,'','','',''),(260,'260','260',2,1000,2,2,100,100,100,0,1590198518,1582905600,1,1,'','','',''),(261,'261','261',2,100,1,1,100,100,100,0,1590198518,1582992000,1,1,'','','',''),(262,'262','262',2,200,2,2,100,100,100,0,1590198508,1583078400,1,1,'','','',''),(263,'263','263',2,300,1,3,100,100,100,0,1590198509,1583164800,1,1,'','','',''),(264,'264','264',2,400,2,4,100,100,100,0,1590198510,1583251200,1,1,'','','',''),(265,'265','265',2,500,1,5,100,100,100,0,1590198515,1583337600,1,1,'','','',''),(266,'266','266',2,600,2,1,100,100,100,0,1590198515,1583424000,1,1,'','','',''),(267,'267','267',2,700,2,3,100,100,100,0,1590198515,1583510400,1,1,'','','',''),(268,'268','268',2,800,2,4,100,100,100,0,1590198519,1583596800,1,1,'','','',''),(269,'269','269',2,900,1,5,100,100,100,0,1590198520,1583683200,1,1,'','','',''),(270,'270','270',2,1000,2,2,100,100,100,0,1590198507,1583769600,1,1,'','','',''),(271,'271','271',2,100,1,1,100,100,100,0,1590198518,1583856000,1,1,'','','',''),(272,'272','272',2,200,2,2,100,100,100,0,1590198508,1583942400,1,1,'','','',''),(273,'273','273',2,300,1,3,100,100,100,0,1590198508,1584028800,1,1,'','','',''),(274,'274','274',2,400,2,4,100,100,100,0,1590198516,1584115200,1,1,'','','',''),(275,'275','275',2,500,1,5,100,100,100,0,1590198518,1584201600,1,1,'','','',''),(276,'276','276',2,600,2,1,100,100,100,0,1590198508,1584288000,1,1,'','','',''),(277,'277','277',2,700,2,3,100,100,100,0,1590198515,1584374400,1,1,'','','',''),(278,'278','278',2,800,2,4,100,100,100,0,1590198518,1584460800,1,1,'','','',''),(279,'279','279',2,900,1,5,100,100,100,0,1590198520,1584547200,1,1,'','','',''),(280,'280','280',2,1000,2,2,100,100,100,0,1590198510,1584633600,1,1,'','','',''),(281,'281','281',2,100,1,1,100,100,100,0,1590198519,1584720000,1,1,'','','',''),(282,'282','282',2,200,2,2,100,100,100,0,1590198508,1584806400,1,1,'','','',''),(283,'283','283',2,300,1,3,100,100,100,0,1590198515,1584892800,1,1,'','','',''),(284,'284','284',2,400,2,4,100,100,100,0,1590198519,1584979200,1,1,'','','',''),(285,'285','285',2,500,1,5,100,100,100,0,1590198521,1585065600,1,1,'','','',''),(286,'286','286',2,600,2,1,100,100,100,0,1590198519,1585152000,1,1,'','','',''),(287,'287','287',2,700,1,3,100,100,100,0,1590198519,1585238400,1,1,'','','',''),(288,'288','288',2,800,2,4,100,100,100,0,1590198508,1585324800,1,1,'','','',''),(289,'289','289',2,900,2,5,100,100,100,0,1590198508,1585411200,1,1,'','','',''),(290,'290','290',2,1000,2,2,100,100,100,0,1590198510,1585497600,1,1,'','','',''),(291,'291','291',2,100,1,1,100,100,100,0,1590198515,1585584000,1,1,'','','',''),(292,'292','292',2,200,2,2,100,100,100,0,1590198518,1585670400,1,1,'','','',''),(293,'293','293',2,300,2,3,100,100,100,0,1590198515,1585756800,1,1,'','','',''),(294,'294','294',2,400,1,4,100,100,100,0,1590198521,1585843200,1,1,'','','',''),(295,'295','295',2,500,2,5,100,100,100,0,1590198520,1585929600,1,1,'','','',''),(296,'296','296',2,600,1,1,100,100,100,0,1590198515,1586016000,1,1,'','','',''),(297,'297','297',2,700,2,3,100,100,100,0,1590198508,1586102400,1,1,'','','',''),(298,'298','298',2,800,1,4,100,100,100,0,1590198518,1586188800,1,1,'','','',''),(299,'299','299',2,900,2,5,100,100,100,0,1590198510,1586275200,1,1,'','','',''),(300,'300','300',2,1000,1,2,100,100,100,0,1590198508,1586361600,1,1,'','','',''),(301,'301','301',3,100,1,1,100,100,100,0,1590198510,1577808000,1,1,'','','',''),(302,'302','302',2,200,2,2,100,100,100,0,1590198496,1577894400,1,1,'','','',''),(303,'303','303',2,300,1,3,100,100,100,0,1590198508,1577980800,1,1,'','','',''),(304,'304','304',1,400,2,4,100,100,100,0,1590198508,1578067200,1,1,'','','',''),(305,'305','305',1,500,1,5,100,100,100,0,1590198510,1578153600,1,1,'','','',''),(306,'306','306',2,600,2,1,100,100,100,0,1590198508,1578240000,1,1,'','','',''),(307,'307','307',3,700,2,3,100,100,100,0,1590198501,1578326400,1,1,'','','',''),(308,'308','308',1,800,2,4,100,100,100,0,1590198508,1578412800,1,1,'','','',''),(309,'309','309',3,900,1,5,100,100,100,0,1590198508,1578499200,1,1,'','','',''),(310,'310','310',2,1000,2,2,100,100,100,0,1590198507,1578585600,1,1,'','','',''),(311,'311','311',2,100,1,1,100,100,100,0,1590198508,1578672000,1,1,'','','',''),(312,'312','312',2,200,2,2,100,100,100,0,1590198508,1578758400,1,1,'','','',''),(313,'313','313',2,300,1,3,100,100,100,0,1590198508,1578844800,1,1,'','','',''),(314,'314','314',2,400,2,4,100,100,100,0,1590198509,1578931200,1,1,'','','',''),(315,'315','315',2,500,1,5,100,100,100,0,1590198508,1579017600,1,1,'','','',''),(316,'316','316',2,600,2,1,100,100,100,0,1590198508,1579104000,1,1,'','','',''),(317,'317','317',2,700,2,3,100,100,100,0,1590198508,1579190400,1,1,'','','',''),(318,'318','318',2,800,2,4,100,100,100,0,1590198510,1579276800,1,1,'','','',''),(319,'319','319',2,900,1,5,100,100,100,0,1590198515,1579363200,1,1,'','','',''),(320,'320','320',2,1000,2,2,100,100,100,0,1590198519,1579449600,1,1,'','','',''),(321,'321','321',2,100,1,1,100,100,100,0,1590198510,1579536000,1,1,'','','',''),(322,'322','322',2,200,2,2,100,100,100,0,1590198521,1579622400,1,1,'','','',''),(323,'323','323',2,300,1,3,100,100,100,0,1590198515,1579708800,1,1,'','','',''),(324,'324','324',2,400,2,4,100,100,100,0,1590198510,1579795200,1,1,'','','',''),(325,'325','325',2,500,1,5,100,100,100,0,1590198518,1579881600,1,1,'','','',''),(326,'326','326',2,600,2,1,100,100,100,0,1590198508,1579968000,1,1,'','','',''),(327,'327','327',2,700,2,3,100,100,100,0,1590198518,1580054400,1,1,'','','',''),(328,'328','328',2,800,2,4,100,100,100,0,1590198519,1580140800,1,1,'','','',''),(329,'329','329',2,900,1,5,100,100,100,0,1590198515,1580227200,1,1,'','','',''),(330,'330','330',2,1000,2,2,100,100,100,0,1590198521,1580313600,1,1,'','','',''),(331,'331','331',2,100,1,1,100,100,100,0,1590198521,1580400000,1,1,'','','',''),(332,'332','332',2,200,2,2,100,100,100,0,1590198519,1580486400,1,1,'','','',''),(333,'333','333',2,300,1,3,100,100,100,0,1590198515,1580572800,1,1,'','','',''),(334,'334','334',2,400,2,4,100,100,100,0,1590198508,1580659200,1,1,'','','',''),(335,'335','335',2,500,1,5,100,100,100,0,1590198518,1580745600,1,1,'','','',''),(336,'336','336',2,600,2,1,100,100,100,0,1590198519,1580832000,1,1,'','','',''),(337,'337','337',2,700,2,3,100,100,100,0,1590198515,1580918400,1,1,'','','',''),(338,'338','338',2,800,2,4,100,100,100,0,1590198519,1581004800,1,1,'','','',''),(339,'339','339',2,900,1,5,100,100,100,0,1590198519,1581091200,1,1,'','','',''),(340,'340','340',2,1000,2,2,100,100,100,0,1590198521,1581177600,1,1,'','','',''),(341,'341','341',2,100,1,1,100,100,100,0,1590198516,1581264000,1,1,'','','',''),(342,'342','342',2,200,2,2,100,100,100,0,1590198515,1581350400,1,1,'','','',''),(343,'343','343',2,300,1,3,100,100,100,0,1590198521,1581436800,1,1,'','','',''),(344,'344','344',2,400,2,4,100,100,100,0,1590198512,1581523200,1,1,'','','',''),(345,'345','345',2,500,1,5,100,100,100,0,1590198520,1581609600,1,1,'','','',''),(346,'346','346',2,600,2,1,100,100,100,0,1590198515,1581696000,1,1,'','','',''),(347,'347','347',2,700,2,3,100,100,100,0,1590198508,1581782400,1,1,'','','',''),(348,'348','348',2,800,2,4,100,100,100,0,1590198521,1581868800,1,1,'','','',''),(349,'349','349',2,900,1,5,100,100,100,0,1590198515,1581955200,1,1,'','','',''),(350,'350','350',2,1000,2,2,100,100,100,0,1590198515,1582041600,1,1,'','','',''),(351,'351','351',2,100,1,1,100,100,100,0,1590198521,1582128000,1,1,'','','',''),(352,'352','352',2,200,2,2,100,100,100,0,1590198519,1582214400,1,1,'','','',''),(353,'353','353',2,300,1,3,100,100,100,0,1590198515,1582300800,1,1,'','','',''),(354,'354','354',2,400,2,4,100,100,100,0,1590198521,1582387200,1,1,'','','',''),(355,'355','355',2,500,1,5,100,100,100,0,1590198518,1582473600,1,1,'','','',''),(356,'356','356',2,600,2,1,100,100,100,0,1590198521,1582560000,1,1,'','','',''),(357,'357','357',2,700,2,3,100,100,100,0,1590198510,1582646400,1,1,'','','',''),(358,'358','358',2,800,2,4,100,100,100,0,1590198517,1582732800,1,1,'','','',''),(359,'359','359',2,900,1,5,100,100,100,0,1590198510,1582819200,1,1,'','','',''),(360,'360','360',2,1000,2,2,100,100,100,0,1590198518,1582905600,1,1,'','','',''),(361,'361','361',2,100,1,1,100,100,100,0,1590198518,1582992000,1,1,'','','',''),(362,'362','362',2,200,2,2,100,100,100,0,1590198508,1583078400,1,1,'','','',''),(363,'363','363',2,300,1,3,100,100,100,0,1590198509,1583164800,1,1,'','','',''),(364,'364','364',2,400,2,4,100,100,100,0,1590198510,1583251200,1,1,'','','',''),(365,'365','365',2,500,1,5,100,100,100,0,1590198515,1583337600,1,1,'','','',''),(366,'366','366',2,600,2,1,100,100,100,0,1590198515,1583424000,1,1,'','','',''),(367,'367','367',2,700,2,3,100,100,100,0,1590198515,1583510400,1,1,'','','',''),(368,'368','368',2,800,2,4,100,100,100,0,1590198519,1583596800,1,1,'','','',''),(369,'369','369',2,900,1,5,100,100,100,0,1590198520,1583683200,1,1,'','','',''),(370,'370','370',2,1000,2,2,100,100,100,0,1590198507,1583769600,1,1,'','','',''),(371,'371','371',2,100,1,1,100,100,100,0,1590198518,1583856000,1,1,'','','',''),(372,'372','372',2,200,2,2,100,100,100,0,1590198508,1583942400,1,1,'','','',''),(373,'373','373',2,300,1,3,100,100,100,0,1590198508,1584028800,1,1,'','','',''),(374,'374','374',2,400,2,4,100,100,100,0,1590198516,1584115200,1,1,'','','',''),(375,'375','375',2,500,1,5,100,100,100,0,1590198518,1584201600,1,1,'','','',''),(376,'376','376',2,600,2,1,100,100,100,0,1590198508,1584288000,1,1,'','','',''),(377,'377','377',2,700,2,3,100,100,100,0,1590198515,1584374400,1,1,'','','',''),(378,'378','378',2,800,2,4,100,100,100,0,1590198518,1584460800,1,1,'','','',''),(379,'379','379',2,900,1,5,100,100,100,0,1590198520,1584547200,1,1,'','','',''),(380,'380','380',2,1000,2,2,100,100,100,0,1590198510,1584633600,1,1,'','','',''),(381,'381','381',2,100,1,1,100,100,100,0,1590198519,1584720000,1,1,'','','',''),(382,'382','382',2,200,2,2,100,100,100,0,1590198508,1584806400,1,1,'','','',''),(383,'383','383',2,300,1,3,100,100,100,0,1590198515,1584892800,1,1,'','','',''),(384,'384','384',2,400,2,4,100,100,100,0,1590198519,1584979200,1,1,'','','',''),(385,'385','385',2,500,1,5,100,100,100,0,1590198521,1585065600,1,1,'','','',''),(386,'386','386',2,600,2,1,100,100,100,0,1590198519,1585152000,1,1,'','','',''),(387,'387','387',2,700,1,3,100,100,100,0,1590198519,1585238400,1,1,'','','',''),(388,'388','388',2,800,2,4,100,100,100,0,1590198508,1585324800,1,1,'','','',''),(389,'389','389',2,900,2,5,100,100,100,0,1590198508,1585411200,1,1,'','','',''),(390,'390','390',2,1000,2,2,100,100,100,0,1590198510,1585497600,1,1,'','','',''),(391,'391','391',2,100,1,1,100,100,100,0,1590198515,1585584000,1,1,'','','',''),(392,'392','392',2,200,2,2,100,100,100,0,1590198518,1585670400,1,1,'','','',''),(393,'393','393',2,300,2,3,100,100,100,0,1590198515,1585756800,1,1,'','','',''),(394,'394','394',2,400,1,4,100,100,100,0,1590198521,1585843200,1,1,'','','',''),(395,'395','395',2,500,2,5,100,100,100,0,1590198520,1585929600,1,1,'','','',''),(396,'396','396',2,600,1,1,100,100,100,0,1590198515,1586016000,1,1,'','','',''),(397,'397','397',2,700,2,3,100,100,100,0,1590198508,1586102400,1,1,'','','',''),(398,'398','398',2,800,1,4,100,100,100,0,1590198518,1586188800,1,1,'','','',''),(399,'399','399',2,900,2,5,100,100,100,0,1590198510,1586275200,1,1,'','','',''),(400,'400','400',2,1000,1,2,100,100,100,0,1590198508,1586361600,1,1,'','','',''),(401,'401','401',3,100,1,1,100,100,100,0,1590198510,1577808000,1,1,'','','',''),(402,'402','402',2,200,2,2,100,100,100,0,1590198496,1577894400,1,1,'','','',''),(403,'403','403',2,300,1,3,100,100,100,0,1590198508,1577980800,1,1,'','','',''),(404,'404','404',1,400,2,4,100,100,100,0,1590198508,1578067200,1,1,'','','',''),(405,'405','405',1,500,1,5,100,100,100,0,1590198510,1578153600,1,1,'','','',''),(406,'406','406',2,600,2,1,100,100,100,0,1590198508,1578240000,1,1,'','','',''),(407,'407','407',3,700,2,3,100,100,100,0,1590198501,1578326400,1,1,'','','',''),(408,'408','408',1,800,2,4,100,100,100,0,1590198508,1578412800,1,1,'','','',''),(409,'409','409',3,900,1,5,100,100,100,0,1590198508,1578499200,1,1,'','','',''),(410,'410','410',2,1000,2,2,100,100,100,0,1590198507,1578585600,1,1,'','','',''),(411,'411','411',2,100,1,1,100,100,100,0,1590198508,1578672000,1,1,'','','',''),(412,'412','412',2,200,2,2,100,100,100,0,1590198508,1578758400,1,1,'','','',''),(413,'413','413',2,300,1,3,100,100,100,0,1590198508,1578844800,1,1,'','','',''),(414,'414','414',2,400,2,4,100,100,100,0,1590198509,1578931200,1,1,'','','',''),(415,'415','415',2,500,1,5,100,100,100,0,1590198508,1579017600,1,1,'','','',''),(416,'416','416',2,600,2,1,100,100,100,0,1590198508,1579104000,1,1,'','','',''),(417,'417','417',2,700,2,3,100,100,100,0,1590198508,1579190400,1,1,'','','',''),(418,'418','418',2,800,2,4,100,100,100,0,1590198510,1579276800,1,1,'','','',''),(419,'419','419',2,900,1,5,100,100,100,0,1590198515,1579363200,1,1,'','','',''),(420,'420','420',2,1000,2,2,100,100,100,0,1590198519,1579449600,1,1,'','','',''),(421,'421','421',2,100,1,1,100,100,100,0,1590198510,1579536000,1,1,'','','',''),(422,'422','422',2,200,2,2,100,100,100,0,1590198521,1579622400,1,1,'','','',''),(423,'423','423',2,300,1,3,100,100,100,0,1590198515,1579708800,1,1,'','','',''),(424,'424','424',2,400,2,4,100,100,100,0,1590198510,1579795200,1,1,'','','',''),(425,'425','425',2,500,1,5,100,100,100,0,1590198518,1579881600,1,1,'','','',''),(426,'426','426',2,600,2,1,100,100,100,0,1590198508,1579968000,1,1,'','','',''),(427,'427','427',2,700,2,3,100,100,100,0,1590198518,1580054400,1,1,'','','',''),(428,'428','428',2,800,2,4,100,100,100,0,1590198519,1580140800,1,1,'','','',''),(429,'429','429',2,900,1,5,100,100,100,0,1590198515,1580227200,1,1,'','','',''),(430,'430','430',2,1000,2,2,100,100,100,0,1590198521,1580313600,1,1,'','','',''),(431,'431','431',2,100,1,1,100,100,100,0,1590198521,1580400000,1,1,'','','',''),(432,'432','432',2,200,2,2,100,100,100,0,1590198519,1580486400,1,1,'','','',''),(433,'433','433',2,300,1,3,100,100,100,0,1590198515,1580572800,1,1,'','','',''),(434,'434','434',2,400,2,4,100,100,100,0,1590198508,1580659200,1,1,'','','',''),(435,'435','435',2,500,1,5,100,100,100,0,1590198518,1580745600,1,1,'','','',''),(436,'436','436',2,600,2,1,100,100,100,0,1590198519,1580832000,1,1,'','','',''),(437,'437','437',2,700,2,3,100,100,100,0,1590198515,1580918400,1,1,'','','',''),(438,'438','438',2,800,2,4,100,100,100,0,1590198519,1581004800,1,1,'','','',''),(439,'439','439',2,900,1,5,100,100,100,0,1590198519,1581091200,1,1,'','','',''),(440,'440','440',2,1000,2,2,100,100,100,0,1590198521,1581177600,1,1,'','','',''),(441,'441','441',2,100,1,1,100,100,100,0,1590198516,1581264000,1,1,'','','',''),(442,'442','442',2,200,2,2,100,100,100,0,1590198515,1581350400,1,1,'','','',''),(443,'443','443',2,300,1,3,100,100,100,0,1590198521,1581436800,1,1,'','','',''),(444,'444','444',2,400,2,4,100,100,100,0,1590198512,1581523200,1,1,'','','',''),(445,'445','445',2,500,1,5,100,100,100,0,1590198520,1581609600,1,1,'','','',''),(446,'446','446',2,600,2,1,100,100,100,0,1590198515,1581696000,1,1,'','','',''),(447,'447','447',2,700,2,3,100,100,100,0,1590198508,1581782400,1,1,'','','',''),(448,'448','448',2,800,2,4,100,100,100,0,1590198521,1581868800,1,1,'','','',''),(449,'449','449',2,900,1,5,100,100,100,0,1590198515,1581955200,1,1,'','','',''),(450,'450','450',2,1000,2,2,100,100,100,0,1590198515,1582041600,1,1,'','','',''),(451,'451','451',2,100,1,1,100,100,100,0,1590198521,1582128000,1,1,'','','',''),(452,'452','452',2,200,2,2,100,100,100,0,1590198519,1582214400,1,1,'','','',''),(453,'453','453',2,300,1,3,100,100,100,0,1590198515,1582300800,1,1,'','','',''),(454,'454','454',2,400,2,4,100,100,100,0,1590198521,1582387200,1,1,'','','',''),(455,'455','455',2,500,1,5,100,100,100,0,1590198518,1582473600,1,1,'','','',''),(456,'456','456',2,600,2,1,100,100,100,0,1590198521,1582560000,1,1,'','','',''),(457,'457','457',2,700,2,3,100,100,100,0,1590198510,1582646400,1,1,'','','',''),(458,'458','458',2,800,2,4,100,100,100,0,1590198517,1582732800,1,1,'','','',''),(459,'459','459',2,900,1,5,100,100,100,0,1590198510,1582819200,1,1,'','','',''),(460,'460','460',2,1000,2,2,100,100,100,0,1590198518,1582905600,1,1,'','','',''),(461,'461','461',2,100,1,1,100,100,100,0,1590198518,1582992000,1,1,'','','',''),(462,'462','462',2,200,2,2,100,100,100,0,1590198508,1583078400,1,1,'','','',''),(463,'463','463',2,300,1,3,100,100,100,0,1590198509,1583164800,1,1,'','','',''),(464,'464','464',2,400,2,4,100,100,100,0,1590198510,1583251200,1,1,'','','',''),(465,'465','465',2,500,1,5,100,100,100,0,1590198515,1583337600,1,1,'','','',''),(466,'466','466',2,600,2,1,100,100,100,0,1590198515,1583424000,1,1,'','','',''),(467,'467','467',2,700,2,3,100,100,100,0,1590198515,1583510400,1,1,'','','',''),(468,'468','468',2,800,2,4,100,100,100,0,1590198519,1583596800,1,1,'','','',''),(469,'469','469',2,900,1,5,100,100,100,0,1590198520,1583683200,1,1,'','','',''),(470,'470','470',2,1000,2,2,100,100,100,0,1590198507,1583769600,1,1,'','','',''),(471,'471','471',2,100,1,1,100,100,100,0,1590198518,1583856000,1,1,'','','',''),(472,'472','472',2,200,2,2,100,100,100,0,1590198508,1583942400,1,1,'','','',''),(473,'473','473',2,300,1,3,100,100,100,0,1590198508,1584028800,1,1,'','','',''),(474,'474','474',2,400,2,4,100,100,100,0,1590198516,1584115200,1,1,'','','',''),(475,'475','475',2,500,1,5,100,100,100,0,1590198518,1584201600,1,1,'','','',''),(476,'476','476',2,600,2,1,100,100,100,0,1590198508,1584288000,1,1,'','','',''),(477,'477','477',2,700,2,3,100,100,100,0,1590198515,1584374400,1,1,'','','',''),(478,'478','478',2,800,2,4,100,100,100,0,1590198518,1584460800,1,1,'','','',''),(479,'479','479',2,900,1,5,100,100,100,0,1590198520,1584547200,1,1,'','','',''),(480,'480','480',2,1000,2,2,100,100,100,0,1590198510,1584633600,1,1,'','','',''),(481,'481','481',2,100,1,1,100,100,100,0,1590198519,1584720000,1,1,'','','',''),(482,'482','482',2,200,2,2,100,100,100,0,1590198508,1584806400,1,1,'','','',''),(483,'483','483',2,300,1,3,100,100,100,0,1590198515,1584892800,1,1,'','','',''),(484,'484','484',2,400,2,4,100,100,100,0,1590198519,1584979200,1,1,'','','',''),(485,'485','485',2,500,1,5,100,100,100,0,1590198521,1585065600,1,1,'','','',''),(486,'486','486',2,600,2,1,100,100,100,0,1590198519,1585152000,1,1,'','','',''),(487,'487','487',2,700,1,3,100,100,100,0,1590198519,1585238400,1,1,'','','',''),(488,'488','488',2,800,2,4,100,100,100,0,1590198508,1585324800,1,1,'','','',''),(489,'489','489',2,900,2,5,100,100,100,0,1590198508,1585411200,1,1,'','','',''),(490,'490','490',2,1000,2,2,100,100,100,0,1590198510,1585497600,1,1,'','','',''),(491,'491','491',2,100,1,1,100,100,100,0,1590198515,1585584000,1,1,'','','',''),(492,'492','492',2,200,2,2,100,100,100,0,1590198518,1585670400,1,1,'','','',''),(493,'493','493',2,300,2,3,100,100,100,0,1590198515,1585756800,1,1,'','','',''),(494,'494','494',2,400,1,4,100,100,100,0,1590198521,1585843200,1,1,'','','',''),(495,'495','495',2,500,2,5,100,100,100,0,1590198520,1585929600,1,1,'','','',''),(496,'496','496',2,600,1,1,100,100,100,0,1590198515,1586016000,1,1,'','','',''),(497,'497','497',2,700,2,3,100,100,100,0,1590198508,1586102400,1,1,'','','',''),(498,'498','498',2,800,1,4,100,100,100,0,1590198518,1586188800,1,1,'','','',''),(499,'499','499',2,900,2,5,100,100,100,0,1590198510,1586275200,1,1,'','','',''),(500,'500','500',2,1000,1,2,100,100,100,0,1590198508,1586361600,1,1,'','','',''),(501,'501','501',3,100,1,1,100,100,100,0,1590198510,1577808000,1,1,'','','',''),(502,'502','502',2,200,2,2,100,100,100,0,1590198496,1577894400,1,1,'','','',''),(503,'503','503',2,300,1,3,100,100,100,0,1590198508,1577980800,1,1,'','','',''),(504,'504','504',1,400,2,4,100,100,100,0,1590198508,1578067200,1,1,'','','',''),(505,'505','505',1,500,1,5,100,100,100,0,1590198510,1578153600,1,1,'','','',''),(506,'506','506',2,600,2,1,100,100,100,0,1590198508,1578240000,1,1,'','','',''),(507,'507','507',3,700,2,3,100,100,100,0,1590198501,1578326400,1,1,'','','',''),(508,'508','508',1,800,2,4,100,100,100,0,1590198508,1578412800,1,1,'','','',''),(509,'509','509',3,900,1,5,100,100,100,0,1590198508,1578499200,1,1,'','','',''),(510,'510','510',2,1000,2,2,100,100,100,0,1590198507,1578585600,1,1,'','','',''),(511,'511','511',2,100,1,1,100,100,100,0,1590198508,1578672000,1,1,'','','',''),(512,'512','512',2,200,2,2,100,100,100,0,1590198508,1578758400,1,1,'','','',''),(513,'513','513',2,300,1,3,100,100,100,0,1590198508,1578844800,1,1,'','','',''),(514,'514','514',2,400,2,4,100,100,100,0,1590198509,1578931200,1,1,'','','',''),(515,'515','515',2,500,1,5,100,100,100,0,1590198508,1579017600,1,1,'','','',''),(516,'516','516',2,600,2,1,100,100,100,0,1590198508,1579104000,1,1,'','','',''),(517,'517','517',2,700,2,3,100,100,100,0,1590198508,1579190400,1,1,'','','',''),(518,'518','518',2,800,2,4,100,100,100,0,1590198510,1579276800,1,1,'','','',''),(519,'519','519',2,900,1,5,100,100,100,0,1590198515,1579363200,1,1,'','','',''),(520,'520','520',2,1000,2,2,100,100,100,0,1590198519,1579449600,1,1,'','','',''),(521,'521','521',2,100,1,1,100,100,100,0,1590198510,1579536000,1,1,'','','',''),(522,'522','522',2,200,2,2,100,100,100,0,1590198521,1579622400,1,1,'','','',''),(523,'523','523',2,300,1,3,100,100,100,0,1590198515,1579708800,1,1,'','','',''),(524,'524','524',2,400,2,4,100,100,100,0,1590198510,1579795200,1,1,'','','',''),(525,'525','525',2,500,1,5,100,100,100,0,1590198518,1579881600,1,1,'','','',''),(526,'526','526',2,600,2,1,100,100,100,0,1590198508,1579968000,1,1,'','','',''),(527,'527','527',2,700,2,3,100,100,100,0,1590198518,1580054400,1,1,'','','',''),(528,'528','528',2,800,2,4,100,100,100,0,1590198519,1580140800,1,1,'','','',''),(529,'529','529',2,900,1,5,100,100,100,0,1590198515,1580227200,1,1,'','','',''),(530,'530','530',2,1000,2,2,100,100,100,0,1590198521,1580313600,1,1,'','','',''),(531,'531','531',2,100,1,1,100,100,100,0,1590198521,1580400000,1,1,'','','',''),(532,'532','532',2,200,2,2,100,100,100,0,1590198519,1580486400,1,1,'','','',''),(533,'533','533',2,300,1,3,100,100,100,0,1590198515,1580572800,1,1,'','','',''),(534,'534','534',2,400,2,4,100,100,100,0,1590198508,1580659200,1,1,'','','',''),(535,'535','535',2,500,1,5,100,100,100,0,1590198518,1580745600,1,1,'','','',''),(536,'536','536',2,600,2,1,100,100,100,0,1590198519,1580832000,1,1,'','','',''),(537,'537','537',2,700,2,3,100,100,100,0,1590198515,1580918400,1,1,'','','',''),(538,'538','538',2,800,2,4,100,100,100,0,1590198519,1581004800,1,1,'','','',''),(539,'539','539',2,900,1,5,100,100,100,0,1590198519,1581091200,1,1,'','','',''),(540,'540','540',2,1000,2,2,100,100,100,0,1590198521,1581177600,1,1,'','','',''),(541,'541','541',2,100,1,1,100,100,100,0,1590198516,1581264000,1,1,'','','',''),(542,'542','542',2,200,2,2,100,100,100,0,1590198515,1581350400,1,1,'','','',''),(543,'543','543',2,300,1,3,100,100,100,0,1590198521,1581436800,1,1,'','','',''),(544,'544','544',2,400,2,4,100,100,100,0,1590198512,1581523200,1,1,'','','',''),(545,'545','545',2,500,1,5,100,100,100,0,1590198520,1581609600,1,1,'','','',''),(546,'546','546',2,600,2,1,100,100,100,0,1590198515,1581696000,1,1,'','','',''),(547,'547','547',2,700,2,3,100,100,100,0,1590198508,1581782400,1,1,'','','',''),(548,'548','548',2,800,2,4,100,100,100,0,1590198521,1581868800,1,1,'','','',''),(549,'549','549',2,900,1,5,100,100,100,0,1590198515,1581955200,1,1,'','','',''),(550,'550','550',2,1000,2,2,100,100,100,0,1590198515,1582041600,1,1,'','','',''),(551,'551','551',2,100,1,1,100,100,100,0,1590198521,1582128000,1,1,'','','',''),(552,'552','552',2,200,2,2,100,100,100,0,1590198519,1582214400,1,1,'','','',''),(553,'553','553',2,300,1,3,100,100,100,0,1590198515,1582300800,1,1,'','','',''),(554,'554','554',2,400,2,4,100,100,100,0,1590198521,1582387200,1,1,'','','',''),(555,'555','555',2,500,1,5,100,100,100,0,1590198518,1582473600,1,1,'','','',''),(556,'556','556',2,600,2,1,100,100,100,0,1590198521,1582560000,1,1,'','','',''),(557,'557','557',2,700,2,3,100,100,100,0,1590198510,1582646400,1,1,'','','',''),(558,'558','558',2,800,2,4,100,100,100,0,1590198517,1582732800,1,1,'','','',''),(559,'559','559',2,900,1,5,100,100,100,0,1590198510,1582819200,1,1,'','','',''),(560,'560','560',2,1000,2,2,100,100,100,0,1590198518,1582905600,1,1,'','','',''),(561,'561','561',2,100,1,1,100,100,100,0,1590198518,1582992000,1,1,'','','',''),(562,'562','562',2,200,2,2,100,100,100,0,1590198508,1583078400,1,1,'','','',''),(563,'563','563',2,300,1,3,100,100,100,0,1590198509,1583164800,1,1,'','','',''),(564,'564','564',2,400,2,4,100,100,100,0,1590198510,1583251200,1,1,'','','',''),(565,'565','565',2,500,1,5,100,100,100,0,1590198515,1583337600,1,1,'','','',''),(566,'566','566',2,600,2,1,100,100,100,0,1590198515,1583424000,1,1,'','','',''),(567,'567','567',2,700,2,3,100,100,100,0,1590198515,1583510400,1,1,'','','',''),(568,'568','568',2,800,2,4,100,100,100,0,1590198519,1583596800,1,1,'','','',''),(569,'569','569',2,900,1,5,100,100,100,0,1590198520,1583683200,1,1,'','','',''),(570,'570','570',2,1000,2,2,100,100,100,0,1590198507,1583769600,1,1,'','','',''),(571,'571','571',2,100,1,1,100,100,100,0,1590198518,1583856000,1,1,'','','',''),(572,'572','572',2,200,2,2,100,100,100,0,1590198508,1583942400,1,1,'','','',''),(573,'573','573',2,300,1,3,100,100,100,0,1590198508,1584028800,1,1,'','','',''),(574,'574','574',2,400,2,4,100,100,100,0,1590198516,1584115200,1,1,'','','',''),(575,'575','575',2,500,1,5,100,100,100,0,1590198518,1584201600,1,1,'','','',''),(576,'576','576',2,600,2,1,100,100,100,0,1590198508,1584288000,1,1,'','','',''),(577,'577','577',2,700,2,3,100,100,100,0,1590198515,1584374400,1,1,'','','',''),(578,'578','578',2,800,2,4,100,100,100,0,1590198518,1584460800,1,1,'','','',''),(579,'579','579',2,900,1,5,100,100,100,0,1590198520,1584547200,1,1,'','','',''),(580,'580','580',2,1000,2,2,100,100,100,0,1590198510,1584633600,1,1,'','','',''),(581,'581','581',2,100,1,1,100,100,100,0,1590198519,1584720000,1,1,'','','',''),(582,'582','582',2,200,2,2,100,100,100,0,1590198508,1584806400,1,1,'','','',''),(583,'583','583',2,300,1,3,100,100,100,0,1590198515,1584892800,1,1,'','','',''),(584,'584','584',2,400,2,4,100,100,100,0,1590198519,1584979200,1,1,'','','',''),(585,'585','585',2,500,1,5,100,100,100,0,1590198521,1585065600,1,1,'','','',''),(586,'586','586',2,600,2,1,100,100,100,0,1590198519,1585152000,1,1,'','','',''),(587,'587','587',2,700,1,3,100,100,100,0,1590198519,1585238400,1,1,'','','',''),(588,'588','588',2,800,2,4,100,100,100,0,1590198508,1585324800,1,1,'','','',''),(589,'589','589',2,900,2,5,100,100,100,0,1590198508,1585411200,1,1,'','','',''),(590,'590','590',2,1000,2,2,100,100,100,0,1590198510,1585497600,1,1,'','','',''),(591,'591','591',2,100,1,1,100,100,100,0,1590198515,1585584000,1,1,'','','',''),(592,'592','592',2,200,2,2,100,100,100,0,1590198518,1585670400,1,1,'','','',''),(593,'593','593',2,300,2,3,100,100,100,0,1590198515,1585756800,1,1,'','','',''),(594,'594','594',2,400,1,4,100,100,100,0,1590198521,1585843200,1,1,'','','',''),(595,'595','595',2,500,2,5,100,100,100,0,1590198520,1585929600,1,1,'','','',''),(596,'596','596',2,600,1,1,100,100,100,0,1590198515,1586016000,1,1,'','','',''),(597,'597','597',2,700,2,3,100,100,100,0,1590198508,1586102400,1,1,'','','',''),(598,'598','598',2,800,1,4,100,100,100,0,1590198518,1586188800,1,1,'','','',''),(599,'599','599',2,900,2,5,100,100,100,0,1590198510,1586275200,1,1,'','','',''),(600,'600','600',2,1000,1,2,100,100,100,0,1590198508,1586361600,1,1,'','','',''),(601,'601','601',3,100,1,1,100,100,100,0,1590198510,1577808000,1,1,'','','',''),(602,'602','602',2,200,2,2,100,100,100,0,1590198496,1577894400,1,1,'','','',''),(603,'603','603',2,300,1,3,100,100,100,0,1590198508,1577980800,1,1,'','','',''),(604,'604','604',1,400,2,4,100,100,100,0,1590198508,1578067200,1,1,'','','',''),(605,'605','605',1,500,1,5,100,100,100,0,1590198510,1578153600,1,1,'','','',''),(606,'606','606',2,600,2,1,100,100,100,0,1590198508,1578240000,1,1,'','','',''),(607,'607','607',3,700,2,3,100,100,100,0,1590198501,1578326400,1,1,'','','',''),(608,'608','608',1,800,2,4,100,100,100,0,1590198508,1578412800,1,1,'','','',''),(609,'609','609',3,900,1,5,100,100,100,0,1590198508,1578499200,1,1,'','','',''),(610,'610','610',2,1000,2,2,100,100,100,0,1590198507,1578585600,1,1,'','','',''),(611,'611','611',2,100,1,1,100,100,100,0,1590198508,1578672000,1,1,'','','',''),(612,'612','612',2,200,2,2,100,100,100,0,1590198508,1578758400,1,1,'','','',''),(613,'613','613',2,300,1,3,100,100,100,0,1590198508,1578844800,1,1,'','','',''),(614,'614','614',2,400,2,4,100,100,100,0,1590198509,1578931200,1,1,'','','',''),(615,'615','615',2,500,1,5,100,100,100,0,1590198508,1579017600,1,1,'','','',''),(616,'616','616',2,600,2,1,100,100,100,0,1590198508,1579104000,1,1,'','','',''),(617,'617','617',2,700,2,3,100,100,100,0,1590198508,1579190400,1,1,'','','',''),(618,'618','618',2,800,2,4,100,100,100,0,1590198510,1579276800,1,1,'','','',''),(619,'619','619',2,900,1,5,100,100,100,0,1590198515,1579363200,1,1,'','','',''),(620,'620','620',2,1000,2,2,100,100,100,0,1590198519,1579449600,1,1,'','','',''),(621,'621','621',2,100,1,1,100,100,100,0,1590198510,1579536000,1,1,'','','',''),(622,'622','622',2,200,2,2,100,100,100,0,1590198521,1579622400,1,1,'','','',''),(623,'623','623',2,300,1,3,100,100,100,0,1590198515,1579708800,1,1,'','','',''),(624,'624','624',2,400,2,4,100,100,100,0,1590198510,1579795200,1,1,'','','',''),(625,'625','625',2,500,1,5,100,100,100,0,1590198518,1579881600,1,1,'','','',''),(626,'626','626',2,600,2,1,100,100,100,0,1590198508,1579968000,1,1,'','','',''),(627,'627','627',2,700,2,3,100,100,100,0,1590198518,1580054400,1,1,'','','',''),(628,'628','628',2,800,2,4,100,100,100,0,1590198519,1580140800,1,1,'','','',''),(629,'629','629',2,900,1,5,100,100,100,0,1590198515,1580227200,1,1,'','','',''),(630,'630','630',2,1000,2,2,100,100,100,0,1590198521,1580313600,1,1,'','','',''),(631,'631','631',2,100,1,1,100,100,100,0,1590198521,1580400000,1,1,'','','',''),(632,'632','632',2,200,2,2,100,100,100,0,1590198519,1580486400,1,1,'','','',''),(633,'633','633',2,300,1,3,100,100,100,0,1590198515,1580572800,1,1,'','','',''),(634,'634','634',2,400,2,4,100,100,100,0,1590198508,1580659200,1,1,'','','',''),(635,'635','635',2,500,1,5,100,100,100,0,1590198518,1580745600,1,1,'','','',''),(636,'636','636',2,600,2,1,100,100,100,0,1590198519,1580832000,1,1,'','','',''),(637,'637','637',2,700,2,3,100,100,100,0,1590198515,1580918400,1,1,'','','',''),(638,'638','638',2,800,2,4,100,100,100,0,1590198519,1581004800,1,1,'','','',''),(639,'639','639',2,900,1,5,100,100,100,0,1590198519,1581091200,1,1,'','','',''),(640,'640','640',2,1000,2,2,100,100,100,0,1590198521,1581177600,1,1,'','','',''),(641,'641','641',2,100,1,1,100,100,100,0,1590198516,1581264000,1,1,'','','',''),(642,'642','642',2,200,2,2,100,100,100,0,1590198515,1581350400,1,1,'','','',''),(643,'643','643',2,300,1,3,100,100,100,0,1590198521,1581436800,1,1,'','','',''),(644,'644','644',2,400,2,4,100,100,100,0,1590198512,1581523200,1,1,'','','',''),(645,'645','645',2,500,1,5,100,100,100,0,1590198520,1581609600,1,1,'','','',''),(646,'646','646',2,600,2,1,100,100,100,0,1590198515,1581696000,1,1,'','','',''),(647,'647','647',2,700,2,3,100,100,100,0,1590198508,1581782400,1,1,'','','',''),(648,'648','648',2,800,2,4,100,100,100,0,1590198521,1581868800,1,1,'','','',''),(649,'649','649',2,900,1,5,100,100,100,0,1590198515,1581955200,1,1,'','','',''),(650,'650','650',2,1000,2,2,100,100,100,0,1590198515,1582041600,1,1,'','','',''),(651,'651','651',2,100,1,1,100,100,100,0,1590198521,1582128000,1,1,'','','',''),(652,'652','652',2,200,2,2,100,100,100,0,1590198519,1582214400,1,1,'','','',''),(653,'653','653',2,300,1,3,100,100,100,0,1590198515,1582300800,1,1,'','','',''),(654,'654','654',2,400,2,4,100,100,100,0,1590198521,1582387200,1,1,'','','',''),(655,'655','655',2,500,1,5,100,100,100,0,1590198518,1582473600,1,1,'','','',''),(656,'656','656',2,600,2,1,100,100,100,0,1590198521,1582560000,1,1,'','','',''),(657,'657','657',2,700,2,3,100,100,100,0,1590198510,1582646400,1,1,'','','',''),(658,'658','658',2,800,2,4,100,100,100,0,1590198517,1582732800,1,1,'','','',''),(659,'659','659',2,900,1,5,100,100,100,0,1590198510,1582819200,1,1,'','','',''),(660,'660','660',2,1000,2,2,100,100,100,0,1590198518,1582905600,1,1,'','','',''),(661,'661','661',2,100,1,1,100,100,100,0,1590198518,1582992000,1,1,'','','',''),(662,'662','662',2,200,2,2,100,100,100,0,1590198508,1583078400,1,1,'','','',''),(663,'663','663',2,300,1,3,100,100,100,0,1590198509,1583164800,1,1,'','','',''),(664,'664','664',2,400,2,4,100,100,100,0,1590198510,1583251200,1,1,'','','',''),(665,'665','665',2,500,1,5,100,100,100,0,1590198515,1583337600,1,1,'','','',''),(666,'666','666',2,600,2,1,100,100,100,0,1590198515,1583424000,1,1,'','','',''),(667,'667','667',2,700,2,3,100,100,100,0,1590198515,1583510400,1,1,'','','',''),(668,'668','668',2,800,2,4,100,100,100,0,1590198519,1583596800,1,1,'','','',''),(669,'669','669',2,900,1,5,100,100,100,0,1590198520,1583683200,1,1,'','','',''),(670,'670','670',2,1000,2,2,100,100,100,0,1590198507,1583769600,1,1,'','','',''),(671,'671','671',2,100,1,1,100,100,100,0,1590198518,1583856000,1,1,'','','',''),(672,'672','672',2,200,2,2,100,100,100,0,1590198508,1583942400,1,1,'','','',''),(673,'673','673',2,300,1,3,100,100,100,0,1590198508,1584028800,1,1,'','','',''),(674,'674','674',2,400,2,4,100,100,100,0,1590198516,1584115200,1,1,'','','',''),(675,'675','675',2,500,1,5,100,100,100,0,1590198518,1584201600,1,1,'','','',''),(676,'676','676',2,600,2,1,100,100,100,0,1590198508,1584288000,1,1,'','','',''),(677,'677','677',2,700,2,3,100,100,100,0,1590198515,1584374400,1,1,'','','',''),(678,'678','678',2,800,2,4,100,100,100,0,1590198518,1584460800,1,1,'','','',''),(679,'679','679',2,900,1,5,100,100,100,0,1590198520,1584547200,1,1,'','','',''),(680,'680','680',2,1000,2,2,100,100,100,0,1590198510,1584633600,1,1,'','','',''),(681,'681','681',2,100,1,1,100,100,100,0,1590198519,1584720000,1,1,'','','',''),(682,'682','682',2,200,2,2,100,100,100,0,1590198508,1584806400,1,1,'','','',''),(683,'683','683',2,300,1,3,100,100,100,0,1590198515,1584892800,1,1,'','','',''),(684,'684','684',2,400,2,4,100,100,100,0,1590198519,1584979200,1,1,'','','',''),(685,'685','685',2,500,1,5,100,100,100,0,1590198521,1585065600,1,1,'','','',''),(686,'686','686',2,600,2,1,100,100,100,0,1590198519,1585152000,1,1,'','','',''),(687,'687','687',2,700,1,3,100,100,100,0,1590198519,1585238400,1,1,'','','',''),(688,'688','688',2,800,2,4,100,100,100,0,1590198508,1585324800,1,1,'','','',''),(689,'689','689',2,900,2,5,100,100,100,0,1590198508,1585411200,1,1,'','','',''),(690,'690','690',2,1000,2,2,100,100,100,0,1590198510,1585497600,1,1,'','','',''),(691,'691','691',2,100,1,1,100,100,100,0,1590198515,1585584000,1,1,'','','',''),(692,'692','692',2,200,2,2,100,100,100,0,1590198518,1585670400,1,1,'','','',''),(693,'693','693',2,300,2,3,100,100,100,0,1590198515,1585756800,1,1,'','','',''),(694,'694','694',2,400,1,4,100,100,100,0,1590198521,1585843200,1,1,'','','',''),(695,'695','695',2,500,2,5,100,100,100,0,1590198520,1585929600,1,1,'','','',''),(696,'696','696',2,600,1,1,100,100,100,0,1590198515,1586016000,1,1,'','','',''),(697,'697','697',2,700,2,3,100,100,100,0,1590198508,1586102400,1,1,'','','',''),(698,'698','698',2,800,1,4,100,100,100,0,1590198518,1586188800,1,1,'','','',''),(699,'699','699',2,900,2,5,100,100,100,0,1590198510,1586275200,1,1,'','','',''),(700,'700','700',2,1000,1,2,100,100,100,0,1590198508,1586361600,1,1,'','','',''),(701,'701','701',3,100,1,1,100,100,100,0,1590198510,1577808000,1,1,'','','',''),(702,'702','702',2,200,2,2,100,100,100,0,1590198496,1577894400,1,1,'','','',''),(703,'703','703',2,300,1,3,100,100,100,0,1590198508,1577980800,1,1,'','','',''),(704,'704','704',1,400,2,4,100,100,100,0,1590198508,1578067200,1,1,'','','',''),(705,'705','705',1,500,1,5,100,100,100,0,1590198510,1578153600,1,1,'','','',''),(706,'706','706',2,600,2,1,100,100,100,0,1590198508,1578240000,1,1,'','','',''),(707,'707','707',3,700,2,3,100,100,100,0,1590198501,1578326400,1,1,'','','',''),(708,'708','708',1,800,2,4,100,100,100,0,1590198508,1578412800,1,1,'','','',''),(709,'709','709',3,900,1,5,100,100,100,0,1590198508,1578499200,1,1,'','','',''),(710,'710','710',2,1000,2,2,100,100,100,0,1590198507,1578585600,1,1,'','','',''),(711,'711','711',2,100,1,1,100,100,100,0,1590198508,1578672000,1,1,'','','',''),(712,'712','712',2,200,2,2,100,100,100,0,1590198508,1578758400,1,1,'','','',''),(713,'713','713',2,300,1,3,100,100,100,0,1590198508,1578844800,1,1,'','','',''),(714,'714','714',2,400,2,4,100,100,100,0,1590198509,1578931200,1,1,'','','',''),(715,'715','715',2,500,1,5,100,100,100,0,1590198508,1579017600,1,1,'','','',''),(716,'716','716',2,600,2,1,100,100,100,0,1590198508,1579104000,1,1,'','','',''),(717,'717','717',2,700,2,3,100,100,100,0,1590198508,1579190400,1,1,'','','',''),(718,'718','718',2,800,2,4,100,100,100,0,1590198510,1579276800,1,1,'','','',''),(719,'719','719',2,900,1,5,100,100,100,0,1590198515,1579363200,1,1,'','','',''),(720,'720','720',2,1000,2,2,100,100,100,0,1590198519,1579449600,1,1,'','','',''),(721,'721','721',2,100,1,1,100,100,100,0,1590198510,1579536000,1,1,'','','',''),(722,'722','722',2,200,2,2,100,100,100,0,1590198521,1579622400,1,1,'','','',''),(723,'723','723',2,300,1,3,100,100,100,0,1590198515,1579708800,1,1,'','','',''),(724,'724','724',2,400,2,4,100,100,100,0,1590198510,1579795200,1,1,'','','',''),(725,'725','725',2,500,1,5,100,100,100,0,1590198518,1579881600,1,1,'','','',''),(726,'726','726',2,600,2,1,100,100,100,0,1590198508,1579968000,1,1,'','','',''),(727,'727','727',2,700,2,3,100,100,100,0,1590198518,1580054400,1,1,'','','',''),(728,'728','728',2,800,2,4,100,100,100,0,1590198519,1580140800,1,1,'','','',''),(729,'729','729',2,900,1,5,100,100,100,0,1590198515,1580227200,1,1,'','','',''),(730,'730','730',2,1000,2,2,100,100,100,0,1590198521,1580313600,1,1,'','','',''),(731,'731','731',2,100,1,1,100,100,100,0,1590198521,1580400000,1,1,'','','',''),(732,'732','732',2,200,2,2,100,100,100,0,1590198519,1580486400,1,1,'','','',''),(733,'733','733',2,300,1,3,100,100,100,0,1590198515,1580572800,1,1,'','','',''),(734,'734','734',2,400,2,4,100,100,100,0,1590198508,1580659200,1,1,'','','',''),(735,'735','735',2,500,1,5,100,100,100,0,1590198518,1580745600,1,1,'','','',''),(736,'736','736',2,600,2,1,100,100,100,0,1590198519,1580832000,1,1,'','','',''),(737,'737','737',2,700,2,3,100,100,100,0,1590198515,1580918400,1,1,'','','',''),(738,'738','738',2,800,2,4,100,100,100,0,1590198519,1581004800,1,1,'','','',''),(739,'739','739',2,900,1,5,100,100,100,0,1590198519,1581091200,1,1,'','','',''),(740,'740','740',2,1000,2,2,100,100,100,0,1590198521,1581177600,1,1,'','','',''),(741,'741','741',2,100,1,1,100,100,100,0,1590198516,1581264000,1,1,'','','',''),(742,'742','742',2,200,2,2,100,100,100,0,1590198515,1581350400,1,1,'','','',''),(743,'743','743',2,300,1,3,100,100,100,0,1590198521,1581436800,1,1,'','','',''),(744,'744','744',2,400,2,4,100,100,100,0,1590198512,1581523200,1,1,'','','',''),(745,'745','745',2,500,1,5,100,100,100,0,1590198520,1581609600,1,1,'','','',''),(746,'746','746',2,600,2,1,100,100,100,0,1590198515,1581696000,1,1,'','','',''),(747,'747','747',2,700,2,3,100,100,100,0,1590198508,1581782400,1,1,'','','',''),(748,'748','748',2,800,2,4,100,100,100,0,1590198521,1581868800,1,1,'','','',''),(749,'749','749',2,900,1,5,100,100,100,0,1590198515,1581955200,1,1,'','','',''),(750,'750','750',2,1000,2,2,100,100,100,0,1590198515,1582041600,1,1,'','','',''),(751,'751','751',2,100,1,1,100,100,100,0,1590198521,1582128000,1,1,'','','',''),(752,'752','752',2,200,2,2,100,100,100,0,1590198519,1582214400,1,1,'','','',''),(753,'753','753',2,300,1,3,100,100,100,0,1590198515,1582300800,1,1,'','','',''),(754,'754','754',2,400,2,4,100,100,100,0,1590198521,1582387200,1,1,'','','',''),(755,'755','755',2,500,1,5,100,100,100,0,1590198518,1582473600,1,1,'','','',''),(756,'756','756',2,600,2,1,100,100,100,0,1590198521,1582560000,1,1,'','','',''),(757,'757','757',2,700,2,3,100,100,100,0,1590198510,1582646400,1,1,'','','',''),(758,'758','758',2,800,2,4,100,100,100,0,1590198517,1582732800,1,1,'','','',''),(759,'759','759',2,900,1,5,100,100,100,0,1590198510,1582819200,1,1,'','','',''),(760,'760','760',2,1000,2,2,100,100,100,0,1590198518,1582905600,1,1,'','','',''),(761,'761','761',2,100,1,1,100,100,100,0,1590198518,1582992000,1,1,'','','',''),(762,'762','762',2,200,2,2,100,100,100,0,1590198508,1583078400,1,1,'','','',''),(763,'763','763',2,300,1,3,100,100,100,0,1590198509,1583164800,1,1,'','','',''),(764,'764','764',2,400,2,4,100,100,100,0,1590198510,1583251200,1,1,'','','',''),(765,'765','765',2,500,1,5,100,100,100,0,1590198515,1583337600,1,1,'','','',''),(766,'766','766',2,600,2,1,100,100,100,0,1590198515,1583424000,1,1,'','','',''),(767,'767','767',2,700,2,3,100,100,100,0,1590198515,1583510400,1,1,'','','',''),(768,'768','768',2,800,2,4,100,100,100,0,1590198519,1583596800,1,1,'','','',''),(769,'769','769',2,900,1,5,100,100,100,0,1590198520,1583683200,1,1,'','','',''),(770,'770','770',2,1000,2,2,100,100,100,0,1590198507,1583769600,1,1,'','','',''),(771,'771','771',2,100,1,1,100,100,100,0,1590198518,1583856000,1,1,'','','',''),(772,'772','772',2,200,2,2,100,100,100,0,1590198508,1583942400,1,1,'','','',''),(773,'773','773',2,300,1,3,100,100,100,0,1590198508,1584028800,1,1,'','','',''),(774,'774','774',2,400,2,4,100,100,100,0,1590198516,1584115200,1,1,'','','',''),(775,'775','775',2,500,1,5,100,100,100,0,1590198518,1584201600,1,1,'','','',''),(776,'776','776',2,600,2,1,100,100,100,0,1590198508,1584288000,1,1,'','','',''),(777,'777','777',2,700,2,3,100,100,100,0,1590198515,1584374400,1,1,'','','',''),(778,'778','778',2,800,2,4,100,100,100,0,1590198518,1584460800,1,1,'','','',''),(779,'779','779',2,900,1,5,100,100,100,0,1590198520,1584547200,1,1,'','','',''),(780,'780','780',2,1000,2,2,100,100,100,0,1590198510,1584633600,1,1,'','','',''),(781,'781','781',2,100,1,1,100,100,100,0,1590198519,1584720000,1,1,'','','',''),(782,'782','782',2,200,2,2,100,100,100,0,1590198508,1584806400,1,1,'','','',''),(783,'783','783',2,300,1,3,100,100,100,0,1590198515,1584892800,1,1,'','','',''),(784,'784','784',2,400,2,4,100,100,100,0,1590198519,1584979200,1,1,'','','',''),(785,'785','785',2,500,1,5,100,100,100,0,1590198521,1585065600,1,1,'','','',''),(786,'786','786',2,600,2,1,100,100,100,0,1590198519,1585152000,1,1,'','','',''),(787,'787','787',2,700,1,3,100,100,100,0,1590198519,1585238400,1,1,'','','',''),(788,'788','788',2,800,2,4,100,100,100,0,1590198508,1585324800,1,1,'','','',''),(789,'789','789',2,900,2,5,100,100,100,0,1590198508,1585411200,1,1,'','','',''),(790,'790','790',2,1000,2,2,100,100,100,0,1590198510,1585497600,1,1,'','','',''),(791,'791','791',2,100,1,1,100,100,100,0,1590198515,1585584000,1,1,'','','',''),(792,'792','792',2,200,2,2,100,100,100,0,1590198518,1585670400,1,1,'','','',''),(793,'793','793',2,300,2,3,100,100,100,0,1590198515,1585756800,1,1,'','','',''),(794,'794','794',2,400,1,4,100,100,100,0,1590198521,1585843200,1,1,'','','',''),(795,'795','795',2,500,2,5,100,100,100,0,1590198520,1585929600,1,1,'','','',''),(796,'796','796',2,600,1,1,100,100,100,0,1590198515,1586016000,1,1,'','','',''),(797,'797','797',2,700,2,3,100,100,100,0,1590198508,1586102400,1,1,'','','',''),(798,'798','798',2,800,1,4,100,100,100,0,1590198518,1586188800,1,1,'','','',''),(799,'799','799',2,900,2,5,100,100,100,0,1590198510,1586275200,1,1,'','','',''),(800,'800','800',2,1000,1,2,100,100,100,0,1590198508,1586361600,1,1,'','','',''),(801,'801','801',3,100,1,1,100,100,100,0,1590198510,1577808000,1,1,'','','',''),(802,'802','802',2,200,2,2,100,100,100,0,1590198496,1577894400,1,1,'','','',''),(803,'803','803',2,300,1,3,100,100,100,0,1590198508,1577980800,1,1,'','','',''),(804,'804','804',1,400,2,4,100,100,100,0,1590198508,1578067200,1,1,'','','',''),(805,'805','805',1,500,1,5,100,100,100,0,1590198510,1578153600,1,1,'','','',''),(806,'806','806',2,600,2,1,100,100,100,0,1590198508,1578240000,1,1,'','','',''),(807,'807','807',3,700,2,3,100,100,100,0,1590198501,1578326400,1,1,'','','',''),(808,'808','808',1,800,2,4,100,100,100,0,1590198508,1578412800,1,1,'','','',''),(809,'809','809',3,900,1,5,100,100,100,0,1590198508,1578499200,1,1,'','','',''),(810,'810','810',2,1000,2,2,100,100,100,0,1590198507,1578585600,1,1,'','','',''),(811,'811','811',2,100,1,1,100,100,100,0,1590198508,1578672000,1,1,'','','',''),(812,'812','812',2,200,2,2,100,100,100,0,1590198508,1578758400,1,1,'','','',''),(813,'813','813',2,300,1,3,100,100,100,0,1590198508,1578844800,1,1,'','','',''),(814,'814','814',2,400,2,4,100,100,100,0,1590198509,1578931200,1,1,'','','',''),(815,'815','815',2,500,1,5,100,100,100,0,1590198508,1579017600,1,1,'','','',''),(816,'816','816',2,600,2,1,100,100,100,0,1590198508,1579104000,1,1,'','','',''),(817,'817','817',2,700,2,3,100,100,100,0,1590198508,1579190400,1,1,'','','',''),(818,'818','818',2,800,2,4,100,100,100,0,1590198510,1579276800,1,1,'','','',''),(819,'819','819',2,900,1,5,100,100,100,0,1590198515,1579363200,1,1,'','','',''),(820,'820','820',2,1000,2,2,100,100,100,0,1590198519,1579449600,1,1,'','','',''),(821,'821','821',2,100,1,1,100,100,100,0,1590198510,1579536000,1,1,'','','',''),(822,'822','822',2,200,2,2,100,100,100,0,1590198521,1579622400,1,1,'','','',''),(823,'823','823',2,300,1,3,100,100,100,0,1590198515,1579708800,1,1,'','','',''),(824,'824','824',2,400,2,4,100,100,100,0,1590198510,1579795200,1,1,'','','',''),(825,'825','825',2,500,1,5,100,100,100,0,1590198518,1579881600,1,1,'','','',''),(826,'826','826',2,600,2,1,100,100,100,0,1590198508,1579968000,1,1,'','','',''),(827,'827','827',2,700,2,3,100,100,100,0,1590198518,1580054400,1,1,'','','',''),(828,'828','828',2,800,2,4,100,100,100,0,1590198519,1580140800,1,1,'','','',''),(829,'829','829',2,900,1,5,100,100,100,0,1590198515,1580227200,1,1,'','','',''),(830,'830','830',2,1000,2,2,100,100,100,0,1590198521,1580313600,1,1,'','','',''),(831,'831','831',2,100,1,1,100,100,100,0,1590198521,1580400000,1,1,'','','',''),(832,'832','832',2,200,2,2,100,100,100,0,1590198519,1580486400,1,1,'','','',''),(833,'833','833',2,300,1,3,100,100,100,0,1590198515,1580572800,1,1,'','','',''),(834,'834','834',2,400,2,4,100,100,100,0,1590198508,1580659200,1,1,'','','',''),(835,'835','835',2,500,1,5,100,100,100,0,1590198518,1580745600,1,1,'','','',''),(836,'836','836',2,600,2,1,100,100,100,0,1590198519,1580832000,1,1,'','','',''),(837,'837','837',2,700,2,3,100,100,100,0,1590198515,1580918400,1,1,'','','',''),(838,'838','838',2,800,2,4,100,100,100,0,1590198519,1581004800,1,1,'','','',''),(839,'839','839',2,900,1,5,100,100,100,0,1590198519,1581091200,1,1,'','','',''),(840,'840','840',2,1000,2,2,100,100,100,0,1590198521,1581177600,1,1,'','','',''),(841,'841','841',2,100,1,1,100,100,100,0,1590198516,1581264000,1,1,'','','',''),(842,'842','842',2,200,2,2,100,100,100,0,1590198515,1581350400,1,1,'','','',''),(843,'843','843',2,300,1,3,100,100,100,0,1590198521,1581436800,1,1,'','','',''),(844,'844','844',2,400,2,4,100,100,100,0,1590198512,1581523200,1,1,'','','',''),(845,'845','845',2,500,1,5,100,100,100,0,1590198520,1581609600,1,1,'','','',''),(846,'846','846',2,600,2,1,100,100,100,0,1590198515,1581696000,1,1,'','','',''),(847,'847','847',2,700,2,3,100,100,100,0,1590198508,1581782400,1,1,'','','',''),(848,'848','848',2,800,2,4,100,100,100,0,1590198521,1581868800,1,1,'','','',''),(849,'849','849',2,900,1,5,100,100,100,0,1590198515,1581955200,1,1,'','','',''),(850,'850','850',2,1000,2,2,100,100,100,0,1590198515,1582041600,1,1,'','','',''),(851,'851','851',2,100,1,1,100,100,100,0,1590198521,1582128000,1,1,'','','',''),(852,'852','852',2,200,2,2,100,100,100,0,1590198519,1582214400,1,1,'','','',''),(853,'853','853',2,300,1,3,100,100,100,0,1590198515,1582300800,1,1,'','','',''),(854,'854','854',2,400,2,4,100,100,100,0,1590198521,1582387200,1,1,'','','',''),(855,'855','855',2,500,1,5,100,100,100,0,1590198518,1582473600,1,1,'','','',''),(856,'856','856',2,600,2,1,100,100,100,0,1590198521,1582560000,1,1,'','','',''),(857,'857','857',2,700,2,3,100,100,100,0,1590198510,1582646400,1,1,'','','',''),(858,'858','858',2,800,2,4,100,100,100,0,1590198517,1582732800,1,1,'','','',''),(859,'859','859',2,900,1,5,100,100,100,0,1590198510,1582819200,1,1,'','','',''),(860,'860','860',2,1000,2,2,100,100,100,0,1590198518,1582905600,1,1,'','','',''),(861,'861','861',2,100,1,1,100,100,100,0,1590198518,1582992000,1,1,'','','',''),(862,'862','862',2,200,2,2,100,100,100,0,1590198508,1583078400,1,1,'','','',''),(863,'863','863',2,300,1,3,100,100,100,0,1590198509,1583164800,1,1,'','','',''),(864,'864','864',2,400,2,4,100,100,100,0,1590198510,1583251200,1,1,'','','',''),(865,'865','865',2,500,1,5,100,100,100,0,1590198515,1583337600,1,1,'','','',''),(866,'866','866',2,600,2,1,100,100,100,0,1590198515,1583424000,1,1,'','','',''),(867,'867','867',2,700,2,3,100,100,100,0,1590198515,1583510400,1,1,'','','',''),(868,'868','868',2,800,2,4,100,100,100,0,1590198519,1583596800,1,1,'','','',''),(869,'869','869',2,900,1,5,100,100,100,0,1590198520,1583683200,1,1,'','','',''),(870,'870','870',2,1000,2,2,100,100,100,0,1590198507,1583769600,1,1,'','','',''),(871,'871','871',2,100,1,1,100,100,100,0,1590198518,1583856000,1,1,'','','',''),(872,'872','872',2,200,2,2,100,100,100,0,1590198508,1583942400,1,1,'','','',''),(873,'873','873',2,300,1,3,100,100,100,0,1590198508,1584028800,1,1,'','','',''),(874,'874','874',2,400,2,4,100,100,100,0,1590198516,1584115200,1,1,'','','',''),(875,'875','875',2,500,1,5,100,100,100,0,1590198518,1584201600,1,1,'','','',''),(876,'876','876',2,600,2,1,100,100,100,0,1590198508,1584288000,1,1,'','','',''),(877,'877','877',2,700,2,3,100,100,100,0,1590198515,1584374400,1,1,'','','',''),(878,'878','878',2,800,2,4,100,100,100,0,1590198518,1584460800,1,1,'','','',''),(879,'879','879',2,900,1,5,100,100,100,0,1590198520,1584547200,1,1,'','','',''),(880,'880','880',2,1000,2,2,100,100,100,0,1590198510,1584633600,1,1,'','','',''),(881,'881','881',2,100,1,1,100,100,100,0,1590198519,1584720000,1,1,'','','',''),(882,'882','882',2,200,2,2,100,100,100,0,1590198508,1584806400,1,1,'','','',''),(883,'883','883',2,300,1,3,100,100,100,0,1590198515,1584892800,1,1,'','','',''),(884,'884','884',2,400,2,4,100,100,100,0,1590198519,1584979200,1,1,'','','',''),(885,'885','885',2,500,1,5,100,100,100,0,1590198521,1585065600,1,1,'','','',''),(886,'886','886',2,600,2,1,100,100,100,0,1590198519,1585152000,1,1,'','','',''),(887,'887','887',2,700,1,3,100,100,100,0,1590198519,1585238400,1,1,'','','',''),(888,'888','888',2,800,2,4,100,100,100,0,1590198508,1585324800,1,1,'','','',''),(889,'889','889',2,900,2,5,100,100,100,0,1590198508,1585411200,1,1,'','','',''),(890,'890','890',2,1000,2,2,100,100,100,0,1590198510,1585497600,1,1,'','','',''),(891,'891','891',2,100,1,1,100,100,100,0,1590198515,1585584000,1,1,'','','',''),(892,'892','892',2,200,2,2,100,100,100,0,1590198518,1585670400,1,1,'','','',''),(893,'893','893',2,300,2,3,100,100,100,0,1590198515,1585756800,1,1,'','','',''),(894,'894','894',2,400,1,4,100,100,100,0,1590198521,1585843200,1,1,'','','',''),(895,'895','895',2,500,2,5,100,100,100,0,1590198520,1585929600,1,1,'','','',''),(896,'896','896',2,600,1,1,100,100,100,0,1590198515,1586016000,1,1,'','','',''),(897,'897','897',2,700,2,3,100,100,100,0,1590198508,1586102400,1,1,'','','',''),(898,'898','898',2,800,1,4,100,100,100,0,1590198518,1586188800,1,1,'','','',''),(899,'899','899',2,900,2,5,100,100,100,0,1590198510,1586275200,1,1,'','','',''),(900,'900','900',2,1000,1,2,100,100,100,0,1590198508,1586361600,1,1,'','','',''),(901,'901','901',3,100,1,1,100,100,100,0,1590198510,1577808000,1,1,'','','',''),(902,'902','902',2,200,2,2,100,100,100,0,1590198496,1577894400,1,1,'','','',''),(903,'903','903',2,300,1,3,100,100,100,0,1590198508,1577980800,1,1,'','','',''),(904,'904','904',1,400,2,4,100,100,100,0,1590198508,1578067200,1,1,'','','',''),(905,'905','905',1,500,1,5,100,100,100,0,1590198510,1578153600,1,1,'','','',''),(906,'906','906',2,600,2,1,100,100,100,0,1590198508,1578240000,1,1,'','','',''),(907,'907','907',3,700,2,3,100,100,100,0,1590198501,1578326400,1,1,'','','',''),(908,'908','908',1,800,2,4,100,100,100,0,1590198508,1578412800,1,1,'','','',''),(909,'909','909',3,900,1,5,100,100,100,0,1590198508,1578499200,1,1,'','','',''),(910,'910','910',2,1000,2,2,100,100,100,0,1590198507,1578585600,1,1,'','','',''),(911,'911','911',2,100,1,1,100,100,100,0,1590198508,1578672000,1,1,'','','',''),(912,'912','912',2,200,2,2,100,100,100,0,1590198508,1578758400,1,1,'','','',''),(913,'913','913',2,300,1,3,100,100,100,0,1590198508,1578844800,1,1,'','','',''),(914,'914','914',2,400,2,4,100,100,100,0,1590198509,1578931200,1,1,'','','',''),(915,'915','915',2,500,1,5,100,100,100,0,1590198508,1579017600,1,1,'','','',''),(916,'916','916',2,600,2,1,100,100,100,0,1590198508,1579104000,1,1,'','','',''),(917,'917','917',2,700,2,3,100,100,100,0,1590198508,1579190400,1,1,'','','',''),(918,'918','918',2,800,2,4,100,100,100,0,1590198510,1579276800,1,1,'','','',''),(919,'919','919',2,900,1,5,100,100,100,0,1590198515,1579363200,1,1,'','','',''),(920,'920','920',2,1000,2,2,100,100,100,0,1590198519,1579449600,1,1,'','','',''),(921,'921','921',2,100,1,1,100,100,100,0,1590198510,1579536000,1,1,'','','',''),(922,'922','922',2,200,2,2,100,100,100,0,1590198521,1579622400,1,1,'','','',''),(923,'923','923',2,300,1,3,100,100,100,0,1590198515,1579708800,1,1,'','','',''),(924,'924','924',2,400,2,4,100,100,100,0,1590198510,1579795200,1,1,'','','',''),(925,'925','925',2,500,1,5,100,100,100,0,1590198518,1579881600,1,1,'','','',''),(926,'926','926',2,600,2,1,100,100,100,0,1590198508,1579968000,1,1,'','','',''),(927,'927','927',2,700,2,3,100,100,100,0,1590198518,1580054400,1,1,'','','',''),(928,'928','928',2,800,2,4,100,100,100,0,1590198519,1580140800,1,1,'','','',''),(929,'929','929',2,900,1,5,100,100,100,0,1590198515,1580227200,1,1,'','','',''),(930,'930','930',2,1000,2,2,100,100,100,0,1590198521,1580313600,1,1,'','','',''),(931,'931','931',2,100,1,1,100,100,100,0,1590198521,1580400000,1,1,'','','',''),(932,'932','932',2,200,2,2,100,100,100,0,1590198519,1580486400,1,1,'','','',''),(933,'933','933',2,300,1,3,100,100,100,0,1590198515,1580572800,1,1,'','','',''),(934,'934','934',2,400,2,4,100,100,100,0,1590198508,1580659200,1,1,'','','',''),(935,'935','935',2,500,1,5,100,100,100,0,1590198518,1580745600,1,1,'','','',''),(936,'936','936',2,600,2,1,100,100,100,0,1590198519,1580832000,1,1,'','','',''),(937,'937','937',2,700,2,3,100,100,100,0,1590198515,1580918400,1,1,'','','',''),(938,'938','938',2,800,2,4,100,100,100,0,1590198519,1581004800,1,1,'','','',''),(939,'939','939',2,900,1,5,100,100,100,0,1590198519,1581091200,1,1,'','','',''),(940,'940','940',2,1000,2,2,100,100,100,0,1590198521,1581177600,1,1,'','','',''),(941,'941','941',2,100,1,1,100,100,100,0,1590198516,1581264000,1,1,'','','',''),(942,'942','942',2,200,2,2,100,100,100,0,1590198515,1581350400,1,1,'','','',''),(943,'943','943',2,300,1,3,100,100,100,0,1590198521,1581436800,1,1,'','','',''),(944,'944','944',2,400,2,4,100,100,100,0,1590198512,1581523200,1,1,'','','',''),(945,'945','945',2,500,1,5,100,100,100,0,1590198520,1581609600,1,1,'','','',''),(946,'946','946',2,600,2,1,100,100,100,0,1590198515,1581696000,1,1,'','','',''),(947,'947','947',2,700,2,3,100,100,100,0,1590198508,1581782400,1,1,'','','',''),(948,'948','948',2,800,2,4,100,100,100,0,1590198521,1581868800,1,1,'','','',''),(949,'949','949',2,900,1,5,100,100,100,0,1590198515,1581955200,1,1,'','','',''),(950,'950','950',2,1000,2,2,100,100,100,0,1590198515,1582041600,1,1,'','','',''),(951,'951','951',2,100,1,1,100,100,100,0,1590198521,1582128000,1,1,'','','',''),(952,'952','952',2,200,2,2,100,100,100,0,1590198519,1582214400,1,1,'','','',''),(953,'953','953',2,300,1,3,100,100,100,0,1590198515,1582300800,1,1,'','','',''),(954,'954','954',2,400,2,4,100,100,100,0,1590198521,1582387200,1,1,'','','',''),(955,'955','955',2,500,1,5,100,100,100,0,1590198518,1582473600,1,1,'','','',''),(956,'956','956',2,600,2,1,100,100,100,0,1590198521,1582560000,1,1,'','','',''),(957,'957','957',2,700,2,3,100,100,100,0,1590198510,1582646400,1,1,'','','',''),(958,'958','958',2,800,2,4,100,100,100,0,1590198517,1582732800,1,1,'','','',''),(959,'959','959',2,900,1,5,100,100,100,0,1590198510,1582819200,1,1,'','','',''),(960,'960','960',2,1000,2,2,100,100,100,0,1590198518,1582905600,1,1,'','','',''),(961,'961','961',2,100,1,1,100,100,100,0,1590198518,1582992000,1,1,'','','',''),(962,'962','962',2,200,2,2,100,100,100,0,1590198508,1583078400,1,1,'','','',''),(963,'963','963',2,300,1,3,100,100,100,0,1590198509,1583164800,1,1,'','','',''),(964,'964','964',2,400,2,4,100,100,100,0,1590198510,1583251200,1,1,'','','',''),(965,'965','965',2,500,1,5,100,100,100,0,1590198515,1583337600,1,1,'','','',''),(966,'966','966',2,600,2,1,100,100,100,0,1590198515,1583424000,1,1,'','','',''),(967,'967','967',2,700,2,3,100,100,100,0,1590198515,1583510400,1,1,'','','',''),(968,'968','968',2,800,2,4,100,100,100,0,1590198519,1583596800,1,1,'','','',''),(969,'969','969',2,900,1,5,100,100,100,0,1590198520,1583683200,1,1,'','','',''),(970,'970','970',2,1000,2,2,100,100,100,0,1590198507,1583769600,1,1,'','','',''),(971,'971','971',2,100,1,1,100,100,100,0,1590198518,1583856000,1,1,'','','',''),(972,'972','972',2,200,2,2,100,100,100,0,1590198508,1583942400,1,1,'','','',''),(973,'973','973',2,300,1,3,100,100,100,0,1590198508,1584028800,1,1,'','','',''),(974,'974','974',2,400,2,4,100,100,100,0,1590198516,1584115200,1,1,'','','',''),(975,'975','975',2,500,1,5,100,100,100,0,1590198518,1584201600,1,1,'','','',''),(976,'976','976',2,600,2,1,100,100,100,0,1590198508,1584288000,1,1,'','','',''),(977,'977','977',2,700,2,3,100,100,100,0,1590198515,1584374400,1,1,'','','',''),(978,'978','978',2,800,2,4,100,100,100,0,1590198518,1584460800,1,1,'','','',''),(979,'979','979',2,900,1,5,100,100,100,0,1590198520,1584547200,1,1,'','','',''),(980,'980','980',2,1000,2,2,100,100,100,0,1590198510,1584633600,1,1,'','','',''),(981,'981','981',2,100,1,1,100,100,100,0,1590198519,1584720000,1,1,'','','',''),(982,'982','982',2,200,2,2,100,100,100,0,1590198508,1584806400,1,1,'','','',''),(983,'983','983',2,300,1,3,100,100,100,0,1590198515,1584892800,1,1,'','','',''),(984,'984','984',2,400,2,4,100,100,100,0,1590198519,1584979200,1,1,'','','',''),(985,'985','985',2,500,1,5,100,100,100,0,1590198521,1585065600,1,1,'','','',''),(986,'986','986',2,600,2,1,100,100,100,0,1590198519,1585152000,1,1,'','','',''),(987,'987','987',2,700,1,3,100,100,100,0,1590198519,1585238400,1,1,'','','',''),(988,'988','988',2,800,2,4,100,100,100,0,1590198508,1585324800,1,1,'','','',''),(989,'989','989',2,900,2,5,100,100,100,0,1590198508,1585411200,1,1,'','','',''),(990,'990','990',2,1000,2,2,100,100,100,0,1590198510,1585497600,1,1,'','','',''),(991,'991','991',2,100,1,1,100,100,100,0,1590198515,1585584000,1,1,'','','',''),(992,'992','992',2,200,2,2,100,100,100,0,1590198518,1585670400,1,1,'','','',''),(993,'993','993',2,300,2,3,100,100,100,0,1590198515,1585756800,1,1,'','','',''),(994,'994','994',2,400,1,4,100,100,100,0,1590198521,1585843200,1,1,'','','',''),(995,'995','995',2,500,2,5,100,100,100,0,1590198520,1585929600,1,1,'','','',''),(996,'996','996',2,600,1,1,100,100,100,0,1590198515,1586016000,1,1,'','','',''),(997,'997','997',2,700,2,3,100,100,100,0,1590198508,1586102400,1,1,'','','',''),(998,'998','998',2,800,1,4,100,100,100,0,1590198518,1586188800,1,1,'','','',''),(999,'999','999',2,900,2,5,100,100,100,0,1590198510,1586275200,1,1,'','','',''),(1000,'1000','1000',2,1000,1,2,100,100,100,0,1590198508,1586361600,1,1,'','','','');
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
  `exp` bigint(20) unsigned NOT NULL DEFAULT 0 COMMENT '经验',
  `time` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '时间',
  PRIMARY KEY (`id`) USING BTREE,
  KEY `role_id` (`role_id`) USING BTREE,
  KEY `time` (`time`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci ROW_FORMAT=COMPRESSED COMMENT='角色日志表';
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `role_log`
--

LOCK TABLES `role_log` WRITE;
/*!40000 ALTER TABLE `role_log` DISABLE KEYS */;
/*!40000 ALTER TABLE `role_log` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `sensitive_word_data`
--

DROP TABLE IF EXISTS `sensitive_word_data`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `sensitive_word_data` (
  `word` varchar(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '敏感词',
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
-- Table structure for table `shop`
--

DROP TABLE IF EXISTS `shop`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `shop` (
  `role_id` bigint(20) unsigned NOT NULL DEFAULT 0 COMMENT '角色ID(select)',
  `shop_id` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '商店ID',
  `number` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '数量',
  `flag` varchar(0) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '标识(flag)',
  PRIMARY KEY (`role_id`,`shop_id`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci ROW_FORMAT=DYNAMIC COMMENT='角色商店表';
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `shop`
--

LOCK TABLES `shop` WRITE;
/*!40000 ALTER TABLE `shop` DISABLE KEYS */;
INSERT INTO `shop` VALUES (1,1,1,'');
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
  `pay_assets` varchar(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '货币类型',
  `price` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '价格',
  `number` int(10) unsigned NOT NULL DEFAULT 1 COMMENT '数量',
  `level` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '等级限制',
  `limit` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '购买上限',
  `vip_level` int(10) unsigned NOT NULL DEFAULT 0 COMMENT 'vip等级限购',
  `vip_limit` varchar(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT 'vip等级购买上限',
  `description` char(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '描述',
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
  `id` int(10) unsigned NOT NULL AUTO_INCREMENT COMMENT 'ID',
  `role_id` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '角色ID',
  `shop_id` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '商店ID',
  `number` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '购买数量',
  `time` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '时间',
  PRIMARY KEY (`id`) USING BTREE,
  KEY `role_id` (`role_id`) USING BTREE,
  KEY `time` (`time`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci ROW_FORMAT=COMPRESSED COMMENT='商店日志表';
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `shop_log`
--

LOCK TABLES `shop_log` WRITE;
/*!40000 ALTER TABLE `shop_log` DISABLE KEYS */;
/*!40000 ALTER TABLE `shop_log` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `skill`
--

DROP TABLE IF EXISTS `skill`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `skill` (
  `role_id` bigint(20) unsigned NOT NULL DEFAULT 0 COMMENT '角色ID(select)',
  `skill_id` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '技能ID',
  `level` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '等级',
  `flag` varchar(0) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '标识(flag)',
  PRIMARY KEY (`role_id`,`skill_id`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci ROW_FORMAT=DYNAMIC COMMENT='角色技能表';
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `skill`
--

LOCK TABLES `skill` WRITE;
/*!40000 ALTER TABLE `skill` DISABLE KEYS */;
INSERT INTO `skill` VALUES (1,1,1,''),(1,2,1,'');
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
  `type` varchar(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '类型(validate(skill_type))',
  `name` char(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '名字',
  `condition` varchar(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '学习条件',
  `cost` varchar(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '升级消耗',
  `effect` varchar(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '作用效果',
  `cd` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '冷却时间',
  `radius` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '作用半径',
  `distance` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '作用距离',
  `number` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '作用对象数',
  `buffs` varchar(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '作用Buff',
  `before_effects` varchar(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '效果前',
  `hit_effects` varchar(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '击中效果',
  `after_effects` varchar(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '效果后',
  `description` char(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '描述',
  PRIMARY KEY (`skill_id`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci ROW_FORMAT=DYNAMIC COMMENT='技能配置表';
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `skill_data`
--

LOCK TABLES `skill_data` WRITE;
/*!40000 ALTER TABLE `skill_data` DISABLE KEYS */;
INSERT INTO `skill_data` VALUES (1,'active','普攻技能','','','[1]',1,1000,1000,1,'','','','','对目标造成180%的伤害'),(2,'active','群攻技能','','','[2]',1,1000,1000,30,'','','','','对3个目标造成150%的伤害'),(3,'passive','增益','','','[8]',10,1,1,1,'','','','','每秒扣血，总血量万分之50'),(5,'active','普攻技能','','','',1,1,1,1,'','','','','普通技能');
/*!40000 ALTER TABLE `skill_data` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `text_data`
--

DROP TABLE IF EXISTS `text_data`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `text_data` (
  `key` varchar(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '参数键',
  `value` char(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '参数值',
  `description` char(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '描述',
  PRIMARY KEY (`key`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci ROW_FORMAT=DYNAMIC COMMENT='游戏文本配置表';
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `text_data`
--

LOCK TABLES `text_data` WRITE;
/*!40000 ALTER TABLE `text_data` DISABLE KEYS */;
INSERT INTO `text_data` VALUES ('1','不用买，随便爆','成龙台词'),('2','是兄弟就来砍我','古天乐台词'),('3','卸载掉手机那个假传奇','甄子丹台词'),('add_item_content','你的益达','背包满内容'),('add_item_title','背包满','背包满标题'),('test','😂','😒');
/*!40000 ALTER TABLE `text_data` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `title`
--

DROP TABLE IF EXISTS `title`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `title` (
  `role_id` bigint(20) unsigned NOT NULL DEFAULT 0 COMMENT '角色ID(select)(update_role_id)',
  `title_id` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '称号ID(select_id)',
  `type` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '类型',
  `expire_time` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '过期时间',
  `flag` varchar(0) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '标识(flag)',
  PRIMARY KEY (`role_id`,`title_id`) USING BTREE,
  KEY ```title_id``` (`title_id`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci ROW_FORMAT=DYNAMIC COMMENT='角色称号表';
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `title`
--

LOCK TABLES `title` WRITE;
/*!40000 ALTER TABLE `title` DISABLE KEYS */;
INSERT INTO `title` VALUES (1,101,1,0,'');
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
  `multi` varchar(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '同类型可否拥有多个(validate(boolean))',
  `unique` varchar(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '是否全服唯一(validate(boolean))',
  `time` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '有效时间',
  `attribute` varchar(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '属性',
  `name` char(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '称号名字',
  `description` char(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '称号描述',
  PRIMARY KEY (`title_id`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci ROW_FORMAT=DYNAMIC COMMENT='称号配置表';
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `title_data`
--

LOCK TABLES `title_data` WRITE;
/*!40000 ALTER TABLE `title_data` DISABLE KEYS */;
INSERT INTO `title_data` VALUES (101,1,'false','false',0,'[{3,30},{4,40}]','小试牛刀','VIP1可获得'),(102,1,'false','false',0,'[{3,30},{4,40}]','有钱任性','VIP2可获得'),(103,1,'false','false',0,'[{3,30},{4,40}]','一掷千金','VIP3可获得'),(104,1,'false','false',0,'[{3,30},{4,40}]','腰缠万贯','VIP4可获得'),(105,1,'false','false',0,'[{3,30},{4,40}]','挥金如土','VIP5可获得'),(106,1,'false','false',0,'[{3,30},{4,40}]','富甲天下','VIP6可获得'),(107,1,'false','false',0,'[{3,30},{4,40}]','富可敌国','VIP7可获得'),(108,1,'false','false',0,'[{3,30},{4,40}]','人生巅峰','VIP8可获得'),(109,1,'false','false',0,'[{3,30},{4,40}]','至尊王者','VIP9可获得'),(110,1,'false','false',0,'[{3,30},{4,40}]','高手对决','VIP0可获得'),(201,2,'true','false',0,'[{6,60},{7,70}]','武艺超群','开服冲榜活动获取'),(202,2,'true','false',0,'[{6,60},{7,70}]','出神入化','开服冲榜活动获取'),(203,2,'true','false',0,'[{6,60},{7,70}]','仙武主宰','开服冲榜活动获取'),(204,2,'true','false',0,'[{6,60},{7,70}]','锻造大师','开服冲榜活动获取'),(205,2,'true','false',0,'[{6,60},{7,70}]','黑暗主宰','开服冲榜活动获取'),(206,2,'true','false',0,'[{6,60},{7,70}]','聚魂先锋','开服冲榜活动获取'),(207,2,'true','false',0,'[{6,60},{7,70}]','全职高手','开服冲榜活动获取'),(208,2,'true','false',0,'[{6,60},{7,70}]','人中之龙','开服冲榜活动获取'),(209,2,'true','false',0,'[{6,60},{7,70}]','勇者无畏','开服冲榜活动获取'),(210,2,'true','false',0,'[{6,60},{7,70}]','称霸天下','开服冲榜活动获取'),(10010,3,'false','true',0,'[{5,50}]','归隐山林','充值获取');
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
  `from` varchar(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '来源',
  `time` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '时间',
  PRIMARY KEY (`id`) USING BTREE,
  KEY `role_id` (`role_id`) USING BTREE,
  KEY `time` (`time`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci ROW_FORMAT=COMPRESSED COMMENT='称号日志表';
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `title_log`
--

LOCK TABLES `title_log` WRITE;
/*!40000 ALTER TABLE `title_log` DISABLE KEYS */;
/*!40000 ALTER TABLE `title_log` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `vip`
--

DROP TABLE IF EXISTS `vip`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `vip` (
  `role_id` bigint(20) unsigned NOT NULL DEFAULT 0 COMMENT '角色id',
  `vip_level` tinyint(2) unsigned NOT NULL DEFAULT 0 COMMENT 'vip等级',
  `exp` bigint(20) unsigned NOT NULL DEFAULT 0 COMMENT 'vip经验',
  `expire_time` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '过期时间',
  PRIMARY KEY (`role_id`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci ROW_FORMAT=DYNAMIC COMMENT='角色vip表';
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `vip`
--

LOCK TABLES `vip` WRITE;
/*!40000 ALTER TABLE `vip` DISABLE KEYS */;
INSERT INTO `vip` VALUES (1,1,0,0);
/*!40000 ALTER TABLE `vip` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `vip_data`
--

DROP TABLE IF EXISTS `vip_data`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `vip_data` (
  `vip` tinyint(1) unsigned NOT NULL DEFAULT 0 COMMENT 'VIP等级',
  `exp` bigint(20) unsigned NOT NULL DEFAULT 0 COMMENT '经验',
  PRIMARY KEY (`vip`) USING BTREE
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

-- Dump completed on 2020-05-23 10:21:20

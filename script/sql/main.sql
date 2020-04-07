-- MariaDB dump 10.17  Distrib 10.4.12-MariaDB, for Win64 (AMD64)
--
-- Host: localhost    Database: main
-- ------------------------------------------------------
-- Server version	10.4.12-MariaDB

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
  `mode` tinyint(1) NOT NULL DEFAULT 0 COMMENT '活动模式(validate(node_type_integer))',
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
INSERT INTO `asset` VALUES (1,1000000,1000000,1000000,1011200,1000000);
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
) ENGINE=InnoDB AUTO_INCREMENT=2 DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci ROW_FORMAT=DYNAMIC COMMENT='拍卖信息表';
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
  `event` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '事件(validate(event))',
  `condition` varchar(255) COLLATE utf8_unicode_ci NOT NULL DEFAULT '' COMMENT '条件',
  `cost` varchar(255) COLLATE utf8_unicode_ci NOT NULL DEFAULT '' COMMENT '消耗',
  `day_number` varchar(255) COLLATE utf8_unicode_ci NOT NULL DEFAULT '' COMMENT '每日次数',
  `buy_number` varchar(255) COLLATE utf8_unicode_ci NOT NULL DEFAULT '' COMMENT '购买次数',
  `module` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '代码模块(validate(module))',
  `function` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '入口函数(validate(function))',
  `map_id` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '地图ID',
  `monsters` varchar(255) COLLATE utf8_unicode_ci NOT NULL DEFAULT '' COMMENT '怪物',
  `boss` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT 'Boss',
  `time` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '时间',
  `award` varchar(255) COLLATE utf8_unicode_ci NOT NULL DEFAULT '' COMMENT '奖励',
  `name` char(255) COLLATE utf8_unicode_ci NOT NULL DEFAULT '' COMMENT '名字',
  `description` char(255) COLLATE utf8_unicode_ci NOT NULL DEFAULT '' COMMENT '描述',
  PRIMARY KEY (`dungeon_id`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_unicode_ci ROW_FORMAT=DYNAMIC COMMENT='副本配置表';
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
INSERT INTO `guild` VALUES (1,1,0,0,0,'1','',1,'','','','','',''),(2,2,0,0,0,'2','',2,'','','','','',''),(3,3,0,0,0,'3','',3,'','','','','','');
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
INSERT INTO `guild_apply` VALUES (1,3,0,'','','','','','',''),(1,4,0,'','','','','','',''),(2,3,0,'','','','','','',''),(2,5,0,'','','','','','','');
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
INSERT INTO `guild_role` VALUES (1,1,1,0,0,0,'','','','','','',''),(2,2,1,0,0,0,'','','','','','',''),(3,3,1,0,0,0,'','','','','','','');
/*!40000 ALTER TABLE `guild_role` ENABLE KEYS */;
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
INSERT INTO `increment` VALUES ('increment_server',0),('map',0),('monster',10010);
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
) ENGINE=InnoDB AUTO_INCREMENT=4 DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci ROW_FORMAT=COMPRESSED COMMENT='物品消费日志表';
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
INSERT INTO `map_data` VALUES (100000,'slice','false','','role','hurt','share','[{10,10},{20,10},{30,10},{40,10},{50,10},{60,10},{70,10},{10,10},{90,10},{100,10}]','','','',''),(100001,'full','false','','guild','hurt','share','[{10,10},{20,10},{30,10},{40,10},{50,10},{60,10},{70,10},{10,10},{90,10},{100,10}]','','','',''),(100002,'full','false','','team','hurt','share','[{10,10},{20,10},{30,10},{40,10},{50,10},{60,10},{70,10},{10,10},{90,10},{100,10}]','','','',''),(100003,'full','false','','camp','hurt','share','[{10,10},{20,10},{30,10},{40,10},{50,10},{60,10},{70,10},{10,10},{90,10},{100,10}]','','','',''),(200001,'full','false','','role','hurt','share','[{10,10},{20,10},{30,10},{40,10},{50,10},{60,10},{70,10},{10,10},{90,10},{100,10}]','','','',''),(200002,'full','false','','guild','hurt','share','[{10,10},{20,10},{30,10},{40,10},{50,10},{60,10},{70,10},{10,10},{90,10},{100,10}]','','','',''),(200003,'full','false','','team','hurt','share','[{10,10},{20,10},{30,10},{40,10},{50,10},{60,10},{70,10},{10,10},{90,10},{100,10}]','','','',''),(200004,'slice','true','','camp','hurt','share','[{10,10},{20,10},{30,10},{40,10},{50,10},{60,10},{70,10},{10,10},{90,10},{100,10}]','','','','');
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
INSERT INTO `node_data` VALUES ('center','小跨服','','',0,10001,'center','','','','',0,0),('dev','开发服','','',10004,4,'local','center','','','',0,0),('main','主测服','','',10001,1,'local','center','','','',0,0),('publish','版署服','','',10005,5,'local','center','','','',0,0),('stable','稳定服','','',10002,2,'local','center','','','',0,0),('test','测试服','','',10003,3,'local','center','','','',0,0),('world','大世界','','',0,0,'world','','','','',0,0);
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
) ENGINE=InnoDB AUTO_INCREMENT=1109 DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci ROW_FORMAT=COMPRESSED COMMENT='在线统计日志';
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `online_log`
--

LOCK TABLES `online_log` WRITE;
/*!40000 ALTER TABLE `online_log` DISABLE KEYS */;
INSERT INTO `online_log` VALUES (1,10,10,0,17,1585301431),(2,0,0,0,17,1585301491),(3,0,0,0,14,1585981970),(4,0,0,0,14,1585982030),(5,0,0,0,14,1585982090),(6,0,0,0,14,1585982150),(7,0,0,0,14,1585982210),(8,0,0,0,14,1585982270),(9,0,0,0,14,1585982330),(10,0,0,0,14,1585982390),(11,0,0,0,14,1585982450),(12,0,0,0,14,1585982510),(13,0,0,0,14,1585982570),(14,0,0,0,14,1585982630),(15,0,0,0,14,1585982690),(16,0,0,0,14,1585982750),(17,0,0,0,14,1585982810),(18,0,0,0,14,1585982870),(19,0,0,0,14,1585982930),(20,0,0,0,14,1585982990),(21,0,0,0,14,1585983050),(22,0,0,0,14,1585983110),(23,0,0,0,14,1585983170),(24,0,0,0,14,1585983230),(25,0,0,0,14,1585983290),(26,0,0,0,14,1585983350),(27,0,0,0,14,1585983410),(28,0,0,0,14,1585983470),(29,0,0,0,14,1585983530),(30,0,0,0,14,1585983590),(31,0,0,0,15,1585983650),(32,0,0,0,15,1585983710),(33,0,0,0,15,1585983770),(34,0,0,0,15,1585983830),(35,0,0,0,15,1585983890),(36,0,0,0,15,1585983950),(37,0,0,0,15,1585984010),(38,0,0,0,15,1585984070),(39,0,0,0,15,1585984130),(40,0,0,0,15,1585984190),(41,0,0,0,15,1585984250),(42,0,0,0,15,1585984310),(43,0,0,0,15,1585984370),(44,0,0,0,15,1585984430),(45,0,0,0,15,1585984490),(46,0,0,0,15,1585984550),(47,0,0,0,15,1585984610),(48,0,0,0,15,1585984670),(49,0,0,0,15,1585984730),(50,0,0,0,15,1585984790),(51,0,0,0,15,1585984850),(52,0,0,0,15,1585984910),(53,0,0,0,15,1585984970),(54,0,0,0,15,1585985030),(55,0,0,0,15,1585985090),(56,0,0,0,15,1585985150),(57,0,0,0,15,1585985210),(58,0,0,0,15,1585985270),(59,0,0,0,15,1585985330),(60,0,0,0,15,1585985390),(61,0,0,0,15,1585985450),(62,0,0,0,15,1585985510),(63,0,0,0,15,1585985570),(64,0,0,0,15,1585985630),(65,0,0,0,15,1585985690),(66,0,0,0,15,1585985750),(67,0,0,0,15,1585985810),(68,0,0,0,15,1585985870),(69,0,0,0,15,1585985930),(70,0,0,0,15,1585985990),(71,0,0,0,15,1585986050),(72,0,0,0,15,1585986110),(73,0,0,0,15,1585986170),(74,0,0,0,15,1585986230),(75,0,0,0,15,1585986290),(76,0,0,0,15,1585986350),(77,0,0,0,15,1585986410),(78,0,0,0,15,1585986470),(79,0,0,0,15,1585986530),(80,0,0,0,15,1585986590),(81,0,0,0,15,1585986650),(82,0,0,0,15,1585986710),(83,0,0,0,15,1585986770),(84,0,0,0,15,1585986830),(85,0,0,0,15,1585986890),(86,0,0,0,15,1585986950),(87,0,0,0,15,1585987010),(88,0,0,0,15,1585987070),(89,0,0,0,15,1585987130),(90,0,0,0,15,1585987190),(91,0,0,0,16,1585987250),(92,0,0,0,16,1585987310),(93,0,0,0,16,1585987370),(94,0,0,0,16,1585987430),(95,0,0,0,16,1585987490),(96,0,0,0,16,1585987550),(97,0,0,0,16,1585987610),(98,0,0,0,16,1585987670),(99,0,0,0,16,1585987730),(100,0,0,0,16,1585987790),(101,0,0,0,16,1585987850),(102,0,0,0,16,1585987910),(103,0,0,0,16,1585987970),(104,0,0,0,16,1585988030),(105,0,0,0,16,1585988090),(106,0,0,0,16,1585988150),(107,0,0,0,16,1585988210),(108,0,0,0,16,1585988270),(109,0,0,0,16,1585988330),(110,0,0,0,16,1585988390),(111,0,0,0,16,1585988450),(112,0,0,0,16,1585988510),(113,0,0,0,16,1585988570),(114,0,0,0,16,1585988741),(115,0,0,0,16,1585988801),(116,0,0,0,16,1585988861),(117,0,0,0,16,1585988921),(118,0,0,0,16,1585988981),(119,0,0,0,16,1585989041),(120,0,0,0,16,1585989101),(121,0,0,0,16,1585989161),(122,0,0,0,16,1585989221),(123,0,0,0,16,1585989281),(124,0,0,0,16,1585989341),(125,0,0,0,16,1585989401),(126,0,0,0,16,1585989461),(127,0,0,0,16,1585989521),(128,0,0,0,16,1585989581),(129,0,0,0,16,1585989641),(130,0,0,0,16,1585989701),(131,0,0,0,16,1585989761),(132,0,0,0,16,1585989821),(133,0,0,0,16,1585989881),(134,0,0,0,16,1585989941),(135,0,0,0,16,1585990001),(136,0,0,0,16,1585990061),(137,0,0,0,16,1585990121),(138,0,0,0,16,1585990181),(139,0,0,0,16,1585990241),(140,0,0,0,16,1585990301),(141,0,0,0,16,1585990361),(142,0,0,0,16,1585990421),(143,0,0,0,16,1585990481),(144,0,0,0,16,1585990541),(145,0,0,0,16,1585990601),(146,0,0,0,16,1585990661),(147,0,0,0,16,1585990721),(148,0,0,0,16,1585990781),(149,0,0,0,17,1585990841),(150,0,0,0,17,1585990901),(151,0,0,0,17,1585990961),(152,0,0,0,17,1585991021),(153,0,0,0,17,1585991081),(154,0,0,0,17,1585991141),(155,0,0,0,17,1585991201),(156,0,0,0,17,1585991261),(157,0,0,0,17,1585991321),(158,0,0,0,17,1585991381),(159,0,0,0,17,1585991441),(160,0,0,0,17,1585991501),(161,0,0,0,17,1585991561),(162,0,0,0,17,1585991621),(163,0,0,0,17,1585991681),(164,0,0,0,17,1585991741),(165,0,0,0,17,1585991801),(166,0,0,0,17,1585991861),(167,0,0,0,17,1585991921),(168,0,0,0,19,1585998899),(169,0,0,0,19,1585998959),(170,0,0,0,19,1585999019),(171,0,0,0,19,1585999079),(172,0,0,0,19,1585999139),(173,0,0,0,19,1585999199),(174,0,0,0,19,1585999259),(175,0,0,0,19,1585999319),(176,0,0,0,19,1585999379),(177,0,0,0,19,1585999439),(178,0,0,0,19,1585999499),(179,0,0,0,19,1585999559),(180,0,0,0,19,1585999619),(181,0,0,0,19,1585999679),(182,0,0,0,19,1585999739),(183,0,0,0,19,1585999799),(184,0,0,0,19,1585999859),(185,0,0,0,19,1585999919),(186,0,0,0,19,1585999979),(187,0,0,0,19,1586000039),(188,0,0,0,19,1586000099),(189,0,0,0,19,1586000159),(190,0,0,0,19,1586000219),(191,0,0,0,19,1586000279),(192,0,0,0,19,1586000339),(193,0,0,0,19,1586000399),(194,0,0,0,19,1586000459),(195,0,0,0,19,1586000519),(196,0,0,0,19,1586000579),(197,0,0,0,19,1586000639),(198,0,0,0,19,1586000699),(199,0,0,0,19,1586000759),(200,0,0,0,19,1586000819),(201,0,0,0,19,1586000879),(202,0,0,0,19,1586000939),(203,0,0,0,19,1586000999),(204,0,0,0,19,1586001059),(205,0,0,0,19,1586001119),(206,0,0,0,19,1586001179),(207,0,0,0,19,1586001239),(208,0,0,0,19,1586001299),(209,0,0,0,19,1586001359),(210,0,0,0,19,1586001419),(211,0,0,0,19,1586001479),(212,0,0,0,19,1586001539),(213,0,0,0,19,1586001599),(214,0,0,0,20,1586001659),(215,0,0,0,20,1586001719),(216,0,0,0,20,1586001779),(217,0,0,0,20,1586001839),(218,0,0,0,20,1586001899),(219,0,0,0,20,1586001959),(220,0,0,0,20,1586002019),(221,0,0,0,20,1586002158),(222,0,0,0,20,1586002218),(223,0,0,0,20,1586002278),(224,0,0,0,20,1586002338),(225,0,0,0,20,1586002398),(226,0,0,0,20,1586002458),(227,0,0,0,20,1586002518),(228,0,0,0,20,1586002578),(229,0,0,0,20,1586002638),(230,0,0,0,20,1586002698),(231,0,0,0,20,1586002758),(232,0,0,0,20,1586002818),(233,0,0,0,20,1586002878),(234,0,0,0,20,1586002938),(235,0,0,0,20,1586002998),(236,0,0,0,20,1586003058),(237,0,0,0,20,1586003118),(238,0,0,0,20,1586003178),(239,0,0,0,20,1586003238),(240,0,0,0,20,1586003298),(241,0,0,0,20,1586003358),(242,0,0,0,20,1586003418),(243,0,0,0,20,1586003478),(244,0,0,0,20,1586003538),(245,0,0,0,20,1586003598),(246,0,0,0,20,1586003658),(247,0,0,0,20,1586003718),(248,0,0,0,20,1586003778),(249,0,0,0,20,1586003838),(250,0,0,0,20,1586003898),(251,0,0,0,20,1586003958),(252,0,0,0,20,1586004018),(253,0,0,0,20,1586004078),(254,0,0,0,20,1586004138),(255,0,0,0,20,1586004198),(256,0,0,0,20,1586004258),(257,0,0,0,20,1586004318),(258,0,0,0,20,1586004378),(259,0,0,0,20,1586004438),(260,0,0,0,20,1586004498),(261,0,0,0,20,1586004558),(262,0,0,0,20,1586004618),(263,0,0,0,20,1586004678),(264,0,0,0,20,1586004738),(265,0,0,0,20,1586004798),(266,0,0,0,20,1586004858),(267,0,0,0,20,1586004918),(268,0,0,0,20,1586004978),(269,0,0,0,20,1586005038),(270,0,0,0,20,1586005098),(271,0,0,0,20,1586005158),(272,0,0,0,21,1586005218),(273,0,0,0,21,1586005278),(274,0,0,0,21,1586005338),(275,0,0,0,21,1586005398),(276,0,0,0,21,1586005458),(277,0,0,0,21,1586005518),(278,0,0,0,21,1586005579),(279,0,0,0,21,1586005639),(280,0,0,0,21,1586005699),(281,0,0,0,21,1586005759),(282,0,0,0,21,1586005819),(283,0,0,0,21,1586005879),(284,0,0,0,21,1586005939),(285,0,0,0,21,1586005999),(286,0,0,0,21,1586006059),(287,0,0,0,21,1586006119),(288,0,0,0,21,1586006179),(289,0,0,0,21,1586006239),(290,0,0,0,21,1586006299),(291,0,0,0,21,1586006359),(292,0,0,0,21,1586006419),(293,0,0,0,21,1586006479),(294,0,0,0,21,1586006708),(295,0,0,0,21,1586006942),(296,0,0,0,21,1586007002),(297,0,0,0,21,1586007062),(298,0,0,0,21,1586007122),(299,0,0,0,21,1586007182),(300,0,0,0,21,1586007242),(301,0,0,0,21,1586007302),(302,0,0,0,21,1586007362),(303,0,0,0,21,1586007422),(304,0,0,0,21,1586007482),(305,0,0,0,21,1586007542),(306,0,0,0,21,1586007602),(307,0,0,0,21,1586007662),(308,0,0,0,21,1586007813),(309,0,0,0,21,1586007873),(310,0,0,0,21,1586007933),(311,0,0,0,21,1586007993),(312,0,0,0,21,1586008053),(313,0,0,0,21,1586008113),(314,0,0,0,21,1586008173),(315,0,0,0,21,1586008233),(316,0,0,0,21,1586008293),(317,0,0,0,21,1586008353),(318,0,0,0,21,1586008413),(319,0,0,0,21,1586008473),(320,0,0,0,21,1586008533),(321,0,0,0,21,1586008593),(322,0,0,0,21,1586008653),(323,0,0,0,22,1586008973),(324,0,0,0,22,1586009033),(325,0,0,0,22,1586009093),(326,0,0,0,22,1586009153),(327,0,0,0,22,1586009213),(328,0,0,0,22,1586009273),(329,0,0,0,22,1586009333),(330,0,0,0,22,1586009393),(331,0,0,0,22,1586009453),(332,0,0,0,22,1586009513),(333,0,0,0,22,1586009573),(334,0,0,0,22,1586009633),(335,0,0,0,22,1586009693),(336,0,0,0,22,1586009753),(337,0,0,0,22,1586009813),(338,0,0,0,22,1586009873),(339,0,0,0,22,1586009933),(340,0,0,0,22,1586009993),(341,0,0,0,22,1586010054),(342,0,0,0,22,1586010114),(343,0,0,0,22,1586010174),(344,0,0,0,22,1586010234),(345,0,0,0,22,1586010294),(346,0,0,0,22,1586010354),(347,0,0,0,22,1586010414),(348,0,0,0,22,1586010474),(349,0,0,0,22,1586010534),(350,0,0,0,22,1586010594),(351,0,0,0,22,1586010654),(352,0,0,0,22,1586010714),(353,0,0,0,22,1586010774),(354,0,0,0,22,1586010834),(355,0,0,0,22,1586010894),(356,0,0,0,22,1586010954),(357,0,0,0,22,1586011014),(358,0,0,0,22,1586011074),(359,0,0,0,22,1586011134),(360,0,0,0,22,1586011194),(361,0,0,0,22,1586011254),(362,0,0,0,22,1586011314),(363,0,0,0,22,1586011374),(364,0,0,0,22,1586011434),(365,0,0,0,22,1586011494),(366,0,0,0,22,1586011554),(367,0,0,0,22,1586011614),(368,0,0,0,22,1586011674),(369,0,0,0,22,1586011734),(370,0,0,0,22,1586011794),(371,0,0,0,22,1586011854),(372,0,0,0,22,1586011914),(373,0,0,0,22,1586011974),(374,0,0,0,22,1586012034),(375,0,0,0,22,1586012094),(376,0,0,0,22,1586012154),(377,0,0,0,22,1586012214),(378,0,0,0,22,1586012274),(379,0,0,0,22,1586012334),(380,0,0,0,22,1586012394),(381,0,0,0,23,1586012454),(382,0,0,0,23,1586012514),(383,0,0,0,23,1586012574),(384,0,0,0,23,1586012634),(385,0,0,0,23,1586012694),(386,0,0,0,23,1586012754),(387,0,0,0,23,1586012814),(388,0,0,0,23,1586012874),(389,0,0,0,23,1586012934),(390,0,0,0,23,1586012994),(391,0,0,0,23,1586013054),(392,0,0,0,23,1586013114),(393,0,0,0,23,1586013174),(394,0,0,0,23,1586013234),(395,0,0,0,23,1586013294),(396,0,0,0,23,1586013354),(397,0,0,0,23,1586013414),(398,0,0,0,23,1586013474),(399,0,0,0,23,1586013534),(400,0,0,0,23,1586013594),(401,0,0,0,23,1586013654),(402,0,0,0,23,1586013714),(403,0,0,0,23,1586013774),(404,0,0,0,23,1586013834),(405,0,0,0,23,1586013895),(406,0,0,0,23,1586013955),(407,0,0,0,23,1586014015),(408,0,0,0,23,1586014075),(409,0,0,0,23,1586014135),(410,0,0,0,23,1586014195),(411,0,0,0,23,1586014255),(412,0,0,0,23,1586014315),(413,0,0,0,23,1586014375),(414,0,0,0,23,1586014435),(415,0,0,0,23,1586014495),(416,0,0,0,23,1586014555),(417,0,0,0,23,1586014615),(418,0,0,0,23,1586014675),(419,0,0,0,23,1586014735),(420,0,0,0,23,1586014795),(421,0,0,0,23,1586014855),(422,0,0,0,23,1586014915),(423,0,0,0,23,1586014975),(424,0,0,0,23,1586015035),(425,0,0,0,23,1586015095),(426,0,0,0,23,1586015155),(427,0,0,0,23,1586015215),(428,0,0,0,23,1586015275),(429,0,0,0,23,1586015335),(430,0,0,0,23,1586015395),(431,0,0,0,23,1586015455),(432,0,0,0,23,1586015515),(433,0,0,0,23,1586015575),(434,0,0,0,23,1586015635),(435,0,0,0,23,1586015695),(436,0,0,0,23,1586015755),(437,0,0,0,23,1586015815),(438,0,0,0,23,1586015875),(439,0,0,0,23,1586015935),(440,0,0,0,23,1586015995),(441,0,0,0,0,1586016055),(442,0,0,0,0,1586016115),(443,0,0,0,0,1586016175),(444,0,0,0,0,1586016235),(445,0,0,0,0,1586016295),(446,0,0,0,0,1586016355),(447,0,0,0,0,1586016415),(448,0,0,0,0,1586016475),(449,0,0,0,0,1586016535),(450,0,0,0,0,1586016595),(451,0,0,0,0,1586016655),(452,0,0,0,0,1586016715),(453,0,0,0,0,1586016775),(454,0,0,0,0,1586016835),(455,0,0,0,0,1586016895),(456,0,0,0,0,1586016955),(457,0,0,0,0,1586017015),(458,0,0,0,0,1586017075),(459,0,0,0,0,1586017135),(460,0,0,0,0,1586017195),(461,0,0,0,0,1586017256),(462,0,0,0,0,1586017316),(463,0,0,0,0,1586017376),(464,0,0,0,0,1586017436),(465,0,0,0,0,1586017496),(466,0,0,0,0,1586017556),(467,0,0,0,0,1586017616),(468,0,0,0,0,1586017676),(469,0,0,0,0,1586017736),(470,0,0,0,0,1586017796),(471,0,0,0,0,1586017856),(472,0,0,0,0,1586017916),(473,0,0,0,0,1586017976),(474,0,0,0,0,1586018036),(475,0,0,0,0,1586018096),(476,0,0,0,0,1586018156),(477,0,0,0,0,1586018216),(478,0,0,0,0,1586018276),(479,0,0,0,0,1586018336),(480,0,0,0,0,1586018396),(481,0,0,0,0,1586018456),(482,0,0,0,0,1586018516),(483,0,0,0,0,1586018576),(484,0,0,0,0,1586018636),(485,0,0,0,0,1586018696),(486,0,0,0,0,1586018756),(487,0,0,0,0,1586018816),(488,0,0,0,0,1586018876),(489,0,0,0,0,1586018936),(490,0,0,0,0,1586018996),(491,0,0,0,0,1586019056),(492,0,0,0,0,1586019116),(493,0,0,0,0,1586019176),(494,0,0,0,0,1586019236),(495,0,0,0,0,1586019296),(496,0,0,0,0,1586019356),(497,0,0,0,0,1586019416),(498,0,0,0,0,1586019476),(499,0,0,0,0,1586019536),(500,0,0,0,0,1586019596),(501,0,0,0,1,1586019656),(502,0,0,0,1,1586019716),(503,0,0,0,1,1586019776),(504,0,0,0,1,1586019836),(505,0,0,0,1,1586019896),(506,0,0,0,1,1586019956),(507,0,0,0,1,1586020016),(508,0,0,0,1,1586020076),(509,0,0,0,1,1586020136),(510,0,0,0,1,1586020196),(511,0,0,0,1,1586020256),(512,0,0,0,1,1586020316),(513,0,0,0,1,1586020376),(514,0,0,0,1,1586020436),(515,0,0,0,1,1586020496),(516,0,0,0,1,1586020556),(517,0,0,0,1,1586020616),(518,0,0,0,1,1586020676),(519,0,0,0,1,1586020736),(520,0,0,0,1,1586020796),(521,0,0,0,1,1586020856),(522,0,0,0,1,1586020916),(523,0,0,0,1,1586020976),(524,0,0,0,1,1586021036),(525,0,0,0,1,1586021096),(526,0,0,0,1,1586021156),(527,0,0,0,1,1586021216),(528,0,0,0,1,1586021276),(529,0,0,0,1,1586021337),(530,0,0,0,1,1586021397),(531,0,0,0,1,1586021457),(532,0,0,0,1,1586021517),(533,0,0,0,1,1586021577),(534,0,0,0,1,1586021637),(535,0,0,0,1,1586021697),(536,0,0,0,1,1586021757),(537,0,0,0,1,1586021817),(538,0,0,0,1,1586021877),(539,0,0,0,1,1586021937),(540,0,0,0,1,1586021997),(541,0,0,0,1,1586022057),(542,0,0,0,1,1586022117),(543,0,0,0,1,1586022177),(544,0,0,0,1,1586022237),(545,0,0,0,1,1586022297),(546,0,0,0,1,1586022357),(547,0,0,0,1,1586022417),(548,0,0,0,1,1586022477),(549,0,0,0,1,1586022537),(550,0,0,0,1,1586022597),(551,0,0,0,1,1586022657),(552,0,0,0,1,1586022717),(553,0,0,0,1,1586022777),(554,0,0,0,1,1586022837),(555,0,0,0,1,1586022897),(556,0,0,0,1,1586022957),(557,0,0,0,1,1586023017),(558,0,0,0,1,1586023077),(559,0,0,0,1,1586023137),(560,0,0,0,1,1586023197),(561,0,0,0,2,1586023257),(562,0,0,0,2,1586023317),(563,0,0,0,2,1586023377),(564,0,0,0,2,1586023437),(565,0,0,0,2,1586023497),(566,0,0,0,2,1586023557),(567,0,0,0,2,1586023617),(568,0,0,0,2,1586023677),(569,0,0,0,2,1586023737),(570,0,0,0,2,1586023797),(571,0,0,0,2,1586023857),(572,0,0,0,2,1586023917),(573,0,0,0,2,1586023977),(574,0,0,0,2,1586024037),(575,0,0,0,2,1586024097),(576,0,0,0,2,1586024157),(577,0,0,0,2,1586024217),(578,0,0,0,2,1586024277),(579,0,0,0,2,1586024337),(580,0,0,0,2,1586024397),(581,0,0,0,2,1586024457),(582,0,0,0,2,1586024517),(583,0,0,0,2,1586024577),(584,0,0,0,2,1586024637),(585,0,0,0,2,1586024697),(586,0,0,0,2,1586024757),(587,0,0,0,2,1586024817),(588,0,0,0,2,1586024877),(589,0,0,0,2,1586024937),(590,0,0,0,2,1586024997),(591,0,0,0,2,1586025057),(592,0,0,0,2,1586025117),(593,0,0,0,2,1586025177),(594,0,0,0,2,1586025237),(595,0,0,0,2,1586025297),(596,0,0,0,2,1586025357),(597,0,0,0,2,1586025417),(598,0,0,0,2,1586025477),(599,0,0,0,2,1586025537),(600,0,0,0,2,1586025598),(601,0,0,0,2,1586025658),(602,0,0,0,2,1586025718),(603,0,0,0,2,1586025778),(604,0,0,0,2,1586025838),(605,0,0,0,2,1586025898),(606,0,0,0,2,1586025958),(607,0,0,0,2,1586026018),(608,0,0,0,2,1586026078),(609,0,0,0,2,1586026138),(610,0,0,0,2,1586026198),(611,0,0,0,2,1586026258),(612,0,0,0,2,1586026318),(613,0,0,0,2,1586026378),(614,0,0,0,2,1586026438),(615,0,0,0,2,1586026498),(616,0,0,0,2,1586026558),(617,0,0,0,2,1586026618),(618,0,0,0,2,1586026678),(619,0,0,0,2,1586026738),(620,0,0,0,2,1586026798),(621,0,0,0,3,1586026858),(622,0,0,0,3,1586026918),(623,0,0,0,3,1586026978),(624,0,0,0,3,1586027038),(625,0,0,0,3,1586027098),(626,0,0,0,3,1586027158),(627,0,0,0,3,1586027218),(628,0,0,0,3,1586027278),(629,0,0,0,3,1586027338),(630,0,0,0,3,1586027398),(631,0,0,0,3,1586027458),(632,0,0,0,3,1586027518),(633,0,0,0,3,1586027578),(634,0,0,0,3,1586027638),(635,0,0,0,3,1586027698),(636,0,0,0,3,1586027758),(637,0,0,0,3,1586027818),(638,0,0,0,3,1586027878),(639,0,0,0,3,1586027938),(640,0,0,0,3,1586027998),(641,0,0,0,3,1586028058),(642,0,0,0,3,1586028118),(643,0,0,0,3,1586028178),(644,0,0,0,3,1586028238),(645,0,0,0,3,1586028298),(646,0,0,0,3,1586028358),(647,0,0,0,3,1586028418),(648,0,0,0,3,1586028478),(649,0,0,0,3,1586028538),(650,0,0,0,3,1586028598),(651,0,0,0,3,1586028658),(652,0,0,0,3,1586028718),(653,0,0,0,3,1586028778),(654,0,0,0,3,1586028838),(655,0,0,0,3,1586028898),(656,0,0,0,3,1586028958),(657,0,0,0,3,1586029018),(658,0,0,0,3,1586029078),(659,0,0,0,3,1586029138),(660,0,0,0,3,1586029198),(661,0,0,0,3,1586029258),(662,0,0,0,3,1586029318),(663,0,0,0,3,1586029378),(664,0,0,0,3,1586029439),(665,0,0,0,3,1586029499),(666,0,0,0,3,1586029559),(667,0,0,0,3,1586029619),(668,0,0,0,3,1586029679),(669,0,0,0,3,1586029739),(670,0,0,0,3,1586029799),(671,0,0,0,3,1586029859),(672,0,0,0,3,1586029919),(673,0,0,0,3,1586029979),(674,0,0,0,3,1586030039),(675,0,0,0,3,1586030099),(676,0,0,0,3,1586030159),(677,0,0,0,3,1586030219),(678,0,0,0,3,1586030279),(679,0,0,0,3,1586030339),(680,0,0,0,3,1586030399),(681,0,0,0,4,1586030459),(682,0,0,0,4,1586030519),(683,0,0,0,4,1586030579),(684,0,0,0,4,1586030639),(685,0,0,0,4,1586030699),(686,0,0,0,4,1586030759),(687,0,0,0,4,1586030819),(688,0,0,0,4,1586030879),(689,0,0,0,4,1586030939),(690,0,0,0,4,1586030999),(691,0,0,0,4,1586031059),(692,0,0,0,4,1586031119),(693,0,0,0,4,1586031179),(694,0,0,0,4,1586031239),(695,0,0,0,4,1586031299),(696,0,0,0,4,1586031359),(697,0,0,0,4,1586031419),(698,0,0,0,4,1586031479),(699,0,0,0,4,1586031539),(700,0,0,0,4,1586031599),(701,0,0,0,4,1586031659),(702,0,0,0,4,1586031719),(703,0,0,0,4,1586031779),(704,0,0,0,4,1586031839),(705,0,0,0,4,1586031899),(706,0,0,0,4,1586031959),(707,0,0,0,4,1586032019),(708,0,0,0,4,1586032079),(709,0,0,0,4,1586032139),(710,0,0,0,4,1586032199),(711,0,0,0,4,1586032259),(712,0,0,0,4,1586032319),(713,0,0,0,4,1586032379),(714,0,0,0,4,1586032439),(715,0,0,0,4,1586032499),(716,0,0,0,4,1586032559),(717,0,0,0,4,1586032619),(718,0,0,0,4,1586032679),(719,0,0,0,4,1586032739),(720,0,0,0,4,1586032799),(721,0,0,0,4,1586032859),(722,0,0,0,4,1586032919),(723,0,0,0,4,1586032979),(724,0,0,0,4,1586033039),(725,0,0,0,4,1586033099),(726,0,0,0,4,1586033159),(727,0,0,0,4,1586033219),(728,0,0,0,4,1586033279),(729,0,0,0,4,1586033339),(730,0,0,0,4,1586033399),(731,0,0,0,4,1586033459),(732,0,0,0,4,1586033520),(733,0,0,0,4,1586033580),(734,0,0,0,4,1586033640),(735,0,0,0,4,1586033700),(736,0,0,0,4,1586033760),(737,0,0,0,4,1586033820),(738,0,0,0,4,1586033880),(739,0,0,0,4,1586033940),(740,0,0,0,5,1586034000),(741,0,0,0,5,1586034060),(742,0,0,0,5,1586034120),(743,0,0,0,5,1586034180),(744,0,0,0,5,1586034240),(745,0,0,0,5,1586034300),(746,0,0,0,5,1586034360),(747,0,0,0,5,1586034420),(748,0,0,0,5,1586034480),(749,0,0,0,5,1586034540),(750,0,0,0,5,1586034600),(751,0,0,0,5,1586034660),(752,0,0,0,5,1586034720),(753,0,0,0,5,1586034780),(754,0,0,0,5,1586034840),(755,0,0,0,5,1586034900),(756,0,0,0,5,1586034960),(757,0,0,0,5,1586035020),(758,0,0,0,5,1586035080),(759,0,0,0,5,1586035140),(760,0,0,0,5,1586035200),(761,0,0,0,5,1586035260),(762,0,0,0,5,1586035320),(763,0,0,0,5,1586035380),(764,0,0,0,5,1586035440),(765,0,0,0,5,1586035500),(766,0,0,0,5,1586035560),(767,0,0,0,5,1586035620),(768,0,0,0,5,1586035680),(769,0,0,0,5,1586035740),(770,0,0,0,5,1586035800),(771,0,0,0,5,1586035860),(772,0,0,0,5,1586035920),(773,0,0,0,5,1586035980),(774,0,0,0,5,1586036040),(775,0,0,0,5,1586036100),(776,0,0,0,5,1586036160),(777,0,0,0,5,1586036220),(778,0,0,0,5,1586036280),(779,0,0,0,5,1586036340),(780,0,0,0,5,1586036400),(781,0,0,0,5,1586036460),(782,0,0,0,5,1586036520),(783,0,0,0,5,1586036580),(784,0,0,0,5,1586036640),(785,0,0,0,5,1586036700),(786,0,0,0,5,1586036760),(787,0,0,0,5,1586036820),(788,0,0,0,5,1586036880),(789,0,0,0,5,1586036940),(790,0,0,0,5,1586037000),(791,0,0,0,5,1586037060),(792,0,0,0,5,1586037120),(793,0,0,0,5,1586037180),(794,0,0,0,5,1586037240),(795,0,0,0,5,1586037300),(796,0,0,0,5,1586037360),(797,0,0,0,5,1586037420),(798,0,0,0,5,1586037480),(799,0,0,0,5,1586037540),(800,0,0,0,6,1586037600),(801,0,0,0,6,1586037661),(802,0,0,0,6,1586037721),(803,0,0,0,6,1586037781),(804,0,0,0,6,1586037841),(805,0,0,0,6,1586037901),(806,0,0,0,6,1586037961),(807,0,0,0,6,1586038021),(808,0,0,0,6,1586038081),(809,0,0,0,6,1586038141),(810,0,0,0,6,1586038201),(811,0,0,0,6,1586038261),(812,0,0,0,6,1586038321),(813,0,0,0,6,1586038381),(814,0,0,0,6,1586038441),(815,0,0,0,6,1586038501),(816,0,0,0,6,1586038561),(817,0,0,0,6,1586038621),(818,0,0,0,6,1586038681),(819,0,0,0,6,1586038741),(820,0,0,0,6,1586038801),(821,0,0,0,6,1586038861),(822,0,0,0,6,1586038921),(823,0,0,0,6,1586038981),(824,0,0,0,6,1586039041),(825,0,0,0,6,1586039101),(826,0,0,0,6,1586039161),(827,0,0,0,6,1586039221),(828,0,0,0,6,1586039281),(829,0,0,0,6,1586039341),(830,0,0,0,6,1586039401),(831,0,0,0,6,1586039461),(832,0,0,0,6,1586039521),(833,0,0,0,6,1586039581),(834,0,0,0,6,1586039641),(835,0,0,0,6,1586039701),(836,0,0,0,6,1586039761),(837,0,0,0,6,1586039821),(838,0,0,0,6,1586039881),(839,0,0,0,6,1586039941),(840,0,0,0,6,1586040001),(841,0,0,0,6,1586040061),(842,0,0,0,6,1586040121),(843,0,0,0,6,1586040181),(844,0,0,0,6,1586040241),(845,0,0,0,6,1586040301),(846,0,0,0,6,1586040361),(847,0,0,0,6,1586040421),(848,0,0,0,6,1586040481),(849,0,0,0,6,1586040541),(850,0,0,0,6,1586040601),(851,0,0,0,6,1586040661),(852,0,0,0,6,1586040721),(853,0,0,0,6,1586040781),(854,0,0,0,6,1586040841),(855,0,0,0,6,1586040901),(856,0,0,0,6,1586040961),(857,0,0,0,6,1586041021),(858,0,0,0,6,1586041081),(859,0,0,0,6,1586041141),(860,0,0,0,7,1586041201),(861,0,0,0,7,1586041261),(862,0,0,0,7,1586041322),(863,0,0,0,7,1586041382),(864,0,0,0,7,1586041442),(865,0,0,0,7,1586041502),(866,0,0,0,7,1586041562),(867,0,0,0,7,1586041622),(868,0,0,0,7,1586041682),(869,0,0,0,7,1586041742),(870,0,0,0,7,1586041802),(871,0,0,0,7,1586041862),(872,0,0,0,7,1586041922),(873,0,0,0,7,1586041982),(874,0,0,0,7,1586042042),(875,0,0,0,7,1586042102),(876,0,0,0,7,1586042162),(877,0,0,0,7,1586042222),(878,0,0,0,7,1586042282),(879,0,0,0,7,1586042342),(880,0,0,0,7,1586042402),(881,0,0,0,7,1586042462),(882,0,0,0,7,1586042522),(883,0,0,0,7,1586042582),(884,0,0,0,7,1586042642),(885,0,0,0,7,1586042702),(886,0,0,0,7,1586042762),(887,0,0,0,7,1586042822),(888,0,0,0,7,1586042882),(889,0,0,0,7,1586042942),(890,0,0,0,7,1586043002),(891,0,0,0,7,1586043062),(892,0,0,0,7,1586043122),(893,0,0,0,7,1586043182),(894,0,0,0,7,1586043242),(895,0,0,0,7,1586043302),(896,0,0,0,7,1586043362),(897,0,0,0,7,1586043422),(898,0,0,0,7,1586043482),(899,0,0,0,7,1586043542),(900,0,0,0,7,1586043602),(901,0,0,0,7,1586043662),(902,0,0,0,7,1586043722),(903,0,0,0,7,1586043782),(904,0,0,0,7,1586043842),(905,0,0,0,7,1586043902),(906,0,0,0,7,1586043962),(907,0,0,0,7,1586044022),(908,0,0,0,7,1586044082),(909,0,0,0,7,1586044142),(910,0,0,0,7,1586044202),(911,0,0,0,7,1586044262),(912,0,0,0,7,1586044322),(913,0,0,0,7,1586044382),(914,0,0,0,7,1586044442),(915,0,0,0,7,1586044502),(916,0,0,0,7,1586044562),(917,0,0,0,7,1586044622),(918,0,0,0,7,1586044682),(919,0,0,0,7,1586044742),(920,0,0,0,8,1586044802),(921,0,0,0,8,1586044862),(922,0,0,0,8,1586044922),(923,0,0,0,8,1586044982),(924,0,0,0,8,1586045042),(925,0,0,0,8,1586045102),(926,0,0,0,8,1586045163),(927,0,0,0,8,1586045223),(928,0,0,0,8,1586045283),(929,0,0,0,8,1586045343),(930,0,0,0,8,1586045403),(931,0,0,0,8,1586045463),(932,0,0,0,8,1586045523),(933,0,0,0,8,1586045583),(934,0,0,0,8,1586045643),(935,0,0,0,8,1586045703),(936,0,0,0,8,1586045763),(937,0,0,0,8,1586045823),(938,0,0,0,8,1586045883),(939,0,0,0,8,1586045943),(940,0,0,0,8,1586046003),(941,0,0,0,8,1586046063),(942,0,0,0,8,1586046123),(943,0,0,0,8,1586046183),(944,0,0,0,8,1586046243),(945,0,0,0,8,1586046303),(946,0,0,0,8,1586046363),(947,0,0,0,8,1586046423),(948,0,0,0,8,1586046483),(949,0,0,0,8,1586046543),(950,0,0,0,8,1586046603),(951,0,0,0,8,1586046663),(952,0,0,0,8,1586046723),(953,0,0,0,8,1586046783),(954,0,0,0,8,1586046843),(955,0,0,0,8,1586046903),(956,0,0,0,8,1586046963),(957,0,0,0,8,1586047023),(958,0,0,0,8,1586047083),(959,0,0,0,8,1586047143),(960,0,0,0,8,1586047203),(961,0,0,0,8,1586047263),(962,0,0,0,8,1586047323),(963,0,0,0,8,1586047383),(964,0,0,0,8,1586047443),(965,0,0,0,8,1586047503),(966,0,0,0,8,1586047563),(967,0,0,0,8,1586047623),(968,0,0,0,8,1586047683),(969,0,0,0,8,1586047743),(970,0,0,0,8,1586047803),(971,0,0,0,8,1586047863),(972,0,0,0,8,1586047923),(973,0,0,0,8,1586047983),(974,0,0,0,8,1586048043),(975,0,0,0,8,1586048103),(976,0,0,0,8,1586048163),(977,0,0,0,8,1586048223),(978,0,0,0,8,1586048283),(979,0,0,0,8,1586048343),(980,0,0,0,9,1586048403),(981,0,0,0,9,1586048463),(982,0,0,0,9,1586048523),(983,0,0,0,9,1586048583),(984,0,0,0,9,1586048643),(985,0,0,0,9,1586048703),(986,0,0,0,9,1586048763),(987,0,0,0,9,1586048823),(988,0,0,0,9,1586048883),(989,0,0,0,9,1586048943),(990,0,0,0,9,1586049003),(991,0,0,0,9,1586049063),(992,0,0,0,9,1586049123),(993,0,0,0,9,1586049183),(994,0,0,0,9,1586049243),(995,0,0,0,9,1586049303),(996,0,0,0,9,1586049364),(997,0,0,0,9,1586049424),(998,0,0,0,9,1586049484),(999,0,0,0,9,1586049544),(1000,0,0,0,9,1586049604),(1001,0,0,0,9,1586049664),(1002,0,0,0,9,1586049724),(1003,0,0,0,9,1586049784),(1004,0,0,0,9,1586049844),(1005,0,0,0,9,1586049904),(1006,0,0,0,9,1586049964),(1007,0,0,0,9,1586050024),(1008,0,0,0,9,1586050084),(1009,0,0,0,9,1586050144),(1010,0,0,0,9,1586050204),(1011,0,0,0,9,1586050264),(1012,0,0,0,9,1586050324),(1013,0,0,0,9,1586050384),(1014,0,0,0,9,1586050444),(1015,0,0,0,9,1586050504),(1016,0,0,0,9,1586050564),(1017,0,0,0,9,1586050624),(1018,0,0,0,9,1586050684),(1019,0,0,0,9,1586050744),(1020,0,0,0,9,1586050804),(1021,0,0,0,9,1586050864),(1022,0,0,0,9,1586050924),(1023,0,0,0,9,1586050984),(1024,0,0,0,9,1586051044),(1025,0,0,0,9,1586051104),(1026,0,0,0,9,1586051164),(1027,0,0,0,9,1586051224),(1028,0,0,0,9,1586051284),(1029,0,0,0,9,1586051344),(1030,0,0,0,9,1586051404),(1031,0,0,0,9,1586051464),(1032,0,0,0,9,1586051524),(1033,0,0,0,9,1586051584),(1034,0,0,0,9,1586051644),(1035,0,0,0,9,1586051704),(1036,0,0,0,9,1586051764),(1037,0,0,0,9,1586051824),(1038,0,0,0,9,1586051884),(1039,0,0,0,9,1586051944),(1040,0,0,0,10,1586052004),(1041,0,0,0,10,1586052064),(1042,0,0,0,10,1586052124),(1043,0,0,0,10,1586052184),(1044,0,0,0,10,1586052244),(1045,0,0,0,10,1586052304),(1046,0,0,0,10,1586052364),(1047,0,0,0,10,1586052424),(1048,0,0,0,10,1586052484),(1049,0,0,0,10,1586052544),(1050,0,0,0,10,1586052604),(1051,0,0,0,10,1586052664),(1052,0,0,0,10,1586052724),(1053,0,0,0,10,1586052784),(1054,0,0,0,10,1586052844),(1055,0,0,0,10,1586052904),(1056,0,0,0,10,1586052964),(1057,0,0,0,10,1586053024),(1058,0,0,0,10,1586053084),(1059,0,0,0,10,1586053144),(1060,0,0,0,10,1586053204),(1061,0,0,0,10,1586053264),(1062,0,0,0,10,1586053324),(1063,0,0,0,10,1586053385),(1064,0,0,0,10,1586053445),(1065,0,0,0,10,1586053505),(1066,0,0,0,10,1586053565),(1067,0,0,0,10,1586053625),(1068,0,0,0,10,1586053685),(1069,0,0,0,10,1586053745),(1070,0,0,0,10,1586053805),(1071,0,0,0,10,1586053865),(1072,0,0,0,10,1586053925),(1073,0,0,0,10,1586053985),(1074,0,0,0,10,1586054045),(1075,0,0,0,10,1586054105),(1076,0,0,0,10,1586054165),(1077,0,0,0,10,1586054225),(1078,0,0,0,10,1586054285),(1079,0,0,0,10,1586054345),(1080,0,0,0,10,1586054405),(1081,0,0,0,10,1586054465),(1082,0,0,0,10,1586054525),(1083,0,0,0,10,1586054585),(1084,0,0,0,10,1586054645),(1085,0,0,0,10,1586054705),(1086,0,0,0,10,1586054765),(1087,0,0,0,10,1586054825),(1088,0,0,0,10,1586054885),(1089,0,0,0,10,1586054945),(1090,0,0,0,10,1586055005),(1091,0,0,0,10,1586055065),(1092,0,0,0,10,1586055125),(1093,0,0,0,10,1586055185),(1094,0,0,0,10,1586055245),(1095,0,0,0,10,1586055305),(1096,0,0,0,10,1586055365),(1097,0,0,0,10,1586055425),(1098,0,0,0,10,1586055485),(1099,0,0,0,10,1586055545),(1100,0,0,0,11,1586055605),(1101,0,0,0,11,1586055665),(1102,0,0,0,11,1586055725),(1103,0,0,0,11,1586055785),(1104,0,0,0,11,1586055845),(1105,0,0,0,11,1586055905),(1106,0,0,0,21,1586178022),(1107,0,0,0,21,1586178082),(1108,0,0,0,21,1586178142);
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
  `rank` bigint(20) unsigned NOT NULL DEFAULT 0 COMMENT '排名',
  `key` bigint(20) unsigned NOT NULL DEFAULT 0 COMMENT '键',
  `value` bigint(20) unsigned NOT NULL DEFAULT 0 COMMENT '值',
  `time` int(20) unsigned NOT NULL DEFAULT 0 COMMENT '时间',
  `name` char(16) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '名字',
  `digest` varchar(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '摘要数据',
  `extra` varchar(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '额外数据',
  `other` varchar(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '其他数据',
  `flag` varchar(0) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '标识((flag)/default(1))',
  PRIMARY KEY (`type`,`rank`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci ROW_FORMAT=DYNAMIC COMMENT='角色排行表';
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `rank`
--

LOCK TABLES `rank` WRITE;
/*!40000 ALTER TABLE `rank` DISABLE KEYS */;
INSERT INTO `rank` VALUES (1,1,1,1,1,'1','','','',''),(1,2,7,7,7,'7','','','',''),(1,3,6,6,6,'6','','','',''),(1,4,5,5,5,'5','','','',''),(1,5,4,4,4,'4','','','',''),(1,6,3,3,3,'3','','','',''),(1,7,2,2,2,'2','','','','');
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
  `account` char(16) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '平台账号ID',
  `channel_id` smallint(5) unsigned NOT NULL DEFAULT 0 COMMENT '渠道ID',
  `server_id` smallint(5) unsigned NOT NULL DEFAULT 0 COMMENT '区服ID',
  `role_id` bigint(20) unsigned NOT NULL DEFAULT 0 COMMENT '玩家ID',
  `role_name` char(16) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '玩家名称',
  `money` decimal(10,2) unsigned NOT NULL DEFAULT 0.00 COMMENT '充值金额',
  `gold` int(11) unsigned NOT NULL DEFAULT 0 COMMENT '金币',
  `status` tinyint(1) unsigned NOT NULL DEFAULT 0 COMMENT '状态(0:未发放/1:已发放)',
  `time` int(11) unsigned NOT NULL DEFAULT 0 COMMENT '订单时间',
  `receive_time` int(11) unsigned NOT NULL DEFAULT 0 COMMENT '发放时间',
  PRIMARY KEY (`recharge_no`) USING BTREE,
  KEY `role_id` (`role_id`,`status`) USING BTREE,
  KEY `channel_id` (`channel_id`) USING BTREE,
  KEY `time` (`time`)
) ENGINE=InnoDB AUTO_INCREMENT=13 DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci ROW_FORMAT=DYNAMIC COMMENT='角色充值订单表';
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `recharge`
--

LOCK TABLES `recharge` WRITE;
/*!40000 ALTER TABLE `recharge` DISABLE KEYS */;
INSERT INTO `recharge` VALUES (1,1,'',0,0,0,'',6.00,0,0,1577808000,0),(2,2,'',0,0,0,'',18.00,0,0,1577894400,0),(3,3,'',0,0,0,'',68.00,0,0,1577980800,0),(4,4,'',0,0,0,'',128.00,0,0,1578067200,0),(5,5,'',0,0,0,'',268.00,0,0,1578153600,0),(6,6,'',0,0,0,'',588.00,0,0,1577808000,0),(7,7,'',0,0,0,'',688.00,0,0,1577808000,0),(8,8,'',0,0,0,'',888.00,0,0,1577894400,0),(9,9,'',0,0,0,'',1288.00,0,0,1577980800,0),(10,10,'',0,0,0,'',8888.00,0,0,1577808000,0);
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
  KEY `online_time` (`online_time`),
  KEY `register_time` (`register_time`)
) ENGINE=InnoDB AUTO_INCREMENT=8 DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci ROW_FORMAT=DYNAMIC COMMENT='角色信息表';
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `role`
--

LOCK TABLES `role` WRITE;
/*!40000 ALTER TABLE `role` DISABLE KEYS */;
INSERT INTO `role` VALUES (1,'1','1',3,100,1,1,100,100,100,0,1577808000,1577808000,1,1,'','','',''),(2,'2','2',2,200,2,2,100,100,100,0,1577894400,1577894400,1,1,'','','',''),(3,'3','3',2,300,1,3,100,100,100,0,1577980800,1577980800,1,1,'','','',''),(4,'4','4',1,400,2,4,100,100,100,0,1578067200,1578067200,1,1,'','','',''),(5,'5','5',1,500,1,5,100,100,100,0,1578153600,1578153600,1,1,'','','',''),(6,'6','6',1,600,2,6,100,100,100,0,1577808000,1577808000,1,1,'','','',''),(7,'7','7',1,700,2,7,100,100,100,0,1577808000,1577808000,1,1,'','','','');
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
INSERT INTO `shop_data` VALUES (1,1,1,'gold',10,1,0,0,0,'','');
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
  `title_id` int(10) NOT NULL DEFAULT 0 COMMENT '称号ID(select_id)',
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

-- Dump completed on 2020-04-07 20:21:12

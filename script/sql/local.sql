-- MariaDB dump 10.19  Distrib 10.5.11-MariaDB, for debian-linux-gnu (x86_64)
--
-- Host: 127.0.0.1    Database: local
-- ------------------------------------------------------
-- Server version	10.5.11-MariaDB-1:10.5.11+maria~buster

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
  `mode` tinyint(3) unsigned NOT NULL DEFAULT 0 COMMENT '活动模式(validate(node_type_integer))',
  `service` varchar(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '服务进程模块(validate(module))',
  `type` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '类型',
  `subtype` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '子类型',
  `award_type` tinyint(3) unsigned NOT NULL DEFAULT 0 COMMENT '领奖类型(自动:0/手动:1)',
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
  `type` tinyint(3) unsigned NOT NULL DEFAULT 0 COMMENT '拍卖类型(1:全服/2:公会)',
  `bid_type` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '竞拍类型(1:竞价/2:一口价)',
  `start_time` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '开始时间',
  `end_time` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '结束时间',
  `from` varchar(32) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '物品来源',
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
  `role_name` char(16) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '获得者名字',
  `server_id` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '获得者服务器ID',
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
  `auction_no` bigint(20) unsigned NOT NULL DEFAULT 0 COMMENT '拍品编号(delete_by_no)',
  `server_id` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '服务器ID',
  `role_id` bigint(20) unsigned NOT NULL DEFAULT 0 COMMENT '出价者ID',
  `role_name` char(16) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '出价者名字',
  `guild_id` bigint(20) unsigned NOT NULL DEFAULT 0 COMMENT '出价者公会ID',
  `guild_name` char(16) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '出价者公会名字',
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
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COMMENT='聊天气泡数据';
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
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COMMENT='聊天气泡配置';
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
  `from` varchar(32) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '来源',
  `time` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '时间',
  PRIMARY KEY (`id`) USING BTREE,
  KEY `role_id` (`role_id`) USING BTREE,
  KEY `time` (`time`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci ROW_FORMAT=COMPRESSED COMMENT='气泡日志表';
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
  `attribute` varchar(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '属性',
  `effect` varchar(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '效果',
  `is_temporary` varchar(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '是否临时的(切地图失效)(validate(boolean))',
  `overlap_type` tinyint(3) unsigned NOT NULL DEFAULT 0 COMMENT '叠加类型(validate(overlap_type))',
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
INSERT INTO `buff_data` VALUES (1,1,1800,'','[9]','false',3,'铜币',''),(2,1,3600,'','[10]','false',3,'经验',''),(3,2,0,'[{3,100}]','','false',2,'攻击',''),(4,2,0,'[{4,100}]','','false',2,'防御',''),(5,2,60,'','[3]','false',1,'眩晕',''),(6,3,60,'','[5]','false',0,'扣血','');
/*!40000 ALTER TABLE `buff_data` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `classes_data`
--

DROP TABLE IF EXISTS `classes_data`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `classes_data` (
  `classes` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '物品配置ID',
  `name` char(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '资产类型',
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
  `condition` varchar(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '条件',
  `cost` varchar(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '消耗',
  `day_number` varchar(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '每日次数',
  `buy_number` varchar(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '购买次数',
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
INSERT INTO `effect_data` VALUES (1,'active','battle','','10000','_','add','self','hurt','','Hurt',0,'','伤害'),(2,'active','battle','','10000','_','add','self','hurt','','Hurt * 1.5',0,'','增加50%伤害'),(3,'active','battle','','10000','_','add','self','attribute','vertigo','1',0,'','眩晕'),(4,'active','battle','','10000','_','reduce','self','attribute','vertigo','0',0,'','清除眩晕'),(5,'active','battle','','10000','_','reduce','self','attribute','hp','Rival.Attribute.health * 0.01',3600,'','每秒扣血，总血量百分之1'),(6,'active','battle','','10000','_','add','mate','attribute','attack','Mate.Attribute.attack * 1.5',3,'','增加队友攻击150%'),(7,'active','battle','','10000','_','add','mate','attribute','defense','Mate.Attribute.defense * 1.5',3,'','增加队友防御150%'),(8,'active','battle','','10000','_','add','self','buff','','[1]',0,'','添加Buff'),(9,'active','user','','10000','_','add','self','asset','copper','1.5',0,'','增加150%铜币'),(10,'active','user','','10000','_','add','self','asset','exp','2',0,'','增加200%经验');
/*!40000 ALTER TABLE `effect_data` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `error_code_data`
--

DROP TABLE IF EXISTS `error_code_data`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `error_code_data` (
  `type` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '类型',
  `key` varchar(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '键',
  `en` char(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '英文',
  `sc` char(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '简体中文',
  `tc` char(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '繁体中文',
  PRIMARY KEY (`type`,`key`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci ROW_FORMAT=DYNAMIC COMMENT='错误码配置表';
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `error_code_data`
--

LOCK TABLES `error_code_data` WRITE;
/*!40000 ALTER TABLE `error_code_data` DISABLE KEYS */;
INSERT INTO `error_code_data` VALUES (10002,'create_limit','create limit','角色达到上限','角色达到上限'),(10002,'create_refuse','no such account','没有此账户','没有此账户'),(10002,'duplicate','duplicate','重复创建账号','重复创建账号'),(10002,'invalid_classes','invalid classes','无效职业','无效职业'),(10002,'invalid_sex','invalid sex','无效性别','无效性别'),(10002,'name_duplicate','name duplicate','名字重复','名字重复'),(10002,'name_length','name length max','名字长度不对','名字长度不对'),(10002,'name_not_utf8','name not utf8 charset','未知字符','未知字符'),(10002,'name_sensitive','name sensitive','名字包含敏感词','名字包含敏感词'),(10002,'refuse','refuse','禁止登录','禁止登录'),(10002,'server_id_not_match','server id not match','服务器ID不匹配','服务器ID不匹配'),(10003,'duplicate','duplicate','重复登录','重复登录'),(10003,'no_such_account','no such account','没有此账号','没有此账号'),(10003,'permission_denied','permission denied','权限不够','权限不够'),(10003,'refuse','refuse','禁止登录','禁止登录'),(10003,'server_id_not_match','server id not match','服务器ID不匹配','服务器ID不匹配'),(10003,'server_update','server update','服务器更新','服务器更新'),(10004,'heartbeat_packet_fast_error','heartbeat packet fast error','心跳包速度过快','心跳包速度过快'),(10004,'logout','logout','注销','注销'),(10004,'no_such_name','no such name','没有此用户名','没有此用户名'),(10004,'packet_fast_error','packet fast error','包速度过快','包速度过快'),(10004,'server_id_not_match','server id not match','服务器ID不匹配','服务器ID不匹配'),(10004,'server_update','server update','服务器更新','服务器更新'),(11106,'configure_not_found','configure not found','配置错误','配置错误'),(11106,'invalid_item','invalid item','无效物品','无效物品'),(11106,'item_cannot_use_directly','item cannot use directly','物品不能直接使用','物品不能直接使用'),(11106,'use_number_max','use number max','使用个数超过单次使用上限','使用个数超过单次使用上限'),(11202,'condition_not_met','condition not met','条件不满足','条件不满足'),(11202,'configure_not_found','configure not found','配置错误','配置错误'),(11202,'no_such_quest','no such quest','没有此任务','没有此任务'),(11202,'not_next_quest','not next quest','请按顺序完成','请按顺序完成'),(11202,'pre_quest_not_complete','pre quest not complete','前置任务还没完成','前置任务还没完成'),(11203,'configure_not_found','configure not found','配置错误','配置错误'),(11203,'no_such_quest','no such quest','没有此任务','没有此任务'),(11203,'quest_already_submit','quest already submit','任务已提交','任务已提交'),(11203,'quest_not_complete','quest not complete','任务还没完成','任务还没完成'),(11302,'asset_not_enough','asset not enough','资产不足','资产不足'),(11302,'buy_max','buy max','已达到购买上限','已达到购买上限'),(11302,'configure_not_found','configure not found','配置错误','配置错误'),(11302,'level_not_satisfy','level not enough','等级不满足','等级不满足'),(11302,'number_invalid','number invalid','购买数量错误','购买数量错误'),(11302,'vip_level_not_satisfy','vip level not enough','Vip等级不满足','Vip等级不满足'),(11402,'already_read','already read','邮件已阅读过','邮件已阅读过'),(11402,'no_such_mail','no such mail','没有此邮件','没有此邮件'),(11403,'bag_full','bag full','背包已满','背包已满'),(11403,'no_attachment','no attachment','没有可领取附件','没有可领取附件'),(11403,'no_such_mail','no such mail','没有此邮件','没有此邮件'),(11502,'friend_level_not_satisfy','friend level not enough','对方好友未开放','对方好友未开放'),(11502,'friend_number_max','friend number max','好友数量达到上限','好友数量达到上限'),(11502,'level_not_satisfy','level not enough','好友未开放','好友未开放'),(11502,'user_offline','user offline','对方不在线','对方不在线'),(11503,'no_such_apply','no such apply','没有此好友的申请','没有此好友的申请'),(11601,'level_not_satisfy','level not enough','等级不足','等级不足'),(11601,'time_in_cd','time in cd','时间冷却中','时间冷却中'),(11602,'level_not_satisfy','level not enough','等级不足','等级不足'),(11602,'no_guild','no guild','没加入公会','没加入公会'),(11602,'time_in_cd','time in cd','时间冷却中','时间冷却中'),(11603,'level_not_satisfy','level not enough','等级不足','等级不足'),(11603,'user_offline','user offline','对方不在线','对方不在线'),(11702,'condition_not_met','condition not met','条件不足','条件不足'),(11702,'configure_not_found','configure not found','配置错误','配置错误'),(11702,'item_not_enough','item not enough','材料不足','材料不足'),(15001,'already_sign_today','already sign today','今天已经签到过了','今天已经签到过了'),(15001,'award_error','award error','奖励配置错误','奖励配置错误'),(15002,'key_already_active','key already active','此兑换码已经兑换过了','此兑换码已经兑换过了'),(15002,'timeout','timeout','请求超时','请求超时'),(15004,'lucky_money_already_receive','lucky money already receive','红包已领取过','红包已领取过'),(15004,'lucky_money_expire','lucky money expire','红包已过期','红包已过期'),(15004,'no_such_lucky_money','no such lucky money','此兑换码已经兑换过了','此兑换码已经兑换过了'),(15004,'timeout','timeout','请求超时','请求超时'),(16102,'gold_not_enough','gold not enough','元宝不足','元宝不足'),(16102,'no_such_auction','no such auction','没有此拍品','没有此拍品'),(16102,'price_change','price change','价格已变化','价格已变化'),(16102,'timeout','timeout','请求超时','请求超时'),(17002,'condition_not_met','condition not met','条件不满足','条件不满足'),(17002,'configure_not_found','configure not found','配置错误','配置错误'),(17002,'item_not_enough','item not enough','消耗材料不足','消耗材料不足'),(17002,'today_number_limit','today number limit','今天进入次数已达到上限','今天进入次数已达到上限'),(18001,'no_such_boss','no such boss','没有此Boss','没有此Boss'),(30107,'already_join_guild','already join guild','你已经加入过公会了','你已经加入过公会了'),(30107,'condition_not_met','condition not met','条件不足','条件不足'),(30107,'cost_not_enough','cost not enough','资产不足','资产不足'),(30107,'duplicate','duplicate','公会名字重复','公会名字重复'),(30107,'length','length','长度不对','长度不对'),(30107,'not_utf8','not utf8','未知字符','未知字符'),(30107,'sensitive','sensitive','名字包含敏感词','名字包含敏感词'),(30107,'time_in_join_cd','time in join cd','创建公会时间冷却中','创建公会时间冷却中'),(30107,'timeout','timeout','请求超时','请求超时'),(30107,'unknown_type','unknown type','未知类型','未知类型'),(30108,'already_join_guild','already join guild','你已经加入过公会了','你已经加入过公会了'),(30108,'condition_not_met','condition not met','条件不足','条件不足'),(30108,'no_such_guild','no such guild','没有此公会','没有此公会'),(30108,'time_in_join_cd','time in join cd','加入公会时间冷却中','加入公会时间冷却中'),(30108,'timeout','timeout','请求超时','请求超时'),(30109,'timeout','timeout','请求超时','请求超时'),(30110,'timeout','timeout','请求超时','请求超时'),(30111,'already_join_guild','already join guild','已加入其它公会','已加入其它公会'),(30111,'member_number_limit','member number limit','已达到成员上限','已达到成员上限'),(30111,'no_such_apply','no such apply','没有此申请','没有此申请'),(30111,'no_such_guild','no such guild','没有此公会','没有此公会'),(30111,'permission_denied','permission denied','权限不足','权限不足'),(30111,'timeout','timeout','请求超时','请求超时'),(30112,'permission_denied','permission denied','权限不足','权限不足'),(30112,'timeout','timeout','请求超时','请求超时'),(30113,'permission_denied','permission denied','权限不足','权限不足'),(30113,'timeout','timeout','请求超时','请求超时'),(30113,'you_not_join_guild','you not join guild','你没有加入任何公会','你没有加入任何公会'),(30114,'permission_denied','permission denied','权限不足','权限不足'),(30114,'timeout','timeout','请求超时','请求超时'),(30115,'timeout','timeout','请求超时','请求超时'),(30115,'you_not_join_guild','you not join guild','你没有加入任何公会','你没有加入任何公会'),(30116,'permission_denied','permission denied','权限不足','权限不足'),(30116,'timeout','timeout','请求超时','请求超时'),(30116,'you_not_join_guild','you not join guild','你没有加入任何公会','你没有加入任何公会'),(30117,'cannot_kick_self','cannot kick self','不可剔除自己','不可剔除自己'),(30117,'he_not_join_guild','he not join guild','此人没有加入公会','此人没有加入公会'),(30117,'permission_denied','permission denied','权限不足','权限不足'),(30117,'timeout','timeout','请求超时','请求超时'),(30117,'you_not_join_guild','you not join guild','你没有加入任何公会','你没有加入任何公会'),(30118,'cannot_update_self','cannot update self','不可升级自己','不可升级自己'),(30118,'he_not_join_guild','he not join guild','此人没有加入公会','此人没有加入公会'),(30118,'job_invalid','job invalid','位置无效','位置无效'),(30118,'permission_denied','permission denied','权限不足','权限不足'),(30118,'timeout','timeout','请求超时','请求超时'),(30118,'you_not_join_guild','you not join guild','你没有加入任何公会','你没有加入任何公会'),(30119,'timeout','timeout','请求超时','请求超时'),(30120,'timeout','timeout','请求超时','请求超时'),(60002,'no_such_command','no such command','没有找到命令','没有找到命令');
/*!40000 ALTER TABLE `error_code_data` ENABLE KEYS */;
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
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COMMENT='玩家时装表';
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
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COMMENT='时装配置表';
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `fashion_data`
--

LOCK TABLES `fashion_data` WRITE;
/*!40000 ALTER TABLE `fashion_data` DISABLE KEYS */;
INSERT INTO `fashion_data` VALUES (101,1,'false',0,'[{3,30},{4,40}]','小试牛刀','VIP1可获得'),(102,1,'false',0,'[{3,30},{4,40}]','有钱任性','VIP2可获得'),(103,1,'false',0,'[{3,30},{4,40}]','一掷千金','VIP3可获得'),(104,1,'false',0,'[{3,30},{4,40}]','腰缠万贯','VIP4可获得'),(105,1,'false',0,'[{3,30},{4,40}]','挥金如土','VIP5可获得'),(106,1,'false',0,'[{3,30},{4,40}]','富甲天下','VIP6可获得'),(107,1,'false',0,'[{3,30},{4,40}]','富可敌国','VIP7可获得'),(108,1,'false',0,'[{3,30},{4,40}]','人生巅峰','VIP8可获得'),(109,1,'false',0,'[{3,30},{4,40}]','至尊王者','VIP9可获得'),(110,1,'false',0,'[{3,30},{4,40}]','高手对决','VIP0可获得'),(201,2,'false',0,'[{6,60},{7,70}]','武艺超群','开服冲榜活动获取'),(202,2,'false',0,'[{6,60},{7,70}]','出神入化','开服冲榜活动获取'),(203,2,'false',0,'[{6,60},{7,70}]','仙武主宰','开服冲榜活动获取'),(204,2,'false',0,'[{6,60},{7,70}]','锻造大师','开服冲榜活动获取'),(205,2,'false',0,'[{6,60},{7,70}]','黑暗主宰','开服冲榜活动获取'),(206,2,'false',0,'[{6,60},{7,70}]','聚魂先锋','开服冲榜活动获取'),(207,2,'false',0,'[{6,60},{7,70}]','全职高手','开服冲榜活动获取'),(208,2,'false',0,'[{6,60},{7,70}]','人中之龙','开服冲榜活动获取'),(209,2,'false',0,'[{6,60},{7,70}]','勇者无畏','开服冲榜活动获取'),(210,2,'false',0,'[{6,60},{7,70}]','称霸天下','开服冲榜活动获取'),(10010,3,'true',0,'[{5,50}]','归隐山林','充值获取');
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
  `from` varchar(32) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '来源',
  `time` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '时间',
  PRIMARY KEY (`id`) USING BTREE,
  KEY `role_id` (`role_id`) USING BTREE,
  KEY `time` (`time`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci ROW_FORMAT=COMPRESSED COMMENT='时装日志表';
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
  `guild_name` char(16) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '名字(update_name)',
  `exp` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '经验',
  `wealth` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '财富',
  `level` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '等级',
  `create_time` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '时间',
  `notice` char(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '公告(update_notice)',
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
  `condition` varchar(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '条件',
  `cost` varchar(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '消耗',
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
  `name` char(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '名字',
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
  `operation` varchar(32) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '操作',
  `from` varchar(32) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '来源',
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
  `type` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '类型(validate(item_type))',
  `overlap` int(10) unsigned NOT NULL DEFAULT 1 COMMENT '叠加数',
  `category` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '分类ID',
  `expire_time` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '过期时间',
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
INSERT INTO `item_data` VALUES (1,1,1000,0,0,0,'',0,'rust','file_type_rust.svg',''),(2,1,100,0,0,0,'',0,'erlang','file_type_erlang.svg',''),(3,1,10,0,0,0,'',0,'php','file_type_php.svg',''),(4,2,1,0,0,0,'',0,'lua','file_type_lua.svg',''),(5,2,1,0,0,0,'',0,'js','file_type_js.svg',''),(6,2,1,0,0,0,'',0,'html','file_type_html.svg',''),(7,2,1,0,0,0,'',0,'css','file_type_css.svg',''),(100001,10,1,0,0,0,'gold',0,'gold','file_type_gold.svg',''),(100002,10,1,0,0,0,'silver',0,'silver','file_type_silver.svg',''),(100003,10,1,0,0,0,'copper',0,'copper','file_type_copper.svg',''),(100004,10,1,0,0,0,'exp',0,'exp','file_type_exp.svg',''),(100005,10,1,0,0,0,'coin',0,'coin','file_type_coin.svg','');
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
  `operation` varchar(32) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '操作',
  `from` varchar(32) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '来源',
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
  `key` char(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '码(select_by_key)',
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
  `is_unique` varchar(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '是否唯一(validate(boolean))',
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
  `ip` char(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '登录IP',
  `device_id` char(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '设备ID',
  `login_time` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '登录时间',
  `online_time` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '在线时间',
  `logout_time` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '登出时间',
  `time` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '时间',
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
  `lucky_money_no` bigint(20) unsigned NOT NULL AUTO_INCREMENT COMMENT '红包编号',
  `server_id` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '服务器ID',
  `role_id` bigint(20) unsigned NOT NULL DEFAULT 0 COMMENT '角色ID',
  `role_name` char(16) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '角色名',
  `guild_id` bigint(20) unsigned NOT NULL DEFAULT 0 COMMENT '公会ID',
  `guild_name` char(16) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '公会名',
  `total_gold` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '总金币',
  `remain_gold` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '剩余金币',
  `total_number` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '总人数',
  `receive_number` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '已领取人数',
  `receive_list` varchar(0) GENERATED ALWAYS AS ('') VIRTUAL COMMENT '领取列表',
  `scope` varchar(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '范围',
  `restrict` bigint(20) unsigned NOT NULL DEFAULT 0 COMMENT '限制',
  `skin` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '皮肤',
  `message` char(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '消息',
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
  `role_name` char(16) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '角色名',
  `guild_id` bigint(20) unsigned NOT NULL DEFAULT 0 COMMENT '公会ID',
  `guild_name` char(16) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '公会名',
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
  `is_read` tinyint(3) unsigned NOT NULL DEFAULT 0 COMMENT '是否已经读取(update_read)',
  `read_time` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '读取时间(update_read)',
  `expire_time` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '过期时间',
  `is_receive_attachment` tinyint(3) unsigned NOT NULL DEFAULT 0 COMMENT '是否领取附件(update_receive)',
  `receive_attachment_time` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '领取附件时间(update_receive)',
  `title` char(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '标题',
  `content` char(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '内容',
  `attachment` varchar(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '附件',
  `from` varchar(32) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '来源',
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
INSERT INTO `map_data` VALUES (100000,'slice','false','','','hurt','share','','','','',''),(110001,'full','false','','','hurt','share','','','','',''),(110002,'full','false','','','hurt','share','','','','',''),(110003,'full','false','','','hurt','share','','','','',''),(120001,'full','false','','','hurt','share','','','','',''),(120002,'full','false','','','hurt','share','','','','',''),(120003,'full','false','','','hurt','share','','','','',''),(200001,'slice','true','','role','hurt','share','','','','',''),(200002,'slice','true','','role','hurt','share','','','','',''),(200003,'slice','true','','role','hurt','share','','','','','');
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
  `name` char(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '怪物名称',
  `description` char(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '怪物描述',
  `level` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '等级',
  `hp` bigint(20) unsigned NOT NULL DEFAULT 0 COMMENT '血量',
  `map_id` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '地图ID',
  `camp` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '阵营',
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
INSERT INTO `monster_data` VALUES (1,1,'active','active',1,100,200001,1,1,300,0,'active','[role]','[1]','[{10,10},{20,10},{30,10},{40,10},{50,10},{60,10},{70,10},{10,10},{90,10},{100,10}]','[{100005,100}]'),(2,2,'passive','passive',1,200,200002,1,2,300,0,'passive','[enemy]','[1]','[{40,10}]','[{100005,200}]'),(3,3,'movable','movable',1,300,200003,1,3,300,0,'movable','','','[{60,10}]','[{100005,300}]'),(4,4,'fix','fix',1,400,0,1,4,300,0,'fix','','','[{80,10}]',''),(5,5,'act','act',1,500,0,1,5,300,0,'fix','[enemy]','','[{100,10}]',''),(6,6,'boom','boom',1,600,0,1,6,300,0,'active','[{monster, 20}, {monster, 50}, role]','','[{120,10}]','[{100005,600}]'),(7,5,'act','act',1,700,0,1,7,300,0,'fix','[enemy]','','[{140,10}]',''),(8,6,'boom','boom',1,800,0,1,8,300,0,'fix','[{monster, 20}, {monster, 50}, role]','','[{160,10}]','');
/*!40000 ALTER TABLE `monster_data` ENABLE KEYS */;
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
INSERT INTO `parameter_data` VALUES ('bag_size','100','装备背包大小'),('chat_cd','0','聊天冷却时间'),('chat_guild_size_limit','100','公会聊天保留条数'),('chat_level','0','聊天开放等级'),('chat_private_size_limit','100','私聊保留条数'),('chat_system_size_limit','100','系统信息条数'),('chat_world_size_limit','100','世界聊天保留条数'),('dungeon_inspire_buff_id','3','副本鼓舞BuffID'),('friend_level','0','好友开放等级'),('friend_number','50','好友上限'),('guild_create_cd','86400','公会创建冷却时间'),('guild_join_cd','86400','公会加入冷却时间'),('guild_member_limit','[{0, 50}, {1, 60}, {2, 70}, {3, 80}, {4, 90}, {5, 100}]','公会人员数'),('item_size','100','道具背包大小'),('language','sc','默认语言'),('login_cd','180','登录时间间隔'),('mail_expire_time','604800','邮件过期时间'),('mail_max_item','10','单封邮件最大物品数'),('store_size','100','仓库大小'),('time_zone','8','时区');
/*!40000 ALTER TABLE `parameter_data` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `quest`
--

DROP TABLE IF EXISTS `quest`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `quest` (
  `role_id` bigint(20) unsigned NOT NULL DEFAULT 0 COMMENT '角色ID(select_by_role_id)',
  `quest_id` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '任务ID',
  `type` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '类型',
  `target` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '目标',
  `number` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '数量',
  `is_award` tinyint(3) unsigned NOT NULL DEFAULT 0 COMMENT '是否领取奖励',
  `flag` tinyint(3) unsigned GENERATED ALWAYS AS (0) VIRTUAL COMMENT '标识(flag)',
  PRIMARY KEY (`role_id`,`type`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci ROW_FORMAT=DYNAMIC COMMENT='角色任务表';
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `quest`
--

LOCK TABLES `quest` WRITE;
/*!40000 ALTER TABLE `quest` DISABLE KEYS */;
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
INSERT INTO `quest_data` VALUES (1,1,0,2,'event_kill_monster','nc',0,3,'','','[{1,1}]','','',''),(2,1,1,3,'event_level_upgrade','ge',5,1,'','[{100003, 100}]','[{1,10}]','','',''),(3,1,2,4,'event_dungeon_passed','ge',2,1,'[{level, 10}]','','[{1,100}]','','',''),(4,1,3,5,'event_shop_buy','eq',1,1,'','','[{1,1000}]','','',''),(5,1,4,0,'event_guild_join','nc',0,1,'','','[{1,1000}]','','',''),(6,1,5,0,'event_friend_add','nc',0,5,'','','[{1,10}]','','',''),(1001,2,0,1002,'event_dungeon_passed','ge',3,1,'','','[{1,10}]','','',''),(1002,2,1001,0,'event_friend_add','eq',1,1,'','','[{1,10}]','','',''),(100001,3,0,100002,'event_shop_buy','eq',1,1,'','','[{1,10}]','','',''),(100002,3,100001,0,'event_guild_join','nc',0,1,'','','[{1,10}]','','','');
/*!40000 ALTER TABLE `quest_data` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `quest_log`
--

DROP TABLE IF EXISTS `quest_log`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `quest_log` (
  `id` bigint(20) unsigned NOT NULL AUTO_INCREMENT COMMENT 'ID',
  `role_id` bigint(20) unsigned NOT NULL DEFAULT 0 COMMENT '角色ID',
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
  `type` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '类型(select_by_type)(delete_by_type)',
  `order` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '排名',
  `key` bigint(20) unsigned NOT NULL DEFAULT 0 COMMENT '键',
  `value` bigint(20) unsigned NOT NULL DEFAULT 0 COMMENT '值',
  `time` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '时间',
  `name` char(16) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '名字',
  `server_id` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '服务器ID',
  `digest` varchar(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '摘要数据',
  `extra` varchar(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '额外数据',
  `other` varchar(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '其他数据',
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
-- Table structure for table `recharge`
--

DROP TABLE IF EXISTS `recharge`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `recharge` (
  `recharge_no` bigint(20) unsigned NOT NULL AUTO_INCREMENT COMMENT '充值编号',
  `recharge_id` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '充值ID',
  `order_id` char(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '订单ID',
  `channel` char(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '渠道',
  `role_id` bigint(20) unsigned NOT NULL DEFAULT 0 COMMENT '玩家ID',
  `role_name` char(16) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '玩家名称',
  `money` decimal(10,2) unsigned NOT NULL DEFAULT 0.00 COMMENT '充值金额',
  `status` tinyint(3) unsigned NOT NULL DEFAULT 0 COMMENT '状态(update_status)',
  `time` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '订单时间',
  PRIMARY KEY (`recharge_no`) USING BTREE,
  UNIQUE KEY `order_id` (`order_id`) USING BTREE,
  KEY `channel` (`channel`) USING BTREE,
  KEY `time` (`time`) USING BTREE,
  KEY `role_id` (`role_id`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci ROW_FORMAT=DYNAMIC COMMENT='角色充值订单表';
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `recharge`
--

LOCK TABLES `recharge` WRITE;
/*!40000 ALTER TABLE `recharge` DISABLE KEYS */;
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
INSERT INTO `recharge_data` VALUES (1,3,1,6,6.00,6.00,6,0,1,9999,1,'0','至尊神兵宝箱',''),(2,1,1,18,18.00,18.00,18,5,1,9999,2,'1','元宝',''),(3,1,1,68,68.00,68.00,68,40,1,9999,3,'2','元宝',''),(4,1,1,128,128.00,128.00,128,90,1,9999,4,'3','元宝',''),(5,1,1,268,268.00,268.00,268,190,1,9999,5,'4','元宝',''),(6,1,1,588,588.00,588.00,588,330,1,9999,6,'5','元宝',''),(7,1,1,688,688.00,688.00,688,590,1,9999,7,'6','元宝',''),(8,1,1,888,888.00,888.00,888,1300,1,9999,8,'7','元宝',''),(9,2,1,1288,1288.00,1288.00,1288,0,1,9999,0,'','周卡',''),(10,6,1,8888,8888.00,8888.00,8888,0,1,9999,0,'','月卡','');
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
  `role_name` char(16) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '角色名(update_name)',
  `server_id` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '服务器ID',
  `account_name` char(16) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '账户',
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
  `first_recharge_time` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '首充时间',
  `last_recharge_time` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '最后充值时间',
  `recharge_total` decimal(10,2) unsigned NOT NULL DEFAULT 0.00 COMMENT '总充值',
  `item_size` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '普通背包大小',
  `bag_size` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '装备背包大小',
  `store_size` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '仓库背包大小',
  `map` varchar(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '地图',
  `channel` char(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '渠道',
  `device_id` char(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '设备ID',
  `device_type` char(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '设备类型',
  `mac` char(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT 'Mac地址',
  `ip` char(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT 'IP地址',
  PRIMARY KEY (`role_id`) USING BTREE,
  UNIQUE KEY `role_name` (`role_name`) USING BTREE,
  KEY `account_name` (`account_name`) USING BTREE,
  KEY `register_time` (`register_time`) USING BTREE,
  KEY `first_recharge_time` (`first_recharge_time`) USING BTREE
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
-- Table structure for table `sex_data`
--

DROP TABLE IF EXISTS `sex_data`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `sex_data` (
  `sex` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '物品配置ID',
  `name` char(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '资产类型',
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
  `pay_asset` varchar(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '货币类型(validate(asset))',
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
  `id` bigint(20) unsigned NOT NULL AUTO_INCREMENT COMMENT 'ID',
  `role_id` bigint(20) unsigned NOT NULL DEFAULT 0 COMMENT '角色ID',
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
-- Table structure for table `sign_data`
--

DROP TABLE IF EXISTS `sign_data`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `sign_data` (
  `day` tinyint(3) unsigned NOT NULL DEFAULT 0 COMMENT '签到天数',
  `award` varchar(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '奖励',
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
  `type` varchar(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '类型(validate(skill_type))',
  `classes` tinyint(3) unsigned NOT NULL DEFAULT 0 COMMENT '职业(validate(classes))',
  `name` char(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '名字',
  `condition` varchar(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '学习条件',
  `cost` varchar(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '升级消耗',
  `attribute` varchar(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '属性',
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
  `name` char(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '名字',
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
-- Table structure for table `text_data`
--

DROP TABLE IF EXISTS `text_data`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `text_data` (
  `key` varchar(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '键',
  `en` char(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '英文',
  `sc` char(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '简体中文',
  `tc` char(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '繁体中文',
  `description` char(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '描述',
  PRIMARY KEY (`key`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci ROW_FORMAT=DYNAMIC COMMENT='游戏文本配置表';
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `text_data`
--

LOCK TABLES `text_data` WRITE;
/*!40000 ALTER TABLE `text_data` DISABLE KEYS */;
INSERT INTO `text_data` VALUES ('add_item_content','Your bag is full, the new props have been put in the mail, please check.','您的背包已满，新增的道具已经放到了邮件里，请注意查收。','您的背包已滿，新增的道具已經放到了郵件裏，請注意查收。','背包满内容'),('add_item_title','Bag full','背包已满','背包已滿','背包满标题'),('auction_income_content','Your share of auction revenue.','您的拍卖收入分成。','您的拍賣收入分成。','拍卖分红内容'),('auction_income_title','Your auction revenue','拍卖收入','拍賣收入','拍卖分红标题'),('auction_success_content','Please check your auction items.','您的拍卖物品，请注意查收。','您的拍賣物品，請注意查收。','拍卖成功内容'),('auction_success_title','Auction Succeed','拍卖成功','拍賣成功','拍卖成功标题'),('guild_create','Celebrate <id>~w</id> ~s create guild <id>~w</id> ~s','<id>~w</id>~s创建公会<id>~w</id>~s','<id>~w</id>~s創建公會<id>~w</id>~s','创建公会公告'),('level_upgrade','Celebrate <id>~w</id> ~s upgrade level to ~w','恭喜<id>~w</id>~s升到~w级','祝賀<id>~w</id>~s升到~w級','升级公告'),('test','😂','😂','😂','😒'),('vip_upgrade','Celebrate <id>~w</id> ~s upgrade Vip level to ~w','恭喜<id>~w</id>~sVip升到~w级','祝賀<id>~w</id>~sVip升到~w級','Vip升级公告');
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
  `multi` varchar(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '同类型可否拥有多个(validate(boolean))',
  `is_unique` varchar(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '是否全服唯一(validate(boolean))',
  `expire_time` int(10) unsigned NOT NULL DEFAULT 0 COMMENT '过期时间',
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
  `from` varchar(32) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '来源',
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
-- Table structure for table `validate_data`
--

DROP TABLE IF EXISTS `validate_data`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `validate_data` (
  `type` varchar(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '类型',
  `key` varchar(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '键',
  `value` char(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '值',
  PRIMARY KEY (`type`,`key`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci COMMENT='数据校验配置表';
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `validate_data`
--

LOCK TABLES `validate_data` WRITE;
/*!40000 ALTER TABLE `validate_data` DISABLE KEYS */;
INSERT INTO `validate_data` VALUES ('act_script','enemy','敌人'),('act_script','location','位置'),('act_script','monster','怪物'),('act_script','role','玩家'),('act_type','active','主动'),('act_type','fix','固定'),('act_type','movable','移动'),('act_type','passive','被动'),('activity_service','','无'),('activity_service','auction','拍卖'),('activity_service','boss','BOSS'),('asset','','无'),('asset','coin','硬币'),('asset','copper','铜币'),('asset','exp','经验'),('asset','gold','金币'),('asset','silver','银币'),('bool','0','否'),('bool','1','是'),('boolean','false','否'),('boolean','true','是'),('classes','0','无限制'),('classes','1','七杀'),('classes','2','天师'),('classes','3','飞羽'),('classes','4','御灵'),('classes','5','妙音'),('classes','6','星术'),('compare','eq','等于'),('compare','ge','大于等于'),('compare','gt','大于'),('compare','le','小于等于'),('compare','lt','小于'),('compare','nc','不比较'),('compare','ne','不等于'),('dungeon_type','0','无'),('dungeon_type','1','经验副本'),('dungeon_type','2','铜币副本'),('effect_attribute','asset','资产'),('effect_attribute','attribute','属性'),('effect_attribute','buff','Buff'),('effect_attribute','hurt','伤害'),('effect_attribute','skill','技能'),('effect_field','','无'),('effect_field','attack','攻击'),('effect_field','copper','铜币'),('effect_field','defense','防御'),('effect_field','destroy','毁灭'),('effect_field','duck','闪避'),('effect_field','exp','经验'),('effect_field','fc','战力'),('effect_field','freeze','冰冻'),('effect_field','health','生命'),('effect_field','hit','命中'),('effect_field','hp','血量'),('effect_field','vertigo','眩晕'),('effect_object','mate','队友'),('effect_object','rival','对方'),('effect_object','self','自己'),('effect_operation','add','增加'),('effect_operation','clear','清除'),('effect_operation','reduce','减少'),('effect_operation','set','设置'),('effect_scope','battle','战斗'),('effect_scope','user','玩家'),('effect_type','active','主动'),('effect_type','buff','Buff'),('effect_type','passive','被动'),('event','','无'),('event','event_add_friend','添加好友'),('event','event_dungeon_passed','通关副本'),('event','event_friend_add','添加好友'),('event','event_guild_join','加入公会'),('event','event_kill_monster','杀怪'),('event','event_level_upgrade','升级'),('event','event_shop_buy','商店购买'),('function','','无'),('function','check_quest','检查任务'),('function','start','开始'),('item_type','1','道具'),('item_type','10','资产'),('item_type','2','装备'),('item_type','3','身上'),('item_type','4','仓库'),('item_type','5','符文'),('item_type','6','寻宝'),('item_type','7','神兽'),('item_type','8','聚魂'),('item_type','9','饕餮'),('map_rank_key','','无'),('map_rank_key','camp','阵营'),('map_rank_key','guild','公会'),('map_rank_key','role','个人'),('map_rank_key','team','队伍'),('map_rank_mode','','不用排行'),('map_rank_mode','global','全局'),('map_rank_mode','local','不共享'),('map_rank_mode','share','共享'),('map_rank_value','','无'),('map_rank_value','hurt','伤害'),('map_type','full','全图'),('map_type','slice','九宫格'),('module','','无'),('module','auction_server','拍卖'),('module','boss_server','BOSS'),('module','dungeon_map','通用副本'),('module','friend','好友'),('module','role','角色'),('module','shop','商店'),('node_type_atom','center','跨服'),('node_type_atom','center_world','跨服和大世界'),('node_type_atom','local','本地'),('node_type_atom','local_center','本地和跨服'),('node_type_atom','local_center_world','全部'),('node_type_atom','local_world','本地和大世界'),('node_type_atom','world','大世界'),('node_type_integer','1','本地'),('node_type_integer','2','跨服'),('node_type_integer','3','本地和跨服'),('node_type_integer','4','大世界'),('node_type_integer','5','本地和大世界'),('node_type_integer','6','跨服和大世界'),('node_type_integer','7','全部'),('sex','0','无限制'),('sex','1','男'),('sex','2','女'),('skill_type','active','主动'),('skill_type','passive','被动'),('use_effect','','无'),('use_effect','coin','硬币'),('use_effect','copper','铜币'),('use_effect','exp','经验'),('use_effect','gold','金币'),('use_effect','silver','银币');
/*!40000 ALTER TABLE `validate_data` ENABLE KEYS */;
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

-- Dump completed on 2021-07-07 16:23:55

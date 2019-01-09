-- MySQL dump 10.13  Distrib 5.7.24, for Linux (x86_64)
--
-- Host: localhost    Database: game
-- ------------------------------------------------------
-- Server version	5.7.24-0ubuntu0.18.04.1

/*!40101 SET @OLD_CHARACTER_SET_CLIENT=@@CHARACTER_SET_CLIENT */;
/*!40101 SET @OLD_CHARACTER_SET_RESULTS=@@CHARACTER_SET_RESULTS */;
/*!40101 SET @OLD_COLLATION_CONNECTION=@@COLLATION_CONNECTION */;
/*!40101 SET NAMES utf8 */;
/*!40103 SET @OLD_TIME_ZONE=@@TIME_ZONE */;
/*!40103 SET TIME_ZONE='+00:00' */;
/*!40014 SET @OLD_UNIQUE_CHECKS=@@UNIQUE_CHECKS, UNIQUE_CHECKS=0 */;
/*!40014 SET @OLD_FOREIGN_KEY_CHECKS=@@FOREIGN_KEY_CHECKS, FOREIGN_KEY_CHECKS=0 */;
/*!40101 SET @OLD_SQL_MODE=@@SQL_MODE, SQL_MODE='NO_AUTO_VALUE_ON_ZERO' */;
/*!40111 SET @OLD_SQL_NOTES=@@SQL_NOTES, SQL_NOTES=0 */;

--
-- Table structure for table `assets`
--

DROP TABLE IF EXISTS `assets`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `assets` (
  `player_id` bigint(20) unsigned NOT NULL DEFAULT '0' COMMENT '玩家ID',
  `gold` bigint(20) unsigned NOT NULL DEFAULT '0' COMMENT '元宝',
  `silver` bigint(20) unsigned NOT NULL DEFAULT '0' COMMENT '银币',
  `copper` bigint(20) unsigned NOT NULL DEFAULT '0' COMMENT '铜币',
  `exp` bigint(20) unsigned NOT NULL DEFAULT '0' COMMENT '经验',
  PRIMARY KEY (`player_id`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci COMMENT='资产表';
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `assets`
--

LOCK TABLES `assets` WRITE;
/*!40000 ALTER TABLE `assets` DISABLE KEYS */;
INSERT INTO `assets` VALUES (1,10,100,1000,8974);
/*!40000 ALTER TABLE `assets` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `data_accost`
--

DROP TABLE IF EXISTS `data_accost`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `data_accost` (
  `num_id` smallint(5) unsigned NOT NULL DEFAULT '0' COMMENT '头像序列ID, 取值: 1-105',
  `type` tinyint(1) unsigned NOT NULL DEFAULT '0' COMMENT '类型, 取值: 1门客/ 2红颜/ 3NPC',
  `obj_id` int(11) unsigned NOT NULL DEFAULT '0' COMMENT '头像对象ID, 取值:NPC ID/ 门客ID/ 红颜ID',
  `day_of_week` tinyint(1) unsigned NOT NULL DEFAULT '0' COMMENT '周几, 取值: 1-7',
  `hour_start` tinyint(11) unsigned NOT NULL DEFAULT '0' COMMENT '出现小时',
  `hour_end` tinyint(11) unsigned NOT NULL DEFAULT '0' COMMENT '离开小时',
  `flag` tinyint(1) unsigned NOT NULL DEFAULT '0' COMMENT '特殊标识, 取值: 1拜访红颜, 2VIP红颜, 3郭富城/ 其他',
  `position` tinyint(1) unsigned NOT NULL DEFAULT '0' COMMENT '前端出现位置',
  PRIMARY KEY (`num_id`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COMMENT='搭讪配置表';
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `data_accost`
--

LOCK TABLES `data_accost` WRITE;
/*!40000 ALTER TABLE `data_accost` DISABLE KEYS */;
INSERT INTO `data_accost` VALUES (1,2,200001,1,9,10,0,1),(2,1,100001,1,9,10,0,2),(3,3,300010,1,9,10,0,3),(4,2,200046,1,12,13,2,4),(5,1,100046,1,12,13,0,5),(6,3,300013,1,12,13,0,6),(7,2,200013,1,15,16,1,7),(8,1,100013,1,15,16,0,8),(9,3,300007,1,15,16,0,9),(10,2,200005,1,18,19,1,1),(11,1,100005,1,18,19,0,2),(12,3,300007,1,18,19,0,3),(13,2,200012,1,21,22,1,4),(14,1,100012,1,21,22,0,5),(15,3,300006,1,21,22,0,6),(16,2,200003,2,9,10,1,7),(17,1,100003,2,9,10,0,8),(18,3,300003,2,9,10,0,9),(19,2,200019,2,12,13,0,1),(20,1,100041,2,12,13,0,2),(21,3,300013,2,12,13,0,3),(22,2,200020,2,15,16,1,4),(23,1,100020,2,15,16,0,5),(24,3,300012,2,15,16,0,6),(25,2,200001,2,18,19,0,7),(26,1,100001,2,18,19,0,8),(27,3,300012,2,18,19,0,9),(28,2,200048,2,21,22,2,1),(29,1,100048,2,21,22,0,2),(30,1,100050,2,21,22,3,3),(31,2,200002,3,9,10,0,4),(32,1,100021,3,9,10,0,5),(33,3,300005,3,9,10,0,6),(34,2,200016,3,12,13,0,7),(35,1,100016,3,12,13,0,8),(36,3,300001,3,12,13,0,9),(37,2,200047,3,15,16,2,1),(38,1,100047,3,15,16,0,2),(39,3,300002,3,15,16,0,3),(40,2,200042,3,18,19,0,4),(41,1,100042,3,18,19,0,5),(42,3,300003,3,18,19,0,6),(43,2,200011,3,21,22,1,7),(44,1,100011,3,21,22,0,8),(45,3,300003,3,21,22,0,9),(46,2,200016,4,9,10,0,1),(47,1,100022,4,9,10,0,2),(48,3,300001,4,9,10,0,3),(49,2,200052,4,12,13,0,4),(50,1,100010,4,12,13,0,5),(51,3,300005,4,12,13,0,6),(52,2,200009,4,15,16,1,7),(53,1,100009,4,15,16,0,8),(54,3,300006,4,15,16,0,9),(55,2,200045,4,18,19,2,1),(56,1,100045,4,18,19,0,2),(57,3,300005,4,18,19,0,3),(58,2,200006,4,21,22,1,4),(59,1,100006,4,21,22,0,5),(60,1,100050,4,21,22,3,6),(61,2,200018,5,9,10,0,7),(62,1,100018,5,9,10,0,8),(63,3,300001,5,9,10,0,9),(64,2,200004,5,12,13,1,1),(65,1,100004,5,12,13,0,2),(66,3,300007,5,12,13,0,3),(67,2,200017,5,15,16,0,4),(68,1,100017,5,15,16,0,5),(69,3,300005,5,15,16,0,6),(70,2,200014,5,18,19,1,7),(71,1,100014,5,18,19,0,8),(72,3,300006,5,18,19,0,9),(73,2,200007,5,21,22,1,1),(74,1,100007,5,21,22,0,2),(75,3,300013,5,21,22,0,3),(76,2,200001,6,9,10,0,4),(77,1,100001,6,9,10,0,5),(78,3,300010,6,9,10,0,6),(79,2,200018,6,12,13,0,7),(80,1,100051,6,12,13,0,8),(81,3,300005,6,12,13,0,9),(82,2,200008,6,15,16,1,1),(83,1,100008,6,15,16,0,2),(84,3,300007,6,15,16,0,3),(85,2,200044,6,18,19,2,4),(86,1,100044,6,18,19,0,5),(87,3,300007,6,18,19,0,6),(88,2,200019,6,21,22,0,7),(89,1,100019,6,21,22,0,8),(90,1,100050,6,21,22,3,9),(91,2,200042,7,9,10,0,1),(92,1,100049,7,9,10,0,2),(93,3,300003,7,9,10,0,3),(94,2,200017,7,12,13,0,4),(95,1,100033,7,12,13,0,5),(96,3,300010,7,12,13,0,6),(97,2,200002,7,15,16,0,7),(98,1,100002,7,15,16,0,8),(99,3,300005,7,15,16,0,9),(100,2,200015,7,18,19,1,1),(101,1,100015,7,18,19,0,2),(102,3,300002,7,18,19,0,3),(103,2,200043,7,21,22,2,4),(104,1,100043,7,21,22,0,5),(105,3,300013,7,21,22,0,6);
/*!40000 ALTER TABLE `data_accost` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `data_fashion`
--

DROP TABLE IF EXISTS `data_fashion`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `data_fashion` (
  `id` int(11) unsigned NOT NULL DEFAULT '0' COMMENT 'ID',
  `sex` tinyint(1) unsigned NOT NULL DEFAULT '0' COMMENT '(`data_sex`.`sex`,`data_sex`.`name`)',
  `style` tinyint(1) unsigned NOT NULL DEFAULT '0' COMMENT '样式',
  KEY `sex` (`sex`) USING BTREE,
  CONSTRAINT `data_fashion_ibfk_1` FOREIGN KEY (`sex`) REFERENCES `data_sex` (`sex`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COMMENT='时装配置表';
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `data_fashion`
--

LOCK TABLES `data_fashion` WRITE;
/*!40000 ALTER TABLE `data_fashion` DISABLE KEYS */;
INSERT INTO `data_fashion` VALUES (1,0,0),(2,1,0),(3,2,0);
/*!40000 ALTER TABLE `data_fashion` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `data_guild_param`
--

DROP TABLE IF EXISTS `data_guild_param`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `data_guild_param` (
  `type` varchar(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '类型',
  `param` varchar(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '子类型',
  `value` varchar(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '值',
  `name` varchar(255) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '名字'
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci COMMENT='公会参数';
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `data_guild_param`
--

LOCK TABLES `data_guild_param` WRITE;
/*!40000 ALTER TABLE `data_guild_param` DISABLE KEYS */;
INSERT INTO `data_guild_param` VALUES ('create','1','[{level, 10}, {vip, 0}, {gold, 0}]','一级'),('create','2','[{level, 10}, {vip, 1}, {gold, 100}]','二级');
/*!40000 ALTER TABLE `data_guild_param` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `data_item`
--

DROP TABLE IF EXISTS `data_item`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `data_item` (
  `data_id` int(11) NOT NULL DEFAULT '0' COMMENT '基础id',
  `name` varchar(100) NOT NULL DEFAULT '' COMMENT '名字(string)',
  `type` int(11) DEFAULT '0' COMMENT '类型',
  `bind` tinyint(1) DEFAULT '0' COMMENT '绑定',
  `overlap` int(11) DEFAULT '1' COMMENT '叠加数',
  PRIMARY KEY (`data_id`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COMMENT='物品配置表';
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `data_item`
--

LOCK TABLES `data_item` WRITE;
/*!40000 ALTER TABLE `data_item` DISABLE KEYS */;
INSERT INTO `data_item` VALUES (1,'金币',0,0,1),(2,'银币',0,0,1),(3,'铜币',0,0,1);
/*!40000 ALTER TABLE `data_item` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `data_key`
--

DROP TABLE IF EXISTS `data_key`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `data_key` (
  `key` varchar(24) NOT NULL DEFAULT '' COMMENT '码(string)',
  `type` tinyint(1) unsigned NOT NULL DEFAULT '0' COMMENT '类型',
  PRIMARY KEY (`key`) USING BTREE,
  KEY `key` (`key`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COMMENT='激活码配置';
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `data_key`
--

LOCK TABLES `data_key` WRITE;
/*!40000 ALTER TABLE `data_key` DISABLE KEYS */;
INSERT INTO `data_key` VALUES ('0jevljhlspujvmix',1),('0qaxzn7vhe3act4t',1),('17ybelfj4t47zu9r',1),('1juqzpc7gqni8fj4',1),('28clih28j7xdaxrb',1),('2itj8ufqbwcseshk',1),('3m3pi3pxpgizcpsi',1),('40dw9lm9hsaxa5ne',1),('4lxmclndcn7xkvlc',1),('4yqwalsilhft4eae',1),('5b6umib8syho0luc',1),('9e06bpvj6nrloe6z',1),('9ldoixwfqjskwcko',1),('9slf8q9ndhmudg4l',1),('aa6xrn4uwd5cmgki',1),('ahhytcxlfvvxtuyr',1),('alr3wosnp30lnygr',1),('apackapjez6nqykb',1),('avl2gmqrslf9aotm',1),('ax2pxqxmyhni5xgj',1),('bllblh0r8qolnhxr',1),('bohek7kz79j0qrly',1),('btwzf0loopcx4w65',1),('bxlcp8t8rv24kuos',1),('byvbmkw3bznfnzeg',1),('cbyei4awfzry6rxj',1),('cidpaobdhkztcl7x',1),('cjzv2iylvlmcldci',1),('ctofmrmiq4xlwslf',1),('czygzujulmua4ibv',1),('dgomv5jg6tmgyksg',1),('dhs8ayyj0vaa0lbf',1),('dyb7emtinghegy47',1),('e5pbm7mpldeey2ot',1),('ex6acn0rqlicszbz',1),('f4xhwzksv9bujqtd',1),('fnptzhcjh4xmq5oi',1),('fy5oa3hlawmpgylp',1),('fynhjqu2g9vjmlym',1),('glcbluer9rvephs9',1),('gqdcqrf5bavpimpr',1),('gxuyz4jzzgldsvuz',1),('hadulztd2jev4cxl',1),('hoq2gyywuyevbgfm',1),('i0ifghtrhuwlqe3v',1),('ilrt6fk7zk9msque',1),('jga8ryujp59ducku',1),('jgksb6xkp0byqdrf',1),('jhfowmav9xbjhnub',1),('jq9yfngm4mvce7gc',1),('jzl3ef2vab2pq4vh',1),('kdhuqsrweebtmppy',1),('kxn5bur3ubctiuby',1),('l52emsihlaxu7jd4',1),('laslmsn9zh1qxotg',1),('ldfsd67ymf1wdq24',1),('let5febxmiladlma',1),('lgpjzkfumt2d1zfs',1),('mbrgzrcldq3l81gc',1),('mlaqd4onmjtxtfd4',1),('mnmwi1uvyis9yw7x',1),('mo0lpwlddugp4uim',1),('mu87gjnmbzos2fge',1),('n31muoin5lvpr9wl',1),('nc7xgqfu8ewdvs1k',1),('ndum3lclmzcwut2r',1),('nlvloeergautsqgh',1),('nqhhshjzrljgf4sp',1),('nsfqn7shiqyzkwna',1),('nxhco5ipifyylw68',1),('nyieaagjnvzgzhxu',1),('olvmzikgz3phxfdb',1),('p44741cstjmiyvqz',1),('peyz1kjfcveykemi',1),('pitmqox5fzp97cx7',1),('plysqpxdcvl4hm7z',1),('pqokiljv3lgryz6f',1),('qiwnq0s7sm827li9',1),('qjdmb3p4xrweoe2f',1),('qp5a0ppmotmnurm8',1),('quswvga5lvepu9kk',1),('rvl1jeyxhu1koyx9',1),('skvti5nhl9ghhxva',1),('sl0dlrfmsh6adraq',1),('t2ynoiplapoqgq5i',1),('tzyt0jyv3lvluuhf',1),('uiazpvwj4ostzrt7',1),('ujmm0cafcmq9y64t',1),('uxk6sjtkmfywbymy',1),('vsekceqifuogkp2j',1),('vzqn4berg4whhraj',1),('w0yz6jyirkcuqxoo',1),('wtwhmf2da9ndfqmn',1),('xaaa3zvfonxiaeml',1),('xxlc0yfxdatitlwx',1),('y6cigowrtvuvygnf',1),('yisekafgyp4plymf',1),('zffjlbtlfmv1lnay',1),('zirz5r8ub37ubchi',1),('zoen9u9fw98xsqpl',1);
/*!40000 ALTER TABLE `data_key` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `data_key_award`
--

DROP TABLE IF EXISTS `data_key_award`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `data_key_award` (
  `type` int(255) unsigned NOT NULL DEFAULT '0' COMMENT '类型',
  `only` tinyint(1) unsigned NOT NULL DEFAULT '0' COMMENT '唯一',
  `award` varchar(200) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '奖励'
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci COMMENT='激活码奖励配置';
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `data_key_award`
--

LOCK TABLES `data_key_award` WRITE;
/*!40000 ALTER TABLE `data_key_award` DISABLE KEYS */;
INSERT INTO `data_key_award` VALUES (1,0,'[{700001,1},{700002,2},{700003,3}]'),(2,0,'[{700001,1},{700002,2},{700003,3}]');
/*!40000 ALTER TABLE `data_key_award` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `data_level`
--

DROP TABLE IF EXISTS `data_level`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `data_level` (
  `level` bigint(20) unsigned NOT NULL DEFAULT '0' COMMENT '等级',
  `exp` bigint(20) unsigned NOT NULL DEFAULT '0' COMMENT '经验'
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci COMMENT='等级配置表';
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `data_level`
--

LOCK TABLES `data_level` WRITE;
/*!40000 ALTER TABLE `data_level` DISABLE KEYS */;
INSERT INTO `data_level` VALUES (0,100),(1,200),(2,300),(3,400),(4,500),(5,600),(6,700),(7,800),(8,900),(9,1000);
/*!40000 ALTER TABLE `data_level` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `data_parameter`
--

DROP TABLE IF EXISTS `data_parameter`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `data_parameter` (
  `key` varchar(100) NOT NULL DEFAULT '' COMMENT '参数KEY',
  `name` varchar(200) NOT NULL DEFAULT '' COMMENT '参数名称(string)',
  `value` varchar(1000) NOT NULL DEFAULT '[]' COMMENT '参数VALUE',
  PRIMARY KEY (`key`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COMMENT='游戏参数配置表';
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `data_parameter`
--

LOCK TABLES `data_parameter` WRITE;
/*!40000 ALTER TABLE `data_parameter` DISABLE KEYS */;
INSERT INTO `data_parameter` VALUES ('activity_recharge_mail_content','充值活动邮件内容','<<\"这是您在充值活动中未领取的奖励\">>'),('activity_recharge_mail_title','充值活动邮件标题','<<\"充值活动奖励发放\">>'),('beauty_largess','红颜赏赐效果','[{13010006,1,0},{13010007,2,0},{13010002,0,1},{13010004,0,2}]'),('beauty_tease','红颜逗狗效果，[{GoodsId,Intimacy}]','[{13010001,1},{13010003,2}]'),('chat_expire_days','聊天消息过期天数','30'),('chat_num_limit','聊天消息数量限制','200'),('chat_world_timespan','世界聊天发言间隔','3'),('chuyou_add_seat_cost','红颜出游开启位置消耗钻石[{开启的位置下标,消耗元宝}...]','[{1, 300}, {2, 600}, {3, 1000}, {4, 2000}, {5, 3000}, {6, 4000}, {7, 5000}, {8, 5000}, {9, 5000}]'),('chuyou_duration','红颜出游持续时间,秒','10800'),('chuyou_max_batai_seat','获取出游吧台席位数量上限','4'),('chuyou_max_jiaban_seat','获取出游夹板席位数量上限','6'),('crusade_auto_limit','一键讨伐胜场数限制','50'),('crusade_guanka_limit','讨伐关卡限制','10801'),('feast_attend_limit','宴会赴宴每日次数限制','3'),('feast_exchange_limit','宴会兑换显示个数限制','9'),('feast_exchange_refresh_limit','宴会兑换刷新次数限制','10'),('feast_exchange_refresh_time','宴会兑换刷新时长','7200'),('feast_message_limit','宴会消息数量限制','50'),('feast_rank_limit','宴会榜数量限制','300'),('first_recharge_reward','首冲奖励','[100043,[{11010002,4},{12010001,10},{12010005,10}]]'),('fuben_boss_first_wave','副本BOSS','701'),('fuben_boss_open_time','副本boss开启时分','{20,0}'),('fuben_boss_over_time','副本boss结束时分','{21,0}'),('fuben_boss_stage_list','BOSS阶段奖励列表,[{阶段万分比值, [{概率万分比值,物品ID,物品数量}]},...]','[{8000,[{3333,13055001,1},{3333,13055002,1},{3334,13055003,1}]}, {6000,[{3333,13055001,1},{3333,13055002,1},{3334,13055003,1}]}, {4000,[{3333,13055001,1},{3333,13055002,1},{3334,13055003,1}]}, {2000,[{3333,13055001,1},{3333,13055002,1},{3334,13055003,1}]}]'),('fuben_clear_time','副本清理时分','{23,0}'),('fuben_drop_list_limit','副本掉落列表数量限制','50'),('fuben_first_wave','副本第一波小怪波数','101'),('fuben_hurt_rank_award','副本伤害排行奖励','[{1,1,150,150,60},{2,2,120,120,50},{3,3,100,100,40},{4,5,80,80,30},{6,10,70,70,25},{11,20,60,60,20},{21,50,50,50,15},{51,100,40,40,10}]'),('fuben_hurt_rank_limit','副本伤害排行数量限制','100'),('fuben_kill_rank_limit','副本击杀榜数量限制','100'),('fuben_open_time','副本小怪开启时分','{12,0}'),('fuben_over_time','副本小怪结束时分','{14,0}'),('guild_create_gold','创建商会消耗元宝','2000'),('guild_elite_merit_limit','商会精英历史贡献限制','1000'),('guild_join_timespan','加入商会冷却时长，单位秒','86400'),('guild_name_gold','商会改名消耗元宝','500'),('guild_rank_timespan','商会榜单刷新间隔，单位秒','3600'),('guild_vice_merit_limit','商会副盟主历史贡献限制','2000'),('heir_energy_cd_time','子嗣活力冷却时长，单位秒','10800'),('heir_loc','子嗣初始席位','2'),('heir_loc_gold','子嗣席位扩展元宝数','[{3,300},{4,600},{5,1000},{6,2000},{7,3000},{8,4000},{9,5000},{10,5000}]'),('heir_loc_limit','子嗣席位限制','10'),('heir_num_limit','子嗣数量限制','9999'),('heir_pet_id_ratio','宠物出生形象随机概率列表，[{万分比值,父亲1/母亲2}]','[{0, 1}, {10000, 2}]'),('heir_quality_ratio','子嗣出生资质概率，[{最小亲密,最大亲密,概率权重,资质},...]','[{1,20,70,1},{1,20,30,2},{21,100,50,1},{21,100,30,2},{21,100,20,3},{101,200,30,1},{101,200,30,2},{101,200,20,3},{101,200,20,4},{201,500,40,2},{201,500,30,3},{201,500,20,4},{201,500,10,5},{501,9999999999,30,2},{501,9999999999,30,3},{501,9999999999,20,4},{501,9999999999,20,5}]'),('heir_rand_icon_list','子嗣图标随机列表','[{1, [2001,2002,2003,2004]},{2, [2101,2102,2103,2104]}]'),('heir_ratio','子嗣出生概率','[{1,10},{2,80}]'),('heir_train_exp','子嗣培养一次增加经验','10'),('lifetime_card_award','终生卡首次购买奖励[MenkeID, [{GoodsID, GoodsNum}, ...]]','[0, []]'),('lifetime_card_miracle_count','终生卡附加神迹次数','0'),('luxury_one','奢饰品单次抽奖钻石','56'),('luxury_ten','奢饰品10次抽奖钻石','560'),('mail_attachment_limit','邮件附件数量限制','10'),('mail_expire_days','邮件过期天数','30'),('marriage_tiqin_expire_timespan','提亲过期时长，单位秒','259200'),('marriage_zhaoqin_num_limit','招亲刷新数量限制','5'),('marriage_zhaoqin_refresh_gold','招亲刷新元宝','100'),('marriage_zhaoqin_refresh_timespan','招亲刷新时长，单位秒','3600'),('menke_car_quality_limit','汽车同品质数量限制[{品质,数量},...]','[{6,4},{5,4},{4,4},{3,10},{2,10},{1,10}]'),('menke_expatriate_add_seat_cost','外派增加位置消耗元宝数[{当前位置数,消耗元宝}...]','[{1, 300}, {2, 600}, {3, 1000}, {4, 2000}, {5, 3000}, {6, 4000}, {7, 5000}, {8, 5000}, {9, 5000}]'),('menke_expatriate_max_seat','外派最大地点数','10'),('menke_expatriate_menke_limit','外派开启需要员工数量','30'),('menke_expatriate_valid_day','外派有效期,天','100'),('month_card_award','月卡首次购买奖励[MenkeID, [{GoodsID, GoodsNum}, ...]]','[0, []]'),('month_card_miracle_count','月卡附加神迹次数','0'),('penalty_box_award','工作应酬宝箱奖励物品ID','11010007'),('penalty_message_num','工作应酬消息限制条数','200'),('penalty_rank_cd_time','工作应酬排名更新冷却时长，秒','3600'),('penalty_rank_num','工作应酬排名显示条数','10'),('prison_prisoner_limit','囚犯数量上限','10'),('prison_punish_take_hp','惩罚囚犯血量消耗','1'),('rank_mobai_award','排行榜膜拜奖励','[{60,10},{30,20},{10,50}]'),('sign_award_cycle','签到奖励周期','21'),('study_add_seat','书院增加位置消耗元宝数[{当前位置数,消耗元宝}...]','[{1,300},{2,600},{3,1000},{4,2000},{5,3000},{6,4000},{7,5000},{8,5000},{9,5000}]'),('study_max_seat','书院最大位置数','10'),('trade_auto_limit','一键通商单日胜场数限制','50'),('trade_job_limit','通商官品限制','10'),('visit_limit','寻访限制,{官品,Vip}','{3,2}'),('visit_recover_cd','寻访自动恢复cd,秒,{体力恢复,运势恢复}','{3600,900}'),('visit_recover_num','寻访自动恢复数值,{体力恢复,运势恢复}','{1,1}'),('yamen_attribute_first','衙门论战第一轮临时属性','[63,64,65]'),('yamen_battle_limit','衙门论战常规出使次数限制','4'),('yamen_defend_message_limit','衙门论战防守消息数量限制','30'),('yamen_foe_kill_limit','衙门论战仇人击杀数限制','5'),('yamen_foe_list_limit','衙门论战仇人列表数量限制','30'),('yamen_global_kill_limit','衙门论战上榜消息击杀数限制','20'),('yamen_global_message_limit','衙门论战上榜消息数量限制','200'),('yamen_menke_lv_limit','衙门论战门客等级限制','60'),('year_card_award','年卡奖励,元宝','100'),('yuehui_accept_limit','约会每日可接受次数','3'),('yuehui_act_cd','约会行动力自动恢复CD，秒','10800'),('yuehui_act_cd_gold','约会行动力恢复消耗元宝','10'),('yuehui_auto_limit','一键约会VIP限制','3'),('yuehui_job_limit','约会官品限制','2'),('yuehui_num_ratio_list','约会短信条数概率列表，[{Num,Ratio}]','[{2,500},{3,200}]'),('yuehui_timespan','约会时间段区间值，秒','900'),('zhengwu_cd_time','政务CD时长,秒','1800'),('zhengwu_finish_award','政务完成奖励列表，[{权重,物品ID,物品数量},...]','[{5000,12025017,1},{5000,13016001,1}]');
/*!40000 ALTER TABLE `data_parameter` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `data_sex`
--

DROP TABLE IF EXISTS `data_sex`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `data_sex` (
  `sex` tinyint(1) unsigned NOT NULL DEFAULT '0' COMMENT '性别',
  `name` varchar(100) NOT NULL DEFAULT '' COMMENT '性别',
  PRIMARY KEY (`sex`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COMMENT='性别配置表';
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `data_sex`
--

LOCK TABLES `data_sex` WRITE;
/*!40000 ALTER TABLE `data_sex` DISABLE KEYS */;
INSERT INTO `data_sex` VALUES (0,'无限制'),(1,'男性'),(2,'女性');
/*!40000 ALTER TABLE `data_sex` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `data_vip`
--

DROP TABLE IF EXISTS `data_vip`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `data_vip` (
  `vip` tinyint(1) unsigned NOT NULL DEFAULT '0' COMMENT 'VIP等级',
  `job_limit` tinyint(1) unsigned NOT NULL DEFAULT '0' COMMENT '官品限制',
  `money` int(11) unsigned NOT NULL DEFAULT '0' COMMENT '充值金额',
  `menke_id` int(11) unsigned NOT NULL DEFAULT '0' COMMENT '奖励门客ID',
  `beauty_id` int(11) unsigned NOT NULL DEFAULT '0' COMMENT '奖励红颜ID',
  `goods_list` varchar(200) NOT NULL DEFAULT '[]' COMMENT '奖励物品列表，[{物品ID,物品数量},...]',
  PRIMARY KEY (`vip`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COMMENT='VIP配置表';
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `data_vip`
--

LOCK TABLES `data_vip` WRITE;
/*!40000 ALTER TABLE `data_vip` DISABLE KEYS */;
INSERT INTO `data_vip` VALUES (1,1,30,0,0,'[]'),(2,1,100,0,200044,'[{11010002,4},{13010003,10},{13010004,10},{13015001,10}]'),(3,1,256,100044,0,'[{11010002,4},{13025002,1},{13060001,5},{13060002,5}]'),(4,1,580,0,200043,'[{11010002,4},{13016001,1},{13016002,2},{13016003,3}]'),(5,1,1000,100045,0,'[{11010002,4},{13055001,1},{13055002,1},{13055003,1}]'),(6,1,4000,0,200046,'[{11010002,4},{13055004,1},{13055005,1},{13055006,1}]'),(7,1,10000,100046,0,'[{11010002,4},{13055007,1},{13055008,1},{13055009,1}]'),(8,1,30000,0,200045,'[{11010002,4},{13055010,1},{13055011,1},{13055012,1}]'),(9,1,100000,100047,200047,'[{11010002,4},{13055013,1},{13055014,1},{13055015,1}]'),(10,1,200000,100048,200048,'[{11010002,4},{13055016,1},{13055017,1},{13055018,1}]'),(11,1,400000,0,0,'[]');
/*!40000 ALTER TABLE `data_vip` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `fashion`
--

DROP TABLE IF EXISTS `fashion`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `fashion` (
  `player_id` int(11) unsigned NOT NULL DEFAULT '0' COMMENT '玩家id(select)',
  `fashion_id` int(11) unsigned NOT NULL DEFAULT '0' COMMENT '时装id',
  `state` tinyint(1) unsigned NOT NULL DEFAULT '0' COMMENT '时装状态(update_state)(update_time)',
  `score` int(11) unsigned NOT NULL DEFAULT '0' COMMENT '积分(once)',
  `point` int(11) NOT NULL DEFAULT '0' COMMENT '积分(update_point)',
  `expire_time` int(11) unsigned NOT NULL DEFAULT '0' COMMENT '过期时间(update_time)',
  `list` varchar(0) NOT NULL DEFAULT '' COMMENT '列表',
  `string` varchar(0) DEFAULT NULL COMMENT 'string(ignore)',
  `extra` char(0) DEFAULT NULL COMMENT 'extra(ignore)',
  PRIMARY KEY (`player_id`,`fashion_id`) USING BTREE,
  KEY `fashion_id` (`fashion_id`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COMMENT='时装表';
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `fashion`
--

LOCK TABLES `fashion` WRITE;
/*!40000 ALTER TABLE `fashion` DISABLE KEYS */;
/*!40000 ALTER TABLE `fashion` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `guild`
--

DROP TABLE IF EXISTS `guild`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `guild` (
  `guild_id` bigint(20) unsigned NOT NULL AUTO_INCREMENT COMMENT '公会id',
  `guild_name` char(16) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '名字',
  `create_time` int(11) unsigned NOT NULL DEFAULT '0' COMMENT '时间',
  `exp` bigint(20) unsigned NOT NULL DEFAULT '0' COMMENT '经验',
  `wealth` bigint(20) unsigned NOT NULL DEFAULT '0' COMMENT '财富',
  `notice` char(100) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '公告(update_notice)',
  `leader_id` char(0) COLLATE utf8mb4_unicode_ci DEFAULT NULL COMMENT '会长id(ignore)',
  `leader_name` char(0) COLLATE utf8mb4_unicode_ci DEFAULT '' COMMENT '会长名字(ignore)',
  `extra` char(0) COLLATE utf8mb4_unicode_ci DEFAULT NULL COMMENT '额外(ignore)(save_flag)',
  PRIMARY KEY (`guild_id`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci COMMENT='公会表';
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `guild`
--

LOCK TABLES `guild` WRITE;
/*!40000 ALTER TABLE `guild` DISABLE KEYS */;
/*!40000 ALTER TABLE `guild` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `guild_player`
--

DROP TABLE IF EXISTS `guild_player`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `guild_player` (
  `guild_id` int(20) unsigned NOT NULL DEFAULT '0' COMMENT '公会id(`guild`.`id`)',
  `player_id` int(20) unsigned NOT NULL DEFAULT '0' COMMENT '玩家id(`player`.`id`)',
  `job` tinyint(1) unsigned NOT NULL DEFAULT '0' COMMENT '职位',
  `join_time` int(11) unsigned NOT NULL DEFAULT '0' COMMENT '加入时间',
  `leave_time` int(11) unsigned NOT NULL DEFAULT '0' COMMENT '离开时间',
  `guild_name` char(0) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '帮派名(ignore)(`guild`.`name`)',
  `player_name` char(0) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '玩家名(ignore)(`player`.`name`)',
  `player_nick` char(0) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '玩家昵称(ignore)(`player`.`nick`)',
  `extra` char(0) COLLATE utf8mb4_unicode_ci DEFAULT NULL COMMENT '额外(ignore)(save_flag)',
  PRIMARY KEY (`guild_id`,`player_id`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci COMMENT='公会玩家表';
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `guild_player`
--

LOCK TABLES `guild_player` WRITE;
/*!40000 ALTER TABLE `guild_player` DISABLE KEYS */;
/*!40000 ALTER TABLE `guild_player` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `guild_status`
--

DROP TABLE IF EXISTS `guild_status`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `guild_status` (
  `guild` tinyint(3) unsigned NOT NULL DEFAULT '0' COMMENT '公会',
  `player` tinyint(3) unsigned NOT NULL DEFAULT '0' COMMENT '玩家',
  `apply` tinyint(3) unsigned NOT NULL DEFAULT '0' COMMENT '申请'
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci COMMENT='公会状态';
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `guild_status`
--

LOCK TABLES `guild_status` WRITE;
/*!40000 ALTER TABLE `guild_status` DISABLE KEYS */;
/*!40000 ALTER TABLE `guild_status` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `item`
--

DROP TABLE IF EXISTS `item`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `item` (
  `id` int(20) NOT NULL AUTO_INCREMENT COMMENT 'id',
  `user_id` int(20) NOT NULL DEFAULT '0' COMMENT '玩家id(select)(once)',
  `data_id` int(20) NOT NULL DEFAULT '0' COMMENT '基础id(once)',
  `amount` int(20) NOT NULL DEFAULT '0' COMMENT '数量',
  `extra` char(0) DEFAULT NULL COMMENT '额外(ignore)(save_flag)',
  PRIMARY KEY (`id`) USING BTREE,
  KEY `user_id` (`user_id`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COMMENT='物品表';
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `item`
--

LOCK TABLES `item` WRITE;
/*!40000 ALTER TABLE `item` DISABLE KEYS */;
/*!40000 ALTER TABLE `item` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `key`
--

DROP TABLE IF EXISTS `key`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `key` (
  `player_id` bigint(20) unsigned NOT NULL DEFAULT '0' COMMENT 'ID(select)',
  `key` varchar(24) NOT NULL DEFAULT '' COMMENT '码',
  `extra` varchar(0) NOT NULL DEFAULT '' COMMENT '额外(ignore)(save_flag)',
  PRIMARY KEY (`player_id`,`key`) USING BTREE,
  KEY `key` (`key`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COMMENT='激活码';
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `key`
--

LOCK TABLES `key` WRITE;
/*!40000 ALTER TABLE `key` DISABLE KEYS */;
/*!40000 ALTER TABLE `key` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `log_player`
--

DROP TABLE IF EXISTS `log_player`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `log_player` (
  `id` int(11) unsigned NOT NULL AUTO_INCREMENT,
  `user_id` int(11) unsigned DEFAULT NULL,
  `exp` int(11) unsigned DEFAULT NULL,
  `time` int(11) unsigned DEFAULT NULL,
  PRIMARY KEY (`id`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COMMENT='玩家日志表';
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `log_player`
--

LOCK TABLES `log_player` WRITE;
/*!40000 ALTER TABLE `log_player` DISABLE KEYS */;
/*!40000 ALTER TABLE `log_player` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `log_rank`
--

DROP TABLE IF EXISTS `log_rank`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `log_rank` (
  `id` int(11) unsigned NOT NULL AUTO_INCREMENT COMMENT '编号',
  `rank_type` varchar(100) NOT NULL DEFAULT '' COMMENT '排行榜类型',
  `role_id` int(11) unsigned NOT NULL DEFAULT '0' COMMENT '玩家ID',
  `role_name` varchar(100) NOT NULL DEFAULT '' COMMENT '玩家名称',
  `pos` int(11) unsigned NOT NULL DEFAULT '0' COMMENT '排名',
  `count_num` bigint(20) unsigned NOT NULL DEFAULT '0' COMMENT '计数',
  `count_time` int(11) unsigned NOT NULL DEFAULT '0' COMMENT '数据更新时间',
  `time` int(11) unsigned NOT NULL DEFAULT '0' COMMENT '操作时间',
  PRIMARY KEY (`id`) USING BTREE,
  KEY `rank_type` (`rank_type`) USING BTREE,
  KEY `role_id` (`role_id`) USING BTREE,
  KEY `time` (`time`) USING BTREE
) ENGINE=InnoDB AUTO_INCREMENT=44707 DEFAULT CHARSET=utf8 COMMENT='玩家排行榜日志';
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `log_rank`
--

LOCK TABLES `log_rank` WRITE;
/*!40000 ALTER TABLE `log_rank` DISABLE KEYS */;
/*!40000 ALTER TABLE `log_rank` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `online`
--

DROP TABLE IF EXISTS `online`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `online` (
  `id` bigint(20) unsigned NOT NULL DEFAULT '0' COMMENT 'id',
  `name` char(100) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '用户名',
  `nick` char(100) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '昵称',
  `pid` int(10) unsigned NOT NULL COMMENT '玩家进程pid',
  `pid_sender` int(10) unsigned NOT NULL COMMENT '玩家发送进程pid',
  PRIMARY KEY (`id`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci COMMENT='玩家在线信息';
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `online`
--

LOCK TABLES `online` WRITE;
/*!40000 ALTER TABLE `online` DISABLE KEYS */;
/*!40000 ALTER TABLE `online` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `player`
--

DROP TABLE IF EXISTS `player`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `player` (
  `id` bigint(1) unsigned NOT NULL AUTO_INCREMENT COMMENT 'ID',
  `name` varchar(16) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '用户名(once)(update_name)',
  `nick` char(16) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '昵称(once)(update_nick)',
  `sex` tinyint(1) NOT NULL DEFAULT '0' COMMENT '性别',
  `level` int(11) unsigned NOT NULL DEFAULT '0' COMMENT '等级',
  `classes` tinyint(1) unsigned NOT NULL DEFAULT '0' COMMENT '职业',
  `focus` varchar(255) NOT NULL DEFAULT '' COMMENT '关注(convert)',
  `extra` varchar(0) DEFAULT NULL COMMENT '额外(ignore)',
  PRIMARY KEY (`id`) USING BTREE,
  UNIQUE KEY `role_name` (`name`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COMMENT='玩家信息表';
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `player`
--

LOCK TABLES `player` WRITE;
/*!40000 ALTER TABLE `player` DISABLE KEYS */;
/*!40000 ALTER TABLE `player` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `rank`
--

DROP TABLE IF EXISTS `rank`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `rank` (
  `type` tinyint(2) unsigned NOT NULL COMMENT '类型',
  `key` bigint(20) unsigned NOT NULL COMMENT '键',
  `value` bigint(20) unsigned NOT NULL COMMENT '值',
  `time` bigint(20) unsigned NOT NULL COMMENT '时间',
  `rank` bigint(20) unsigned NOT NULL COMMENT '排名',
  `extra` varchar(0) NOT NULL DEFAULT '' COMMENT '额外(ignore)(0)',
  PRIMARY KEY (`type`,`key`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COMMENT='排行';
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `rank`
--

LOCK TABLES `rank` WRITE;
/*!40000 ALTER TABLE `rank` DISABLE KEYS */;
/*!40000 ALTER TABLE `rank` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `user`
--

DROP TABLE IF EXISTS `user`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `user` (
  `player` tinyint(1) NOT NULL DEFAULT '0' COMMENT '玩家表',
  `assets` varchar(0) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '资产表',
  `item` varchar(0) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '物品表',
  `bag` varchar(0) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '装备背包',
  `quest` varchar(0) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '任务表',
  `mail` varchar(0) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '邮件表',
  `friend` varchar(0) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '好友表',
  `shop` varchar(0) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '商店表',
  `vip` varchar(0) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT 'vip表',
  `id` tinyint(1) NOT NULL DEFAULT '0' COMMENT 'id(ignore)',
  `name` char(0) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '用户名',
  `nick` char(0) COLLATE utf8mb4_unicode_ci NOT NULL DEFAULT '' COMMENT '昵称',
  `pid` tinyint(1) NOT NULL DEFAULT '0' COMMENT '玩家进程pid(ignore)',
  `pid_sender` tinyint(1) NOT NULL DEFAULT '0' COMMENT '玩家发送进程pid(ignore)',
  `socket` tinyint(1) NOT NULL DEFAULT '0' COMMENT '套接字(ignore)',
  `online_time` tinyint(1) NOT NULL DEFAULT '0' COMMENT '在线时间(ignore)',
  `tick` tinyint(1) NOT NULL DEFAULT '0' COMMENT '保存时间(ignore)',
  `timeout` tinyint(1) NOT NULL DEFAULT '0' COMMENT '超时时间(ignore)'
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci COMMENT='玩家数据';
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `user`
--

LOCK TABLES `user` WRITE;
/*!40000 ALTER TABLE `user` DISABLE KEYS */;
/*!40000 ALTER TABLE `user` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `vip`
--

DROP TABLE IF EXISTS `vip`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `vip` (
  `player_id` bigint(20) unsigned NOT NULL DEFAULT '0' COMMENT '玩家id',
  `vip` tinyint(2) unsigned NOT NULL DEFAULT '0' COMMENT 'vip等级',
  `expire_time` int(11) unsigned NOT NULL DEFAULT '0' COMMENT '过期时间',
  PRIMARY KEY (`player_id`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COMMENT='玩家vip表';
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `vip`
--

LOCK TABLES `vip` WRITE;
/*!40000 ALTER TABLE `vip` DISABLE KEYS */;
/*!40000 ALTER TABLE `vip` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `words`
--

DROP TABLE IF EXISTS `words`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `words` (
  `word` varchar(100) CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci DEFAULT '' COMMENT '敏感词'
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COMMENT='敏感词';
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `words`
--

LOCK TABLES `words` WRITE;
/*!40000 ALTER TABLE `words` DISABLE KEYS */;
INSERT INTO `words` VALUES ('【】'),('毛泽东'),('周恩来'),('刘少奇'),('朱德'),('彭德怀'),('林彪'),('刘伯承'),('陈毅'),('贺龙'),('聂荣臻'),('徐向前'),('罗荣桓'),('叶剑英'),('李大钊'),('陈独秀'),('孙中山'),('孙文'),('孙逸仙'),('邓小平'),('陈云'),('江泽民'),('李鹏'),('朱镕基'),('李瑞环'),('尉健行'),('李岚清'),('胡锦涛'),('罗干'),('温家宝'),('吴邦国'),('曾庆红'),('贾庆林'),('黄菊'),('吴官正'),('李长春'),('吴仪'),('回良玉'),('曾培炎'),('周永康'),('曹刚川'),('唐家璇'),('华建敏'),('陈至立'),('陈良宇'),('张德江'),('张立昌'),('俞正声'),('王乐泉'),('刘云山'),('王刚'),('王兆国'),('刘淇'),('贺国强'),('郭伯雄'),('胡耀邦'),('王乐泉'),('王兆国'),('周永康'),('李登辉'),('连战'),('陈水扁'),('宋楚瑜'),('吕秀莲'),('郁慕明'),('蒋介石'),('蒋中正'),('蒋经国'),('马英九'),('吴帮国'),('无帮国'),('无邦国'),('无帮过'),('瘟家宝'),('假庆林'),('甲庆林'),('假青林'),('离长春'),('习远平'),('袭近平'),('李磕墙'),('贺过墙'),('和锅枪'),('粥永康'),('轴永康'),('肘永康'),('周健康'),('粥健康'),('周小康'),('姜异康'),('孙政才'),('令计划'),('陆昊'),('范长龙'),('许其亮'),('习近平'),('马凯'),('王岐山'),('王沪宁'),('刘云山'),('刘延东'),('刘奇葆'),('许其亮'),('孙春兰'),('孙政才'),('李克强'),('李建国'),('李源潮'),('汪洋'),('张春贤'),('张高丽'),('张德江'),('孟建柱'),('赵乐际'),('胡春华'),('俞正声'),('栗战书'),('郭金龙'),('韩正'),('刘奇葆'),('赵乐际'),('栗战书'),('杜青林'),('赵洪祝'),('杨晶'),('胡锦涛'),(''),(''),(''),(''),(''),(''),(''),(''),(''),(''),(''),(''),(''),(''),(''),(''),('        刘延东'),('李源潮'),('汪洋'),('张高丽'),('张德江'),('俞正声'),('徐才厚'),('郭伯雄'),('李鹏'),('万里'),('乔石'),('朱镕基'),('李瑞环'),('宋平'),('尉健行'),('李岚清'),('曾庆红'),('吴官正'),('罗干'),('范长龙'),('许其亮'),('何勇'),('令计划'),('王沪宁'),('于广洲'),('马凯'),('马飚'),('马兴瑞'),('马晓天'),('王君'),('王侠'),('王珉'),('王勇'),('王晨'),('王毅'),('王三运'),('王万宾'),('王玉普'),('王正伟'),('王东明'),('王光亚'),('王伟光'),('王安顺'),('王志刚'),('王岐山'),('王沪宁'),('王国生'),('王学军'),('王建平'),('王胜俊'),('王洪尧'),('王宪魁'),('王冠中'),('王家瑞'),('王教成'),('王新宪'),('王儒林'),('支树平'),('尤权'),('车俊'),('尹蔚民'),('巴音朝鲁'),('巴特尔'),('卢展工'),('叶小文'),('田中'),('田修思'),('白玛赤林'),('白春礼'),('令计划'),('吉炳轩'),('朱小丹'),('朱福熙'),('全哲洙'),('刘鹏'),('刘源'),('刘鹤'),('刘云山'),('刘亚洲'),('刘成军'),('刘伟平'),('刘延东'),('刘奇葆'),('刘晓江'),('刘家义'),('刘粤军'),('刘福连'),('许达哲'),('许其亮'),('许耀元'),('孙怀山'),('孙建国'),('孙春兰'),('孙政才'),('孙思敬'),('苏树林'),('杜青林'),('杜金才'),('杜恒岩'),('李伟'),('李斌'),('李从军'),('李东生'),('李立国'),('李纪恒'),('李克强'),('李学勇'),('李建华'),('李建国'),('李鸿忠'),('李源潮'),('杨晶'),('杨传堂'),('杨金山'),('杨栋梁'),('杨洁篪'),('杨焕宁'),('肖钢'),('肖捷'),('吴昌德'),('吴胜利'),('吴爱英'),('吴新雄'),('何毅亭'),('冷溶'),('汪洋'),('汪永清'),('沈跃跃'),('沈德咏'),('宋大涵'),('宋秀岩'),('张阳'),('张茅'),('张毅'),('张又侠'),('张仕波'),('张庆伟'),('张庆黎'),('张志军'),('张国清'),('张宝顺'),('张春贤'),('张高丽'),('张海阳'),('张裔炯'),('张德江'),('陆昊'),('陈希'),('陈雷'),('陈全国'),('陈求发'),('陈宝生'),('陈政高'),('陈敏尔'),('努尔·白克力'),('苗圩'),('林军'),('林左鸣'),('尚福林'),('罗志军'),('罗保铭'),('周济'),('周强'),('周本顺'),('周生贤'),('郑卫平'),('房峰辉'),('孟学农'),('孟建柱'),('项俊波'),('赵实'),('赵正永'),('赵乐际'),('赵克石'),('赵克志'),('赵宗岐'),('赵洪祝'),('胡泽君'),('胡春华'),('俞正声'),('姜大明'),('姜异康'),('骆惠宁'),('秦光荣'),('袁纯清'),('袁贵仁'),('耿惠昌'),('聂卫国'),('栗战书'),('贾廷安'),('夏宝龙'),('铁凝'),('徐守盛'),('徐绍史'),('徐粉林'),('高虎城'),('郭声琨'),('郭金龙'),('郭庚茂'),('郭树清'),('黄兴国'),('黄奇帆'),('黄树贤'),('曹建明'),('戚建国'),('常万全'),('鹿心社'),('彭勇'),('彭清华'),('蒋定之'),('蒋建国'),('蒋洁敏'),('韩正'),('韩长赋'),('焦焕成'),('谢伏瞻'),('强卫'),('楼继伟'),('解振华'),('褚益民'),('蔡武'),('蔡名照'),('蔡英挺'),('蔡赴朝'),('雒树刚'),('魏亮'),('魏凤和'),('于春生'),('马勇霞'),('王伟'),('王炜'),('王长河'),('王东峰'),('王立英'),('王仲田'),('王华庆'),('王会生'),('王岐山'),('王怀臣'),('王忠民'),('王和民'),('王宜林'),('王晓龙'),('王家胜'),('王宾宜'),('王森泰'),('王瑞生'),('丹珠昂奔'),('尹晋华'),('石生龙'),('叶青纯'),('申维辰'),('付建华'),('冯惠敏'),('宁高宁'),('弘强'),('曲青山'),('曲淑辉'),('吕建成'),('任泽民'),('多杰热旦'),('刘滨'),('刘长银'),('刘生杰'),('刘向松'),('刘金国'),('刘建华'),('　刘晓滨'),('刘赐贵'),('江必新'),('安立敏'),('苏波'),('杜金才'),('杜金富'),('李宁'),('李刚'),('李熙'),('李五四'),('李书磊'),('李玉赋'),('李兆前'),('李法泉'),('李建波'),('李适时'),('李秋芳'),('李家祥'),('杨立顺'),('杨志今'),('杨明生'),('杨晓渡'),('肖亚庆'),('吴刚'),('吴玉良'),('吴杰明'),('岑旭'),('邱学强'),('何平'),('余欣荣'),('辛维光　汪民'),('宋明昌'),('宋爱荣'),('宋璇涛'),('张力'),('张军'),('张勇'),('张立军'),('张纪南'),('张昌平'),('张晓兰'),('　张晓刚'),('陈伦'),('陈大卫'),('陈文清'),('陈训秋'),('陈建民'),('陈绪国'),('陈新权'),('苗华'),('金书波'),('周英'),('周泽民'),('周福启'),('郑国光'),('赵洪祝'),('胡玉敏'),('胡问鸣'),('侯凯'),('侯长安'),('侯贺华'),('俞贵麟'),('姚增科'),('袁彦鹏'),('耿文清'),('耿燎原'),('柴绍良'),('徐敬业'),('郭永平'),('郭向远'),('黄先耀'),('黄建国'),('黄建盛'),('黄树贤'),('黄晓薇'),('黄殿中'),('曹培玺'),('崔少鹏'),('梁滨'),('董力'),('韩亨林'),('谢杭生'),('谢国明'),('　强卫东'),('臧献甫'),('熊维平'),('黎晓宏'),('布什'),('布莱尔'),('小泉'),('纯一郎'),('萨马兰奇'),('安南'),('阿拉法特'),('普京'),('默克尔'),('克林顿'),('里根'),('尼克松'),('林肯'),('杜鲁门'),('赫鲁晓夫'),('列宁'),('斯大林'),('马克思'),('恩格斯'),('金正日'),('金日成'),('萨达姆'),('胡志明'),('西哈努克'),('希拉克'),('撒切尔'),('阿罗约'),('曼德拉'),('卡斯特罗'),('富兰克林'),('华盛顿'),('艾森豪威尔'),('拿破仑'),('亚历山大'),('路易'),('鲍威尔'),('奥巴马'),('本拉登'),('奥马尔'),('柴玲'),('达赖喇嘛'),('江青'),('张春桥'),('姚文元'),('王洪文'),('东条英机'),('希特勒'),('墨索里尼'),('冈村秀树'),('冈村宁次'),('高丽朴'),('赵紫阳'),('王丹'),('沃尔开西'),('李洪志'),('李大师'),('赖昌星'),('马加爵'),('班禅'),('额尔德尼'),('山本五十六'),('阿扁'),('阿扁万岁'),('热那亚'),('热比娅'),('六四'),('六四运动'),('美国之音'),('密宗'),('民国'),('民进党'),('民运'),('民主'),('民主潮'),('摩门教'),('纳粹'),('南华早报'),('南蛮'),('明慧网'),('起义'),('亲民党'),('瘸腿帮'),('人民报'),('法轮功'),('　✪☸龘☎☂♞☪'),('打倒共产党'),('台独万岁'),('圣战'),('示威'),('台独'),('台独分子'),('台联'),('台湾民国'),('台湾岛国'),('台湾国'),('台湾独立'),('太子党'),('天安门事件'),('屠杀'),('小泉'),('新党'),('新疆独立'),('新疆分裂'),('新疆国'),('疆独'),('西藏独立'),('西藏分裂'),('西藏国'),('藏独'),('藏青会'),('藏妇会'),('学潮'),('学运'),('一党专政'),('一中一台'),('两个中国'),('一贯道'),('游行'),('造反'),('真善忍'),('镇压'),('政变'),('政治'),('政治反对派'),('政治犯'),('中共'),('共产党'),('反党'),('反共'),('政府'),('民主党'),('中国之春'),('转法轮'),('自焚'),('共党'),('共匪'),('苏家屯'),('基地组织'),('塔利班'),('东亚病夫'),('支那'),('高治联'),('高自联'),('专政'),('专制'),('世界维吾尔大会'),('核工业基地'),('核武器'),('铀'),('原子弹'),('氢弹'),('导弹'),('核潜艇'),('大参考'),('小参考'),('国内动态清样'),('多维'),('河殇'),('摩门教'),('穆罕默德'),('圣战'),('耶和华'),('耶稣'),('伊斯兰'),('真主安拉'),('白莲教'),('天主教'),('基督教'),('东正教'),('占中'),('占领中环'),('八一'),('七宗罪'),('中国大法'),('法轮'),('法轮功'),('瘸腿帮'),('真理教'),('真善忍'),('转法轮'),('自焚'),('走向圆满'),('黄大仙'),('跳大神'),('神汉'),('神婆'),('真理教'),('大卫教'),('阎王'),('黑白无常'),('牛头马面'),('藏独'),('高丽棒子'),('疆独'),('蒙古鞑子'),('台独'),('台独分子'),('台联'),('台湾民国'),('西藏独立'),('新疆独立'),('南蛮'),('老毛子'),('回民吃猪肉'),('谋杀'),('杀人'),('吸毒'),('贩毒'),('赌博'),('拐卖'),('走私'),('卖淫'),('造反'),('强奸'),('轮奸'),('抢劫'),('先奸后杀'),('下注'),('抽头'),('坐庄'),('赌马'),('赌球'),('筹码'),('老虎机'),('轮盘赌'),('安非他命'),('大麻'),('可卡因'),('海洛因'),('冰毒'),('摇头丸'),('杜冷丁'),('鸦片'),('罂粟'),('迷幻药'),('白粉'),('嗑药'),('吸毒'),('AIDS'),('aids'),('Aids'),('DICK'),('dick'),('Dick'),('penis'),('sex'),('SM'),('屙'),('爱滋'),('淋病'),('梅毒'),('爱液'),('屄'),('逼'),('臭机八'),('臭鸡巴'),('吹喇叭'),('吹箫'),('催情药'),('屌'),('肛交'),('肛门'),('龟头'),('黄色'),('机八'),('机巴'),('鸡八'),('鸡巴'),('机掰'),('机巴'),('鸡叭'),('鸡鸡'),('鸡掰'),('鸡奸'),('妓女'),('奸'),('茎'),('精液'),('精子'),('尻'),('口交'),('滥交'),('乱交'),('轮奸'),('卖淫'),('屁眼'),('嫖娼'),('强奸'),('强奸犯'),('情色'),('肉棒'),('乳房'),('乳峰'),('乳交'),('乳头'),('乳晕'),('三陪'),('色情'),('射精'),('手淫'),('威而钢'),('威而柔'),('伟哥'),('性高潮'),('性交'),('性虐'),('性欲'),('穴'),('颜射'),('阳物'),('一夜情'),('阴部'),('阴唇'),('阴道'),('阴蒂'),('阴核'),('阴户'),('阴茎'),('阴门'),('淫'),('淫秽'),('淫乱'),('淫水'),('淫娃'),('淫液'),('淫汁'),('淫穴'),('淫洞'),('援交妹'),('做爱'),('梦遗'),('阳痿'),('早泄'),('奸淫'),('性欲'),('性交'),('Bitch'),('bt'),('cao'),('FUCK'),('Fuck'),('fuck'),('kao'),('NMD'),('NND'),('sb'),('shit'),('SHIT'),('SUCK'),('Suck'),('tmd'),('TMD'),('tnnd'),('K他命'),('白痴'),('笨蛋'),('屄'),('变态'),('婊子'),('操她妈'),('操妳妈'),('操你'),('操你妈'),('操他妈'),('草你'),('肏'),('册那'),('侧那'),('测拿'),('插'),('蠢猪'),('荡妇'),('发骚'),('废物'),('干她妈'),('干妳'),('干妳娘'),('干你'),('干你妈'),('干你妈B'),('干你妈b'),('干你妈逼'),('干你娘'),('干他妈'),('狗娘养的'),('滚'),('鸡奸'),('贱货'),('贱人'),('烂人'),('老母'),('老土'),('妈比'),('妈的'),('马的'),('妳老母的'),('妳娘的'),('你妈逼'),('破鞋'),('仆街'),('去她妈'),('去妳的'),('去妳妈'),('去你的'),('去你妈'),('去死'),('去他妈'),('日'),('日你'),('赛她娘'),('赛妳娘'),('赛你娘'),('赛他娘'),('骚货'),('傻B'),('傻比'),('傻子'),('上妳'),('上你'),('神经病'),('屎'),('屎妳娘'),('屎你娘'),('他妈的'),('王八蛋'),('我操'),('我日'),('乡巴佬'),('猪猡'),('屙'),('干'),('尿'),('掯'),('屌'),('操'),('骑你'),('湿了'),('操你'),('操他'),('操她'),('骑你'),('骑他'),('骑她'),('欠骑'),('欠人骑'),('来爽我'),('来插我'),('干你'),('干他'),('干她'),('干死'),('干爆'),('干机'),('FUCK'),('机叭'),('臭鸡'),('臭机'),('烂鸟'),('览叫'),('阳具'),('肉棒'),('肉壶'),('奶子'),('摸咪咪'),('干鸡'),('干入'),('小穴'),('强奸'),('插你'),('插你'),('爽你'),('爽你'),('干干'),('干X'),('我操'),('他干'),('干它'),('干牠'),('干您'),('干汝'),('干林'),('操林'),('干尼'),('操尼'),('我咧干'),('干勒'),('干我'),('干到'),('干啦'),('干爽'),('欠干'),('狗干'),('我干'),('来干'),('轮干'),('轮流干'),('干一干'),('援交'),('骑你'),('我操'),('轮奸'),('鸡奸'),('奸暴'),('再奸'),('我奸'),('奸你'),('奸你'),('奸他'),('奸她'),('奸一奸'),('淫水'),('淫湿'),('鸡歪'),('仆街'),('臭西'),('尻'),('遗精'),('烂逼'),('大血比'),('叼你妈'),('靠你妈'),('草你'),('干你'),('日你'),('插你'),('奸你'),('戳你'),('逼你老母'),('挨球'),('我日你'),('草拟妈'),('卖逼'),('狗操卖逼'),('奸淫'),('日死'),('奶子'),('阴茎'),('奶娘'),('他娘'),('她娘'),('骚B'),('你妈了妹'),('逼毛'),('插你妈'),('叼你'),('渣波波'),('嫩b'),('weelaa'),('缔顺'),('帝顺'),('蒂顺'),('系统消息'),('午夜'),('看下'),('草泥马'),('法克鱿'),('雅蠛蝶'),('潜烈蟹'),('菊花蚕'),('尾申鲸'),('吉跋猫'),('搞栗棒'),('吟稻雁'),('达菲鸡'),('SM'),('ML'),('3P'),('群P'),('马勒戈壁'),('双飞'),('fuck'),('共产党'),('urban'),('我操'),('cao'),('他妈的'),('TMD'),('鸡巴'),('煞笔'),('傻B'),('法轮功'),('江泽民'),('胡锦涛'),('温家宝'),('urban-rivals'),('rivals'),('我日'),('UR'),('ur'),('性交'),('口交'),('婊子'),('妓女'),(''),('她妈'),('牛逼'),('牛B'),('牛比'),('煞笔'),('傻逼'),('傻B'),('操你妈'),('装逼'),('装B'),('日你妈'),('不玩了'),('删号'),('卖号'),('删号'),('妈的'),('妈逼'),('草你妈'),('T.M.D'),('JB'),('jb'),(''),('出售账号'),('出售此号'),('卖号'),('U/R'),('U-R'),('j8'),('吗的'),('8仙'),('狗日'),('出售神符'),('色情'),('黄色'),('藏独'),('台独'),('法轮大法'),('鸡巴'),('毛泽东'),('NPC'),('*法*轮*功*'),('*李*洪*志*阿扁'),('阿扁万岁'),('阿拉'),('阿拉法特'),('挨球'),('安南'),('安全局'),('澳洲光明网'),('八九'),('八九风波'),('办理文凭'),('办理证件'),('包皮'),('保钓'),('保监会'),('保密局'),('鸨'),('鲍岳桥'),('暴动'),('暴乱'),('暴徒'),('北京之春'),('贝肉'),('本拉登'),('本?拉登'),('苯比'),('笨屄'),('笨逼'),('屄'),('屄毛'),('逼毛'),('逼你老母'),('逼样'),('比毛'),('婊'),('婊子'),('宾周'),('冰毒'),('波霸'),('博讯'),('薄一波'),('布莱尔'),('布雷尔'),('布什'),('布什'),('财政部'),('参事室'),('藏独'),('藏独'),('藏独'),('操'),('操GM'),('操Gm'),('操gM'),('操gm'),('操XX'),('操逼'),('操比'),('操蛋'),('操你'),('操你八辈祖宗'),('操你妈'),('操你妈屄'),('操他'),('曹刚川'),('草的你妈'),('草妈'),('草你妈'),('草拟妈'),('肏'),('测绘局'),('插GM'),('插Gm'),('插gM'),('插gm'),('插妳'),('插你'),('插你妈'),('插深些'),('产权局'),('朝鲜'),('车臣'),('车仑'),('陈功'),('陈良宇'),('陈水扁'),('陈希同'),('陈晓宁'),('陈毅'),('陈至立'),('成人电影'),('成人片'),('吃大便'),('吃屎'),('迟浩田'),('赤匪'),('抽插'),('抽你丫的'),('臭化西'),('出售假币'),('出售枪支'),('出售手枪'),('吹喇叭'),('吹箫'),('春药'),('蠢猪'),('戳你'),('粗制吗啡'),('催情药'),('达赖'),('达赖喇嘛'),('打炮'),(''),('大B'),('大逼'),('大便'),('大波波'),('大麻'),('大麻树脂'),('大麻油'),('大师'),('戴维教'),('大学骚乱'),('大血B'),('大血比'),('呆卵'),('戴海静'),('戴红'),('戴晶'),('戴维教'),('党主席'),('荡妇'),('档案局'),('盗窃犯'),('盗窃犯'),('道教'),('邓小平'),('帝国主义'),('电监会'),('叼你'),('叼你妈'),('屌'),('屌7'),('屌鸠'),('屌毛'),('屌妳'),('屌七'),('屌西'),('钓鱼台'),('丁关根'),('丁子霖'),('东北独立'),('东升'),('东条英机'),('东突'),('东突暴动和独立'),('东突组织'),('东亚病夫'),('董建华'),('董贱华'),('董文华'),('懂文华'),('独立'),('独立台湾会'),('恩格斯'),('二B'),('二屄'),('二逼'),('二乙基酰胺发抡'),('发抡功'),('发伦'),('发伦功'),('发轮'),('发论'),('发论公'),('发论功'),('发骚'),('法(轮)功'),('法*轮*功'),('法功'),('法愣'),('法仑'),('法轮'),('法轮大法'),('法轮功'),('法西斯'),('法制办'),('反动'),('反革命'),('反共'),('反华'),('反恐委员会'),('反日'),('反政府'),('分裂祖国'),('佛教'),('佛展千手法'),('佛祖'),('斧头镰刀'),('阝月'),('傅鹏'),('傅作义'),('干GM'),('干Gm'),('干gM'),('干gm'),('干拎娘'),('干妳'),('干妳老母'),('干妳妈'),('干妳娘'),('干你'),('干你妈'),('干你妈b'),('干你妈逼'),('干你娘'),('干七八'),('干死你'),('肛'),('肛交'),('肛门'),('港澳办'),('高俊'),('高丽棒子'),('高校暴乱'),('高校群体事件'),('高校骚乱'),('睾'),('睾丸'),('膏药旗'),('弓虽女干'),('公安'),('公安部'),('公安局'),('共产党'),('共产主义'),('共匪'),('共狗'),('狗b'),('狗操'),('狗卵'),('狗娘'),('狗屁'),('狗日'),('狗日的'),('狗屎'),('观世音'),('官逼民反'),('官商勾结'),('龟儿子'),('龟公'),('龟孙子'),('龟头'),('鬼村'),('滚'),('郭伯雄'),('国安局'),('国防部'),('国防科工委'),('国管局'),('国际法院'),('国家民委'),('国家主席'),('国家主要部委'),('国民党'),('国民党万岁'),('海洛因'),('海洋局'),('何候华'),('贺国强'),('贺龙'),('黑社会'),('黑手党'),('黑手党'),('黑手党'),('红卫兵'),('洪兴'),('洪志'),('后庭'),('胡XX'),('胡紧涛'),('胡紧掏'),('胡紧套'),('胡锦涛'),('胡锦淘'),('胡乔木'),('胡耀邦'),('胡主席'),('花柳'),('华国锋'),('华建敏'),('换妻'),('黄　菊'),('黄菊'),('黄色电影'),('黄色小电影'),('回教'),('回良玉'),('回民暴动'),('回族人吃猪肉'),('昏药'),('火棒'),('机八'),('机巴'),('鸡八'),('鸡巴'),('鸡叭'),('鸡芭'),('鸡掰'),('鸡奸'),('基地组织'),('基督'),('基督教'),('激情电影'),('激情小电影'),('鸡'),('计牌软件'),('计生委'),('妓'),('妓女'),('妓院'),('贾庆林'),('奸'),('奸夫淫妇'),('奸你'),('奸淫'),('贱'),('贱逼'),('贱货'),('贱人'),('江Core'),('江八'),('江八点'),('江独裁'),('江核心'),('江青'),('江戏子'),('江择民'),('江泽民'),('江贼民'),('江折民'),('江猪'),('江猪媳'),('江主席'),('僵贼民'),('疆独'),('蒋介石'),('蒋经国'),('蒋中正'),('酱猪媳'),('交通部'),('姣西'),('叫床'),('叫鸡'),('叫小姐'),('教育部'),('她妈的金日成'),('金正日'),('禁书'),('经济社会理事会'),('经社理事会'),('精液'),('精子'),('警匪一家'),('敬国神社'),('靖国神社'),('静坐'),('纠察员'),('鸠'),('鸠屎'),('军长发威'),('军国主义'),('军妓'),('尻'),('靠'),('靠你妈'),('靠腰'),('可待因'),('可卡叶'),('可卡因'),('克林顿'),('恐怖份子'),('恐怖主义'),('口交'),('寇晓伟'),('狂操'),('狂操你全家'),('拉登'),('拉姆斯菲尔德'),('懒教'),('烂B'),('烂屄'),('烂逼'),('烂比'),('烂屌'),('烂货'),('劳+教+所'),('劳动保障部'),('老逼'),('老毛子'),('老母'),('黎阳评'),('李长春'),('李登辉'),('李弘旨'),('李红志'),('李宏旨'),('李宏志'),('李洪志'),('李岚清'),('李鹏'),('李鹏*'),('李瑞环'),('李山'),('李铁映'),('李先念'),('连战'),('联大'),('联合国'),('联合国大会'),('联易'),('联易互动'),('粮食局'),('两腿之间'),('列宁'),('林彪'),('林业局'),('刘　淇'),('刘军'),('刘淇'),('刘少奇'),('刘云山'),('流氓'),('六.四'),('六。四'),('六?四'),('六合彩'),('六四'),('六-四'),('六四事件'),('六四真相'),('龙新民'),('吕秀莲'),('旅游局'),('卵'),('轮功'),('轮奸'),('罗　干'),('罗干'),('骡干'),('妈逼'),('妈比'),('妈卖妈屁'),('妈批'),('妈祖'),('妈B'),('妈的'),('麻醉钢枪'),('麻醉枪'),('麻醉药'),('麻醉乙醚'),('马克思'),('马卖马屁'),('马英九'),('吗啡'),('吗啡碱'),('吗啡片'),('买财富'),('买卖枪支'),('麦角酸'),('卖.国'),('卖B'),('卖ID'),('卖QQ'),('卖逼'),('卖比'),('卖财富'),('卖党求荣'),('卖国'),('卖国求荣'),('卖号'),('卖卡'),('卖软件'),('卖淫'),('毛XX'),('毛厕洞'),('毛一鲜'),('毛泽东'),('毛贼东'),('毛主席'),('梅花网'),('美国'),('美国佬'),('美国之音'),('美利坚'),('蒙尘药'),('蒙独'),('蒙古达子'),('蒙古独立'),('迷魂药'),('迷奸药'),('迷歼药'),('迷药'),('密洞'),('密宗'),('民航局'),('民进党'),('民运'),('民政部'),('明慧网'),('摩门教'),('莫索里尼'),('穆罕默德'),('穆斯林'),('乳头'),('奶子'),('妳老母的'),('妳妈的'),('妳马的'),('妳娘的'),('南联盟'),('南蛮子'),('南蛮子'),('嫩B'),('嫩b'),('伱妈'),('你爸'),('你大爷'),('你二大爷'),('你老母'),('你老味'),('你姥'),('你姥姥的'),('你妈'),('你妈逼'),('你妈的'),('你娘'),('你爷爷的'),('鸟GM'),('鸟Gm'),('鸟gM'),('鸟gm'),('鸟你'),('牛逼'),('牛比'),('农业部'),('虐待'),('拍肩神药'),('喷你'),('彭真'),('皮条'),('屁眼'),('嫖客'),('苹果日报'),('破坏'),('破鞋'),('仆街'),('普京'),('气象局'),('钱其琛'),('枪决女犯'),('枪决现场'),('枪支弹药'),('强奸'),('强奸犯'),('强卫'),('强效失意药'),('强硬发言'),('抢劫'),('乔石'),('侨办'),('切七'),('窃听器'),('窃听器材'),('亲民党'),('青天白日'),('情色'),('去你妈的'),('去死'),('全国人大'),('瘸腿帮'),('人大'),('人大代表'),('人代会'),('ssb'),('人民'),('人民大会堂'),('人民广场'),('人民日报'),('人民银行'),('人体炸弹'),('日GM'),('日Gm'),('日gM'),('日gm'),('日X妈'),('日本RING'),('日本鬼子'),('日你'),('日你妈'),('日你娘'),('日他娘'),('肉棒'),('肉壁'),('肉洞'),('肉缝'),('肉棍'),('肉棍子'),('肉穴'),('乳'),('乳波臀浪'),('乳房'),('乳交'),('乳头'),('撒尿'),('萨达姆'),('塞白'),('塞你爸'),('塞你公'),('塞你老母'),('塞你老师'),('塞你母'),('塞你娘'),('三个呆婊'),('三个代婊'),('三级片'),('三民主义'),('三陪'),('三陪女'),('三去车仑'),('三唑仑'),('骚'),('骚B'),('骚逼'),('骚货'),('骚'),('色情'),('色情电影'),('色情服务'),('色情小电影'),('杀人犯'),('傻B'),('傻屄'),('傻逼'),('傻比'),('傻吊'),('傻卵'),('傻子'),('煞逼'),('商务部'),('上妳'),('上你'),('社科院'),('射精'),('身份生成器'),('神经病'),('神通加持法'),('生鸦片'),('圣女峰'),('十八摸'),('十年动乱石进'),('食捻屎'),('食屎'),('驶你爸'),('驶你公'),('驶你老母'),('驶你老师'),('驶你母'),('驶你娘'),('是鸡'),('手淫'),('受虐狂'),('售ID'),('售号'),('售软件'),('双峰微颤'),('氵去'),('水利部'),('水去车仑'),('税务总局'),('司法部'),('私服'),('私/服'),('私\\\\服'),('私服'),('私-服'),('私—服'),('斯大林'),('死gd'),('死GD'),('死gm'),('死GM'),('死全家'),('四川独立'),('四人帮'),('宋楚瑜'),('宋祖英'),('孙文'),('孙逸仙'),('孙中山'),('他爹'),('他妈'),('他妈的'),('他马的'),('他母亲'),('他祖宗'),('台办'),('台独'),('台联'),('台湾党'),('台湾帝国'),('台湾独立'),('台湾共产党'),('台湾共和国'),('台湾狗'),('台湾国'),('台湾民国'),('太监'),('太子党'),('唐家璇'),('天皇陛下'),('田纪云'),('舔西'),('投毒杀人'),('透视软件'),('推油'),('外　挂'),('外挂'),('外/挂'),('外\\\\挂'),('外_挂'),('外挂'),('外-挂'),('外—挂'),('外汇局'),('外交部'),('外专局'),('晚年周恩来'),('万税'),('王八蛋'),('王宝森'),('王刚'),('王昊'),('王乐泉'),('王岐山'),('王太华'),('王兆国'),('王震'),('网管'),('威而钢'),('威而柔'),('卫生部'),('尉健行'),('温加宝'),('温家宝'),('温家保'),('温馨'),('温总理'),('文化部'),('文物局'),('倭国'),('倭寇'),('我操'),('我操你'),('我干'),('我妳老爸'),('我日'),('我日你'),('无界浏览器'),('吴　仪'),('吴邦国'),('吴官正'),('吴仪'),('五星红旗'),('西藏独立'),('西藏天葬'),('希拉克'),('希特勒'),('希望之声'),('洗脑班'),('系统'),('系统公告'),('系统讯息'),('鲜族'),('乡巴佬'),('想上你'),('小鸡鸡'),('小泉'),('小泉纯一郎'),('小日本'),('小肉粒'),('小乳头'),('小穴'),('邪教'),('新疆独立'),('兴奋剂'),('性爱'),('性交'),('性虐待'),('性无能'),('性欲'),('徐光春'),('学潮'),('血逼'),('血腥图片'),('鸦片'),('鸦片液'),('鸦片渣'),('烟草局'),('严方军'),('阳精'),('阳具'),('摇头丸'),('摇头玩'),('耶和华'),('耶苏'),('耶稣'),('叶剑英'),('夜情'),('一党专制'),('一贯道'),('一国两制'),('一夜情'),('一中一台'),('伊拉克'),('伊朗'),('伊斯兰'),('以茎至洞'),('抑制剂'),('阴部'),('阴唇'),('阴道'),('阴蒂'),('阴核'),('阴户'),('阴茎'),('阴毛'),('阴水'),('阴小撕大'),('淫'),('淫荡'),('淫秽'),('淫货'),('淫贱'),('淫叫'),('淫毛'),('淫靡'),('淫水'),('淫娃'),('淫语连连'),('淫欲'),('英雄纪念碑'),('硬挺'),('邮政局'),('游戏发奖员'),('游戏宫理员'),('游戏管理员'),('游行'),('俞正声'),('舆论钳制'),('玉杵'),('欲火焚身'),('原子能机构'),('援交'),('远程偷拍'),('曰GM'),('曰Gm'),('曰gM'),('曰gm'),('曰你'),('月经'),('月经不调'),('月经'),('扎卡维是英雄'),('杂种'),('造反'),('曾培炎'),('曾庆红'),('扎卡维'),('张朝阳'),('张潮阳'),('张德江'),('张磊'),('张立昌'),('张小平'),('赵紫阳'),('侦探设备'),('真理教'),('中国恐怖组织'),('中华民国'),('中南海'),('中宣部'),('周恩来'),('周永康'),('周总理'),('朱德'),('朱容鸡'),('朱容基'),('朱熔基'),('朱镕基'),('朱总理'),('猪操'),('猪容基'),('主席'),('转法轮'),('转法轮'),('装屄'),('装逼'),('追查国际'),('子女任职名单'),('自焚'),('自杀手册'),('自杀指南'),('自制手枪'),('自治机关'),('宗教'),('总局'),('总理'),('作爱'),('坐台的'),('做爱'),('共产党'),('江泽民'),('胡锦涛'),('温家宝'),('严方军'),('屄毛'),('操逼毛'),('东突'),('骚货'),('婊子'),('妓女'),('他妈'),('她妈'),('牛逼'),('牛比'),('牛B'),('煞笔'),('傻逼'),('傻B'),('你妈'),('操你妈'),('装逼'),('装B'),('日你妈'),('不玩了'),('删号'),('卖号'),('垃圾游戏'),('烂游戏'),('删号'),('妈的'),('妈逼'),('草你妈'),('T.M.D'),('JB'),('jb'),('淘宝'),('出售账号'),('出售此号'),('卖号'),('U/R'),('U-R'),('cao'),('j8'),('吗的'),('8仙'),('狗日'),('出售神符'),('色情'),('黄色'),('h站'),('龙虎'),('虎门'),('龙虎门'),('WEB牌戰'),('WEB战牌'),('战牌'),('8仙'),('ＵＲ'),('ur'),('UR'),('街头对抗'),('藏独'),('台独'),('法轮大法'),('混沌决'),('ur'),('UR'),('urban'),('鸡巴'),('坐台的'),('作爱'),('总理'),('宗教'),('自治机关'),('自制手枪'),('你大爷'),('你二大爷'),('你老母'),('你老味'),('你姥'),('你姥姥的'),('你妈'),('你妈逼'),('你妈的'),('你娘'),('你爷爷的'),('鸟GM'),('鸟Gm'),('鸟gM'),('鸟gm'),('鸟你'),('牛逼'),('牛比'),('农业部'),('虐待'),('拍肩神药'),('喷你'),('彭真'),('皮条'),('屁眼'),('嫖客'),('苹果日报'),('破坏'),('破鞋'),('仆街'),('普京'),('气象局'),('钱其琛'),('枪决女犯'),('枪决现场'),('枪支弹药'),('强奸'),('强奸犯'),('强卫'),('强效失意药'),('强硬发言'),('抢劫'),('乔石'),('侨办'),('切七'),('窃听器'),('窃听器材'),('亲民党'),('青天白日'),('情色'),('去你妈的'),('去死'),('全国人大'),('瘸腿帮'),('人大'),('人大代表'),('人代会'),('人弹'),('人民'),('人民大会堂'),('人民广场'),('人民日报'),('人民银行'),('人体炸弹'),('日GM'),('日Gm'),('日gM'),('日gm'),('日X妈'),('日本RING'),('日本鬼子'),('日你'),('日你妈'),('日你娘'),('日他娘'),('肉棒'),('肉壁'),('肉洞'),('肉缝'),('肉棍'),('肉棍子'),('肉穴'),('乳'),('乳波臀浪'),('乳房'),('乳交'),('乳头'),('撒尿'),('萨达姆'),('塞白'),('塞你爸'),('塞你公'),('塞你老母'),('塞你老师'),('塞你母'),('塞你娘'),('三个呆婊'),('三个代婊'),('三级片'),('三民主义'),('三陪'),('三陪女'),('三去车仑'),('三唑仑'),('骚'),('骚B'),('骚逼'),('骚货'),('骚'),('色情'),('色情电影'),('色情服务'),('色情小电影'),('杀人犯'),('傻B'),('傻屄'),('傻逼'),('傻比'),('傻吊'),('傻卵'),('傻子'),('煞逼'),('商务部'),('上妳'),('上你'),('社.会.正.义.力.量'),('社保基金会'),('社会主义'),('社科院'),('射精'),('身份生成器'),('神经病'),('神通加持法'),('审计署'),('升达毕业证'),('生春袋'),('生孩子没屁眼'),('生鸦片'),('圣女峰'),('湿透的内裤'),('十八摸'),('十年动乱'),('十五周年'),('石进'),('食捻屎'),('食屎'),('驶你爸'),('驶你公'),('驶你老母'),('驶你老师'),('驶你母'),('驶你娘'),('世界日报'),('是鸡'),('手机复制'),('手淫'),('受虐狂'),('售ID'),('售号'),('售软件'),('双峰微颤'),('氵去'),('水利部'),('水去车仑'),('税务总局'),('司法部'),('私服'),('私/服'),('私\\\\服'),('私服'),('私-服'),('私—服'),('斯大林'),('死gd'),('死GD'),('死gm'),('死GM'),('死全家'),('四川独立'),('四人帮'),('宋楚瑜'),('宋祖英'),('孙文'),('孙逸仙'),('孙中山'),('他爹'),('他妈'),('他妈的'),('他马的'),('他母亲'),('他祖宗'),('台办'),('台独'),('台联'),('台湾党'),('台湾帝国'),('台湾独立'),('台湾共产党'),('台湾共和国'),('台湾狗'),('台湾国'),('台湾民国'),('太监'),('太子党'),('唐家璇'),('特别公告'),('特码'),('体育总局'),('天安门'),('天安门档案'),('天安门录像带'),('天安门事件'),('天安门屠杀'),('天安门一代'),('天鹅之旅'),('天皇'),('天皇陛下'),('田纪云'),('舔西'),('铁道部'),('统计局'),('曾庆红'),('扎卡维'),('扎卡维是英雄'),('张朝阳'),('张潮阳'),('张德江'),('张磊'),('张立昌'),('张小平'),('赵紫阳'),('侦探设备'),('真理教'),('真善忍'),('镇压'),('正见网'),('正清网'),('正悟网'),('证监会'),('政变'),('政协'),('值勤'),('值勤账号'),('指导员'),('质检局'),('致幻剂'),('中共'),('中共中央'),('中国'),('中国共产党'),('中国恐怖组织'),('中华民国'),('中华人民共和国'),('中科院'),('中南海'),('中宣部'),('中央'),('中央电视台'),('中央政治局'),('中医药局'),('周恩来'),('周永康'),('周总理'),('朱德'),('朱容鸡'),('朱容基'),('朱熔基'),('朱镕基'),('朱总理'),('猪操'),('猪容基'),('主席'),('转法轮'),('转法轮'),('装屄'),('装逼'),('追查国际'),('子女任职名单'),('自焚'),('自杀手册'),('自杀指南'),('自由亚洲电台'),('自由之门'),('自制手枪'),('自治机关'),('@sshole'),('a$$hole'),('a$shole'),('Admin'),('as$hole'),('ASS'),('asshole'),('bastard'),('bbscity'),('beijingspring'),('bignews'),('bitch'),('Bitch'),('bjzc'),('boxun'),('bt'),('butthead'),('butthole'),('cctv'),('CCTV'),('cdjp'),('chengmingmag'),('chinesenewsweek'),('cunt'),('dajiyuan'),('damm'),('damn'),('dick'),('Dick'),('DICK'),('epochtimes'),('F.L.G'),('falun'),('fawanghuihui'),('fgmtv'),('flg'),('FLG'),('fofg'),('fosaon'),('fu('),('fuc'),('Fuck'),('fuck'),('FUCK'),('FUCKYOU'),('fuckyou'),('fuk'),('fv('),('fvc'),('gamemaster'),('GAMEMASTER'),('gameMASTER'),('GAMEmaster'),('ＧＡＭＥ　ｍａｓｔｅｒ'),('ｇａｍｅ　ＭＡＳＴＥＲ'),('ＧＡＭＥ　ＭＡＳＴＥＲ'),('ｇａｍｅ　ｍａｓｔｅｒ'),('GameMaste'),('GameMaster'),('GAMEMASTER'),('gc365'),('globalrescue'),('Gm'),('gM'),('gm'),('minghui'),('mingpaonews'),('minhui'),('NMD'),('NND'),('nnd'),('on9'),('ON9'),('orgasmus'),('peacehall'),('penis'),('phuc'),('piss'),('PUSSY'),('pussy'),('renminbao'),('ri'),('SB'),('sb'),('screw'),('secretchina'),('sega'),('sex'),('sf'),('sh!t'),('shengda'),('SHIT'),('shit'),('shyt'),('SM'),('snatch'),('soundofhope'),('SUCK'),('suck'),('Suck'),('TMD'),('tmd'),('TNND'),('tnnd'),('WG'),('wg'),('WG'),('Wg'),('wG'),('wg'),('xinguangming'),('xinsheng'),('yuanming'),('zhengjian'),('zhengqing'),('zhengwunet'),('zhongguohun'),('pps'),('PPS'),('爱滋'),('小穴'),('法轮功'),('共产党'),('政治'),('卡耐基'),('李总理'),('李总统'),('论公'),('论功'),('论攻'),('論公'),('論功'),('論攻'),('涛哥'),('万亦凡'),('青春妇'),('护士'),('黄色江'),('江泽民'),('毛泽东'),('裸聊'),('淫水'),('内衣秀'),('公安局'),('派出所'),('黄色小说'),('陈水扁'),('马的阳具'),('阳萎'),('野鸡'),('阴部'),('阴唇'),('阴道'),('阴茎'),('阴毛'),('淫'),('淫荡'),('淫妇'),('淫秽'),('淫水'),('淫西'),('月经'),('杂种'),('招妓'),('祖宗'),('做爱'),('回民吃猪肉'),('台独'),('台湾独立'),('疆独'),('藏独'),('周恩来'),('邓小平'),('习近平'),('温家宝'),('胡锦涛'),('强奸'),('手枪'),('自杀'),('共产党'),('海洛因'),('枪支'),('入党'),('国家'),('炸药'),('独裁'),('狗日的'),('胡主席'),('反共'),('鸡巴'),('迷幻药'),('贱人'),('恐怖组织'),('犯罪'),('枪毙'),('民权'),('特码'),('四人帮'),('子弹'),('周恩来'),('周总理'),('穆斯林'),('连战'),('马英九'),('赌博'),('骚货'),('贱货'),('开枪'),('腐败'),('总书记'),('氯胺酮'),('雷管'),('引爆'),('盗窃'),('刑法'),('死刑'),('摇头丸'),('K粉'),('妖术'),('鸦片'),('违法'),('八路军'),('非法'),('政府'),('公安局'),('派出所'),('间谍'),('导弹'),('邪教'),('推翻'),('李长春'),('黄色小说'),('台独'),('藏独'),('疆独'),('朱镕基'),('新疆独立'),('西藏分裂'),('造反'),('政变'),('色情'),('嫖娼'),('卖淫'),('淫秽'),('游行'),('自焚'),('垮台'),('镇压'),('东突'),('共匪'),('天安门'),('淫荡'),('梵蒂冈'),('颠覆'),('人权'),('治安'),('做爱'),('报仇'),('侮辱'),('出卖'),('诋毁'),('性病'),('原子弹'),('成人电影'),('成人书刊'),('成人图片'),('城管'),('暴力执法'),('中共'),('冰毒'),('催情'),('春药'),('达赖'),('喇嘛'),('打砸抢'),('钓鱼岛'),('赌球'),('独立'),('反人类'),('反中共'),('二奶'),('反封锁'),('你妈的B'),('我操你妈'),('干你妈'),('妈逼的'),('暴乱'),('睾丸'),('狗逼'),('机密文件'),('鸡奸'),('李洪志'),('李宏志'),('假钞'),('假钱'),('激情电影'),('双乳'),('禁书'),('精液'),('市委会'),('伊斯兰'),('内衣'),('淫水'),('遥控炸弹'),('抵制'),('裸体'),('贩毒'),('强暴'),('邪恶'),('脱光'),('脱衣服'),('A片'),('屠杀'),('证件'),('佛主'),('六合彩'),('6合彩'),('录像'),('操你全家'),('不得好死'),('你妈B的'),('成人网站'),('暴政'),('伪造'),('绝密档案'),('熟女'),('中国'),('爱滋病'),('成人电影'),('三级片'),('政府'),('天安門'),('臺灣獨立'),('台湾'),('他娘的'),('它妈的'),('四川独立'),('中共'),('政治'),('中国'),('政治'),('中华人民共和国'),('阴蒂'),('乱伦'),('手淫'),('阴唇'),('阴户'),('强奸'),('阴茎'),('法轮'),('反政府'),('色情小说'),('黄网'),('共产党'),('裸聊'),('无码片'),('无码'),('西藏'),('西藏独立'),('下体'),('香港'),('新疆独立'),('性爱'),('性交'),('性免费电影'),('性虐待'),('性网站'),('性網站'),('胸部'),('阳具'),('阳痿'),('一夜情迷奸'),('一中一台'),('阴胫'),('阴毛'),('阴毛小穴'),('阴门'),('阴囊'),('阴水'),('陰蒂'),('淫虫'),('淫蟲'),('淫荡'),('淫蕩'),('淫奸'),('二奶'),('四六级答案'),('走私'),('皮箱炸弹'),('人民法院'),('练功'),('两个中国'),('轮奸'),('麻醉药'),('枪决'),('侵犯'),('窃听'),('色情服务'),('招妓'),('召妓'),('妓女'),('幼女'),('蛊惑'),('蒋介石'),('马的阳具'),('阳萎'),('野鸡'),('阴部'),('阴唇'),('阴道'),('阴茎'),('阴毛'),('淫'),('淫荡'),('淫妇'),('淫秽'),('淫水'),('淫西'),('月经'),('杂种'),('招妓'),('祖宗'),('做爱'),('回民吃猪肉'),('台独'),('台湾独立'),('疆独'),('藏独'),('周恩来'),('烂逼'),('烂比'),('烂货'),('老婊子'),('老虎机'),('老虎機'),('老人政治'),('李总理'),('两性狂情'),('裸干'),('裸聊'),('裸聊合法'),('裸女对对碰'),('裸女對對碰'),('裸体'),('骆冰淫传'),('雒树刚'),('妈逼'),('妈个'),('妈妈的'),('妈批'),('媽個'),('媽媽的'),('美女视频'),('美女視頻'),('美女野球拳'),('美女做愛'),('美少女麻雀'),('美腿'),('美腿写真'),('美腿寫真'),('妹疼'),('妹痛'),('夢幻麻將館'),('猛插'),('猛男强奸'),('迷幻精华素'),('迷幻药'),('迷幻液'),('迷昏蚊'),('迷昏香烟'),('迷昏药'),('迷魂'),('魂蚊香'),('迷魂香'),('迷魂香烟'),('迷魂药'),('内裤'),('内射'),('内争人权'),('奶头真红'),('奶子'),('女教師'),('女女'),('女神教'),('女优'),('女优片'),('女優'),('虐待'),('喷剂'),('喷精'),('喷尿'),('喷雾迷药'),('喷雾型'),('窃听'),('窃听器'),('窃听器材'),('窃听设备'),('窃听王'),('竊聽器'),('竊聽器材'),('清纯少妇'),('清官团'),('清海师父'),('清海无上师'),('清华帮'),('情妇'),('情色'),('情色{5}'),('情色大片'),('情色电影'),('情色光碟'),('情色呻吟'),('情色贴图'),('情色图片'),('情色文学'),('情色小说'),('情色影片'),('底走光'),('群发器'),('群發器'),('群奸'),('群交'),('群射'),('骚逼'),('骚妇'),('骚乱'),('骚穴'),('色情'),('色戒未删减'),('色空寺'),('色狼'),('色狼网'),('色狼網'),('色情'),('色情{5'),('色情电视'),('色情电影'),('色情電視色情服务'),('色情图片'),('色情圖片'),('色情网站'),('色情網站'),('色情小电影'),('色情小電影'),('色情小说'),('色友'),('色欲'),('杀人事件'),('我靠'),('fuck'),('李洪志'),('法轮功'),('他妈的'),('性爱'),('法轮功'),('falundafa'),('falun'),('江泽民'),('操你妈'),('三级片'),('做爱'),('裸聊'),('fuck'),('妈的'),('你妈'),('他妈'),('fuck'),('shit'),('法轮'),('我操'),('我日'),('色情'),('黄色小说'),('色情网络小说'),('色情电影下载'),('裸聊'),('内衣秀'),('淫水'),('鸡巴'),('罗干'),('郑义'),('王刚'),('乔石'),('黄菊'),('江青'),('程凯'),('钱其琛'),('李长春'),('李瑞环'),('邓小平'),('尉健行'),('李岚清'),('于幼军'),('温家宝'),('吴邦国'),('姬胜德'),('吴官正'),('贾庆林'),('曹刚川'),('李岚清'),('曾培炎'),('张万年'),('由喜贵'),('吴官正'),('郭伯雄'),('廖锡龙'),('徐才厚'),('丁关根'),('吴邦国'),('徐匡迪'),('尉健行'),('王沪宁'),('王宝森'),('滕文生'),('刘华清'),('迟浩田'),('戴相龙'),('华建敏'),('梁光烈'),('李继耐'),('项怀诚'),('陈希同'),('陈小同'),('邓笑贫'),('曾庆红'),('庆红'),('朱?F基'),('朱容基'),('毛泽东'),('毛厕洞'),('毛贼东'),('胡锦涛'),('锦涛'),('胡锦滔'),('胡锦淘'),('胡紧掏'),('胡景涛'),('胡总书记'),('黄丽满'),('王瑞林'),('钱国梁'),('姜春云'),('盛华仁'),('贾廷安'),('李登辉'),('陈水扁'),('马英九'),('李洪志'),('BTO'),('毛主席'),('tianwang'),('台独'),('藏独'),('肛门'),('B样'),('法$'),('法^轮'),('法^^轮'),('法@轮'),('法@@轮'),('法~轮'),('法~~轮'),('法??'),('李洪'),('轮功'),('胡锦涛'),('吴官正'),('黄菊吴仪'),('罗干'),('邹家华'),('真善忍'),('李洪志'),('赵紫阳'),('胡耀邦'),('贾庆林'),('曾庆红'),('李长春'),('王乐泉'),('王兆国'),('回良玉'),('刘淇'),('刘云山'),('张立昌'),('张德江'),('陈良宇'),('周永康'),('俞正声'),('贺国强'),('郭伯雄'),('曹刚川'),('曾培炎'),('李鹏'),('朱?F基'),('李瑞环'),('尉健行'),('萨达姆'),('拉登'),('法轮大法'),('台湾独立'),('恐怖主义'),('明慧网'),('西藏分裂'),('台湾总统'),('新疆独立'),('保卫台湾'),('台湾国'),('陈总统'),('没有人权'),('政府无能'),('皇军'),('造反'),('民运'),('推翻'),('政变'),('压迫'),('色情'),('情色'),('嫖娼'),('卖淫'),('淫秽'),('爆炸'),('集会'),('游行'),('示威'),('自焚'),('垮台'),('倒台'),('分裂'),('镇压'),('迫害'),('独立'),('东突'),('李登辉'),('两国论'),('一边一国'),('陆肆'),('国家主席'),('总理'),('政治局'),('常委'),('中共'),('共军'),('纳粹'),('日本帝国'),('大东亚'),('共荣'),('圈赤匪'),('共匪16大十六大'),('天安门'),('中南海'),('领导'),('独裁'),('统治'),('万岁'),('私服'),('私服'),('私服'),('Bastard'),('bitch'),('cao'),('falundafa'),('fuckyou'),('fuck'),('J8'),('nnd'),('snk.ni8.net'),('TMD'),('YOUXIKA爱滋'),('扒光'),('白痴'),('白?V'),('白烂'),('包皮'),('?卤?'),('婊子'),('婊子'),('操逼'),('操机'),('操??'),('操??妈'),('操??娘'),('操??全家'),('操??祖宗'),('操你'),('操你老妈'),('操你老母'),('操你妈'),('操你奶奶'),('操你娘'),('操你全家'),('操你祖宗'),('??您妈'),('??您娘?H'),('插你'),('插死你'),('吃屎'),('吹箫'),('打炮'),('荡妇'),('?诺?娘'),('放荡'),('肥西干'),('x娘'),('干机掰'),('??老母'),('干??妈'),('干??马'),('干??娘'),('干你'),('干你老母'),('干你良'),('干你妈'),('干你娘'),('干您娘'),('干七八'),('干死CS'),('干死GM'),('干死客服'),('干死你'),('?指幽?娘'),('?改隳?'),('狗狼养的'),('狗娘养的'),('狗屁'),('狗日'),('狗日的'),('龟儿子'),('龟头'),('鬼公'),('花柳'),('机八'),('机巴'),('鸡8'),('鸡八'),('鸡巴'),('鸡叭'),('鸡吧'),('芭'),('鸡鸡'),('鸡奸'),('?u巴'),('几八'),('几巴'),('几叭'),('几芭'),('妓'),('妓女'),('妓院'),('奸'),('??贱'),('B贱货'),('贱人'),('?v'),('交配'),('姣西'),('叫床'),('她妈的'),('她马的'),('她奶奶的'),('军妓'),('靠爸'),('靠母'),('靠腰口肯'),('懒8'),('懒八'),('懒教'),('烂逼'),('烂货'),('??'),('浪叫'),('老母'),('老味'),('轮奸'),('妈B'),('妈逼'),('妈比'),('妈的'),('妈的B'),('乳房'),('乳头'),('撒尿'),('塞你娘赛??阿母'),('骚'),('骚逼'),('骚货'),('傻B'),('傻逼'),('傻比'),('射精'),('射你'),('马的阳具'),('阳萎'),('野鸡'),('阴部'),('阴唇'),('阴道'),('阴茎'),('阴毛'),('淫'),('淫荡'),('淫妇'),('淫秽'),('淫水'),('淫西'),('月经'),('杂种'),('招妓'),('祖宗'),('做爱'),('回民吃猪肉'),('台独（台湾独立）'),('疆独'),('藏独'),('周恩来'),('抵制游行'),('大善大忍'),('宗教迫害'),('赤党'),('网络封锁'),('相互勾联'),('党禁'),('敌对分子'),('反共'),('海外民运'),('宪政'),('梵蒂冈'),('天灭中共'),('平反六四'),('渗透中共'),('政治变革'),('呼吁书'),('党棍官'),('网络活动颠覆'),('人权保障'),('巴哈依教'),('台人治台'),('公民私权'),('告全国同胞书'),('西藏地震'),('大法'),('自焚'),('迫害致死'),('邪恶势力'),('李洪志'),('九评'),('赤裸'),('口交'),('性欲'),('肛交'),('爱液'),('奶头'),('精液'),('做爱'),('作爱'),('强暴'),('淫水'),('新疆独立'),('疆独'),('颠覆国家'),('共匪'),('共党'),('中国共产党'),('台独'),('代练'),('动网'),('动网先锋'),('六合彩'),('6合彩'),('汽车炸弹'),('法轮功'),('海洛因'),('枪支'),('入党'),('共产党'),('炸药'),('独裁'),('独派'),('狗日的'),('统治'),('维权'),('绿营'),('胡主席'),('反共'),('愤青'),('日本'),('抗日'),('她妈的'),('保平安'),('圣火'),('钓鱼岛'),('鸡吧'),('天安门'),('飞弹'),('炸药'),('宪法'),('法律'),('屠杀'),('自杀'),('强奸'),('民权'),('迷药'),('迷幻'),('贱人'),('销售军用手枪'),('陈水扁'),('误国'),('垃圾'),('恐怖'),('袭击'),('犯法'),('政治'),('犯罪'),('犯法'),('枪毙'),('丁子霖'),('7号文件'),('3号文件'),('4号文件'),('5号文件'),('6号文件'),('特码'),('七贤帮'),('三民主义'),('四人帮'),('干部'),('世维'),('东突'),('伊斯兰'),('余传琮'),('公开信'),('公投'),('雷管'),('手枪'),('吾吉买买提'),('吾尔凯西'),('周永康'),('烧光'),('抢光'),('地震'),('国民党'),('土制'),('圣战'),('胡锦涛'),('温家宝'),('习近平'),('江泽民'),('毛泽东'),('毛主席'),('温总理'),('周恩来'),('周总理'),('穆斯林'),('兄弟会'),('立法'),('立委'),('法院'),('连战'),('陈水扁'),('马英九'),('赛马'),('赌博'),('骰子'),('黄大仙'),('白小姐'),('彩票'),('办证'),('骚货'),('贱货'),('配方'),('曾道人'),('西北帮'),('开枪'),('腐败'),('真主党'),('胡温'),('氯胺酮'),('民权'),('货到付款'),('拉登'),('引爆'),('左派'),('右派'),('司法院'),('吕秀莲'),('危机'),('间谍'),('宗教'),('教主'),('司令'),('自缢'),('嫌犯'),('逃犯'),('派出所'),('公安局'),('检察机关'),('大会堂'),('检察院'),('刑讯'),('逼迫'),('刑法'),('死刑'),('上吊'),('盗窃'),('在押'),('办案'),('摇头丸'),('K粉'),('妖术'),('鸦片'),('罂粟'),('狗屎'),('警察'),('贱'),('奸'),('专制'),('违反'),('违法'),('八路军'),('游行'),('伊斯兰'),('宗教'),('教主'),('非法'),('暴行'),('地震'),('政府'),('腐败'),('内讧'),('裁决'),('公安局'),('派出所'),('法律'),('黑幕'),('奸商'),('间谍'),('飞弹'),('导弹'),('条约'),('邪教'),('推翻'),('本拉登'),('生死'),('政策'),('远程炸弹'),('远程攻击'),('軍官'),('基地'),('枪杀'),('致死'),('天灭'),('平反'),('炸弹'),('抵制'),('荡妇'),('干b'),('干B卧槽'),('含丸'),('舔菊'),('革命'),('A片'),('a片'),('骚'),('违反'),('违法'),('抢劫'),('反动'),('阳具'),('共产'),('侮辱'),('诋毁'),('战争'),('欺骗'),('灭国'),('换妻'),('脱光'),('群交'),('裸奔'),('避孕套'),('少儿'),('花柳'),('性病'),('001工程'),('17大'),('17权力布局'),('18禁'),('1Pondo'),('21世纪中国基金会'),('301医院'),('314事件'),('371工程'),('3退'),('4．25事件'),('51ini.com'),('571工程'),('6-4tianwang'),('6.4事件'),('610洗脑班'),('64大屠杀'),('64学潮'),('6合彩'),('AV麻将'),('AV麻將'),('AV女'),('A级'),('A片'),('bitch'),('C4等炸弹制作方法炸药制作'),('C4塑胶TNT'),('C4炸药制作'),('c4制作方法'),('DalaiLama自傳'),('DIY原子弹'),('falun'),('falu'),('falun'),('falundafa'),('falungong'),('FaLunGong'),('fangong'),('fanhua'),('fantizi520'),('Fa轮功'),('fa轮功'),('fuck'),('GHB'),('GHB粉'),('GHB强效迷幻液'),('GPS预警器'),('GPS預警器'),('hongzhi'),('J巴'),('K粉'),('lama'),('lihongzhi'),('MC军团'),('MC軍團'),('功学T'),('PE-6拦截器'),('PE-6攔截器'),('peacehall'),('PK黑社会'),('qiang支'),('qq幸运用户抽奖'),('qq幸哂脩舫楠?'),('SIM卡抽奖'),('SM虐待'),('suck'),('svdc'),('taip'),('The3FeelOnline'),('tianwang'),('tibetalk'),('TMD=>真美妙'),('TND'),('TNND'),('TNT炸弹制作方法'),('tnt炸药'),('TNT炸药制作法'),('VOA采访热比娅'),('[av]'),('[hz]'),('[sm]'),('“震撼”视频文件'),('震撼中”文件'),('《12个春秋》'),('《风波记》'),('《目睹中国改革开放28年之怪现状》'),('《苹果》'),('《色戒》'),('《天音》'),('《天音》专辑'),('《中国民主》'),('６10办公室'),('８.9事件'),('阿不来提'),('阿里布达年代记'),('艾司唑仑'),('艾斯海提'),('爱国运动'),('爱国者同盟'),('爱国者同盟网站'),('爱姐妹'),('爱神之传奇'),('爱滋村里的爱'),('愛姐妹'),('安定片'),('安魂网'),('安立敏'),('八九见证'),('八九学潮'),('八荣八耻'),('巴赫'),('巴拉斯'),('巴特尔'),('巴音朝鲁'),('罢工'),('罢课'),('白军'),('白立朴'),('白梦'),('白皮书'),('白小姐'),('办高利贷'),('办理文凭'),('办理证件'),('办证'),('伴我淫'),('辦理文憑'),('辦理證件'),('包娃衣'),('包夜'),('保钓'),('报告汇编'),('报禁'),('报码聊天'),('鲍戈'),('鲍彤'),('暴动'),('暴動'),('暴干'),('暴力拆迁'),('暴力冲突'),('暴力镇压'),('暴乱'),('暴亂'),('暴政'),('爆炸物'),('北京黑幕'),('北京劲展鹏'),('北京市委常委'),('北京市委黑幕'),('北京宣传部长'),('北京战争'),('北京政法委书记'),('北京政权'),('北京政坛清华名人'),('本拉登'),('苯巴比妥'),('苯乙酸诺龙'),('避孕套'),('变革之风'),('变声电话'),('变声器'),('变态'),('變聲電話'),('變聲器'),('婊子'),('冰毒'),('冰火'),('兵种教材'),('病业说'),('波霸'),('波动少女'),('波動少女'),('伯希来'),('博彩'),('博讯'),('薄格'),('薄熙来'),('不良少女日记'),('不是易非毁责。志空服金生'),('步枪'),('财政部绝密'),('蔡崇国'),('参谋业务参考资料'),('沧澜曲'),('藏春阁'),('藏春閣'),('藏獨'),('藏独'),('藏妇会'),('藏青会'),('操B'),('操逼'),('操比'),('操蛋'),('操你'),('操你妈'),('操你娘'),('操死'),('操'),('操他'),('曹长青'),('曹刚川'),('草莓牛奶'),('草你妈'),('插B'),('插插'),('插她'),('插你'),('插他'),('插我'),('柴玲'),('禅密功'),('長瀨愛'),('常劲'),('常委'),('常委汪兆钧'),('超常科学'),('超越红墙'),('朝河蘭'),('潮吹'),('炒股国歌'),('炒股國歌'),('车仑'),('车牌反光'),('陈炳基'),('陈军'),('陈良宇'),('陈良宇沉浮之路'),('陈蒙'),('陈破空'),('陈水扁'),('陈水扁总统'),('陈希同'),('陈小同'),('陈宣良'),('陈一谘'),('陈总统'),('陳方安生'),('成佛做主'),('成人{5}'),('成人表演'),('成人电影'),('成人電影'),('成人激情'),('成人交友'),('成人卡通'),('成人录像'),('成人论坛'),('成人論壇'),('成人漫画'),('成人配色'),('成人片'),('成人书库'),('成人贴图'),('成人貼圖'),('成人图片'),('成人圖片'),('成人网站'),('成人網站'),('成人文学'),('成人小说'),('成人小說'),('成人笑话'),('成人杂志'),('成人雜誌'),('城管'),('程凯'),('程铁军'),('程渭山'),('程真'),('痴汉是犯罪'),('癡漢是犯罪'),('迟浩田'),('迟来的救灾'),('持不同政见'),('赤匪'),('赤化'),('抽插'),('臭作'),('出售冰毒'),('出售答案'),('出售弹药'),('出售二手走私车'),('出售发票'),('出售工字'),('出售假币'),('出售假幣'),('出售雷管'),('出售雷管炸药自制炸弹'),('出售枪支'),('出售槍支'),('出售手枪'),('出售手槍'),('出售银行'),('出售炸药'),('出售走私车'),('除湿机'),('处女'),('处女终结者{MOD}'),('处女终结者**'),('川島和津實'),('传九退三'),('传说的胡曾联手是一种假象'),('传真群发'),('传中共中央关于17大的人事安排意见'),('创世之子猎艳之旅'),('吹萧'),('春夏之交'),('春夏自由论坛'),('春药'),('春藥'),('慈悲功'),('粗口歌'),('促红细胞生成素'),('崔会烈'),('催泪弹'),('催泪枪'),('催情春药'),('催情粉'),('催情水'),('催情药'),('催情藥'),('催情液'),('达赖'),('达赖喇嘛'),('达赖领奖'),('达米宣教会'),('答案卫星接收机'),('打{10}倒{10}'),('{10}产{10}党'),('打保单'),('打倒共产党'),('打飞机'),('打炮'),('打坦克手册'),('打砸抢'),('大b'),('大{10}纪{10}元'),('大逼'),('大比'),('大法'),('大法大福'),('大法大纪园'),('大法弟子'),('大法洪传'),('大法师傅'),('大法新闻社'),('大法修炼者'),('大法之声'),('大砝弟子'),('大花逼'),('大鸡巴'),('大记元'),('大纪'),('大纪元'),('大纪元时报'),('大纪元新闻网'),('大纪元杂志'),('大纪园'),('大紀元'),('大陆独裁者'),('大学暴动'),('大学骚乱'),('大學騷亂'),('大圆满法'),('大祚荣'),('代办'),('代办签证'),('代办证件'),('代办证件{MOD}'),('代办证件**'),('代辦'),('代缴发票'),('代缴税'),('代开'),('代开发票'),('代开发票{MOD}'),('代开发票**'),('代开商业发票'),('代开增值税'),('代開'),('代開發票'),('代卖发票'),('代售发票'),('代孕'),('戴海静'),('戴海靜'),('戴相龙'),('耽美'),('党保平安'),('党棍'),('党国'),('党禁'),('党魁'),('党妈妈'),('党内内部资料'),('党内权力'),('党内权争'),('党内危机'),('党内言事潮'),('党委书记'),('党文化'),('党政人事'),('荡妇'),('登辉'),('等级答案'),('邓二世'),('邓力群自述'),('邓派'),('邓小平'),('邓笑贫'),('邓颖超日记'),('鄧小平'),('狄玉明'),('迪里夏提'),('底裤'),('地磅仪'),('地磅儀'),('地方召会'),('地块规划'),('地西泮片'),('地下教会'),('地下刊物'),('地下先烈'),('地震天谴'),('弟疼'),('弟痛'),('弟子'),('帝国之梦'),('帝國之夢'),('递进民主'),('第二代身份证'),('第三党'),('第三道路党'),('第十六次代表'),('第四代'),('第五代红人'),('第五代接班梯队'),('第五代中央领导人'),('第一书记'),('颠覆中国政权'),('颠覆中华人民共和国政'),('点对点裸聊'),('电车之狼'),('电动葫芦'),('电话拦截'),('电视流氓'),('电信路藏民'),('电子狗'),('電車之狼'),('電話攔截'),('電子狗'),('钓鱼岛'),('调试家用卫星'),('調教'),('丁关根'),('丁元'),('丁子霖'),('叮丙诺菲'),('定情粉'),('定情药'),('东北独立'),('东突'),('东突厥斯坦'),('东突厥斯坦伊斯兰'),('东突厥斯坦伊斯兰运动'),('东土耳其斯坦'),('东西南北论坛'),('东洲'),('董存瑞问'),('董存瑞問'),('董宜胜'),('动乱'),('毒气赌具'),('独裁'),('独裁政治'),('独立台湾会'),('独立中文笔会'),('赌博粉'),('赌博专用'),('赌球'),('赌球网站'),('賭博粉'),('賭博專用'),('杜智富'),('渡海登陆作战选编'),('短信广告'),('短信猫'),('短信群发'),('短信群发器'),('短信群发器{MOD}'),('短信群发器**'),('短信商务广告'),('对共产党清算'),('对日强硬'),('对外高层人事'),('对中共的姑息就是对死难者的残忍'),('多党'),('多党执政'),('多黨'),('多吉才让'),('多美康'),('多维新闻'),('屙民'),('恶党'),('恶警'),('恩诺欣'),('二B'),('二逼'),('二奶大奖'),('二奶大赛'),('二奶大賽'),('发-票'),('发愣'),('发抡'),('发抡功'),('发仑'),('发仑da发'),('发伦'),('发伦工'),('发伦功'),('发囵'),('发沦'),('发纶'),('发轮'),('发轮功'),('发轮功陈果'),('发论'),('发论公'),('发论功'),('发票'),('发票代开'),('发正念'),('發-票'),('發倫'),('發淪'),('發輪'),('發論'),('發票'),('法轮功'),('法*功'),('法+轮+功'),('法+輪+功'),('法.{0'),('6}轮.{0'),('6}功'),('法.{0'),('6}輪.{0'),('6}功'),('法.轮.功'),('法?'),('法lun功'),('法L功'),('法{10}轮{10}功'),('法？轮？功'),('法车仑工力'),('法功'),('法国游'),('法拉盛'),('法拉盛缅街'),('法愣'),('法抡'),('法抡功'),('法仑'),('法伦'),('法囵'),('法沦'),('法纶'),('法轮'),('法轮{5}'),('法轮大法'),('法轮弟子'),('法轮佛法'),('法轮功'),('法倫'),('法淪'),('法輪'),('法论'),('法論'),('法十轮十功'),('法一轮'),('法谪'),('法谪功'),('法正'),('法正乾坤'),('法正人间'),('砝仑'),('砝伦'),('砝轮'),('珐(工力)学T'),('珐.輪功'),('樊守志'),('反党'),('反动'),('反对共产党'),('反对共产主义'),('反封锁'),('反封锁技术'),('反腐败论坛'),('反腐总攻'),('反革命政变纲领'),('反攻大陆'),('反共'),('反共传单'),('反共言论'),('反华'),('反雷达测速'),('反雷達測速'),('反民主'),('反人类'),('反人类罪'),('反社会'),('反政府'),('反中'),('反中共黑色暴力'),('饭岛爱'),('飯島愛'),('方励之'),('方针定调'),('方舟子'),('方祖岐'),('芳香型智悟气功'),('防拍器'),('防身药水'),('仿真枪'),('放荡青春'),('放光明电视制作中心'),('放下生死'),('飞天舞蹈学校'),('飞扬论坛'),('斐得勒'),('废除劳教签名'),('废统'),('费良勇'),('分家在'),('分裂'),('粉嫩小洞'),('粉饰太平'),('粉碎林陈材料'),('粉碎林陈反党集团反革命政变的斗'),('风骚侍女'),('风雨神州论坛'),('风月大陆'),('封从德'),('封杀'),('風花'),('冯东海'),('冯素英'),('佛怀煽仇录'),('佛展千手法'),('夫妇乐园'),('服务器托管'),('府软弱'),('腐败'),('腐敗'),('付申奇'),('妇的哀羞'),('复员军官代表'),('复员军官诉求书'),('复转军人'),('副书记'),('副主席'),('傅成玉'),('傅申奇'),('傅志寰'),('改革六大成绩'),('改革阵痛'),('干你妈'),('干她'),('干妳'),('干妳老母'),('干妳妈'),('干妳娘'),('干你'),('干你妈'),('干你妈b'),('干你妈逼'),('干你娘'),('干扰器'),('干扰赈灾募捐事件回放'),('干死你'),('幹擾器'),('赣江学院'),('肛交'),('肛门'),('港澳博球网'),('高潮'),('高低温交变湿热试验箱'),('高干'),('高干新子弟'),('高干子弟'),('高干子弟名单'),('高干子女'),('高幹'),('高幹子弟'),('高幹子女'),('高官'),('高文谦'),('高效春药'),('高校罢餐'),('高校暴乱'),('高校暴亂'),('高校群体事件'),('高校骚乱'),('高校騷亂'),('高薪养廉'),('高瞻'),('高智晟'),('高自联'),('睾丸'),('睾丸素'),('告全国股民同胞书'),('告全国人大书'),('告全体网民书'),('告中国人民解放军广大官兵书'),('戈扬'),('哥疼'),('哥痛'),('鸽派'),('割肉人'),('歌功颂德'),('革命'),('革命无罪'),('葛振峰'),('蛤蟆'),('蛤蟆转世'),('个人崇拜'),('个人圆满说'),('个邪的党（魔教）'),('跟踪定位器{MOD}'),('跟踪定位器**'),('更衣'),('工力'),('工业炸药配方设计'),('工自联'),('公安'),('公安部'),('公安文件'),('公安与武警的区别'),('公款'),('功法'),('功学'),('功学员'),('功友'),('功友弟子'),('攻台'),('龚平?'),('共+产+党'),('共+產+黨'),('共+铲+党'),('共.{0'),('6}产.{0'),('6}党'),('共.{0'),('6}產.{0'),('6}黨'),('共.{0'),('6}铲.{0'),('6}党'),('共.产.党'),('共.产党'),('共chang党'),('共残党'),('共惨党'),('共产'),('共产.党'),('共产党'),('共产党的报应'),('共产党的末日'),('共产极权'),('共产王朝'),('共产无赖'),('共产小丑'),('共产主义'),('共产主义黑皮书'),('共产专制'),('共铲党'),('共党'),('共黨'),('共匪'),('共狗'),('共和国之怒'),('共军'),('共奴'),('勾清明'),('狗b'),('狗逼'),('狗操'),('狗卵子'),('狗娘'),('古方迷香'),('古拉格'),('乖乖药'),('关于对敌斗争中有关政政策界限汇编'),('关于做好定案材料工作的意见'),('关卓中'),('观音法门'),('官场日志'),('官商勾结'),('官员成为政治新星'),('贯通两极法'),('光端机'),('广安事件'),('广东饶平钱东镇'),('广闻'),('廣安事件'),('龟头'),('硅谷民主促进会'),('龜頭'),('桂世镛'),('郭伯雄'),('郭罗基'),('郭平'),('郭岩华'),('国际声援西藏'),('国际西藏运动'),('国际援藏网'),('国家安全'),('国家机密'),('国家粮油统计报表'),('国家政府档案'),('国军'),('国民党'),('国民党名单'),('国色天香网'),('国新办主任'),('国研新闻邮件'),('国贼'),('国之利刃'),('哈批'),('海盗的悠闲'),('海乐神'),('海外护法'),('酣乐欣'),('韩东方'),('韩联潮'),('韩杼滨'),('汉风'),('号码生成器'),('何德普'),('何勇'),('和奸成瘾'),('和解的智慧'),('和平请愿书'),('和平修练'),('和平演变道路已'),('被中共堵死'),('和弦'),('河殇'),('河殇核污染'),('核设施'),('核污染'),('贺邦靖'),('褐铁矿选矿设备'),('黑车'),('黑幕重重腐败到底'),('黑皮书'),('黑枪'),('黑社会'),('黑社會'),('黑手党'),('黑瞎子岛'),('黑星女侠'),('黑窑工母亲网上寻助'),('黑窑母亲群体募捐'),('嘿咻'),('弘法会'),('红病历'),('红朝谎言录'),('红潮谎言录'),('红潮艳史'),('红灯区'),('红色恐怖'),('红兽'),('红头文件'),('红卫兵'),('红血丝'),('红志'),('红智'),('洪法交流'),('洪巨平'),('洪水的志向'),('洪吟'),('洪哲胜'),('洪志'),('洪治'),('洪智'),('紅河谷論壇'),('紅志'),('紅智'),('后方想定的编写'),('后勤资料手册'),('呼喊派'),('胡J涛'),('胡春华'),('胡的接班人'),('胡江'),('胡紧掏'),('胡紧套'),('胡锦涛'),('胡锦滔'),('胡锦淘'),('胡鍧?胡鍧?胡进涛'),('胡景涛'),('胡派'),('胡平'),('胡书记'),('胡温'),('胡温怒批政法系统'),('胡溫怒批政法系統'),('胡瘟'),('胡系'),('胡下台'),('胡新宇'),('胡耀邦'),('胡耀帮'),('胡中央'),('胡主席'),('胡总书记'),('华藏功'),('华国锋'),('华建敏'),('华通时事论坛'),('华夏文摘'),('华语世界论坛'),('华岳时事论坛'),('话在肉身显现'),('换妻'),('換妻'),('黃海暗殺事件'),('黃菊'),('黃麗滿'),('黃色電影'),('黃色圖片'),('黃色小電影'),('黄JU'),('黄慈萍'),('黄海暗杀'),('黄海暗杀事件'),('黄海事件'),('黄褐斑'),('黄华华'),('黄祸'),('黄金书'),('黄局'),('黄菊'),('黄菊　'),('黄菊遗孀'),('黄丽满'),('黄霉素'),('黄色{5}'),('黄色电影'),('黄色漫画'),('黄色图片'),('黄色网站'),('黄色文学'),('黄色小电影'),('黄色小说'),('黄色影视'),('黄网导航'),('黄翔'),('黄作兴'),('簧片'),('回良玉'),('回民暴'),('回民暴动'),('回民猪'),('回忆六四'),('悔过书'),('惠澤社群'),('慧网'),('昏迷剂'),('昏药'),('昏藥'),('混蛋神风流史'),('活佛'),('活体'),('火药制作'),('霍英东'),('鸡八'),('鸡巴'),('鸡吧'),('鸡奸'),('鸡毛信文汇'),('姬胜德'),('积克馆'),('基督'),('基督灵恩布道团'),('激流中国'),('激情大片'),('激情电影'),('激情電影'),('激情聊天'),('激情美女'),('激情视频'),('激情視頻'),('激情图片'),('激情圖片'),('激情文学'),('激情小电影'),('激情小電影'),('激情自拍'),('及川奈央'),('吉炳轩'),('吉祥宝贝'),('疾病业债说'),('集体上访'),('集体做爱'),('记者原子弹的DIY制作'),('纪律检查'),('纪元'),('妓女=>jnv'),('妓女的口号'),('加盖机密××××'),('佳静安定片'),('家用天线'),('家用卫星'),('贾庆林'),('贾廷安'),('贾系'),('贾育台'),('贾治邦'),('賈慶林'),('假币=>>'),('假币出售'),('假钞'),('假鈔'),('假教育'),('监听'),('监听宝'),('监听器'),('监听设备'),('监听王'),('监狱管理局'),('监狱里的斗争'),('監聽寶'),('監聽器'),('監聽王'),('简鸿章'),('简易原子弹制作'),('简易炸药制作'),('简易制作C4炸弹教程'),('建定防火'),('建国党'),('剑教材'),('贱逼'),('贱比'),('践踏中国女性'),('江core'),('江ze民'),('江z民'),('江{1}泽{2}民'),('江八点'),('江独裁'),('江恶人'),('江二世'),('江蛤蟆'),('江公子'),('江核心'),('江黑心'),('江胡'),('江湖淫娘'),('江昏君'),('江祸心'),('江家帮'),('江姐问'),('江姐問'),('江锦恒'),('江老贼'),('江理论'),('江流氓'),('江路线'),('江驴'),('江罗'),('江罗集团'),('江绵恒'),('江魔头'),('江牌'),('江派'),('江派和胡派'),('江派人马'),('江青'),('江青××'),('江青同志在批林整风汇报会议华东组和中南组会上的讲话'),('江青在批林反孔会议上的讲话×××'),('江泉集团'),('江人马'),('三条腿'),('江三秀'),('江山美人志'),('江神经'),('江氏'),('江氏集团'),('江氏家族'),('江梳头'),('江宋'),('江戏子'),('江系'),('江系人马'),('江宰民'),('江则民'),('江择min'),('江泽公审'),('江泽慧'),('江泽民'),('江泽民瑞士'),('江泽民宋祖英'),('江澤民'),('江贼'),('江贼民'),('江折民'),('江者民'),('江浙民'),('江朱'),('江猪'),('江猪媳'),('江主席'),('江作秀'),('姜春云'),('姜凤阁'),('将则民'),('僵贼'),('僵贼民'),('疆獨'),('疆独'),('疆独藏独'),('讲法'),('讲事实要说法'),('蒋匪军陆军部队师以上主官情况调查'),('蒋彦永'),('降灾民'),('酱猪媳'),('交媾'),('交警'),('交友网'),('胶质炸弹'),('焦焕成'),('角落里的枪'),('狡猾的风水相师'),('叫床'),('叫春'),('教徒'),('教徒人权'),('教养院'),('阶级敌人'),('接班群体'),('揭个黑幕'),('揭批书'),('姐疼'),('姐痛'),('解放军的最新式军服>*@*'),('解厚铨'),('解码器'),('解碼器'),('解体的命运'),('解体中共'),('解学智'),('解振华'),('金伯帆'),('金道铭'),('金鳞岂是池中物'),('金尧如'),('金银焕'),('金澤文子'),('津人治津'),('锦涛'),('近平'),('近親'),('禁断少女'),('禁忌试玩'),('禁忌試玩'),('禁看'),('禁书'),('禁网禁片'),('京郊旅游'),('经文'),('经租房'),('惊暴双乳'),('惊悚空间'),('晶白体'),('精液'),('驚悚空間'),('警察'),('警用教材'),('净白体'),('净水器'),('靖志远'),('静坐'),('九-评'),('九.评'),('九.十.三运动'),('九评'),('九评{5}'),('九评××'),('九评公产党'),('九评共产党'),('九十三运动'),('旧金山总领馆'),('旧民运'),('具有中国特色的魔鬼辞典'),('绝版'),('绝人寰的暴行'),('觉醒了的中国'),('军长发威PK'),('军队干部转业复员工作文件汇编'),('军队走私'),('军火'),('军妓'),('军事标号'),('军事地图'),('军事训练大纲'),('军事训练评定标准'),('军委公开信'),('军需物质保管'),('军需物资保管'),('军用地图'),('军用教材'),('军用手枪'),('军政名单'),('军转安置'),('军转干部'),('軍長發威PK'),('軍火'),('卡耐基'),('卡辛纳大道和三福大道交界处'),('开垦民主'),('开天目'),('康成元'),('康涛杰'),('磁悬浮'),('中共当局'),('考后付款'),('考前发放'),('靠你妈'),('科西嘉独立'),('颗粒包装机'),('刻章'),('空难'),('空難'),('空中民主墙'),('恐怖分子'),('恐怖分子第一步炸弹'),('恐怖分子DIY'),('恐怖分子必备'),('恐怖分子皮箱炸弹'),('恐怖分子傻瓜手册'),('恐怖牢笼'),('恐怖牢?恐共'),('恐惧杀手'),('恐懼殺手'),('口技'),('口交'),('口香糖炸弹'),('口淫'),('扣应队'),('酷刑罪{MOD}'),('酷刑罪**'),('跨世纪的良心犯'),('快操我'),('狂操'),('狂插'),('狂干'),('狂搞'),('邝锦文'),('昆仑女神功'),('捆绑'),('拉帮游说'),('拉登'),('拉登熟知中国'),('拉登熟知中國'),('拉票贿选'),('拉萨僧人接连抗议'),('拉萨事件'),('喇嘛'),('赖昌星'),('拦截器'),('攔截器'),('烂逼'),('烂比'),('烂货'),('郎咸平'),('劳动教养所'),('劳改'),('劳工观察'),('劳教'),('劳教基金'),('老婊子'),('老虎机'),('老虎機'),('老江'),('老毛'),('老人政治'),('雷达测速器'),('雷達測速器'),('雷管'),('雷管制造技术'),('雷鸣球'),('冷宽'),('黎安友'),('黎阳平'),('黎阳评'),('黎陽評'),('黎智英'),('李洪志'),('李{10}洪{10}志'),('李长春'),('李長春'),('李崇礼'),('李崇仁'),('李崇禧'),('李传卿'),('李大尸'),('李大师'),('李登辉'),('李登辉总统'),('李弘旨'),('李红志'),('李红痔'),('李红智'),('李宏志'),('李宏治'),('李宏智'),('李洪宽'),('李洪志'),('李洪智'),('李洪痣'),('李洪誌'),('李鸿志'),('李鸿智'),('李继耐'),('李兰菊'),('李岚清'),('李嵐清'),('李良辉'),('李录'),('李禄'),('李鹏'),('李鹏之女'),('李鵬'),('李瑞环'),('李瑞環'),('李三共志'),('李少民'),('李淑娴'),('李旺阳'),('李伟信的笔供'),('李文斌'),('李向东'),('李小朋'),('李小鹏'),('李远哲'),('李遠哲'),('李月月鸟'),('李志绥'),('李总理'),('李总统'),('里藏春'),('历史篡改者'),('历史上的真实故事'),('俪影蝎心'),('连队资料'),('连胜德'),('连线机'),('莲花艺术团'),('連發'),('联合起诉廉政大论坛'),('联合起诉最高人民法院'),('联名上书'),('联总'),('联总这声传单'),('联总之声'),('联总之声传单'),('廉政大论坛'),('练功群众'),('炼功'),('梁光烈'),('梁擎墩'),('两岸关系'),('两岸三地论坛'),('两个中国'),('两会'),('两会报道'),('两会新闻'),('两派争斗'),('两性狂情'),('两性淫乱'),('兩性淫亂'),('辽阳工潮'),('廖锡龙'),('劣等人种博彩'),('猎枪'),('猎杀熊猫'),('林保华'),('林彪'),('林彪事件真相×××××'),('林彪元帅军事论文'),('林长盛'),('林樵清'),('林慎立'),('林昭纪念奖'),('临震预报'),('灵修团体{BANNED}'),('灵修团体**'),('凌锋'),('凌辱'),('铃声下载'),('菱恝'),('领导班子名单外泄'),('领导财产公示'),('领导层预备人选'),('領導財產公示'),('令狐安'),('令狐计划'),('令计划'),('刘宾深'),('刘宾雁'),('刘春良'),('刘刚'),('刘国凯'),('刘华清'),('刘俊国'),('刘凯中'),('刘奇葆'),('刘千石'),('刘青'),('刘山青'),('刘少奇'),('刘士贤'),('刘文胜'),('刘晓波'),('刘晓竹'),('刘永川'),('流氓警察'),('流氓民运'),('流氓无产者'),('流脑'),('流水穴'),('流亡'),('流亡藏人'),('流亡政府'),('流亡中的自在'),('留四进三'),('六●四'),('六彩'),('六代接班人'),('六合'),('六合采'),('六合彩'),('六级答案'),('六四'),('六四母亲'),('六四青年网'),('六四事件'),('六四受难者家属证辞'),('六四屠城'),('六四屠杀'),('六四宣言'),('六四学潮'),('六四血案'),('六四运动'),('六四政变'),('六四之役'),('六四资料馆'),('六星合彩'),('六月联盟'),('六月聯盟'),('龙虎豹'),('龙新民'),('龍虎豹'),('露点'),('露拉3D'),('露毛'),('露乳'),('鲁俊'),('陆空军地图'),('陆肆'),('陆委'),('陆委会'),('吕京花'),('吕秀莲'),('铝热剂'),('氯三苯乙烯'),('氯硝安定'),('乱交'),('乱伦'),('亂倫'),('抡功'),('伦敦西藏网'),('伦公'),('伦功'),('伦攻'),('沦公'),('沦功'),('沦攻'),('轮暴致死'),('轮大'),('轮公'),('轮功'),('轮攻'),('轮奸'),('轮奸虐杀'),('轮子小报'),('倫公'),('倫功'),('倫攻'),('淪公'),('淪功'),('淪攻'),('輪公'),('輪功'),('輪攻'),('论公'),('论功'),('论攻'),('论坛自动发贴机'),('論公'),('論功'),('論攻'),('罗干'),('罗礼诗'),('羅幹'),('裸干'),('裸聊'),('裸聊合法'),('裸女对对碰'),('裸女對對碰'),('裸体'),('骆冰淫传'),('雒树刚'),('妈逼'),('妈个'),('妈妈的=>他*的'),('妈批'),('媽個'),('媽媽的=>他*的'),('麻古'),('麻衣'),('麻醉'),('麻醉钢枪'),('麻醉鋼槍'),('麻醉迷幻极品'),('麻醉迷'),('麻醉枪'),('麻醉槍'),('麻醉药'),('麻醉藥'),('麻醉乙醚'),('马大维'),('马加爵'),('马来酸迷达唑仑'),('马良骏'),('马三家'),('马时敏'),('马馼'),('马英九总统'),('码会'),('吗啡碱cv'),('吗啡片'),('买春堂'),('买答案'),('买卖枪支'),('買春堂'),('買賣槍支'),('麦角酸'),('卖逼'),('卖比'),('卖答案'),('卖国'),('卖淫'),('猫肉'),('貓肉'),('毛{1}泽{2}东'),('毛厕东'),('毛厕洞'),('毛都露出'),('毛共'),('毛光烈'),('毛派'),('毛如柏'),('毛时代大饥荒揭秘'),('毛一鲜'),('毛一鮮'),('毛泽东'),('毛泽东复活'),('毛泽东侄子'),('毛贼'),('毛贼东'),('毛主席的嫡孙'),('毛主席复活'),('毛主席復活'),('毛主席关于人民军队人民战争及其战略战术论述的摘录'),('毛主席批示'),('毛主席有关指示'),('毛主席致江青同志的信'),('主席资料'),('美国维吾尔'),('美国之音'),('美甲'),('美女视频'),('美女視頻'),('美女野球拳'),('美女做愛'),('美少女麻雀'),('美腿'),('美腿写真'),('美腿寫真'),('妹疼'),('妹痛'),('夢幻麻將館'),('猛插'),('猛男强奸'),('蒙独'),('蒙古独'),('蒙古独立'),('蒙古回归'),('蒙古教育'),('化基金会'),('蒙古牧民联合会'),('蒙古之友'),('蒙汗'),('蒙汗{5}'),('蒙汗药'),('蒙汗药粉'),('蒙汗藥'),('蒙进喜'),('梦幻麻将馆'),('梦网洪志'),('咪达唑仑'),('迷/魂/药/迷/昏/药/三/唑/仑'),('迷{5}药'),('迷幻精华素'),('迷幻药'),('迷幻液'),('迷昏蚊香'),('迷昏香烟'),('迷昏药'),('迷魂'),('魂蚊香'),('迷魂香'),('迷魂香烟'),('迷魂药=>>'),('迷魂药激情'),('迷魂藥'),('迷奸'),('迷奸药'),('迷歼药'),('迷殲藥'),('迷情药=>>'),('迷失北京'),('迷香'),('迷药'),('迷藥'),('秘录'),('秘密潜入'),('秘密潛入'),('密穴'),('绵恒'),('灭中共'),('民国'),('民进党'),('民警'),('民联'),('民殇'),('民意'),('民意论坛'),('民运'),('民运分子'),('民运信息中心'),('民?民?民阵'),('民猪'),('民主'),('民主不能等待'),('民主论坛'),('民主评论'),('民主墙'),('民主运动'),('民主之声'),('民族矛盾'),('民族问题'),('闵耀中'),('名山县'),('明Hui'),('明hui'),('明慧'),('明慧{5}'),('明慧网'),('魔教'),('魔难论'),('末世劫难'),('末世论'),('莫伟强'),('生命自私到了极点的中共政权'),('牟新生'),('母子'),('木犀地'),('木子论'),('内裤'),('内射'),('内争人权'),('奶头真红'),('奶子'),('南大自由论坛'),('南华早报'),('闹事'),('能力主'),('尼总务石'),('吸其目哦轻易'),('倪育贤'),('倪志福'),('你爸'),('你妈逼'),('你妈的'),('你娘的'),('你说我说论坛'),('你他妈'),('年春衫薄'),('牛比'),('钮茂生'),('农民真正贫困的根源'),('女教師'),('女女'),('女神教'),('女优'),('女优片'),('女優'),('虐待'),('诺龙'),('锘挎瘺娉戒笢1234'),('锘挎瘺娉戒笢12345'),('欧泽高'),('欧洲圆明网'),('拍花子'),('拍肩迷'),('拍肩神药'),('拍肩神藥'),('派系斗争'),('潘国平'),('盘古乐队'),('盤古樂隊'),('炮兵参谋手册'),('炮轰铁道部'),('跑官要官'),('泡沫经济'),('喷剂'),('喷精'),('喷尿'),('喷雾迷药'),('喷雾型'),('彭钢'),('彭丽媛'),('批林整风参考材料'),('皮箱炸弹构造'),('皮质类固醇'),('屁眼'),('骗局'),('騙局'),('嫖鸡'),('嫖雞'),('嫖妓'),('平安夜自杀'),('平反六四'),('平银芳'),('迫害'),('迫害珐(工力)国际组织'),('迫害无辜何时了'),('迫害致死'),('菩提功'),('蒲海清'),('蒲团'),('濮洪九'),('朴鸣呼'),('曝光王'),('七宗罪'),('祁建'),('齐墨'),('企业汇总会计报表'),('企业如何建立网站'),('汽车爆炸案现场'),('汽車爆炸案現場'),('器官贩卖'),('千源'),('前期的弓足别提在理睬你'),('荨麻疹'),('钱达'),('钱国梁'),('钱其琛'),('钱运录'),('钳制网络言论'),('潜在用'),('枪货到付款'),('枪zhi'),('枪决女犯'),('枪决现场'),('枪手'),('枪淫少妇'),('枪支弹药'),('槍決女犯'),('槍決現場'),('槍支彈藥'),('强暴'),('强奸'),('强歼'),('强烈抗议中共当局'),('强卫'),('强效失意药'),('强硬发言'),('抢答器'),('抢粮记'),('抢圣火'),('羟丁酸'),('乔波室内滑雪馆'),('乔石'),('窃听'),('窃听器'),('窃听器材'),('窃听设备'),('窃听王'),('竊聽器'),('竊聽器材'),('亲共分子'),('亲共媒体'),('亲共行动'),('亲美'),('侵犯国外专利'),('钦本立'),('钦点接班人'),('秦晋'),('秦青的幸'),('沁园春血'),('青岛石化项目引发市民恐慌'),('青天白日旗'),('轻松制作原子弹'),('轻舟快讯'),('清场内幕'),('清纯少妇'),('清官团'),('清海师父'),('清海无上师'),('清华帮'),('情妇'),('情色'),('情色{5}'),('情色大片'),('情色电影'),('情色光碟'),('情色呻吟'),('情色贴图'),('情色图片'),('情色文学'),('情色小说'),('情色影片'),('庆红'),('去他娘的'),('全范围教会'),('全国二奶'),('全国两会'),('全國二奶'),('权斗'),('权力瓜分'),('权力核心'),('确保党国家机密安全规定'),('裙底走光'),('群发器'),('群發器'),('群奸'),('群交'),('群射'),('群体灭绝'),('群体灭绝罪'),('群体事件'),('群體事件'),('绕过封锁'),('热比娅'),('热血江湖'),('热站政论网'),('人工少女'),('人类罪恶论'),('人民报'),('人民报讯'),('人民币恶搞'),('人民大众时事参考'),('人民内情真相'),('人权恶棍'),('人体艺术'),('人宇特能功'),('仁吉旺姆'),('仁青加'),('仁寿警方'),('任泽民'),('日本小泉'),('日逼'),('日内瓦金融'),('日你'),('日你妈=>我是猪'),('日您'),('日批'),('日死你'),('日他'),('肉棒'),('肉洞'),('肉棍'),('肉穴{MOD}'),('肉穴**'),('肉欲'),('如何制作原子弹'),('如何做炸弹'),('如何做炸药'),('乳交'),('辱师'),('入党申请书'),('瑞士金融大学'),('萨斯病'),('赛克网'),('三班仆人派'),('三挫仑'),('三二二攻台作战'),('三二二攻台作战计划'),('三坊七巷火灾'),('三级片'),('三級片=>我該死'),('三面翻'),('三浦愛佳'),('三去车仑'),('三去车仑工力'),('三三九乘元功'),('三水法轮'),('三退'),('三唑'),('三唑仑'),('三唑仑片'),('三唑侖'),('骚逼'),('骚妇'),('骚乱'),('骚穴'),('色-情'),('色戒未删减'),('色空寺'),('色狼'),('色狼网'),('色狼網'),('色情'),('色情{5}'),('色情电视'),('色情电影'),('色情電視'),('色情服务'),('色情图片'),('色情圖片'),('色情网站'),('色情網站'),('色情小电影'),('色情小電影'),('色情小说'),('色友'),('色欲'),('杀人事件'),('沙皇李长春'),('傻B'),('傻X'),('傻逼'),('煞笔'),('山寨手机'),('汕尾事件'),('善恶有报'),('上床'),('上访军'),('上海帮'),('上海帮的黄昏'),('上海拆迁悲喜剧'),('上海孤儿院'),('上海交大'),('尚福林'),('尚勇'),('烧国旗'),('少儿不宜'),('少妇'),('少妇白洁'),('少女高潮'),('邵家健'),('舌战法庭'),('社会民主主义'),('社民党'),('射精'),('涉台政局'),('身份证生成'),('身份证生成器'),('身份證生成'),('神的教会'),('神佛'),('神通加持法'),('神州忏悔录'),('神州电影制片厂'),('神洲电影'),('沈昌人体科技'),('沈默殺手'),('沈彤'),('审查参赛者'),('渗透中共'),('升达'),('升达毕业证'),('升達'),('升天'),('生成身份证'),('生化武器'),('生命树的分叉'),('生者和死者'),('省委大门集合'),('省政府大门集合'),('圣灵重建教会'),('盛光祖'),('盛华仁'),('盛雪'),('失密'),('失身药'),('失忆药'),('失意药'),('失意藥'),('师父法身'),('师涛'),('十七大权力争霸战'),('十七人事安排'),('十七位老部长'),('石戈'),('石云生'),('石宗源'),('淫水'),('淫图'),('淫穴'),('尹庆民'),('隐私1图片'),('隱私1圖片'),('隱形鏡'),('隱形噴劑'),('印尼抢劫华人资产'),('印尼伊斯兰祈祷团'),('英语枪手'),('英语四六级答案'),('英語槍手'),('优化官员'),('优昙婆罗花'),('由喜贵'),('游行'),('游行示威'),('右派'),('幼齿'),('幼交'),('幼女'),('于大海'),('于浩成'),('于幼军'),('余英时'),('於淼林'),('於幼軍'),('舆论'),('舆论反制'),('宇明网'),('宇宙真理'),('宇宙主佛'),('宇宙最高法理'),('玉蒲团'),('玉蒲團'),('预测领导人'),('域名备案'),('欲望'),('欲望格斗'),('欲望格鬥'),('欲望之血'),('元极功'),('原谅朱镕基'),('原子弹'),('原子弹的DIY制作'),('原子弹的简易制法'),('原子弹的理论'),('原子弹制作方法'),('原子弹专家'),('袁纯清'),('袁伟民'),('援交'),('远程偷拍'),('远志明'),('遠程偷拍'),('约会强暴药'),('岳武'),('杂种'),('在家里做原子弹的方法'),('在十月'),('昝爱宗'),('脏独'),('造爱'),('造反'),('则民'),('择民'),('泽东'),('泽民'),('泽民轮功'),('贼民'),('曾道人'),('曾培炎'),('曾庆红'),('曾慶紅'),('炸弹怎么做'),('炸药'),('炸药配方'),('炸药制作方法'),('战斗条令'),('战役学纲××××'),('张伯笠'),('张钢'),('张宏堡'),('张健'),('张林'),('张丕林'),('张万年'),('张伟国'),('张五常'),('张小平'),('张昭富'),('张志清'),('張丕林'),('張五常'),('張小平'),('找工总行'),('找建总行'),('找农总行'),('找政府评理'),('赵海青'),('赵南'),('赵品潞'),('赵铁锤'),('赵晓微'),('赵紫阳'),('赵紫阳最后的机会'),('哲民'),('针孔'),('针孔摄像'),('针孔摄像机'),('侦探设备'),('真n善n忍'),('真善忍'),('真善忍'),('真空度测试仪'),('真名三退'),('真善美'),('真善忍'),('真善忍?'),('針孔攝像'),('偵探設備'),('镇压'),('镇压学生'),('争鸣论坛'),('争鸣杂志'),('征途sf'),('征途私服'),('拯救紫坪铺'),('整肃官场'),('整形美容'),('正法'),('正法时期'),('正见网'),('正见周刊'),('正念正行'),('正派民运'),('正邪大决战'),('正义党论坛'),('证件办理'),('郑义'),('政变'),('政府软弱'),('政府无能'),('政治'),('政治恶棍'),('政治反对派'),('政治犯'),('政治风波'),('政治小丑'),('政治侦察学×××××'),('支持台湾'),('支联会'),('支那'),('脂溢性皮炎'),('职业托福枪手'),('指点江山论坛'),('制作原子弹'),('制作原子弹-新闻'),('制作炸弹首先要制作炸药'),('致胡书记的公开信'),('中俄边界新约'),('中俄密约'),('中功'),('中共'),('中共暴政'),('中共当局'),('中共党魁'),('中共第六代'),('中共独裁'),('中共独枭'),('中共恶霸'),('中共监狱'),('中共警察'),('中共内斗'),('中共迫害'),('中共十七大幕前戏'),('中共太子'),('中共统治'),('中共亡'),('中共王朝'),('中共伪政权'),('中共心中最大的恐惧'),('中共政权'),('中共政坛腐败内幕'),('中共政治流氓'),('中共政治新星'),('中共中央材料'),('中共中央大换血'),('中共中央黑幕'),('中共中央文件'),('中共中央资料'),('中共专制'),('中共走狗'),('中国报禁'),('中国泛蓝联盟'),('中国复兴论坛'),('中国改革年代政治斗争'),('中国高层权力斗争'),('中国高层人事变动解读'),('中国革命党'),('中国共产党亡'),('中国孤儿院'),('中国国家领导人子女任职名单'),('中国海外腐败兵团'),('中国六四真相'),('中国民主党联合总部'),('中国社会的艾滋病'),('中国社会进步党'),('中国社会论坛'),('中国问题论坛'),('中国在统一问题上的投降主义'),('中国真实内容'),('中国政坛“明日之星”'),('中国政坛“清华帮”盛极而衰'),('中国政坛新星'),('中国政坛新星中的四大天王'),('中国政治新星'),('中国之春'),('中国猪'),('中國當局'),('中國國家領導人子女任職名單'),('中华大地思考'),('中华大众'),('中华讲清'),('中华联邦政府'),('中华民国'),('中华人民实话实说'),('中华人民正邪'),('中华时事'),('中华养生益智功'),('中华真实报道'),('中南海的权利游戏'),('中南海斗争'),('中南海高层权利斗争'),('中南海惊现东宫小朝廷'),('中南海秘闻'),('中南海内斗'),('中南海内幕'),('中南海浓云密布'),('中央文件'),('中央资料'),('钟山风雨论坛'),('重庆服装厂'),('重庆钢构'),('重庆婚纱摄影'),('重庆滤油机'),('重庆木门厂'),('重庆网站建设'),('重庆物业公司'),('周恩来忏悔'),('周恩来后悔'),('周恩来自责'),('周刊纪事'),('周容重'),('周水同志在全省计划工交工作会议上的报告'),('周天法'),('朱蒙'),('朱容基'),('朱镕基'),('朱鎔基'),('朱颜血'),('猪操'),('猪哥6合网中国网址联盟'),('猪聋畸'),('主席复活'),('专业代考'),('专政'),('专制'),('專政'),('專制'),('转{10}法{10}轮'),('转法轮'),('轉法輪'),('壮阳药'),('啄木鸟公司'),('啄木鳥公司'),('子女任职名单'),('自殘'),('自焚'),('自摸'),('自拍'),('自杀'),('自杀手册'),('自杀指南'),('自殺手冊'),('自殺指南'),('自慰'),('自由蒙古在线'),('自由民主论坛'),('自由网'),('自由西藏'),('自由西藏学生运动'),('自制手枪'),('自制手槍'),('自制原子弹'),('自主择业军官'),('宗教压迫'),('总理接班人'),('足球博彩'),('阻止中华人民共和国统'),('最高绝密新闻'),('最快报码室'),('作爱'),('坐交'),('坐台'),('做爱'),('做爱挑逗'),('做鸡'),('做雞'),('做鸭'),('做原子弹'),('假币=>'),('六合***'),('刻章***'),('法轮'),('法一轮'),('大纪元'),('真善忍'),('阴唇'),('阴户'),('强奸'),('阴茎'),('阴蒂'),('乱伦'),('手淫'),('做鸡'),('罗干'),('死亡笔记'),('三唑仑'),('麻醉药'),('麻醉乙醚'),('短信群发器'),('毛一鲜'),('黎阳平'),('PK黑社会'),('恶搞晚会'),('枪决女犯'),('投毒杀人'),('强硬发言'),('出售假币'),('监听王'),('昏药'),('侦探设备'),('麻醉钢枪'),('升达毕业证'),('手机复制'),('张小平'),('佳静安定片'),('蒙汗药粉'),('古方迷香'),('强效失意药'),('子女任职名单'),('天鹅之旅'),('盘古乐队'),('高校群体事件'),('大学骚乱'),('高校骚乱'),('催情药'),('拍肩神药'),('春药'),('枪决现场'),('出售手枪'),('麻醉枪'),('办理文凭'),('疆独'),('藏独'),('高干子弟'),('高干子女'),('枪支弹药'),('血腥图片'),('反政府'),('禁书'),('无界浏览器'),('特码'),('成人片'),('成人电影'),('三级片'),('黄海暗杀'),('政治风波'),('陈良宇'),('反民主'),('政府软弱'),('政府无能'),('共狗'),('回民暴动'),('手机复制卡'),('手机魔卡'),('暴力拆迁'),('轮暴致死'),('平安夜自杀'),('轮奸虐杀'),('暴力镇压'),('陈水扁'),('转法轮'),('达赖'),('天安门事件'),('自由亚州'),('历史上的真实故事'),('性免费电影'),('成人贴图'),('激情电影'),('色情小说'),('黄网导航'),('暴动'),('九评共产党'),('性虐待'),('赵紫阳'),('点对点裸聊'),('裸聊合法'),('松花江污染'),('汕尾事件'),('胡新宇'),('公务员工资'),('反中'),('支持台湾'),('仁寿警方'),('杀人事件'),('金伯帆'),('马加爵'),('学生暴动'),('镇压学生'),('色空寺'),('死刑过程'),('时代互联'),('时代论坛'),('时期打了几次我再次哦唯'),('实际神'),('世界基督教统一神灵协会'),('世界经济导报'),('世界十大独裁者'),('世界以利亚福音宣教会'),('世界之门'),('示威'),('事实独立'),('是束告项瓷'),('害电突网解'),('是中国社会的爱滋病'),('手qiang'),('手把手教你作原子弹'),('手把手教你做原子弹'),('手机复制'),('手机复制卡'),('手机跟踪'),('手机监听'),('手机监听器'),('手机铃声'),('手机铃声下载'),('手机魔卡'),('手机窃听'),('手机窃听器'),('手机窃听设备'),('手机游戏'),('手机追踪器'),('手機復制'),('手機跟蹤'),('手機監聽器'),('手拍肩'),('手枪'),('手枪电话如何做'),('手淫'),('兽交'),('售麻醉'),('熟女之惑'),('树亲民形象不如推动政改'),('双规'),('双十节'),('双腿间的禁地'),('爽死'),('谁是胡的接班人'),('谁是新中国'),('谁为腐败晚餐买单'),('氵去'),('氵去车仑工力'),('水扁'),('税力'),('税务总局致歉'),('睡着的武神'),('丝袜'),('丝袜写真'),('司法警官'),('司马晋'),('司马璐'),('司马璐回忆录'),('司马义铁力瓦尔地'),('司徒华'),('私彩'),('私处'),('私服'),('斯鑫良'),('絲襪寫真'),('死亡笔记'),('死亡筆記'),('死亡日志'),('死亡日誌'),('死刑过程'),('四川大地震异象揭密'),('四川独'),('四川独立'),('四川朱昱'),('四二五事件'),('四海龙女'),('四级答案'),('四六级答案'),('四人帮'),('四人帮材料'),('四小码'),('四種當'),('松花江污染'),('宋平'),('宋平顺'),('宋平順'),('宋书元'),('宋祖英'),('苏丹达'),('苏家屯集中营'),('苏军情况汇编'),('苏绍智'),('苏晓康'),('素女'),('宿命论'),('塑胶原料的流动性'),('孙悟空和雅典娜的故事'),('他妈的=>他*的'),('他媽的=>他*的'),('他娘的'),('它妈的'),('塔克拉玛干人权联合会'),('台獨'),('台独'),('台海盾牌'),('台海军刀'),('台盟'),('台湾独'),('台湾独立'),('台湾共合国'),('台湾狗'),('台湾国'),('台湾建国运动组织'),('台湾青年独立联盟'),('台湾应该独立'),('台湾有权独立'),('台湾政论区'),('台湾自由联盟'),('台湾总统'),('臺灣獨立'),('太王四神记'),('太阳圣殿教'),('太子党'),('太子黨'),('泰州文教局停售通知'),('覃志刚'),('谭力'),('探测定位器'),('探测狗'),('探測狗'),('汤光中'),('唐柏桥'),('唐家璇'),('唐捷'),('唐人电视台'),('涛哥'),('逃亡艳旅'),('陶驷驹'),('讨伐中宣部'),('特别党费'),('特价国际机票'),('特码'),('特碼'),('特务机构'),('特务民运'),('腾文生'),('滕久明'),('滕文生'),('替考托福'),('天安门'),('天安门大屠杀'),('天安门纪念基金会'),('天安门录影带'),('天安门事件'),('天安门屠杀'),('天安门一代'),('天安門'),('天鹅之旅'),('天鵝之旅'),('天府广场集会'),('天父的儿女'),('天国乐团'),('天互数据'),('天津事件'),('天伦王朝'),('天灭中'),('天灭中共'),('天怒'),('天权'),('天要灭'),('天要灭中'),('天要亡'),('天音功'),('天葬'),('田凤山'),('田纪云'),('舔奶'),('挺胡'),('通示达'),('通信维权'),('同房'),('同居社区'),('同居社區'),('同样的人权'),('童屹'),('统独'),('统独论坛'),('统一教'),('统战'),('统治术'),('捅你'),('捅我'),('偷窥'),('偷窥有罪'),('偷窺有罪'),('偷拍'),('偷情'),('投毒杀人'),('投毒殺人'),('透视镜'),('透视器'),('视眼镜'),('透视照片'),('透視器'),('透視眼鏡'),('透視照片'),('突破技术'),('突破网封索的软件'),('突破网络'),('图说文化大革命'),('涂运普'),('屠龙别记'),('屠杀'),('土制C4'),('团派'),('推翻社会主义制度'),('退h集会'),('退出共产'),('退出共产党'),('退出中共'),('退党'),('退党声明'),('退改离'),('退团'),('退团声明'),('脫衣舞'),('脱党'),('脱光'),('脱团'),('脱衣'),('脱衣舞'),('瓦解专制'),('外交论坛'),('外交与方略'),('外蒙'),('外围赌球'),('外围码'),('晚年周恩来'),('万达卫浴'),('万润南'),('读者论坛'),('万晓东'),('万言书'),('汪岷'),('亡党'),('亡共者胡'),('亡国'),('王宝森'),('王炳章'),('王策'),('王超华'),('王丹'),('王辅臣'),('王刚'),('王涵万'),('王沪宁'),('王军涛'),('王力雄'),('王瑞林'),('王润生'),('王若望'),('王维林'),('王希哲'),('王秀丽'),('王冶坪'),('王子淫传'),('网络代理'),('网络宣传的真相'),('网络自由联盟'),('网上博彩'),('网上代考'),('网上淫秽色情游戏'),('网特'),('网赚'),('微型无线隐型耳机'),('为党不为国'),('维多利亚包'),('维权份子'),('维权网'),('维吾尔人权'),('维吾尔信息社'),('維多利亞包'),('伪民运'),('伪水'),('伪装美女'),('尾行'),('偽裝美女'),('隗福临'),('卫生计划财务参考资料'),('卫星安装调试'),('卫星电视安装'),('未来的自由中国在民间'),('尉健行'),('慰安妇'),('魏东死亡'),('魏京生'),('魏新生'),('温宝宝'),('温加饱'),('温家宝'),('温家保'),('温家堡'),('温假饱'),('温下台'),('温休曾退'),('温爷爷'),('温元凯'),('温总理'),('溫家寶'),('瘟家'),('瘟家鸨'),('瘟假鸨'),('文革'),('文革××'),('文革释放'),('文化大革命'),('文件秘密'),('文字狱'),('文字獄'),('翁虹'),('瓮安'),('瓮安暴乱'),('瓮安骚乱'),('窝囊的中国'),('窝囊中国'),('我XX你'),('我操'),('我操你'),('我草'),('我的后讨伐中宣部时代'),('我的鳥羽兄弟'),('我的性启蒙'),('我靠=>我*'),('我们自杀吧'),('我日'),('我是回民'),('我虽死去'),('我所犯严重的方向线路错误的检查'),('我在黑暗中'),('乌兰木伦'),('乌云其木格'),('邬书林'),('巫毒娃娃'),('无帮国'),('无官正'),('无界'),('无界{5}'),('无界浏览'),('无界浏览器'),('无码电影'),('无码片'),('无码影'),('无毛'),('无网界'),('无网界浏览'),('无线跟踪设备'),('无线隐形耳机'),('吳邦國'),('吳瓊花問'),('吴百益'),('吴邦国'),('吴方城'),('吴官正'),('吴弘达'),('吴宏达'),('吴基传'),('吴琼花问'),('吴仁华'),('吴学灿'),('吴学璨'),('吴野渡'),('吾尔开西'),('吾尔开希'),('吾尔凯西'),('梧桐文化'),('無界'),('無網界'),('無修正'),('五不'),('五出三进'),('五大常委'),('五个部长一个省长'),('五毛党'),('五奶小青'),('五套功法'),('五星宏辉'),('伍凡'),('伍绍祖'),('武汉办証'),('武警'),('武腾兰'),('武藤兰'),('武藤蘭'),('侮辱老师'),('夕樹舞子'),('西藏'),('西藏獨立'),('西藏独'),('西藏独立'),('西藏国家民主党'),('西藏基金会'),('西藏论坛'),('西藏青年大会'),('西藏人民大起义运动'),('西藏人权与民主'),('西藏骚乱'),('西藏天葬'),('西藏网'),('西藏信息中心'),('西藏正义'),('西藏之声'),('西藏之页'),('西藏作家组织'),('西单民主墙'),('西山会议'),('吸储'),('吸儲'),('吸血莱恩'),('吸血萊恩'),('希望之声国际广播电台'),('奚国华'),('洗脑'),('洗瑙'),('下法轮'),('下届总理人选'),('下体'),('先天健康法'),('现代人的五大悲哀'),('现代艳帝'),('香港GHB水'),('香港GHB水/色/粉'),('香港彩宝必中六肖'),('香港大游行'),('香港黄大仙'),('香港惠泽社群'),('香港六和采'),('香港马会'),('香港明报'),('香港支联会'),('香港总部'),('香港总彩'),('香功'),('香烟型麻醉药'),('向巴平措'),('项怀诚'),('项小吉'),('项宗西'),('消业论'),('消业之说'),('硝化甘油'),('硝化棉'),('硝酸甘油'),('小参考'),('小电影'),('小活佛'),('小来子'),('小泉恶搞'),('小穴'),('小穴六四'),('小泽玛莉亚'),('小泽圆'),('小澤園'),('肖强'),('校花沉沦记'),('校级复员军官'),('校园静坐'),('邪党'),('邪恶'),('邪恶?'),('邪恶党徒'),('邪恶共产党'),('邪恶江泽民'),('邪教'),('谢长廷'),('谢选骏'),('谢中之'),('心藏大恶'),('辛灏年'),('新八荣八耻'),('新党'),('新观察论坛'),('新国歌'),('新华举报'),('新华内情'),('新华通论坛'),('新疆暴乱'),('新疆獨立'),('新疆独'),('新疆独立'),('新疆反恐纪实'),('新生网'),('新式军服'),('新唐人'),('新唐人电视台'),('新唐人晚会'),('新搪人电视台'),('新闻封锁'),('新闻自由导报'),('新闻总署粗暴'),('新语丝'),('新约教会'),('新中华战记'),('新诸侯'),('信用危机'),('兴华论谈'),('星火报'),('星崎未來'),('星亚网络影视公司'),('刑警'),('行房'),('邢铮'),('性爱'),('性爱电影'),('性爱教育'),('性爱文字'),('性愛'),('性愛電影'),('性福人生'),('性感'),('性感克'),('性感撲克'),('性感沙滩'),('性感沙灘'),('性高潮'),('性交'),('性交大赛'),('性教官'),('性免费电影'),('性虐待'),('性网站'),('性網站'),('性息'),('性游戏'),('胸部'),('胸罩'),('熊炎'),('熊焱'),('修炼之歌'),('徐邦秦'),('徐才厚'),('徐匡迪'),('徐水良'),('许家屯'),('玄`机'),('薛伟'),('学潮'),('学潮事件'),('学联'),('学生爱国运动正名'),('学生暴动'),('学生静坐'),('学生妹'),('学生信仰'),('学生运动'),('学院+暴动'),('暴动'),('学运'),('学自联'),('雪山狮子'),('血溅人民天堂'),('血卡'),('血色京机'),('血色京畿'),('血色黎明'),('血腥图片'),('血腥圖片'),('寻找林昭的灵魂'),('循环轮回论'),('鸦片液'),('鸦片渣'),('亚热'),('亚太正悟网'),('亚洲床上色情'),('亚洲美女总'),('亚洲自由电台'),('亚洲自由之声'),('严家其'),('严家祺'),('言禁'),('盐雾试验箱'),('阎明复'),('颜射'),('艳舞'),('燕玲论坛'),('燕南评论'),('央视内部晚会'),('阳具'),('阳痿'),('杨怀安'),('杨建利'),('杨思敏'),('杨巍'),('杨月清'),('杨周'),('楊思敏'),('养殖户的求救书'),('姚月谦'),('搖頭丸'),('摇头丸'),('摇头玩'),('耶稣基督血水圣灵全备福音布道团'),('业力回报'),('业力轮'),('叶子楣'),('夜半加税'),('夜话紫禁城'),('夜激情'),('夜勤病栋'),('夜总会'),('夜總會'),('液压马达'),('一般炸药制作'),('一本道'),('一党'),('一党独裁'),('一党专政'),('一党专制'),('一黨'),('一个人的奥林匹克'),('一级黄电视'),('一军两策'),('一码中特'),('一四我周容重题工无亮'),('一通功'),('一夜情'),('一夜情迷奸'),('一中一台'),('一字解特码'),('伊波拉瘟疫'),('伊東'),('伊斯兰运动'),('伊扎布特'),('宜昌当阳县级市长'),('遗忘药'),('乙醚'),('以身护法'),('以血护法'),('义解'),('亦凡'),('异氟烷'),('异见人士'),('异议人士'),('抑制剂'),('易丹轩'),('易趣在线购物'),('易志熹'),('阴唇'),('阴道'),('阴道被捅'),('阴蒂'),('阴户'),('阴茎'),('阴胫'),('阴毛'),('阴毛小穴'),('阴门'),('阴囊'),('阴水'),('陰蒂'),('淫虫'),('淫蟲'),('淫荡'),('淫蕩'),('淫奸'),('淫间道'),('淫浪'),('淫乱'),('淫乱电影'),('淫亂'),('淫亂電影'),('淫妹'),('淫靡'),('淫魔'),('淫魔舞会'),('淫母'),('淫女'),('淫情女老师'),('淫兽学园'),('淫书'),('淫書'),('淫术炼金士'),('拉登'),('美女视频'),('成人交友'),('日本小泉'),('小泉恶搞'),('火药制作'),('第十六次代表'),('幼女'),('蒋彦永'),('多党执政'),('玉蒲团'),('车牌反光'),('喷剂'),('周容重'),('毛主席复活'),('针孔摄像机'),('绕过封锁'),('践踏中国女性'),('短信猫'),('共产党的末日'),('罢工'),('人体艺术'),('无界浏览'),('情色光碟'),('情色呻吟'),('手追踪器'),('波拉瘟疫'),('传真群发'),('革命无罪'),('二奶大奖'),('军火'),('代开发票'),('干扰器'),('美女做愛'),('快操我'),('裙底走光'),('乖乖药'),('一级黄电视'),('无码片'),('女优片'),('亚洲美女总'),('太王四神记'),('仿真枪'),('针孔'),('代办证件'),('江泽民瑞士'),('电车之狼'),('暴力冲突'),('手机监听'),('主席复活'),('步枪'),('麻醉枪'),('猎枪'),('成人小说'),('裸体'),('激情图片'),('迷魂药激情'),('裙底走光'),('女优'),('亚洲床上色情'),('阴毛小穴'),('无码影'),('淫妹'),('流水穴'),('激情文学'),('达赖喇嘛'),('达赖'),('达赖领奖'),('朱蒙'),('三坊七巷火灾'),('四级答案'),('六级答案'),('买答案'),('枪手'),('四六级答案'),('等级答案'),('养殖户的求救书'),('迷魂'),('迷情药'),('操'),('假币'),('成人网站'),('暗杀'),('精子'),('辱骂'),('监狱'),('群体斗殴'),('群殴'),('去死'),('奶奶的'),('客服'),('系统'),('官方'),('爹'),('娘'),('爸'),('妈'),('父'),('母'),('法轮大法'),('法轮大法'),('人弹'),('人弹'),('司法部'),('\\'),('\\\''),('\\\\'),('♩');
/*!40000 ALTER TABLE `words` ENABLE KEYS */;
UNLOCK TABLES;
/*!40103 SET TIME_ZONE=@OLD_TIME_ZONE */;

/*!40101 SET SQL_MODE=@OLD_SQL_MODE */;
/*!40014 SET FOREIGN_KEY_CHECKS=@OLD_FOREIGN_KEY_CHECKS */;
/*!40014 SET UNIQUE_CHECKS=@OLD_UNIQUE_CHECKS */;
/*!40101 SET CHARACTER_SET_CLIENT=@OLD_CHARACTER_SET_CLIENT */;
/*!40101 SET CHARACTER_SET_RESULTS=@OLD_CHARACTER_SET_RESULTS */;
/*!40101 SET COLLATION_CONNECTION=@OLD_COLLATION_CONNECTION */;
/*!40111 SET SQL_NOTES=@OLD_SQL_NOTES */;

-- Dump completed on 2019-01-09 13:01:23

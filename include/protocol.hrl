%%%-------------------------------------------------------------------
%%% @doc
%%% protocol define
%%% @end
%%%-------------------------------------------------------------------

%%%-------------------------------------------------------------------
%%% @start Account
-define(PROTOCOL_ACCOUNT_HEARTBEAT,                   10000).   %% 心跳包
-define(PROTOCOL_ACCOUNT_LOGIN,                       10001).   %% 登录
-define(PROTOCOL_ACCOUNT_LOGOUT,                      10002).   %% 退出
-define(PROTOCOL_ACCOUNT_CREATE,                      10003).   %% 创建角色
-define(PROTOCOL_ACCOUNT_QUERY,                       10003).   %% 查询角色
%%% @end
%%%-------------------------------------------------------------------

%%%-------------------------------------------------------------------
%%% @start role
-define(PROTOCOL_ROLE,                                10101).  %% 获取用户信息
-define(PROTOCOL_ASSET,                               10201).  %% 个人货币信息
-define(PROTOCOL_VIP,                                 10301).  %% 个人VIP信息
%%% @end
%%%-------------------------------------------------------------------

%%%-------------------------------------------------------------------
%%% @start Item
-define(PROTOCOL_ITEM,                                11101).  %% 物品列表
-define(PROTOCOL_BAG,                                 11102).  %% 背包列表
-define(PROTOCOL_STORE,                               11103).  %% 仓库列表
-define(PROTOCOL_ITEM_DELETE,                         11104).  %% 道具使用
-define(PROTOCOL_ITEM_EQUIP,                          11105).  %% 穿戴装备
-define(PROTOCOL_ITEM_USE,                            11106).  %% 道具使用
%%% @end
%%%-------------------------------------------------------------------

%%%-------------------------------------------------------------------
%%% @start Quest
-define(PROTOCOL_QUEST,                               11201).  %% 任务列表
-define(PROTOCOL_QUEST_ACCEPT,                        11202).  %% 接受任务
-define(PROTOCOL_QUEST_SUBMIT,                        11203).  %% 提交任务
%%% @end
%%%-------------------------------------------------------------------

%%%-------------------------------------------------------------------
%%% @start Shop
-define(PROTOCOL_SHOP,                                11301).  %% 商店
-define(PROTOCOL_SHOP_BUY,                            11302).  %% 商店购买
%%% @end
%%%-------------------------------------------------------------------

%%%-------------------------------------------------------------------
%%% @start Mail
-define(PROTOCOL_MAIL,                                11401).  %% 邮件列表
-define(PROTOCOL_MAIL_READ,                           11402).  %% 读取邮件
-define(PROTOCOL_MAIL_RECEIVE_ATTACHMENT,             11403).  %% 领取附件
%%% @end
%%%-------------------------------------------------------------------

%%%-------------------------------------------------------------------
%%% @start Friend
-define(PROTOCOL_FRIEND,                              11501).  %% 好友
-define(PROTOCOL_FRIEND_APPLY,                        11502).  %% 好友申请
-define(PROTOCOL_FRIEND_AGREE,                        11503).  %% 同意申请
-define(PROTOCOL_FRIEND_DELETE,                       11504).  %% 删除好友
%%% @end
%%%-------------------------------------------------------------------

%%%-------------------------------------------------------------------
%%% @start Chat
-define(PROTOCOL_CHAT_WORLD,                          11601).  %% 世界聊天
-define(PROTOCOL_CHAT_GUILD,                          11602).  %% 公会聊天
-define(PROTOCOL_CHAT_SCENE,                          11603).  %% 场景聊天
-define(PROTOCOL_CHAT_TEAM,                           11604).  %% 队伍聊天
-define(PROTOCOL_CHAT_PRIVATE,                        11605).  %% 个人私聊
-define(PROTOCOL_CHAT_PRIVATE_OFFLINE,                11606).  %% 离线消息
%%% @end
%%%-------------------------------------------------------------------

%%%-------------------------------------------------------------------
%%% @start Skill
-define(PROTOCOL_SKILL,                               11701).  %% 技能
%%% @end
%%%-------------------------------------------------------------------

%%%-------------------------------------------------------------------
%%% @start Buff
-define(PROTOCOL_BUFF,                                11801).  %% Buff
-define(PROTOCOL_BUFF_DELETE,                         11802).  %% Buff
%%% @end
%%%-------------------------------------------------------------------

%%%-------------------------------------------------------------------
%%% @start Title
-define(PROTOCOL_TITLE,                               11901).  %% 称号
-define(PROTOCOL_TITLE_DELETE,                        11902).  %% 删除称号
%%% @end
%%%-------------------------------------------------------------------

%%%-------------------------------------------------------------------
%%% @start Welfare
-define(PROTOCOL_KEY_AWARD,                           15001).  %% 领取兑换码奖励
-define(PROTOCOL_LUCKY_MONEY_LIST,                    15002).  %% 红包列表
-define(PROTOCOL_RECEIVE_LUCKY_MONEY,                 15003).  %% 领取红包
%%% @end
%%%-------------------------------------------------------------------

%%%-------------------------------------------------------------------
%%% @start Activity
-define(PROTOCOL_ACTIVITY,                            16001).  %% 活动
%%% @end
%%%-------------------------------------------------------------------

%%%-------------------------------------------------------------------
%%% @start Auction
-define(PROTOCOL_AUCTION_LIST,                        16101).  %% 拍卖列表
-define(PROTOCOL_AUCTION_BID,                         16102).  %% 参与拍卖
%%% @end
%%%-------------------------------------------------------------------

%%%-------------------------------------------------------------------
%%% @start Dungeon
-define(PROTOCOL_DUNGEON,                             17001).  %% 副本
-define(PROTOCOL_DUNGEON_ENTER,                       17002).  %% 进入副本
-define(PROTOCOL_DUNGEON_START,                       17003).  %% 副本开始
-define(PROTOCOL_DUNGEON_OVER,                        17004).  %% 副本结束
%%% @end
%%%-------------------------------------------------------------------

%%%-------------------------------------------------------------------
%%% @start War
-define(PROTOCOL_BOSS_BATTLE,                         18001).  %% 挑战Boss
%%% @end
%%%-------------------------------------------------------------------

%%%-------------------------------------------------------------------
%%% @start Rank
-define(PROTOCOL_RANK,                                19000).  %% 排行榜
-define(PROTOCOL_RANK_CENTER,                         19100).  %% 排行榜
-define(PROTOCOL_RANK_WORLD,                          19200).  %% 排行榜
%%% @end
%%%-------------------------------------------------------------------

%%%-------------------------------------------------------------------
%%% @start Map
-define(PROTOCOL_MAP,                                 20001).  %% 地图信息
-define(PROTOCOL_MAP_SELF,                            20002).  %% 自身信息
-define(PROTOCOL_MAP_FIGHTER,                         20003).  %% 战斗对象列表
-define(PROTOCOL_MAP_FIGHTER_MOVE,                    20004).  %% 战斗对象移动
-define(PROTOCOL_MAP_FIGHTER_LEAVE,                   20005).  %% 战斗对象离开
-define(PROTOCOL_MAP_ROLE_MOVE,                       20006).  %% 玩家移动
-define(PROTOCOL_MAP_FIGHTER_ATTACK,                  20007).  %% 发起战斗
%%% @end
%%%-------------------------------------------------------------------

%%%-------------------------------------------------------------------
%%% @start Guild
-define(PROTOCOL_GUILD_LIST,                          30101).  %% 公会列表
-define(PROTOCOL_GUILD_ROLE_LIST,                     30102).  %% 成员列表
-define(PROTOCOL_GUILD_APPLY_LIST,                    30103).  %% 申请列表
-define(PROTOCOL_GUILD_SELF_GUILD,                    30104).  %% 自身公会信息
-define(PROTOCOL_GUILD_SELF_ROLE,                     30105).  %% 自身成员信息
-define(PROTOCOL_GUILD_SELF_APPLY,                    30106).  %% 自身申请信息
-define(PROTOCOL_GUILD_CREATE,                        30107).  %% 创建公会
-define(PROTOCOL_GUILD_APPLY,                         30108).  %% 申请加入
-define(PROTOCOL_GUILD_CANCEL_APPLY,                  30109).  %% 取消申请
-define(PROTOCOL_GUILD_CANCEL_ALL_APPLY,              30110).  %% 取消申请
-define(PROTOCOL_GUILD_APPROVE_APPLY,                 30111).  %% 允许申请
-define(PROTOCOL_GUILD_APPROVE_ALL_APPLY,             30112).  %% 允许全部申请
-define(PROTOCOL_GUILD_REJECT_APPLY,                  30113).  %% 拒绝申请
-define(PROTOCOL_GUILD_REJECT_ALL_APPLY,              30114).  %% 拒绝全部申请
-define(PROTOCOL_GUILD_LEAVE,                         30115).  %% 离开公会
-define(PROTOCOL_GUILD_DISMISS,                       30116).  %% 解散公会
-define(PROTOCOL_GUILD_KICK,                          30117).  %% 踢出成员
-define(PROTOCOL_GUILD_UPDATE_JOB,                    30118).  %% 更改职位
-define(PROTOCOL_GUILD_UPGRADE_LEVEL,                 30119).  %% 升级公会
-define(PROTOCOL_GUILD_DEVOTE,                        30120).  %% 公会贡献
%%% @end
%%%-------------------------------------------------------------------

%%%-------------------------------------------------------------------
%%% @start Notice
-define(PROTOCOL_NOTICE,                              50001).  %% 系统公告
-define(PROTOCOL_NOTICE_RED_DOT,                      51002).  %% 红点消息
-define(PROTOCOL_SERVER_TIME,                         52001).  %% 服务器时间
-define(PROTOCOL_CLIENT_ERROR,                        53001).  %% 客服端错误记录日志
-define(PROTOCOL_TIP_OFF,                             54001).  %% 角色举报
-define(PROTOCOL_CHAT_CONTROL,                        55001).  %% 聊天监控
%%% @end
%%%-------------------------------------------------------------------

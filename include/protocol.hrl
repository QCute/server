
%%%===================================================================
%%% account
%%%===================================================================

-define(PROTOCOL_ACCOUNT_HEARTBEAT,                   10000).
-define(PROTOCOL_ACCOUNT_QUERY,                       10001).
-define(PROTOCOL_ACCOUNT_CREATE,                      10002).
-define(PROTOCOL_ACCOUNT_LOGIN,                       10003).
-define(PROTOCOL_ACCOUNT_LOGOUT,                      10004).
-define(PROTOCOL_ACCOUNT_HANDLE_PACKET,               0).

%%%===================================================================
%%% role
%%%===================================================================

-define(PROTOCOL_ROLE_QUERY,                          10101).
-define(PROTOCOL_ROLE_ASSET_QUERY,                    10102).
-define(PROTOCOL_ROLE_VIP_QUERY,                      10103).

%%%===================================================================
%%% item
%%%===================================================================

-define(PROTOCOL_ITEM_QUERY_ITEM,                     11101).
-define(PROTOCOL_ITEM_QUERY_BAG,                      11102).
-define(PROTOCOL_ITEM_QUERY_STORE,                    11103).
-define(PROTOCOL_ITEM_DELETE,                         11104).
-define(PROTOCOL_ITEM_USE,                            11106).

%%%===================================================================
%%% task
%%%===================================================================

-define(PROTOCOL_TASK_QUERY,                          11201).
-define(PROTOCOL_TASK_ACCEPT,                         11202).
-define(PROTOCOL_TASK_SUBMIT,                         11203).

%%%===================================================================
%%% shop
%%%===================================================================

-define(PROTOCOL_SHOP_QUERY,                          11301).
-define(PROTOCOL_SHOP_BUY,                            11302).

%%%===================================================================
%%% mail
%%%===================================================================

-define(PROTOCOL_MAIL_QUERY,                          11401).
-define(PROTOCOL_MAIL_READ,                           11402).
-define(PROTOCOL_MAIL_RECEIVE_ATTACHMENT,             11403).
-define(PROTOCOL_MAIL_DELETE,                         11404).

%%%===================================================================
%%% friend
%%%===================================================================

-define(PROTOCOL_FRIEND_QUERY,                        11501).
-define(PROTOCOL_FRIEND_APPLY,                        11502).
-define(PROTOCOL_FRIEND_AGREE,                        11503).
-define(PROTOCOL_FRIEND_DELETE,                       11504).
-define(PROTOCOL_FRIEND_BLOCK,                        11505).
-define(PROTOCOL_FRIEND_CANCEL_BLOCK,                 11506).

%%%===================================================================
%%% chat
%%%===================================================================

-define(PROTOCOL_CHAT_GET_SYSTEM_LIST,                11602).
-define(PROTOCOL_CHAT_WORLD,                          11603).
-define(PROTOCOL_CHAT_GET_WORLD_LIST,                 11604).
-define(PROTOCOL_CHAT_GUILD,                          11605).
-define(PROTOCOL_CHAT_GET_GUILD_LIST,                 11606).
-define(PROTOCOL_CHAT_PRIVATE,                        11607).
-define(PROTOCOL_CHAT_GET_PRIVATE_LIST,               11608).

%%%===================================================================
%%% skill
%%%===================================================================

-define(PROTOCOL_SKILL_QUERY,                         11701).
-define(PROTOCOL_SKILL_LEARN,                         11702).

%%%===================================================================
%%% buff
%%%===================================================================

-define(PROTOCOL_BUFF_QUERY,                          11801).
-define(PROTOCOL_BUFF_DELETE,                         11802).

%%%===================================================================
%%% title
%%%===================================================================

-define(PROTOCOL_TITLE_QUERY,                         11901).
-define(PROTOCOL_TITLE_DELETE,                        11902).

%%%===================================================================
%%% fashion
%%%===================================================================

-define(PROTOCOL_FASHION_QUERY,                       12001).
-define(PROTOCOL_FASHION_DELETE,                      12002).

%%%===================================================================
%%% bubble
%%%===================================================================

-define(PROTOCOL_BUBBLE_QUERY,                        12101).
-define(PROTOCOL_BUBBLE_DELETE,                       12102).

%%%===================================================================
%%% achievement
%%%===================================================================

-define(PROTOCOL_ACHIEVEMENT_QUERY_COUNT,             12301).
-define(PROTOCOL_ACHIEVEMENT_QUERY,                   12202).
-define(PROTOCOL_ACHIEVEMENT_AWARD,                   12203).

%%%===================================================================
%%% daily
%%%===================================================================

-define(PROTOCOL_DAILY_QUERY_COUNT,                   12301).
-define(PROTOCOL_DAILY_QUERY,                         12302).
-define(PROTOCOL_DAILY_AWARD,                         12303).
-define(PROTOCOL_DAILY_AWARD_ACTIVE,                  12304).

%%%===================================================================
%%% welfare
%%%===================================================================

-define(PROTOCOL_WELFARE_SIGN,                        15001).
-define(PROTOCOL_WELFARE_AWARD,                       15002).
-define(PROTOCOL_WELFARE_QUERY_LUCKY_MONEY,           15003).
-define(PROTOCOL_WELFARE_RECEIVE_LUCKY_MONEY,         15004).
-define(PROTOCOL_WELFARE_LUCKY_MONEY_COMING,          15005).

%%%===================================================================
%%% auction
%%%===================================================================

-define(PROTOCOL_AUCTION_QUERY,                       16101).
-define(PROTOCOL_AUCTION_BID,                         16102).

%%%===================================================================
%%% dungeon
%%%===================================================================

-define(PROTOCOL_DUNGEON_QUERY,                       17001).
-define(PROTOCOL_DUNGEON_ENTER,                       17002).
-define(PROTOCOL_DUNGEON_START,                       17003).
-define(PROTOCOL_DUNGEON_OVER,                        17004).
-define(PROTOCOL_DUNGEON_INSPIRE,                     17005).

%%%===================================================================
%%% war
%%%===================================================================

-define(PROTOCOL_WAR_BATTLE,                          18001).

%%%===================================================================
%%% map
%%%===================================================================

-define(PROTOCOL_MAP_QUERY,                           20001).
-define(PROTOCOL_MAP_SELF,                            20002).
-define(PROTOCOL_MAP_FIGHTER,                         20003).
-define(PROTOCOL_MAP_FIGHTER_MOVE,                    20004).
-define(PROTOCOL_MAP_FIGHTER_LEAVE,                   20005).
-define(PROTOCOL_MAP_MOVE,                            20006).
-define(PROTOCOL_MAP_ATTACK,                          20007).

%%%===================================================================
%%% guild
%%%===================================================================

-define(PROTOCOL_GUILD_QUERY_GUILD,                   30101).
-define(PROTOCOL_GUILD_QUERY_ROLE,                    30102).
-define(PROTOCOL_GUILD_QUERY_APPLY,                   30103).
-define(PROTOCOL_GUILD_QUERY_SELF_GUILD,              30104).
-define(PROTOCOL_GUILD_QUERY_SELF_ROLE,               30105).
-define(PROTOCOL_GUILD_QUERY_SELF_APPLY,              30106).
-define(PROTOCOL_GUILD_CREATE,                        30107).
-define(PROTOCOL_GUILD_APPLY,                         30108).
-define(PROTOCOL_GUILD_CANCEL_APPLY,                  30109).
-define(PROTOCOL_GUILD_CANCEL_ALL_APPLY,              30110).
-define(PROTOCOL_GUILD_APPROVE_APPLY,                 30111).
-define(PROTOCOL_GUILD_APPROVE_ALL_APPLY,             30112).
-define(PROTOCOL_GUILD_REJECT_APPLY,                  30113).
-define(PROTOCOL_GUILD_REJECT_ALL_APPLY,              30114).
-define(PROTOCOL_GUILD_LEAVE,                         30115).
-define(PROTOCOL_GUILD_DISMISS,                       30116).
-define(PROTOCOL_GUILD_KICK,                          30117).
-define(PROTOCOL_GUILD_UPDATE_JOB,                    30118).
-define(PROTOCOL_GUILD_UPGRADE_LEVEL,                 30119).
-define(PROTOCOL_GUILD_CHANGE_NOTICE,                 30120).

%%%===================================================================
%%% notice
%%%===================================================================

-define(PROTOCOL_NOTICE_QUERY,                        50001).
-define(PROTOCOL_NOTICE_BROADCAST,                    50002).

%%%===================================================================
%%% cheat
%%%===================================================================

-define(PROTOCOL_CHEAT_QUERY,                         60001).
-define(PROTOCOL_CHEAT_CHEAT,                         60002).


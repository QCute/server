%%%-------------------------------------------------------------------
%%% @doc
%%% protocol define
%%% @end
%%%-------------------------------------------------------------------

%%%-------------------------------------------------------------------
%%% @start 账户信息
-define(PP_ACCOUNT_LOGIN,                       10001).        %% 登陆
-define(PP_ACCOUNT_USER_LIST,                   10002).        %% 返回角色列表
-define(PP_ACCOUNT_SELECT,                      10003).        %% 选择角色进入游戏, 返回人物信息
-define(PP_ACCOUNT_CREATE,                      10004).        %% 创建角色
-define(PP_ACCOUNT_HEARTBEAT,                   10006).        %% 心跳包
-define(PP_ACCOUNT_DELETE_ROLE,                 10007).        %% 删除角色
-define(PP_ACCOUNT_LOST_CONNECT,                10012).        %% 帐号断开连接
-define(PP_LOGIN,                               10013).        %% 用户登陆
-define(PP_GM_CHAT_LOGIN,                       10021).        %% 用户登陆
%%% @end
%%%-------------------------------------------------------------------



%%%-------------------------------------------------------------------
%%% @start 人物信息
-define(PP_QUERY_SELF_OTHER,                    20002).        %% 获取用户信息
-define(PP_UPDATE_SELF_INFO,                    20003).        %% 个人货币信息
-define(PP_UPDATE_USER_INFO,                    20004).        %% 更新玩家信息
-define(PP_ONLINE_DAY_INFO,                     20005).        %% 总登录天数
-define(PP_PLAYER_MOVE, 						12010).		   %% 玩家移动
%%% @end
%%%-------------------------------------------------------------------



%%%-------------------------------------------------------------------
%%% @start 物品信息
-define(PP_ITEM_INFO,                           21006).        %% 物品更新
-define(PP_ITEM_UPDATE,                         21006).        %% 物品更新
-define(PP_ITEM_EQUIP_ITEM,                     21007).        %% 穿戴装备
-define(PP_ITEM_USE,                            21012).        %% 道具使用
-define(PP_ITEM_COMPOSE,                        21028).        %% 道具合成
%%% @end
%%%-------------------------------------------------------------------



%%%-------------------------------------------------------------------
%%% @start 系统信息
-define(PP_SYS_MESS,                            30005).        %% 系统消息
-define(PP_SERVER_TIME,                         30001).        %% 服务器时间
-define(PP_SERVER_TIME_DEBUG,                   30008).        %% 时间debug
-define(PP_NOTICE_MESSAGE,                      30010).        %% 系统信息(公告/提示等)

-define(PP_GM_SEND_MESSAGE,                     30030).        %% GM发送反馈信息
-define(PP_GM_CONTROL,                          30040).        %% GM控制信息
-define(PP_GM_MONITOR_CHAT,                     30041).        %% 聊天监控
-define(PP_GM_TEST_PACK,                        30051).        %% 客户端包测试
-define(PP_GM_TICK_INFO,                        30052).        %% 后台踢所有玩家退出客户端
-define(PP_GM_PLAYER_COMPLAIN,                  30061).        %% 玩家举报
-define(PP_GM_CLIENT_ERROR,                     30081).        %% 客服端错误记录
%%% @end
%%%-------------------------------------------------------------------
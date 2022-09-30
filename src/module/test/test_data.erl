-module(test_data).
-export([zhCN/1]).
-export([text/1]).
-export([type/1]).
-export([level/0]).
-export([type_list/0]).
-export([min_max_level/0]).
-export([text_count/0]).
-export([max_text/0]).
-export([ref/2]).
-export([ref_range/2]).
-export([get_level_by_exp_asc/1]).
-export([get/1]).


-spec zhCN(Key :: atom()) -> ZhCN :: binary() | Default :: [].
zhCN(account_create_max) ->
    <<"服务器角色数量已达到上限"/utf8>>;
zhCN(account_login_forbidden) ->
    <<"账号禁止登录"/utf8>>;
zhCN(account_logout) ->
    <<"登出"/utf8>>;
zhCN(account_not_found) ->
    <<"没有找到此账号"/utf8>>;
zhCN(account_permission_denied) ->
    <<"账号权限不足"/utf8>>;
zhCN(achievement_not_completed) ->
    <<"成就未完成"/utf8>>;
zhCN(achievement_not_found) ->
    <<"没有找到此成就"/utf8>>;
zhCN(asset_copper_not_enough) ->
    <<"铜币不足"/utf8>>;
zhCN(asset_gold_not_enough) ->
    <<"金币不足"/utf8>>;
zhCN(asset_not_enough) ->
    <<"资产不足"/utf8>>;
zhCN(asset_silver_not_enough) ->
    <<"银币不足"/utf8>>;
zhCN(auction_not_found) ->
    <<"没有找到此拍品"/utf8>>;
zhCN(auction_price_changed) ->
    <<"拍品价格已发生变化"/utf8>>;
zhCN(award_already_received) ->
    <<"奖励已经领取过了"/utf8>>;
zhCN(award_error) ->
    <<"奖励领取错误"/utf8>>;
zhCN(award_pre_not_received) ->
    <<"前置奖励未领取"/utf8>>;
zhCN(boss_dead) ->
    <<"BOSS已经死亡"/utf8>>;
zhCN(boss_not_found) ->
    <<"没有找到此Boss"/utf8>>;
zhCN(bubble_duplicated) ->
    <<"气泡重复"/utf8>>;
zhCN(buff_duplicated) ->
    <<"Buff重复"/utf8>>;
zhCN(chat_cannot_with_self) ->
    <<"不能和自己聊天"/utf8>>;
zhCN(chat_too_frequently) ->
    <<"发言太频繁"/utf8>>;
zhCN(cheat_command_not_found) ->
    <<"没有找到此命令"/utf8>>;
zhCN(condition_not_met) ->
    <<"条件不满足"/utf8>>;
zhCN(configure_not_found) ->
    <<"没有找到此配置"/utf8>>;
zhCN(daily_not_completed) ->
    <<"日常任务未完成"/utf8>>;
zhCN(daily_score_not_enough) ->
    <<"日常活跃度不足"/utf8>>;
zhCN(dungeon_not_found) ->
    <<"没有找到此副本"/utf8>>;
zhCN(dungeon_today_number_limit) ->
    <<"今日进入次数已达到上限"/utf8>>;
zhCN(fashion_duplicated) ->
    <<"时装重复"/utf8>>;
zhCN(friend_apply_not_found) ->
    <<"没有找到此好友的申请"/utf8>>;
zhCN(friend_in_apply) ->
    <<"对方已在申请列表中"/utf8>>;
zhCN(friend_in_be_block) ->
    <<"你已被对方拉黑"/utf8>>;
zhCN(friend_in_block) ->
    <<"对方已在黑名单中"/utf8>>;
zhCN(friend_in_list) ->
    <<"对方已在好友列表中"/utf8>>;
zhCN(friend_level_not_met) ->
    <<"对方好友等级不满足"/utf8>>;
zhCN(friend_not_found) ->
    <<"没有找到此好友"/utf8>>;
zhCN(friend_number_max) ->
    <<"好友数量达到上限"/utf8>>;
zhCN(guild_already_joined) ->
    <<"你已经加入过公会了"/utf8>>;
zhCN(guild_apply_frequently) ->
    <<"公会申请太频繁"/utf8>>;
zhCN(guild_apply_not_found) ->
    <<"没有找到此申请"/utf8>>;
zhCN(guild_cannot_kick_self) ->
    <<"不可剔除自己"/utf8>>;
zhCN(guild_cannot_update_self) ->
    <<"不可升级自己"/utf8>>;
zhCN(guild_create_frequently) ->
    <<"公会创建太频繁"/utf8>>;
zhCN(guild_member_not_found) ->
    <<"没有找到此成员"/utf8>>;
zhCN(guild_member_number_limit) ->
    <<"公会成员数量已达到上限"/utf8>>;
zhCN(guild_not_found) ->
    <<"没有找到此商会"/utf8>>;
zhCN(guild_not_joined) ->
    <<"没有加入公会"/utf8>>;
zhCN(guild_permission_denied) ->
    <<"公会权限不足"/utf8>>;
zhCN(invalid_classes) ->
    <<"无效职业"/utf8>>;
zhCN(invalid_item) ->
    <<"无效物品"/utf8>>;
zhCN(invalid_number) ->
    <<"无效数量"/utf8>>;
zhCN(invalid_sex) ->
    <<"无效性别"/utf8>>;
zhCN(invalid_type) ->
    <<"无效类型"/utf8>>;
zhCN(item_bag_full) ->
    <<"背包已满"/utf8>>;
zhCN(item_cannot_use_directly) ->
    <<"物品不能直接使用"/utf8>>;
zhCN(item_not_enough) ->
    <<"物品不足"/utf8>>;
zhCN(item_use_number_max) ->
    <<"使用个数超过单次使用上限"/utf8>>;
zhCN(key_already_activated) ->
    <<"激活码已激活过"/utf8>>;
zhCN(key_already_active) ->
    <<"此兑换码已经兑换过了"/utf8>>;
zhCN(level_not_met) ->
    <<"等级不满足"/utf8>>;
zhCN(lucky_money_already_received) ->
    <<"红包已领取过"/utf8>>;
zhCN(lucky_money_expired) ->
    <<"红包已过期"/utf8>>;
zhCN(lucky_money_not_found) ->
    <<"没有找到此红包"/utf8>>;
zhCN(mail_already_read) ->
    <<"邮件已阅读过"/utf8>>;
zhCN(mail_attachment_empty) ->
    <<"附件为空"/utf8>>;
zhCN(mail_not_found) ->
    <<"没有找到此邮件"/utf8>>;
zhCN(mail_text_add_item_content) ->
    <<"您的背包已满，新增的道具已经放到了邮件里，请注意查收。"/utf8>>;
zhCN(mail_text_add_item_title) ->
    <<"背包已满"/utf8>>;
zhCN(mail_text_auction_income_content) ->
    <<"您的拍卖收入分成。"/utf8>>;
zhCN(mail_text_auction_income_title) ->
    <<"拍卖收入"/utf8>>;
zhCN(mail_text_auction_success_content) ->
    <<"您的拍卖物品，请注意查收。"/utf8>>;
zhCN(mail_text_auction_success_title) ->
    <<"拍卖成功"/utf8>>;
zhCN(name_duplicate) ->
    <<"名字重复"/utf8>>;
zhCN(name_duplicated) ->
    <<"名字重复"/utf8>>;
zhCN(name_length) ->
    <<"名字长度不对"/utf8>>;
zhCN(name_length_invalid) ->
    <<"名字长度无效"/utf8>>;
zhCN(name_not_utf8_charset) ->
    <<"名字非UTF8字符"/utf8>>;
zhCN(name_sensitive) ->
    <<"名字敏感"/utf8>>;
zhCN(notice_text_guild_create) ->
    <<"~s创建公会"/utf8>>;
zhCN(notice_text_level_upgrade) ->
    <<"恭喜"/utf8>>;
zhCN(notice_text_vip_upgrade) ->
    <<"恭喜"/utf8>>;
zhCN(packet_heartbeat_too_fast) ->
    <<"心跳包速度过快"/utf8>>;
zhCN(packet_too_fast) ->
    <<"包速度过快"/utf8>>;
zhCN(role_cannot_change_same_classes) ->
    <<"职业不能相同"/utf8>>;
zhCN(role_cannot_change_same_name) ->
    <<"名字不能相同"/utf8>>;
zhCN(role_cannot_change_same_sex) ->
    <<"性别不能相同"/utf8>>;
zhCN(server_create_forbidden) ->
    <<"服务器禁止创建角色"/utf8>>;
zhCN(server_id_mismatch) ->
    <<"服务器ID不匹配"/utf8>>;
zhCN(server_login_forbidden) ->
    <<"服务器禁止登录"/utf8>>;
zhCN(server_update) ->
    <<"服务器更新"/utf8>>;
zhCN(shop_buy_num_max) ->
    <<"已达到购买数量上限"/utf8>>;
zhCN(signed_already) ->
    <<"已经签到过了"/utf8>>;
zhCN(task_already_submitted) ->
    <<"任务已提交"/utf8>>;
zhCN(task_not_completed) ->
    <<"任务还没完成"/utf8>>;
zhCN(task_not_found) ->
    <<"没有找到此任务"/utf8>>;
zhCN(task_not_next) ->
    <<"请按顺序完成"/utf8>>;
zhCN(task_pre_not_completed) ->
    <<"前置任务还没完成"/utf8>>;
zhCN(timeout) ->
    <<"请求超时"/utf8>>;
zhCN(title_duplicated) ->
    <<"称号重复"/utf8>>;
zhCN(user_offline) ->
    <<"对方不在线"/utf8>>;
zhCN(vip_level_not_met) ->
    <<"Vip等级不满足"/utf8>>;
zhCN(_) ->
    [].


-spec text(Key :: atom()) -> Text :: {Key :: atom(), ZhCN :: binary(), Description :: binary()} | Default :: [].
text(account_create_max) ->
    {account_create_max, <<"服务器角色数量已达到上限"/utf8>>, <<"文本"/utf8>>};
text(account_login_forbidden) ->
    {account_login_forbidden, <<"账号禁止登录"/utf8>>, <<"文本"/utf8>>};
text(account_logout) ->
    {account_logout, <<"登出"/utf8>>, <<"文本"/utf8>>};
text(account_not_found) ->
    {account_not_found, <<"没有找到此账号"/utf8>>, <<"文本"/utf8>>};
text(account_permission_denied) ->
    {account_permission_denied, <<"账号权限不足"/utf8>>, <<"文本"/utf8>>};
text(achievement_not_completed) ->
    {achievement_not_completed, <<"成就未完成"/utf8>>, <<"文本"/utf8>>};
text(achievement_not_found) ->
    {achievement_not_found, <<"没有找到此成就"/utf8>>, <<"文本"/utf8>>};
text(asset_copper_not_enough) ->
    {asset_copper_not_enough, <<"铜币不足"/utf8>>, <<"文本"/utf8>>};
text(asset_gold_not_enough) ->
    {asset_gold_not_enough, <<"金币不足"/utf8>>, <<"文本"/utf8>>};
text(asset_not_enough) ->
    {asset_not_enough, <<"资产不足"/utf8>>, <<"文本"/utf8>>};
text(asset_silver_not_enough) ->
    {asset_silver_not_enough, <<"银币不足"/utf8>>, <<"文本"/utf8>>};
text(auction_not_found) ->
    {auction_not_found, <<"没有找到此拍品"/utf8>>, <<"文本"/utf8>>};
text(auction_price_changed) ->
    {auction_price_changed, <<"拍品价格已发生变化"/utf8>>, <<"文本"/utf8>>};
text(award_already_received) ->
    {award_already_received, <<"奖励已经领取过了"/utf8>>, <<"文本"/utf8>>};
text(award_error) ->
    {award_error, <<"奖励领取错误"/utf8>>, <<"文本"/utf8>>};
text(award_pre_not_received) ->
    {award_pre_not_received, <<"前置奖励未领取"/utf8>>, <<"文本"/utf8>>};
text(boss_dead) ->
    {boss_dead, <<"BOSS已经死亡"/utf8>>, <<"文本"/utf8>>};
text(boss_not_found) ->
    {boss_not_found, <<"没有找到此Boss"/utf8>>, <<"文本"/utf8>>};
text(bubble_duplicated) ->
    {bubble_duplicated, <<"气泡重复"/utf8>>, <<"文本"/utf8>>};
text(buff_duplicated) ->
    {buff_duplicated, <<"Buff重复"/utf8>>, <<"文本"/utf8>>};
text(chat_cannot_with_self) ->
    {chat_cannot_with_self, <<"不能和自己聊天"/utf8>>, <<"文本"/utf8>>};
text(chat_too_frequently) ->
    {chat_too_frequently, <<"发言太频繁"/utf8>>, <<"文本"/utf8>>};
text(cheat_command_not_found) ->
    {cheat_command_not_found, <<"没有找到此命令"/utf8>>, <<"文本"/utf8>>};
text(condition_not_met) ->
    {condition_not_met, <<"条件不满足"/utf8>>, <<"文本"/utf8>>};
text(configure_not_found) ->
    {configure_not_found, <<"没有找到此配置"/utf8>>, <<"文本"/utf8>>};
text(daily_not_completed) ->
    {daily_not_completed, <<"日常任务未完成"/utf8>>, <<"文本"/utf8>>};
text(daily_score_not_enough) ->
    {daily_score_not_enough, <<"日常活跃度不足"/utf8>>, <<"文本"/utf8>>};
text(dungeon_not_found) ->
    {dungeon_not_found, <<"没有找到此副本"/utf8>>, <<"文本"/utf8>>};
text(dungeon_today_number_limit) ->
    {dungeon_today_number_limit, <<"今日进入次数已达到上限"/utf8>>, <<"文本"/utf8>>};
text(fashion_duplicated) ->
    {fashion_duplicated, <<"时装重复"/utf8>>, <<"文本"/utf8>>};
text(friend_apply_not_found) ->
    {friend_apply_not_found, <<"没有找到此好友的申请"/utf8>>, <<"文本"/utf8>>};
text(friend_in_apply) ->
    {friend_in_apply, <<"对方已在申请列表中"/utf8>>, <<"文本"/utf8>>};
text(friend_in_be_block) ->
    {friend_in_be_block, <<"你已被对方拉黑"/utf8>>, <<"文本"/utf8>>};
text(friend_in_block) ->
    {friend_in_block, <<"对方已在黑名单中"/utf8>>, <<"文本"/utf8>>};
text(friend_in_list) ->
    {friend_in_list, <<"对方已在好友列表中"/utf8>>, <<"文本"/utf8>>};
text(friend_level_not_met) ->
    {friend_level_not_met, <<"对方好友等级不满足"/utf8>>, <<"文本"/utf8>>};
text(friend_not_found) ->
    {friend_not_found, <<"没有找到此好友"/utf8>>, <<"文本"/utf8>>};
text(friend_number_max) ->
    {friend_number_max, <<"好友数量达到上限"/utf8>>, <<"文本"/utf8>>};
text(guild_already_joined) ->
    {guild_already_joined, <<"你已经加入过公会了"/utf8>>, <<"文本"/utf8>>};
text(guild_apply_frequently) ->
    {guild_apply_frequently, <<"公会申请太频繁"/utf8>>, <<"文本"/utf8>>};
text(guild_apply_not_found) ->
    {guild_apply_not_found, <<"没有找到此申请"/utf8>>, <<"文本"/utf8>>};
text(guild_cannot_kick_self) ->
    {guild_cannot_kick_self, <<"不可剔除自己"/utf8>>, <<"文本"/utf8>>};
text(guild_cannot_update_self) ->
    {guild_cannot_update_self, <<"不可升级自己"/utf8>>, <<"文本"/utf8>>};
text(guild_create_frequently) ->
    {guild_create_frequently, <<"公会创建太频繁"/utf8>>, <<"文本"/utf8>>};
text(guild_member_not_found) ->
    {guild_member_not_found, <<"没有找到此成员"/utf8>>, <<"文本"/utf8>>};
text(guild_member_number_limit) ->
    {guild_member_number_limit, <<"公会成员数量已达到上限"/utf8>>, <<"文本"/utf8>>};
text(guild_not_found) ->
    {guild_not_found, <<"没有找到此商会"/utf8>>, <<"文本"/utf8>>};
text(guild_not_joined) ->
    {guild_not_joined, <<"没有加入公会"/utf8>>, <<"文本"/utf8>>};
text(guild_permission_denied) ->
    {guild_permission_denied, <<"公会权限不足"/utf8>>, <<"文本"/utf8>>};
text(invalid_classes) ->
    {invalid_classes, <<"无效职业"/utf8>>, <<"文本"/utf8>>};
text(invalid_item) ->
    {invalid_item, <<"无效物品"/utf8>>, <<"文本"/utf8>>};
text(invalid_number) ->
    {invalid_number, <<"无效数量"/utf8>>, <<"文本"/utf8>>};
text(invalid_sex) ->
    {invalid_sex, <<"无效性别"/utf8>>, <<"文本"/utf8>>};
text(invalid_type) ->
    {invalid_type, <<"无效类型"/utf8>>, <<"文本"/utf8>>};
text(item_bag_full) ->
    {item_bag_full, <<"背包已满"/utf8>>, <<"文本"/utf8>>};
text(item_cannot_use_directly) ->
    {item_cannot_use_directly, <<"物品不能直接使用"/utf8>>, <<"文本"/utf8>>};
text(item_not_enough) ->
    {item_not_enough, <<"物品不足"/utf8>>, <<"文本"/utf8>>};
text(item_use_number_max) ->
    {item_use_number_max, <<"使用个数超过单次使用上限"/utf8>>, <<"文本"/utf8>>};
text(key_already_activated) ->
    {key_already_activated, <<"激活码已激活过"/utf8>>, <<"文本"/utf8>>};
text(key_already_active) ->
    {key_already_active, <<"此兑换码已经兑换过了"/utf8>>, <<"文本"/utf8>>};
text(level_not_met) ->
    {level_not_met, <<"等级不满足"/utf8>>, <<"文本"/utf8>>};
text(lucky_money_already_received) ->
    {lucky_money_already_received, <<"红包已领取过"/utf8>>, <<"文本"/utf8>>};
text(lucky_money_expired) ->
    {lucky_money_expired, <<"红包已过期"/utf8>>, <<"文本"/utf8>>};
text(lucky_money_not_found) ->
    {lucky_money_not_found, <<"没有找到此红包"/utf8>>, <<"文本"/utf8>>};
text(mail_already_read) ->
    {mail_already_read, <<"邮件已阅读过"/utf8>>, <<"文本"/utf8>>};
text(mail_attachment_empty) ->
    {mail_attachment_empty, <<"附件为空"/utf8>>, <<"文本"/utf8>>};
text(mail_not_found) ->
    {mail_not_found, <<"没有找到此邮件"/utf8>>, <<"文本"/utf8>>};
text(mail_text_add_item_content) ->
    {mail_text_add_item_content, <<"您的背包已满，新增的道具已经放到了邮件里，请注意查收。"/utf8>>, <<"背包满内容"/utf8>>};
text(mail_text_add_item_title) ->
    {mail_text_add_item_title, <<"背包已满"/utf8>>, <<"背包满标题"/utf8>>};
text(mail_text_auction_income_content) ->
    {mail_text_auction_income_content, <<"您的拍卖收入分成。"/utf8>>, <<"拍卖分红内容"/utf8>>};
text(mail_text_auction_income_title) ->
    {mail_text_auction_income_title, <<"拍卖收入"/utf8>>, <<"拍卖分红标题"/utf8>>};
text(mail_text_auction_success_content) ->
    {mail_text_auction_success_content, <<"您的拍卖物品，请注意查收。"/utf8>>, <<"拍卖成功内容"/utf8>>};
text(mail_text_auction_success_title) ->
    {mail_text_auction_success_title, <<"拍卖成功"/utf8>>, <<"拍卖成功标题"/utf8>>};
text(name_duplicate) ->
    {name_duplicate, <<"名字重复"/utf8>>, <<"文本"/utf8>>};
text(name_duplicated) ->
    {name_duplicated, <<"名字重复"/utf8>>, <<"文本"/utf8>>};
text(name_length) ->
    {name_length, <<"名字长度不对"/utf8>>, <<"文本"/utf8>>};
text(name_length_invalid) ->
    {name_length_invalid, <<"名字长度无效"/utf8>>, <<"文本"/utf8>>};
text(name_not_utf8_charset) ->
    {name_not_utf8_charset, <<"名字非UTF8字符"/utf8>>, <<"文本"/utf8>>};
text(name_sensitive) ->
    {name_sensitive, <<"名字敏感"/utf8>>, <<"文本"/utf8>>};
text(notice_text_guild_create) ->
    {notice_text_guild_create, <<"~s创建公会"/utf8>>, <<"创建公会公告"/utf8>>};
text(notice_text_level_upgrade) ->
    {notice_text_level_upgrade, <<"恭喜"/utf8>>, <<"升级公告"/utf8>>};
text(notice_text_vip_upgrade) ->
    {notice_text_vip_upgrade, <<"恭喜"/utf8>>, <<"Vip升级公告"/utf8>>};
text(packet_heartbeat_too_fast) ->
    {packet_heartbeat_too_fast, <<"心跳包速度过快"/utf8>>, <<"文本"/utf8>>};
text(packet_too_fast) ->
    {packet_too_fast, <<"包速度过快"/utf8>>, <<"文本"/utf8>>};
text(role_cannot_change_same_classes) ->
    {role_cannot_change_same_classes, <<"职业不能相同"/utf8>>, <<"文本"/utf8>>};
text(role_cannot_change_same_name) ->
    {role_cannot_change_same_name, <<"名字不能相同"/utf8>>, <<"文本"/utf8>>};
text(role_cannot_change_same_sex) ->
    {role_cannot_change_same_sex, <<"性别不能相同"/utf8>>, <<"文本"/utf8>>};
text(server_create_forbidden) ->
    {server_create_forbidden, <<"服务器禁止创建角色"/utf8>>, <<"文本"/utf8>>};
text(server_id_mismatch) ->
    {server_id_mismatch, <<"服务器ID不匹配"/utf8>>, <<"文本"/utf8>>};
text(server_login_forbidden) ->
    {server_login_forbidden, <<"服务器禁止登录"/utf8>>, <<"文本"/utf8>>};
text(server_update) ->
    {server_update, <<"服务器更新"/utf8>>, <<"文本"/utf8>>};
text(shop_buy_num_max) ->
    {shop_buy_num_max, <<"已达到购买数量上限"/utf8>>, <<"文本"/utf8>>};
text(signed_already) ->
    {signed_already, <<"已经签到过了"/utf8>>, <<"文本"/utf8>>};
text(task_already_submitted) ->
    {task_already_submitted, <<"任务已提交"/utf8>>, <<"文本"/utf8>>};
text(task_not_completed) ->
    {task_not_completed, <<"任务还没完成"/utf8>>, <<"文本"/utf8>>};
text(task_not_found) ->
    {task_not_found, <<"没有找到此任务"/utf8>>, <<"文本"/utf8>>};
text(task_not_next) ->
    {task_not_next, <<"请按顺序完成"/utf8>>, <<"文本"/utf8>>};
text(task_pre_not_completed) ->
    {task_pre_not_completed, <<"前置任务还没完成"/utf8>>, <<"文本"/utf8>>};
text(timeout) ->
    {timeout, <<"请求超时"/utf8>>, <<"文本"/utf8>>};
text(title_duplicated) ->
    {title_duplicated, <<"称号重复"/utf8>>, <<"文本"/utf8>>};
text(user_offline) ->
    {user_offline, <<"对方不在线"/utf8>>, <<"文本"/utf8>>};
text(vip_level_not_met) ->
    {vip_level_not_met, <<"Vip等级不满足"/utf8>>, <<"文本"/utf8>>};
text(_) ->
    [].


-spec type(Type :: integer()) -> [MonsterId :: integer()] | Default :: [].
type(1) ->
    [1];
type(2) ->
    [2];
type(3) ->
    [3];
type(4) ->
    [4];
type(5) ->
    [5, [7]];
type(6) ->
    [6, [8]];
type(_) ->
    [].


-spec level() -> [Level :: integer()].
level() ->
    [0, 1, 2, 3, 4, 5, 6, 7, 8, 9].


-spec type_list() -> [Type :: integer()].
type_list() ->
    [1, 2, 3, 4, 5, 6].


-spec min_max_level() -> MinMaxLevel :: {MinLevel :: integer(), MaxLevel :: integer()}.
min_max_level() ->
    {0, 9}.


-spec text_count() -> CountZhCN :: integer().
text_count() ->
    102.


-spec max_text() -> MaxText :: {MaxKey :: atom(), MaxZhCN :: binary()}.
max_text() ->
    {vip_level_not_met, <<"附件为空"/utf8>>}.


-spec ref(Key :: atom(), Value :: binary()) -> Description :: binary() | Default :: [].
ref(act_script, <<"{monster, group_id}"/utf8>>) ->
    <<"特定怪物"/utf8>>;
ref(act_script, <<"enemy"/utf8>>) ->
    <<"敌人"/utf8>>;
ref(act_script, <<"monster"/utf8>>) ->
    <<"怪物"/utf8>>;
ref(act_script, <<"role"/utf8>>) ->
    <<"玩家"/utf8>>;
ref(condition, <<"{classes, n}"/utf8>>) ->
    <<"职业为n"/utf8>>;
ref(condition, <<"{dog_level, n}"/utf8>>) ->
    <<"宠物等级n级"/utf8>>;
ref(condition, <<"{friend, n}"/utf8>>) ->
    <<"拥有n个好友"/utf8>>;
ref(condition, <<"{level, n}"/utf8>>) ->
    <<"等级n级"/utf8>>;
ref(condition, <<"{login, n}"/utf8>>) ->
    <<"累计登录n天"/utf8>>;
ref(condition, <<"{seed_num, n}"/utf8>>) ->
    <<"喂养n次"/utf8>>;
ref(condition, <<"{sex, n}"/utf8>>) ->
    <<"性别为n"/utf8>>;
ref(condition, <<"{steal_coin, n}"/utf8>>) ->
    <<"偷币n次"/utf8>>;
ref(condition, <<"{touch_dog, n}"/utf8>>) ->
    <<"撸好友的狗n次"/utf8>>;
ref(condition, <<"{vip, n}"/utf8>>) ->
    <<"VIP等级n级"/utf8>>;
ref(_, _) ->
    [].


-spec ref_range(Key :: atom(), Value :: binary()) -> Description :: binary() | Default :: [].
ref_range(act_script, Value) when <<"{monster, group_id}"/utf8>> < Value ->
    <<"特定怪物"/utf8>>;
ref_range(act_script, Value) when <<"enemy"/utf8>> < Value ->
    <<"敌人"/utf8>>;
ref_range(act_script, Value) when <<"monster"/utf8>> < Value ->
    <<"怪物"/utf8>>;
ref_range(act_script, Value) when <<"role"/utf8>> < Value ->
    <<"玩家"/utf8>>;
ref_range(condition, Value) when <<"{classes, n}"/utf8>> < Value ->
    <<"职业为n"/utf8>>;
ref_range(condition, Value) when <<"{dog_level, n}"/utf8>> < Value ->
    <<"宠物等级n级"/utf8>>;
ref_range(condition, Value) when <<"{friend, n}"/utf8>> < Value ->
    <<"拥有n个好友"/utf8>>;
ref_range(condition, Value) when <<"{level, n}"/utf8>> < Value ->
    <<"等级n级"/utf8>>;
ref_range(condition, Value) when <<"{login, n}"/utf8>> < Value ->
    <<"累计登录n天"/utf8>>;
ref_range(condition, Value) when <<"{seed_num, n}"/utf8>> < Value ->
    <<"喂养n次"/utf8>>;
ref_range(condition, Value) when <<"{sex, n}"/utf8>> < Value ->
    <<"性别为n"/utf8>>;
ref_range(condition, Value) when <<"{steal_coin, n}"/utf8>> < Value ->
    <<"偷币n次"/utf8>>;
ref_range(condition, Value) when <<"{touch_dog, n}"/utf8>> < Value ->
    <<"撸好友的狗n次"/utf8>>;
ref_range(condition, Value) when <<"{vip, n}"/utf8>> < Value ->
    <<"VIP等级n级"/utf8>>;
ref_range(_, _) ->
    [].


-spec get_level_by_exp_asc(Exp :: integer()) -> Level :: integer() | Default :: [].
get_level_by_exp_asc(Exp) when Exp < 100 ->
    0;
get_level_by_exp_asc(Exp) when Exp < 200 ->
    1;
get_level_by_exp_asc(Exp) when Exp < 300 ->
    2;
get_level_by_exp_asc(Exp) when Exp < 400 ->
    3;
get_level_by_exp_asc(Exp) when Exp < 500 ->
    4;
get_level_by_exp_asc(Exp) when Exp < 600 ->
    5;
get_level_by_exp_asc(Exp) when Exp < 700 ->
    6;
get_level_by_exp_asc(Exp) when Exp < 800 ->
    7;
get_level_by_exp_asc(Exp) when Exp < 900 ->
    8;
get_level_by_exp_asc(Exp) when Exp < 1000 ->
    9;
get_level_by_exp_asc(_) ->
    [].


-spec get(Key :: atom()) -> Value :: integer() | Default :: [].
get(bag_size) ->
    100;
get(item_size) ->
    100;
get(store_size) ->
    100;
get(_) ->
    [].



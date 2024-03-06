-module(text_data).
-export([zhCN/1]).
-export([text/1]).
-export([text/2]).

-spec zhCN(Key :: term()) -> binary().
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
    <<"<id>~w</id>~s创建公会<id>~w</id>~s"/utf8>>;
zhCN(notice_text_level_upgrade) ->
    <<"恭喜<id>~w</id>~s升到~w级"/utf8>>;
zhCN(notice_text_vip_upgrade) ->
    <<"恭喜<id>~w</id>~sVip升到~w级"/utf8>>;
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
zhCN(Key) ->
    Key.

-spec text(Key :: atom()) -> Text :: binary() | Key :: atom().
text(Key) ->
    text(Key, parameter_data:get(language)).

-spec text(Key :: atom(), Lang :: atom()) -> Text :: binary() | Key :: atom().
text(Key, zhCN) ->
    zhCN(Key).

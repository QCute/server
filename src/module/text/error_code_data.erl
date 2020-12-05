-module(error_code_data).
-compile(nowarn_export_all).
-compile(export_all).


en(10001, no_such_account) ->
    <<"no such account"/utf8>>;
en(10002, duplicate) ->
    <<"duplicate"/utf8>>;
en(10002, name_duplicate) ->
    <<"name duplicate"/utf8>>;
en(10002, name_length) ->
    <<"name length max"/utf8>>;
en(10002, name_not_utf8) ->
    <<"name not utf8 charset"/utf8>>;
en(10002, name_sensitive) ->
    <<"name sensitive"/utf8>>;
en(10002, refuse) ->
    <<"refuse"/utf8>>;
en(10002, server_id_not_match) ->
    <<"server id not match"/utf8>>;
en(10003, duplicate) ->
    <<"duplicate"/utf8>>;
en(10003, no_such_name) ->
    <<"no such name"/utf8>>;
en(10003, permission_denied) ->
    <<"permission denied"/utf8>>;
en(10003, refuse) ->
    <<"refuse"/utf8>>;
en(10003, server_id_not_match) ->
    <<"server id not match"/utf8>>;
en(10003, server_update) ->
    <<"server update"/utf8>>;
en(10004, heartbeat_packet_fast_error) ->
    <<"heartbeat packet fast error"/utf8>>;
en(10004, logout) ->
    <<"logout"/utf8>>;
en(10004, no_such_name) ->
    <<"no such name"/utf8>>;
en(10004, packet_fast_error) ->
    <<"packet fast error"/utf8>>;
en(10004, server_id_not_match) ->
    <<"server id not match"/utf8>>;
en(10004, server_update) ->
    <<"server update"/utf8>>;
en(11106, configure_not_found) ->
    <<"configure not found"/utf8>>;
en(11106, invalid_item) ->
    <<"invalid item"/utf8>>;
en(11106, item_cannot_use_directly) ->
    <<"item cannot use directly"/utf8>>;
en(11106, use_number_max) ->
    <<"use number max"/utf8>>;
en(11202, condition_not_met) ->
    <<"condition not met"/utf8>>;
en(11202, configure_not_found) ->
    <<"configure not found"/utf8>>;
en(11202, no_such_quest) ->
    <<"no such quest"/utf8>>;
en(11202, not_next_quest) ->
    <<"not next quest"/utf8>>;
en(11202, pre_quest_not_complete) ->
    <<"pre quest not complete"/utf8>>;
en(11203, configure_not_found) ->
    <<"configure not found"/utf8>>;
en(11203, no_such_quest) ->
    <<"no such quest"/utf8>>;
en(11203, quest_already_submit) ->
    <<"quest already submit"/utf8>>;
en(11203, quest_not_complete) ->
    <<"quest not complete"/utf8>>;
en(11302, asset_not_enough) ->
    <<"asset not enough"/utf8>>;
en(11302, buy_max) ->
    <<"buy max"/utf8>>;
en(11302, configure_not_found) ->
    <<"configure not found"/utf8>>;
en(11302, level_not_enough) ->
    <<"level not enough"/utf8>>;
en(11302, number_invalid) ->
    <<"number invalid"/utf8>>;
en(11302, vip_level_not_enough) ->
    <<"vip level not enough"/utf8>>;
en(11402, already_read) ->
    <<"already read"/utf8>>;
en(11402, no_such_mail) ->
    <<"no such mail"/utf8>>;
en(11403, bag_full) ->
    <<"bag full"/utf8>>;
en(11403, no_attachment) ->
    <<"no attachment"/utf8>>;
en(11403, no_such_mail) ->
    <<"no such mail"/utf8>>;
en(11502, friend_level_not_enough) ->
    <<"friend level not enough"/utf8>>;
en(11502, friend_number_max) ->
    <<"friend number max"/utf8>>;
en(11502, level_not_enough) ->
    <<"level not enough"/utf8>>;
en(11502, user_offline) ->
    <<"user offline"/utf8>>;
en(11503, no_such_apply) ->
    <<"no such apply"/utf8>>;
en(11601, level_not_enough) ->
    <<"level not enough"/utf8>>;
en(11601, time_in_cd) ->
    <<"time in cd"/utf8>>;
en(11602, level_not_enough) ->
    <<"level not enough"/utf8>>;
en(11602, no_guild) ->
    <<"no guild"/utf8>>;
en(11602, time_in_cd) ->
    <<"time in cd"/utf8>>;
en(11603, level_not_enough) ->
    <<"level not enough"/utf8>>;
en(11603, user_offline) ->
    <<"user offline"/utf8>>;
en(11702, condition_not_met) ->
    <<"condition not met"/utf8>>;
en(11702, configure_not_found) ->
    <<"configure not found"/utf8>>;
en(11702, item_not_enough) ->
    <<"item not enough"/utf8>>;
en(15001, already_sign_today) ->
    <<"already sign today"/utf8>>;
en(15001, award_error) ->
    <<"award error"/utf8>>;
en(15002, key_already_active) ->
    <<"key already active"/utf8>>;
en(15002, timeout) ->
    <<"timeout"/utf8>>;
en(15004, lucky_money_already_receive) ->
    <<"lucky money already receive"/utf8>>;
en(15004, lucky_money_expire) ->
    <<"lucky money expire"/utf8>>;
en(15004, no_such_lucky_money) ->
    <<"no such lucky money"/utf8>>;
en(15004, timeout) ->
    <<"timeout"/utf8>>;
en(16102, gold_not_enough) ->
    <<"gold not enough"/utf8>>;
en(16102, no_such_auction) ->
    <<"no such auction"/utf8>>;
en(16102, price_change) ->
    <<"price change"/utf8>>;
en(16102, timeout) ->
    <<"timeout"/utf8>>;
en(17002, condition_not_met) ->
    <<"condition not met"/utf8>>;
en(17002, configure_not_found) ->
    <<"configure not found"/utf8>>;
en(17002, item_not_enough) ->
    <<"item not enough"/utf8>>;
en(17002, today_number_limit) ->
    <<"today number limit"/utf8>>;
en(18001, no_such_boss) ->
    <<"no such boss"/utf8>>;
en(30107, already_join_guild) ->
    <<"already join guild"/utf8>>;
en(30107, condition_not_met) ->
    <<"condition not met"/utf8>>;
en(30107, cost_not_enough) ->
    <<"cost not enough"/utf8>>;
en(30107, duplicate) ->
    <<"duplicate"/utf8>>;
en(30107, length) ->
    <<"length"/utf8>>;
en(30107, not_utf8) ->
    <<"not utf8"/utf8>>;
en(30107, sensitive) ->
    <<"sensitive"/utf8>>;
en(30107, time_in_join_cd) ->
    <<"time in join cd"/utf8>>;
en(30107, timeout) ->
    <<"timeout"/utf8>>;
en(30107, unknown_type) ->
    <<"unknown type"/utf8>>;
en(30108, already_join_guild) ->
    <<"already join guild"/utf8>>;
en(30108, condition_not_met) ->
    <<"condition not met"/utf8>>;
en(30108, no_such_guild) ->
    <<"no such guild"/utf8>>;
en(30108, time_in_join_cd) ->
    <<"time in join cd"/utf8>>;
en(30108, timeout) ->
    <<"timeout"/utf8>>;
en(30109, timeout) ->
    <<"timeout"/utf8>>;
en(30110, timeout) ->
    <<"timeout"/utf8>>;
en(30111, already_join_guild) ->
    <<"already join guild"/utf8>>;
en(30111, member_number_limit) ->
    <<"member number limit"/utf8>>;
en(30111, no_such_apply) ->
    <<"no such apply"/utf8>>;
en(30111, no_such_guild) ->
    <<"no such guild"/utf8>>;
en(30111, permission_denied) ->
    <<"permission denied"/utf8>>;
en(30111, timeout) ->
    <<"timeout"/utf8>>;
en(30112, permission_denied) ->
    <<"permission denied"/utf8>>;
en(30112, timeout) ->
    <<"timeout"/utf8>>;
en(30113, permission_denied) ->
    <<"permission denied"/utf8>>;
en(30113, timeout) ->
    <<"timeout"/utf8>>;
en(30113, you_not_join_guild) ->
    <<"you not join guild"/utf8>>;
en(30114, permission_denied) ->
    <<"permission denied"/utf8>>;
en(30114, timeout) ->
    <<"timeout"/utf8>>;
en(30115, timeout) ->
    <<"timeout"/utf8>>;
en(30115, you_not_join_guild) ->
    <<"you not join guild"/utf8>>;
en(30116, permission_denied) ->
    <<"permission denied"/utf8>>;
en(30116, timeout) ->
    <<"timeout"/utf8>>;
en(30116, you_not_join_guild) ->
    <<"you not join guild"/utf8>>;
en(30117, cannot_kick_self) ->
    <<"cannot kick self"/utf8>>;
en(30117, he_not_join_guild) ->
    <<"he not join guild"/utf8>>;
en(30117, permission_denied) ->
    <<"permission denied"/utf8>>;
en(30117, timeout) ->
    <<"timeout"/utf8>>;
en(30117, you_not_join_guild) ->
    <<"you not join guild"/utf8>>;
en(30118, cannot_update_self) ->
    <<"cannot update self"/utf8>>;
en(30118, he_not_join_guild) ->
    <<"he not join guild"/utf8>>;
en(30118, job_invalid) ->
    <<"job invalid"/utf8>>;
en(30118, permission_denied) ->
    <<"permission denied"/utf8>>;
en(30118, timeout) ->
    <<"timeout"/utf8>>;
en(30118, you_not_join_guild) ->
    <<"you not join guild"/utf8>>;
en(30119, timeout) ->
    <<"timeout"/utf8>>;
en(30120, timeout) ->
    <<"timeout"/utf8>>;
en(60002, no_such_command) ->
    <<"no such command"/utf8>>;
en(_Type, _Key) ->
    _Key.


sc(10001, no_such_account) ->
    <<"没有此账户"/utf8>>;
sc(10002, duplicate) ->
    <<"重复创建账号"/utf8>>;
sc(10002, name_duplicate) ->
    <<"名字重复"/utf8>>;
sc(10002, name_length) ->
    <<"名字长度不对"/utf8>>;
sc(10002, name_not_utf8) ->
    <<"未知字符"/utf8>>;
sc(10002, name_sensitive) ->
    <<"名字包含敏感词"/utf8>>;
sc(10002, refuse) ->
    <<"禁止登录"/utf8>>;
sc(10002, server_id_not_match) ->
    <<"服务器ID不匹配"/utf8>>;
sc(10003, duplicate) ->
    <<"重复登录"/utf8>>;
sc(10003, no_such_name) ->
    <<"没有此用户名"/utf8>>;
sc(10003, permission_denied) ->
    <<"权限不够"/utf8>>;
sc(10003, refuse) ->
    <<"禁止登录"/utf8>>;
sc(10003, server_id_not_match) ->
    <<"服务器ID不匹配"/utf8>>;
sc(10003, server_update) ->
    <<"服务器更新"/utf8>>;
sc(10004, heartbeat_packet_fast_error) ->
    <<"心跳包速度过快"/utf8>>;
sc(10004, logout) ->
    <<"注销"/utf8>>;
sc(10004, no_such_name) ->
    <<"没有此用户名"/utf8>>;
sc(10004, packet_fast_error) ->
    <<"包速度过快"/utf8>>;
sc(10004, server_id_not_match) ->
    <<"服务器ID不匹配"/utf8>>;
sc(10004, server_update) ->
    <<"服务器更新"/utf8>>;
sc(11106, configure_not_found) ->
    <<"配置错误"/utf8>>;
sc(11106, invalid_item) ->
    <<"无效物品"/utf8>>;
sc(11106, item_cannot_use_directly) ->
    <<"物品不能直接使用"/utf8>>;
sc(11106, use_number_max) ->
    <<"使用个数超过单次使用上限"/utf8>>;
sc(11202, condition_not_met) ->
    <<"条件不满足"/utf8>>;
sc(11202, configure_not_found) ->
    <<"配置错误"/utf8>>;
sc(11202, no_such_quest) ->
    <<"没有此任务"/utf8>>;
sc(11202, not_next_quest) ->
    <<"请按顺序完成"/utf8>>;
sc(11202, pre_quest_not_complete) ->
    <<"前置任务还没完成"/utf8>>;
sc(11203, configure_not_found) ->
    <<"配置错误"/utf8>>;
sc(11203, no_such_quest) ->
    <<"没有此任务"/utf8>>;
sc(11203, quest_already_submit) ->
    <<"任务已提交"/utf8>>;
sc(11203, quest_not_complete) ->
    <<"任务还没完成"/utf8>>;
sc(11302, asset_not_enough) ->
    <<"资产不足"/utf8>>;
sc(11302, buy_max) ->
    <<"已达到购买上限"/utf8>>;
sc(11302, configure_not_found) ->
    <<"配置错误"/utf8>>;
sc(11302, level_not_enough) ->
    <<"等级不满足"/utf8>>;
sc(11302, number_invalid) ->
    <<"购买数量错误"/utf8>>;
sc(11302, vip_level_not_enough) ->
    <<"Vip等级不满足"/utf8>>;
sc(11402, already_read) ->
    <<"邮件已阅读过"/utf8>>;
sc(11402, no_such_mail) ->
    <<"没有此邮件"/utf8>>;
sc(11403, bag_full) ->
    <<"背包已满"/utf8>>;
sc(11403, no_attachment) ->
    <<"没有可领取附件"/utf8>>;
sc(11403, no_such_mail) ->
    <<"没有此邮件"/utf8>>;
sc(11502, friend_level_not_enough) ->
    <<"对方好友未开放"/utf8>>;
sc(11502, friend_number_max) ->
    <<"好友数量达到上限"/utf8>>;
sc(11502, level_not_enough) ->
    <<"好友未开放"/utf8>>;
sc(11502, user_offline) ->
    <<"对方不在线"/utf8>>;
sc(11503, no_such_apply) ->
    <<"没有此好友的申请"/utf8>>;
sc(11601, level_not_enough) ->
    <<"等级不足"/utf8>>;
sc(11601, time_in_cd) ->
    <<"时间冷却中"/utf8>>;
sc(11602, level_not_enough) ->
    <<"等级不足"/utf8>>;
sc(11602, no_guild) ->
    <<"没加入公会"/utf8>>;
sc(11602, time_in_cd) ->
    <<"时间冷却中"/utf8>>;
sc(11603, level_not_enough) ->
    <<"等级不足"/utf8>>;
sc(11603, user_offline) ->
    <<"对方不在线"/utf8>>;
sc(11702, condition_not_met) ->
    <<"条件不足"/utf8>>;
sc(11702, configure_not_found) ->
    <<"配置错误"/utf8>>;
sc(11702, item_not_enough) ->
    <<"材料不足"/utf8>>;
sc(15001, already_sign_today) ->
    <<"今天已经签到过了"/utf8>>;
sc(15001, award_error) ->
    <<"奖励配置错误"/utf8>>;
sc(15002, key_already_active) ->
    <<"此兑换码已经兑换过了"/utf8>>;
sc(15002, timeout) ->
    <<"请求超时"/utf8>>;
sc(15004, lucky_money_already_receive) ->
    <<"红包已领取过"/utf8>>;
sc(15004, lucky_money_expire) ->
    <<"红包已过期"/utf8>>;
sc(15004, no_such_lucky_money) ->
    <<"此兑换码已经兑换过了"/utf8>>;
sc(15004, timeout) ->
    <<"请求超时"/utf8>>;
sc(16102, gold_not_enough) ->
    <<"元宝不足"/utf8>>;
sc(16102, no_such_auction) ->
    <<"没有此拍品"/utf8>>;
sc(16102, price_change) ->
    <<"价格已变化"/utf8>>;
sc(16102, timeout) ->
    <<"请求超时"/utf8>>;
sc(17002, condition_not_met) ->
    <<"条件不满足"/utf8>>;
sc(17002, configure_not_found) ->
    <<"配置错误"/utf8>>;
sc(17002, item_not_enough) ->
    <<"消耗材料不足"/utf8>>;
sc(17002, today_number_limit) ->
    <<"今天进入次数已达到上限"/utf8>>;
sc(18001, no_such_boss) ->
    <<"没有此Boss"/utf8>>;
sc(30107, already_join_guild) ->
    <<"你已经加入过公会了"/utf8>>;
sc(30107, condition_not_met) ->
    <<"条件不足"/utf8>>;
sc(30107, cost_not_enough) ->
    <<"资产不足"/utf8>>;
sc(30107, duplicate) ->
    <<"公会名字重复"/utf8>>;
sc(30107, length) ->
    <<"长度不对"/utf8>>;
sc(30107, not_utf8) ->
    <<"未知字符"/utf8>>;
sc(30107, sensitive) ->
    <<"名字包含敏感词"/utf8>>;
sc(30107, time_in_join_cd) ->
    <<"创建公会时间冷却中"/utf8>>;
sc(30107, timeout) ->
    <<"请求超时"/utf8>>;
sc(30107, unknown_type) ->
    <<"未知类型"/utf8>>;
sc(30108, already_join_guild) ->
    <<"你已经加入过公会了"/utf8>>;
sc(30108, condition_not_met) ->
    <<"条件不足"/utf8>>;
sc(30108, no_such_guild) ->
    <<"没有此公会"/utf8>>;
sc(30108, time_in_join_cd) ->
    <<"加入公会时间冷却中"/utf8>>;
sc(30108, timeout) ->
    <<"请求超时"/utf8>>;
sc(30109, timeout) ->
    <<"请求超时"/utf8>>;
sc(30110, timeout) ->
    <<"请求超时"/utf8>>;
sc(30111, already_join_guild) ->
    <<"已加入其它公会"/utf8>>;
sc(30111, member_number_limit) ->
    <<"已达到成员上限"/utf8>>;
sc(30111, no_such_apply) ->
    <<"没有此申请"/utf8>>;
sc(30111, no_such_guild) ->
    <<"没有此公会"/utf8>>;
sc(30111, permission_denied) ->
    <<"权限不足"/utf8>>;
sc(30111, timeout) ->
    <<"请求超时"/utf8>>;
sc(30112, permission_denied) ->
    <<"权限不足"/utf8>>;
sc(30112, timeout) ->
    <<"请求超时"/utf8>>;
sc(30113, permission_denied) ->
    <<"权限不足"/utf8>>;
sc(30113, timeout) ->
    <<"请求超时"/utf8>>;
sc(30113, you_not_join_guild) ->
    <<"你没有加入任何公会"/utf8>>;
sc(30114, permission_denied) ->
    <<"权限不足"/utf8>>;
sc(30114, timeout) ->
    <<"请求超时"/utf8>>;
sc(30115, timeout) ->
    <<"请求超时"/utf8>>;
sc(30115, you_not_join_guild) ->
    <<"你没有加入任何公会"/utf8>>;
sc(30116, permission_denied) ->
    <<"权限不足"/utf8>>;
sc(30116, timeout) ->
    <<"请求超时"/utf8>>;
sc(30116, you_not_join_guild) ->
    <<"你没有加入任何公会"/utf8>>;
sc(30117, cannot_kick_self) ->
    <<"不可剔除自己"/utf8>>;
sc(30117, he_not_join_guild) ->
    <<"此人没有加入公会"/utf8>>;
sc(30117, permission_denied) ->
    <<"权限不足"/utf8>>;
sc(30117, timeout) ->
    <<"请求超时"/utf8>>;
sc(30117, you_not_join_guild) ->
    <<"你没有加入任何公会"/utf8>>;
sc(30118, cannot_update_self) ->
    <<"不可升级自己"/utf8>>;
sc(30118, he_not_join_guild) ->
    <<"此人没有加入公会"/utf8>>;
sc(30118, job_invalid) ->
    <<"位置无效"/utf8>>;
sc(30118, permission_denied) ->
    <<"权限不足"/utf8>>;
sc(30118, timeout) ->
    <<"请求超时"/utf8>>;
sc(30118, you_not_join_guild) ->
    <<"你没有加入任何公会"/utf8>>;
sc(30119, timeout) ->
    <<"请求超时"/utf8>>;
sc(30120, timeout) ->
    <<"请求超时"/utf8>>;
sc(60002, no_such_command) ->
    <<"没有找到命令"/utf8>>;
sc(_Type, _Key) ->
    _Key.


tc(10001, no_such_account) ->
    <<"没有此账户"/utf8>>;
tc(10002, duplicate) ->
    <<"重复创建账号"/utf8>>;
tc(10002, name_duplicate) ->
    <<"名字重复"/utf8>>;
tc(10002, name_length) ->
    <<"名字长度不对"/utf8>>;
tc(10002, name_not_utf8) ->
    <<"未知字符"/utf8>>;
tc(10002, name_sensitive) ->
    <<"名字包含敏感词"/utf8>>;
tc(10002, refuse) ->
    <<"禁止登录"/utf8>>;
tc(10002, server_id_not_match) ->
    <<"服务器ID不匹配"/utf8>>;
tc(10003, duplicate) ->
    <<"重复登录"/utf8>>;
tc(10003, no_such_name) ->
    <<"没有此用户名"/utf8>>;
tc(10003, permission_denied) ->
    <<"权限不够"/utf8>>;
tc(10003, refuse) ->
    <<"禁止登录"/utf8>>;
tc(10003, server_id_not_match) ->
    <<"服务器ID不匹配"/utf8>>;
tc(10003, server_update) ->
    <<"服务器更新"/utf8>>;
tc(10004, heartbeat_packet_fast_error) ->
    <<"心跳包速度过快"/utf8>>;
tc(10004, logout) ->
    <<"注销"/utf8>>;
tc(10004, no_such_name) ->
    <<"没有此用户名"/utf8>>;
tc(10004, packet_fast_error) ->
    <<"包速度过快"/utf8>>;
tc(10004, server_id_not_match) ->
    <<"服务器ID不匹配"/utf8>>;
tc(10004, server_update) ->
    <<"服务器更新"/utf8>>;
tc(11106, configure_not_found) ->
    <<"配置错误"/utf8>>;
tc(11106, invalid_item) ->
    <<"无效物品"/utf8>>;
tc(11106, item_cannot_use_directly) ->
    <<"物品不能直接使用"/utf8>>;
tc(11106, use_number_max) ->
    <<"使用个数超过单次使用上限"/utf8>>;
tc(11202, condition_not_met) ->
    <<"条件不满足"/utf8>>;
tc(11202, configure_not_found) ->
    <<"配置错误"/utf8>>;
tc(11202, no_such_quest) ->
    <<"没有此任务"/utf8>>;
tc(11202, not_next_quest) ->
    <<"请按顺序完成"/utf8>>;
tc(11202, pre_quest_not_complete) ->
    <<"前置任务还没完成"/utf8>>;
tc(11203, configure_not_found) ->
    <<"配置错误"/utf8>>;
tc(11203, no_such_quest) ->
    <<"没有此任务"/utf8>>;
tc(11203, quest_already_submit) ->
    <<"任务已提交"/utf8>>;
tc(11203, quest_not_complete) ->
    <<"任务还没完成"/utf8>>;
tc(11302, asset_not_enough) ->
    <<"资产不足"/utf8>>;
tc(11302, buy_max) ->
    <<"已达到购买上限"/utf8>>;
tc(11302, configure_not_found) ->
    <<"配置错误"/utf8>>;
tc(11302, level_not_enough) ->
    <<"等级不满足"/utf8>>;
tc(11302, number_invalid) ->
    <<"购买数量错误"/utf8>>;
tc(11302, vip_level_not_enough) ->
    <<"Vip等级不满足"/utf8>>;
tc(11402, already_read) ->
    <<"邮件已阅读过"/utf8>>;
tc(11402, no_such_mail) ->
    <<"没有此邮件"/utf8>>;
tc(11403, bag_full) ->
    <<"背包已满"/utf8>>;
tc(11403, no_attachment) ->
    <<"没有可领取附件"/utf8>>;
tc(11403, no_such_mail) ->
    <<"没有此邮件"/utf8>>;
tc(11502, friend_level_not_enough) ->
    <<"对方好友未开放"/utf8>>;
tc(11502, friend_number_max) ->
    <<"好友数量达到上限"/utf8>>;
tc(11502, level_not_enough) ->
    <<"好友未开放"/utf8>>;
tc(11502, user_offline) ->
    <<"对方不在线"/utf8>>;
tc(11503, no_such_apply) ->
    <<"没有此好友的申请"/utf8>>;
tc(11601, level_not_enough) ->
    <<"等级不足"/utf8>>;
tc(11601, time_in_cd) ->
    <<"时间冷却中"/utf8>>;
tc(11602, level_not_enough) ->
    <<"等级不足"/utf8>>;
tc(11602, no_guild) ->
    <<"没加入公会"/utf8>>;
tc(11602, time_in_cd) ->
    <<"时间冷却中"/utf8>>;
tc(11603, level_not_enough) ->
    <<"等级不足"/utf8>>;
tc(11603, user_offline) ->
    <<"对方不在线"/utf8>>;
tc(11702, condition_not_met) ->
    <<"条件不足"/utf8>>;
tc(11702, configure_not_found) ->
    <<"配置错误"/utf8>>;
tc(11702, item_not_enough) ->
    <<"材料不足"/utf8>>;
tc(15001, already_sign_today) ->
    <<"今天已经签到过了"/utf8>>;
tc(15001, award_error) ->
    <<"奖励配置错误"/utf8>>;
tc(15002, key_already_active) ->
    <<"此兑换码已经兑换过了"/utf8>>;
tc(15002, timeout) ->
    <<"请求超时"/utf8>>;
tc(15004, lucky_money_already_receive) ->
    <<"红包已领取过"/utf8>>;
tc(15004, lucky_money_expire) ->
    <<"红包已过期"/utf8>>;
tc(15004, no_such_lucky_money) ->
    <<"此兑换码已经兑换过了"/utf8>>;
tc(15004, timeout) ->
    <<"请求超时"/utf8>>;
tc(16102, gold_not_enough) ->
    <<"元宝不足"/utf8>>;
tc(16102, no_such_auction) ->
    <<"没有此拍品"/utf8>>;
tc(16102, price_change) ->
    <<"价格已变化"/utf8>>;
tc(16102, timeout) ->
    <<"请求超时"/utf8>>;
tc(17002, condition_not_met) ->
    <<"条件不满足"/utf8>>;
tc(17002, configure_not_found) ->
    <<"配置错误"/utf8>>;
tc(17002, item_not_enough) ->
    <<"消耗材料不足"/utf8>>;
tc(17002, today_number_limit) ->
    <<"今天进入次数已达到上限"/utf8>>;
tc(18001, no_such_boss) ->
    <<"没有此Boss"/utf8>>;
tc(30107, already_join_guild) ->
    <<"你已经加入过公会了"/utf8>>;
tc(30107, condition_not_met) ->
    <<"条件不足"/utf8>>;
tc(30107, cost_not_enough) ->
    <<"资产不足"/utf8>>;
tc(30107, duplicate) ->
    <<"公会名字重复"/utf8>>;
tc(30107, length) ->
    <<"长度不对"/utf8>>;
tc(30107, not_utf8) ->
    <<"未知字符"/utf8>>;
tc(30107, sensitive) ->
    <<"名字包含敏感词"/utf8>>;
tc(30107, time_in_join_cd) ->
    <<"创建公会时间冷却中"/utf8>>;
tc(30107, timeout) ->
    <<"请求超时"/utf8>>;
tc(30107, unknown_type) ->
    <<"未知类型"/utf8>>;
tc(30108, already_join_guild) ->
    <<"你已经加入过公会了"/utf8>>;
tc(30108, condition_not_met) ->
    <<"条件不足"/utf8>>;
tc(30108, no_such_guild) ->
    <<"没有此公会"/utf8>>;
tc(30108, time_in_join_cd) ->
    <<"加入公会时间冷却中"/utf8>>;
tc(30108, timeout) ->
    <<"请求超时"/utf8>>;
tc(30109, timeout) ->
    <<"请求超时"/utf8>>;
tc(30110, timeout) ->
    <<"请求超时"/utf8>>;
tc(30111, already_join_guild) ->
    <<"已加入其它公会"/utf8>>;
tc(30111, member_number_limit) ->
    <<"已达到成员上限"/utf8>>;
tc(30111, no_such_apply) ->
    <<"没有此申请"/utf8>>;
tc(30111, no_such_guild) ->
    <<"没有此公会"/utf8>>;
tc(30111, permission_denied) ->
    <<"权限不足"/utf8>>;
tc(30111, timeout) ->
    <<"请求超时"/utf8>>;
tc(30112, permission_denied) ->
    <<"权限不足"/utf8>>;
tc(30112, timeout) ->
    <<"请求超时"/utf8>>;
tc(30113, permission_denied) ->
    <<"权限不足"/utf8>>;
tc(30113, timeout) ->
    <<"请求超时"/utf8>>;
tc(30113, you_not_join_guild) ->
    <<"你没有加入任何公会"/utf8>>;
tc(30114, permission_denied) ->
    <<"权限不足"/utf8>>;
tc(30114, timeout) ->
    <<"请求超时"/utf8>>;
tc(30115, timeout) ->
    <<"请求超时"/utf8>>;
tc(30115, you_not_join_guild) ->
    <<"你没有加入任何公会"/utf8>>;
tc(30116, permission_denied) ->
    <<"权限不足"/utf8>>;
tc(30116, timeout) ->
    <<"请求超时"/utf8>>;
tc(30116, you_not_join_guild) ->
    <<"你没有加入任何公会"/utf8>>;
tc(30117, cannot_kick_self) ->
    <<"不可剔除自己"/utf8>>;
tc(30117, he_not_join_guild) ->
    <<"此人没有加入公会"/utf8>>;
tc(30117, permission_denied) ->
    <<"权限不足"/utf8>>;
tc(30117, timeout) ->
    <<"请求超时"/utf8>>;
tc(30117, you_not_join_guild) ->
    <<"你没有加入任何公会"/utf8>>;
tc(30118, cannot_update_self) ->
    <<"不可升级自己"/utf8>>;
tc(30118, he_not_join_guild) ->
    <<"此人没有加入公会"/utf8>>;
tc(30118, job_invalid) ->
    <<"位置无效"/utf8>>;
tc(30118, permission_denied) ->
    <<"权限不足"/utf8>>;
tc(30118, timeout) ->
    <<"请求超时"/utf8>>;
tc(30118, you_not_join_guild) ->
    <<"你没有加入任何公会"/utf8>>;
tc(30119, timeout) ->
    <<"请求超时"/utf8>>;
tc(30120, timeout) ->
    <<"请求超时"/utf8>>;
tc(60002, no_such_command) ->
    <<"没有找到命令"/utf8>>;
tc(_Type, _Key) ->
    _Key.



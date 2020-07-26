-module(text_data).
-compile(nowarn_export_all).
-compile(export_all).


en(add_item_content) ->
    <<""/utf8>>;
en(add_item_title) ->
    <<""/utf8>>;
en(auction_income_content) ->
    <<""/utf8>>;
en(auction_income_title) ->
    <<""/utf8>>;
en(auction_success_content) ->
    <<""/utf8>>;
en(auction_success_title) ->
    <<""/utf8>>;
en(guild_create) ->
    <<""/utf8>>;
en(level_upgrade) ->
    <<""/utf8>>;
en(test) ->
    <<""/utf8>>;
en(Key) ->
    Key.


sc(add_item_content) ->
    <<"您的背包已满，新增的道具已经放到了邮件里，请注意查收。"/utf8>>;
sc(add_item_title) ->
    <<"背包已满"/utf8>>;
sc(auction_income_content) ->
    <<"您的拍卖收入分成"/utf8>>;
sc(auction_income_title) ->
    <<"拍卖收入"/utf8>>;
sc(auction_success_content) ->
    <<"您的拍卖物品，请注意查收。"/utf8>>;
sc(auction_success_title) ->
    <<"拍卖成功"/utf8>>;
sc(guild_create) ->
    <<"创建公会"/utf8>>;
sc(level_upgrade) ->
    <<"升级"/utf8>>;
sc(test) ->
    <<"😂"/utf8>>;
sc(Key) ->
    Key.


tc(add_item_content) ->
    <<""/utf8>>;
tc(add_item_title) ->
    <<""/utf8>>;
tc(auction_income_content) ->
    <<""/utf8>>;
tc(auction_income_title) ->
    <<""/utf8>>;
tc(auction_success_content) ->
    <<""/utf8>>;
tc(auction_success_title) ->
    <<""/utf8>>;
tc(guild_create) ->
    <<""/utf8>>;
tc(level_upgrade) ->
    <<""/utf8>>;
tc(test) ->
    <<""/utf8>>;
tc(Key) ->
    Key.



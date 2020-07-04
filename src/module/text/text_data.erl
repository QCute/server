-module(text_data).
-compile(nowarn_export_all).
-compile(export_all).


sc(add_item_content) ->
    <<"您的背包已满，新增的道具已经放到了邮件里，请注意查收。"/utf8>>;
sc(add_item_title) ->
    <<"背包已满"/utf8>>;
sc(auction_income_content) ->
    <<"您的拍卖收入分成"/utf8>>;
sc(auction_income_title) ->
    <<"拍卖收入"/utf8>>;
sc(auction_success_content) ->
    <<"您的拍卖物品"/utf8>>;
sc(auction_success_title) ->
    <<"拍卖成功"/utf8>>;
sc(guild_create) ->
    <<"创建公会"/utf8>>;
sc(level_upgrade) ->
    <<"恭喜~s"/utf8>>;
sc(test) ->
    <<"😂"/utf8>>;
sc(Key) ->
    Key.



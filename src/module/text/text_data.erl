-module(text_data).
-compile(nowarn_export_all).
-compile(export_all).


en(add_item_content) ->
    <<"Your bag is full, the new props have been put in the mail, please check."/utf8>>;
en(add_item_title) ->
    <<"Bag full"/utf8>>;
en(auction_income_content) ->
    <<"Your share of auction revenue."/utf8>>;
en(auction_income_title) ->
    <<"Your auction revenue"/utf8>>;
en(auction_success_content) ->
    <<"Please check your auction items."/utf8>>;
en(auction_success_title) ->
    <<"Auction Succeed"/utf8>>;
en(guild_create) ->
    <<"Celebrate <id>~w</id> ~s create guild <id>~w</id> ~s"/utf8>>;
en(level_upgrade) ->
    <<"Celebrate <id>~w</id> ~s upgrade level to ~w"/utf8>>;
en(test) ->
    <<"😂"/utf8>>;
en(vip_upgrade) ->
    <<"Celebrate <id>~w</id> ~s upgrade Vip level to ~w"/utf8>>;
en(Key) ->
    Key.


tc(add_item_content) ->
    <<"您的背包已滿，新增的道具已經放到了郵件裏，請注意查收。"/utf8>>;
tc(add_item_title) ->
    <<"背包已滿"/utf8>>;
tc(auction_income_content) ->
    <<"您的拍賣收入分成。"/utf8>>;
tc(auction_income_title) ->
    <<"拍賣收入"/utf8>>;
tc(auction_success_content) ->
    <<"您的拍賣物品，請注意查收。"/utf8>>;
tc(auction_success_title) ->
    <<"拍賣成功"/utf8>>;
tc(guild_create) ->
    <<"<id>~w</id>~s創建公會<id>~w</id>~s"/utf8>>;
tc(level_upgrade) ->
    <<"祝賀<id>~w</id>~s升到~w級"/utf8>>;
tc(test) ->
    <<"😂"/utf8>>;
tc(vip_upgrade) ->
    <<"祝賀<id>~w</id>~sVip升到~w級"/utf8>>;
tc(Key) ->
    Key.


sc(add_item_content) ->
    <<"您的背包已满，新增的道具已经放到了邮件里，请注意查收。"/utf8>>;
sc(add_item_title) ->
    <<"背包已满"/utf8>>;
sc(auction_income_content) ->
    <<"您的拍卖收入分成。"/utf8>>;
sc(auction_income_title) ->
    <<"拍卖收入"/utf8>>;
sc(auction_success_content) ->
    <<"您的拍卖物品，请注意查收。"/utf8>>;
sc(auction_success_title) ->
    <<"拍卖成功"/utf8>>;
sc(guild_create) ->
    <<"<id>~w</id>~s创建公会<id>~w</id>~s"/utf8>>;
sc(level_upgrade) ->
    <<"恭喜<id>~w</id>~s升到~w级"/utf8>>;
sc(test) ->
    <<"😂"/utf8>>;
sc(vip_upgrade) ->
    <<"恭喜<id>~w</id>~sVip升到~w级"/utf8>>;
sc(Key) ->
    Key.



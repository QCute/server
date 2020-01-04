-module(text_data).
-compile(nowarn_export_all).
-compile(export_all).


get(1) ->
    <<"不用买，随便爆"/utf8>>;
get(2) ->
    <<"是兄弟就来砍我"/utf8>>;
get(3) ->
    <<"卸载掉手机那个假传奇"/utf8>>;
get(add_item_content) ->
    <<"你的益达"/utf8>>;
get(add_item_title) ->
    <<"背包满"/utf8>>;
get(test) ->
    <<"😂"/utf8>>;
get(_) ->
    [].



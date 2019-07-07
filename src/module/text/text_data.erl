-module(text_data).
-compile(nowarn_export_all).
-compile(export_all).


get(1) ->
    <<"不用买，随便爆">>;
get(2) ->
    <<"是兄弟就来砍我">>;
get(3) ->
    <<"卸载掉手机那个假传奇">>;
get(add_item_content) ->
    <<"你的益达">>;
get(add_item_title) ->
    <<"背包满">>;
get(test) ->
    <<"">>;
get(_) -> 
    [].


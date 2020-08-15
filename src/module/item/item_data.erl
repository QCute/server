-module(item_data).
-compile(nowarn_export_all).
-compile(export_all).
-include("item.hrl").


get(1) ->
    #item_data{item_id = 1, type = 1, overlap = 1000, category = 0, time = 0, use_number = 0, use_effect = [], use_value = 0, name = <<"rust"/utf8>>, icon = <<"file_type_rust.svg"/utf8>>, description = <<""/utf8>>};
get(2) ->
    #item_data{item_id = 2, type = 1, overlap = 100, category = 0, time = 0, use_number = 0, use_effect = [], use_value = 0, name = <<"erlang"/utf8>>, icon = <<"file_type_erlang.svg"/utf8>>, description = <<""/utf8>>};
get(3) ->
    #item_data{item_id = 3, type = 1, overlap = 10, category = 0, time = 0, use_number = 0, use_effect = [], use_value = 0, name = <<"php"/utf8>>, icon = <<"file_type_php.svg"/utf8>>, description = <<""/utf8>>};
get(4) ->
    #item_data{item_id = 4, type = 2, overlap = 1, category = 0, time = 0, use_number = 0, use_effect = [], use_value = 0, name = <<"lua"/utf8>>, icon = <<"file_type_lua.svg"/utf8>>, description = <<""/utf8>>};
get(5) ->
    #item_data{item_id = 5, type = 2, overlap = 1, category = 0, time = 0, use_number = 0, use_effect = [], use_value = 0, name = <<"js"/utf8>>, icon = <<"file_type_js.svg"/utf8>>, description = <<""/utf8>>};
get(6) ->
    #item_data{item_id = 6, type = 2, overlap = 1, category = 0, time = 0, use_number = 0, use_effect = [], use_value = 0, name = <<"html"/utf8>>, icon = <<"file_type_html.svg"/utf8>>, description = <<""/utf8>>};
get(7) ->
    #item_data{item_id = 7, type = 2, overlap = 1, category = 0, time = 0, use_number = 0, use_effect = [], use_value = 0, name = <<"css"/utf8>>, icon = <<"file_type_css.svg"/utf8>>, description = <<""/utf8>>};
get(100001) ->
    #item_data{item_id = 100001, type = 10, overlap = 1, category = 0, time = 0, use_number = 0, use_effect = gold, use_value = 0, name = <<"gold"/utf8>>, icon = <<"file_type_gold.svg"/utf8>>, description = <<""/utf8>>};
get(100002) ->
    #item_data{item_id = 100002, type = 10, overlap = 1, category = 0, time = 0, use_number = 0, use_effect = silver, use_value = 0, name = <<"silver"/utf8>>, icon = <<"file_type_silver.svg"/utf8>>, description = <<""/utf8>>};
get(100003) ->
    #item_data{item_id = 100003, type = 10, overlap = 1, category = 0, time = 0, use_number = 0, use_effect = copper, use_value = 0, name = <<"copper"/utf8>>, icon = <<"file_type_copper.svg"/utf8>>, description = <<""/utf8>>};
get(100004) ->
    #item_data{item_id = 100004, type = 10, overlap = 1, category = 0, time = 0, use_number = 0, use_effect = exp, use_value = 0, name = <<"exp"/utf8>>, icon = <<"file_type_exp.svg"/utf8>>, description = <<""/utf8>>};
get(100005) ->
    #item_data{item_id = 100005, type = 10, overlap = 1, category = 0, time = 0, use_number = 0, use_effect = coin, use_value = 0, name = <<"coin"/utf8>>, icon = <<"file_type_coin.svg"/utf8>>, description = <<""/utf8>>};
get(_) ->
    [].



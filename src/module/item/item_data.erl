-module(item_data).
-compile(nowarn_export_all).
-compile(export_all).
-include("item.hrl").


get(1) ->
    #item_data{
        item_id = 1,
        type = 1,
        asset = [],
        overlap = 1000,
        name = <<"rust">>,
        icon = <<"file_type_rust.svg">>,
        description = <<"">>
    };
get(2) ->
    #item_data{
        item_id = 2,
        type = 1,
        asset = [],
        overlap = 100,
        name = <<"erlang">>,
        icon = <<"file_type_erlang.svg">>,
        description = <<"">>
    };
get(3) ->
    #item_data{
        item_id = 3,
        type = 1,
        asset = [],
        overlap = 10,
        name = <<"php">>,
        icon = <<"file_type_php.svg">>,
        description = <<"">>
    };
get(4) ->
    #item_data{
        item_id = 4,
        type = 2,
        asset = [],
        overlap = 1,
        name = <<"lua">>,
        icon = <<"file_type_lua.svg">>,
        description = <<"">>
    };
get(5) ->
    #item_data{
        item_id = 5,
        type = 2,
        asset = [],
        overlap = 1,
        name = <<"js">>,
        icon = <<"file_type_js.svg">>,
        description = <<"">>
    };
get(6) ->
    #item_data{
        item_id = 6,
        type = 2,
        asset = [],
        overlap = 1,
        name = <<"html">>,
        icon = <<"file_type_html.svg">>,
        description = <<"">>
    };
get(7) ->
    #item_data{
        item_id = 7,
        type = 2,
        asset = [],
        overlap = 1,
        name = <<"css">>,
        icon = <<"file_type_css.svg">>,
        description = <<"">>
    };
get(100001) ->
    #item_data{
        item_id = 100001,
        type = 101,
        asset = gold,
        overlap = 1,
        name = <<"gold">>,
        icon = <<"file_type_gold.svg">>,
        description = <<"">>
    };
get(100002) ->
    #item_data{
        item_id = 100002,
        type = 102,
        asset = sliver,
        overlap = 1,
        name = <<"silver">>,
        icon = <<"file_type_sliver.svg">>,
        description = <<"">>
    };
get(100003) ->
    #item_data{
        item_id = 100003,
        type = 103,
        asset = copper,
        overlap = 1,
        name = <<"copper">>,
        icon = <<"file_type_copper.svg">>,
        description = <<"">>
    };
get(100004) ->
    #item_data{
        item_id = 100004,
        type = 104,
        asset = exp,
        overlap = 1,
        name = <<"exp">>,
        icon = <<"file_type_exp.svg">>,
        description = <<"">>
    };
get(100005) ->
    #item_data{
        item_id = 100005,
        type = 105,
        asset = coin,
        overlap = 1,
        name = <<"coin">>,
        icon = <<"file_type_coin.svg">>,
        description = <<"">>
    };
get(_) -> 
    [].


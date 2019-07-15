-module(item_data).
-compile(nowarn_export_all).
-compile(export_all).
-include("item.hrl").


get() ->
    [
        #item_data{
            item_id = 1,
            name = <<"金币">>,
            type = 1,
            overlap = 1
        },
        #item_data{
            item_id = 2,
            name = <<"银币">>,
            type = 1,
            overlap = 1
        },
        #item_data{
            item_id = 3,
            name = <<"铜币">>,
            type = 1,
            overlap = 1
        }
    ].

get(1) ->
    #item_data{
        item_id = 1,
        name = <<"金币">>,
        type = 1,
        overlap = 1
    };
get(2) ->
    #item_data{
        item_id = 2,
        name = <<"银币">>,
        type = 1,
        overlap = 1
    };
get(3) ->
    #item_data{
        item_id = 3,
        name = <<"铜币">>,
        type = 1,
        overlap = 1
    };
get(_) -> 
    [].


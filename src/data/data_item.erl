-module(data_item).
-compile(nowarn_export_all).
-compile(export_all).
-include("item.hrl").


get() ->
    [
        #data_item{
            data_id = 1,
            name = <<"金币">>,
            type = 1,
            overlap = 1
        },
        #data_item{
            data_id = 2,
            name = <<"银币">>,
            type = 1,
            overlap = 1
        },
        #data_item{
            data_id = 3,
            name = <<"铜币">>,
            type = 1,
            overlap = 1
        }
    ].

get(1) ->
    #data_item{
        data_id = 1,
        name = <<"金币">>,
        type = 1,
        overlap = 1
    };
get(2) ->
    #data_item{
        data_id = 2,
        name = <<"银币">>,
        type = 1,
        overlap = 1
    };
get(3) ->
    #data_item{
        data_id = 3,
        name = <<"铜币">>,
        type = 1,
        overlap = 1
    };
get(_) -> 
    [].


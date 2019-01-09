-module(data_key).
-compile(nowarn_export_all).
-compile(export_all).
-include("key.hrl").


get(<<"b653f7199b878a79">>) ->
    1;
get(<<"b659bce3a0d44fe9">>) ->
    2;
get(_) -> 
    0.

award(1) ->
    #data_key_award{
        type = 1,
        only = 0,
        award = [{700001,1},{700002,2},{700003,3}]
    };
award(2) ->
    #data_key_award{
        type = 2,
        only = 0,
        award = [{700001,1},{700002,2},{700003,3}]
    };
award(_) -> 
    [].


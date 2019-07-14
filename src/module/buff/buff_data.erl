-module(buff_data).
-compile(nowarn_export_all).
-compile(export_all).
-include("buff.hrl").


get(1) ->
    #buff_data{
        buff_id = 1,
        group_id = 1,
        type = 1,
        time = 0,
        name = <<"扣血">>,
        effect = [5],
        temporary = 0,
        overlap_type = 0,
        replace_buffs = [],
        description = []
    };
get(_) -> 
    [].


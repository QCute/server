-module(buff_data).
-compile(nowarn_export_all).
-compile(export_all).
-include("buff.hrl").


get(1) ->
    #buff_data{buff_id = 1, type = 1, time = 0, name = <<"铜币"/utf8>>, effect = [9], temporary = 0, overlap_type = 1, replace_buffs = [], description = []};
get(2) ->
    #buff_data{buff_id = 2, type = 1, time = 0, name = <<"经验"/utf8>>, effect = [10], temporary = 0, overlap_type = 1, replace_buffs = [], description = []};
get(_) ->
    [].



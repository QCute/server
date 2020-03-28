-module(buff_data).
-compile(nowarn_export_all).
-compile(export_all).
-include("buff.hrl").


get(1) ->
    #buff_data{buff_id = 1, type = 1, time = 0, effect = [9], temporary = 0, overlap_type = 1, name = <<"铜币"/utf8>>, description = []};
get(2) ->
    #buff_data{buff_id = 2, type = 2, time = 60, effect = [10], temporary = 0, overlap_type = 2, name = <<"经验"/utf8>>, description = []};
get(3) ->
    #buff_data{buff_id = 3, type = 3, time = 120, effect = [9], temporary = 0, overlap_type = 3, name = <<"经验"/utf8>>, description = []};
get(4) ->
    #buff_data{buff_id = 4, type = 4, time = 0, effect = [10], temporary = 0, overlap_type = 2, name = <<"经验"/utf8>>, description = []};
get(5) ->
    #buff_data{buff_id = 5, type = 5, time = 0, effect = [10], temporary = 0, overlap_type = 1, name = <<"经验"/utf8>>, description = []};
get(6) ->
    #buff_data{buff_id = 6, type = 6, time = 0, effect = [9], temporary = 0, overlap_type = 0, name = <<"铜币"/utf8>>, description = []};
get(_) ->
    [].



-module(buff_data).
-compile(nowarn_export_all).
-compile(export_all).
-include("buff.hrl").


get(1) ->
    #buff_data{buff_id = 1, type = 1, expire_time = 1800, attribute = [], effect = [9], is_temporary = false, overlap_type = 3, name = <<"铜币"/utf8>>, description = []};
get(2) ->
    #buff_data{buff_id = 2, type = 1, expire_time = 3600, attribute = [], effect = [10], is_temporary = false, overlap_type = 3, name = <<"经验"/utf8>>, description = []};
get(3) ->
    #buff_data{buff_id = 3, type = 2, expire_time = 0, attribute = [{3,100}], effect = [], is_temporary = false, overlap_type = 2, name = <<"攻击"/utf8>>, description = []};
get(4) ->
    #buff_data{buff_id = 4, type = 2, expire_time = 0, attribute = [{4,100}], effect = [], is_temporary = false, overlap_type = 2, name = <<"防御"/utf8>>, description = []};
get(5) ->
    #buff_data{buff_id = 5, type = 2, expire_time = 60, attribute = [], effect = [3], is_temporary = false, overlap_type = 1, name = <<"眩晕"/utf8>>, description = []};
get(6) ->
    #buff_data{buff_id = 6, type = 3, expire_time = 60, attribute = [], effect = [5], is_temporary = false, overlap_type = 0, name = <<"扣血"/utf8>>, description = []};
get(_) ->
    [].



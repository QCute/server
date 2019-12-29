-module(key_award_data).
-compile(nowarn_export_all).
-compile(export_all).
-include("key.hrl").


award(1) ->
    #key_award_data{type = 1, only = 0, award = [{700001,1},{700002,2},{700003,3}]};
award(2) ->
    #key_award_data{type = 2, only = 0, award = [{700001,1},{700002,2},{700003,3}]};
award(_) ->
    [].



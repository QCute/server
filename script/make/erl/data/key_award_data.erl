-module(key_award_data).
-export([award/1]).
-include("key.hrl").

-spec award(Type :: non_neg_integer()) -> #key_award_data{}.
award(1) ->
    #key_award_data{type = 1, is_unique = false, award = [{700001,1},{700002,2},{700003,3}]};
award(2) ->
    #key_award_data{type = 2, is_unique = true, award = [{700001,1},{700002,2},{700003,3}]};
award(_) ->
    undefined.


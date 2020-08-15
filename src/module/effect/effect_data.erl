-module(effect_data).
-compile(nowarn_export_all).
-compile(export_all).
-include("effect.hrl").


get(1) ->
    #effect_data{effect_id = 1, scope = battle, object = self, operation = add, attribute = hurt, field = []};
get(2) ->
    #effect_data{effect_id = 2, scope = battle, object = self, operation = add, attribute = hurt, field = []};
get(3) ->
    #effect_data{effect_id = 3, scope = battle, object = self, operation = add, attribute = attribute, field = vertigo};
get(4) ->
    #effect_data{effect_id = 4, scope = battle, object = self, operation = reduce, attribute = attribute, field = vertigo};
get(5) ->
    #effect_data{effect_id = 5, scope = battle, object = self, operation = reduce, attribute = attribute, field = hp};
get(6) ->
    #effect_data{effect_id = 6, scope = battle, object = mate, operation = add, attribute = attribute, field = attack};
get(7) ->
    #effect_data{effect_id = 7, scope = battle, object = mate, operation = add, attribute = attribute, field = defense};
get(8) ->
    #effect_data{effect_id = 8, scope = battle, object = self, operation = add, attribute = buff, field = []};
get(9) ->
    #effect_data{effect_id = 9, scope = user, object = self, operation = add, attribute = asset, field = copper};
get(10) ->
    #effect_data{effect_id = 10, scope = user, object = self, operation = add, attribute = asset, field = exp};
get(_) ->
    [].



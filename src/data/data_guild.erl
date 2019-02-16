-module(data_guild).
-compile(nowarn_export_all).
-compile(export_all).
-include("guild.hrl").


param(cd, create) ->
    86400;
param(cd, join) ->
    86400;
param(create, 1) ->
    [{level, 10}, {vip, 0}, {gold, 0}];
param(create, 2) ->
    [{level, 10}, {vip, 1}, {gold, 100}];
param(_, _) -> 
    [].


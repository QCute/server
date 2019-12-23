%%%------------------------------------------------------------------
%%% @doc
%%% module map script
%%% @end
%%%------------------------------------------------------------------
-module(map_script).
-export([main/1]).
%%%==================================================================
%%% API functions
%%%==================================================================
main(_) ->
    code:add_path(filename:dirname(escript:script_name()) ++ "/../../../beam/"),
    Result = map_maker:start("config/map/", "src/data/data_map_point.erl"),
    io:format("~p~n", [Result]).

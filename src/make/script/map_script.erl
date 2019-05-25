%%%-------------------------------------------------------------------
%%% @doc
%%% module map script
%%% @end
%%%-------------------------------------------------------------------
-module(map_script).
-export([main/1]).
%%%===================================================================
%%% API
%%%===================================================================
main(_) ->
    ScriptPath = filename:dirname(escript:script_name()),
    Path = ScriptPath ++ "/../../../",
    code:add_path(Path ++ "/beam/"),
    Result = map_maker:start(Path ++ "config/map/", Path ++ "src/data/data_map_point.erl"),
    io:format("~p~n", [Result]).

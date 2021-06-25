%%%-------------------------------------------------------------------
%%% @doc
%%% map script for map maker
%%% @end
%%%-------------------------------------------------------------------
-module(map_script).
-export([main/1]).
-include("../../../include/journal.hrl").
%%%===================================================================
%%% API functions
%%%===================================================================
main(_) ->
    code:add_path(filename:dirname(escript:script_name()) ++ "/../../../beam/"),
    try
        io:format("~p~n", [map_maker:start("config/map/", "src/data/data_map_point.erl")])
    catch ?EXCEPTION(_Class, Reason, Stacktrace) ->
        ?ERROR_STACKTRACE(Reason, Stacktrace)
    end.

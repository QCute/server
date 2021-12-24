%%%-------------------------------------------------------------------
%%! +pc unicode
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
    io:setopts([{encoding, unicode}]),
    code:add_path(filename:dirname(escript:script_name()) ++ "/../../../beam/"),
    try
        io:format("~tp~n", [map_maker:start("config/map/", "src/data/data_map_point.erl")])
    catch ?EXCEPTION(Class, Reason, Stacktrace) ->
        ?HALT(Class, Reason, Stacktrace)
    end.

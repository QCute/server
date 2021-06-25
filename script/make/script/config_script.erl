%%%-------------------------------------------------------------------
%%% @doc
%%% config script for config maker
%%% @end
%%%-------------------------------------------------------------------
-module(config_script).
-export([main/1]).
-include("../../../include/journal.hrl").
%%%===================================================================
%%% API functions
%%%===================================================================
main(_) ->
    code:add_path(filename:dirname(escript:script_name()) ++ "/../../../beam/"),
    try
        io:format("~p~n", [config_maker:start("config/src/local.config.src", "src/tool/assistant/config.erl")])
    catch ?EXCEPTION(_Class, Reason, Stacktrace) ->
        ?ERROR_STACKTRACE(Reason, Stacktrace)
    end.

%%%-------------------------------------------------------------------
%%% @doc
%%% module config script
%%% @end
%%%-------------------------------------------------------------------
-module(config_script).
-export([main/1]).
%%%===================================================================
%%% API functions
%%%===================================================================
main(_) ->
    code:add_path(filename:dirname(escript:script_name()) ++ "/../../../beam/"),
    Result = config_maker:start("config/local.config", "src/tool/assistant/config.erl"),
    io:format("~p~n", [Result]).

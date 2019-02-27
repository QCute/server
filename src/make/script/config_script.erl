%%%-------------------------------------------------------------------
%%% @doc
%%% module config script
%%% @end
%%%-------------------------------------------------------------------
-module(config_script).
-export([main/1]).
%%%===================================================================
%%% API
%%%===================================================================
main(_) ->
    code:add_path("beam"),
    code:add_path("../beam"),
    code:add_path("../../beam"),
    code:add_path("../../../beam"),
    Result = config_maker:start("config/main.config", "src/tool/config.erl"),
    io:format("~p~n", [Result]).
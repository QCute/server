%%%-------------------------------------------------------------------
%%% @doc
%%% config script for config maker
%%% @end
%%%-------------------------------------------------------------------
-module(config_script).
-export([main/1]).
%%%===================================================================
%%% API functions
%%%===================================================================
main(_) ->
    code:add_path(filename:dirname(escript:script_name()) ++ "/../../../beam/"),
    io:format("~p~n", [catch config_maker:start("config/example/local.config.example", "src/tool/assistant/config.erl")]).

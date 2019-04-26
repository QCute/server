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
    ScriptPath = filename:dirname(escript:script_name()),
    Path = ScriptPath ++ "/../../../",
    code:add_path(Path ++ "/beam/"),
    Result = config_maker:start(Path ++ "config/main.config", Path ++"src/tool/config.erl"),
    io:format("~p~n", [Result]).

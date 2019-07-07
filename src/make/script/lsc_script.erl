%%%-------------------------------------------------------------------
%%% @doc
%%% module user load/save/clean script
%%% @end
%%%-------------------------------------------------------------------
-module(lsc_script).
-export([main/1]).
%%%===================================================================
%%% API
%%%===================================================================
main(_) ->
    ScriptPath = filename:dirname(escript:script_name()),
    Path = ScriptPath ++ "/../../../",
    code:add_path(Path ++ "/beam/"),
    Result = lsc_maker:start(Path ++ "include/user.hrl", Path ++ "src/module/role/role_loader.erl", Path ++ "src/module/role/role_saver.erl", Path ++ "src/module/role/role_cleaner.erl"),
    io:format("~p~n", [Result]).

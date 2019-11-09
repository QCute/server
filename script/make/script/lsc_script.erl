%%%------------------------------------------------------------------
%%% @doc
%%% module user load/save/clean script
%%% @end
%%%------------------------------------------------------------------
-module(lsc_script).
-export([main/1]).
%%%==================================================================
%%% API functions
%%%==================================================================
main(_) ->
    ScriptPath = filename:dirname(escript:script_name()),
    Path = ScriptPath ++ "/../../../",
    code:add_path(Path ++ "/beam/"),
    Result = lsc_maker:start(Path ++ "include/user.hrl", Path ++ "src/module/user/user_loader.erl", Path ++ "src/module/user/user_saver.erl", Path ++ "src/module/user/user_cleaner.erl"),
    io:format("~p~n", [Result]).

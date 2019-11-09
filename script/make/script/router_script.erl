%%%------------------------------------------------------------------
%%% @doc
%%% module router script
%%% @end
%%%------------------------------------------------------------------
-module(router_script).
-export([main/1]).
%%%==================================================================
%%% API functions
%%%==================================================================
main(_) ->
    ScriptPath = filename:dirname(escript:script_name()),
    Path = ScriptPath ++ "/../../../",
    code:add_path(Path ++ "/beam/"),
    Result = router_maker:start(Path ++ "script/make/protocol/", Path ++ "src/module/user/user_router.erl", router()),
    io:format("~p~n", [Result]).

%% ignore list (only except handle route)
router() ->
    [
        "account",
        "notice"
    ].

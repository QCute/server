%%%-------------------------------------------------------------------
%%% @doc
%%% module router script
%%% @end
%%%-------------------------------------------------------------------
-module(router_script).
-export([main/1]).
%%%===================================================================
%%% API
%%%===================================================================
main(_) ->
    ScriptPath = filename:dirname(escript:script_name()),
    Path = ScriptPath ++ "/../../../",
    code:add_path(Path ++ "/beam/"),
    Result = router_maker:start(Path ++ "src/make/protocol/", Path ++ "src/module/role/role_router.erl", router()),
    io:format("~p~n", [Result]).

%% ignore list (only except handle route)
router() ->
    [
        "account",
        "notice"
    ].

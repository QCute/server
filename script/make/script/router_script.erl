%%%-------------------------------------------------------------------
%%% @doc
%%% module router script
%%% @end
%%%-------------------------------------------------------------------
-module(router_script).
-export([main/1]).
%%%===================================================================
%%% API functions
%%%===================================================================
main(_) ->
    code:add_path(filename:dirname(escript:script_name()) ++ "/../../../beam/"),
    Result = router_maker:start("script/make/protocol/", "src/module/user/user_router.erl", router()),
    io:format("~p~n", [Result]).

%% ignore list (only except handle route)
router() ->
    [
        "account",
        "notice"
    ].

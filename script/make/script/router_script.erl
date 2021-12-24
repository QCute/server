%%%-------------------------------------------------------------------
%%! +pc unicode
%%% @doc
%%% router script for router maker
%%% @end
%%%-------------------------------------------------------------------
-module(router_script).
-export([main/1]).
-include("../../../include/journal.hrl").
%%%===================================================================
%%% API functions
%%%===================================================================
main(_) ->
    io:setopts([{encoding, unicode}]),
    code:add_path(filename:dirname(escript:script_name()) ++ "/../../../beam/"),
    Result = router_maker:start("script/make/protocol/", "src/module/user/user_router.erl", "include/protocol.hrl", router()),
    io:format("~tp~n", [Result]).

%% ignore list (only except handle route)
router() ->
    [
        "account",
        "notice"
    ].

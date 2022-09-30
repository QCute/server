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
    io:setopts(standard_error, [{encoding, unicode}]),
    code:add_path(filename:dirname(escript:script_name()) ++ "/../../../beam/"),
    Result = router_maker:start("script/make/protocol/", "src/module/user/user_router.erl", "include/protocol.hrl", "script/make/protocol/html/Protocol.html", "script/make/protocol/lua/ProtocolRouter.lua", "script/make/protocol/lua/meta/ProtocolDefine.lua", "script/make/protocol/js/ProtocolRouter.js", "script/make/protocol/js/meta/ProtocolDefine.js", "script/make/protocol/cs/ProtocolRouter.cs", "script/make/protocol/cs/meta/ProtocolDefine.cs", router()),
    io:format("~tp~n", [Result]).

%% ignore list (only except handle route)
router() ->
    [
        "account",
        "test"
    ].

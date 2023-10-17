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
    try
        io:format("~tp~n", [router_maker:start(router())])
    catch ?EXCEPTION(Class, Reason, Stacktrace) ->
        ?HALT(Class, Reason, Stacktrace)
    end.

%%%===================================================================
%%% loop options
%%%===================================================================
router() ->
    [
        #{
            file => "src/module/user/user_router.erl",
            path => "script/make/protocol/",
            lang => erl,
            type => router,
            ignore => [
                "account",
                "test"
            ]
        },
        #{
            file => "include/protocol.hrl",
            path => "script/make/protocol/",
            lang => erl,
            type => define
        },

        #{
            file => "script/make/protocol/lua/ProtocolRouter.lua",
            path => "script/make/protocol/",
            lang => lua,
            type => router
        },
        #{
            file => "script/make/protocol/lua/meta/ProtocolDefine.lua",
            path => "script/make/protocol/",
            lang => lua,
            type => define
        },

        #{
            file => "script/make/protocol/js/ProtocolRouter.js",
            path => "script/make/protocol/",
            lang => js,
            type => router
        },
        #{
            file => "script/make/protocol/js/meta/ProtocolDefine.js",
            path => "script/make/protocol/",
            lang => js,
            type => define
        },

        #{
            file => "script/make/protocol/cs/ProtocolRouter.cs",
            path => "script/make/protocol/",
            lang => cs,
            type => router
        },
        #{
            file => "script/make/protocol/cs/meta/ProtocolDefine.cs",
            path => "script/make/protocol/",
            lang => cs,
            type => define
        },

        #{
            file => "script/make/protocol/html/Protocol.html",
            path => "script/make/protocol/",
            lang => html,
            type => define
        }
    ].

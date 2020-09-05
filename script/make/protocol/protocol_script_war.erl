%%%-------------------------------------------------------------------
%%% @doc
%%% protocol read write define
%%% @end
%%%-------------------------------------------------------------------
-module(protocol_script_war).
-export([main/1]).
-include("../../../include/serialize.hrl").
%%%===================================================================
%%% API functions
%%%===================================================================
main([]) ->
    code:add_path(filename:dirname(escript:script_name()) ++ "/../../../beam/"),
    io:format("~p~n", [catch protocol_maker:start(protocol())]);
main(_) ->
    io:format("invalid argument~n").

%%%===================================================================
%%% protocol config
%%%===================================================================
protocol() ->
    [#protocol{
        number = 180,
        handler = "src/module/war/war_handler.erl",
        erl = "src/module/war/war_protocol.erl",
        js = "script/make/protocol/js/WarProtocol.js",
        lua = "script/make/protocol/lua/WarProtocol.lua",
        includes = [],
        io = [
            #io{
                protocol = 18001,
                comment = "挑战Boss",
                handler = #handler{module = boss_server, function = battle},
                read = [
                    #u32{name = monster_id, comment = "怪物Id"}
                ],
                write = [
                    #rst{name = result, comment = "结果"}
                ]
            }
        ]
    }].

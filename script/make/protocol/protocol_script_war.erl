%%%-------------------------------------------------------------------
%%! +pc unicode
%%% @doc
%%% protocol read write define
%%% @end
%%%-------------------------------------------------------------------
-module(protocol_script_war).
-export([main/1]).
-include("../../../include/journal.hrl").
-include("../../../include/serialize.hrl").
%%%===================================================================
%%% API functions
%%%===================================================================
main(_) ->
    code:add_path(filename:dirname(escript:script_name()) ++ "/../../../beam/"),
    try
        io:format("~tp~n", [protocol_maker:start(protocol())])
    catch ?EXCEPTION(Class, Reason, Stacktrace) ->
        ?HALT(Class, Reason, Stacktrace)
    end.

%%%===================================================================
%%% protocol config
%%%===================================================================
protocol() ->
    #protocol{
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
    }.

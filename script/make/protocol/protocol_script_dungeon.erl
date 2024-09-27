%%%-------------------------------------------------------------------
%%! +pc unicode
%%% @doc
%%% protocol read write define
%%% @end
%%%-------------------------------------------------------------------
-module(protocol_script_dungeon).
-export([main/1]).
-include("../../../include/journal.hrl").
-include("../../../include/serialize.hrl").
-include("../../../include/dungeon.hrl").
%%%===================================================================
%%% API functions
%%%===================================================================
main(_) ->
    io:setopts([{encoding, unicode}]),
    io:setopts(standard_error, [{encoding, unicode}]),
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
        number = 170,
        comment = "副本",
        erl = "script/make/protocol/erl/dungeon_protocol.erl",
        html = "script/make/protocol/html/DungeonProtocol.html",
        lua = "script/make/protocol/lua/DungeonProtocol.lua",
        js = "script/make/protocol/js/DungeonProtocol.js",
        cs = "script/make/protocol/cs/DungeonProtocol.cs",
        io = [
            #io{
                number = 17001,
                comment = "副本信息",
                handler = #handler{module = dungeon, function = query},
                decode = [],
                encode = [
                    #list{name = list, comment = "", explain = #dungeon{
                        dungeon_id = #u32{comment = "副本Id"},
                        today_number = #u16{comment = "今天次数"},
                        total_number = #u16{comment = "总次数"}
                    }}
                ]
            },
            #io{
                number = 17002,
                comment = "进入副本",
                handler = #handler{module = dungeon, function = enter},
                decode = [
                    #u32{name = dungeon_id, comment = "副本Id"}
                ],
                encode = [
                    #rst{name = result, comment = "结果"}
                ]
            },
            #io{
                number = 17003,
                comment = "副本开始",
                handler = #handler{alias = "start"},
                encode = [
                    #rst{name = result, comment = "结果"}
                ]
            },
            #io{
                number = 17004,
                comment = "副本结束",
                handler = #handler{alias = "over"},
                encode = [
                    #rst{name = result, comment = "结果"}
                ]
            },
            #io{
                number = 17005,
                comment = "副本鼓舞",
                handler = #handler{module = dungeon_map, function = inspire},
                decode = [],
                encode = [
                    #rst{name = result, comment = "结果"}
                ]
            }
        ]
    }.

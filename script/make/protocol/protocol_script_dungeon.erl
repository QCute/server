%%%------------------------------------------------------------------
%%% @doc
%%% module protocol read write
%%% @end
%%%------------------------------------------------------------------
-module(protocol_script_dungeon).
-export([main/1]).
-include("../../../include/serialize.hrl").
-include("../../../include/dungeon.hrl").
%%%==================================================================
%%% API functions
%%%==================================================================
main([]) ->
    code:add_path(filename:dirname(escript:script_name()) ++ "/../../../beam/"),
    io:format("~p~n", [catch protocol_maker:start(protocol())]);
main(_) ->
    io:format("invalid argument~n").

%%%==================================================================
%%% protocol config
%%%==================================================================
protocol() ->
    [#protocol{
        number = 170,
        handler = "src/module/dungeon/dungeon_handler.erl",
        erl = "src/module/dungeon/dungeon_protocol.erl",
        json = "script/make/protocol/json/DungeonProtocol.js",
        lua = "script/make/protocol/lua/DungeonProtocol.lua",
        includes = ["dungeon.hrl"],
        io = [
            #io{
                protocol = 17001,
                comment = "副本信息",
                handler = #handler{module = dungeon, function = query},
                read = [],
                write = [
                    #list{name = list, comment = "", explain = #dungeon{
                        dungeon_id = #u32{comment = "副本Id"},
                        today_number = #u16{comment = "今天次数"},
                        total_number = #u16{comment = "总次数"}
                    }}
                ]
            },
            #io{
                protocol = 17002,
                comment = "进入副本",
                handler = #handler{module = dungeon, function = enter},
                text = [{configure_not_found, "配置错误"}, {condition_not_met, "条件不满足"}, {today_number_limit, "今天进入次数已达到上限"}, {item_not_enough, "消耗材料不足"}],
                read = [
                    #u32{name = dungeon_id, comment = "副本Id"}
                ],
                write = [
                    #rst{name = result, comment = "结果"}
                ]
            },
            #io{
                protocol = 17003,
                comment = "副本开始",
                write = [
                    #rst{name = result, comment = "结果"}
                ]
            },
            #io{
                protocol = 17004,
                comment = "副本结束",
                write = [
                    #rst{name = result, comment = "结果"}
                ]
            }
        ]
    }].

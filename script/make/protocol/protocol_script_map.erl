%%%------------------------------------------------------------------
%%% @doc
%%% module protocol read write
%%% @end
%%%------------------------------------------------------------------
-module(protocol_script_map).
-export([main/1]).
-include("../../../include/serialize.hrl").
-include("../../../include/map.hrl").
%%%==================================================================
%%% API functions
%%%==================================================================
main([]) ->
    code:add_path(filename:dirname(escript:script_name()) ++ "/../../../beam/"),
    console:stacktrace(catch protocol_maker:start([protocol()]));
main(_) ->
    io:format("invalid argument~n").

%%%==================================================================
%%% protocol config
%%%==================================================================
protocol() ->
    #protocol{
        name = 200,
        handler = "src/module/map/map_handler.erl",
        erl = "src/module/map/map_protocol.erl",
        json = "script/make/protocol/json/MapProtocol.js",
        lua = "script/make/protocol/lua/MapProtocol.lua",
        includes = ["map.hrl"],
        io = [
            #io{
                name = 20001,
                comment = "Current Map",
                handler = #handler{module = map_server, function = query},
                read = [],
                write = [
                    #list{name = list, comment = "对象列表", explain = #fighter{
                        id = #u64{comment = "ID"},
                        type = #u8{comment = "类型"},
                        x = #u16{comment = "X坐标"},
                        y = #u16{comment = "Y坐标"}
                    }}
                ]
            },
            #io{
                name = 20007,
                comment = "Figther Move",
                handler = #handler{module = map_server, function = move},
                read = [
                    #u16{name = x, comment = "X坐标"},
                    #u16{name = y, comment = "Y坐标"}
                ],
                write = [
                    #fighter{
                        id = #u64{name = id, comment = "ID"},
                        x = #u16{name = x, comment = "X坐标"},
                        y = #u16{name = y, comment = "Y坐标"}
                    }
                ]
            },
            #io{
                name = 20008,
                comment = "Monster Move",
                read = [],
                write = [
                    #fighter{
                        id = #u64{name = id, comment = "ID"},
                        x = #u16{name = x, comment = "X坐标"},
                        y = #u16{name = y, comment = "Y坐标"}
                    }
                ]
            }
        ]
    }.

%%%-------------------------------------------------------------------
%%% @doc
%%% module protocol read write
%%% @end
%%%-------------------------------------------------------------------
-module(protocol_script_map).
-export([main/1]).
-include("../../../include/serialize.hrl").
-include("../../../include/map.hrl").
%%%===================================================================
%%% API
%%%===================================================================
main([]) ->
    code:add_path(filename:dirname(escript:script_name()) ++ "/../../../beam/"),
    console:stacktrace(catch protocol_maker:start([protocol()]));
main(_) ->
    io:format("invail argument~n").

%%%===================================================================
%%% protocol config
%%%===================================================================
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
                read = [],
                write = [
                    #u32{name = map_id, comment = "地图ID"},
                    #u16{name = x, comment = "X坐标"},
                    #u16{name = y, comment = "Y坐标"}
                ],
                handler = #handler{
                    module = map_server,
                    function = push
                }
            },
            #io{
                name = 20002,
                comment = "Map Move",
                read = [
                    #u16{name = x, comment = "X坐标"},
                    #u16{name = y, comment = "Y坐标"}
                ],
                write = [
                    #u16{name = x, comment = "X坐标"},
                    #u16{name = y, comment = "Y坐标"}
                ],
                handler = #handler{
                    module = map_server,
                    function = move
                }
            }
        ]
    }.

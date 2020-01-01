%%%------------------------------------------------------------------
%%% @doc
%%% module protocol read write
%%% @end
%%%------------------------------------------------------------------
-module(protocol_script_map).
-export([main/1]).
-include("../../../include/serialize.hrl").
-include("../../../include/map.hrl").
-include("../../../include/attribute.hrl").
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
        name = 200,
        handler = "src/module/map/map_handler.erl",
        erl = "src/module/map/map_protocol.erl",
        json = "script/make/protocol/json/MapProtocol.js",
        lua = "script/make/protocol/lua/MapProtocol.lua",
        includes = ["map.hrl", "attribute.hrl"],
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
                name = 20002,
                comment = "自身信息",
                handler = #handler{module = map_server, function = query},
                write = [
                    #fighter{
                        id = #u64{comment = "ID"},
                        type = #u8{comment = "类型"},
                        attribute = #record{explain = #attribute{
                            fc = #u64{name = fc, comment = "战力"},
                            hp = #u64{name = hp, comment = "血量"},
                            health = #u64{name = health, comment = "原始血量"}
                        }},
                        x = #u16{comment = "X坐标"},
                        y = #u16{comment = "Y坐标"}
                    }
                ]
            },
            #io{
                name = 20003,
                comment = "战斗对象列表",
                handler = #handler{module = map_server, function = query},
                write = [
                    #list{name = list, comment = "对象列表", explain = #fighter{
                        id = #u64{comment = "ID"},
                        type = #u8{comment = "类型"},
                        attribute = #record{explain = #attribute{
                            hp = #u64{name = hp, comment = "血量"}
                        }},
                        x = #u16{comment = "X坐标"},
                        y = #u16{comment = "Y坐标"}
                    }}
                ]
            },
            #io{
                name = 20004,
                comment = "战斗对象移动",
                handler = #handler{module = map_server, function = query},
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
                name = 20005,
                comment = "删除战斗对象",
                handler = #handler{module = map_server, function = query},
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
    }].

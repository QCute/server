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
        number = 200,
        handler = "src/module/map/map_handler.erl",
        erl = "src/module/map/map_protocol.erl",
        json = "script/make/protocol/json/MapProtocol.js",
        lua = "script/make/protocol/lua/MapProtocol.lua",
        includes = ["map.hrl", "attribute.hrl"],
        io = [
            #io{
                protocol = 20001,
                comment = "地图信息",
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
                protocol = 20002,
                comment = "自身信息",
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
                protocol = 20003,
                comment = "战斗对象列表",
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
                protocol = 20004,
                comment = "战斗对象移动",
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
                protocol = 20005,
                comment = "删除战斗对象",
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
                protocol = 20006,
                comment = "玩家移动",
                handler = #handler{module = map_server, function = move},
                read = [
                    #u16{name = x, comment = "X坐标"},
                    #u16{name = y, comment = "Y坐标"}
                ]
            },
            #io{
                protocol = 20007,
                comment = "发起战斗",
                handler = #handler{module = map_server, function = attack},
                read = [
                    #u32{name = skill_id, comment = "技能Id"},
                    #list{name = target_list, comment = "对象列表", explain =
                        #u64{name = target_id, comment = "ID"}
                    }
                ]
            }
        ]
    }].

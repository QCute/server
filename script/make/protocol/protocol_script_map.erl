%%%-------------------------------------------------------------------
%%! +pc unicode
%%% @doc
%%% protocol read write define
%%% @end
%%%-------------------------------------------------------------------
-module(protocol_script_map).
-export([main/1]).
-include("../../../include/journal.hrl").
-include("../../../include/serialize.hrl").
-include("../../../include/map.hrl").
-include("../../../include/attribute.hrl").
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
        number = 200,
        comment = "地图",
        handler = "src/module/map/map_handler.erl",
        erl = "src/module/map/map_protocol.erl",
        html = "script/make/protocol/html/MapProtocol.html",
        lua = "script/make/protocol/lua/MapProtocol.lua",
        js = "script/make/protocol/js/MapProtocol.js",
        cs = "script/make/protocol/cs/MapProtocol.cs",
        includes = ["map.hrl", "attribute.hrl"],
        io = [
            #io{
                protocol = 20001,
                comment = "地图信息",
                handler = #handler{module = map_server, function = query},
                read = [],
                write = []
            },


            #io{
                protocol = 20011,
                comment = "战斗对象列表",
                handler = #handler{module = map_server, function = fighter_list, alias = "fighter"},
                read = [],
                write = [
                    #list{name = list, comment = "对象列表", explain = #fighter{
                        id = #u64{comment = "ID"},
                        type = #u8{comment = "类型"},
                        attribute = #tuple{explain = #attribute{
                            fc = #u64{comment = "战力"},
                            hp = #u64{comment = "血量"},
                            health = #u64{comment = "健康"}
                        }},
                        skill = #list{comment = "技能列表", explain = #battle_skill{
                            skill_id = #u32{comment = "技能ID"},
                            time = #u32{comment = "时间"},
                            number = #u32{comment = "数量"}
                        }},
                        buff = #list{comment = "Buff列表", explain = #battle_buff{
                            buff_id = #u32{comment = "BuffID"},
                            expire_time = #u32{comment = "过期时间"},
                            overlap = #u32{comment = "数量"}
                        }},
                        x = #u16{comment = "X坐标"},
                        y = #u16{comment = "Y坐标"}
                    }}
                ]
            },
            #io{
                protocol = 20012,
                comment = "战斗对象移动",
                handler = #handler{module = map_server, function = move, alias = "fighter_move"},
                read = [
                    #u16{name = x, comment = "x坐标"},
                    #u16{name = y, comment = "y坐标"}
                ],
                write = [
                    #fighter{
                        id = #u64{comment = "ID"},
                        x = #u16{comment = "X坐标"},
                        y = #u16{comment = "Y坐标"}
                    }
                ]
            },
            #io{
                protocol = 20013,
                comment = "战斗对象离开",
                handler = #handler{alias = "fighter_leave"},
                write = [
                    #fighter{
                        id = #u64{comment = "ID"}
                    }
                ]
            },
            #io{
                protocol = 20014,
                comment = "发起战斗",
                handler = #handler{module = map_server, function = attack},
                read = [
                    #u32{name = skill_id, comment = "技能Id"},
                    #list{name = target_list, comment = "对象列表", explain =
                        #u64{name = target_id, comment = "ID"}
                    }
                ],
                write = [
                    #u64{name = fighter_id, comment = "战斗对象Id"},
                    #u32{name = perform_skill_id, comment = "技能Id"},
                    #list{name = list, comment = "对象列表", explain = #fighter{
                        id = #u64{comment = "ID"},
                        type = #u8{comment = "类型"},
                        attribute = #tuple{explain = #attribute{
                            fc = #u64{comment = "战力"},
                            hp = #u64{comment = "血量"},
                            health = #u64{comment = "健康"}
                        }},
                        skill = #list{comment = "技能列表", explain = #battle_skill{
                            skill_id = #u32{comment = "技能ID"},
                            time = #u32{comment = "时间"},
                            number = #u32{comment = "数量"}
                        }},
                        buff = #list{comment = "Buff列表", explain = #battle_buff{
                            buff_id = #u32{comment = "BuffID"},
                            expire_time = #u32{comment = "过期时间"},
                            overlap = #u32{comment = "数量"}
                        }},
                        x = #u16{comment = "X坐标"},
                        y = #u16{comment = "Y坐标"}
                    }}
                ]
            }
        ]
    }.

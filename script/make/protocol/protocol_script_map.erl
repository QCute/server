%%%-------------------------------------------------------------------
%%! +pc unicode -pa beam
%%% @doc
%%% protocol read write define
%%% @end
%%%-------------------------------------------------------------------
-module(protocol_script_map).
-mode(compile).
-compile({parse_transform, protocol_maker_transform}).
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
    ets:insert(ets:new(shell_records, [set, public]), [{Tag, Form} || Form = {attribute, _, record, {Tag, _}} <- lists:append([element(2, epp:parse_file(Header, [], [])) || Header <- filelib:wildcard(filename:dirname(escript:script_name()) ++ "/../../../include/*.hrl")])]),
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
        erl = "script/make/protocol/erl/map_protocol.erl",
        html = "script/make/protocol/html/MapProtocol.html",
        lua = "script/make/protocol/lua/MapProtocol.lua",
        js = "script/make/protocol/js/MapProtocol.js",
        cs = "script/make/protocol/cs/MapProtocol.cs",
        io = [
            #io{
                number = 20001,
                comment = "地图信息",
                handler = #handler{module = map_server, function = query},
                decode = {},
                encode = #map{
                    map_no = u64(),                        %% 地图编号
                    map_id = u32(),                        %% 地图ID
                    fighter = [
                        #fighter{
                            id = u64(),                    %% ID
                            type = u8(),                   %% 类型
                            attribute = #attribute{        %% 属性
                                fc = u64(),                %% 战力
                                hp = u64(),                %% 血量
                                health = u64()             %% 健康
                            },
                            skill = [                      %% 技能列表
                                #battle_skill{
                                    skill_id = u32(),      %% 技能ID
                                    time = u32(),          %% 时间
                                    number = u32()         %% 数量
                                }
                            ],
                            buff = [                       %% Buff列表
                                #battle_buff{
                                    buff_id = u32(),       %% BuffID
                                    expire_time = u32(),   %% 过期时间
                                    overlap = u32()        %% 数量
                                }
                            ],
                            x = u16(),                     %% X坐标
                            y = u16()                      %% Y坐标
                        }
                    ]
                }
            },
            #io{
                number = 20011,
                comment = "战斗对象列表",
                handler = #handler{module = map_server, function = fighter_list, alias = "fighter"},
                encode = [
                    #fighter{
                        id = u64(),                        %% ID
                        type = u8(),                       %% 类型
                        attribute = #attribute{            %% 属性
                            fc = u64(),                    %% 战力
                            hp = u64(),                    %% 血量
                            health = u64()                 %% 健康
                        },
                        skill = [                          %% 技能列表
                            #battle_skill{
                                skill_id = u32(),          %% 技能ID
                                time = u32(),              %% 时间
                                number = u32()             %% 数量
                            }
                        ],
                        buff = [                           %% Buff列表
                            #battle_buff{
                                buff_id = u32(),           %% BuffID
                                expire_time = u32(),       %% 过期时间
                                overlap = u32()            %% 数量
                            }
                        ],
                        x = u16(),                         %% X坐标
                        y = u16()                          %% Y坐标
                    }
                ]
            },
            #io{
                number = 20012,
                comment = "战斗对象移动",
                handler = #handler{module = map_server, function = move, alias = "fighter_move"},
                decode = {
                    x = u16(),                             %% X坐标
                    y = u16()                              %% Y坐标
                },
                encode = #fighter{
                    id = u64(),                            %% ID
                    x = u16(),                             %% X坐标
                    y = u16()                              %% Y坐标
                }
            },
            #io{
                number = 20013,
                comment = "战斗对象离开",
                handler = #handler{alias = "fighter_leave"},
                encode = #fighter{
                    id = u64()                             %% 战斗对象ID
                }
            },
            #io{
                number = 20014,
                comment = "发起战斗",
                handler = #handler{module = map_server, function = attack},
                decode = {
                    skill_id = u32(),                      %% 技能Id
                    target_list = [                        %% 战斗对象ID列表
                        u64()                              %% 战斗对象ID
                    ]
                },
                encode = {
                    fighter_id = u64(),                    %% 战斗对象Id
                    perform_skill_id = u32(),              %% 技能Id
                    fighter_list = [
                        #fighter{
                            id = u64(),                    %% ID
                            type = u8(),                   %% 类型
                            attribute = #attribute{        %% 属性
                                fc = u64(),                %% 战力
                                hp = u64(),                %% 血量
                                health = u64()             %% 健康
                            },
                            skill = [                      %% 技能列表
                                #battle_skill{
                                    skill_id = u32(),      %% 技能ID
                                    time = u32(),          %% 时间
                                    number = u32()         %% 数量
                                }
                            ],
                            buff = [                       %% Buff列表
                                #battle_buff{
                                    buff_id = u32(),       %% BuffID
                                    expire_time = u32(),   %% 过期时间
                                    overlap = u32()        %% 数量
                                }
                            ],
                            x = u16(),                     %% X坐标
                            y = u16()                      %% Y坐标
                        }
                    ]
                }
            }
        ]
    }.

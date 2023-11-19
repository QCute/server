%%%-------------------------------------------------------------------
%%! +pc unicode -pa beam
%%% @doc
%%% protocol read write define
%%% @end
%%%-------------------------------------------------------------------
-module(protocol_script_rank).
-mode(compile).
-compile({parse_transform, protocol_maker_transform}).
-export([main/1]).
-include("../../../include/journal.hrl").
-include("../../../include/serialize.hrl").
-include("../../../include/rank.hrl").
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
        number = 190,
        comment = "排行榜",
        erl = "script/make/protocol/erl/rank_protocol.erl",
        lua = "script/make/protocol/lua/RankProtocol.lua",
        js = "script/make/protocol/js/RankProtocol.js",
        cs = "script/make/protocol/cs/RankProtocol.cs",
        html = "script/make/protocol/html/RankProtocol.html",
        io = [
            #io{
                number = 19001,
                comment = "等级榜",
                handler = #handler{module = rank_server, function = query, alias = level, protocol = true},
                decode = {},
                encode = [
                    #rank{
                        type = u16(),                      %% 类型
                        key = u64(),                       %% 键
                        value = u64(),                     %% 值
                        time = u32(),                      %% 时间
                        order = u64(),                     %% 排名
                        name = bst(),                      %% 名字
                        server_id = u16()                  %% 服务器ID
                    }
                ]
            },
            #io{
                number = 19002,
                comment = "战力榜",
                handler = #handler{module = rank_server, function = query, alias = fight, protocol = true},
                decode = {},
                encode = [
                    #rank{
                        type = u16(),                      %% 类型
                        key = u64(),                       %% 键
                        value = u64(),                     %% 值
                        time = u32(),                      %% 时间
                        order = u64(),                     %% 排名
                        name = bst(),                      %% 名字
                        server_id = u16(),                 %% 服务器ID
                        other = {
                            level = u16(),                 %% 等级
                            classes = u8()                 %% 职业
                        }
                    }
                ]
            },
            #io{
                number = 19003,
                comment = "成就榜",
                handler = #handler{module = rank_server, function = query, alias = achievement, protocol = true},
                decode = {},
                encode = [
                    #rank{
                        type = u16(),                      %% 类型
                        key = u64(),                       %% 键
                        value = u64(),                     %% 值
                        time = u32(),                      %% 时间
                        order = u64(),                     %% 排名
                        name = bst(),                      %% 名字
                        server_id = u16(),                 %% 服务器ID
                        other = {
                            level = u16(),                 %% 等级
                            classes = u8(),                %% 职业
                            sex = u8()                     %% 性别
                        }
                    }
                ]
            },
            #io{
                number = 19004,
                comment = "财富榜",
                handler = #handler{module = rank_server, function = query, alias = wealth, protocol = true},
                decode = {},
                encode = [
                    #rank{
                        type = u16(),                      %% 类型
                        key = u64(),                       %% 键
                        value = u64(),                     %% 值
                        time = u32(),                      %% 时间
                        order = u64(),                     %% 排名
                        name = bst(),                      %% 名字
                        server_id = u16(),                 %% 服务器ID
                        other = {
                            level = u16(),                 %% 等级
                            classes = u8(),                %% 职业
                            sex = u8(),                    %% 性别
                            vip_level = u8()               %% VIP等级
                        }
                    }
                ]
            },
            #io{
                number = 19005,
                comment = "职业榜",
                handler = #handler{module = rank_server, function = query, alias = classes, protocol = true},
                decode = {},
                encode = [
                    #rank{
                        type = u16(),                      %% 类型
                        key = u64(),                       %% 键
                        value = u64(),                     %% 值
                        time = u32(),                      %% 时间
                        order = u64(),                     %% 排名
                        name = bst(),                      %% 名字
                        server_id = u16(),                 %% 服务器ID
                        other = {
                            level = u16(),                 %% 等级
                            classes = u8(),                %% 职业
                            sex = u8(),                    %% 性别
                            vip_level = u8(),              %% VIP等级
                            avatar = u8()                  %% 头像
                        }
                    }
                ]
            }
        ]
    }.

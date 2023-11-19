%%%-------------------------------------------------------------------
%%! +pc unicode -pa beam
%%% @doc
%%% protocol read write define
%%% @end
%%%-------------------------------------------------------------------
-module(protocol_script_item).
-mode(compile).
-compile({parse_transform, protocol_maker_transform}).
-export([main/1]).
-include("../../../include/journal.hrl").
-include("../../../include/serialize.hrl").
-include("../../../include/item.hrl").
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
        number = 111,
        comment = "物品",
        erl = "script/make/protocol/erl/item_protocol.erl",
        html = "script/make/protocol/html/ItemProtocol.html",
        lua = "script/make/protocol/lua/ItemProtocol.lua",
        js = "script/make/protocol/js/ItemProtocol.js",
        cs = "script/make/protocol/cs/ItemProtocol.cs",
        io = [
            #io{
                number = 11101,
                comment = "道具列表",
                handler = #handler{module = item, function = query_item},
                decode = {},
                encode = [
                    #item{
                        item_no = u64(),                   %% 物品编号
                        item_id = u32(),                   %% 物品ID
                        type = u8(),                       %% 类型
                        number = u16()                     %% 数量
                    }
                ]
            },
            #io{
                number = 11102,
                comment = "背包列表",
                handler = #handler{module = item, function = query_bag},
                decode = {},
                encode = [
                    #item{
                        item_no = u64(),                   %% 物品编号
                        item_id = u32(),                   %% 物品ID
                        type = u8(),                       %% 类型
                        number = u16()                     %% 数量
                    }
                ]
            },
            #io{
                number = 11103,
                comment = "仓库列表",
                handler = #handler{module = item, function = query_store},
                decode = {},
                encode = [
                    #item{
                        item_no = u64(),                   %% 物品编号
                        item_id = u32(),                   %% 物品ID
                        type = u8(),                       %% 类型
                        number = u16()                     %% 数量
                    }
                ]
            },
            #io{
                number = 11104,
                comment = "删除物品",
                handler = #handler{alias = "delete"},
                encode = [
                    #item{
                        item_no = u64(),                   %% 物品编号
                        item_id = u32(),                   %% 物品ID
                        type = u8()                        %% 类型
                    }
                ]
            },
            #io{
                number = 11106,
                comment = "使用物品",
                handler = #handler{module = item_use, function = use},
                decode = {
                    item_no = u64(),                       %% 物品编号
                    number = u16(),                        %% 数量
                    type = u8()                            %% 类型
                },
                encode = ast()                             %% 结果
            }
        ]
    }.

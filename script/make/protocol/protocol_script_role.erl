%%%-------------------------------------------------------------------
%%% @doc
%%% protocol read write define
%%% @end
%%%-------------------------------------------------------------------
-module(protocol_script_role).
-export([main/1]).
-include("../../../include/journal.hrl").
-include("../../../include/serialize.hrl").
-include("../../../include/role.hrl").
-include("../../../include/asset.hrl").
-include("../../../include/vip.hrl").
%%%===================================================================
%%% API functions
%%%===================================================================
main(_) ->
    code:add_path(filename:dirname(escript:script_name()) ++ "/../../../beam/"),
    try
        io:format("~p~n", [protocol_maker:start(protocol())])
    catch ?EXCEPTION(Class, Reason, Stacktrace) ->
        ?ERROR_STACKTRACE(Class, Reason, Stacktrace)
    end.

%%%===================================================================
%%% protocol config
%%%===================================================================
protocol() ->
    [#protocol{
        number = 101,
        handler = "src/module/role/role_handler.erl",
        erl = "src/module/role/role_protocol.erl",
        js = "script/make/protocol/js/RoleProtocol.js",
        lua = "script/make/protocol/lua/RoleProtocol.lua",
        includes = ["role.hrl", "asset.hrl", "vip.hrl"],
        io = [
            #io{
                protocol = 10101,
                comment = "角色",
                handler = #handler{module = role, function = query},
                read = [],
                write = [
                    #role{
                        role_id = #u64{comment = "角色ID"},
                        role_name = #bst{comment = "角色名"},
                        sex = #u8{comment = "性别"},
                        level = #u64{comment = "等级"},
                        classes = #u8{comment = "职业"},
                        item_size = #u16{comment = "普通背包大小"},
                        bag_size = #u16{comment = "装备背包大小"},
                        store_size = #u16{comment = "仓库背包大小"} 
                    }
                ]
            },
            #io{
                protocol = 10102,
                comment = "资产",
                handler = #handler{module = asset, function = query, alias = "asset_query"},
                read = [],
                write = [
                    #asset{
                        gold = #u64{comment = "金币"},                          %% Gold
                        silver = #u32{comment = "银币"},                        %% Silver
                        copper = #u64{comment = "铜币"},                        %% Copper
                        exp = #u64{comment = "经验"}                            %% Exp
                    }
                ]
            },
            #io{
                protocol = 10103,
                comment = "vip",
                handler = #handler{module = vip, function = query, alias = "vip_query"},
                read = [],
                write = [
                    #vip{
                        vip_level = #u8{comment = "等级"},
                        exp = #u64{comment = "经验"},
                        expire_time = #u32{comment = "过期时间"}
                    }
                ]
            }
        ]
    }].

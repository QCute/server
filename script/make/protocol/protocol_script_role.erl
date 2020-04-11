%%%-------------------------------------------------------------------
%%% @doc
%%% module protocol read write
%%% @end
%%%-------------------------------------------------------------------
-module(protocol_script_role).
-export([main/1]).
-include("../../../include/serialize.hrl").
-include("../../../include/role.hrl").
%%%===================================================================
%%% API functions
%%%===================================================================
main([]) ->
    code:add_path(filename:dirname(escript:script_name()) ++ "/../../../beam/"),
    io:format("~p~n", [catch protocol_maker:start(protocol())]);
main(_) ->
    io:format("invalid argument~n").

%%%===================================================================
%%% protocol config
%%%===================================================================
protocol() ->
    [#protocol{
        number = 101,
        handler = "src/module/role/role_handler.erl",
        erl = "src/module/role/role_protocol.erl",
        json = "script/make/protocol/json/RoleProtocol.js",
        lua = "script/make/protocol/lua/RoleProtocol.lua",
        includes = ["role.hrl"],
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
            }
        ]
    }].

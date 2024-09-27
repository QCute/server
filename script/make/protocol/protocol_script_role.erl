%%%-------------------------------------------------------------------
%%! +pc unicode
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
        number = 101,
        comment = "角色",
        erl = "script/make/protocol/erl/role_protocol.erl",
        html = "script/make/protocol/html/RoleProtocol.html",
        lua = "script/make/protocol/lua/RoleProtocol.lua",
        js = "script/make/protocol/js/RoleProtocol.js",
        cs = "script/make/protocol/cs/RoleProtocol.cs",
        io = [
            #io{
                number = 10101,
                comment = "角色",
                handler = #handler{module = role, function = query, alias = query},
                decode = [],
                encode = [
                    #role{
                        role_id = #u64{comment = "角色ID"},
                        role_name = #bst{comment = "角色名"},
                        sex = #u8{comment = "性别"},
                        level = #u64{comment = "等级"},
                        classes = #u8{comment = "职业"}
                    }
                ]
            },
            #io{
                number = 10102,
                comment = "资产",
                handler = #handler{module = asset, function = query, alias = asset_query},
                decode = [],
                encode = [
                    #asset{
                        gold = #u64{comment = "金币"},                          %% Gold
                        silver = #u32{comment = "银币"},                        %% Silver
                        copper = #u64{comment = "铜币"},                        %% Copper
                        exp = #u64{comment = "经验"}                            %% Exp
                    }
                ]
            },
            #io{
                number = 10103,
                comment = "vip",
                handler = #handler{module = vip, function = query, alias = vip_query},
                decode = [],
                encode = [
                    #vip{
                        vip_level = #u8{comment = "等级"},
                        exp = #u64{comment = "经验"},
                        expire_time = #u32{comment = "过期时间"}
                    }
                ]
            }
        ]
    }.

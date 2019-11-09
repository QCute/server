%%%------------------------------------------------------------------
%%% @doc
%%% module protocol read write
%%% @end
%%%------------------------------------------------------------------
-module(protocol_script_asset).
-export([main/1]).
-include("../../../include/serialize.hrl").
-include("../../../include/asset.hrl").
%%%==================================================================
%%% API functions
%%%==================================================================
main([]) ->
    code:add_path(filename:dirname(escript:script_name()) ++ "/../../../beam/"),
    console:stacktrace(catch protocol_maker:start([protocol()]));
main(_) ->
    io:format("invalid argument~n").

%%%==================================================================
%%% protocol config
%%%==================================================================
protocol() ->
    #protocol{
        name = 102,
        handler = "src/module/asset/asset_handler.erl",
        erl = "src/module/asset/asset_protocol.erl",
        json = "script/make/protocol/json/AssetProtocol.js",
        lua = "script/make/protocol/lua/AssetProtocol.lua",
        includes = ["asset.hrl"],
        io = [
            #io{
                name = 10201,
                comment = "Assets",
                handler = #handler{module = asset, function = query},
                read = [],
                write = [
                    #asset{
                        gold = #u64{comment = "金币"},                          %% Gold
                        silver = #u32{comment = "银币"},                        %% Silver
                        copper = #u64{comment = "铜币"},                        %% Copper
                        exp = #u64{comment = "经验"}                            %% Exp
                    }
                ]
            }
        ]
    }.

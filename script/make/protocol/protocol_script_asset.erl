%%%-------------------------------------------------------------------
%%% @doc
%%% module protocol read write
%%% @end
%%%-------------------------------------------------------------------
-module(protocol_script_asset).
-export([main/1]).
-include("../../../include/serialize.hrl").
-include("../../../include/asset.hrl").
%%%===================================================================
%%% API
%%%===================================================================
main([]) ->
    code:add_path(filename:dirname(escript:script_name()) ++ "/../../../beam/"),
    Protocol = #protocol{erl = File} = protocol(),
    console:stacktrace(catch protocol_maker:start([{File, Protocol}]));
main(_) ->
    io:format("invail argument~n").

%%%===================================================================
%%% protocol config
%%%===================================================================
protocol() ->
    #protocol{
        name = 102,
        erl = "src/module/asset/asset_protocol.erl",
        includes = ["asset.hrl"],
        io = [
            #io{
                name = 10201,
                comment = "Assets",
                read = [],
                write = [
                    #asset{
                        gold = #u64{},                          %% Gold
                        silver = #u32{},                        %% Silver
                        copper = #u64{},                        %% Copper
                        exp = #u64{}                            %% Exp
                    }
                ]
            }
        ]
    }.

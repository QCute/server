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
    Protocol = #protocol{file = File} = protocol(),
    console:stacktrace(catch maker:start(fun protocol_maker:parse/2, [{File, Protocol}])),
    ok;
main(_) ->
    io:format("invail argument~n").

%%%===================================================================
%%% protocol config
%%%===================================================================
protocol() ->
    #protocol{
        name = 102,
        file = "src/module/asset/asset_protocol.erl",
        include = ["asset.hrl"],
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

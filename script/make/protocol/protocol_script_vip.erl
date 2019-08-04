%%%-------------------------------------------------------------------
%%% @doc
%%% module protocol read write
%%% @end
%%%-------------------------------------------------------------------
-module(protocol_script_vip).
-export([main/1]).
-include("../../../include/serialize.hrl").
-include("../../../include/vip.hrl").
%%%===================================================================
%%% API
%%%===================================================================
main([]) ->
    code:add_path(filename:dirname(escript:script_name()) ++ "/../../../beam/"),
    Protocol = #protocol{file = File} = protocol(),
    console:stacktrace(catch protocol_maker:start([{File, Protocol}]));
main(_) ->
    io:format("invail argument~n").

%%%===================================================================
%%% protocol config
%%%===================================================================
protocol() ->
    #protocol{
        name = 103,
        file = "src/module/vip/vip_protocol.erl",
        include = ["vip.hrl"],
        io = [
            #io{
                name = 10301,
                comment = "vip",
                read = [],
                write = [
                    #vip{
                        level = #u8{},                       %% level
                        exp = #u64{},                        %% exp
                        expire_time = #u32{}                 %% expire time
                    }
                ]
            }
        ]
    }.

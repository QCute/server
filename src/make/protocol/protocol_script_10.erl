%%%-------------------------------------------------------------------
%%% @doc
%%% module protocol read
%%% @end
%%%-------------------------------------------------------------------
-module(protocol_script_10).
-export([main/1]).
-include("../../../include/serialize.hrl").
%%%===================================================================
%%% API
%%%===================================================================
main([]) ->
    code:add_path("beam"),
    code:add_path("../beam"),
    code:add_path("../../beam"),
    code:add_path("../../../beam"),
    Protocol = #protocol{file = File} = protocol(),
    console:stack_trace(catch maker:start(fun protocol_maker:parse/2, [{File, Protocol}])),
    ok;
main(_) ->
    io:format("invail argument~n").

%%%===================================================================
%%% protocol config
%%%===================================================================
protocol() ->
    #protocol{
        file = "src/protocol/protocol_10.erl",
        include = [],
        io = [
            #io{
                name = 12345,
                comment = "",
                read = [
                    #param{name = server_id, desc = #u16{}},
                    #param{name = sex, desc = #u16{}},
                    #param{name = career, desc = #u8{}},
                    #param{name = agent_id, desc = #u16{}},
                    #param{name = name, desc = #str{}},
                    #param{name = nick, desc = #str{}},
                    #param{name = device, desc = #str{}},
                    #param{name = mac, desc = #str{}},
                    #param{name = device_type, desc = #str{}}
                ],
                write = []
            },
            #io{
                name = 23456,
                comment = "",
                read = [
                    #param{name = server_id, desc = #u16{}},
                    #param{name = id, desc = #u64{}},
                    #param{name = name, desc = #str{}}
                ],
                write = []
            }
        ]
    }.

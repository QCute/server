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
                comment = "Create Account",
                read = [
                    #param{name = server_id, desc = #u16{comment = "ServerId"}},
                    #param{name = sex, desc = #u16{comment = "Sex"}},
                    #param{name = career, desc = #u8{comment = "Career"}},
                    #param{name = agent_id, desc = #u16{comment = "AgentId"}},
                    #param{name = name, desc = #str{comment = "Name"}},
                    #param{name = nick, desc = #str{comment = "Nick"}},
                    #param{name = device, desc = #str{comment = "Device"}},
                    #param{name = mac, desc = #str{comment = "Mac"}},
                    #param{name = device_type, desc = #str{comment = "DeviceType"}}
                ],
                write = []
            },
            #io{
                name = 23456,
                comment = "Select Account",
                read = [
                    #param{name = server_id, desc = #u16{comment = "ServerId"}},
                    #param{name = id, desc = #u64{comment = "Id"}},
                    #param{name = name, desc = #str{comment = "Name"}}
                ],
                write = []
            }
        ]
    }.

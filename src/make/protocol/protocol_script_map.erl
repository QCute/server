%%%-------------------------------------------------------------------
%%% @doc
%%% module protocol read write
%%% @end
%%%-------------------------------------------------------------------
-module(protocol_script_map).
-export([main/1]).
-include("../../../include/serialize.hrl").
-include("../../../include/map.hrl").
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
        name = 200,
        file = "src/module/map/map_protocol.erl",
        include = ["map.hrl"],
        io = [
            #io{
                name = 20001,
                comment = "Packet Test",
                read = [],
                write = []
            },
            #io{
                name = 20002,
                comment = "Packet Move",
                read = [
                    #u16{name = x},
                    #u16{name = y}
                ],
                write = [
                    #u16{name = x},
                    #u16{name = y}
                ]
            },
            #io{
                name = 20003,
                comment = "Packet Walk",
                read = [],
                write = [
                    #u16{name = x},
                    #u16{name = y}
                ]
            },
            #io{
                name = 20003,
                comment = "Packet Map",
                read = [],
                write = [
                    #list{name = list, desc = #fighter{
                        x = #u16{name = x},
                        y = #u16{name = y}
                    }}
                ]
            },
            #io{
                name = 20004,
                comment = "Packet Map",
                read = [],
                write = [
                    #list{name = list, desc = #monster{
                        x = #u16{name = x},
                        y = #u16{name = y}
                    }}
                ]
            }
        ]
    }.

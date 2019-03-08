%%%-------------------------------------------------------------------
%%% @doc
%%% module protocol read write
%%% @end
%%%-------------------------------------------------------------------
-module(protocol_script_10).
-export([main/1]).
-include("../../../include/serialize.hrl").
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
        file = "src/protocol/protocol_10.erl",
        include = [],
        io = [
            #io{
                name = 10001,
                comment = "Login",
                read = [
                    #u16{name = server_id},       %% ServerId
                    #btr{name = name}             %% Name
                ],
                write = [#u8{name = result}]      %% login result 
            },
            #io{
                name = 10004,
                comment = "Create Account",
                read = [
                    #u16{name = server_id},       %% ServerId
                    #u8{name = sex},              %% Sex
                    #u8{name = career},           %% Career
                    #u16{name = agent_id},        %% AgentId
                    #btr{name = name},            %% Name
                    #btr{name = nick},            %% Nick
                    #btr{name = device},          %% Device
                    #btr{name = mac},             %% Mac
                    #btr{name = device_type}      %% DeviceType
                ],
                write = []
            },
            #io{
                name = 10003,
                comment = "Select Account",
                read = [
                    #u16{name = server_id},       %% ServerId
                    #u64{name = id},              %% Id
                    #btr{name = name}             %% Name
                ],
                write = []
            },

            #io{
                name = 34567,
                comment = "Query Account",
                read = [],
                write = [
                    #ets{name = rank_list, desc = {#u8{name = key}, #u8{name = value}}},            %% list
                    #ets{name = rank_list_list, desc = [{#u8{name = key}, #u8{name = value}}]}      %% list - list
                ]
            }
        ]
    }.

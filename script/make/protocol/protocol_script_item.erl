%%%-------------------------------------------------------------------
%%% @doc
%%% module protocol read write
%%% @end
%%%-------------------------------------------------------------------
-module(protocol_script_item).
-export([main/1]).
-include("../../../include/serialize.hrl").
-include("../../../include/item.hrl").
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
        name = 111,
        file = "src/module/item/item_protocol.erl",
        include = ["item.hrl"],
        io = [
            #io{
                name = 11101,
                comment = "Item List",
                read = [],
                write = [
                    #list{name = list, explain = #item{           %% Item List
                        unique_id = #u64{},                       %% |-- Id
                        item_id = #u32{},                         %% |-- ItemId
                        type = #u8{},                             %% |-- Type
                        amount = #u16{},                          %% |-- Amount
                        bind = #u8{}                              %% |-- Bind
                    }}
                ]
            }
        ]
    }.

%%%-------------------------------------------------------------------
%%% @doc
%%% protocol read write define
%%% @end
%%%-------------------------------------------------------------------
-module(protocol_script_bubble).
-export([main/1]).
-include("../../../include/serialize.hrl").
-include("../../../include/bubble.hrl").
%%%===================================================================
%%% API functions
%%%===================================================================
main([]) ->
    code:add_path(filename:dirname(escript:script_name()) ++ "/../../../beam/"),
    io:format("~p~n", [catch protocol_maker:start(protocol())]);
main(_) ->
    io:format("invalid argument~n").

%%%===================================================================
%%% protocol config
%%%===================================================================
protocol() ->
    [#protocol{
        number = 121,
        handler = "src/module/bubble/bubble_handler.erl",
        erl = "src/module/bubble/bubble_protocol.erl",
        js = "script/make/protocol/js/BubbleProtocol.js",
        lua = "script/make/protocol/lua/BubbleProtocol.lua",
        includes = ["bubble.hrl"],
        io = [
            #io{
                protocol = 12101,
                comment = "气泡列表",
                handler = #handler{module = bubble, function = query},
                read = [],
                write = [
                    #list{name = list, comment = "气泡列表", explain = #bubble{
                        bubble_id = #u32{comment = "气泡ID"},
                        expire_time = #u32{comment = "过期时间"}
                    }}
                ]
            },
            #io{
                protocol = 12102,
                handler = #handler{alias = "delete"},
                comment = "删除气泡",
                write = [
                    #list{name = list, comment = "气泡ID列表", explain = #bubble{
                        bubble_id = #u32{comment = "气泡ID"}
                    }}
                ]
            }
        ]
    }].

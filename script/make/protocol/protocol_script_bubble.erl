%%%-------------------------------------------------------------------
%%! +pc unicode
%%% @doc
%%% protocol read write define
%%% @end
%%%-------------------------------------------------------------------
-module(protocol_script_bubble).
-export([main/1]).
-include("../../../include/journal.hrl").
-include("../../../include/serialize.hrl").
-include("../../../include/bubble.hrl").
%%%===================================================================
%%% API functions
%%%===================================================================
main(_) ->
    io:setopts([{encoding, unicode}]),
    io:setopts(standard_error, [{encoding, unicode}]),
    code:add_path(filename:dirname(escript:script_name()) ++ "/../../../beam/"),
    try
        io:format("~tp~n", [protocol_maker:start(protocol())])
    catch ?EXCEPTION(Class, Reason, Stacktrace) ->
        ?HALT(Class, Reason, Stacktrace)
    end.

%%%===================================================================
%%% protocol config
%%%===================================================================
protocol() ->
    #protocol{
        number = 121,
        comment = "气泡",
        erl = "script/make/protocol/erl/bubble_protocol.erl",
        html = "script/make/protocol/html/BubbleProtocol.html",
        lua = "script/make/protocol/lua/BubbleProtocol.lua",
        js = "script/make/protocol/js/BubbleProtocol.js",
        cs = "script/make/protocol/cs/BubbleProtocol.cs",
        io = [
            #io{
                number = 12101,
                comment = "气泡列表",
                handler = #handler{module = bubble, function = query},
                decode = [],
                encode = [
                    #list{name = list, comment = "气泡列表", explain = #bubble{
                        bubble_id = #u32{comment = "气泡ID"},
                        expire_time = #u32{comment = "过期时间"}
                    }}
                ]
            },
            #io{
                number = 12102,
                handler = #handler{alias = "delete"},
                comment = "删除气泡",
                encode = [
                    #list{name = list, comment = "气泡ID列表", explain = #bubble{
                        bubble_id = #u32{comment = "气泡ID"}
                    }}
                ]
            }
        ]
    }.

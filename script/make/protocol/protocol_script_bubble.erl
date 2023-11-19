%%%-------------------------------------------------------------------
%%! +pc unicode -pa beam
%%% @doc
%%% protocol read write define
%%% @end
%%%-------------------------------------------------------------------
-module(protocol_script_bubble).
-mode(compile).
-compile({parse_transform, protocol_maker_transform}).
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
    ets:insert(ets:new(shell_records, [set, public]), [{Tag, Form} || Form = {attribute, _, record, {Tag, _}} <- lists:append([element(2, epp:parse_file(Header, [], [])) || Header <- filelib:wildcard(filename:dirname(escript:script_name()) ++ "/../../../include/*.hrl")])]),
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
                decode = {},
                encode = [                                 %% 气泡列表
                    #bubble{
                        bubble_id = u32(),                 %% 气泡ID
                        expire_time = u32()                %% 过期时间
                    }
                ]
            },
            #io{
                number = 12102,
                handler = #handler{alias = "delete"},
                comment = "删除气泡",
                encode = [                                 %% 气泡ID列表
                    u32()                                  %% 气泡ID
                ]
            }
        ]
    }.

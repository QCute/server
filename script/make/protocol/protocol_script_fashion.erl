%%%-------------------------------------------------------------------
%%! +pc unicode -pa beam
%%% @doc
%%% protocol read write define
%%% @end
%%%-------------------------------------------------------------------
-module(protocol_script_fashion).
-mode(compile).
-compile({parse_transform, protocol_maker_transform}).
-export([main/1]).
-include("../../../include/journal.hrl").
-include("../../../include/serialize.hrl").
-include("../../../include/fashion.hrl").
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
        number = 120,
        comment = "时装",
        erl = "script/make/protocol/erl/fashion_protocol.erl",
        html = "script/make/protocol/html/FashionProtocol.html",
        lua = "script/make/protocol/lua/FashionProtocol.lua",
        js = "script/make/protocol/js/FashionProtocol.js",
        cs = "script/make/protocol/cs/FashionProtocol.cs",
        io = [
            #io{
                number = 12001,
                comment = "时装列表",
                handler = #handler{module = fashion, function = query},
                decode = {},
                encode = [                                 %% 时装列表
                    #fashion{
                        fashion_id = u32(),                %% 时装ID
                        expire_time = u32()                %% 过期时间
                    }
                ]
            },
            #io{
                number = 12002,
                handler = #handler{alias = "delete"},
                comment = "删除时装",
                encode = [                                 %% 时装ID列表
                    u32()                                  %% 时装ID
                ]
            }
        ]
    }.

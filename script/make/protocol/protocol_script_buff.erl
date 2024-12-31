%%%-------------------------------------------------------------------
%%! +pc unicode -pa beam
%%% @doc
%%% protocol read write define
%%% @end
%%%-------------------------------------------------------------------
-module(protocol_script_buff).
-mode(compile).
-compile({parse_transform, protocol_maker_transform}).
-export([main/1]).
-include("../../../include/journal.hrl").
-include("../../../include/serialize.hrl").
-include("../../../include/buff.hrl").
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
        number = 118,
        comment = "buff",
        erl = "script/make/protocol/erl/buff_protocol.erl",
        html = "script/make/protocol/html/BuffProtocol.html",
        lua = "script/make/protocol/lua/BuffProtocol.lua",
        js = "script/make/protocol/js/BuffProtocol.js",
        cs = "script/make/protocol/cs/BuffProtocol.cs",
        io = [
            #io{
                number = 11801,
                comment = "Buff列表",
                handler = #handler{module = buff, function = query},
                decode = {},
                encode = [                                 %% Buff列表
                    #buff{
                        buff_id = u32(),                   %% BuffID
                        expire_time = u32(),               %% 结束时间
                        overlap = u16()                    %% 叠加数量
                    }
                ]
            },
            #io{
                number = 11802,
                comment = "删除Buff列表",
                handler = #handler{alias = "delete"},
                encode = [                                 %% BuffID列表
                    u32()                                  %% BuffID
                ]
            }
        ]
    }.

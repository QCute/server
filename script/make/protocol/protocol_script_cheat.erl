%%%-------------------------------------------------------------------
%%! +pc unicode -pa beam
%%% @doc
%%% protocol read write define
%%% @end
%%%-------------------------------------------------------------------
-module(protocol_script_cheat).
-mode(compile).
-compile({parse_transform, protocol_maker_transform}).
-export([main/1]).
-include("../../../include/journal.hrl").
-include("../../../include/serialize.hrl").
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
        number = 600,
        comment = "秘籍",
        erl = "script/make/protocol/erl/cheat_protocol.erl",
        html = "script/make/protocol/html/CheatProtocol.html",
        lua = "script/make/protocol/lua/CheatProtocol.lua",
        js = "script/make/protocol/js/CheatProtocol.js",
        cs = "script/make/protocol/cs/CheatProtocol.cs",
        io = [
            #io{
                number = 60001,
                comment = "秘籍",
                handler = #handler{module = cheat, function = query},
                decode = {},
                encode = [                                 %% 命令列表
                    {
                        description = bst(),               %% 描述
                        command = bst()                    %% 命令
                    }
                ]
            },
            #io{
                number = 60002,
                comment = "秘籍",
                handler = #handler{module = cheat, function = cheat},
                decode = bst(),                            %% 命令
                encode = ast()                             %% 结果
            }
        ]
    }.

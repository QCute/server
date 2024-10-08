%%%-------------------------------------------------------------------
%%! +pc unicode -pa beam
%%% @doc
%%% protocol read write define
%%% @end
%%%-------------------------------------------------------------------
-module(protocol_script_cheat).
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
                decode = [],
                encode = [
                    {
                        bst(description),      %% 描述
                        bst(command)           %% 命令
                    }
                ],
                encode = [
                    #list{name = cheat_list, comment = "秘籍列表", explain = {
                        #bst{name = description, comment = "描述"},
                        #bst{name = command, comment = "命令"}
                    }}
                ]
            },
            #io{
                number = 60002,
                comment = "秘籍",
                handler = #handler{module = cheat, function = cheat},
                decode = [
                    #bst{name = command, comment = "命令"}
                ],
                encode = [
                    #rst{name = result, comment = "结果"}
                ]
            }
        ]
    }.

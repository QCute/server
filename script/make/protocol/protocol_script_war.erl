%%%-------------------------------------------------------------------
%%! +pc unicode -pa beam
%%% @doc
%%% protocol read write define
%%% @end
%%%-------------------------------------------------------------------
-module(protocol_script_war).
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
        number = 180,
        comment = "战场",
        erl = "script/make/protocol/erl/war_protocol.erl",
        html = "script/make/protocol/html/WarProtocol.html",
        lua = "script/make/protocol/lua/WarProtocol.lua",
        js = "script/make/protocol/js/WarProtocol.js",
        cs = "script/make/protocol/cs/WarProtocol.cs",
        io = [
            #io{
                number = 18001,
                comment = "挑战Boss",
                handler = #handler{module = boss_server, function = battle},
                decode = u32(),                            %% 怪物Id
                encode = ast()                             %% 结果
            }
        ]
    }.

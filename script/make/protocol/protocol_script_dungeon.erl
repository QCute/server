%%%-------------------------------------------------------------------
%%! +pc unicode -pa beam
%%% @doc
%%% protocol read write define
%%% @end
%%%-------------------------------------------------------------------
-module(protocol_script_dungeon).
-mode(compile).
-compile({parse_transform, protocol_maker_transform}).
-export([main/1]).
-include("../../../include/journal.hrl").
-include("../../../include/serialize.hrl").
-include("../../../include/dungeon.hrl").
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
        number = 170,
        comment = "副本",
        erl = "script/make/protocol/erl/dungeon_protocol.erl",
        html = "script/make/protocol/html/DungeonProtocol.html",
        lua = "script/make/protocol/lua/DungeonProtocol.lua",
        js = "script/make/protocol/js/DungeonProtocol.js",
        cs = "script/make/protocol/cs/DungeonProtocol.cs",
        io = [
            #io{
                number = 17001,
                comment = "副本信息",
                handler = #handler{module = dungeon, function = query},
                decode = {},
                encode = [
                    #dungeon{
                        dungeon_id = u32(),                %% 副本Id
                        today_number = u16(),              %% 今天次数
                        total_number = u16()               %% 总次数
                    }
                ]
            },
            #io{
                number = 17002,
                comment = "进入副本",
                handler = #handler{module = dungeon, function = enter},
                decode = u32(),                            %% 副本Id
                encode = ast()                             %% 结果
            },
            #io{
                number = 17003,
                comment = "副本开始",
                handler = #handler{alias = "start"},
                encode = ast()                             %% 结果
            },
            #io{
                number = 17004,
                comment = "副本结束",
                handler = #handler{alias = "over"},
                encode = ast()                             %% 结果
            },
            #io{
                number = 17005,
                comment = "副本鼓舞",
                handler = #handler{module = dungeon_map, function = inspire},
                decode = {},
                encode = ast()                             %% 结果
            }
        ]
    }.

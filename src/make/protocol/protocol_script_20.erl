%%%-------------------------------------------------------------------
%%% @doc
%%% module protocol read write
%%% @end
%%%-------------------------------------------------------------------
-module(protocol_script_20).
-export([main/1]).
-include("../../../include/serialize.hrl").
-include("../../../include/quest.hrl").
%%%===================================================================
%%% API
%%%===================================================================
main([]) ->
    code:add_path("beam"),
    code:add_path("../beam"),
    code:add_path("../../beam"),
    code:add_path("../../../beam"),
    Protocol = #protocol{file = File} = protocol(),
    console:stacktrace(catch maker:start(fun protocol_maker:parse/2, [{File, Protocol}])),
    ok;
main(_) ->
    io:format("invail argument~n").

%%%===================================================================
%%% protocol config
%%%===================================================================
protocol() ->
    #protocol{
        file = "src/protocol/protocol_20.erl",
        include = ["quest.hrl"],
        io = [
            #io{
                name = 56789,
                comment = "Accept Quest",
                read = [
                    #u32{name = quest_id}                       %% QuestId
                ],
                write = [
                    #quest{
                        quest_id = #u32{name = quest_id},       %% QuestId
                        progress = [#quest_progress{            %% Progress List
                            id = #u8{name = id},                %% progress id
                            value = #u32{name = value}          %% progress value
                        }]
                    }
                ]
            }
        ]
    }.

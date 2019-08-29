%%%-------------------------------------------------------------------
%%% @doc
%%% module protocol read write
%%% @end
%%%-------------------------------------------------------------------
-module(protocol_script_rank).
-export([main/1]).
-include("../../../include/serialize.hrl").
-include("../../../include/rank.hrl").
%%%===================================================================
%%% API
%%%===================================================================
main([]) ->
    code:add_path(filename:dirname(escript:script_name()) ++ "/../../../beam/"),
    console:stacktrace(catch protocol_maker:start([protocol()]));
main(_) ->
    io:format("invail argument~n").

%%%===================================================================
%%% protocol config
%%%===================================================================
protocol() ->
    #protocol{
        name = 190,
        handler = "src/module/rank/rank_handler.erl",
        erl = "src/module/rank/rank_protocol.erl",
        json = "script/make/protocol/json/RankProtocol.js",
        lua = "script/make/protocol/lua/RankProtocol.lua",
        includes = ["rank.hrl"],
        io = [
            #io{
                name = 19001,
                comment = "Rank",
                read = [
                    #u8{name = type}
                ],
                write = [
                    #list{name = list, comment = "排行榜", explain = #rank{
                        type = #u16{comment = "类型"},
                        key = #u64{comment = "键"},
                        value = #u64{comment = "值"},
                        time = #u32{comment = "时间"},
                        rank = #u64{comment = "排名"},
                        name = #bst{comment = "名字"}
                    }}
                ],
                handler = #handler{
                    state_name = [],
                    module = rank_server,
                    function = push
                }
            }
        ]
    }.

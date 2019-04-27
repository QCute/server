%%%-------------------------------------------------------------------
%%% @doc
%%% module protocol read write
%%% @end
%%%-------------------------------------------------------------------
-module(protocol_script_11).
-export([main/1]).
-include("../../../include/serialize.hrl").
-include("../../../include/player.hrl").
-include("../../../include/assets.hrl").
%%%===================================================================
%%% API
%%%===================================================================
main([]) ->
    code:add_path(filename:dirname(escript:script_name()) ++ "/../../../beam/"),
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
        file = "src/protocol/protocol_11.erl",
        include = ["player.hrl", "assets.hrl"],
        io = [
            #io{
                name = 11001,
                comment = "Player",
                read = [],
                write = [
                    #player{
                        id = #u64{},                                      %% ID
                        sex = #u8{},                                      %% 性别
                        level = #u64{},                                   %% 等级
                        classes = #u8{},                                  %% 职业
                        item_size = #u16{},                               %% 普通背包大小
                        bag_size = #u16{},                                %% 装备背包大小
                        store_size = #u16{}                               %% 仓库背包大小
                    }
                ]
            },
            #io{
                name = 11002,
                comment = "Assets",
                read = [],
                write = [
                    #assets{
                        gold = #u64{},                          %% Gold
                        silver = #u32{},                        %% Silver
                        copper = #u64{},                        %% Copper
                        exp = #u64{}                            %% Exp
                    }
                ]
            }
        ]
    }.

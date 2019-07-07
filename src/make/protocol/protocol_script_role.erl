%%%-------------------------------------------------------------------
%%% @doc
%%% module protocol read write
%%% @end
%%%-------------------------------------------------------------------
-module(protocol_script_role).
-export([main/1]).
-include("../../../include/serialize.hrl").
-include("../../../include/role.hrl").
-include("../../../include/asset.hrl").
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
        name = 101,
        file = "src/module/role/role_protocol.erl",
        include = ["role.hrl"],
        io = [
            #io{
                name = 10101,
                comment = "role",
                read = [],
                write = [
                    #role{
                        id = #u64{},                                      %% ID
                        sex = #u8{},                                      %% 性别
                        level = #u64{},                                   %% 等级
                        classes = #u8{},                                  %% 职业
                        item_size = #u16{},                               %% 普通背包大小
                        bag_size = #u16{},                                %% 装备背包大小
                        store_size = #u16{}                               %% 仓库背包大小
                    }
                ]
            }
        ]
    }.

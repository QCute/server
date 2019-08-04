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
    console:stacktrace(catch protocol_maker:start([{File, Protocol}]));
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
                        role_id = #u64{},                                 %% 角色ID
                        role_name = #bst{},                               %% 角色名
                        account_id = #bst{},                              %% 账号ID
                        account_name = #bst{},                            %% 账号名
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

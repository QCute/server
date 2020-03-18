%%%------------------------------------------------------------------
%%% @doc
%%% module protocol read write
%%% @end
%%%------------------------------------------------------------------
-module(protocol_script_title).
-export([main/1]).
-include("../../../include/serialize.hrl").
-include("../../../include/title.hrl").
%%%==================================================================
%%% API functions
%%%==================================================================
main([]) ->
    code:add_path(filename:dirname(escript:script_name()) ++ "/../../../beam/"),
    io:format("~p~n", [catch protocol_maker:start(protocol())]);
main(_) ->
    io:format("invalid argument~n").

%%%==================================================================
%%% protocol config
%%%==================================================================
protocol() ->
    [#protocol{
        number = 119,
        handler = "src/module/title/title_handler.erl",
        erl = "src/module/title/title_protocol.erl",
        json = "script/make/protocol/json/TitleProtocol.js",
        lua = "script/make/protocol/lua/TitleProtocol.lua",
        includes = ["title.hrl"],
        io = [
            #io{
                protocol = 11901,
                comment = "称号列表",
                handler = #handler{module = title, function = query},
                read = [],
                write = [
                    #list{name = list, comment = "称号列表", explain = #title{
                        title_id = #u32{comment = "称号ID"},
                        expire_time = #u32{comment = "过期时间"}
                    }}
                ]
            },
            #io{
                protocol = 11902,
                comment = "删除称号",
                write = [
                    #list{name = list, comment = "称号ID列表", explain = #title{
                        title_id = #u32{comment = "称号ID"}
                    }}
                ]
            }
        ]
    }].

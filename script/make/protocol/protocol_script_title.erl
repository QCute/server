%%%-------------------------------------------------------------------
%%! +pc unicode
%%% @doc
%%% protocol read write define
%%% @end
%%%-------------------------------------------------------------------
-module(protocol_script_title).
-export([main/1]).
-include("../../../include/journal.hrl").
-include("../../../include/serialize.hrl").
-include("../../../include/title.hrl").
%%%===================================================================
%%% API functions
%%%===================================================================
main(_) ->
    io:setopts([{encoding, unicode}]),
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
        number = 119,
        handler = "src/module/title/title_handler.erl",
        erl = "src/module/title/title_protocol.erl",
        js = "script/make/protocol/js/TitleProtocol.js",
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
                handler = #handler{alias = "delete"},
                comment = "删除称号",
                write = [
                    #list{name = list, comment = "称号ID列表", explain = #title{
                        title_id = #u32{comment = "称号ID"}
                    }}
                ]
            }
        ]
    }.

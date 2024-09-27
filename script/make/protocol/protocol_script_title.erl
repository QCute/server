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
        number = 119,
        comment = "称号",
        erl = "script/make/protocol/erl/title_protocol.erl",
        html = "script/make/protocol/html/TitleProtocol.html",
        lua = "script/make/protocol/lua/TitleProtocol.lua",
        js = "script/make/protocol/js/TitleProtocol.js",
        cs = "script/make/protocol/cs/TitleProtocol.cs",
        io = [
            #io{
                number = 11901,
                comment = "称号列表",
                handler = #handler{module = title, function = query},
                decode = [],
                encode = [
                    #list{name = list, comment = "称号列表", explain = #title{
                        title_id = #u32{comment = "称号ID"},
                        expire_time = #u32{comment = "过期时间"}
                    }}
                ]
            },
            #io{
                number = 11902,
                handler = #handler{alias = "delete"},
                comment = "删除称号",
                encode = [
                    #list{name = list, comment = "称号ID列表", explain = #title{
                        title_id = #u32{comment = "称号ID"}
                    }}
                ]
            }
        ]
    }.

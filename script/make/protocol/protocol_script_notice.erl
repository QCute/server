%%%-------------------------------------------------------------------
%%! +pc unicode -pa beam
%%% @doc
%%% protocol read write define
%%% @end
%%%-------------------------------------------------------------------
-module(protocol_script_notice).
-mode(compile).
-compile({parse_transform, protocol_maker_transform}).
-export([main/1]).
-include("../../../include/journal.hrl").
-include("../../../include/serialize.hrl").
-include("../../../include/notice.hrl").
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
        number = 500,
        comment = "消息",
        erl = "script/make/protocol/erl/notice_protocol.erl",
        html = "script/make/protocol/html/NoticeProtocol.html",
        lua = "script/make/protocol/lua/NoticeProtocol.lua",
        js = "script/make/protocol/js/NoticeProtocol.js",
        cs = "script/make/protocol/cs/NoticeProtocol.cs",
        io = [
            #io{
                number = 50001,
                handler = #handler{module = notice, function = query},
                comment = "公告列表",
                decode = {},
                encode = [                                 %% 公告列表
                    #notice_role{
                        notice_id = u64(),                 %% 公告ID
                        receive_time = u32(),              %% 收到时间
                        read_time = u32(),                 %% 读取时间
                        title = bst(),                     %% 标题
                        content = bst()                    %% 内容
                    }
                ]
            },
            #io{
                number = 50002,
                handler = #handler{alias = "broadcast"},
                comment = "公告",
                encode = {
                    scope = u8(),                          %% 范围
                    type = u8(),                           %% 类型
                    title = bst(),                         %% 标题
                    msg = bst()                            %% 消息
                }
            }
        ]
    }.

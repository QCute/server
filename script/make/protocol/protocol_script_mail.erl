%%%-------------------------------------------------------------------
%%! +pc unicode
%%% @doc
%%% protocol read write define
%%% @end
%%%-------------------------------------------------------------------
-module(protocol_script_mail).
-export([main/1]).
-include("../../../include/journal.hrl").
-include("../../../include/serialize.hrl").
-include("../../../include/mail.hrl").
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
        number = 114,
        comment = "邮件",
        erl = "script/make/protocol/erl/mail_protocol.erl",
        html = "script/make/protocol/html/MailProtocol.html",
        lua = "script/make/protocol/lua/MailProtocol.lua",
        js = "script/make/protocol/js/MailProtocol.js",
        cs = "script/make/protocol/cs/MailProtocol.cs",
        io = [
            #io{
                number = 11401,
                comment = "邮件列表",
                handler = #handler{module = mail, function = query},
                decode = [],
                encode = [
                    #mail{
                        mail_id = u64(),                  %% 邮件ID
                        receive_time = u32(),             %% 接收时间
                        expire_time = u32(),              %% 有效时间
                        read_time = u32(),                %% 读取时间
                        receive_attachment_time = u32(),  %% 领取附件时间
                        title = bst(),                    %% 标题
                        content = bst(),                  %% 内容
                        attachment = [                    %% 附件列表
                            {
                                item_id = u32(),          %% 物品ID
                                number = u16()            %% 数量
                            }
                        ]
                    }
                ],
                encode = [
                    #list{name = list, comment = "邮件列表", explain = #mail{
                        mail_id = #u64{comment = "邮件ID"},
                        receive_time = #u32{comment = "接收时间"},
                        expire_time = #u32{comment = "有效时间"},
                        read_time = #u32{comment = "读取时间"},
                        receive_attachment_time = #u32{comment = "领取附件时间"},
                        title = #bst{comment = "标题"},
                        content = #bst{comment = "内容"},
                        attachment = #list{comment = "附件列表", explain = {
                            #u32{name = item_id, comment = "物品ID"},
                            #u16{name = number, comment = "数量"}
                        }}
                    }}
                ]
            },
            #io{
                number = 11402,
                comment = "阅读",
                handler = #handler{module = mail, function = read},
                decode = [
                    #u64{name = mail_id, comment = "邮件ID"}
                ],
                encode = [
                    #rst{name = result, comment = "结果"}
                ]
            },
            #io{
                number = 11403,
                comment = "领取附件",
                handler = #handler{module = mail, function = receive_attachment},
                decode = [
                    #u64{name = mail_id, comment = "邮件ID"}
                ],
                encode = [
                    #rst{name = result, comment = "结果"}
                ]
            },
            #io{
                number = 11404,
                comment = "删除邮件",
                handler = #handler{module = mail, function = delete},
                decode = [
                    #u64{name = mail_id, comment = "邮件ID"}
                ],
                encode = [
                    #rst{name = result, comment = "结果"}
                ]
            }
        ]
    }.

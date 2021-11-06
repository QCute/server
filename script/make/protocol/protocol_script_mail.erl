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
    code:add_path(filename:dirname(escript:script_name()) ++ "/../../../beam/"),
    try
        io:format("~p~n", [protocol_maker:start(protocol())])
    catch ?EXCEPTION(Class, Reason, Stacktrace) ->
        ?ERROR_STACKTRACE(Class, Reason, Stacktrace)
    end.

%%%===================================================================
%%% protocol config
%%%===================================================================
protocol() ->
    #protocol{
        number = 114,
        handler = "src/module/mail/mail_handler.erl",
        erl = "src/module/mail/mail_protocol.erl",
        js = "script/make/protocol/js/MailProtocol.js",
        lua = "script/make/protocol/lua/MailProtocol.lua",
        includes = ["mail.hrl"],
        io = [
            #io{
                protocol = 11401,
                comment = "邮件列表",
                handler = #handler{module = mail, function = query},
                read = [],
                write = [
                    #list{name = list, comment = "邮件列表", explain = #mail{
                        mail_id = #u64{comment = "邮件ID"},
                        receive_time = #u32{comment = "接收时间"},
                        is_read = #u8{comment = "是否已经读取"},
                        read_time = #u32{comment = "读取时间"},
                        expire_time = #u32{comment = "有效时间"},
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
                protocol = 11402,
                comment = "阅读",
                handler = #handler{module = mail, function = read},
                read = [
                    #u64{name = mail_id, comment = "邮件ID"}
                ],
                write = [
                    #rst{name = result, comment = "结果"}
                ]
            },
            #io{
                protocol = 11403,
                comment = "领取附件",
                handler = #handler{module = mail, function = receive_attachment},
                read = [
                    #u64{name = mail_id, comment = "邮件ID"}
                ],
                write = [
                    #rst{name = result, comment = "结果"}
                ]
            },
            #io{
                protocol = 11404,
                comment = "删除邮件",
                handler = #handler{module = mail, function = delete},
                read = [
                    #u64{name = mail_id, comment = "邮件ID"}
                ],
                write = [
                    #rst{name = result, comment = "结果"}
                ]
            }
        ]
    }.

%%%------------------------------------------------------------------
%%% @doc
%%% module protocol read write
%%% @end
%%%------------------------------------------------------------------
-module(protocol_script_mail).
-export([main/1]).
-include("../../../include/serialize.hrl").
-include("../../../include/mail.hrl").
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
        name = 114,
        handler = "src/module/mail/mail_handler.erl",
        erl = "src/module/mail/mail_protocol.erl",
        json = "script/make/protocol/json/MailProtocol.js",
        lua = "script/make/protocol/lua/MailProtocol.lua",
        includes = ["mail.hrl"],
        io = [
            #io{
                name = 11401,
                comment = "Mail",
                handler = #handler{module = mail, function = query},
                read = [],
                write = [
                    #list{name = mail, comment = "邮件列表", explain = #mail{ 
                        mail_id = #u64{comment = "邮件ID"},
                        sender_id = #u64{comment = "发送者ID"},
                        sender_nick = #bst{comment = "发送者昵称"},
                        receiver_id = #u64{comment = "接受者ID"},
                        receiver_nick = #bst{comment = "接受者昵称"},
                        is_read = #u8{comment = "是否已经读取"},
                        read_time = #u32{comment = "读取时间"},
                        receive_time = #u32{comment = "接收时间"},
                        expire_time = #u32{comment = "有效时间"},
                        title = #bst{comment = "标题"},
                        content = #bst{comment = "内容"},
                        attachment = #list{comment = "附件列表", explain = {
                            #u32{name = item_id, comment = "物品ID"},
                            #u16{name = number, comment = "数量"},
                            #u8{name = bind, comment = "是否绑定"}
                        }}
                    }}
                ]
            },
            #io{
                name = 11402,
                comment = "Read",
                handler = #handler{module = mail, function = read},
                text = [],
                read = [
                    #u64{name = mail_id, comment = "邮件ID"}
                ],
                write = [
                    #rst{name = result, comment = "结果"}
                ]
            },
            #io{
                name = 11403,
                comment = "receive attachment",
                handler = #handler{module = mail, function = receive_attachment},
                text = [{no_such_mail, "没有此邮件"}, {bag_full, "背包已满"}],
                read = [
                    #u64{name = mail_id, comment = "邮件ID"}
                ],
                write = [
                    #rst{name = result, comment = "结果"}
                ]
            }
        ]
    }].

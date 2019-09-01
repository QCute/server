%%%-------------------------------------------------------------------
%%% @doc
%%% module protocol read write
%%% @end
%%%-------------------------------------------------------------------
-module(protocol_script_mail).
-export([main/1]).
-include("../../../include/serialize.hrl").
-include("../../../include/mail.hrl").
%%%===================================================================
%%% API
%%%===================================================================
main([]) ->
    code:add_path(filename:dirname(escript:script_name()) ++ "/../../../beam/"),
    console:stacktrace(catch protocol_maker:start([protocol()]));
main(_) ->
    io:format("invail argument~n").

%%%===================================================================
%%% protocol config
%%%===================================================================
protocol() ->
    #protocol{
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
                        valid_time = #u32{comment = "有效时间"},
                        title = #bst{comment = "标题"},
                        content = #bst{comment = "内容"},
                        attachment = #list{comment = "附件列表", explain = {
                            #u32{name = item_id, comment = "物品ID"},
                            #u16{name = amount, comment = "数量"},
                            #u8{name = bind, comment = "是否绑定"}
                        }}
                    }}
                ],
                handler = #handler{
                    module = mail,
                    function = query
                }
            },
            #io{
                name = 11402,
                comment = "Read",
                read = [
                    #u64{name = mail_id, comment = "邮件ID"}
                ],
                write = [
                    #u8{name = result, comment = "结果"}
                ],
                handler = #handler{
                    module = mail,
                    function = read
                }
            },
            #io{
                name = 11403,
                comment = "receive attachment",
                read = [
                    #u64{name = mail_id, comment = "邮件ID"}
                ],
                write = [
                    #u8{name = result, comment = "结果"}
                ],
                handler = #handler{
                    module = mail,
                    function = receive_attachment
                }
            }
        ]
    }.

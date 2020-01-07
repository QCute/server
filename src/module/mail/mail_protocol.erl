-module(mail_protocol).
-export([read/2, write/2]).
-include("mail.hrl").


read(11401, <<>>) ->
    {ok, []};

read(11402, <<MailId:64>>) ->
    {ok, MailId};

read(11403, <<MailId:64>>) ->
    {ok, MailId};

read(Code, Binary) ->
    {error, Code, Binary}.



write(11401, Mail) ->
    {ok, protocol:pack(11401, <<(length(Mail)):16, <<<<MailId:64, SenderId:64, (byte_size(SenderNick)):16, (SenderNick)/binary, ReceiverId:64, (byte_size(ReceiverNick)):16, (ReceiverNick)/binary, ReceiveTime:32, IsRead:8, ReadTime:32, ExpireTime:32, (byte_size(Title)):16, (Title)/binary, (byte_size(Content)):16, (Content)/binary, (length(Attachment)):16, <<<<ItemId:32, Number:16, Bind:8>> || {ItemId, Number, Bind} <- Attachment>>/binary>> || #mail{mail_id = MailId, sender_id = SenderId, sender_nick = SenderNick, receiver_id = ReceiverId, receiver_nick = ReceiverNick, receive_time = ReceiveTime, is_read = IsRead, read_time = ReadTime, expire_time = ExpireTime, title = Title, content = Content, attachment = Attachment} <- Mail>>/binary>>)};

write(11402, Result) ->
    {ok, protocol:pack(11402, <<(text(11402, Result))/binary>>)};

write(11403, Result) ->
    {ok, protocol:pack(11403, <<(text(11403, Result))/binary>>)};

write(Code, Content) ->
    {error, Code, Content}.



text(_, ok) ->
    <<0:16>>;
text(Protocol, Reason) ->
    text(Protocol, Reason, parameter_data:get(language)).

text(11402, already_read, sc) ->
    <<18:16, "邮件已阅读过"/utf8>>;
text(11402, no_such_mail, sc) ->
    <<15:16, "没有此邮件"/utf8>>;
text(11403, bag_full, sc) ->
    <<12:16, "背包已满"/utf8>>;
text(11403, no_such_mail, sc) ->
    <<15:16, "没有此邮件"/utf8>>;
text(_, _, Reason) ->
    protocol:write_bit_string(type:to_binary(Reason)).


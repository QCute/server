-module(mail_protocol).
-export([read/2, write/2]).
-include("mail.hrl").


read(11401, <<>>) ->
    {ok, []};

read(11402, <<MailId:64>>) ->
    {ok, [MailId]};

read(11403, <<MailId:64>>) ->
    {ok, [MailId]};

read(Code, Binary) ->
    {error, Code, Binary}.



write(11401, [Mail]) ->
    {ok, protocol:pack(11401, <<(length(Mail)):16, <<<<MailId:64, SenderId:64, (byte_size(SenderNick)):16, (SenderNick)/binary, ReceiverId:64, (byte_size(ReceiverNick)):16, (ReceiverNick)/binary, IsRead:8, ReadTime:32, ReceiveTime:32, ValidTime:32, (byte_size(Title)):16, (Title)/binary, (byte_size(Content)):16, (Content)/binary, (length(Attachment)):16, <<<<ItemId:32, Amount:16, Bind:8>> || {ItemId, Amount, Bind} <- Attachment>>/binary>> || #mail{mail_id = MailId, sender_id = SenderId, sender_nick = SenderNick, receiver_id = ReceiverId, receiver_nick = ReceiverNick, is_read = IsRead, read_time = ReadTime, receive_time = ReceiveTime, valid_time = ValidTime, title = Title, content = Content, attachment = Attachment} <- Mail>>/binary>>)};

write(11402, [Result]) ->
    {ok, protocol:pack(11402, <<Result:8>>)};

write(11403, [Result]) ->
    {ok, protocol:pack(11403, <<Result:8>>)};

write(Code, Content) ->
    {error, Code, Content}.

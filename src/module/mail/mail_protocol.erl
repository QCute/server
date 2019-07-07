-module(mail_protocol).
-compile(nowarn_export_all).
-compile(export_all).
-include("mail.hrl").


read(Code, Binary) ->
    {error, Code, Binary}.



write(11401, [List]) ->
    ListBinary = <<(length(List)):16, <<<<Id:64, SenderId:64, (byte_size(SenderNick)):16, (SenderNick)/binary, ReceiverId:64, (byte_size(ReceiverNick)):16, (ReceiverNick)/binary, IsRead:8, ReadTime:32, ReceiveTime:32, ValidTime:32, (byte_size(Title)):16, (Title)/binary, (byte_size(Content)):16, (Content)/binary, (length(Item)):16, <<<<ItemId:32, Amount:16, Bind:8>> || {ItemId, Amount, Bind} <- Item>>/binary>> || #mail{id = Id, sender_id = SenderId, sender_nick = SenderNick, receiver_id = ReceiverId, receiver_nick = ReceiverNick, is_read = IsRead, read_time = ReadTime, receive_time = ReceiveTime, valid_time = ValidTime, title = Title, content = Content, attachment = Item} <- List>>/binary>>,
    {ok, protocol:pack(11401, <<ListBinary/binary>>)};

write(Code, Content) ->
    {error, Code, Content}.

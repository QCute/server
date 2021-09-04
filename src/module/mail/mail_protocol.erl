-module(mail_protocol).
-export([read/2, write/2]).
-include("mail.hrl").


read(11401, <<>>) ->
    {ok, []};

read(11402, <<MailId:64>>) ->
    {ok, MailId};

read(11403, <<MailId:64>>) ->
    {ok, MailId};

read(11404, <<MailId:64>>) ->
    {ok, MailId};

read(Code, Binary) ->
    {error, Code, Binary}.



write(11401, List) ->
    ListBinary = protocol:write_list(fun(#mail{mail_id = MailId, receive_time = ReceiveTime, is_read = IsRead, read_time = ReadTime, expire_time = ExpireTime, title = Title, content = Content, attachment = Attachment}) -> AttachmentBinary = protocol:write_list(fun({ItemId, Number}) -> <<ItemId:32, Number:16>> end, Attachment), <<MailId:64, ReceiveTime:32, IsRead:8, ReadTime:32, ExpireTime:32, (byte_size(Title)):16, (Title)/binary, (byte_size(Content)):16, (Content)/binary, AttachmentBinary/binary>> end, List),
    {ok, protocol:pack(11401, <<ListBinary/binary>>)};

write(11402, Result) ->
    {ok, protocol:pack(11402, <<(protocol:text(11402, Result))/binary>>)};

write(11403, Result) ->
    {ok, protocol:pack(11403, <<(protocol:text(11403, Result))/binary>>)};

write(11404, Result) ->
    {ok, protocol:pack(11404, <<(protocol:text(11404, Result))/binary>>)};

write(Code, Content) ->
    {error, Code, Content}.


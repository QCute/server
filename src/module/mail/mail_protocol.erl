-module(mail_protocol).
-export([read/2, write/2]).
-include("mail.hrl").


-spec read(Protocol :: non_neg_integer(), Binary :: binary()) -> {ok, [integer() | binary() | list()]} | {error, Protocol :: non_neg_integer(), Binary :: binary()}.
read(11401, <<>>) ->
    {ok, []};

read(11402, <<MailId:64>>) ->
    {ok, MailId};

read(11403, <<MailId:64>>) ->
    {ok, MailId};

read(11404, <<MailId:64>>) ->
    {ok, MailId};

read(Protocol, Binary) ->
    {error, Protocol, Binary}.


-spec write(Protocol :: non_neg_integer(), Data :: atom() | tuple() | binary() | list()) -> {ok, binary()} | {error, Protocol :: non_neg_integer(), Data :: atom() | tuple() | binary() | list()}.
write(11401, List) ->
    ListBinary = protocol:write_list(fun(#mail{mail_id = MailId, receive_time = ReceiveTime, expire_time = ExpireTime, read_time = ReadTime, receive_attachment_time = ReceiveAttachmentTime, title = Title, content = Content, attachment = Attachment}) -> AttachmentBinary = protocol:write_list(fun({ItemId, Number}) -> <<ItemId:32, Number:16>> end, Attachment), <<MailId:64, ReceiveTime:32, ExpireTime:32, ReadTime:32, ReceiveAttachmentTime:32, (byte_size(Title)):16, (Title)/binary, (byte_size(Content)):16, (Content)/binary, AttachmentBinary/binary>> end, List),
    {ok, protocol:pack(11401, <<ListBinary/binary>>)};

write(11402, Result) ->
    {ok, protocol:pack(11402, <<(protocol:text(Result))/binary>>)};

write(11403, Result) ->
    {ok, protocol:pack(11403, <<(protocol:text(Result))/binary>>)};

write(11404, Result) ->
    {ok, protocol:pack(11404, <<(protocol:text(Result))/binary>>)};

write(Protocol, Data) ->
    {error, Protocol, Data}.



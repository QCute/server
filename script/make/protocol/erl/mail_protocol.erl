-module(mail_protocol).
-export([decode/2, encode/2]).
-include("mail.hrl").

-spec decode(Protocol :: non_neg_integer(), Binary :: binary()) -> {ok, [integer() | binary() | list()]} | {error, Protocol :: non_neg_integer(), Binary :: binary()}.
decode(11401, _Rest_ = <<_/binary>>) ->
    {ok, {}};

decode(11402, _Rest_ = <<_/binary>>) ->
    <<Data:64, _DataRest_/binary>> = _Rest_,
    {ok, Data};

decode(11403, _Rest_ = <<_/binary>>) ->
    <<Data:64, _DataRest_/binary>> = _Rest_,
    {ok, Data};

decode(11404, _Rest_ = <<_/binary>>) ->
    <<Data:64, _DataRest_/binary>> = _Rest_,
    {ok, Data};

decode(Protocol, Binary) ->
    {error, Protocol, Binary}.


-spec encode(Protocol :: non_neg_integer(), Data :: atom() | tuple() | binary() | list()) -> {ok, binary()} | {error, Protocol :: non_neg_integer(), Data :: atom() | tuple() | binary() | list()}.
encode(11401, Data) ->
    Data11401 = <<(encode_data_11401(<<>>, 0, Data))/binary>>,
    {ok, <<(byte_size(Data11401)):16, 11401:16, Data11401/binary>>};

encode(11402, Data) ->
    Data11402 = <<(protocol:text(Data))/binary>>,
    {ok, <<(byte_size(Data11402)):16, 11402:16, Data11402/binary>>};

encode(11403, Data) ->
    Data11403 = <<(protocol:text(Data))/binary>>,
    {ok, <<(byte_size(Data11403)):16, 11403:16, Data11403/binary>>};

encode(11404, Data) ->
    Data11404 = <<(protocol:text(Data))/binary>>,
    {ok, <<(byte_size(Data11404)):16, 11404:16, Data11404/binary>>};

encode(Protocol, Data) ->
    {error, Protocol, Data}.

encode_data_11401(Acc = <<_/binary>>, Length, []) ->
    <<Length:16, Acc/binary>>;
encode_data_11401(Acc = <<_/binary>>, Length, [#mail{mail_id = MailId, receive_time = ReceiveTime, expire_time = ExpireTime, read_time = ReadTime, receive_attachment_time = ReceiveAttachmentTime, title = Title, content = Content, attachment = Attachment} | Data]) ->
    encode_data_11401(<<Acc/binary, MailId:64, ReceiveTime:32, ExpireTime:32, ReadTime:32, ReceiveAttachmentTime:32, (byte_size(Title)):16, (Title)/binary, (byte_size(Content)):16, (Content)/binary, (encode_attachment_11401(<<>>, 0, Attachment))/binary>>, Length + 1, Data).

encode_attachment_11401(Acc = <<_/binary>>, Length, []) ->
    <<Length:16, Acc/binary>>;
encode_attachment_11401(Acc = <<_/binary>>, Length, [{AttachmentItemId, AttachmentNumber} | Attachment]) ->
    encode_attachment_11401(<<Acc/binary, AttachmentItemId:32, AttachmentNumber:16>>, Length + 1, Attachment).


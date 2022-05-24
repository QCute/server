-module(notice_protocol).
-export([read/2, write/2]).
-include("notice.hrl").


-spec read(Protocol :: non_neg_integer(), Binary :: binary()) -> {ok, [integer() | binary() | list()]} | {error, Protocol :: non_neg_integer(), Binary :: binary()}.
read(50001, <<>>) ->
    {ok, []};

read(Protocol, Binary) ->
    {error, Protocol, Binary}.


-spec write(Protocol :: non_neg_integer(), Data :: atom() | tuple() | binary() | list()) -> {ok, binary()} | {error, Protocol :: non_neg_integer(), Data :: atom() | tuple() | binary() | list()}.
write(50001, NoticeList) ->
    NoticeListBinary = protocol:write_list(fun(#role_notice{notice_id = NoticeId, receive_time = ReceiveTime, read_time = ReadTime, title = Title, content = Content}) -> <<NoticeId:64, ReceiveTime:32, ReadTime:32, (byte_size(Title)):16, (Title)/binary, (byte_size(Content)):16, (Content)/binary>> end, NoticeList),
    {ok, protocol:pack(50001, <<NoticeListBinary/binary>>)};

write(50002, [Scope, Type, Title, Msg]) ->
    {ok, protocol:pack(50002, <<Scope:8, Type:8, (byte_size(Title)):16, (Title)/binary, (byte_size(Msg)):16, (Msg)/binary>>)};

write(Protocol, Data) ->
    {error, Protocol, Data}.



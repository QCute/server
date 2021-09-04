-module(title_protocol).
-export([read/2, write/2]).
-include("title.hrl").


read(11901, <<>>) ->
    {ok, []};

read(Code, Binary) ->
    {error, Code, Binary}.



write(11901, List) ->
    ListBinary = protocol:write_list(fun(#title{title_id = TitleId, expire_time = ExpireTime}) -> <<TitleId:32, ExpireTime:32>> end, List),
    {ok, protocol:pack(11901, <<ListBinary/binary>>)};

write(11902, List) ->
    ListBinary = protocol:write_list(fun(#title{title_id = TitleId}) -> <<TitleId:32>> end, List),
    {ok, protocol:pack(11902, <<ListBinary/binary>>)};

write(Code, Content) ->
    {error, Code, Content}.


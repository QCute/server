-module(title_protocol).
-export([read/2, write/2]).
-include("title.hrl").


read(11901, <<>>) ->
    {ok, []};

read(Code, Binary) ->
    {error, Code, Binary}.



write(11901, List) ->
    {ok, protocol:pack(11901, <<(length(List)):16, <<<<TitleId:32, ExpireTime:32>> || #title{title_id = TitleId, expire_time = ExpireTime} <- List>>/binary>>)};

write(11902, List) ->
    {ok, protocol:pack(11902, <<(length(List)):16, <<<<TitleId:32>> || #title{title_id = TitleId} <- List>>/binary>>)};

write(Code, Content) ->
    {error, Code, Content}.


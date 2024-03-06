-module(item_handler).
-export([handle/3]).
-export([send_query_item/2]).
-export([send_query_bag/2]).
-export([send_query_store/2]).
-export([send_use/2]).
-include("user.hrl").

handle(User, 11101, []) ->
    item:query_item(User);

handle(User, 11102, []) ->
    item:query_bag(User);

handle(User, 11103, []) ->
    item:query_store(User);

handle(User, 11106, [ItemNo, Number, Type]) ->
    item_use:use(User, ItemNo, Number, Type);

handle(_, Protocol, Data) ->
    {error, Protocol, Data}.

send_query_item(User, List) ->
    {ok, Binary} = item_protocol:encode(11101, List),
    User#user{buffer = <<(User#user.buffer)/binary, Binary/binary>>}.

send_query_bag(User, List) ->
    {ok, Binary} = item_protocol:encode(11102, List),
    User#user{buffer = <<(User#user.buffer)/binary, Binary/binary>>}.

send_query_store(User, List) ->
    {ok, Binary} = item_protocol:encode(11103, List),
    User#user{buffer = <<(User#user.buffer)/binary, Binary/binary>>}.

send_use(User, Result) ->
    {ok, Binary} = item_protocol:encode(11106, Result),
    User#user{buffer = <<(User#user.buffer)/binary, Binary/binary>>}.


-module(item_handler).
-export([handle/3]).
-export([send_query_item/2]).
-export([send_query_bag/2]).
-export([send_query_store/2]).
-export([send_use/2]).
-include("user.hrl").

handle(User, 11101, Data) ->
    item:query_item(User, Data);

handle(User, 11102, Data) ->
    item:query_bag(User, Data);

handle(User, 11103, Data) ->
    item:query_store(User, Data);

handle(User, 11106, Data) ->
    item_use:use(User, Data);

handle(_, Protocol, Data) ->
    {error, Protocol, Data}.

send_query_item(User, Data) ->
    {ok, Binary} = item_protocol:encode(11101, Data),
    User#user{buffer = <<(User#user.buffer)/binary, Binary/binary>>}.

send_query_bag(User, Data) ->
    {ok, Binary} = item_protocol:encode(11102, Data),
    User#user{buffer = <<(User#user.buffer)/binary, Binary/binary>>}.

send_query_store(User, Data) ->
    {ok, Binary} = item_protocol:encode(11103, Data),
    User#user{buffer = <<(User#user.buffer)/binary, Binary/binary>>}.

send_use(User, Data) ->
    {ok, Binary} = item_protocol:encode(11106, Data),
    User#user{buffer = <<(User#user.buffer)/binary, Binary/binary>>}.


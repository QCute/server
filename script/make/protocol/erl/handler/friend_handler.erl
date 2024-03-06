-module(friend_handler).
-export([handle/3]).
-export([send_query/2]).
-export([send_apply/2]).
-export([send_agree/2]).
-export([send_delete/2]).
-export([send_block/2]).
-export([send_cancel_block/2]).
-include("user.hrl").

handle(User, 11501, Data) ->
    friend:query(User, Data);

handle(User, 11502, Data) ->
    friend:apply(User, Data);

handle(User, 11503, Data) ->
    friend:agree(User, Data);

handle(User, 11504, Data) ->
    friend:delete(User, Data);

handle(User, 11505, Data) ->
    friend:block(User, Data);

handle(User, 11506, Data) ->
    friend:cancel_block(User, Data);

handle(_, Protocol, Data) ->
    {error, Protocol, Data}.

send_query(User, Data) ->
    {ok, Binary} = friend_protocol:encode(11501, Data),
    User#user{buffer = <<(User#user.buffer)/binary, Binary/binary>>}.

send_apply(User, Data) ->
    {ok, Binary} = friend_protocol:encode(11502, Data),
    User#user{buffer = <<(User#user.buffer)/binary, Binary/binary>>}.

send_agree(User, Data) ->
    {ok, Binary} = friend_protocol:encode(11503, Data),
    User#user{buffer = <<(User#user.buffer)/binary, Binary/binary>>}.

send_delete(User, Data) ->
    {ok, Binary} = friend_protocol:encode(11504, Data),
    User#user{buffer = <<(User#user.buffer)/binary, Binary/binary>>}.

send_block(User, Data) ->
    {ok, Binary} = friend_protocol:encode(11505, Data),
    User#user{buffer = <<(User#user.buffer)/binary, Binary/binary>>}.

send_cancel_block(User, Data) ->
    {ok, Binary} = friend_protocol:encode(11506, Data),
    User#user{buffer = <<(User#user.buffer)/binary, Binary/binary>>}.


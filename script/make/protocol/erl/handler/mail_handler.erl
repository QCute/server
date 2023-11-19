-module(mail_handler).
-export([handle/3]).
-export([send_query/2]).
-export([send_read/2]).
-export([send_receive_attachment/2]).
-export([send_delete/2]).
-include("user.hrl").

handle(User, 11401, {}) ->
    mail:query(User);

handle(User, 11402, Data) ->
    mail:read(User, Data);

handle(User, 11403, Data) ->
    mail:receive_attachment(User, Data);

handle(User, 11404, Data) ->
    mail:delete(User, Data);

handle(_, Protocol, Data) ->
    {error, Protocol, Data}.

send_query(User, Data) ->
    {ok, Binary} = mail_protocol:encode(11401, Data),
    User#user{buffer = <<(User#user.buffer)/binary, Binary/binary>>}.

send_read(User, Data) ->
    {ok, Binary} = mail_protocol:encode(11402, Data),
    User#user{buffer = <<(User#user.buffer)/binary, Binary/binary>>}.

send_receive_attachment(User, Data) ->
    {ok, Binary} = mail_protocol:encode(11403, Data),
    User#user{buffer = <<(User#user.buffer)/binary, Binary/binary>>}.

send_delete(User, Data) ->
    {ok, Binary} = mail_protocol:encode(11404, Data),
    User#user{buffer = <<(User#user.buffer)/binary, Binary/binary>>}.


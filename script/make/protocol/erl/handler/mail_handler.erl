-module(mail_handler).
-export([handle/3]).
-export([send_query/2]).
-export([send_read/2]).
-export([send_receive_attachment/2]).
-export([send_delete/2]).
-include("user.hrl").

handle(User, 11401, []) ->
    mail:query(User);

handle(User, 11402, MailId) ->
    mail:read(User, MailId);

handle(User, 11403, MailId) ->
    mail:receive_attachment(User, MailId);

handle(User, 11404, MailId) ->
    mail:delete(User, MailId);

handle(_, Protocol, Data) ->
    {error, Protocol, Data}.

send_query(User, List) ->
    {ok, Binary} = mail_protocol:encode(11401, List),
    User#user{buffer = <<(User#user.buffer)/binary, Binary/binary>>}.

send_read(User, Result) ->
    {ok, Binary} = mail_protocol:encode(11402, Result),
    User#user{buffer = <<(User#user.buffer)/binary, Binary/binary>>}.

send_receive_attachment(User, Result) ->
    {ok, Binary} = mail_protocol:encode(11403, Result),
    User#user{buffer = <<(User#user.buffer)/binary, Binary/binary>>}.

send_delete(User, Result) ->
    {ok, Binary} = mail_protocol:encode(11404, Result),
    User#user{buffer = <<(User#user.buffer)/binary, Binary/binary>>}.


-module(mail_handler).
-export([handle/3]).

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

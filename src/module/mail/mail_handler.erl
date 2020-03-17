-module(mail_handler).
-export([handle/3]).

handle(11401, User, []) ->
    mail:query(User);

handle(11402, User, MailId) ->
    mail:read(User, MailId);

handle(11403, User, MailId) ->
    mail:receive_attachment(User, MailId);

handle(11404, User, MailId) ->
    mail:delete(User, MailId);

handle(Protocol, _, Data) ->
    {error, Protocol, Data}.

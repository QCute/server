-module(chat_handler).
-export([handle/3]).

handle(11601, User, Msg) ->
    chat:world(User, Msg);

handle(11602, User, Msg) ->
    chat:guild(User, Msg);

handle(11603, User, [UserId, Msg]) ->
    chat:private(User, UserId, Msg);

handle(Protocol, _, Data) ->
    {error, Protocol, Data}.

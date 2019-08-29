-module(key_handler).
-export([handle/3]).

handle(15001, User, [Key]) ->
    key_server:award(User, Key);

handle(Protocol, _, Data) ->
    {error, Protocol, Data}.

-module(asset_handler).
-export([handle/3]).

handle(10201, User, []) ->
    asset:push(User);

handle(Protocol, _, Data) ->
    {error, Protocol, Data}.

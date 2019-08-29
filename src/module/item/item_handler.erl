-module(item_handler).
-export([handle/3]).

handle(11101, User, []) ->
    item:push_item(User);

handle(11102, User, []) ->
    item:push_bag(User);

handle(11103, User, []) ->
    item:push_store(User);

handle(Protocol, _, Data) ->
    {error, Protocol, Data}.

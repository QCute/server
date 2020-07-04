-module(item_handler).
-export([handle/3]).

handle(11101, User, []) ->
    item:query_item(User);

handle(11102, User, []) ->
    item:query_bag(User);

handle(11103, User, []) ->
    item:query_store(User);

handle(11106, User, [ItemNo, Number, Type]) ->
    item_use:use(User, ItemNo, Number, Type);

handle(Protocol, _, Data) ->
    {error, Protocol, Data}.

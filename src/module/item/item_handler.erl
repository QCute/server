-module(item_handler).
-export([handle/3]).

handle(User, 11101, []) ->
    item:query_item(User);

handle(User, 11102, []) ->
    item:query_bag(User);

handle(User, 11103, []) ->
    item:query_store(User);

handle(User, 11106, [ItemNo, Number, Type]) ->
    item_use:use(User, ItemNo, Number, Type);

handle(_, Protocol, Data) ->
    {error, Protocol, Data}.

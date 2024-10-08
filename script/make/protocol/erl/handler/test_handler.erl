-module(test_handler).
-export([handle/3]).
-export([send_test_protocol/3]).
-include("user.hrl").

handle(User, 65535, Data) ->
    test:test_protocol(User, 65535, Data);

handle(_, Protocol, Data) ->
    {error, Protocol, Data}.

send_test_protocol(User, 65535, Data) ->
    {ok, Binary} = test_protocol:encode(65535, Data),
    User#user{buffer = <<(User#user.buffer)/binary, Binary/binary>>}.


-module(cheat_handler).
-export([handle/3]).
-export([send_query/2]).
-export([send_cheat/2]).
-include("user.hrl").

handle(User, 60001, []) ->
    cheat:query(User);

handle(User, 60002, Command) ->
    cheat:cheat(User, Command);

handle(_, Protocol, Data) ->
    {error, Protocol, Data}.

send_query(User, CheatList) ->
    {ok, Binary} = cheat_protocol:encode(60001, CheatList),
    User#user{buffer = [Binary | User#user.buffer]}.

send_cheat(User, Result) ->
    {ok, Binary} = cheat_protocol:encode(60002, Result),
    User#user{buffer = [Binary | User#user.buffer]}.


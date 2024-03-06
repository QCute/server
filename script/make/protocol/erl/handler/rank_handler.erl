-module(rank_handler).
-export([handle/3]).
-export([send_rank_server_level/3]).
-export([send_rank_server_fight/3]).
-export([send_rank_server_achievement/3]).
-export([send_rank_server_wealth/3]).
-export([send_rank_server_classes/3]).
-include("user.hrl").

handle(User, 19001, []) ->
    rank_server:query(User, 19001);

handle(User, 19002, []) ->
    rank_server:query(User, 19002);

handle(User, 19003, []) ->
    rank_server:query(User, 19003);

handle(User, 19004, []) ->
    rank_server:query(User, 19004);

handle(User, 19005, []) ->
    rank_server:query(User, 19005);

handle(_, Protocol, Data) ->
    {error, Protocol, Data}.

send_rank_server_level(User, 19001, List) ->
    {ok, Binary} = rank_protocol:encode(19001, List),
    User#user{buffer = [Binary | User#user.buffer]}.

send_rank_server_fight(User, 19002, List) ->
    {ok, Binary} = rank_protocol:encode(19002, List),
    User#user{buffer = [Binary | User#user.buffer]}.

send_rank_server_achievement(User, 19003, List) ->
    {ok, Binary} = rank_protocol:encode(19003, List),
    User#user{buffer = [Binary | User#user.buffer]}.

send_rank_server_wealth(User, 19004, List) ->
    {ok, Binary} = rank_protocol:encode(19004, List),
    User#user{buffer = [Binary | User#user.buffer]}.

send_rank_server_classes(User, 19005, List) ->
    {ok, Binary} = rank_protocol:encode(19005, List),
    User#user{buffer = [Binary | User#user.buffer]}.


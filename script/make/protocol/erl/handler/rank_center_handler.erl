-module(rank_center_handler).
-export([handle/3]).
-export([send_rank_server_level/3]).
-export([send_rank_server_fight/3]).
-export([send_rank_server_achievement/3]).
-export([send_rank_server_wealth/3]).
-export([send_rank_server_classes/3]).
-include("user.hrl").

handle(User, 19101, []) ->
    rank_server:query_center(User, 19101);

handle(User, 19102, []) ->
    rank_server:query_center(User, 19102);

handle(User, 19103, []) ->
    rank_server:query_center(User, 19103);

handle(User, 19104, []) ->
    rank_server:query_center(User, 19104);

handle(User, 19105, []) ->
    rank_server:query_center(User, 19105);

handle(_, Protocol, Data) ->
    {error, Protocol, Data}.

send_rank_server_level(User, 19101, List) ->
    {ok, Binary} = rank_center_protocol:encode(19101, List),
    User#user{buffer = [Binary | User#user.buffer]}.

send_rank_server_fight(User, 19102, List) ->
    {ok, Binary} = rank_center_protocol:encode(19102, List),
    User#user{buffer = [Binary | User#user.buffer]}.

send_rank_server_achievement(User, 19103, List) ->
    {ok, Binary} = rank_center_protocol:encode(19103, List),
    User#user{buffer = [Binary | User#user.buffer]}.

send_rank_server_wealth(User, 19104, List) ->
    {ok, Binary} = rank_center_protocol:encode(19104, List),
    User#user{buffer = [Binary | User#user.buffer]}.

send_rank_server_classes(User, 19105, List) ->
    {ok, Binary} = rank_center_protocol:encode(19105, List),
    User#user{buffer = [Binary | User#user.buffer]}.


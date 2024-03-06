-module(role_handler).
-export([handle/3]).
-export([send_query/2]).
-export([send_asset_query/2]).
-export([send_vip_query/2]).
-include("user.hrl").

handle(User, 10101, []) ->
    role:query(User);

handle(User, 10102, []) ->
    asset:query(User);

handle(User, 10103, []) ->
    vip:query(User);

handle(_, Protocol, Data) ->
    {error, Protocol, Data}.

send_query(User, Role) ->
    {ok, Binary} = role_protocol:encode(10101, Role),
    User#user{buffer = [Binary | User#user.buffer]}.

send_asset_query(User, Asset) ->
    {ok, Binary} = role_protocol:encode(10102, Asset),
    User#user{buffer = [Binary | User#user.buffer]}.

send_vip_query(User, Vip) ->
    {ok, Binary} = role_protocol:encode(10103, Vip),
    User#user{buffer = [Binary | User#user.buffer]}.


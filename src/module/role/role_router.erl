%%%-------------------------------------------------------------------
%%% @doc
%%% module role protocol routing
%%% @end
%%%-------------------------------------------------------------------
-module(role_router).
%% API
-export([read/2, write/2]).
-export([handle_routing/3]).
%% Includes
-include("user.hrl").
%%%===================================================================
%%% API
%%%===================================================================
%% @doc handle packet data
-spec read(Protocol :: non_neg_integer(), Binary :: binary()) -> {ok, list()} | {error, non_neg_integer(), binary()}.
read(Protocol, Binary) ->
    case Protocol div 100 of
        100 ->
            account_protocol:read(Protocol, Binary);
        101 ->
            role_protocol:read(Protocol, Binary);
        102 ->
            asset_protocol:read(Protocol, Binary);
        111 ->
            item_protocol:read(Protocol, Binary);
        112 ->
            quest_protocol:read(Protocol, Binary);
        113 ->
            shop_protocol:read(Protocol, Binary);
        114 ->
            mail_protocol:read(Protocol, Binary);
        115 ->
            friend_protocol:read(Protocol, Binary);
        116 ->
            chat_protocol:read(Protocol, Binary);
        150 ->
            key_protocol:read(Protocol, Binary);
        190 ->
            rank_protocol:read(Protocol, Binary);
        200 ->
            map_protocol:read(Protocol, Binary);
        500 ->
            notice_protocol:read(Protocol, Binary);
        _ ->
            {error, Protocol}
    end.


%% @doc handle packet data
-spec write(Protocol :: non_neg_integer(), Binary :: binary()) -> {ok, list()} | {error, non_neg_integer(), binary()}.
write(Protocol, Binary) ->
    case Protocol div 100 of
        100 ->
            account_protocol:write(Protocol, Binary);
        101 ->
            role_protocol:write(Protocol, Binary);
        102 ->
            asset_protocol:write(Protocol, Binary);
        111 ->
            item_protocol:write(Protocol, Binary);
        112 ->
            quest_protocol:write(Protocol, Binary);
        113 ->
            shop_protocol:write(Protocol, Binary);
        114 ->
            mail_protocol:write(Protocol, Binary);
        115 ->
            friend_protocol:write(Protocol, Binary);
        116 ->
            chat_protocol:write(Protocol, Binary);
        150 ->
            key_protocol:write(Protocol, Binary);
        190 ->
            rank_protocol:write(Protocol, Binary);
        200 ->
            map_protocol:write(Protocol, Binary);
        500 ->
            notice_protocol:write(Protocol, Binary);
        _ ->
            {error, Protocol}
    end.


%% @doc protocol routing dispatch
-spec handle_routing(User :: #user{}, Protocol :: non_neg_integer(), Data :: list()) -> Result :: ok | {ok, #user{}} | {reply, term(), #user{}} | {reply, term()} | {error, protocol, non_neg_integer()} | term().
handle_routing(User, Protocol, Data) ->
    case Protocol div 100 of
        100 ->
            ok;
        101 ->
            role_handler:handle(Protocol, User, Data);
        102 ->
            asset_handler:handle(Protocol, User, Data);
        111 ->
            item_handler:handle(Protocol, User, Data);
        112 ->
            quest_handler:handle(Protocol, User, Data);
        113 ->
            shop_handler:handle(Protocol, User, Data);
        114 ->
            mail_handler:handle(Protocol, User, Data);
        115 ->
            friend_handler:handle(Protocol, User, Data);
        116 ->
            chat_handler:handle(Protocol, User, Data);
        150 ->
            key_handler:handle(Protocol, User, Data);
        190 ->
            rank_handler:handle(Protocol, User, Data);
        200 ->
            map_handler:handle(Protocol, User, Data);
        500 ->
            ok;
        _ ->
            {error, protocol, Protocol}
    end.
%%%===================================================================
%%% Internal functions
%%%===================================================================

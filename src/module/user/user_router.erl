%%%-------------------------------------------------------------------
%%% @doc
%%% user router
%%% @end
%%%-------------------------------------------------------------------
-module(user_router).
-compile(nowarn_unused_record).
%% API
-export([read/2, write/2]).
-export([dispatch/3]).
-export([interval/2]).
%% Includes
-include("common.hrl").
-include("net.hrl").
-include("user.hrl").
%% Records
-record(protocol_interval, {}).
%%%===================================================================
%%% API functions
%%%===================================================================
%% @doc read binary data
-spec read(Protocol :: non_neg_integer(), Binary :: binary()) -> {ok, list()} | {error, non_neg_integer(), binary()}.
read(Protocol, Binary) ->
    case Protocol div 100 of
        100 ->
            account_protocol:read(Protocol, Binary);
        101 ->
            role_protocol:read(Protocol, Binary);
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
        117 ->
            skill_protocol:read(Protocol, Binary);
        118 ->
            buff_protocol:read(Protocol, Binary);
        119 ->
            title_protocol:read(Protocol, Binary);
        150 ->
            welfare_protocol:read(Protocol, Binary);
        161 ->
            auction_protocol:read(Protocol, Binary);
        170 ->
            dungeon_protocol:read(Protocol, Binary);
        180 ->
            war_protocol:read(Protocol, Binary);
        190 ->
            rank_protocol:read(Protocol, Binary);
        191 ->
            rank_center_protocol:read(Protocol, Binary);
        192 ->
            rank_world_protocol:read(Protocol, Binary);
        200 ->
            map_protocol:read(Protocol, Binary);
        301 ->
            guild_protocol:read(Protocol, Binary);
        500 ->
            notice_protocol:read(Protocol, Binary);
        600 ->
            cheat_protocol:read(Protocol, Binary);
        _ ->
            {error, Protocol, Binary}
    end.


%% @doc write binary data
-spec write(Protocol :: non_neg_integer(), Data :: term()) -> {ok, binary()} | {error, non_neg_integer(), term()}.
write(Protocol, Data) ->
    case Protocol div 100 of
        100 ->
            account_protocol:write(Protocol, Data);
        101 ->
            role_protocol:write(Protocol, Data);
        111 ->
            item_protocol:write(Protocol, Data);
        112 ->
            quest_protocol:write(Protocol, Data);
        113 ->
            shop_protocol:write(Protocol, Data);
        114 ->
            mail_protocol:write(Protocol, Data);
        115 ->
            friend_protocol:write(Protocol, Data);
        116 ->
            chat_protocol:write(Protocol, Data);
        117 ->
            skill_protocol:write(Protocol, Data);
        118 ->
            buff_protocol:write(Protocol, Data);
        119 ->
            title_protocol:write(Protocol, Data);
        150 ->
            welfare_protocol:write(Protocol, Data);
        161 ->
            auction_protocol:write(Protocol, Data);
        170 ->
            dungeon_protocol:write(Protocol, Data);
        180 ->
            war_protocol:write(Protocol, Data);
        190 ->
            rank_protocol:write(Protocol, Data);
        191 ->
            rank_center_protocol:write(Protocol, Data);
        192 ->
            rank_world_protocol:write(Protocol, Data);
        200 ->
            map_protocol:write(Protocol, Data);
        301 ->
            guild_protocol:write(Protocol, Data);
        500 ->
            notice_protocol:write(Protocol, Data);
        600 ->
            cheat_protocol:write(Protocol, Data);
        _ ->
            {error, Protocol, Data}
    end.


%% @doc protocol dispatch
-spec dispatch(User :: #user{}, Protocol :: non_neg_integer(), Data :: list()) -> Result :: ok() | error() | term().
dispatch(User, Protocol, Data) ->
    case Protocol div 100 of
        100 ->
            ok;
        101 ->
            role_handler:handle(Protocol, User, Data);
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
        117 ->
            skill_handler:handle(Protocol, User, Data);
        118 ->
            buff_handler:handle(Protocol, User, Data);
        119 ->
            title_handler:handle(Protocol, User, Data);
        150 ->
            welfare_handler:handle(Protocol, User, Data);
        161 ->
            auction_handler:handle(Protocol, User, Data);
        170 ->
            dungeon_handler:handle(Protocol, User, Data);
        180 ->
            war_handler:handle(Protocol, User, Data);
        190 ->
            rank_handler:handle(Protocol, User, Data);
        191 ->
            rank_center_handler:handle(Protocol, User, Data);
        192 ->
            rank_world_handler:handle(Protocol, User, Data);
        200 ->
            map_handler:handle(Protocol, User, Data);
        301 ->
            guild_handler:handle(Protocol, User, Data);
        500 ->
            ok;
        600 ->
            cheat_handler:handle(Protocol, User, Data);
        _ ->
            {error, protocol, Protocol}
    end.


%% @doc protocol interval control
-spec interval(State :: #client{}, Protocol :: non_neg_integer()) -> {boolean(), #client{}}.
interval(State = #client{protocol_interval = undefined}, _) ->
    {true, State#client{protocol_interval = #protocol_interval{}}};
interval(State, _) ->
    {true, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

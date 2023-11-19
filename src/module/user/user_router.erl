%%%-------------------------------------------------------------------
%%% @doc
%%% user router
%%% @end
%%%-------------------------------------------------------------------
-module(user_router).
-compile(nowarn_unused_record).
%% API
-export([decode/2, encode/2]).
-export([dispatch/3]).
-export([interval/2]).
%% Includes
-include("common.hrl").
-include("net.hrl").
-include("user.hrl").
%% Records
-record(protocol_interval, {'10000' = 0, '10001' = 0, '10002' = 0, '10003' = 0, '10004' = 0}).
%%%===================================================================
%%% API functions
%%%===================================================================
%% @doc decode binary data
-spec decode(Protocol :: non_neg_integer(), Binary :: binary()) -> {ok, list()} | {error, non_neg_integer(), binary()}.
decode(Protocol, Binary) ->
    case Protocol div 100 of
        100 ->
            account_protocol:decode(Protocol, Binary);
        101 ->
            role_protocol:decode(Protocol, Binary);
        111 ->
            item_protocol:decode(Protocol, Binary);
        112 ->
            task_protocol:decode(Protocol, Binary);
        113 ->
            shop_protocol:decode(Protocol, Binary);
        114 ->
            mail_protocol:decode(Protocol, Binary);
        115 ->
            friend_protocol:decode(Protocol, Binary);
        116 ->
            chat_protocol:decode(Protocol, Binary);
        117 ->
            skill_protocol:decode(Protocol, Binary);
        118 ->
            buff_protocol:decode(Protocol, Binary);
        119 ->
            title_protocol:decode(Protocol, Binary);
        120 ->
            fashion_protocol:decode(Protocol, Binary);
        121 ->
            bubble_protocol:decode(Protocol, Binary);
        122 ->
            achievement_protocol:decode(Protocol, Binary);
        123 ->
            daily_protocol:decode(Protocol, Binary);
        150 ->
            welfare_protocol:decode(Protocol, Binary);
        161 ->
            auction_protocol:decode(Protocol, Binary);
        170 ->
            dungeon_protocol:decode(Protocol, Binary);
        180 ->
            war_protocol:decode(Protocol, Binary);
        190 ->
            rank_protocol:decode(Protocol, Binary);
        191 ->
            rank_center_protocol:decode(Protocol, Binary);
        192 ->
            rank_world_protocol:decode(Protocol, Binary);
        200 ->
            map_protocol:decode(Protocol, Binary);
        301 ->
            guild_protocol:decode(Protocol, Binary);
        500 ->
            notice_protocol:decode(Protocol, Binary);
        600 ->
            cheat_protocol:decode(Protocol, Binary);
        655 ->
            test_protocol:decode(Protocol, Binary);
        _ ->
            {error, Protocol, Binary}
    end.


%% @doc encode binary data
-spec encode(Protocol :: non_neg_integer(), Data :: term()) -> {ok, binary()} | {error, non_neg_integer(), term()}.
encode(Protocol, Data) ->
    case Protocol div 100 of
        100 ->
            account_protocol:encode(Protocol, Data);
        101 ->
            role_protocol:encode(Protocol, Data);
        111 ->
            item_protocol:encode(Protocol, Data);
        112 ->
            task_protocol:encode(Protocol, Data);
        113 ->
            shop_protocol:encode(Protocol, Data);
        114 ->
            mail_protocol:encode(Protocol, Data);
        115 ->
            friend_protocol:encode(Protocol, Data);
        116 ->
            chat_protocol:encode(Protocol, Data);
        117 ->
            skill_protocol:encode(Protocol, Data);
        118 ->
            buff_protocol:encode(Protocol, Data);
        119 ->
            title_protocol:encode(Protocol, Data);
        120 ->
            fashion_protocol:encode(Protocol, Data);
        121 ->
            bubble_protocol:encode(Protocol, Data);
        122 ->
            achievement_protocol:encode(Protocol, Data);
        123 ->
            daily_protocol:encode(Protocol, Data);
        150 ->
            welfare_protocol:encode(Protocol, Data);
        161 ->
            auction_protocol:encode(Protocol, Data);
        170 ->
            dungeon_protocol:encode(Protocol, Data);
        180 ->
            war_protocol:encode(Protocol, Data);
        190 ->
            rank_protocol:encode(Protocol, Data);
        191 ->
            rank_center_protocol:encode(Protocol, Data);
        192 ->
            rank_world_protocol:encode(Protocol, Data);
        200 ->
            map_protocol:encode(Protocol, Data);
        301 ->
            guild_protocol:encode(Protocol, Data);
        500 ->
            notice_protocol:encode(Protocol, Data);
        600 ->
            cheat_protocol:encode(Protocol, Data);
        655 ->
            test_protocol:encode(Protocol, Data);
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
            role_handler:handle(User, Protocol, Data);
        111 ->
            item_handler:handle(User, Protocol, Data);
        112 ->
            task_handler:handle(User, Protocol, Data);
        113 ->
            shop_handler:handle(User, Protocol, Data);
        114 ->
            mail_handler:handle(User, Protocol, Data);
        115 ->
            friend_handler:handle(User, Protocol, Data);
        116 ->
            chat_handler:handle(User, Protocol, Data);
        117 ->
            skill_handler:handle(User, Protocol, Data);
        118 ->
            buff_handler:handle(User, Protocol, Data);
        119 ->
            title_handler:handle(User, Protocol, Data);
        120 ->
            fashion_handler:handle(User, Protocol, Data);
        121 ->
            bubble_handler:handle(User, Protocol, Data);
        122 ->
            achievement_handler:handle(User, Protocol, Data);
        123 ->
            daily_handler:handle(User, Protocol, Data);
        150 ->
            welfare_handler:handle(User, Protocol, Data);
        161 ->
            auction_handler:handle(User, Protocol, Data);
        170 ->
            dungeon_handler:handle(User, Protocol, Data);
        180 ->
            war_handler:handle(User, Protocol, Data);
        190 ->
            rank_handler:handle(User, Protocol, Data);
        191 ->
            rank_center_handler:handle(User, Protocol, Data);
        192 ->
            rank_world_handler:handle(User, Protocol, Data);
        200 ->
            map_handler:handle(User, Protocol, Data);
        301 ->
            guild_handler:handle(User, Protocol, Data);
        500 ->
            notice_handler:handle(User, Protocol, Data);
        600 ->
            cheat_handler:handle(User, Protocol, Data);
        655 ->
            ok;
        _ ->
            {error, Protocol, Data}
    end.


%% @doc protocol interval control
-spec interval(State :: #client{}, Protocol :: non_neg_integer()) -> {boolean(), #client{}}.
interval(State = #client{protocol_interval = ProtocolInterval = #protocol_interval{'10000' = Before}}, 10000) ->
    Now = time:millisecond(),
    case Before + 30000 =< Now of
        true ->
            {true, State#client{protocol_interval = ProtocolInterval#protocol_interval{'10000' = Now}}};
        false ->
            {false, State}
    end;
interval(State = #client{protocol_interval = ProtocolInterval = #protocol_interval{'10001' = Before}}, 10001) ->
    Now = time:millisecond(),
    case Before + 1000 =< Now of
        true ->
            {true, State#client{protocol_interval = ProtocolInterval#protocol_interval{'10001' = Now}}};
        false ->
            {false, State}
    end;
interval(State = #client{protocol_interval = ProtocolInterval = #protocol_interval{'10002' = Before}}, 10002) ->
    Now = time:millisecond(),
    case Before + 1000 =< Now of
        true ->
            {true, State#client{protocol_interval = ProtocolInterval#protocol_interval{'10002' = Now}}};
        false ->
            {false, State}
    end;
interval(State = #client{protocol_interval = ProtocolInterval = #protocol_interval{'10003' = Before}}, 10003) ->
    Now = time:millisecond(),
    case Before + 1000 =< Now of
        true ->
            {true, State#client{protocol_interval = ProtocolInterval#protocol_interval{'10003' = Now}}};
        false ->
            {false, State}
    end;
interval(State = #client{protocol_interval = ProtocolInterval = #protocol_interval{'10004' = Before}}, 10004) ->
    Now = time:millisecond(),
    case Before + 1000 =< Now of
        true ->
            {true, State#client{protocol_interval = ProtocolInterval#protocol_interval{'10004' = Now}}};
        false ->
            {false, State}
    end;
interval(State = #client{protocol_interval = undefined}, _) ->
    {true, State#client{protocol_interval = #protocol_interval{}}};
interval(State, _) ->
    {true, State}.


%%%===================================================================
%%% Internal functions
%%%===================================================================

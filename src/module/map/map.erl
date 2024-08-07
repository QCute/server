%%%-------------------------------------------------------------------
%%% @doc
%%% map
%%% @end
%%%-------------------------------------------------------------------
-module(map).
-export([broadcast/2, broadcast/3]).
-export([notify/4, notify/5]).
-export([enter/2, leave/2, move/4]).
-export([slice/2, is_in_slice/3, is_same_slice/4, is_in_distance/3, is_in_distance/5]).
-include("common.hrl").
-include("protocol.hrl").
-include("map.hrl").
-define(MAP_POSITION_NULL, 0).
-define(SLICE_WIDTH, 500).
-define(SLICE_HEIGHT, 500).
%%%===================================================================
%%% API functions
%%%===================================================================
%% @doc broadcast
-spec broadcast(#map{}, binary()) -> ok.
broadcast(#map{fighter = FighterList}, Binary) ->
    broadcast_loop(FighterList, Binary, 0).

%% @doc broadcast
-spec broadcast(#map{}, binary(), non_neg_integer()) -> ok.
broadcast(#map{fighter = FighterList}, Binary, ExceptId) ->
    broadcast_loop(FighterList, Binary, ExceptId).

broadcast_loop([], _, _) ->
    ok;
broadcast_loop([#fighter{id = ExceptId} | T], Binary, ExceptId) ->
    %% except this fighter
    broadcast_loop(T, Binary, ExceptId);
broadcast_loop([#fighter{data = #fighter_role{sender_pid = SenderPid}} | T], Binary, ExceptId) ->
    %% send message
    user_sender:send(SenderPid, Binary),
    broadcast_loop(T, Binary, ExceptId);
broadcast_loop([_ | T], Binary, ExceptId) ->
    %% other fighter object
    broadcast_loop(T, Binary, ExceptId).

%% @doc notify
-spec notify(#map{}, non_neg_integer(), non_neg_integer(), binary()) -> ok.
notify(State = #map{type = full}, _, _, Binary) ->
    broadcast(State, Binary);
notify(#map{fighter = FighterList}, X, Y, Binary) ->
    notify_slice(FighterList, X, Y, Binary, 0).

%% @doc notify
-spec notify(#map{}, binary(), non_neg_integer(), non_neg_integer(), non_neg_integer()) -> ok.
notify(State = #map{type = full}, _, _, Binary, ExceptId) ->
    broadcast(State, Binary, ExceptId);
notify(#map{fighter = FighterList}, X, Y, Binary, ExceptId) ->
    notify_slice(FighterList, X, Y, Binary, ExceptId).

%% notify data slice
notify_slice([], _, _, _, _) ->
    ok;
notify_slice([#fighter{id = ExceptId} | T], X, Y, Binary, ExceptId) ->
    %% except this fighter
    notify_slice(T, X, Y, Binary, ExceptId);
notify_slice([#fighter{data = #fighter_role{sender_pid = SenderPid}, x = ThisX, y = ThisY} | T], X, Y, Binary, ExceptId) ->
    case is_same_slice(X, Y, ThisX, ThisY) of
        true ->
            user_sender:send(SenderPid, Binary);
        false ->
            skip
    end,
    notify_slice(T, X, Y, Binary, ExceptId);
notify_slice([_ | T], X, Y, Binary, ExceptId) ->
    %% other fighter object
    notify_slice(T, X, Y, Binary, ExceptId).

%% @doc fighter enter
-spec enter(#map{}, #fighter{}) -> ok.
enter(#map{fighter = FighterList}, Fighter = #fighter{id = Id, x = X, y = Y}) ->
    {ok, NewBinary} = user_router:encode(?PROTOCOL_MAP_FIGHTER, [Fighter]),
    enter_notify_slice(FighterList, X, Y, NewBinary, Id).

%% enter notify diff slice
enter_notify_slice([], _, _, _, _) ->
    ok;
enter_notify_slice([#fighter{id = ExceptId} | T], NewX, NewY, New, ExceptId) ->
    %% except this fighter
    enter_notify_slice(T, NewX, NewY, New, ExceptId);
enter_notify_slice([#fighter{data = #fighter_role{sender_pid = SenderPid}, x = ThisX, y = ThisY} | T], NewX, NewY, New, ExceptId) ->
    case is_same_slice(NewX, NewY, ThisX, ThisY) of
        true ->
            user_sender:send(SenderPid, New);
        false ->
            skip
    end,
    enter_notify_slice(T, NewX, NewY, New, ExceptId);
enter_notify_slice([_ | T], NewX, NewY, New, ExceptId) ->
    %% other fighter object
    enter_notify_slice(T, NewX, NewY, New, ExceptId).

%% @doc fighter leave
-spec leave(#map{}, #fighter{}) -> ok.
leave(#map{fighter = FighterList}, Fighter = #fighter{id = Id, x = X, y = Y}) ->
    {ok, LeaveBinary} = user_router:encode(?PROTOCOL_MAP_FIGHTER_LEAVE, Fighter),
    leave_notify_slice(FighterList, X, Y, LeaveBinary, Id).

%% leave notify diff slice
leave_notify_slice([], _, _, _, _) ->
    ok;
leave_notify_slice([#fighter{id = ExceptId} | T], NewX, NewY, Leave, ExceptId) ->
    %% except this fighter
    leave_notify_slice(T, NewX, NewY, Leave, ExceptId);
leave_notify_slice([#fighter{data = #fighter_role{sender_pid = SenderPid}, x = ThisX, y = ThisY} | T], NewX, NewY, Leave, ExceptId) ->
    case is_same_slice(NewX, NewY, ThisX, ThisY) of
        true ->
            user_sender:send(SenderPid, Leave);
        false ->
            skip
    end,
    leave_notify_slice(T, NewX, NewY, Leave, ExceptId);
leave_notify_slice([_ | T], NewX, NewY, Leave, ExceptId) ->
    %% other fighter object
    leave_notify_slice(T, NewX, NewY, Leave, ExceptId).

%% @doc fighter move
-spec move(#map{}, non_neg_integer(), non_neg_integer(), #fighter{}) -> ok.
move(State = #map{type = full}, _, _, NewFighter = #fighter{id = Id}) ->
    {ok, Data} = user_router:encode(?PROTOCOL_MAP_FIGHTER_MOVE, NewFighter),
    broadcast(State, Data, Id);
move(#map{type = slice, fighter = FighterList}, OldX, OldY, NewFighter = #fighter{id = Id, x = NewX, y = NewY}) ->
    {ok, NewBinary} = user_router:encode(?PROTOCOL_MAP_FIGHTER, [NewFighter]),
    {ok, MoveBinary} = user_router:encode(?PROTOCOL_MAP_FIGHTER_MOVE, NewFighter),
    {ok, LeaveBinary} = user_router:encode(?PROTOCOL_MAP_FIGHTER_LEAVE, NewFighter),
    move_notify_slice(FighterList, OldX, OldY, NewX, NewY, NewBinary, MoveBinary, LeaveBinary, Id).

%% move notify diff slice
move_notify_slice([], _, _, _, _, _, _, _, _) ->
    ok;
move_notify_slice([#fighter{id = ExceptId} | T], OldX, OldY, NewX, NewY, New, Move, Leave, ExceptId) ->
    %% except this fighter
    move_notify_slice(T, OldX, OldY, NewX, NewY, New, Move, Leave, ExceptId);
move_notify_slice([#fighter{data = #fighter_role{sender_pid = SenderPid}, x = ThisX, y = ThisY} | T], OldX, OldY, NewX, NewY, New, Move, Leave, ExceptId) ->
    case {is_same_slice(NewX, NewY, ThisX, ThisY), is_same_slice(OldX, OldY, ThisX, ThisY)} of
        {true, false} ->
            %% not old slice, is new slice
            user_sender:send(SenderPid, New);
        {true, true} ->
            %% old and new is same slice
            user_sender:send(SenderPid, Move);
        {false, true} ->
            %% is out of slice
            user_sender:send(SenderPid, Leave);
        {false, false} ->
            skip
    end,
    move_notify_slice(T, OldX, OldY, NewX, NewY, New, Move, Leave, ExceptId);
move_notify_slice([_ | T], OldX, OldY, NewX, NewY, New, Move, Leave, ExceptId) ->
    %% other fighter object
    move_notify_slice(T, OldX, OldY, NewX, NewY, New, Move, Leave, ExceptId).

%% @doc slice 9
-spec slice(pos_integer(), pos_integer()) -> #slice{}.
slice(X, Y) ->
    BaseWidth  = X div    ?SLICE_WIDTH  * ?SLICE_WIDTH,
    BaseHeight = Y div    ?SLICE_HEIGHT * ?SLICE_HEIGHT,
    Left   = BaseWidth -  ?SLICE_WIDTH,
    Bottom = BaseHeight - ?SLICE_HEIGHT,
    Right  = BaseWidth +  ?SLICE_WIDTH  * 2,
    Top    = BaseHeight + ?SLICE_HEIGHT * 2,
    #slice{left = Left, right = Right, top = Top, bottom = Bottom}.

%% @doc tow position is in same slice 9
-spec is_same_slice(pos_integer(), pos_integer(), pos_integer(), pos_integer()) -> boolean().
is_same_slice(OldX, OldY, NewX, NewY) ->
    (OldX div ?SLICE_WIDTH) == (NewX div ?SLICE_HEIGHT) andalso (OldY div ?SLICE_WIDTH) == (NewY div ?SLICE_HEIGHT).

%% @doc x y is in slice
-spec is_in_slice(pos_integer(), pos_integer(), #slice{}) -> boolean().
is_in_slice(X, Y, #slice{left = Left, right = Right, top = Top, bottom = Bottom}) ->
    Left =< X andalso X < Right andalso Top =< Y andalso Y < Bottom.

%% @doc is in distance
-spec is_in_distance(#fighter{} | {non_neg_integer(), non_neg_integer()}, #fighter{}| {non_neg_integer(), non_neg_integer()}, non_neg_integer()) -> boolean().
is_in_distance(#fighter{x = AttackerX, y = AttackerY}, #fighter{x = DefenderX, y = DefenderY}, Distance) ->
    is_in_distance(AttackerX, AttackerY, DefenderX, DefenderY, Distance);
is_in_distance({AttackerX, AttackerY}, {DefenderX, DefenderY}, Distance) ->
    is_in_distance(AttackerX, AttackerY, DefenderX, DefenderY, Distance).

%% @doc is in distance
-spec is_in_distance(non_neg_integer(), non_neg_integer(), non_neg_integer(), non_neg_integer(), non_neg_integer()) -> boolean().
is_in_distance(AttackerX, AttackerY, DefenderX, DefenderY, Distance) ->
    erlang:abs(AttackerX -  DefenderX) =< Distance andalso erlang:abs(AttackerY - DefenderY) =< Distance.

%%%===================================================================
%%% Internal functions
%%%===================================================================

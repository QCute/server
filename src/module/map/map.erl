%%%------------------------------------------------------------------
%%% @doc
%%% module map
%%% @end
%%%------------------------------------------------------------------
-module(map).
-export([broadcast/2, broadcast/3]).
-export([notify/4, notify/5]).
-export([enter/2, leave/2, move/6]).
-export([slice/2, is_in_slice/3, is_same_slice/4, is_in_distance/3, is_in_distance/5]).
-include("common.hrl").
-include("protocol.hrl").
-include("map.hrl").
-define(MAP_POSITION_NULL, 65535).
-define(SLICE_WIDTH, 500).
-define(SLICE_HEIGHT, 300).
%%%==================================================================
%%% API functions
%%%==================================================================
%% @doc broadcast
-spec broadcast(#map_state{}, binary()) -> ok.
broadcast(#map_state{fighters = Fighters}, Binary) ->
    broadcast_loop(Fighters, Binary, 0).

%% @doc broadcast
-spec broadcast(#map_state{}, binary(), non_neg_integer()) -> ok.
broadcast(#map_state{fighters = Fighters}, Binary, ExceptId) ->
    broadcast_loop(Fighters, Binary, ExceptId).

broadcast_loop([], _, _) ->
    ok;
broadcast_loop([#fighter{id = ExceptId} | T], Binary, ExceptId) ->
    %% except this fighter
    broadcast_loop(T, Binary, ExceptId);
broadcast_loop([#fighter{type = ?MAP_OBJECT_ROLE, sender_pid = SenderPid} | T], Binary, ExceptId) ->
    %% send message
    user_sender:send(SenderPid, Binary),
    broadcast_loop(T, Binary, ExceptId);
broadcast_loop([_ | T], Binary, ExceptId) ->
    %% other fighter object
    broadcast_loop(T, Binary, ExceptId).

%% @doc notify
-spec notify(#map_state{}, non_neg_integer(), non_neg_integer(), binary()) -> ok.
notify(State = #map_state{type = full}, _, _, Binary) ->
    broadcast(State, Binary);
notify(State, X, Y, Binary) ->
    notify_slice(State, X, Y, Binary, 0).

%% @doc notify
-spec notify(#map_state{}, binary(), non_neg_integer(), non_neg_integer(), non_neg_integer()) -> ok.
notify(State = #map_state{type = full}, _, _, Binary, ExceptId) ->
    broadcast(State, Binary, ExceptId);
notify(#map_state{fighters = Fighters}, X, Y, Binary, ExceptId) ->
    notify_slice(Fighters, X, Y, Binary, ExceptId).

%% notify data slice
notify_slice([], _, _, _, _) ->
    ok;
notify_slice([#fighter{id = ExceptId} | T], X, Y, Binary, ExceptId) ->
    %% except this fighter
    notify_slice(T, X, Y, Binary, ExceptId);
notify_slice([#fighter{type = ?MAP_OBJECT_ROLE, sender_pid = SenderPid, x = ThisX, y = ThisY} | T], X, Y, Binary, ExceptId) ->
    case is_same_slice(X, Y, ThisX, ThisY) of
        true ->
            %% not old slice, is new slice
            user_sender:send(SenderPid, Binary);
        false ->
            skip
    end,
    notify_slice(T, X, Y, Binary, ExceptId);
notify_slice([_ | T], X, Y, Binary, ExceptId) ->
    %% other fighter object
    notify_slice(T, X, Y, Binary, ExceptId).

%% @doc fighter enter notify
-spec enter(#map_state{}, #fighter{}) -> ok.
enter(State, Fighter = #fighter{x = X, y = Y}) ->
    move(State, Fighter, ?MAP_POSITION_NULL, ?MAP_POSITION_NULL, X, Y).

%% @doc fighter leave notify
-spec leave(#map_state{}, #fighter{}) -> ok.
leave(State, Fighter = #fighter{x = X, y = Y}) ->
    move(State, Fighter, X, Y, ?MAP_POSITION_NULL, ?MAP_POSITION_NULL).

%% @doc fighter move
-spec move(#map_state{}, #fighter{}, non_neg_integer(), non_neg_integer(), non_neg_integer(), non_neg_integer()) -> ok.
move(State = #map_state{type = full}, Fighter = #fighter{id = Id}, _, _, _, _) ->
    {ok, Data} = user_router:write(?PROTOCOL_MAP_FIGHTER, [Fighter]),
    broadcast(State, Data, Id);
move(State = #map_state{type = slice}, Fighter = #fighter{id = Id}, OldX, OldY, NewX, NewY) ->
    {ok, MoveBinary} = user_router:write(?PROTOCOL_MAP_FIGHTER_MOVE, [Fighter]),
    {ok, NewBinary} = user_router:write(?PROTOCOL_MAP_FIGHTER, [Fighter]),
    {ok, RemoveBinary} = user_router:write(?PROTOCOL_MAP_FIGHTER_REMOVE, [Fighter]),
    move_notify_slice(State, OldX, OldY, NewX, NewY, NewBinary, MoveBinary, RemoveBinary, Id).

%% move notify diff slice
move_notify_slice([], _, _, _, _, _, _, _, _) ->
    ok;
move_notify_slice([#fighter{id = ExceptId} | T], OldX, OldY, NewX, NewY, New, Move, Remove, ExceptId) ->
    %% except this fighter
    move_notify_slice(T, OldX, OldY, NewX, NewY, New, Move, Remove, ExceptId);
move_notify_slice([#fighter{type = ?MAP_OBJECT_ROLE, sender_pid = SenderPid, x = ThisX, y = ThisY} | T], OldX, OldY, NewX, NewY, New, Move, Remove, ExceptId) ->
    case {is_same_slice(NewX, NewY, ThisX, ThisY), is_same_slice(OldX, OldY, ThisX, ThisY)} of
        {true, false} ->
            %% not old slice, is new slice
            user_sender:send(SenderPid, New);
        {true, true} ->
            %% old and new is same slice
            user_sender:send(SenderPid, Move);
        {false, true} ->
            %% is out of slice
            user_sender:send(SenderPid, Remove);
        {false, false} ->
            skip
    end,
    move_notify_slice(T, OldX, OldY, NewX, NewY, New, Move, Remove, ExceptId);
move_notify_slice([_ | T], OldX, OldY, NewX, NewY, New, Move, Remove, ExceptId) ->
    %% other fighter object
    move_notify_slice(T, OldX, OldY, NewX, NewY, New, Move, Remove, ExceptId).

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
    (OldX div ?SLICE_WIDTH) == (OldY div ?SLICE_HEIGHT) andalso (NewX div ?SLICE_WIDTH) == (NewY div ?SLICE_HEIGHT).

%% @doc x y is in slice
-spec is_in_slice(pos_integer(), pos_integer(), #slice{}) -> boolean().
is_in_slice(X, Y, #slice{left = Left, right = Right, top = Top, bottom = Bottom}) ->
    Left =< X andalso X < Right andalso Bottom =< Y andalso Y < Top.

%% @doc is in distance
-spec is_in_distance(#fighter{} | {non_neg_integer(), non_neg_integer()}, #fighter{}| {non_neg_integer(), non_neg_integer()}, non_neg_integer()) -> boolean().
is_in_distance(#fighter{x = AttackerX, y = AttackerY}, #fighter{x = DefenderX, y = DefenderY}, Distance) ->
    erlang:abs(AttackerX -  DefenderX) =< Distance andalso erlang:abs(AttackerY - DefenderY) =< Distance;
is_in_distance({AttackerX, AttackerY}, {DefenderX, DefenderY}, Distance) ->
    erlang:abs(AttackerX -  DefenderX) =< Distance andalso erlang:abs(AttackerY - DefenderY) =< Distance.

%% @doc is in distance
-spec is_in_distance(non_neg_integer(), non_neg_integer(), non_neg_integer(), non_neg_integer(), non_neg_integer()) -> boolean().
is_in_distance(AttackerX, AttackerY, DefenderX, DefenderY, Distance) ->
    erlang:abs(AttackerX -  DefenderX) =< Distance andalso erlang:abs(AttackerY - DefenderY) =< Distance.

%%%==================================================================
%%% Internal functions
%%%==================================================================

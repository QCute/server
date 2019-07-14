%%%-------------------------------------------------------------------
%%% @doc
%%% module map
%%% @end
%%%-------------------------------------------------------------------
-module(map).
-export([broadcast/2, broadcast/3]).
-export([move/7, move/8]).
-export([slice/2, is_in_slice/3, is_same_slice/4, is_in_distance/3]).
-include("common.hrl").
-include("user.hrl").
-include("map.hrl").
-define(SLICE_WIDTH, 500).
-define(SLICE_HEIGHT, 300).

%%%===================================================================
%%% API
%%%===================================================================
broadcast(State, Binary) ->
    broadcast(State, Binary, 0).
broadcast(#map_state{fighters = List}, Binary, ExceptId) ->
    F = fun
        (#fighter{id = RoleId, sender_pid = SenderPid}) when RoleId =/= ExceptId ->
            %% notify role without except given id
             user_sender:send(SenderPid, Binary);
        (_) ->
            skip
    end,
    lists:foreach(F, List).

move(State, Id, OldX, OldY, NewX, NewY, Binary) ->
    move(State, Id, OldX, OldY, NewX, NewY, Binary, 0).
move(State = #map_state{type = Type}, Id, OldX, OldY, NewX, NewY, Binary, ExceptId) ->
    SameSlice = is_same_slice(OldX, OldY, NewX, NewY),
    OldSlice = slice(OldX, OldY),
    NewSlice = slice(NewX, NewY),
    %% full broadcast can support
    F = fun
        %% notify role without except given id
        (#fighter{id = RoleId, sender_pid = Pid, x = X, y = Y}) when RoleId =/= ExceptId ->
            case Type of
                slice ->
                    move_notify(Pid, X, Y, Id, OldSlice, NewSlice, SameSlice, Binary);
                full ->
                    user_sender:send(Pid, Binary)
            end;
        (_) ->
            skip
    end,
    lists:foreach(F, State#map_state.fighters).

%% move notify according map slice 9
move_notify(Pid, X, Y, Id, OldSlice, NewSlice, SameSlice, Binary) ->
    case SameSlice of
        true ->
            %% target move in same slice
            case is_in_slice(X, Y, OldSlice) of
                true ->
                    %% role in old slice, update new position
                    user_sender:send(Pid, Binary);
                false ->
                    %% in other slice, skip it
                    skip
            end;
        false ->
            %% target move in diff slice
            case is_in_slice(X, Y, OldSlice) of
                true ->
                    %% role in old slice, notify client remove it
                    {ok, RemoveBinary} = notice_protocol:write(56789, [Id]),
                    user_sender:send(Pid, RemoveBinary);
                false ->
                    case is_in_slice(X, Y, NewSlice) of
                        true ->
                            %% role in new slice, notify client add it
                            user_sender:send(Pid, Binary);
                        false ->
                            %% role in other slice, skip it
                            skip
                    end
            end
    end.

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
is_in_slice(X, Y, #slice{left = Left, right = Right, top = Top, bottom = Bottom}) ->
    Left =< X andalso X < Right andalso Bottom =< Y andalso Y < Top.

%% @doc is in distance
is_in_distance(#fighter{x = AttackerX, y = AttackerY}, #fighter{x = DefenderX, y = DefenderY}, Distance) ->
    AttackerX -  DefenderX =< Distance orelse AttackerY - DefenderY =< Distance;
is_in_distance(#fighter{x = AttackerX, y = AttackerY}, #monster{x = DefenderX, y = DefenderY}, Distance) ->
    AttackerX -  DefenderX =< Distance orelse AttackerY - DefenderY =< Distance;
is_in_distance(#monster{x = AttackerX, y = AttackerY}, #fighter{x = DefenderX, y = DefenderY}, Distance) ->
    AttackerX -  DefenderX =< Distance orelse AttackerY - DefenderY =< Distance;
is_in_distance(#monster{x = AttackerX, y = AttackerY}, #monster{x = DefenderX, y = DefenderY}, Distance) ->
    AttackerX -  DefenderX =< Distance orelse AttackerY - DefenderY =< Distance.

%%%===================================================================
%%% Internal functions
%%%===================================================================

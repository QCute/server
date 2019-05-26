%%%-------------------------------------------------------------------
%%% @doc
%%% module a_star
%%% a* path finding algorithm
%%% @end
%%%-------------------------------------------------------------------
-module(way).
%% API
-export([next/4]).
-export_type([point/0]).
%% main type
-type point() :: {non_neg_integer(), non_neg_integer()}.
%%%===================================================================
%%% API
%%%===================================================================
%% @doc walkable set
-spec walkable(Id :: non_neg_integer(), Point :: point()) -> boolean().
walkable(_, {X, Y}) when X =< 0 orelse Y =< 0 ->
    false;
walkable(_, _) ->
    true.

%% @doc next way point
-spec next(Id :: non_neg_integer(), Start :: point(), End :: point(), Distance :: non_neg_integer()) -> point().
next(_, {X, Y}, {X, Y}, _) ->
    {X, Y};
next(Id, Start, End, Distance) ->
    case trace_line(Start, End, Distance) of
        true ->
            a_star(Id, Start, End);
        Point ->
            case walkable(Id, Point) of
                true ->
                    Point;
                _ ->
                    a_star(Id, Start, End)
            end
    end.

trace_line(Start, End, Distance) ->
    trace_line(Start, End, Distance, 1).
trace_line(Start = {X1, Y1}, End = {X2, Y2}, Distance, Speed) ->
    case abs(X2 - X1) =< Distance andalso abs(Y2 - Y1) =< Distance of
        true ->
            {X1, Y1};
        _ ->
            line(Start, End, Speed)
    end.

line({X1, Y1}, {X2, Y2}, Speed) when X2 == X1 andalso Y2 > Y1 ->
    {X1, Y1 + Speed};
line({X1, Y1}, {X2, Y2}, Speed) when X2 == X1 andalso Y2 < Y1 ->
    {X1, Y1 - Speed};
line({X1, Y1}, {X2, Y2}, Speed) when X2 > X1 andalso Y2 == Y1 ->
    {X1 + Speed, Y1};
line({X1, Y1}, {X2, Y2}, Speed) when X2 < X1 andalso Y2 == Y1 ->
    {X1 - Speed, Y1};
line({X1, Y1}, {X2, Y2}, Speed) when X2 > X1 andalso Y2 > Y1 ->
    {X1 + Speed, Y1 + Speed};
line({X1, Y1}, {X2, Y2}, Speed) when X2 < X1 andalso Y2 > Y1 ->
    {X1 - Speed, Y1 + Speed};
line({X1, Y1}, {X2, Y2}, Speed) when X2 > X1 andalso Y2 < Y1 ->
    {X1 + Speed, Y1 - Speed};
line({X1, Y1}, {X2, Y2}, Speed) when X2 < X1 andalso Y2 < Y1 ->
    {X1 - Speed, Y1 - Speed};
line(_, _, _) ->
    true.

%% a* path finding algorithm
a_star(Id, Start, End) ->
    a_star(Id, Start, End, 1).
a_star(Id, Start = {X, Y}, End, Speed) ->
    %% 8 directions point
    List = [
        {X + Speed, Y}, {X, Y + Speed},
        {X - Speed, Y}, {X, Y - Speed},
        {X + Speed, Y + Speed}, {X - Speed, Y + Speed},
        {X + Speed, Y - Speed}, {X - Speed, Y - Speed}
    ],
    loop(List, Id, Start, End, Start).

loop([], _, _, _, {X, Y}) ->
    {X, Y};
loop([Point = {X2, Y2} | T], Id, Start, End = {X, Y}, OldPoint = {X1, Y1}) ->
    case walkable(Id, {X2, Y2}) of
        true ->
            Distance1 = abs(X1 - X) * abs(X1 - X) + abs(Y1 - Y) * abs(Y1 - Y),
            Distance2 = abs(X2 - X) * abs(X2 - X) + abs(Y2 - Y) * abs(Y2 - Y),
            case (Distance1 =< Distance2 orelse Start =:= OldPoint) of
                true ->
                    loop(T, Id, Start, End, Point);
                _ ->
                    loop(T, Id, Start, End, OldPoint)
            end;
        false ->
            loop(T, Id, Start, End, OldPoint)
    end.

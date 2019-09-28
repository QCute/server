%%%-------------------------------------------------------------------
%%% @doc
%%% module a_star
%%% a* path finding algorithm
%%% @end
%%%-------------------------------------------------------------------
-module(a_star).
%% API
-export([find/3]).
-export_type([point/0]).
%% main type
-type point() :: {non_neg_integer(), non_neg_integer()}.
%% Macros
%% coordinate directions
-define(DIRECTIONS, [1, 2, 3, 4, 5, 6, 7, 8]).
%% Records
%% state
-record(state, {open_trees, close_sets, parents_trees, dst}).
%% coordinate
-record(coordinate, {point = {0, 0}, id = 0, g = 0, h = 0, f = 0}).

%%%===================================================================
%%% API
%%%===================================================================
%% @doc walkable set
-spec walkable(Coordinate :: #coordinate{}) -> boolean().
walkable(#coordinate{point = {X, Y}}) when X =< 0 orelse Y =< 0 ->
    false;
walkable(_) ->
    true.

%% @doc find
-spec find(non_neg_integer(), point(), point()) -> [point()].
find(Id, Start, End) ->
    State = #state{
        open_trees = gb_trees:empty(),
        close_sets = gb_sets:new(),
        parents_trees = gb_trees:empty(),
        dst = End
    },
    StartPoint = #coordinate{id = Id, point = Start},
    OpenTrees = gb_trees:enter(Start, {0, 0, 0}, State#state.open_trees),
    NewState = State#state{open_trees = OpenTrees},
    find_next_point(StartPoint, NewState).

%%%===================================================================
%%% Internal functions
%%%===================================================================
%% loop find backtrace
find_next_point(#coordinate{point = Point}, #state{dst = Point} = State) ->
    construct_path(Point, State#state.parents_trees, [Point]);
find_next_point(#coordinate{point = Start} = Coordinate, State) ->
    %% open to close
    OpenTrees = gb_trees:delete(Start, State#state.open_trees),
    CloseSets = gb_sets:add(Start, State#state.close_sets),
    NewState = State#state{open_trees = OpenTrees, close_sets = CloseSets},
    %% add around point to open trees
    AroundPoints = find_around_points(?DIRECTIONS, Coordinate, NewState, []),
    NewestState = add_open_trees(AroundPoints, Start, NewState),
    case find_min_f_point(gb_trees:iterator(NewestState#state.open_trees), -1, none) of
        none ->
            [];
        NextPoint ->
            find_next_point(NextPoint, NewestState)
    end.

%% f coefficient
find_min_f_point(Iterator, F, Coordinate) ->
    case gb_trees:next(Iterator) of
        {Point, {NextG, NextH, NextF}, NewIterator} ->
            case NextF < F orelse F =:= -1 of
                true ->
                    NewCoordinate = #coordinate{point = Point, g = NextG, h = NextH, f = NextF},
                    find_min_f_point(NewIterator, NextF, NewCoordinate);
                false ->
                    find_min_f_point(NewIterator, F, Coordinate)
            end;
        none ->
            Coordinate
    end.

%% find around points
find_around_points([], _, _, List) ->
    List;
find_around_points([Direction | T], Parent = #coordinate{point = {X, Y}, id = Id}, State = #state{close_sets = CloseSets}, List) ->
    Point = coordinate(Direction, X, Y),
    case walkable(#coordinate{point = Point, id = Id}) of
        true ->
            case gb_sets:is_element(Point, CloseSets) of
                false ->
                    Coordinate = make_coordinate(Point, Parent, State#state.dst),
                    find_around_points(T, Parent, State, [Coordinate | List]);
                true ->
                    find_around_points(T, Parent, State, List)
            end;
        false ->
            find_around_points(T, Parent, State, List)
    end.

make_coordinate({CurrentX, CurrentY}, #coordinate{g = G, point =  {ParentX, ParentY}, id = Id}, {DstX, DstY}) ->
    case (CurrentX =:= ParentX) orelse (CurrentY =:= ParentY) of
        true ->
            AddG = 10;
        false ->
            AddG = 14
    end,
    CurH = (erlang:abs(CurrentX - DstX) + erlang:abs(CurrentY - DstY)) * 10,
    #coordinate{point = {CurrentX, CurrentY}, id = Id, g = G + AddG, h = CurH, f = G + AddG + CurH}.

%% add open tree
add_open_trees([], _ParentXY, State) ->
    State;
add_open_trees([Point | Tail], ParentPoint, State) ->
    case gb_trees:lookup(Point#coordinate.point, State#state.open_trees) of
        {_XY, {G, _H, _F}} ->
            case Point#coordinate.g < G of
                true ->
                    State1 = do_add_open_trees(Point, ParentPoint, State),
                    add_open_trees(Tail, ParentPoint, State1);
                false ->
                    add_open_trees(Tail, ParentPoint, State)
            end;
        none ->
            State1 = do_add_open_trees(Point, ParentPoint, State),
            add_open_trees(Tail, ParentPoint, State1)
    end.

do_add_open_trees(Coordinate, ParentPoint, State) ->
    #coordinate{point = Point, g = G, h = H, f = F} = Coordinate,
    NewOpenTrees = gb_trees:enter(Point, {G, H, F}, State#state.open_trees),
    NewParentsTrees = gb_trees:enter(Point, ParentPoint, State#state.parents_trees),
    State#state{open_trees = NewOpenTrees, parents_trees = NewParentsTrees}.

%% coordinate neighbors
coordinate(1, X, Y) ->
    {X, Y - 1};
coordinate(2, X, Y) ->
    {X + 1, Y - 1};
coordinate(3, X, Y) ->
    {X + 1, Y};
coordinate(4, X, Y) ->
    {X + 1, Y + 1};
coordinate(5, X, Y) ->
    {X, Y + 1};
coordinate(6, X, Y) ->
    {X - 1, Y + 1};
coordinate(7, X, Y) ->
    {X - 1, Y};
coordinate(8, X, Y) ->
    {X - 1, Y - 1}.

%% construct connect path
construct_path(Point, ParentsTrees, List) ->
    case gb_trees:lookup(Point, ParentsTrees) of
        {value, NewPoint} ->
            construct_path(NewPoint, ParentsTrees, [NewPoint | List]);
        none ->
            List
    end.

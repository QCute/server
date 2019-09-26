%%%-------------------------------------------------------------------
%%% @doc
%%% module ess
%%% ets extended library
%%% ets == Erlang Term Store
%%% ess == Erlang Share Store
%%% @end
%%%-------------------------------------------------------------------
-module(ess).
%% API
-export([page/3]).
-export([map/2, map/3]).
-export([foreach/2, foreach/3]).
%%%===================================================================
%%% API
%%%===================================================================
%% @doc ets page
-spec page(Data :: atom(), Index :: non_neg_integer(), Per :: non_neg_integer()) -> list().
%% @doc ETS
page(Tab, Index, Per) when is_atom(Tab) andalso Index > 0 andalso Per > 0 ->
    EtsLength = ets:info(Tab, size),
    case Index * Per =< EtsLength of
        true ->
            Start = (Index - 1) * Per,
            End = Start + Per - 1,
            [hd(ets:slot(Tab, S)) || S <- lists:seq(Start, End)];
        _ when (Index - 1) * Per =< EtsLength ->
            Length =  EtsLength - (Index - 1) * Per,
            Start = (Index - 1) * Per,
            End = Start + Length - 1,
            [hd(ets:slot(Tab, S)) || S <- lists:seq(Start, End)];
        _ ->
            []
    end;
page(_, _, _) ->
    [].

%% @doc ets each, update element/insert object by callback return verse
-spec map(F :: fun((Element :: term()) -> term()), Tab :: atom()) -> term().
map(F, T) ->
    map(F, T, 0).

-spec map(F :: fun((Element :: term()) -> term()), Tab :: atom(), P :: pos_integer()) -> term().
map(F, T, P) ->
    map_loop(F, T, P, ets:first(T)).

map_loop(_F, _T, _P, '$end_of_table') ->
    ok;
map_loop(F, T, 0, Key) ->
    ets:insert(T, F(ets:lookup(T, Key))),
    map_loop(F, T, 0, ets:next(T, Key));
map_loop(F, T, P, Key) ->
    ets:update_element(T, Key, {P, F(ets:lookup(T, Key))}),
    map_loop(F, T, P, ets:next(T, Key)).

%% @doc ets foreach
-spec foreach(F :: fun((Element :: term()) -> term()), Tab :: atom()) -> ok.
foreach(F, T) ->
    foreach(F, T, 0).

-spec foreach(F :: fun((Element :: term()) -> term()), Tab :: atom(), P :: pos_integer()) -> ok.
foreach(F, T, P) ->
    foreach_loop(F, T, P, ets:first(T)).

foreach_loop(_F, _T, _P, '$end_of_table') ->
    ok;
foreach_loop(F, T, 0, Key) ->
    F(ets:lookup(T, Key)),
    foreach_loop(F, T, 0, ets:next(T, Key));
foreach_loop(F, T, P, Key) ->
    F(ets:lookup_element(T, Key, P)),
    foreach_loop(F, T, P, ets:next(T, Key)).


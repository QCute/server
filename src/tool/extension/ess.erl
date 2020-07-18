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
-export([foreach/2]).
%%%===================================================================
%%% API functions
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

%% @doc ets foreach
-spec foreach(F :: fun((Element :: [tuple()]) -> term()), Tab :: ets:tab()) -> ok.
foreach(F, T) ->
    ets:safe_fixtable(T, true),
    foreach_loop(F, T, ets:first(T)).

foreach_loop(_F, T, '$end_of_table') ->
    ets:safe_fixtable(T, false),
    ok;
foreach_loop(F, T, Key) ->
    F(ets:lookup(T, Key)),
    foreach_loop(F, T, ets:next(T, Key)).

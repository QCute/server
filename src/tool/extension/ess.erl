%%%-------------------------------------------------------------------
%%% @doc
%%% ets extended library
%%% ets == Erlang Term Store
%%% ess == Erlang Share Store
%%% @end
%%%-------------------------------------------------------------------
-module(ess).
%% API
-export([page/3]).
-export([foreach/2]).
-export([walk/2, walk_while/2, walk_if/2, walk_if/3]).
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
foreach(F, Tab) ->
    ets:safe_fixtable(Tab, true),
    foreach_loop(F, Tab, ets:first(Tab)).

foreach_loop(_F, Tab, '$end_of_table') ->
    ets:safe_fixtable(Tab, false),
    ok;
foreach_loop(F, Tab, Key) ->
    F(ets:lookup(Tab, Key)),
    foreach_loop(F, Tab, ets:next(Tab, Key)).

%% @doc ets walk
-spec walk(F :: fun((Element :: term()) -> term()), Tab :: ets:tab()) -> ok.
walk(F, Tab) ->
    ets:safe_fixtable(Tab, true),
    walk_loop(F, Tab, ets:first(Tab)).

walk_loop(_F, Tab, '$end_of_table') ->
    ets:safe_fixtable(Tab, false),
    ok;
walk_loop(F, Tab, Key) ->
    F(Key),
    walk_loop(F, Tab, ets:next(Tab, Key)).

%% @doc ets walk while
-spec walk_while(F :: fun((Element :: term()) -> term()), Tab :: ets:tab()) -> [tuple()] | [].
walk_while(F, Tab) ->
    ets:safe_fixtable(Tab, true),
    walk_while_loop(F, Tab, [], ets:first(Tab)).

walk_while_loop(_F, Tab, List, '$end_of_table') ->
    ets:safe_fixtable(Tab, false),
    List;
walk_while_loop(F, Tab, List, Key) ->
    walk_while_loop(F, Tab, lists:append(F(Key), List), ets:next(Tab, Key)).

%% @doc ets walk if
-spec walk_if(F :: fun((Element :: term()) -> term()), Tab :: ets:tab()) -> [tuple()] | [].
walk_if(F, Tab) ->
    walk_if(F, Tab, []).

%% @doc ets walk if
-spec walk_if(F :: fun((Element :: term()) -> term()), Tab :: ets:tab(), Default :: term()) -> [tuple()] | term().
walk_if(F, Tab, Default) ->
    ets:safe_fixtable(Tab, true),
    walk_if_loop(F, Tab, Default, ets:first(Tab)).

walk_if_loop(_F, Tab, Default, '$end_of_table') ->
    ets:safe_fixtable(Tab, false),
    Default;
walk_if_loop(F, Tab, Default, Key) ->
    case F(Key) of
        [] ->
            walk_if_loop(F, Tab, Default, ets:next(Tab, Key));
        Result ->
            Result
    end.

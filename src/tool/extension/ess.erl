%%%-------------------------------------------------------------------
%%% @doc
%%% ets extended library
%%% ets == Erlang Term Store
%%% ess == Erlang Share Store
%%% @end
%%%-------------------------------------------------------------------
-module(ess).
%% API
-export([lookup/3]).
-export([lookup_element/3, lookup_element/4]).
-export([page/3]).
-export([foreach/2]).
-export([walk/2, collect/2, find_if/2, find_if/3]).
%%%===================================================================
%%% API functions
%%%===================================================================
%% @doc lookup
-spec lookup(Tab :: atom(), Key :: term(), Default :: term()) -> [term()] | term().
lookup(Tab, Key, Default) ->
    case ets:lookup(Tab, Key) of
        [] ->
            Default;
        Object ->
            Object
    end.

%% @doc lookup element
-spec lookup_element(Tab :: atom(), Key :: term(), Pos :: pos_integer()) -> [term()].
lookup_element(Tab, Key, Pos) ->
    lookup_element(Tab, Key, Pos, []).

%% @doc lookup element
-spec lookup_element(Tab :: atom(), Key :: term(), Pos :: pos_integer(), Default :: term()) -> [term()].
lookup_element(Tab, Key, Pos, Default) ->
    case ets:member(Tab, Key) of
        true ->
            %% ets lookup element will exit with bad argument when object not found
            ets:lookup_element(Tab, Key, Pos);
        false ->
            Default
    end.

%% @doc ets page
-spec page(Data :: atom(), Index :: non_neg_integer(), Per :: non_neg_integer()) -> list().
%% @doc ETS
page(Tab, Index, Per) when (is_atom(Tab) orelse is_reference(Tab)) andalso Index > 0 andalso Per > 0 ->
    ets:safe_fixtable(Tab, true),
    Start = (Index - 1) * Per + 1,
    End = Start + Per,
    page_loop(Tab, page_skip_loop(Tab, ets:first(Tab), 1, Start), Start, End, []);
page(_, _, _) ->
    [].

page_skip_loop(_, '$end_of_table', _, _) ->
    '$end_of_table';
page_skip_loop(_, Key, End, End) ->
    Key;
page_skip_loop(Tab, Key, Start, End) ->
    page_skip_loop(Tab, ets:next(Tab, Key), Start + 1, End).

page_loop(Tab, '$end_of_table', _, _, List) ->
    ets:safe_fixtable(Tab, false),
    lists:reverse(List);
page_loop(Tab, _, End, End, List) ->
    ets:safe_fixtable(Tab, false),
    lists:reverse(List);
page_loop(Tab, Key, Start, End, List) ->
    case ets:lookup(Tab, Key) of
        [] ->
            page_loop(Tab, ets:next(Tab, Key), Start, End, List);
        [Object] ->
            page_loop(Tab, ets:next(Tab, Key), Start + 1, End, [Object | List])
    end.

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
-spec walk(F :: fun((Key :: term()) -> term()), Tab :: ets:tab()) -> ok.
walk(F, Tab) ->
    ets:safe_fixtable(Tab, true),
    walk_loop(F, Tab, ets:first(Tab)).

walk_loop(_F, Tab, '$end_of_table') ->
    ets:safe_fixtable(Tab, false),
    ok;
walk_loop(F, Tab, Key) ->
    F(Key),
    walk_loop(F, Tab, ets:next(Tab, Key)).

%% @doc ets collect
-spec collect(F :: fun((Key :: term()) -> term()), Tab :: ets:tab()) -> [tuple()] | [].
collect(F, Tab) ->
    ets:safe_fixtable(Tab, true),
    collect_loop(F, Tab, [], ets:first(Tab)).

collect_loop(_F, Tab, List, '$end_of_table') ->
    ets:safe_fixtable(Tab, false),
    List;
collect_loop(F, Tab, List, Key) ->
    %% append to list/empty list safety
    collect_loop(F, Tab, lists:append(F(Key), List), ets:next(Tab, Key)).

%% @doc ets walk if
-spec find_if(F :: fun((Key :: term()) -> term()), Tab :: ets:tab()) -> [tuple()] | [].
find_if(F, Tab) ->
    find_if(F, Tab, []).

%% @doc ets walk if
-spec find_if(F :: fun((Key :: term()) -> term()), Tab :: ets:tab(), Default :: term()) -> [tuple()] | term().
find_if(F, Tab, Default) ->
    ets:safe_fixtable(Tab, true),
    find_if_loop(F, Tab, Default, ets:first(Tab)).

find_if_loop(_F, Tab, Default, '$end_of_table') ->
    ets:safe_fixtable(Tab, false),
    Default;
find_if_loop(F, Tab, Default, Key) ->
    case F(Key) of
        [] ->
            find_if_loop(F, Tab, Default, ets:next(Tab, Key));
        Result ->
            Result
    end.

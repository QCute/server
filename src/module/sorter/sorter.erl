%%%-------------------------------------------------------------------
%%% @doc
%%% module sorter
%%% @end
%%%-------------------------------------------------------------------
-module(sorter).
-export([new/9]).
-export([update/2]).
-export([first/1, last/1]).
-export([stop/1]).
-include("sorter.hrl").
%%%===================================================================
%%% API
%%%===================================================================
%% @doc new
%% global mode alone process manage, data storage in ets
%% share mode local process manage, data storage in ets
%% local mode local process manage, data storage in sorter list
-spec new(Name :: atom(), Mode :: global | share | local, Type :: replace | add, Limit :: non_neg_integer() | infinity, Key :: non_neg_integer() | undefined, Value :: non_neg_integer() | undefined, Time :: non_neg_integer() | undefined, Rank :: non_neg_integer() | undefined, Data :: list()) -> #sorter{}.
new(Name, global, Type, Limit, Key, Value, Time, Rank, Data) ->
    {ok, Pid} = sorter_server:start(Name, [Name, share, Type, Limit, Key, Value, Time, Rank, Data]),
    #sorter{
        name = Name,
        mode = global,
        pid = Pid
    };
new(Name, local, Type, Limit, Key, Value, Time, Rank, Data) ->
    #sorter{
        name = Name,
        mode = local,
        type = Type,
        limit = Limit,
        list = Data,
        key = Key,
        value = Value,
        time = Time,
        rank = Rank
    };
new(Name, share, Type, Limit, Key, Value, Time, Rank, Data) ->
    catch ets:delete_all_objects(Name),
    catch ets:new(Name, [named_table, {keypos, 1}, {read_concurrency, true}, set]),
    catch ets:insert(Name, {Name, Data}),
    #sorter{
        name = Name,
        mode = local,
        type = Type,
        limit = Limit,
        key = Key,
        value = Value,
        time = Time,
        rank = Rank
    }.

%% @doc update
-spec update(Data :: tuple() | [tuple()], Sorter :: #sorter{}) -> Return :: #sorter{} | ok.
update(Data, Sorter = #sorter{mode = local, list = List}) ->
    NewList = handle_update(Data, List, Sorter),
    Sorter#sorter{list = NewList};
update(Data, #sorter{mode = global, pid = Pid}) when is_pid(Pid) ->
    erlang:send(Pid, {'update', Data}),
    ok;
update(Data, Sorter = #sorter{name = Name, mode = share}) ->
    case catch ets:lookup(Name, Name) of
        [{_, List}] when is_tuple(Data) orelse Data =/= [] ->
            NewList = handle_update(Data, List, Sorter),
            catch ets:insert(Name, {Name, NewList}),
            ok;
        _ ->
            ok
    end.

%% @doc first
-spec first(Sorter :: #sorter{}) -> tuple() | [].
first(#sorter{name = local, list = []}) ->
    [];
first(#sorter{list = [_ | _] = List}) ->
    hd(List);
first(#sorter{name = Name}) ->
    case catch ets:lookup(Name, Name) of
        [{_, List}] ->
            hd(List);
        _ ->
            []
    end.

%% @doc last
-spec last(Sorter :: #sorter{}) -> tuple() | [].
last(#sorter{name = local, list = []}) ->
    [];
last(#sorter{name = local, list = [_ | _] = List}) ->
    hd(lists:reverse(List));
last(#sorter{name = Name}) ->
    case catch ets:lookup(Name, Name) of
        [{_, List}] ->
            hd(lists:reverse(List));
        _ ->
            []
    end.

%% @doc stop
-spec stop(Sorter :: #sorter{}) -> ok.
stop(#sorter{mode = global, pid = Pid}) when is_pid(Pid) ->
    erlang:send(Pid, 'stop'),
    ok;
stop(_) ->
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================
%% update progress
handle_update(Data, List, Sorter = #sorter{type = replace, key = Key}) when is_tuple(Data) ->
    NewList = lists:keystore(element(Key, Data), Key, List, Data),
    update_final(Sorter, NewList);
handle_update(Data, List, Sorter = #sorter{type = add}) when is_tuple(Data) ->
    NewList = update_add([Data], List, Sorter),
    update_final(Sorter, NewList);
handle_update(DataList = [_ | _], List, Sorter = #sorter{type = replace, key = Key}) ->
    %% replace new data into the merge list
    %% Elements from the first list are kept and prioritized.
    NewList = lists:ukeymerge(Key, DataList, List),
    update_final(Sorter, NewList);
handle_update(DataList = [_ | _], List, Sorter = #sorter{type = add}) ->
    %% may be cause performance problem
    NewList = update_add(DataList, List, Sorter),
    update_final(Sorter, NewList).

%% add type, handle list add and merge, low performance op
update_add([], List, _) ->
    List;
update_add([Data | T], List, Sorter = #sorter{key = KeyIndex, value = ValueIndex}) ->
    case lists:keytake(element(KeyIndex, Data), KeyIndex, List) of
        {value, Old, RemainList} ->
            OldValue = element(ValueIndex, Old),
            NewValue = element(ValueIndex, Old),
            NewRank = setelement(ValueIndex, Data, OldValue + NewValue),
            update_add(T, [NewRank | RemainList], Sorter);
        _ ->
            update_add(T, [Data | List], Sorter)
    end.

%% sort, trim and fill index list
update_final(#sorter{limit = infinity} = Sorter, List) ->
    Sort = lists:sort(fun(X, Y) -> compare(X, Y, Sorter) end, List),
    fill_index(Sort, Sorter);
update_final(#sorter{limit = Limit} = Sorter, List) ->
    Sort = lists:sort(fun(X, Y) -> compare(X, Y, Sorter) end, List),
    Sub = lists:sublist(Sort, Limit),
    fill_index(Sub, Sorter).

%% sort data list
compare(X, Y, #sorter{key = KeyIndex, value = ValueIndex, time = TimeIndex}) ->
    %% key
    KeyX = element(KeyIndex, X),
    KeyY = element(KeyIndex, Y),
    %% value
    ValueX = element(ValueIndex, X),
    ValueY = element(ValueIndex, Y),
    %% time
    TimeX = element(TimeIndex, X),
    TimeY = element(TimeIndex, Y),
    %% sort by value desc and time asc and key asc
    ValueX > ValueY orelse (ValueX == ValueY andalso TimeX < TimeY) orelse (ValueX == ValueY andalso TimeX == TimeY andalso KeyX < KeyY).

%% fill data rank position
fill_index(List, Sorter = #sorter{rank = RankIndex}) when is_integer(RankIndex) andalso RankIndex > 0 ->
    fill_index(List, [], 1, Sorter);
fill_index(List, _) ->
    %% do not set rank index
    List.
fill_index([], List, _, _) ->
    lists:reverse(List, []);
fill_index([H | T], List, Index, Sorter = #sorter{rank = RankIndex}) ->
    New = setelement(RankIndex, H, Index),
    fill_index(T, [New | List], Index + 1, Sorter).


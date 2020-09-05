%%%-------------------------------------------------------------------
%%% @doc
%%% sort data by value asc, time desc, key asc
%%% @end
%%%-------------------------------------------------------------------
-module(sorter).
%% API
-export([new/9]).
-export([update/2]).
-export([data/1]).
-export([first/1, last/1]).
-export([drop/1]).
%% Includes
-include("sorter.hrl").
%%%===================================================================
%%% API functions
%%%===================================================================
%% @doc new
%% global mode separate process manage, data storage in ets
%% share mode local process manage, data storage in ets
%% local mode local process manage, data storage in sorter list
-spec new(Name, Mode, Type, Limit, Key, Value, Time, Order, Data) -> Result when
    Name :: atom(),
    Mode :: global | share | local,
    Type :: replace | add,
    Limit :: non_neg_integer() | infinity,
    Key :: non_neg_integer() | undefined,
    Value :: non_neg_integer() | undefined,
    Time :: non_neg_integer() | undefined,
    Order :: non_neg_integer() | undefined,
    Data :: list(),
    Result :: #sorter{}.

new(Name, global, Type, Limit, Key, Value, Time, Order, Data) ->
    {ok, Pid} = sorter_server:start_link(Name, [Name, share, Type, Limit, Key, Value, Time, Order, Data]),
    #sorter{
        name = Name,
        mode = global,
        pid = Pid
    };
new(Name, share, Type, Limit, Key, Value, Time, Order, Data) ->
    ets:new(Name, [named_table, {keypos, 1}, {read_concurrency, true}, set]),
    ets:insert(Name, {Name, Data}),
    #sorter{
        name = Name,
        mode = share,
        type = Type,
        limit = Limit,
        key = Key,
        value = Value,
        time = Time,
        order = Order
    };
new(Name, local, Type, Limit, Key, Value, Time, Order, Data) ->
    #sorter{
        name = Name,
        mode = local,
        type = Type,
        limit = Limit,
        list = Data,
        key = Key,
        value = Value,
        time = Time,
        order = Order
    }.

%% @doc update
-spec update(Data :: tuple() | [tuple()], Sorter :: #sorter{}) -> Return :: #sorter{} | ok.
update([], #sorter{mode = global}) ->
    ok;
update(Data, #sorter{mode = global, pid = Pid}) when is_pid(Pid) ->
    gen_server:cast(Pid, {update, Data}),
    ok;
update([], #sorter{mode = share}) ->
    ok;
update(Data, Sorter = #sorter{name = Name, mode = share}) ->
    case ets:lookup(Name, Name) of
        [{_, List}] when is_tuple(Data) orelse Data =/= [] ->
            NewList = handle_update(Data, List, Sorter),
            ets:insert(Name, {Name, NewList}),
            ok;
        _ ->
            ok
    end;
update([], Sorter = #sorter{mode = local}) ->
    Sorter;
update(Data, Sorter = #sorter{mode = local, list = List}) ->
    NewList = handle_update(Data, List, Sorter),
    Sorter#sorter{list = NewList}.

%% @doc data
-spec data(Sorter :: #sorter{} | atom()) -> list().
data(#sorter{name = local, list = List}) ->
    List;
data(#sorter{name = Name}) ->
    data(Name);
data(Name) when is_atom(Name) ->
    case ets:lookup(Name, Name) of
        [{_, List}] ->
            List;
        _ ->
            []
    end.

%% @doc first
-spec first(Sorter :: #sorter{}) -> tuple() | [].
first(#sorter{name = local, list = []}) ->
    [];
first(#sorter{list = [First | _]}) ->
    First;
first(#sorter{name = Name}) ->
    case ets:lookup(Name, Name) of
        [{_, [First | _]}] ->
            First;
        _ ->
            []
    end.

%% @doc last
-spec last(Sorter :: #sorter{}) -> tuple() | [].
last(#sorter{name = local, list = []}) ->
    [];
last(#sorter{name = local, list = List}) ->
    hd(lists:reverse(List));
last(#sorter{name = Name}) ->
    case ets:lookup(Name, Name) of
        [{_, List}] ->
            hd(lists:reverse(List));
        _ ->
            []
    end.

%% @doc drop sorter data
-spec drop(Sorter :: #sorter{}) -> #sorter{}.
drop(Sorter = #sorter{mode = global, pid = Pid}) when is_pid(Pid) ->
    gen_server:cast(Pid, stop),
    Sorter#sorter{pid = undefined};
drop(Sorter = #sorter{mode = share, name = Name}) ->
    ets:delete(Name),
    Sorter#sorter{name = undefined};
drop(Sorter = #sorter{mode = local}) ->
    Sorter#sorter{list = []};
drop(Sorter) ->
    Sorter.

%%%===================================================================
%%% Internal functions
%%%===================================================================
%% update progress
handle_update(Data, List, Sorter = #sorter{type = replace}) when is_tuple(Data) ->
    NewList = update_replace([Data], List, Sorter),
    update_final(Sorter, NewList);
handle_update(Data, List, Sorter = #sorter{type = add}) when is_tuple(Data) ->
    NewList = update_add([Data], List, Sorter),
    update_final(Sorter, NewList);
handle_update(DataList, List, Sorter = #sorter{type = replace}) when is_list(DataList) ->
    %% replace new data into the merge list
    %% Elements from the first list are kept and prioritized.
    NewList = update_replace(DataList, List, Sorter),
    update_final(Sorter, NewList);
handle_update(DataList, List, Sorter = #sorter{type = add}) when is_list(DataList) ->
    %% may be produce performance problem
    NewList = update_add(DataList, List, Sorter),
    update_final(Sorter, NewList).

%% replace into old list
update_replace([], List, _) ->
    List;
update_replace([Data | T], List, Sorter = #sorter{key = KeyIndex}) ->
    NewList = lists:keystore(element(KeyIndex, Data), KeyIndex, List, Data),
    update_replace(T, NewList, Sorter).

%% add type, handle list add and merge, low performance op
update_add([], List, _) ->
    List;
update_add([Data | T], List, Sorter = #sorter{key = KeyIndex, value = ValueIndex}) ->
    case lists:keytake(element(KeyIndex, Data), KeyIndex, List) of
        {value, Old, RemainList} ->
            OldValue = element(ValueIndex, Old),
            NewValue = element(ValueIndex, Old),
            NewOrder = setelement(ValueIndex, Data, OldValue + NewValue),
            update_add(T, [NewOrder | RemainList], Sorter);
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

%% fill data order position
fill_index(List, Sorter = #sorter{order = OrderIndex}) when is_integer(OrderIndex) andalso OrderIndex > 0 ->
    fill_index(List, [], 1, Sorter);
fill_index(List, _) ->
    %% do not set order index
    List.
fill_index([], List, _, _) ->
    lists:reverse(List, []);
fill_index([H | T], List, Index, Sorter = #sorter{order = OrderIndex}) ->
    New = setelement(OrderIndex, H, Index),
    fill_index(T, [New | List], Index + 1, Sorter).

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

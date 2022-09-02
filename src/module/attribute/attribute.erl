%%%-------------------------------------------------------------------
%%% @doc
%%% attribute
%%% @end
%%%-------------------------------------------------------------------
-module(attribute).
%% API
-export([add/3, remove/2]).
-export([calculate/1]).
-export([recalculate/3]).
-export([calculate_fight_count/1]).
-export([merge/2]).
-export([merge_kv/2, merge_record/2]).
-export([subtract/2]).
-export([subtract_kv/2, subtract_record/2]).
-export_type([attribute/0]).
%% Includes
-include("user.hrl").
-include("attribute.hrl").
%% Types
-type attribute() :: {Key :: non_neg_integer(), Value :: non_neg_integer()}.
%%%===================================================================
%%% API functions
%%%===================================================================
%% @doc add attribute
-spec add(User :: #user{}, Key :: term(), Attribute :: #attribute{} | [#attribute{}] | attribute() | [attribute()]) -> NewUser :: #user{}.
add(User = #user{total_attribute = TotalAttribute, attributes = Attributes}, Key, Attribute) ->
    ThisAttribute = merge(#attribute{}, Attribute),
    NewAttributes = [{Key, ThisAttribute} | Attributes],
    NewTotalAttribute = merge_record(TotalAttribute, ThisAttribute),
    User#user{total_attribute = NewTotalAttribute, attributes = NewAttributes}.

%% @doc remove attribute
-spec remove(User :: #user{}, Key :: term()) -> NewUser :: #user{}.
remove(User = #user{total_attribute = TotalAttribute, attributes = Attributes}, Key) ->
    {value, Attribute, NewAttributes} = lists:keytake(Key, 1, Attributes),
    NewTotalAttribute = subtract_record(TotalAttribute, Attribute),
    User#user{total_attribute = NewTotalAttribute, attributes = NewAttributes}.

%% @doc calculate
-spec calculate(User :: #user{}) -> NewUser :: #user{}.
calculate(User = #user{total_attribute = TotalAttribute}) ->
    User#user{total_attribute = calculate_fight_count(TotalAttribute)}.

%% @doc recalculate all attribute
-spec recalculate(User :: #user{}, Key :: term(), NewAttribute :: #attribute{} | [#attribute{}] | attribute() | [attribute()]) -> NewUser :: #user{}.
recalculate(User = #user{total_attribute = TotalAttribute, attributes = Attributes}, Key, NewAttribute) ->
    case lists:keyfind(Key, 1, Attributes) of
        false ->
            %% no old attribute, new one
            ThisAttribute = merge(#attribute{}, NewAttribute),
            NewTotalAttribute = calculate_fight_count(merge_record(TotalAttribute, ThisAttribute)),
            User#user{total_attribute = NewTotalAttribute, attributes = [{Key, ThisAttribute} | Attributes]};
        {_, OldAttribute} ->
            %% replace old attribute
            ThisAttribute = merge(#attribute{}, NewAttribute),
            NewTotalAttribute = calculate_fight_count(merge_record(subtract_record(TotalAttribute, OldAttribute), ThisAttribute)),
            User#user{total_attribute = NewTotalAttribute, attributes = lists:keyreplace(Key, 1, Attributes, ThisAttribute)}
    end.

%% @doc calculate fight count
-spec calculate_fight_count(Attribute :: #attribute{}) -> NewAttribute :: #attribute{}.
calculate_fight_count(Attribute) ->
    Attribute.

%% @doc merge, single and list value compatible
-spec merge(#attribute{}, #attribute{} | [#attribute{}] | attribute() | [attribute()]) -> #attribute{}.
merge(Attribute, Other = #attribute{}) ->
    merge_record(Attribute, Other);
merge(Attribute, Other = {_, _}) ->
    merge_kv(Attribute, Other);
%% list
merge(Attribute, []) ->
    Attribute;
merge(Attribute, [H | T]) ->
    merge(merge(Attribute, H), T).

%% merge with #attribute record type data
-spec merge_record(X :: #attribute{}, Y :: #attribute{}) -> Z :: #attribute{}.
merge_record(X, Y) ->
    Y#attribute{
        fc = X#attribute.fc + Y#attribute.fc,
        attack = X#attribute.attack + Y#attribute.attack,
        defense = X#attribute.defense + Y#attribute.defense,
        health = X#attribute.health + Y#attribute.health,
        hit = X#attribute.hit + Y#attribute.hit,
        duck = X#attribute.duck + Y#attribute.duck
    }.

%% merge with k,v type data
-spec merge_kv(#attribute{}, {Key :: non_neg_integer(), Value :: non_neg_integer()}) -> #attribute{}.
merge_kv(Attribute = #attribute{fc = Fc}, {1, Value}) ->
    Attribute#attribute{fc = Fc + Value};
merge_kv(Attribute = #attribute{attack = Attack}, {3, Value}) ->
    Attribute#attribute{attack = Attack + Value};
merge_kv(Attribute = #attribute{defense = Defense}, {4, Value}) ->
    Attribute#attribute{defense = Defense + Value};
merge_kv(Attribute = #attribute{health = Health}, {5, Value}) ->
    Attribute#attribute{health = Health + Value};
merge_kv(Attribute = #attribute{hit = Hit}, {6, Value}) ->
    Attribute#attribute{hit = Hit + Value};
merge_kv(Attribute = #attribute{duck = Duck}, {7, Value}) ->
    Attribute#attribute{duck = Duck + Value};
merge_kv(_, Attribute) ->
    Attribute.

%% @doc subtract, single and list value compatible
-spec subtract(#attribute{}, #attribute{} | [#attribute{}] | attribute() | [attribute()]) -> #attribute{}.
subtract(Attribute, Other = #attribute{}) ->
    subtract_record(Attribute, Other);
subtract(Attribute, Other = {_, _}) ->
    subtract_kv(Attribute, Other);
%% list
subtract(Attribute, []) ->
    Attribute;
subtract(Attribute, [H | T]) ->
    subtract(subtract(Attribute, H), T).

%% subtract with k,v type data
-spec subtract_record(X :: #attribute{}, Y :: #attribute{}) -> Z :: #attribute{}.
subtract_record(X, Y) ->
    Y#attribute{
        fc = X#attribute.fc - Y#attribute.fc,
        attack = X#attribute.attack - Y#attribute.attack,
        defense = X#attribute.defense - Y#attribute.defense,
        health = X#attribute.health - Y#attribute.health,
        hit = X#attribute.hit - Y#attribute.hit,
        duck = X#attribute.duck - Y#attribute.duck
    }.

%% subtract with k,v type data
-spec subtract_kv(#attribute{}, {Key :: non_neg_integer(), Value :: non_neg_integer()}) -> #attribute{}.
subtract_kv(Attribute = #attribute{fc = Fc}, {1, Value}) ->
    Attribute#attribute{fc = Fc - Value};
subtract_kv(Attribute = #attribute{attack = Attack}, {3, Value}) ->
    Attribute#attribute{attack = Attack - Value};
subtract_kv(Attribute = #attribute{defense = Defense}, {4, Value}) ->
    Attribute#attribute{defense = Defense - Value};
subtract_kv(Attribute = #attribute{health = Health}, {5, Value}) ->
    Attribute#attribute{health = Health - Value};
subtract_kv(Attribute = #attribute{hit = Hit}, {6, Value}) ->
    Attribute#attribute{hit = Hit - Value};
subtract_kv(Attribute = #attribute{duck = Duck}, {7, Value}) ->
    Attribute#attribute{duck = Duck - Value};
subtract_kv(_, Attribute) ->
    Attribute.


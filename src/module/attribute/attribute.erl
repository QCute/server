%%%-------------------------------------------------------------------
%%% @doc
%%% module attribute
%%% @end
%%%-------------------------------------------------------------------
-module(attribute).
%% API
-export([add/3]).
-export([calculate/1]).
-export([recalculate/3]).
-export([merge/1, merge/2]).
-export([merge_kv/2, merge_record/2]).
-export([subtract/1, subtract/2]).
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
    NewAttributes = [{Key, Attribute} | Attributes],
    NewTotalAttribute = merge_record(TotalAttribute, merge(Attribute)),
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
            NewTotalAttribute = calculate_fight_count(merge_record(TotalAttribute, merge(NewAttribute))),
            User#user{total_attribute = NewTotalAttribute, attributes = [{Key, NewAttribute} | Attributes]};
        {_, OldAttribute} when NewAttribute == [] orelse NewAttribute == #attribute{} ->
            %% empty attribute, remove it
            NewTotalAttribute = calculate_fight_count(subtract_record(TotalAttribute, OldAttribute)),
            User#user{total_attribute = NewTotalAttribute, attributes = lists:keydelete(Key, 1, Attributes)};
        {_, OldAttribute} ->
            %% replace old attribute
            NewTotalAttribute = calculate_fight_count(merge_record(subtract_record(TotalAttribute, OldAttribute), merge(NewAttribute))),
            User#user{total_attribute = NewTotalAttribute, attributes = lists:keyreplace(Key, 1, Attributes, NewAttribute)}
    end.

calculate_fight_count(Attribute) ->
    %% fight count
    Fc = erlang:round(0),
    Attribute#attribute{fc = Fc, health = Fc}.

%% @doc merge
-spec merge(Attribute :: #attribute{} | [#attribute{}] | attribute() | [attribute()]) -> #attribute{}.
merge(Attribute) ->
    merge(Attribute, #attribute{}).

%% @doc merge, single and list value compatible
-spec merge(X :: #attribute{} | [#attribute{}] | attribute() | [attribute()], Y :: #attribute{}) -> #attribute{}.
merge(H = #attribute{}, Attribute) ->
    merge_record(H, Attribute);
merge(H = {_, _}, Attribute) ->
    merge_kv(H, Attribute);
%% list
merge([], Attribute) ->
    Attribute;
merge([H = #attribute{} | T], Attribute) ->
    merge(T, merge_record(H, Attribute));
merge([H = {_, _} | T], Attribute) ->
    merge(T, merge_kv(H, Attribute));
merge([_ | T], Attribute) ->
    merge(T, Attribute).

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
-spec merge_kv({Key :: non_neg_integer(), Value :: non_neg_integer()}, #attribute{}) -> #attribute{}.
merge_kv({1, Value}, Attribute = #attribute{fc = Fc}) ->
    Attribute#attribute{fc = Fc + Value};
merge_kv({3, Value}, Attribute = #attribute{attack = Attack}) ->
    Attribute#attribute{attack = Attack + Value};
merge_kv({4, Value}, Attribute = #attribute{defense = Defense}) ->
    Attribute#attribute{defense = Defense + Value};
merge_kv({5, Value}, Attribute = #attribute{health = Health}) ->
    Attribute#attribute{health = Health + Value};
merge_kv({6, Value}, Attribute = #attribute{hit = Hit}) ->
    Attribute#attribute{hit = Hit + Value};
merge_kv({7, Value}, Attribute = #attribute{duck = Duck}) ->
    Attribute#attribute{duck = Duck + Value};
merge_kv(_, Attribute) ->
    Attribute.

%% @doc subtract
-spec subtract(Attribute :: #attribute{} | [#attribute{}] | attribute() | [attribute()]) -> #attribute{}.
subtract(Attribute) ->
    subtract(Attribute, #attribute{}).

%% @doc subtract, single and list value compatible
-spec subtract(X :: #attribute{} | [#attribute{}] | attribute() | [attribute()], Y :: #attribute{}) -> #attribute{}.
subtract(H = #attribute{}, Attribute) ->
    subtract_record(H, Attribute);
subtract(H = {_, _}, Attribute) ->
    subtract_kv(H, Attribute);
%% list
subtract([], Attribute) ->
    Attribute;
subtract([H = #attribute{} | T], Attribute) ->
    subtract(T, subtract_record(H, Attribute));
subtract([H = {_, _} | T], Attribute) ->
    subtract(T, subtract_kv(H, Attribute));
subtract([_ | T], Attribute) ->
    subtract(T, Attribute).

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
-spec subtract_kv({Key :: non_neg_integer(), Value :: non_neg_integer()}, #attribute{}) -> #attribute{}.
subtract_kv({1, Value}, Attribute = #attribute{fc = Fc}) ->
    Attribute#attribute{fc = Fc - Value};
subtract_kv({3, Value}, Attribute = #attribute{attack = Attack}) ->
    Attribute#attribute{attack = Attack - Value};
subtract_kv({4, Value}, Attribute = #attribute{defense = Defense}) ->
    Attribute#attribute{defense = Defense - Value};
subtract_kv({5, Value}, Attribute = #attribute{health = Health}) ->
    Attribute#attribute{health = Health - Value};
subtract_kv({6, Value}, Attribute = #attribute{hit = Hit}) ->
    Attribute#attribute{hit = Hit - Value};
subtract_kv({7, Value}, Attribute = #attribute{duck = Duck}) ->
    Attribute#attribute{duck = Duck - Value};
subtract_kv(_, Attribute) ->
    Attribute.


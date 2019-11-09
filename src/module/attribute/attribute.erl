%%%------------------------------------------------------------------
%%% @doc
%%% module attribute
%%% @end
%%%------------------------------------------------------------------
-module(attribute).
%% API
-export([calculate/3]).
-export([merge/1, merge/2]).
-export([merge_kv/2, merge_record/2]).
-export([subtract_kv/2, subtract_record/2]).
-export_type([attribute/0]).
%% Includes
-include("user.hrl").
-include("attribute.hrl").
%% Types
-type attribute() :: {Key :: non_neg_integer(), Value :: non_neg_integer()}.
%%%==================================================================
%%% API functions
%%%==================================================================
%% @doc calculate all attribute
-spec calculate(User :: #user{}, Key :: term(), NewAttribute :: #attribute{}) -> NewUser :: #user{}.
calculate(User = #user{total_attribute = TotalAttribute, attributes = Attributes}, Key, NewAttribute) ->
    case lists:keyfind(Key, 1, Attributes) of
        false ->
            TotalAttribute = calculate_fight_count(merge_record(TotalAttribute, NewAttribute)),
            User#user{total_attribute = TotalAttribute, attributes = [{Key, NewAttribute} | Attributes]};
        {_, OldAttribute} ->
            TotalAttribute = calculate_fight_count(merge_record(subtract_record(TotalAttribute, OldAttribute), NewAttribute)),
            User#user{total_attribute = TotalAttribute, attributes = lists:keyreplace(Key, 1, Attributes, NewAttribute)}
    end.

calculate_fight_count(Attribute) ->
    %% fight count
    Fc = numeric:floor(0),
    Attribute#attribute{fc = Fc, health = Fc}.

%% @doc merge
-spec merge(Attribute :: [#attribute{}] | #attribute{} | [attribute()] | attribute()) -> #attribute{}.
merge(Attribute) ->
    merge(Attribute, #attribute{}).

%% @doc merge, single and list value compatible
-spec merge(X :: [#attribute{}] | #attribute{} | [attribute()] | attribute(), Y :: #attribute{}) -> #attribute{}.
merge(H = #attribute{}, Attribute) ->
    merge_record(H, Attribute);
merge([], Attribute) ->
    Attribute;
merge([H = #attribute{} | T], Attribute) ->
    New = merge_record(H, Attribute),
    merge(T, New);
merge([H = {_, _} | T], Attribute) ->
    New = merge_kv(H, Attribute),
    merge(T, New);
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
-spec merge_kv(attribute(), #attribute{}) -> #attribute{}.
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
-spec subtract_kv(attribute(), #attribute{}) -> #attribute{}.
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


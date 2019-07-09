%%%-------------------------------------------------------------------
%%% @doc
%%% module attribute
%%% @end
%%%-------------------------------------------------------------------
-module(attribute).
%% API
-export([merge/1, merge/2]).
-export_type([attribute/0]).
%% includes
-include("role.hrl").
-include("attribute.hrl").
-type attribute() :: {Key :: non_neg_integer(), Value :: non_neg_integer()}.
%%%===================================================================
%%% API
%%%===================================================================
%% @doc merge
-spec merge(Attribute :: [#attribute{}] | #attribute{} | [attribute()] | attribute()) -> #attribute{}.
merge(Attribute) ->
    merge(Attribute, #attribute{}).

%% @doc merge, single and list value compatible
-spec merge(X :: [#attribute{}] | #attribute{}, Y :: #attribute{}) -> #attribute{}.
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
merge_record(X, Y) ->
    Y#attribute{
        attack = X#attribute.attack + Y#attribute.attack,
        attack_speed = X#attribute.attack_speed + Y#attribute.attack_speed,
        defense = X#attribute.defense + Y#attribute.defense,
        dexterity = X#attribute.dexterity + Y#attribute.dexterity,
        duck = X#attribute.duck + Y#attribute.duck,
        duck_rate = X#attribute.duck_rate + Y#attribute.duck_rate,
        hit = X#attribute.hit + Y#attribute.hit,
        hit_rate = X#attribute.hit_rate + Y#attribute.hit_rate,
        hp = X#attribute.hp + Y#attribute.hp,
        intellect = X#attribute.intellect + Y#attribute.intellect,
        move_speed = X#attribute.move_speed/1000 + Y#attribute.move_speed,
        power = X#attribute.power + Y#attribute.power,
        vitality = X#attribute.vitality + Y#attribute.vitality
    }.

%% merge with k,v type data
merge_kv({1, Value}, Attribute = #attribute{attack = Attack}) ->
    Attribute#attribute{attack = Attack + Value};
merge_kv({2, Value}, Attribute = #attribute{attack_speed = AttackSpeed}) ->
    Attribute#attribute{attack_speed = AttackSpeed + Value};
merge_kv({3, Value}, Attribute = #attribute{defense = Defense}) ->
    Attribute#attribute{defense = Defense + Value};
merge_kv({4, Value}, Attribute = #attribute{dexterity = Dexterity}) ->
    Attribute#attribute{dexterity = Dexterity + Value};
merge_kv({5, Value}, Attribute = #attribute{duck = Duck}) ->
    Attribute#attribute{duck = Duck + Value};
merge_kv({6, Value}, Attribute = #attribute{duck_rate = DuckRate}) ->
    Attribute#attribute{duck_rate = DuckRate + Value};
merge_kv({7, Value}, Attribute = #attribute{hit = Hit}) ->
    Attribute#attribute{hit = Hit + Value};
merge_kv({8, Value}, Attribute = #attribute{hit_rate = HitRate}) ->
    Attribute#attribute{hit_rate = HitRate + Value};
merge_kv({9, Value}, Attribute = #attribute{hp = Hp}) ->
    Attribute#attribute{hp = Hp + Value};
merge_kv({10, Value}, Attribute = #attribute{intellect = Intellect}) ->
    Attribute#attribute{intellect = Intellect + Value};
merge_kv({11, Value}, Attribute = #attribute{move_speed = MoveSpeed}) ->
    Attribute#attribute{move_speed = MoveSpeed/1000 + Value};
merge_kv({12, Value}, Attribute = #attribute{power = Power}) ->
    Attribute#attribute{power = Power + Value};
merge_kv({13, Value}, Attribute = #attribute{vitality = Vitality}) ->
    Attribute#attribute{vitality = Vitality + Value};
merge_kv(_, Attribute) ->
    Attribute.

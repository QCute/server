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
        power = X#attribute.power + Y#attribute.power
    }.

%% merge with k,v type data
merge_kv({1, Value}, Attribute = #attribute{power = Power}) ->
    Attribute#attribute{power = Power + Value};
merge_kv(_, Attribute) ->
    Attribute.

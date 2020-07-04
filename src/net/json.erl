%%%-------------------------------------------------------------------
%%% @doc
%%% module json
%%% json object encoder/decoder
%%% @end
%%%-------------------------------------------------------------------
-module(json).
%% API
-export([decode/1]).
-export([get/2, get/3]).
%%%===================================================================
%%% API functions
%%%===================================================================
%% @doc decode json
-spec decode(Binary :: binary()) -> [tuple() | list()].
decode(Binary) ->
    {<<>>, Object} = value(Binary, []),
    Object.

%% @doc get json value
-spec get(Key :: binary(), Object :: [tuple() | list()]) -> [tuple() | list()].
get(Key, Object) ->
    get(Key, Object, undefined).

%% @doc get json value with default
-spec get(Key :: binary(), Object :: [tuple() | list()], Default :: binary() | undefined) -> [tuple() | list()] | undefined.
get(Key, Object, Default) ->
    case lists:keyfind(Key, 1, Object) of
        {_, Value} ->
            Value;
        false ->
            Default
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================
%% value
value(<<>>, List) ->
    {<<>>, List};
value(<<$[, Rest/binary>>, List) ->
    %% array
    array(trim(Rest), List);
value(<<${, Rest/binary>>, _) ->
    %% object
    object_field(trim(Rest), []);
value(<<$", Rest/binary>>, _) ->
    %% string
    {Size, _} = binary:match(Rest, <<$">>),
    <<String:Size/binary, _:8, NewRest/binary>> = Rest,
    {NewRest, unicode_string(String, 0)};
value(<<"null", Rest/binary>>, _) ->
    %% null object
    {Rest, <<"null">>};
value(<<"true", Rest/binary>>, _) ->
    %% boolean object
    {Rest, <<"true">>};
value(<<"false", Rest/binary>>, _) ->
    %% boolean object
    {Rest, <<"false">>};
value(Binary, _) ->
    %% number object (integer or float)
    number(Binary).

%% number
number(<<$-, Rest/binary>>) ->
    number_integer_part(Rest, -1);
number(Binary) ->
    number_integer_part(Binary, 1).

%% integer
number_integer_part(<<$0, Rest/binary>>, Sign) ->
    number_fraction_part(Rest, Sign, 0);
number_integer_part(<<C, Rest/binary>>, Sign) when $1 =< C andalso C =< $9 ->
    number_integer_part_rest(Rest, C - $0, Sign).

number_integer_part_rest(<<C, Rest/binary>>, Sign, Number) when $0 =< C andalso C =< $9 ->
    number_integer_part_rest(Rest, Sign, Number * 10 + C - $0);
number_integer_part_rest(<<Rest/binary>>, Sign, Number) ->
    number_fraction_part(Rest, Sign, Number).

%% float
number_fraction_part(<<$., Rest/binary>>, Sign, Integer) ->
    number_fraction_part_rest(Rest, Sign, Integer, 0);
number_fraction_part(<<Rest/binary>>, Sign, Integer) ->
    number_power_part(Rest, Sign * Integer, 0).

number_fraction_part_rest(<<C, Rest/binary>>, Sign, Number, DecimalOffset) when $0 =< C andalso C =< $9 ->
    number_fraction_part_rest(Rest, Sign, Number * 10 + C - $0, DecimalOffset + 1);
number_fraction_part_rest(<<Rest/binary>>, Sign, Number, DecimalOffset) when DecimalOffset > 0 ->
    number_power_part(Rest, Sign * Number, DecimalOffset).

%% power
number_power_part(<<$e, $+, Rest/binary>>, Number, DecimalOffset) ->
    number_power_part_rest(Rest, Number, DecimalOffset, 1, 0, true);
number_power_part(<<$E, $+, Rest/binary>>, Number, DecimalOffset) ->
    number_power_part_rest(Rest, Number, DecimalOffset, 1, 0, true);
number_power_part(<<$e, $-, Rest/binary>>, Number, DecimalOffset) ->
    number_power_part_rest(Rest, Number, DecimalOffset, -1, 0, true);
number_power_part(<<$E, $-, Rest/binary>>, Number, DecimalOffset) ->
    number_power_part_rest(Rest, Number, DecimalOffset, -1, 0, true);
number_power_part(<<$e, Rest/binary>>, Number, DecimalOffset) ->
    number_power_part_rest(Rest, Number, DecimalOffset, 1, 0, true);
number_power_part(<<$E, Rest/binary>>, Number, DecimalOffset) ->
    number_power_part_rest(Rest, Number, DecimalOffset, 1, 0, true);
number_power_part(Binary, Number, 0) ->
    {Binary, Number};
number_power_part(Binary, Number, DecimalOffset) ->
    {Binary, Number / math:pow(10, DecimalOffset)}.

number_power_part_rest(<<C, Rest/binary>>, Number, DecimalOffset, PowerSign, Power, _) when $0 =< C andalso C =< $9 ->
    number_power_part_rest(Rest, Number, DecimalOffset, PowerSign, Power * 10 + C - $0, false);
number_power_part_rest(Binary, Number, DecimalOffset, PowerSign, Power, false) ->
    {Binary, Number * math:pow(10, PowerSign * Power - DecimalOffset)}.

%% decode array
array(<<$], Rest/binary>>, List) ->
    {Rest, List};
array(Binary, List) ->
    %% ensure value
    {ValueRest, Value} = value(Binary, List),
    next_value(trim(ValueRest), [Value | List]).

next_value(<<"]", Rest/binary>>, List) ->
    %% array end
    {Rest, List};
next_value(<<",", Rest/binary>>, List) ->
    %% ensure value
    {ValueRest, Value} = value(trim(Rest), List),
    %% next
    next_value(trim(ValueRest), [Value | List]).

%% decode object key/value pair
object_field(<<$}, Rest/binary>>, List) ->
    %% object end
    {Rest, List};
object_field(Binary, List) ->
    %% ensure key
    object_map(Binary, List).

object_map(<<$", Rest/binary>>, List) ->
    %% key
    {Size, _} = binary:match(Rest, <<$">>),
    <<Key:Size/binary, _:8, KeyRest/binary>> = Rest,
    %% value
    <<$:, NewRest/binary>> = trim(KeyRest),
    {ValueRest, Value} = value(trim(NewRest), []),
    %% next
    next_map(trim(ValueRest), [{unicode_string(Key, 0), Value} | List]).

next_map(<<$}, Rest/binary>>, List) ->
    %% object end
    {Rest, List};
next_map(<<$,, Rest/binary>>, List) ->
    %% next
    object_map(trim(Rest), List).

%% trim space
trim(<<$\t, Rest/binary>>) ->
    trim(Rest);
trim(<<$\n, Rest/binary>>) ->
    trim(Rest);
trim(<<$\r, Rest/binary>>) ->
    trim(Rest);
trim(<<$ , Rest/binary>>) ->
    trim(Rest);
trim(Binary) ->
    Binary.

%% convert to unicode binary
unicode_string(String, Start) ->
    case binary:match(String, <<$\\, $u>>, [{scope, {Start, byte_size(String) - Start}}]) of
        {Begin, _} ->
            <<Head:Begin/binary, $\\, $u, Unicode:4/binary, Rest/binary>> = String,
            UnicodeBinary = unicode:characters_to_binary([binary_to_integer(Unicode, 16)]),
            unicode_string(<<Head/binary, UnicodeBinary/binary, Rest/binary>>, Begin + byte_size(UnicodeBinary));
        _ ->
            String
    end.

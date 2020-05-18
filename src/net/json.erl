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
    {Rest, Type, Power} = number(Binary, integer, <<>>),
    Size = byte_size(Binary) - byte_size(Rest),
    <<Number:Size/binary, NumberRest/binary>> = Binary,
    {NumberRest, format(Number, Type, Power)}.

number(Binary = <<$], _/binary>>, Type, Power) ->
    {Binary, Type, Power};
number(Binary = <<$}, _/binary>>, Type, Power) ->
    {Binary, Type, Power};
number(Binary = <<$,, _/binary>>, Type, Power) ->
    {Binary, Type, Power};
number(Binary = <<$\t, _/binary>>, Type, Power) ->
    {Binary, Type, Power};
number(Binary = <<$\n, _/binary>>, Type, Power) ->
    {Binary, Type, Power};
number(Binary = <<$\r, _/binary>>, Type, Power) ->
    {Binary, Type, Power};
number(Binary = <<$ , _/binary>>, Type, Power) ->
    {Binary, Type, Power};
number(<<$., Rest/binary>>, _, Power) ->
    number(Rest, float, Power);
number(<<$e, Rest/binary>>, Type, _) ->
    number(Rest, Type, <<$e>>);
number(<<$E, Rest/binary>>, Type, _) ->
    number(Rest, Type, <<$E>>);
number(<<_:8, Rest/binary>>, Type, Power) ->
    number(Rest, Type, Power).

format(<<$0, _:8, _/binary>>, integer, <<>>) ->
    throw('unexpected token');
format(Binary, integer, <<>>) ->
    binary_to_integer(Binary);
format(Binary, float, <<>>) ->
    binary_to_float(Binary);
format(Binary, integer, Power) ->
    [Head, Tail] = binary:split(Binary, Power),
    binary_to_float(<<Head/binary, ".0", Power/binary, Tail/binary>>);
format(Binary, float, _) ->
    binary_to_float(Binary).

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

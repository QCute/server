%%%-------------------------------------------------------------------
%%% @doc
%%% json object encoder/decoder
%%% @end
%%%-------------------------------------------------------------------
-module(json).
%% API
-export([encode/1]).
-export([decode/1, decode/2]).
-export([set/3, get/2, get/3]).
%%%===================================================================
%%% API functions
%%%===================================================================
%% @doc encode json
-spec encode(Data :: term()) -> binary().
encode(Data) ->
    value(Data).

%% @doc decode json
-spec decode(Binary :: binary()) -> undefined | number() | boolean() | map() | list() | binary().
decode(Binary) when is_binary(Binary) ->
    value(Binary, Binary, 0, [terminate]).

%% @doc decode json with default
-spec decode(Binary :: binary(), Default :: term()) -> undefined | number() | boolean() | map() | list() | binary().
decode(<<>>, Default) ->
    Default;
decode(Binary, _) ->
    decode(Binary).

%% @doc set json value
-spec set(Key :: binary(), Object :: map(), Value :: term()) -> map().
set(Key, Object, Value) ->
    maps:put(Key, Object, Value).

%% @doc get json value
-spec get(Key :: binary(), Object :: map()) -> undefined | number() | boolean() | map() | list() | binary().
get(Key, Object) ->
    get(Key, Object, undefined).

%% @doc get json value with default
-spec get(Key :: binary(), Object :: map(), Default :: term()) -> undefined | number() | boolean() | map() | list() | binary().
get(Key, Object, Default) ->
    maps:get(Key, Object, Default).

%%%===================================================================
%%% Encode Part
%%%===================================================================

%% value
value(Value) when is_binary(Value) ->
    string(Value, <<$">>);
value(Value) when is_atom(Value) ->
    atom(Value);
value(Value) when is_integer(Value) ->
    erlang:integer_to_binary(Value);
value(Value) when is_float(Value) ->
    erlang:list_to_binary(io_lib_format:fwrite_g(Value));
value(Value) when is_list(Value) ->
    list(Value);
value(Value) when is_map(Value) ->
    map(maps:to_list(Value)).

%% atom
atom(undefined) ->
    <<"null">>;
atom(true) ->
    <<"true">>;
atom(false) ->
    <<"false">>;
atom(Other) ->
    string(erlang:atom_to_binary(Other), <<$">>).

%% list
list([]) ->
    <<"[]">>;
list([Head | Tail]) ->
    list_loop(Tail, <<$[, (value(Head))/binary>>).

list_loop([], Acc) ->
    <<Acc/binary, $]>>;
list_loop([Head | Tail], Acc) ->
    list_loop(Tail, <<Acc/binary, $,, (value(Head))/binary>>).

%% map
map([]) ->
    <<"{}">>;
map([{Key, Value} | Tail]) ->
    map_loop(Tail, <<${, (key(Key))/binary, $:, (value(Value))/binary>>).

map_loop([], Acc) ->
    <<Acc/binary, $}>>;
map_loop([{Key, Value} | Tail], Acc) ->
    map_loop(Tail, <<Acc/binary, $,, (key(Key))/binary, $:, (value(Value))/binary>>).

%% key
key(Key) when is_binary(Key) ->
    string(Key, <<$">>);
key(Key) when is_atom(Key) ->
    string(erlang:atom_to_binary(Key), <<$">>).

%% TODO UTF-8 convert
%% string
string(<<>>, Acc) ->
    <<Acc/binary, $">>;
string(<<$\b, Rest/binary>>, Acc) ->
    string(Rest, <<Acc/binary, $\\, $b>>);
string(<<$\t, Rest/binary>>, Acc) ->
    string(Rest, <<Acc/binary, $\\, $t>>);
string(<<$\n, Rest/binary>>, Acc) ->
    string(Rest, <<Acc/binary, $\\, $n>>);
string(<<$\f, Rest/binary>>, Acc) ->
    string(Rest, <<Acc/binary, $\\, $f>>);
string(<<$\r, Rest/binary>>, Acc) ->
    string(Rest, <<Acc/binary, $\\, $r>>);
string(<<$", Rest/binary>>, Acc) ->
    string(Rest, <<Acc/binary, $\\, $">>);
string(<<$\\, Rest/binary>>, Acc) ->
    string(Rest, <<Acc/binary, $\\, $\\>>);
string(<<C:8, Rest/binary>>, Acc) ->
    string(Rest, <<Acc/binary, C:8>>).

%%%===================================================================
%%% Decode Part
%%%===================================================================

%% whitespace
value(<<$\t, Rest/binary>>, Original, Skip, Stack) ->
    value(Rest, Original, Skip + 1, Stack);
value(<<$\n, Rest/binary>>, Original, Skip, Stack) ->
    value(Rest, Original, Skip + 1, Stack);
value(<<$\r, Rest/binary>>, Original, Skip, Stack) ->
    value(Rest, Original, Skip + 1, Stack);
value(<<$\s, Rest/binary>>, Original, Skip, Stack) ->
    value(Rest, Original, Skip + 1, Stack);
%% number minus
value(<<$-, Rest/binary>>, Original, Skip, Stack) ->
    number_minus(Rest, Original, Skip, Stack);
%% zero or float number
value(<<$0, Rest/binary>>, Original, Skip, Stack) ->
    number_zero(Rest, Original, Skip, Stack, 1);
%% integer
value(<<$1, Rest/binary>>, Original, Skip, Stack) ->
    number(Rest, Original, Skip, Stack, 1);
value(<<$2, Rest/binary>>, Original, Skip, Stack) ->
    number(Rest, Original, Skip, Stack, 1);
value(<<$3, Rest/binary>>, Original, Skip, Stack) ->
    number(Rest, Original, Skip, Stack, 1);
value(<<$4, Rest/binary>>, Original, Skip, Stack) ->
    number(Rest, Original, Skip, Stack, 1);
value(<<$5, Rest/binary>>, Original, Skip, Stack) ->
    number(Rest, Original, Skip, Stack, 1);
value(<<$6, Rest/binary>>, Original, Skip, Stack) ->
    number(Rest, Original, Skip, Stack, 1);
value(<<$7, Rest/binary>>, Original, Skip, Stack) ->
    number(Rest, Original, Skip, Stack, 1);
value(<<$8, Rest/binary>>, Original, Skip, Stack) ->
    number(Rest, Original, Skip, Stack, 1);
value(<<$9, Rest/binary>>, Original, Skip, Stack) ->
    number(Rest, Original, Skip, Stack, 1);
%% string
value(<<$", Rest/binary>>, Original, Skip, Stack) ->
    string(Rest, Original, Skip + 1, Stack, 0);
%% array
value(<<$[, Rest/binary>>, Original, Skip, Stack) ->
    value(Rest, Original, Skip + 1, [array, [] | Stack]);
value(<<$], Rest/binary>>, Original, Skip, [array, [] | Stack]) ->
    continue(Rest, Original, Skip + 1, Stack, []);
%% object
value(<<${, Rest/binary>>, Original, Skip, Stack) ->
    key(Rest, Original, Skip + 1, [[] | Stack]);
%% boolean
value(<<"true", Rest/binary>>, Original, Skip, Stack) ->
    continue(Rest, Original, Skip + 4, Stack, true);
value(<<"false", Rest/binary>>, Original, Skip, Stack) ->
    continue(Rest, Original, Skip + 5, Stack, false);
%% null
value(<<"null", Rest/binary>>, Original, Skip, Stack) ->
    continue(Rest, Original, Skip + 4, Stack, undefined).

%% number minus
number_minus(<<$0, Rest/binary>>, Original, Skip, Stack) ->
    number_zero(Rest, Original, Skip, Stack, 2);
number_minus(<<$1, Rest/binary>>, Original, Skip, Stack) ->
    number(Rest, Original, Skip, Stack, 2);
number_minus(<<$2, Rest/binary>>, Original, Skip, Stack) ->
    number(Rest, Original, Skip, Stack, 2);
number_minus(<<$3, Rest/binary>>, Original, Skip, Stack) ->
    number(Rest, Original, Skip, Stack, 2);
number_minus(<<$4, Rest/binary>>, Original, Skip, Stack) ->
    number(Rest, Original, Skip, Stack, 2);
number_minus(<<$5, Rest/binary>>, Original, Skip, Stack) ->
    number(Rest, Original, Skip, Stack, 2);
number_minus(<<$6, Rest/binary>>, Original, Skip, Stack) ->
    number(Rest, Original, Skip, Stack, 2);
number_minus(<<$7, Rest/binary>>, Original, Skip, Stack) ->
    number(Rest, Original, Skip, Stack, 2);
number_minus(<<$8, Rest/binary>>, Original, Skip, Stack) ->
    number(Rest, Original, Skip, Stack, 2);
number_minus(<<$9, Rest/binary>>, Original, Skip, Stack) ->
    number(Rest, Original, Skip, Stack, 2).

%% float
number_zero(<<$., Rest/binary>>, Original, Skip, Stack, Length) ->
    number_fraction(Rest, Original, Skip, Stack, Length + 1);
%% float with exp
number_zero(<<$e, Rest/binary>>, Original, Skip, Stack, Length) ->
    number_exp_copy(Rest, Original, Skip + Length + 1, Stack, <<"0">>);
number_zero(<<$E, Rest/binary>>, Original, Skip, Stack, Length) ->
    number_exp_copy(Rest, Original, Skip + Length + 1, Stack, <<"0">>);
%% continue
number_zero(<<Rest/binary>>, Original, Skip, Stack, Length) ->
    continue(Rest, Original, Skip + Length, Stack, 0).

%% number
number(<<$0, Rest/binary>>, Original, Skip, Stack, Length) ->
    number(Rest, Original, Skip, Stack, Length + 1);
number(<<$1, Rest/binary>>, Original, Skip, Stack, Length) ->
    number(Rest, Original, Skip, Stack, Length + 1);
number(<<$2, Rest/binary>>, Original, Skip, Stack, Length) ->
    number(Rest, Original, Skip, Stack, Length + 1);
number(<<$3, Rest/binary>>, Original, Skip, Stack, Length) ->
    number(Rest, Original, Skip, Stack, Length + 1);
number(<<$4, Rest/binary>>, Original, Skip, Stack, Length) ->
    number(Rest, Original, Skip, Stack, Length + 1);
number(<<$5, Rest/binary>>, Original, Skip, Stack, Length) ->
    number(Rest, Original, Skip, Stack, Length + 1);
number(<<$6, Rest/binary>>, Original, Skip, Stack, Length) ->
    number(Rest, Original, Skip, Stack, Length + 1);
number(<<$7, Rest/binary>>, Original, Skip, Stack, Length) ->
    number(Rest, Original, Skip, Stack, Length + 1);
number(<<$8, Rest/binary>>, Original, Skip, Stack, Length) ->
    number(Rest, Original, Skip, Stack, Length + 1);
number(<<$9, Rest/binary>>, Original, Skip, Stack, Length) ->
    number(Rest, Original, Skip, Stack, Length + 1);
%% number with exp
number(<<$., Rest/binary>>, Original, Skip, Stack, Length) ->
    number_fraction(Rest, Original, Skip, Stack, Length + 1);
number(<<$e, Rest/binary>>, Original, Skip, Stack, Length) ->
    <<_:Skip/binary, Prefix:Length/binary, _/binary>> = Original,
    number_exp_copy(Rest, Original, Skip + Length + 1, Stack, Prefix);
number(<<$E, Rest/binary>>, Original, Skip, Stack, Length) ->
    <<_:Skip/binary, Prefix:Length/binary, _/binary>> = Original,
    number_exp_copy(Rest, Original, Skip + Length + 1, Stack, Prefix);
%% continue
number(<<Rest/binary>>, Original, Skip, Stack, Length) ->
    <<_:Skip/binary, Part:Length/binary, _/binary>> = Original,
    continue(Rest, Original, Skip + Length, Stack, erlang:binary_to_integer(Part)).

%% number fraction
number_fraction(<<$0, Rest/binary>>, Original, Skip, Stack, Length) ->
    number_fraction_continue(Rest, Original, Skip, Stack, Length + 1);
number_fraction(<<$1, Rest/binary>>, Original, Skip, Stack, Length) ->
    number_fraction_continue(Rest, Original, Skip, Stack, Length + 1);
number_fraction(<<$2, Rest/binary>>, Original, Skip, Stack, Length) ->
    number_fraction_continue(Rest, Original, Skip, Stack, Length + 1);
number_fraction(<<$3, Rest/binary>>, Original, Skip, Stack, Length) ->
    number_fraction_continue(Rest, Original, Skip, Stack, Length + 1);
number_fraction(<<$4, Rest/binary>>, Original, Skip, Stack, Length) ->
    number_fraction_continue(Rest, Original, Skip, Stack, Length + 1);
number_fraction(<<$5, Rest/binary>>, Original, Skip, Stack, Length) ->
    number_fraction_continue(Rest, Original, Skip, Stack, Length + 1);
number_fraction(<<$6, Rest/binary>>, Original, Skip, Stack, Length) ->
    number_fraction_continue(Rest, Original, Skip, Stack, Length + 1);
number_fraction(<<$7, Rest/binary>>, Original, Skip, Stack, Length) ->
    number_fraction_continue(Rest, Original, Skip, Stack, Length + 1);
number_fraction(<<$8, Rest/binary>>, Original, Skip, Stack, Length) ->
    number_fraction_continue(Rest, Original, Skip, Stack, Length + 1);
number_fraction(<<$9, Rest/binary>>, Original, Skip, Stack, Length) ->
    number_fraction_continue(Rest, Original, Skip, Stack, Length + 1).

number_fraction_continue(<<$0, Rest/binary>>, Original, Skip, Stack, Length) ->
    number_fraction_continue(Rest, Original, Skip, Stack, Length + 1);
number_fraction_continue(<<$1, Rest/binary>>, Original, Skip, Stack, Length) ->
    number_fraction_continue(Rest, Original, Skip, Stack, Length + 1);
number_fraction_continue(<<$2, Rest/binary>>, Original, Skip, Stack, Length) ->
    number_fraction_continue(Rest, Original, Skip, Stack, Length + 1);
number_fraction_continue(<<$3, Rest/binary>>, Original, Skip, Stack, Length) ->
    number_fraction_continue(Rest, Original, Skip, Stack, Length + 1);
number_fraction_continue(<<$4, Rest/binary>>, Original, Skip, Stack, Length) ->
    number_fraction_continue(Rest, Original, Skip, Stack, Length + 1);
number_fraction_continue(<<$5, Rest/binary>>, Original, Skip, Stack, Length) ->
    number_fraction_continue(Rest, Original, Skip, Stack, Length + 1);
number_fraction_continue(<<$6, Rest/binary>>, Original, Skip, Stack, Length) ->
    number_fraction_continue(Rest, Original, Skip, Stack, Length + 1);
number_fraction_continue(<<$7, Rest/binary>>, Original, Skip, Stack, Length) ->
    number_fraction_continue(Rest, Original, Skip, Stack, Length + 1);
number_fraction_continue(<<$8, Rest/binary>>, Original, Skip, Stack, Length) ->
    number_fraction_continue(Rest, Original, Skip, Stack, Length + 1);
number_fraction_continue(<<$9, Rest/binary>>, Original, Skip, Stack, Length) ->
    number_fraction_continue(Rest, Original, Skip, Stack, Length + 1);
%% number fraction with exp
number_fraction_continue(<<$e, Rest/binary>>, Original, Skip, Stack, Length) ->
    number_exp(Rest, Original, Skip, Stack, Length + 1);
number_fraction_continue(<<$E, Rest/binary>>, Original, Skip, Stack, Length) ->
    number_exp(Rest, Original, Skip, Stack, Length + 1);
%% continue
number_fraction_continue(<<Rest/binary>>, Original, Skip, Stack, Length) ->
    <<_:Skip/binary, Part:Length/binary, _/binary>> = Original,
    continue(Rest, Original, Skip + Length, Stack, erlang:binary_to_float(Part)).

%% number exp 
number_exp(<<$0, Rest/binary>>, Original, Skip, Stack, Length) ->
    number_exp_continue(Rest, Original, Skip, Stack, Length + 1);
number_exp(<<$1, Rest/binary>>, Original, Skip, Stack, Length) ->
    number_exp_continue(Rest, Original, Skip, Stack, Length + 1);
number_exp(<<$2, Rest/binary>>, Original, Skip, Stack, Length) ->
    number_exp_continue(Rest, Original, Skip, Stack, Length + 1);
number_exp(<<$3, Rest/binary>>, Original, Skip, Stack, Length) ->
    number_exp_continue(Rest, Original, Skip, Stack, Length + 1);
number_exp(<<$4, Rest/binary>>, Original, Skip, Stack, Length) ->
    number_exp_continue(Rest, Original, Skip, Stack, Length + 1);
number_exp(<<$5, Rest/binary>>, Original, Skip, Stack, Length) ->
    number_exp_continue(Rest, Original, Skip, Stack, Length + 1);
number_exp(<<$6, Rest/binary>>, Original, Skip, Stack, Length) ->
    number_exp_continue(Rest, Original, Skip, Stack, Length + 1);
number_exp(<<$7, Rest/binary>>, Original, Skip, Stack, Length) ->
    number_exp_continue(Rest, Original, Skip, Stack, Length + 1);
number_exp(<<$8, Rest/binary>>, Original, Skip, Stack, Length) ->
    number_exp_continue(Rest, Original, Skip, Stack, Length + 1);
number_exp(<<$9, Rest/binary>>, Original, Skip, Stack, Length) ->
    number_exp_continue(Rest, Original, Skip, Stack, Length + 1);
%% number exp with sign
number_exp(<<$+, Rest/binary>>, Original, Skip, Stack, Length) ->
    number_exp_sign(Rest, Original, Skip, Stack, Length + 1);
number_exp(<<$-, Rest/binary>>, Original, Skip, Stack, Length) ->
    number_exp_sign(Rest, Original, Skip, Stack, Length + 1).

%% number exp sign
number_exp_sign(<<$0, Rest/binary>>, Original, Skip, Stack, Length) ->
    number_exp_continue(Rest, Original, Skip, Stack, Length + 1);
number_exp_sign(<<$1, Rest/binary>>, Original, Skip, Stack, Length) ->
    number_exp_continue(Rest, Original, Skip, Stack, Length + 1);
number_exp_sign(<<$2, Rest/binary>>, Original, Skip, Stack, Length) ->
    number_exp_continue(Rest, Original, Skip, Stack, Length + 1);
number_exp_sign(<<$3, Rest/binary>>, Original, Skip, Stack, Length) ->
    number_exp_continue(Rest, Original, Skip, Stack, Length + 1);
number_exp_sign(<<$4, Rest/binary>>, Original, Skip, Stack, Length) ->
    number_exp_continue(Rest, Original, Skip, Stack, Length + 1);
number_exp_sign(<<$5, Rest/binary>>, Original, Skip, Stack, Length) ->
    number_exp_continue(Rest, Original, Skip, Stack, Length + 1);
number_exp_sign(<<$6, Rest/binary>>, Original, Skip, Stack, Length) ->
    number_exp_continue(Rest, Original, Skip, Stack, Length + 1);
number_exp_sign(<<$7, Rest/binary>>, Original, Skip, Stack, Length) ->
    number_exp_continue(Rest, Original, Skip, Stack, Length + 1);
number_exp_sign(<<$8, Rest/binary>>, Original, Skip, Stack, Length) ->
    number_exp_continue(Rest, Original, Skip, Stack, Length + 1);
number_exp_sign(<<$9, Rest/binary>>, Original, Skip, Stack, Length) ->
    number_exp_continue(Rest, Original, Skip, Stack, Length + 1).

%% number exp continue
number_exp_continue(<<$0, Rest/binary>>, Original, Skip, Stack, Length) ->
    number_exp_continue(Rest, Original, Skip, Stack, Length + 1);
number_exp_continue(<<$1, Rest/binary>>, Original, Skip, Stack, Length) ->
    number_exp_continue(Rest, Original, Skip, Stack, Length + 1);
number_exp_continue(<<$2, Rest/binary>>, Original, Skip, Stack, Length) ->
    number_exp_continue(Rest, Original, Skip, Stack, Length + 1);
number_exp_continue(<<$3, Rest/binary>>, Original, Skip, Stack, Length) ->
    number_exp_continue(Rest, Original, Skip, Stack, Length + 1);
number_exp_continue(<<$4, Rest/binary>>, Original, Skip, Stack, Length) ->
    number_exp_continue(Rest, Original, Skip, Stack, Length + 1);
number_exp_continue(<<$5, Rest/binary>>, Original, Skip, Stack, Length) ->
    number_exp_continue(Rest, Original, Skip, Stack, Length + 1);
number_exp_continue(<<$6, Rest/binary>>, Original, Skip, Stack, Length) ->
    number_exp_continue(Rest, Original, Skip, Stack, Length + 1);
number_exp_continue(<<$7, Rest/binary>>, Original, Skip, Stack, Length) ->
    number_exp_continue(Rest, Original, Skip, Stack, Length + 1);
number_exp_continue(<<$8, Rest/binary>>, Original, Skip, Stack, Length) ->
    number_exp_continue(Rest, Original, Skip, Stack, Length + 1);
number_exp_continue(<<$9, Rest/binary>>, Original, Skip, Stack, Length) ->
    number_exp_continue(Rest, Original, Skip, Stack, Length + 1);
%% continue
number_exp_continue(<<Rest/binary>>, Original, Skip, Stack, Length) ->
    <<_:Skip/binary, Part:Length/binary, _/binary>> = Original,
    continue(Rest, Original, Skip + Length, Stack, erlang:binary_to_float(Part)).

%% number exp copy
number_exp_copy(<<$0, Rest/binary>>, Original, Skip, Stack, Prefix) ->
    number_exp_copy_continue(Rest, Original, Skip, Stack, 1, Prefix);
number_exp_copy(<<$1, Rest/binary>>, Original, Skip, Stack, Prefix) ->
    number_exp_copy_continue(Rest, Original, Skip, Stack, 1, Prefix);
number_exp_copy(<<$2, Rest/binary>>, Original, Skip, Stack, Prefix) ->
    number_exp_copy_continue(Rest, Original, Skip, Stack, 1, Prefix);
number_exp_copy(<<$3, Rest/binary>>, Original, Skip, Stack, Prefix) ->
    number_exp_copy_continue(Rest, Original, Skip, Stack, 1, Prefix);
number_exp_copy(<<$4, Rest/binary>>, Original, Skip, Stack, Prefix) ->
    number_exp_copy_continue(Rest, Original, Skip, Stack, 1, Prefix);
number_exp_copy(<<$5, Rest/binary>>, Original, Skip, Stack, Prefix) ->
    number_exp_copy_continue(Rest, Original, Skip, Stack, 1, Prefix);
number_exp_copy(<<$6, Rest/binary>>, Original, Skip, Stack, Prefix) ->
    number_exp_copy_continue(Rest, Original, Skip, Stack, 1, Prefix);
number_exp_copy(<<$7, Rest/binary>>, Original, Skip, Stack, Prefix) ->
    number_exp_copy_continue(Rest, Original, Skip, Stack, 1, Prefix);
number_exp_copy(<<$8, Rest/binary>>, Original, Skip, Stack, Prefix) ->
    number_exp_copy_continue(Rest, Original, Skip, Stack, 1, Prefix);
number_exp_copy(<<$9, Rest/binary>>, Original, Skip, Stack, Prefix) ->
    number_exp_copy_continue(Rest, Original, Skip, Stack, 1, Prefix);
%% number exp copy with sign
number_exp_copy(<<$+, Rest/binary>>, Original, Skip, Stack, Prefix) ->
    number_exp_copy_sign(Rest, Original, Skip, Stack, 1, Prefix);
number_exp_copy(<<$-, Rest/binary>>, Original, Skip, Stack, Prefix) ->
    number_exp_copy_sign(Rest, Original, Skip, Stack, 1, Prefix).

number_exp_copy_sign(<<$0, Rest/binary>>, Original, Skip, Stack, Length, Prefix) ->
    number_exp_copy_continue(Rest, Original, Skip, Stack, Length + 1, Prefix);
number_exp_copy_sign(<<$1, Rest/binary>>, Original, Skip, Stack, Length, Prefix) ->
    number_exp_copy_continue(Rest, Original, Skip, Stack, Length + 1, Prefix);
number_exp_copy_sign(<<$2, Rest/binary>>, Original, Skip, Stack, Length, Prefix) ->
    number_exp_copy_continue(Rest, Original, Skip, Stack, Length + 1, Prefix);
number_exp_copy_sign(<<$3, Rest/binary>>, Original, Skip, Stack, Length, Prefix) ->
    number_exp_copy_continue(Rest, Original, Skip, Stack, Length + 1, Prefix);
number_exp_copy_sign(<<$4, Rest/binary>>, Original, Skip, Stack, Length, Prefix) ->
    number_exp_copy_continue(Rest, Original, Skip, Stack, Length + 1, Prefix);
number_exp_copy_sign(<<$5, Rest/binary>>, Original, Skip, Stack, Length, Prefix) ->
    number_exp_copy_continue(Rest, Original, Skip, Stack, Length + 1, Prefix);
number_exp_copy_sign(<<$6, Rest/binary>>, Original, Skip, Stack, Length, Prefix) ->
    number_exp_copy_continue(Rest, Original, Skip, Stack, Length + 1, Prefix);
number_exp_copy_sign(<<$7, Rest/binary>>, Original, Skip, Stack, Length, Prefix) ->
    number_exp_copy_continue(Rest, Original, Skip, Stack, Length + 1, Prefix);
number_exp_copy_sign(<<$8, Rest/binary>>, Original, Skip, Stack, Length, Prefix) ->
    number_exp_copy_continue(Rest, Original, Skip, Stack, Length + 1, Prefix);
number_exp_copy_sign(<<$9, Rest/binary>>, Original, Skip, Stack, Length, Prefix) ->
    number_exp_copy_continue(Rest, Original, Skip, Stack, Length + 1, Prefix).

number_exp_copy_continue(<<$0, Rest/binary>>, Original, Skip, Stack, Length, Prefix) ->
    number_exp_copy_continue(Rest, Original, Skip, Stack, Length + 1, Prefix);
number_exp_copy_continue(<<$1, Rest/binary>>, Original, Skip, Stack, Length, Prefix) ->
    number_exp_copy_continue(Rest, Original, Skip, Stack, Length + 1, Prefix);
number_exp_copy_continue(<<$2, Rest/binary>>, Original, Skip, Stack, Length, Prefix) ->
    number_exp_copy_continue(Rest, Original, Skip, Stack, Length + 1, Prefix);
number_exp_copy_continue(<<$3, Rest/binary>>, Original, Skip, Stack, Length, Prefix) ->
    number_exp_copy_continue(Rest, Original, Skip, Stack, Length + 1, Prefix);
number_exp_copy_continue(<<$4, Rest/binary>>, Original, Skip, Stack, Length, Prefix) ->
    number_exp_copy_continue(Rest, Original, Skip, Stack, Length + 1, Prefix);
number_exp_copy_continue(<<$5, Rest/binary>>, Original, Skip, Stack, Length, Prefix) ->
    number_exp_copy_continue(Rest, Original, Skip, Stack, Length + 1, Prefix);
number_exp_copy_continue(<<$6, Rest/binary>>, Original, Skip, Stack, Length, Prefix) ->
    number_exp_copy_continue(Rest, Original, Skip, Stack, Length + 1, Prefix);
number_exp_copy_continue(<<$7, Rest/binary>>, Original, Skip, Stack, Length, Prefix) ->
    number_exp_copy_continue(Rest, Original, Skip, Stack, Length + 1, Prefix);
number_exp_copy_continue(<<$8, Rest/binary>>, Original, Skip, Stack, Length, Prefix) ->
    number_exp_copy_continue(Rest, Original, Skip, Stack, Length + 1, Prefix);
number_exp_copy_continue(<<$9, Rest/binary>>, Original, Skip, Stack, Length, Prefix) ->
    number_exp_copy_continue(Rest, Original, Skip, Stack, Length + 1, Prefix);
%% continue
number_exp_copy_continue(<<Rest/binary>>, Original, Skip, Stack, Length, Prefix) ->
    <<_:Skip/binary, Part:Length/binary, _/binary>> = Original,
    String = <<Prefix/binary, ".0e", Part/binary>>,
    continue(Rest, Original, Skip + Length, Stack, erlang:binary_to_float(String)).

%% array
array(<<$\t, Rest/binary>>, Original, Skip, Stack, Value) ->
    array(Rest, Original, Skip + 1, Stack, Value);
array(<<$\n, Rest/binary>>, Original, Skip, Stack, Value) ->
    array(Rest, Original, Skip + 1, Stack, Value);
array(<<$\r, Rest/binary>>, Original, Skip, Stack, Value) ->
    array(Rest, Original, Skip + 1, Stack, Value);
array(<<$\s, Rest/binary>>, Original, Skip, Stack, Value) ->
    array(Rest, Original, Skip + 1, Stack, Value);
array(<<$,, Rest/binary>>, Original, Skip, [Acc | Stack], Value) ->
    value(Rest, Original, Skip + 1, [array, [Value | Acc] | Stack]);
array(<<$], Rest/binary>>, Original, Skip, [Acc | Stack], Value) ->
    continue(Rest, Original, Skip + 1, Stack, lists:reverse(Acc, [Value])).

%% object
object(<<$\t, Rest/binary>>, Original, Skip, Stack, Value) ->
    object(Rest, Original, Skip + 1, Stack, Value);
object(<<$\n, Rest/binary>>, Original, Skip, Stack, Value) ->
    object(Rest, Original, Skip + 1, Stack, Value);
object(<<$\r, Rest/binary>>, Original, Skip, Stack, Value) ->
    object(Rest, Original, Skip + 1, Stack, Value);
object(<<$\s, Rest/binary>>, Original, Skip, Stack, Value) ->
    object(Rest, Original, Skip + 1, Stack, Value);
object(<<$,, Rest/binary>>, Original, Skip, [Key, Acc | Stack], Value) ->
    key(Rest, Original, Skip + 1, [[{Key, Value} | Acc] | Stack]);
object(<<$}, Rest/binary>>, Original, Skip, [Key, Acc | Stack], Value) ->
    continue(Rest, Original, Skip + 1, Stack, maps:from_list([{Key, Value} | Acc])).

%% key
key(<<$\t, Rest/binary>>, Original, Skip, Stack) ->
    key(Rest, Original, Skip + 1, Stack);
key(<<$\n, Rest/binary>>, Original, Skip, Stack) ->
    key(Rest, Original, Skip + 1, Stack);
key(<<$\r, Rest/binary>>, Original, Skip, Stack) ->
    key(Rest, Original, Skip + 1, Stack);
key(<<$\s, Rest/binary>>, Original, Skip, Stack) ->
    key(Rest, Original, Skip + 1, Stack);
key(<<$", Rest/binary>>, Original, Skip, Stack) ->
    string(Rest, Original, Skip + 1, [key | Stack], 0);
key(<<$}, Rest/binary>>, Original, Skip, [[] | Stack]) ->
    continue(Rest, Original, Skip + 1, Stack, maps:new()).

%% key - value
key(<<$\t, Rest/binary>>, Original, Skip, Stack, Value) ->
    key(Rest, Original, Skip + 1, Stack, Value);
key(<<$\n, Rest/binary>>, Original, Skip, Stack, Value) ->
    key(Rest, Original, Skip + 1, Stack, Value);
key(<<$\r, Rest/binary>>, Original, Skip, Stack, Value) ->
    key(Rest, Original, Skip + 1, Stack, Value);
key(<<$\s, Rest/binary>>, Original, Skip, Stack, Value) ->
    key(Rest, Original, Skip + 1, Stack, Value);
key(<<$:, Rest/binary>>, Original, Skip, Stack, Value) ->
    value(Rest, Original, Skip + 1, [object, Value | Stack]).

%% string
%% TODO UTF-8 check
string(<<$", Rest/binary>>, Original, Skip, Stack, Length) ->
    <<_:Skip/binary, String:Length/binary, _/binary>> = Original,
    continue(Rest, Original, Skip + Length + 1, Stack, String);
string(<<$\\, Rest/binary>>, Original, Skip, Stack, Length) ->
    <<_:Skip/binary, String:Length/binary, _/binary>> = Original,
    escape(Rest, Original, Skip + Length, Stack, String);
string(<<_, Rest/binary>>, Original, Skip, Stack, Length) ->
    string(Rest, Original, Skip, Stack, Length + 1).

%% TODO UTF-8 check
string_acc(<<$", Rest/binary>>, Original, Skip, Stack, Length, Acc) ->
    <<_:Skip/binary, String:Length/binary, _/binary>> = Original,
    continue(Rest, Original, Skip + Length + 1, Stack, <<Acc/binary, String/binary>>);
string_acc(<<$\\, Rest/binary>>, Original, Skip, Stack, Length, Acc) ->
    <<_:Skip/binary, String:Length/binary, _/binary>> = Original,
    escape(Rest, Original, Skip + Length, Stack, <<Acc/binary, String/binary>>);
string_acc(<<_, Rest/binary>>, Original, Skip, Stack, Length, Acc) ->
    string_acc(Rest, Original, Skip, Stack, Length + 1, Acc).

%% escape string
escape(<<$b, Rest/binary>>, Original, Skip, Stack, Acc) ->
    string_acc(Rest, Original, Skip + 2, Stack, 0, <<Acc/binary, $\b>>);
escape(<<$t, Rest/binary>>, Original, Skip, Stack, Acc) ->
    string_acc(Rest, Original, Skip + 2, Stack, 0, <<Acc/binary, $\t>>);
escape(<<$n, Rest/binary>>, Original, Skip, Stack, Acc) ->
    string_acc(Rest, Original, Skip + 2, Stack, 0, <<Acc/binary, $\n>>);
escape(<<$f, Rest/binary>>, Original, Skip, Stack, Acc) ->
    string_acc(Rest, Original, Skip + 2, Stack, 0, <<Acc/binary, $\f>>);
escape(<<$r, Rest/binary>>, Original, Skip, Stack, Acc) ->
    string_acc(Rest, Original, Skip + 2, Stack, 0, <<Acc/binary, $\r>>);
escape(<<$", Rest/binary>>, Original, Skip, Stack, Acc) ->
    string_acc(Rest, Original, Skip + 2, Stack, 0, <<Acc/binary, $">>);
escape(<<$/, Rest/binary>>, Original, Skip, Stack, Acc) ->
    string_acc(Rest, Original, Skip + 2, Stack, 0, <<Acc/binary, $/>>);
escape(<<$\\, Rest/binary>>, Original, Skip, Stack, Acc) ->
    string_acc(Rest, Original, Skip + 2, Stack, 0, <<Acc/binary, $\\>>);
escape(<<$u, Escape:4/binary, Rest/binary>>, Original, Skip, Stack, Acc) ->
    %% the escape unicode
    unicode_high(Rest, Original, Skip + 2 + 4, Stack, binary_to_integer(Escape, 16), Acc).

%% high part of unicode
unicode_high(<<$\\, $u, Escape:4/binary, Rest/binary>>, Original, Skip, Stack, High, Acc)  when 16#D800 =< High andalso High =< 16#DBFF ->
    %% the surrogate pair
    unicode_low(Rest, Original, Skip + 2 + 4, Stack, High, binary_to_integer(Escape, 16), Acc);
unicode_high(<<Rest/binary>>, Original, Skip, Stack, Unicode, Acc) when 0 < Unicode andalso Unicode < 16#DC00 orelse 16#DFFF < Unicode ->
    %% not the second part of surrogate pair (without first part)
    string_acc(Rest, Original, Skip, Stack, 0, <<Acc/binary, Unicode/utf8>>).

%% low part of unicode
unicode_low(Rest, Original, Skip, Stack, High, Low, Acc) when 16#DC00 =< Low andalso Low =< 16#DFFF ->
    <<Unicode/utf16>> = <<High:16, Low:16>>,
    string_acc(Rest, Original, Skip, Stack, 0, <<Acc/binary, Unicode/utf8>>).

%% continue
continue(<<Rest/binary>>, Original, Skip, [terminate | Stack], Value) ->
    terminate(Rest, Original, Skip, Stack, Value);
continue(<<Rest/binary>>, Original, Skip, [array | Stack], Value) ->
    array(Rest, Original, Skip, Stack, Value);
continue(<<Rest/binary>>, Original, Skip, [key | Stack], Value) ->
    key(Rest, Original, Skip, Stack, Value);
continue(<<Rest/binary>>, Original, Skip, [object | Stack], Value) ->
    object(Rest, Original, Skip, Stack, Value).

%% terminate
terminate(<<$\t, Rest/binary>>, Original, Skip, Stack, Value) ->
    terminate(Rest, Original, Skip + 1, Stack, Value);
terminate(<<$\n, Rest/binary>>, Original, Skip, Stack, Value) ->
    terminate(Rest, Original, Skip + 1, Stack, Value);
terminate(<<$\r, Rest/binary>>, Original, Skip, Stack, Value) ->
    terminate(Rest, Original, Skip + 1, Stack, Value);
terminate(<<$\s, Rest/binary>>, Original, Skip, Stack, Value) ->
    terminate(Rest, Original, Skip + 1, Stack, Value);
terminate(<<_/binary>>, _Original, _Skip, _Stack, Value) ->
    Value.

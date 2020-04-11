%%%-------------------------------------------------------------------
%%% @doc
%%% module protocol(binary read/write) tool
%%%
%%% Read bit field list recommend
%%% has binary list: Binary = <<1:8, 2:16, 3:32, 4:64, 5:128, O/binary>>,
%%% use expression : [{A, B, C, D, E} || <<A:8, B:16, C:32, D:64, E:128>> <= Binary].
%%% use pattern split binary list <<Length:16, BinaryList:Length/binary-unit:248, Other/binary>>
%%%
%%% Write bit field list recommend
%%% has tuple list : List = [{A, B, C, D, E} | _],
%%% use expression : << <<A:8, B:16, C:32, D:64, E:128>> || {A, B, C, D, E} <- List >>.
%%% write binary with string
%%% use expression : << <<A:16, B:32, (length(C)):16, (iolist_to_binary(C))/binary, D:64, E:128>> end || {A, B, C, D, E} <- List >>
%%% @end
%%%-------------------------------------------------------------------
-module(protocol).
%% API
-export([revise/1, read/2]).
-export([read_unsigned/2, read_integer/2]).
-export([read_list/2, write_list/2]).
-export([write_ets/2]).
-export([read_string/1, write_string/1]).
-export([read_bit_string/1, write_bit_string/1]).
-export([pack/2]).
%%%===================================================================
%%% API functions
%%%===================================================================
%% @doc read bit/string link style call, revise list data
-spec revise(list()) -> {tuple(), binary()}.
revise([Value, Remain]) ->
    {list_to_tuple(lists:reverse(Value)), Remain}.

%% @doc read bit/string link style call
-spec read(Bit :: string | non_neg_integer(), Binary :: binary() | list()) -> list().
read(string, Binary) when is_binary(Binary) ->
    <<Length:16, Value:Length/binary-unit:8, Remain/binary>> = Binary,
    [[Value], Remain];
read(Bit, Binary) when is_binary(Binary) ->
    <<Value:Bit, Remain/binary>> = Binary,
    [[Value], Remain];
read(string, [LastValue, Binary]) ->
    <<Length:16, Value:Length/binary-unit:8, Remain/binary>> = Binary,
    [[Value | LastValue], Remain];
read(Bit, [LastValue, Binary]) ->
    <<Value:Bit, Remain/binary>> = Binary,
    [[Value | LastValue], Remain].

%% @doc read unsigned integer
-spec read_unsigned(Bit :: non_neg_integer(), Binary :: binary()) -> {non_neg_integer(), binary()}.
read_unsigned(Bit, Binary) ->
    <<Value:Bit, Remain/binary>> = Binary,
    {Value, Remain}.

%% @doc read signed integer
-spec read_integer(Bit :: non_neg_integer(), Binary :: binary()) -> {integer(), binary()}.
read_integer(Bit, Binary) ->
    <<Value:Bit/signed, Remain/binary>> = Binary,
    {Value, Remain}.

%% @doc read string
-spec read_string(binary()) -> {list(), binary()}.
read_string(<<Length:16, BitString:Length/binary-unit:8, Binary/binary>>) ->
    {binary_to_list(BitString), Binary}.

%% @doc read bit string
-spec read_bit_string(binary()) -> {binary(), binary()}.
read_bit_string(<<Length:16, BitString:Length/binary-unit:8, Binary/binary>>) ->
    {BitString, Binary}.

%% @doc write string
-spec write_string(String :: list()) -> binary().
write_string(String) ->
    Binary = list_to_binary(String),
    Length = byte_size(Binary),
    <<Length:16, Binary/binary>>.

%% @doc write bit string
-spec write_bit_string(Binary :: binary()) -> binary().
write_bit_string(Binary) ->
    Length = byte_size(Binary),
    <<Length:16, Binary/binary>>.

%% @doc read list
-spec read_list(F :: fun((Element :: term()) -> binary()), binary()) -> {binary(), list()}.
read_list(F, <<Length:16, Binary/binary>>) ->
    read_list(Binary, F, Length, []).
read_list(Binary, _F, 0, Acc) ->
    {Binary, Acc};
read_list(Binary, F, Length, Acc) ->
    {Result, Remain} = F(Binary),
    read_list(Remain, F, Length - 1, [Result | Acc]).

%% @doc write list
-spec write_list(F :: fun((Element :: term()) -> binary()), L :: list()) -> binary().
write_list(F, L) ->
    write_list(L, F, 0, <<>>).
write_list([], _F, Length, Acc) ->
    <<Length:16, Acc/binary>>;
write_list([H | T], F, Length, Acc) ->
    write_list(T, F, Length + 1, <<Acc/binary, (F(H))/binary>>).

%% @doc write ets
-spec write_ets(F :: fun((Element :: term()) -> binary()), T :: ets:tab()) -> binary().
write_ets(F, T) ->
    write_ets(T, ets:first(T), F, 0, <<>>).
write_ets(_T, '$end_of_table', _F, Length, Acc) ->
    <<Length:16, Acc/binary>>;
write_ets(T, Key, F, Length, Acc) ->
    write_ets(T, ets:next(T, Key), F, Length + 1, <<Acc/binary, (F(ets:lookup(T, Key)))/binary>>).

%% @doc pack package with data length and protocol
-spec pack(Protocol :: non_neg_integer(), Data :: binary()) -> binary().
pack(Protocol, Data) ->
    Length = byte_size(Data) + 4,
    <<Length:16, Protocol:16, Data/binary>>.
%%%===================================================================
%%% Internal functions
%%%===================================================================


%%%-------------------------------------------------------------------
%%% @doc
%%% protocol(binary read/write) tool
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
-export([read_unsigned/2, read_integer/2]).
-export([read_list/2, write_list/2]).
-export([write_ets/2, write_key_ets/3]).
-export([read_string/1, write_string/1]).
-export([read_bit_string/1, write_binary/1]).
-export([pack/2]).
-export([text/1]).
%%%===================================================================
%%% API functions
%%%===================================================================
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
    write_binary(list_to_binary(String)).

%% @doc write binary
-spec write_binary(Binary :: binary()) -> binary().
write_binary(Binary) ->
    <<(byte_size(Binary)):16, Binary/binary>>.

%% @doc read list
-spec read_list(F :: fun((Element :: term()) -> binary()), binary()) -> {list(), binary()}.
read_list(F, <<Length:16, Binary/binary>>) ->
    read_list(Binary, F, Length, []).
read_list(Binary, _F, 0, Acc) ->
    {Acc, Binary};
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
-spec write_ets(F :: fun((Element :: term()) -> binary()), Tab :: ets:tab()) -> binary().
write_ets(F, Tab) ->
    ets:safe_fixtable(Tab, true),
    write_ets(Tab, ets:first(Tab), F, 0, <<>>).
write_ets(Tab, '$end_of_table', _F, Length, Acc) ->
    ets:safe_fixtable(Tab, false),
    <<Length:16, Acc/binary>>;
write_ets(Tab, Key, F, Length, Acc) ->
    case ets:lookup(Tab, Key) of
        [] ->
            write_ets(Tab, ets:next(Tab, Key), F, Length, Acc);
        Object ->
            write_ets(Tab, ets:next(Tab, Key), F, Length + 1, <<Acc/binary, (F(Object))/binary>>)
    end.

%% @doc write ets with key list
-spec write_key_ets(F :: fun((Element :: term()) -> binary()), Tab :: ets:tab(), KeyList :: [term()]) -> binary().
write_key_ets(F, Tab, KeyList) ->
    ets:safe_fixtable(Tab, true),
    write_key_ets(Tab, F, 0, KeyList, <<>>).
write_key_ets(Tab, _F, Length, [], Acc) ->
    ets:safe_fixtable(Tab, false),
    <<Length:16, Acc/binary>>;
write_key_ets(Tab, F, Length, [Key | KeyList], Acc) ->
    case ets:lookup(Tab, Key) of
        [] ->
            write_key_ets(Tab, F, Length, KeyList, Acc);
        Object ->
            write_key_ets(Tab, F, Length + 1, KeyList, <<Acc/binary, (F(Object))/binary>>)
    end.

%% @doc pack package with data length and protocol
-spec pack(Protocol :: non_neg_integer(), Data :: binary()) -> binary().
pack(Protocol, Data) ->
    %% Length = byte_size(Data) + 4,
    <<(byte_size(Data)):16, Protocol:16, Data/binary>>.

%% @doc result text translate
-spec text(Key :: atom()) -> binary().
text(ok) ->
    <<0:16>>;
text(Key) ->
    type:to_binary(text_data:text(Key)).

%%%===================================================================
%%% Internal functions
%%%===================================================================

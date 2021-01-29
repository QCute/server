%%%-------------------------------------------------------------------
%%% @doc
%%% unicode encoding extended library
%%% @end
%%%-------------------------------------------------------------------
-module(encoding).
%% API
-export([to_list/1, to_list_int/1, to_hex/1, to_char/1]).
-export([url_encode/1]).
%%%===================================================================
%%% API functions
%%%===================================================================
%% @doc convert to unicode format
-spec list(term()) -> list() | {error, list(), Rest :: unicode:latin1_chardata() | unicode:chardata() | unicode:external_chardata()} | {incomplete, list(), binary()}.
to_list(Term) ->
    list(type:to_list(Term)).

%% @doc convert to unicode format
-spec to_list_int(list()) -> list() | {error, list(), Rest :: unicode:latin1_chardata() | unicode:chardata() | unicode:external_chardata()} | {incomplete, list(), binary()}.
to_list_int(Term) ->
    int_list(type:to_list(Term)).

%% @doc to hex
-spec to_hex(list()) -> [string()].
to_hex(Term) ->
    [io_lib:format("~4.16.0B", [Code]) || Code <- to_list_int(Term)].

%% @doc to unicode char
-spec to_char([string()]) -> [non_neg_integer()].
to_char(List) ->
    [erlang:list_to_integer(Code, 16) || Code <- List].

%% @doc to utf8 url encode
-spec url_encode(Url :: string() | binary()) -> string().
url_encode(Url) ->
    escape_uri(to_list(Url)).

%%%===================================================================
%%% Internal functions
%%%===================================================================
list(List) when is_list(List) ->
    case catch list_to_binary(List) of
        {'EXIT', _} ->
            case unicode:characters_to_binary(List, utf8) of
                Binary when is_binary(Binary) ->
                    binary_to_list(Binary);
                {error, Encoded, Rest} ->
                    {error, unicode:characters_to_list(Encoded, utf8), Rest};
                {incomplete, Encoded, Rest} ->
                    {incomplete, unicode:characters_to_list(Encoded, utf8), Rest}
            end;
        _ ->
            List
    end.

int_list(List) when is_list(List) ->
    case catch list_to_binary(List) of
        {'EXIT', _} ->
            List;
        Binary ->
            case unicode:characters_to_list(Binary, utf8) of
                IntList when is_list(IntList) ->
                    IntList;
                {error, Encoded, Rest} ->
                    {error, unicode:characters_to_list(Encoded, utf8), Rest};
                {incomplete, Encoded, Rest} ->
                    {incomplete, unicode:characters_to_list(Encoded, utf8), Rest}
            end
    end.

%% escape uri
escape_uri([C | Cs]) when C >= $a, C =< $z ->
    [C | escape_uri(Cs)];
escape_uri([C | Cs]) when C >= $A, C =< $Z ->
    [C | escape_uri(Cs)];
escape_uri([C | Cs]) when C >= $0, C =< $9 ->
    [C | escape_uri(Cs)];
escape_uri([C = $. | Cs]) ->
    [C | escape_uri(Cs)];
escape_uri([C = $- | Cs]) ->
    [C | escape_uri(Cs)];
escape_uri([C = $_ | Cs]) ->
    [C | escape_uri(Cs)];
escape_uri([C | Cs]) ->
    escape_byte(C) ++ escape_uri(Cs);
escape_uri([]) ->
    [].

escape_byte(C) when C >= 0, C =< 255 ->
    [$%, hex_digit(C bsr 4), hex_digit(C band 15)].

hex_digit(N) when N >= 0, N =< 9 ->
    N + $0;
hex_digit(N) when N > 9, N =< 15 ->
    N + $a - 10.

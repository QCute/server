%%%-------------------------------------------------------------------
%%% @doc
%%% module encoding
%%% unicode extended library
%%% @end
%%%-------------------------------------------------------------------
-module(encoding).
%% API
-export([to_list/1, to_list_int/1, to_hex/1, to_char/1]).
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

-spec to_hex(list()) -> [string()].
to_hex(Term) ->
    [io_lib:format("~4.16.0B", [Code]) || Code <- to_list_int(Term)].

-spec to_char([string()]) -> [non_neg_integer()].
to_char(List) ->
    [erlang:list_to_integer(Code, 16) || Code <- List].
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

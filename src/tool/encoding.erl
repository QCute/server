%%%-------------------------------------------------------------------
%%% @doc
%%% module encoding
%%% @end
%%%-------------------------------------------------------------------
-module(encoding).
-export([to_list/1, to_list_int/1]).
%%%===================================================================
%%% API
%%%===================================================================
%% @doc convert to unicode format
-spec to_list(term()) -> list().
to_list(Term) when is_list(Term)          -> list(Term);
to_list(Term) when is_atom(Term)          -> list(atom_to_list(Term));
to_list(Term) when is_binary(Term)        -> list(binary_to_list(Term));
to_list(Term) when is_integer(Term)       -> list(integer_to_list(Term));
to_list(Term)                             -> Term.

%% @doc convert to unicode format
-spec to_list_int(term()) -> list().
to_list_int(Term) when is_list(Term)      -> int_list(Term);
to_list_int(Term) when is_atom(Term)      -> int_list(atom_to_list(Term));
to_list_int(Term) when is_binary(Term)    -> int_list(binary_to_list(Term));
to_list_int(Term) when is_integer(Term)   -> int_list(integer_to_list(Term));
to_list_int(Term)                         -> Term.

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

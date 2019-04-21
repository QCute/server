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
-spec to_list(any()) -> list().
to_list(Term) when is_list(Term)          -> to_list(Term, list);
to_list(Term) when is_atom(Term)          -> to_list(atom_to_list(Term), list);
to_list(Term) when is_binary(Term)        -> to_list(binary_to_list(Term), list);
to_list(Term) when is_integer(Term)       -> to_list(integer_to_list(Term), list);
to_list(Term)                             -> Term.

%% @doc convert to unicode format
-spec to_list_int(any()) -> list().
to_list_int(Term) when is_list(Term)      -> to_list(Term, int);
to_list_int(Term) when is_atom(Term)      -> to_list(atom_to_list(Term), int);
to_list_int(Term) when is_binary(Term)    -> to_list(binary_to_list(Term), int);
to_list_int(Term) when is_integer(Term)   -> to_list(integer_to_list(Term), int);
to_list_int(Term)                         -> Term.

%%%===================================================================
%%% Internal functions
%%%===================================================================
to_list(Encode, int) when is_list(Encode) ->
    case catch list_to_binary(Encode) of
        {'EXIT', _} ->
            Encode;
        Binary ->
            unicode:characters_to_list_int(Binary, utf8)
    end;
to_list(Encode, list) when is_list(Encode) ->
    case catch list_to_binary(Encode) of
        {'EXIT', _} ->
            binary_to_list(unicode:characters_to_binary(Encode, utf8));
        _ ->
            Encode
    end.
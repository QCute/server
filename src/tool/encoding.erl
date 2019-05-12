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
to_list(Term) when is_list(Term)          -> list(Term);
to_list(Term) when is_atom(Term)          -> list(atom_to_list(Term));
to_list(Term) when is_binary(Term)        -> list(binary_to_list(Term));
to_list(Term) when is_integer(Term)       -> list(integer_to_list(Term));
to_list(Term)                             -> Term.

%% @doc convert to unicode format
-spec to_list_int(any()) -> list().
to_list_int(Term) when is_list(Term)      -> int_list(Term);
to_list_int(Term) when is_atom(Term)      -> int_list(atom_to_list(Term));
to_list_int(Term) when is_binary(Term)    -> int_list(binary_to_list(Term));
to_list_int(Term) when is_integer(Term)   -> int_list(integer_to_list(Term));
to_list_int(Term)                         -> Term.

%%%===================================================================
%%% Internal functions
%%%===================================================================
list(Encode) when is_list(Encode) ->
    case catch list_to_binary(Encode) of
        {'EXIT', _} ->
            %% will crash if encoding error
            binary_to_list(unicode:characters_to_binary(Encode, utf8));
        _ ->
            Encode
    end.

int_list(Encode) when is_list(Encode) ->
    case catch list_to_binary(Encode) of
        {'EXIT', _} ->
            Encode;
        Binary ->
            %% will failed if encoding error
            unicode:characters_to_list(Binary, utf8)
    end.

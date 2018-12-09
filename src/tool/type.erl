%%%-------------------------------------------------------------------
%%% @doc
%%% module erlang type convert
%%% @end
%%%-------------------------------------------------------------------
-module(type).
-export([to_list/1, to_atom/1, to_integer/1]).
%%%===================================================================
%%% API
%%%===================================================================
%% @doc convert other type to list
-spec to_list(any()) -> list().
to_list(X) when is_atom(X)             -> atom_to_list(X);
to_list(X) when is_tuple(X)            -> tuple_to_list(X);
to_list(X) when is_integer(X)          -> integer_to_list(X);
to_list(X) when is_binary(X)           -> binary_to_list(X);
to_list(X) when is_float(X)            -> float_to_list(X);
to_list(X) when is_bitstring(X)        -> bitstring_to_list(X);
to_list(X) when is_list(X)             -> X.

%% @doc convert other type to atom
-spec to_atom(any()) -> atom().
to_atom(X) when is_list(X)             -> list_to_atom2(X);
to_atom(X) when is_binary(X)           -> list_to_atom2(binary_to_list(X));
to_atom(X) when is_atom(X)             -> X.
list_to_atom2(List) when is_list(List) ->
    case catch list_to_existing_atom(List) of
        {'EXIT', _} ->
            list_to_atom(List);
        Atom when is_atom(Atom) ->
            Atom
    end.

%% @doc convert other type to integer
-spec to_integer(any()) -> integer().
to_integer(X) when is_binary(X)        -> list_to_integer(binary_to_list(X));
to_integer(X) when is_list(X)          -> list_to_integer(X);
to_integer(X) when is_float(X)         -> round(X);
to_integer(X) when is_integer(X)       -> X.

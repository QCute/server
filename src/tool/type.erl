%%%-------------------------------------------------------------------
%%% @doc
%%% module erlang type convert
%%% @end
%%%-------------------------------------------------------------------
-module(type).
-export([to_list/1, to_binary/1, to_atom/1, to_integer/1]).
-export([what/1, default/1]).
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

-spec to_binary(any()) -> binary().
to_binary(X) when is_atom(X)           -> atom_to_binary(X, utf8);
to_binary(X) when is_integer(X)        -> integer_to_binary(X);
to_binary(X) when is_list(X)           -> list_to_binary(X);
to_binary(X)                           -> X.

%% @doc convert other type to atom
-spec to_atom(any()) -> atom().
to_atom(X) when is_list(X)             -> to_existing_atom(X);
to_atom(X) when is_binary(X)           -> to_existing_atom(X);
to_atom(X) when is_atom(X)             -> X.
to_existing_atom(X) when is_list(X)    ->
    case catch list_to_existing_atom(X) of
        {'EXIT', _} ->
            list_to_atom(X);
        Atom when is_atom(Atom) ->
            Atom
    end;
to_existing_atom(X) when is_binary(X)  ->
    case catch binary_to_existing_atom(X, utf8) of
        {'EXIT', _} ->
            binary_to_atom(X, utf8);
        Atom when is_atom(Atom) ->
            Atom
    end.

%% @doc convert other type to integer
-spec to_integer(any()) -> integer().
to_integer(X) when is_binary(X)        -> binary_to_integer(X);
to_integer(X) when is_list(X)          -> list_to_integer(X);
to_integer(X) when is_float(X)         -> round(X);
to_integer(X) when is_integer(X)       -> X.

%% @doc what type is
-spec what(any()) -> atom().
what(X) when is_atom(X)                -> atom;
what(X) when is_binary(X)              -> binary;
what(X) when is_list(X)                -> list;
what(X) when is_tuple(X)               -> tuple;
what(X) when is_float(X)               -> float;
what(X) when is_integer(X)             -> integer;
what(X) when is_reference(X)           -> reference;
what(X) when is_function(X)            -> function.

%% @doc get type default
-spec default(any()) -> term().
default(X) when is_integer(X)          -> 0;
default(X) when is_atom(X)             -> undefined;
default(X) when is_binary(X)           -> <<>>;
default(X) when is_list(X)             -> [];
default(X) when is_tuple(X)            -> {};
default(X) when is_float(X)            -> 0.0;
default(X) when is_reference(X)        -> make_ref();
default(X) when is_function(X)         -> fun() -> ok end.

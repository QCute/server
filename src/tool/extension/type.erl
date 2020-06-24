%%%-------------------------------------------------------------------
%%% @doc
%%% module erlang type convert
%%% erlang extended library
%%% @end
%%%-------------------------------------------------------------------
-module(type).
%% API
-export([to_list/1, to_binary/1, to_atom/1, to_integer/1, to_float/1]).
-export([to_boolean/1, to_flag/1]).
-export([what/1, default/1]).
%%%===================================================================
%%% API functions
%%%===================================================================
%% @doc convert other type to list
-spec to_list(any()) -> list().
to_list(X) when is_list(X)             -> X;
to_list(X) when is_atom(X)             -> erlang:atom_to_list(X);
to_list(X) when is_tuple(X)            -> erlang:tuple_to_list(X);
to_list(X) when is_integer(X)          -> erlang:integer_to_list(X);
to_list(X) when is_float(X)            -> io_lib_format:fwrite_g(X);
to_list(X) when is_binary(X)           -> erlang:binary_to_list(X);
to_list(X) when is_bitstring(X)        -> erlang:bitstring_to_list(X);
to_list(X) when is_reference(X)        -> erlang:ref_to_list(X);
to_list(X) when is_function(X)         -> erlang:fun_to_list(X);
to_list(X) when is_pid(X)              -> erlang:pid_to_list(X);
to_list(X)                             -> X.

-spec to_binary(any()) -> binary().
to_binary(X) when is_binary(X)         -> X;
to_binary(X) when is_list(X)           -> erlang:list_to_binary(X);
to_binary(X) when is_atom(X)           -> erlang:atom_to_binary(X, utf8);
to_binary(X) when is_integer(X)        -> erlang:integer_to_binary(X);
to_binary(X)                           -> erlang:list_to_binary(to_list(X)).

%% @doc convert other type to atom
-spec to_atom(any()) -> atom().
to_atom(X) when is_atom(X)             -> X;
to_atom(X) when is_list(X)             -> list_to_atom(X);
to_atom(X) when is_binary(X)           -> binary_to_atom(X, utf8);
to_atom(X)                             -> to_atom(to_list(X)).

%% @doc convert other type to integer
-spec to_integer(any()) -> integer().
to_integer(X) when is_integer(X)       -> X;
to_integer(X) when is_binary(X)        -> erlang:binary_to_integer(X);
to_integer(X) when is_list(X)          -> erlang:list_to_integer(X);
to_integer(X) when is_float(X)         -> erlang:round(X);
to_integer(X) when is_boolean(X)       -> to_flag(X);
to_integer(_)                          -> erlang:error(badarg).

%% @doc convert other type to float
-spec to_float(any()) -> float().
to_float(X) when is_float(X)           -> X;
to_float(X) when is_integer(X)         -> list_to_float(lists:flatten(io_lib:format("~w.0", [X])));
to_float(X) when is_atom(X)            -> erlang:list_to_float(erlang:atom_to_list(X));
to_float(X) when is_binary(X)          -> erlang:binary_to_float(X);
to_float(X) when is_list(X)            -> erlang:list_to_float(X);
to_float(_)                            -> erlang:error(badarg).

%% @doc convert 1 | 0 type to true | false
-spec to_boolean(non_neg_integer() | any()) -> boolean().
to_boolean(1)                          -> true;
to_boolean(0)                          -> false;
to_boolean(_)                          -> false.

%% @doc convert true | false type to 1 | 0
-spec to_flag(boolean() | any()) -> non_neg_integer().
to_flag(true)                          -> 1;
to_flag(false)                         -> 0;
to_flag(_)                             -> 0.

%% @doc what type is
-spec what(any()) -> atom().
what(X) when is_atom(X)                -> atom;
what(X) when is_list(X)                -> list;
what(X) when is_tuple(X)               -> tuple;
what(X) when is_binary(X)              -> binary;
what(X) when is_float(X)               -> float;
what(X) when is_integer(X)             -> integer;
what(X) when is_float(X)               -> float;
what(X) when is_number(X)              -> number;
what(X) when is_reference(X)           -> reference;
what(X) when is_function(X)            -> function.

%% @doc get type default
-spec default(any()) -> term().
default(X) when is_atom(X)             -> undefined;
default(X) when is_list(X)             -> [];
default(X) when is_tuple(X)            -> {};
default(X) when is_binary(X)           -> <<>>;
default(X) when is_integer(X)          -> 0;
default(X) when is_float(X)            -> 0.0;
default(X) when is_number(X)           -> -0;
default(X) when is_reference(X)        -> make_ref();
default(X) when is_function(X)         -> fun() -> ok end.

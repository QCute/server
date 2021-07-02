%%%-------------------------------------------------------------------
%%% @doc
%%% unicode/string extended library
%%% @end
%%%-------------------------------------------------------------------
-module(word).
-compile({no_auto_import, [length/1]}).
%% API
-export([validate/1, validate/2, length/1, byte/1, sensitive/1]).
-export([to_hump/1, to_lower_hump/1]).
-export([to_hex/1, to_char/1]).
%%%===================================================================
%%% API functions
%%%===================================================================
%% @doc string check
-spec validate(String :: binary()) -> true | {false, Reason :: term()} | {false, atom(), Reason :: term()}.
validate(String) ->
    validate_loop([{length, 1, 6}, sensitive], String).

-spec validate(String :: binary(), ConditionList :: [{length, non_neg_integer(), non_neg_integer()} | sensitive | {sql, binary()}]) -> true | {false, Reason :: term()} | {false, atom(), Reason :: term()}.
validate(String, ConditionList) ->
    validate_loop(ConditionList, String).

validate_loop([], _String) ->
    true;
validate_loop([{length, Min, Max} | T], String) ->
    case length(String) of
        {ok, Length} when Min =< Length andalso Length =< Max ->
            validate_loop(T, String);
        {ok, Length} ->
            {false, length, Length};
        {error, _} ->
            {false, asn1, bad_utf8_character_encoding}
    end;
validate_loop([sensitive | T], String) ->
    case sensitive(String) of
        false ->
            validate_loop(T, String);
        true ->
            {false, sensitive}
    end;
validate_loop([{sql, Sql} | T], String) ->
    case db:select(Sql) of
        [] ->
            validate_loop(T, String);
        Data ->
            {false, duplicate, Data}
    end.

%% @doc word length
-spec length(String :: binary()) -> {ok, Length :: non_neg_integer()} | {error, Reason :: term()}.
length(String) ->
    case unicode:characters_to_list(String) of
        List when is_list(List) ->
            {ok, erlang:length(List)};
        _ ->
            {error, not_utf8_encoding}
    end.

%% @doc word byte size
-spec byte(String :: binary()) -> {ok, Length :: non_neg_integer()} | {error, Reason :: term()}.
byte(String) ->
    case unicode:characters_to_list(String) of
        List when is_list(List) ->
            {ok, erlang:length(List)};
        _ ->
            {error, not_utf8_encoding}
    end.

%% @doc sensitive word
-spec sensitive(String :: binary()) -> boolean().
sensitive(String) ->
    sensitive_word_data:word(String).

%% @doc hump name
%% hump_name -> HumpName
-spec to_hump(atom() | binary() | string()) -> string().
to_hump(Atom) when is_atom(Atom) ->
    to_hump(atom_to_list(Atom));
to_hump(Binary) when is_binary(Binary) ->
    to_hump(binary_to_list(Binary));
to_hump(Name) when is_list(Name) ->
    lists:concat([[string:to_upper(H) | T] || [H | T] <- string:tokens(Name, "_")]).

%% @doc lower_hump
%% lower_hump/LowerHump -> lowerHump
-spec to_lower_hump(atom() | binary() | string()) -> string().
to_lower_hump(Name) ->
    [Head | Tail] = to_hump(Name),
    [string:to_lower(Head) | Tail].

%% @doc to hex
-spec to_hex(list()) -> [string()].
to_hex(Term) when is_atom(Term) ->
    [io_lib:format("~4.16.0B", [Code]) || Code <- atom_to_list(Term)];
to_hex(Term) when is_binary(Term) ->
    [io_lib:format("~4.16.0B", [Code]) || Code <- unicode:characters_to_list(Term)];
to_hex(Term) when is_list(Term) ->
    [io_lib:format("~4.16.0B", [Code]) || Code <- Term].

%% @doc to unicode char
-spec to_char([string()]) -> [non_neg_integer()].
to_char(List) ->
    [erlang:list_to_integer(Code, 16) || Code <- List].

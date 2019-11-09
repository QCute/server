%%%------------------------------------------------------------------
%%% @doc
%%% module word
%%% unicode/string extended library
%%% @end
%%%------------------------------------------------------------------
-module(word).
-compile({no_auto_import, [length/1]}).
%% API
-export([validate/1, validate/2, length/1, sensitive/1]).
%%%==================================================================
%%% API functions
%%%==================================================================
%% @doc string check
-spec validate(String :: binary() | list()) -> true | {false, Reason :: term()} | {false, atom(), Reason :: term()}.
validate(String) ->
    validate([{length, 1, 6}, sensitive], String).

-spec validate(ConditionList :: list(), String :: binary()) -> true | {false, Reason :: term()} | {false, atom(), Reason :: term()}.
validate([], _String) ->
    true;
validate([{length, Min, Max} | T], String) ->
    case length(String) of
        {ok, Length} when Min =< Length andalso Length =< Max ->
            validate(T, String);
        {ok, Length} ->
            {false, length, Length};
        {error, _} ->
            {false, asn1, bad_utf8_character_encoding}
    end;
validate([sensitive | T], String) ->
    case sensitive(String) of
        false ->
            validate(T, String);
        true ->
            {false, sensitive}
    end;
validate([{sql, Sql} | T], String) ->
    case sql:select(Sql) of
        [] ->
            validate(T, String);
        _ ->
            {false, duplicate}
    end.

%% @doc word length
-spec length(String :: binary() | list()) -> {ok, Length :: non_neg_integer()} | {error, Reason :: term()}.
length(String) ->
    case encoding:to_list_int(String) of
        List when is_list(List) ->
            {ok, erlang:length(List)};
        _ ->
            {error, non_utf8_encoding}
    end.

%% @doc sensitive word
-spec sensitive(String :: binary() | list()) -> true | false.
sensitive(Word) when is_list(Word) ->
    case unicode:characters_to_binary(Word, utf8) of
        Result when is_binary(Result) ->
            sensitive(Result);
        _ ->
            false
    end;
sensitive(Word) ->
    case dict:find(Word, sensitive_word_data:words()) of
        {ok, _} ->
            true;
        _ ->
            false
    end.

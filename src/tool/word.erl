%%%-------------------------------------------------------------------
%%% @doc
%%% module word
%%% @end
%%%-------------------------------------------------------------------
-module(word).
-compile({no_auto_import, [length/1]}).
-export([validate/1, validate/2, length/1, sensitive/1]).
%%%===================================================================
%%% API
%%%===================================================================
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
length(String) when is_list(String) ->
    length(list_to_binary(String));
length(String) ->
    case unicode:characters_to_list_int(String, utf8) of
        {ok, UnicodeList} ->
            {ok, erlang:length(UnicodeList)};
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc sensitive word
-spec sensitive(String :: binary() | list()) -> true | false.
sensitive(Word) when is_list(Word) ->
    sensitive(list_to_binary(Word));
sensitive(Word) ->
    case dict:find(Word, data_sensitive_words:words()) of
        {ok, _} ->
            true;
        _ ->
            false
    end.

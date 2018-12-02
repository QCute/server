%% -*- coding: utf-8 -*-
-module(word).
-compile({no_auto_import,[length/2]}).
-export([valid/1, valid/2, length/1, sensitive/1, words/0]).

%% @doc sensitive word
valid(Word) ->
    valid(Word, 6).
valid(Word, LengthLimit) ->
    WordLength = ?MODULE:length(Word),
    case WordLength =< LengthLimit of
        true ->
            sensitive(Word);
        _ ->
            false
    end.

%% @doc word length
length(Word) ->
    string:len(Word).

%% @doc sensitive word
sensitive(Word) when is_list(Word) ->
    sensitive(list_to_binary(Word));
sensitive(Word) ->
    case dict:find(Word, words()) of
        {ok, _} ->
            true;
        _ ->
            false
    end.


%% @doc sensitive dict
words() -> dict:new().
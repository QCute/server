%%%-------------------------------------------------------------------
%%% @doc
%%% make sensitive words checker code
%%% @end
%%%-------------------------------------------------------------------
-module(word_maker).
-export([start/1]).
%%%===================================================================
%%% API functions
%%%===================================================================
%% @doc for shell
start(List) ->
    maker:start(fun parse_table/1, List).

%%%===================================================================
%%% Internal functions
%%%===================================================================
parse_table(#{file := File, table := Table}) ->
    Module = filename:basename(File, ".erl"),
    WordList = db:select(<<"SELECT * FROM `~s`">>, [Table]),
    Head = io_lib:format("-module(~s).\n-export([word/1]).\n\n-spec word(Word :: binary()) -> boolean().\n", [Module]),
    Code = [io_lib:format("word(~w) -> true;\n", Word) || Word <- WordList],
    Tail = ["word(_) -> false.\n\n"],
    [#{pattern => "(?s).*", code => lists:concat([Head, Code, Tail])}].

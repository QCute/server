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
parse_table({File, Table}) ->
    Module = filename:basename(File, ".erl"),
    WordList = db:select(<<"SELECT * FROM `~s`">>, [Table]),
    Head = io_lib:format("-module(~s).\n-compile(nowarn_export_all).\n-compile(export_all).\n\n", [Module]),
    Code = lists:flatten([io_lib:format("word(~w) -> true;\n", Word) || Word <- WordList]) ++ "word(_) -> false.",
    %% [{"%% @doc sensitive dict\n(?m)(?s)^words.+?(?=\\.$)\\.",""}, {"", Words}].
    [{"(?s).*", Head ++ Code}].

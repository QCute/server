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
    maker:start(fun parse_table/2, List).

%%%===================================================================
%%% Internal functions
%%%===================================================================
parse_table(_, {File, Table}) ->
    Module = filename:basename(File, ".erl"),
    SQL = io_lib:format(<<"SELECT * FROM `~s`">>, [Table]),
    WordList = maker:select(SQL),
    Head = io_lib:format("-module(~s).\n-compile(nowarn_export_all).\n-compile(export_all).\n\n", [Module]),
    Code = lists:flatten([io_lib:format("word(~w) -> true;\n", Word) || Word <- WordList]) ++ "word(_) -> false.",
    %% [{"%% @doc sensitive dict\n(?m)(?s)^words.+?(?=\\.$)\\.",""}, {"", Words}].
    [{"(?s).*", Head ++ Code}].

%%%-------------------------------------------------------------------
%%% @doc
%%% module word maker
%%% make sensitive words to erlang dict
%%% @end
%%%-------------------------------------------------------------------
-module(word_maker).
-export([start/1]).
%%%===================================================================
%%% API
%%%===================================================================
%% @doc for shell
start(List) ->
    maker:start(fun parse_table/2, List).

%% ====================================================================
%% Internal functions
%% ====================================================================
parse_table(_, {File, Table}) ->
    Module = filename:basename(File, ".erl"),
    SQL = io_lib:format(<<"SELECT * FROM `~s`">>, [Table]),
    Raw = maker:select(SQL),
    Head = io_lib:format("-module(~s).\n-compile(nowarn_export_all).\n-compile(export_all).\n\n", [Module]),
    Code = io_lib:format("%% @doc sensitive dict\nwords() ->\n    ~lp.", [dict:from_list([{X, 0} || [X | _] <- Raw])]),
    %[{"%% @doc sensitive dict\n(?m)(?s)^words.+?(?=\\.$)\\.",""}, {"", Words}].
    [{"(?s).*", Head ++ Code}].

%%%-------------------------------------------------------------------
%%% @doc
%%% module database data to erlang term tool
%%% @end
%%%-------------------------------------------------------------------
-module(word_maker).
-export([start/1]).
-export([parse/2]).
%%%===================================================================
%%% API
%%%===================================================================
%% @doc for shell
start(List) ->
    maker:start(fun parse_table/2, List).

%% @doc parse
parse(DataBase, One) ->
    parse_table(DataBase, One).
%% ====================================================================
%% Internal functions
%% ====================================================================
parse_table(DataBase, {_, Name}) ->
    SQL = io_lib:format(<<"SELECT * FROM `~s`">>, [Name]),
    Raw = sql:select(DataBase, Name, SQL),
    Words = io_lib:format("%% @doc sensitive dict\nwords() ->\n    ~lp.", [dict:from_list([{X, 0} || [X | _] <- Raw])]),
    [{"%% @doc sensitive dict\n(?m)(?s)^words.+?(?=\\.$)\\.",""}, {"", Words}].
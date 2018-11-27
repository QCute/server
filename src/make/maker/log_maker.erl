%%%-------------------------------------------------------------------
%%% @doc
%%% module database fields to sql code tool for log
%%% @end
%%%-------------------------------------------------------------------
-module(log_maker).
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
%% parse per table
parse_table(DataBase, {_, Name}) ->
    FieldsSql = io_lib:format(<<"SELECT `COLUMN_NAME`, `COLUMN_DEFAULT`, `DATA_TYPE`, `COLUMN_COMMENT`, `ORDINAL_POSITION`, `COLUMN_KEY`, `EXTRA` FROM information_schema.`COLUMNS` WHERE `TABLE_SCHEMA` = '~s' AND TABLE_NAME = '~s' ORDER BY ORDINAL_POSITION;">>, [DataBase, Name]),
    %% fetch table fields
    RawFields = sql:select(DataBase, Name, FieldsSql),
    %% convert type to format
    AllFields = [[N, D, "'~w'", C, P, K, E] || [N, D, _, C, P, K, E] <- RawFields],
    InsertFields = string:join([io_lib:format("`~s`", [N]) || [N, _, _, _, _, _, E] <- AllFields, E =/= <<"auto_increment">>], ", "),
    InsertFormat = string:join([T || [_, _, T, _, _, _, E] <- AllFields, E =/= <<"auto_increment">>], ", "),
    Sql = io_lib:format("INSERT INTO (~s) VALUES (~s);", [InsertFields, InsertFormat]),
    Patten = io_lib:format("(?s)(do\\(\\[~s.*?)(?=do\\()", [Name]),
    Code = io_lib:format("do([~s | T]) ->\n    sql:insert(io_lib:format(\"~s\", T));\ndo(_) ->\n    ok.", [Name, Sql]),
    [{Patten, ""}, {"(?m)(?s)(do\\(_\\)\\s*->.*?(?:\\.$))", Code}].

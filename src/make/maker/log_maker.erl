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
    maker:start(fun parse_table_sql/2, List),
    maker:start(fun parse_table_log/2, List).

%% @doc parse
parse(DataBase, One) ->
    parse_table_sql(DataBase, One) ++ parse_table_log(DataBase, One).
%% ====================================================================
%% Internal functions
%% ====================================================================
%% parse per table sql
parse_table_sql(DataBase, {_, Table}) ->
    FieldsSql = io_lib:format(<<"SELECT `COLUMN_NAME`, `COLUMN_DEFAULT`, `DATA_TYPE`, `COLUMN_COMMENT`, `ORDINAL_POSITION`, `COLUMN_KEY`, `EXTRA` FROM information_schema.`COLUMNS` WHERE `TABLE_SCHEMA` = '~s' AND `TABLE_NAME` = '~s' ORDER BY `ORDINAL_POSITION`;">>, [DataBase, Table]),
    %% fetch table fields
    RawFields = sql:select(DataBase, Table, FieldsSql),
    %% convert type to format
    F = fun(<<"char">>) -> "'~s'";(_) -> "'~w'" end,
    AllFields = [[N, D, F(T), C, P, K, E] || [N, D, T, C, P, K, E] <- RawFields],
    InsertFields = string:join([io_lib:format("`~s`", [N]) || [N, _, _, _, _, _, E] <- AllFields, E =/= <<"auto_increment">>], ", "),
    InsertFormat = string:join([T || [_, _, T, _, _, _, E] <- AllFields, E =/= <<"auto_increment">>], ", "),
    Sql = io_lib:format("INSERT INTO `~s` (~s) VALUES ", [Table, InsertFields]),
    Patten = io_lib:format("(?s)(?m)(sql\\(~s\\).*?)(?=^sql\\()", [Table]),
    Code = io_lib:format("sql(~s) ->\n    {\"~s\", \"(~s)\"};\n", [Table, Sql, InsertFormat]),
    EndCode = "sql(_) ->\n    ok.\n\n",
    EndPatten = "(?m)(?s)sql\\(_\\)\\s*->.*?(?:\\.$\n?\n?)",
    %% replace with patten/code when if not exists replace with next patten/code
    [{EndPatten, EndCode}, {Patten, Code, [], [{replace, EndPatten, Code ++ EndCode}]}].

%% parse per table log
parse_table_log(DataBase, {_, Table}) ->
    FieldsSql = io_lib:format(<<"SELECT `COLUMN_NAME`, `COLUMN_DEFAULT`, `DATA_TYPE`, `COLUMN_COMMENT`, `ORDINAL_POSITION`, `COLUMN_KEY`, `EXTRA` FROM information_schema.`COLUMNS` WHERE `TABLE_SCHEMA` = '~s' AND `TABLE_NAME` = '~s' ORDER BY `ORDINAL_POSITION`;">>, [DataBase, Table]),
    %% fetch table fields
    RawFields = sql:select(DataBase, Table, FieldsSql),
    FF = fun(Name) -> lists:concat([[case 96 < H andalso H < 123 of true -> H - 32; _ -> H end | T] || [H | T] <- string:tokens(Name, "_")]) end,
    Args = string:join([FF(binary_to_list(Name)) || [Name, _, _, _, _, _, E] <- RawFields, E =/= <<"auto_increment">>], ", "),
    Patten = lists:concat(["(?s)(?m)^", Table, ".*?\\.$\n?\n?"]),
    Code = lists:concat([Table, "(", Args, ") ->\n    log_server:log(", Table, ", [", Args, "]).\n\n"]),
    [{Patten, Code}].
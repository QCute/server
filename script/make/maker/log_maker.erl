%%%-------------------------------------------------------------------
%%% @doc
%%% make database fields to log sql(insert/delete)/code
%%% @end
%%%-------------------------------------------------------------------
-module(log_maker).
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
%% parse per table log
parse_table(DataBase, {_, log, Table}) ->
    FieldsSql = io_lib:format(<<"SELECT `COLUMN_NAME`, `COLUMN_DEFAULT`, `DATA_TYPE`, `COLUMN_COMMENT`, `ORDINAL_POSITION`, `COLUMN_KEY`, `EXTRA` FROM information_schema.`COLUMNS` WHERE `TABLE_SCHEMA` = '~s' AND `TABLE_NAME` = '~s' ORDER BY `ORDINAL_POSITION`;">>, [DataBase, Table]),
    %% fetch table fields
    RawFields = maker:select(FieldsSql),
    %% make hump name list
    Args = string:join([word:to_hump(Name) || [Name, _, _, _, _, _, E] <- RawFields, E =/= <<"auto_increment">>], ", "),
    %% make hump name list and replace zero time
    Value = string:join([word:to_hump(binary_to_list(Name)) || [Name, _, _, _, _, _, E] <- RawFields, E =/= <<"auto_increment">>], ", "),
    %% match replace
    Pattern = lists:concat(["(?s)(?m)^", Table, ".*?\\.$\n?\n?"]),
    Code = lists:concat([Table, "(", Args, ") ->\n    log_server:log(", Table, ", [", Value, "]).\n\n"]),
    [{Pattern, Code}];

%% parse per table sql
parse_table(DataBase, {_, sql, Table}) ->
    FieldsSql = io_lib:format(<<"SELECT `COLUMN_NAME`, `COLUMN_DEFAULT`, `DATA_TYPE`, `COLUMN_COMMENT`, `ORDINAL_POSITION`, `COLUMN_KEY`, `EXTRA` FROM information_schema.`COLUMNS` WHERE `TABLE_SCHEMA` = '~s' AND `TABLE_NAME` = '~s' ORDER BY `ORDINAL_POSITION`;">>, [DataBase, Table]),
    %% fetch table fields
    RawFields = maker:select(FieldsSql),
    %% convert type to format
    F = fun(<<"char">>) -> "'~s'";(<<"varchar">>) -> "'~w'";(_) -> "~w" end,
    AllFields = [[N, D, F(T), C, P, K, E] || [N, D, T, C, P, K, E] <- RawFields],
    InsertFields = string:join([io_lib:format("`~s`", [N]) || [N, _, _, _, _, _, E] <- AllFields, E =/= <<"auto_increment">>], ", "),
    InsertFormat = string:join([T || [_, _, T, _, _, _, E] <- AllFields, E =/= <<"auto_increment">>], ", "),
    Sql = io_lib:format("INSERT INTO `~s` (~s) VALUES ", [Table, InsertFields]),
    Pattern = io_lib:format("(?s)(?m)(sql\\(~s\\).*?;\n?)", [Table]),
    Code = io_lib:format("sql(~s) ->\n    {<<\"~s\">>, <<\"(~s)\">>};\n", [Table, Sql, InsertFormat]),
    EndCode = "sql(_) ->\n    {<<>>, <<>>}.\n",
    EndPattern = "(?m)(?s)sql\\(_\\)\\s*->.*?(?:\\.$\n?)",
    %% delete end code on first, then replace/append code, append end code on the end
    [{EndPattern, ""}, {Pattern, Code}, {EndPattern, EndCode}];

%% parse per table clean sql
parse_table(DataBase, {File, clean, Table}) ->
    parse_table(DataBase, {File, clean, Table, month});
parse_table(DataBase, {File, clean, Table, day}) ->
    parse_table(DataBase, {File, clean, Table, 86400});
parse_table(DataBase, {File, clean, Table, week}) ->
    parse_table(DataBase, {File, clean, Table, 604800});
parse_table(DataBase, {File, clean, Table, month}) ->
    parse_table(DataBase, {File, clean, Table, 2592000});
parse_table(DataBase, {File, clean, Table, year}) ->
    parse_table(DataBase, {File, clean, Table, 31536000});
parse_table(DataBase, {File, clean, Table, ExpireTime}) ->
    TableSql = io_lib:format(<<"SELECT `TABLE_NAME` FROM information_schema.`TABLES` WHERE `TABLE_SCHEMA` = '~s' AND `TABLE_NAME` = '~s';">>, [DataBase, Table]),
    [[TableName]] = maker:select(TableSql),
    %% fetch table fields
    Sql = io_lib:format("DELETE FROM `~s` WHERE `time` < ~~w LIMIT 1000", [TableName]),
    %% Pattern = "(?m)\\s*\\]\\.",
    Line = io_lib:format("        {<<\"~s\">>, ~w}", [Sql, ExpireTime]),
    %% read origin sql code
    {ok, Binary} = file:read_file(maker:relative_path(File)),
    %% extract sql list
    {match, [_, SqlData]} = max(re:run(Binary, "(?m)(?s)sql\\(\\)\\s*->\\n*\\s*\\[(.*?)(?=\\]\\s*\\.$)", [{capture, all, list}]), {match, [[], []]}),
    %% one sql per line
    List = string:tokens(string:strip(SqlData), "\n"),
    %% add/replace new sql
    Code = parse_sql_loop(List, "`" ++ binary_to_list(TableName) ++ "`", Line, []),
    %% replace new code
    [{"(?m)(?s)sql\\(\\)\\s*->.*?\\.$", "sql() ->\n    [\n" ++ Code ++ "\n    ]."}].
    %% [{"(?m)(?s)sql\\(\\)\\s*->.*?\\.(?=$\\|%\\|\\s*)", "sql() ->\n    [\n" ++ Code ++ "\n    ]."}].

parse_sql_loop([], _, Line, List) ->
    string:join(lists:reverse([Line | List]), "\n");
parse_sql_loop([H | T], Table, Line, List) ->
    case string:str(H, Table) of
        0 when T == [] ->
            string:join(lists:reverse([Line, H ++ "," | List]), "\n");
        0 ->
            parse_sql_loop(T, Table, Line, [H | List]);
        _ when T == [] ->
            string:join(lists:reverse([Line | List], T), "\n");
        _ ->
            string:join(lists:reverse([Line ++ "," | List], T), "\n")
    end.

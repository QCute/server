%%%-------------------------------------------------------------------
%%% @doc
%%% make database fields to log sql(insert/delete/dump)/code
%%% @end
%%%-------------------------------------------------------------------
-module(log_maker).
-export([start/1]).
-record(field, {name = [], default = [], type = [], format = [], comment = [], position = 0, key = [], extra = []}).
%%%===================================================================
%%% API functions
%%%===================================================================
%% @doc for shell
start(List) ->
    maker:start(fun parse_table/1, List).

%%%===================================================================
%%% Internal functions
%%%===================================================================
%% parse per table log
parse_table({_, log, Table}) ->
    FieldsSql = io_lib:format(<<"SELECT `COLUMN_NAME`, `COLUMN_DEFAULT`, `COLUMN_TYPE`, `DATA_TYPE`, `COLUMN_COMMENT`, `ORDINAL_POSITION`, `COLUMN_KEY`, `EXTRA` FROM information_schema.`COLUMNS` WHERE `TABLE_SCHEMA` = DATABASE() AND `TABLE_NAME` = '~s' ORDER BY `ORDINAL_POSITION`;">>, [Table]),
    %% fetch table fields
    AllFields = parser:convert(db:select(FieldsSql), field),
    AllFields == [] andalso error(lists:flatten(io_lib:format("Table: ~s Not Found ", [Table]))),
    %% make hump name list
    Args = string:join([word:to_hump(Name) || #field{name = Name, extra = Extra} <- AllFields, Extra =/= <<"auto_increment">>], ", "),
    %% make hump name list and replace zero time
    Value = string:join([word:to_hump(binary_to_list(Name)) || #field{name = Name, extra = Extra} <- AllFields, Extra =/= <<"auto_increment">>], ", "),
    %% match replace
    Pattern = lists:concat(["(?s)(?m)^", Table, ".*?\\.$\n?\n?"]),
    Code = lists:concat([Table, "(", Args, ") ->\n    log_server:log(", Table, ", [", Value, "]).\n\n"]),
    [{Pattern, Code}];

%% parse per table sql
parse_table({_, save, Table}) ->
    FieldsSql = io_lib:format(<<"SELECT `COLUMN_NAME`, `COLUMN_DEFAULT`, `COLUMN_TYPE`, CASE WHEN `DATA_TYPE` = 'char' THEN '\\'~~s\\'' WHEN `DATA_TYPE` = 'varchar' THEN '\\'~~w\\'' ELSE '~~w' END AS `DATA_TYPE`, `COLUMN_COMMENT`, `ORDINAL_POSITION`, `COLUMN_KEY`, `EXTRA` FROM information_schema.`COLUMNS` WHERE `TABLE_SCHEMA` = DATABASE() AND `TABLE_NAME` = '~s' ORDER BY `ORDINAL_POSITION`;">>, [Table]),
    %% fetch table fields
    AllFields = parser:convert(db:select(FieldsSql), field),
    AllFields == [] andalso error(lists:flatten(io_lib:format("Table: ~s Not Found ", [Table]))),
    %% convert type to format
    %% F = fun(<<"char">>) -> "'~s'";(<<"varchar">>) -> "'~w'";(_) -> "~w" end,
    %% AllFields = [[N, D, F(T), C, P, K, E] || [N, D, T, C, P, K, E] <- RawFields],
    InsertFields = string:join([io_lib:format("`~s`", [Name]) || #field{name = Name, extra = Extra} <- AllFields, Extra =/= <<"auto_increment">>], ", "),
    InsertFormat = string:join([binary_to_list(Format) || #field{format = Format, extra = Extra} <- AllFields, Extra =/= <<"auto_increment">>], ", "),
    Sql = io_lib:format("INSERT INTO `~s` (~s) VALUES ", [Table, InsertFields]),
    Pattern = io_lib:format("(?s)(?m)(sql\\(~s\\).*?;\n?)", [Table]),
    Code = io_lib:format("sql(~s) ->\n    {<<\"~s\">>, <<\"(~s)\">>};\n", [Table, Sql, InsertFormat]),
    EndCode = "sql(_) ->\n    {<<>>, <<>>}.\n",
    EndPattern = "(?m)(?s)sql\\(_\\)\\s*->.*?(?:\\.$\n?)",
    %% delete end code on first, then replace/append code, append end code on the end
    [{EndPattern, ""}, {Pattern, Code}, {EndPattern, EndCode}];

%% parse per table clean sql
parse_table({File, clean, Table}) ->
    parse_table({File, clean, Table, month});
parse_table({File, clean, Table, day}) ->
    parse_table({File, clean, Table, 86400});
parse_table({File, clean, Table, week}) ->
    parse_table({File, clean, Table, 604800});
parse_table({File, clean, Table, month}) ->
    parse_table({File, clean, Table, 2592000});
parse_table({File, clean, Table, year}) ->
    parse_table({File, clean, Table, 31536000});
parse_table({File, clean, Table, ExpireTime}) ->
    FieldsSql = io_lib:format(<<"SELECT `COLUMN_NAME`, `COLUMN_DEFAULT`, `COLUMN_TYPE`, CASE WHEN `DATA_TYPE` = 'char' THEN '\\'~~s\\'' WHEN `DATA_TYPE` = 'varchar' THEN '\\'~~w\\'' ELSE '~~w' END AS `DATA_TYPE`, `COLUMN_COMMENT`, `ORDINAL_POSITION`, `COLUMN_KEY`, `EXTRA` FROM information_schema.`COLUMNS` WHERE `TABLE_SCHEMA` = DATABASE() AND `TABLE_NAME` = '~s' ORDER BY `ORDINAL_POSITION`;">>, [Table]),
    %% fetch table fields
    AllFields = parser:convert(db:select(FieldsSql), field),
    AllFields == [] andalso error(lists:flatten(io_lib:format("Table: ~s Not Found ", [Table]))),
    %% fetch table fields
    Sql = io_lib:format("DELETE FROM `~s` WHERE `time` < ~~w LIMIT 1000", [Table]),
    %% Pattern = "(?m)\\s*\\]\\.",
    Line = io_lib:format("        {<<\"~s\">>, ~w}", [Sql, ExpireTime]),
    %% read origin sql code
    {ok, Binary} = file:read_file(maker:relative_path(File)),
    %% extract sql list
    {match, [_, SqlData]} = max(re:run(Binary, "(?m)(?s)sql\\(\\)\\s*->\\n*\\s*\\[(.*?)(?=\\]\\s*\\.$)", [{capture, all, list}]), {match, [[], []]}),
    %% one sql per line
    List = string:tokens(string:strip(SqlData), "\n"),
    %% add/replace new sql
    Code = parse_sql_loop(List, "`" ++ atom_to_list(Table) ++ "`", Line, []),
    %% replace new code
    [{"(?m)(?s)sql\\(\\)\\s*->.*?\\.$", "sql() ->\n    [\n" ++ Code ++ "\n    ]."}];
    %% [{"(?m)(?s)sql\\(\\)\\s*->.*?\\.(?=$\\|%\\|\\s*)", "sql() ->\n    [\n" ++ Code ++ "\n    ]."}].

%% parse per table clean sql
parse_table({File, retain, Table}) ->
    parse_table({File, retain, Table, month});
parse_table({File, retain, Table, day}) ->
    parse_table({File, retain, Table, 86400});
parse_table({File, retain, Table, week}) ->
    parse_table({File, retain, Table, 604800});
parse_table({File, retain, Table, month}) ->
    parse_table({File, retain, Table, 2592000});
parse_table({File, retain, Table, year}) ->
    parse_table({File, retain, Table, 31536000});
parse_table({File, retain, Table, ExpireTime}) ->
    %% delete and return data
    DeleteSql = io_lib:format("DELETE FROM `~s` WHERE `time` < ~~w LIMIT 1000 RETURNING *", [Table]),
    %% replace
    FieldsSql = io_lib:format(<<"SELECT `COLUMN_NAME`, `COLUMN_DEFAULT`, `COLUMN_TYPE`, CASE WHEN `DATA_TYPE` = 'char' THEN '\\'~~s\\'' WHEN `DATA_TYPE` = 'varchar' THEN '\\'~~w\\'' ELSE '~~w' END AS `DATA_TYPE`, `COLUMN_COMMENT`, `ORDINAL_POSITION`, `COLUMN_KEY`, `EXTRA` FROM information_schema.`COLUMNS` WHERE `TABLE_SCHEMA` = DATABASE() AND `TABLE_NAME` = '~s' ORDER BY `ORDINAL_POSITION`;">>, [Table]),
    %% fetch table fields
    AllFields = parser:convert(db:select(FieldsSql), field),
    AllFields == [] andalso error(lists:flatten(io_lib:format("Table: ~s Not Found ", [Table]))),
    %% convert type to format
    %% F = fun(<<"char">>) -> "'~s'";(<<"varchar">>) -> "'~w'";(_) -> "~w" end,
    %% AllFields = [[N, D, F(T), C, P, K, E] || [N, D, T, C, P, K, E] <- RawFields],
    InsertFields = string:join([io_lib:format("`~s`", [Name]) || #field{name = Name} <- AllFields], ", "),
    ReplaceFormat = string:join([binary_to_list(Format) || #field{format = Format} <- AllFields], ", "),
    ReplaceSql = io_lib:format("REPLACE INTO `~s` (~s) VALUES ", [Table, InsertFields]),
    %% delete
    %% AutoIncrementFields = string:join([io_lib:format("`~s`", [Name]) || #field{name = Name, extra = Extra} <- AllFields, Extra == <<"auto_increment">>], ", "),
    %% AutoIncrementFormat = string:join([binary_to_list(Format) || #field{format = Format, extra = Extra} <- AllFields, Extra == <<"auto_increment">>], ", "),
    %% DeleteSql = io_lib:format("DELETE FROM `~s` WHERE ~s IN", [Table, AutoIncrementFields]),
    %% Pattern = "(?m)\\s*\\]\\.",
    %% Line = io_lib:format("        {<<\"~s\">>, {<<\"~s\">>, <<\"(~s)\">>, <<\";\">>}, {<<\"~s (\">>, <<\"~s\">>, <<\")\">>}, ~w}", [DeleteSql, ReplaceSql, ReplaceFormat, DeleteSql, AutoIncrementFormat, ExpireTime]),
    Line = io_lib:format("        {<<\"~s\">>, {<<\"~s\">>, <<\"(~s)\">>, <<\";\">>}, ~w}", [DeleteSql, ReplaceSql, ReplaceFormat, ExpireTime]),
    %% read origin sql code
    {ok, Binary} = file:read_file(maker:relative_path(File)),
    %% extract sql list
    {match, [_, SqlData]} = max(re:run(Binary, "(?m)(?s)sql\\(\\)\\s*->\\n*\\s*\\[(.*?)(?=\\]\\s*\\.$)", [{capture, all, list}]), {match, [[], []]}),
    %% one sql per line
    List = string:tokens(string:strip(SqlData), "\n"),
    %% add/replace new sql
    Code = parse_sql_loop(List, "`" ++ atom_to_list(Table) ++ "`", Line, []),
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

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
parse_table(#{file := File, type := log, table := Table}) ->
    %% fetch table fields
    AllFields = parser:convert(db:select(<<"SELECT `COLUMN_NAME`, `COLUMN_DEFAULT`, `COLUMN_TYPE`, CASE WHEN `DATA_TYPE` = 'char' THEN '\\'~~s\\'' WHEN `DATA_TYPE` = 'varchar' THEN '\\'~~w\\'' ELSE '~~w' END AS `DATA_TYPE`, `COLUMN_COMMENT`, `ORDINAL_POSITION`, `COLUMN_KEY`, `EXTRA` FROM information_schema.`COLUMNS` WHERE `TABLE_SCHEMA` = DATABASE() AND `TABLE_NAME` = '~s' ORDER BY `ORDINAL_POSITION`;">>, [Table]), field),
    AllFields == [] andalso erlang:throw(lists:flatten(io_lib:format("Table: ~s Not Found ", [Table]))),
    %% make hump name spec list
    SpecArgs = string:join([lists:concat([word:to_hump(Name), " :: ", case Format of <<"~w">> -> "integer()"; <<"'~s'">> -> "binary()"; _ -> "term()" end]) || #field{name = Name, format = Format, extra = Extra} <- AllFields, Extra =/= <<"auto_increment">>], ", "),
    %% make hump name list
    Args = string:join([word:to_hump(Name) || #field{name = Name, extra = Extra} <- AllFields, Extra =/= <<"auto_increment">>], ", "),
    %% make hump name list and replace zero time
    Value = string:join([word:to_hump(binary_to_list(Name)) || #field{name = Name, extra = Extra} <- AllFields, Extra =/= <<"auto_increment">>], ", "),
    %% {_, E} = re:run(element(2, file:read_file("src/module/log/log.erl")), "-export.*\n?", [global, {capture, all, list}]),
    %% export
    ExportPattern = lists:concat(["-export\\(\\[", Table, "/\\d+\\]\\)\\.\n?|(?<=\n)$"]),
    Export = lists:concat(["-export([", Table, "/", length(AllFields) - 1, "]).\n"]),
    %% extract function list
    {ok, Binary} = file:read_file(maker:relative_path(File)),
    {match, ExportData} = max(re:run(Binary, "-export.*\n?", [global, {capture, all, list}]), {match, [Export]}),
    NewExportData = re:replace(ExportData, ExportPattern, Export, [{return, binary}]),
    %% match replace
    SpecPattern = lists:concat(["(?s)(?m)^-spec ", Table, ".*?\\.$\n?"]),
    Spec = lists:concat(["-spec ", Table, "(", SpecArgs, ") -> ok.\n"]),
    CodePattern = lists:concat(["(?s)(?m)^", Table, ".*?\\.$\n?\n?"]),
    Code = lists:concat([Table, "(", Args, ") ->\n    log_server:log(", Table, ", [", Value, "]).\n\n"]),
    [#{pattern => "-export.*\n?", code => []}, #{pattern => "-module\\(log\\)\\.\n?", code => ["-module(log).\n" | NewExportData]}, #{pattern => SpecPattern, code => Spec}, #{pattern => CodePattern, code => Code}];

%% parse per table sql
parse_table(#{type := save, table := Table}) ->
    %% fetch table fields
    AllFields = parser:convert(db:select(<<"SELECT `COLUMN_NAME`, `COLUMN_DEFAULT`, `COLUMN_TYPE`, CASE WHEN `DATA_TYPE` = 'char' THEN '\\'~~s\\'' WHEN `DATA_TYPE` = 'varchar' THEN '\\'~~w\\'' ELSE '~~w' END AS `DATA_TYPE`, `COLUMN_COMMENT`, `ORDINAL_POSITION`, `COLUMN_KEY`, `EXTRA` FROM information_schema.`COLUMNS` WHERE `TABLE_SCHEMA` = DATABASE() AND `TABLE_NAME` = '~s' ORDER BY `ORDINAL_POSITION`;">>, [Table]), field),
    AllFields == [] andalso erlang:throw(lists:flatten(io_lib:format("Table: ~s Not Found ", [Table]))),
    %% convert type to format
    %% F = fun(<<"char">>) -> "'~s'";(<<"varchar">>) -> "'~w'";(_) -> "~w" end,
    %% AllFields = [[N, D, F(T), C, P, K, E] || [N, D, T, C, P, K, E] <- RawFields],
    InsertFields = string:join([io_lib:format("`~s`", [Name]) || #field{name = Name, extra = Extra} <- AllFields, Extra =/= <<"auto_increment">>], ", "),
    InsertFormat = string:join([binary_to_list(Format) || #field{format = Format, extra = Extra} <- AllFields, Extra =/= <<"auto_increment">>], ", "),
    Sql = io_lib:format("INSERT INTO `~s` (~s) VALUES ", [Table, InsertFields]),
    EndPattern = "(?m)(?s)^sql\\(_\\)\\s*->.*?(?:\\.$\n?)",
    EndCode = "sql(_) ->\n    {<<>>, <<>>}.\n",
    Pattern = io_lib:format("(?s)(?m)sql\\(~s\\).*?;\n?", [Table]),
    Code = io_lib:format("sql(~s) ->\n    {<<\"~s\">>, <<\"(~s)\">>};\n", [Table, Sql, InsertFormat]),
    %% delete end code on first, then replace/append code, append end code on the end
    [#{pattern => EndPattern, code => ""}, #{pattern => Pattern, code => Code}, #{pattern => EndPattern, code => EndCode}];

%% parse per table clean sql
parse_table(Item = #{file := File, type := clean, table := Table, expire_time := ExpireTime}) ->
    ExpireTime = maps:get(expire_time, Item, month),
    CleanExpireTime = maps:get(ExpireTime, #{day => 86400, week => 604800, month => 259200, year => 31536000}, 259200),
    %% fetch table fields
    AllFields = parser:convert(db:select(<<"SELECT `COLUMN_NAME`, `COLUMN_DEFAULT`, `COLUMN_TYPE`, CASE WHEN `DATA_TYPE` = 'char' THEN '\\'~~s\\'' WHEN `DATA_TYPE` = 'varchar' THEN '\\'~~w\\'' ELSE '~~w' END AS `DATA_TYPE`, `COLUMN_COMMENT`, `ORDINAL_POSITION`, `COLUMN_KEY`, `EXTRA` FROM information_schema.`COLUMNS` WHERE `TABLE_SCHEMA` = DATABASE() AND `TABLE_NAME` = '~s' ORDER BY `ORDINAL_POSITION`;">>, [Table]), field),
    AllFields == [] andalso erlang:throw(lists:flatten(io_lib:format("Table: ~s Not Found ", [Table]))),
    %% fetch table fields
    Sql = io_lib:format("DELETE FROM `~s` WHERE `time` < ~~w LIMIT 1000", [Table]),
    %% Pattern = "(?m)\\s*\\]\\.",
    Line = io_lib:format("        {<<\"~s\">>, ~w}", [Sql, CleanExpireTime]),
    %% read origin sql code
    {ok, Binary} = file:read_file(maker:relative_path(File)),
    %% extract sql list
    {match, [_, SqlData]} = max(re:run(Binary, "(?m)(?s)^sql\\(\\)\\s*->\\n*\\s*\\[(.*?)(?=\\]\\s*\\.$)", [{capture, all, list}]), {match, [[], []]}),
    %% one sql per line
    List = string:tokens(string:strip(SqlData), "\n"),
    %% add/replace new sql
    Code = parse_sql_loop(List, "`" ++ atom_to_list(Table) ++ "`", Line, []),
    %% replace new code
    [#{pattern => "(?m)(?s)^sql\\(\\)\\s*->.*?\\.$", code => "sql() ->\n    [\n" ++ Code ++ "\n    ]."}];

%% parse per table clean sql
parse_table(Item = #{file := File, type := retain, table := Table}) ->
    ExpireTime = maps:get(expire_time, Item, month),
    RetainExpireTime = maps:get(ExpireTime, #{day => 86400, week => 604800, month => 259200, year => 31536000}, 259200),
    %% delete and return data
    DeleteSql = io_lib:format("DELETE FROM `~s` WHERE `time` < ~~w LIMIT 1000 RETURNING *", [Table]),
    %% fetch table fields
    AllFields = parser:convert(db:select(<<"SELECT `COLUMN_NAME`, `COLUMN_DEFAULT`, `COLUMN_TYPE`, CASE WHEN `DATA_TYPE` = 'char' THEN '\\'~~s\\'' WHEN `DATA_TYPE` = 'varchar' THEN '\\'~~w\\'' ELSE '~~w' END AS `DATA_TYPE`, `COLUMN_COMMENT`, `ORDINAL_POSITION`, `COLUMN_KEY`, `EXTRA` FROM information_schema.`COLUMNS` WHERE `TABLE_SCHEMA` = DATABASE() AND `TABLE_NAME` = '~s' ORDER BY `ORDINAL_POSITION`;">>, [Table]), field),
    AllFields == [] andalso erlang:throw(lists:flatten(io_lib:format("Table: ~s Not Found ", [Table]))),
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
    Line = io_lib:format("        {<<\"~s\">>, {<<\"~s\">>, <<\"(~s)\">>, <<\";\">>}, ~w}", [DeleteSql, ReplaceSql, ReplaceFormat, RetainExpireTime]),
    %% read origin sql code
    {ok, Binary} = file:read_file(maker:relative_path(File)),
    %% extract sql list
    {match, [_, SqlData]} = max(re:run(Binary, "(?m)(?s)^sql\\(\\)\\s*->\\n*\\s*\\[(.*?)(?=\\]\\s*\\.$)", [{capture, all, list}]), {match, [[], []]}),
    %% one sql per line
    List = string:tokens(string:strip(SqlData), "\n"),
    %% add/replace new sql
    Code = parse_sql_loop(List, "`" ++ atom_to_list(Table) ++ "`", Line, []),
    %% replace new code
    [#{pattern => "(?m)(?s)^sql\\(\\)\\s*->.*?\\.$", code => "sql() ->\n    [\n" ++ Code ++ "\n    ]."}].

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

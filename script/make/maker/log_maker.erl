%%%------------------------------------------------------------------
%%% @doc
%%% module log maker
%%% database fields to sql code tool for log
%%% @end
%%%------------------------------------------------------------------
-module(log_maker).
-export([start/1]).
%%%==================================================================
%%% API functions
%%%==================================================================
%% @doc for shell
start(List) ->
    maker:start(fun parse_table/2, List).

%%%==================================================================
%%% Internal functions
%%%==================================================================
%% parse per table sql
parse_table(DataBase, {_, sql, Table}) ->
    FieldsSql = io_lib:format(<<"SELECT `COLUMN_NAME`, `COLUMN_DEFAULT`, `DATA_TYPE`, `COLUMN_COMMENT`, `ORDINAL_POSITION`, `COLUMN_KEY`, `EXTRA` FROM information_schema.`COLUMNS` WHERE `TABLE_SCHEMA` = '~s' AND `TABLE_NAME` = '~s' ORDER BY `ORDINAL_POSITION`;">>, [DataBase, Table]),
    %% fetch table fields
    RawFields = maker:select(FieldsSql),
    %% convert type to format
    F = fun(<<"char">>) -> "'~s'";(_) -> "'~w'" end,
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
parse_table(DataBase, {File, clean, Table, Time}) ->
    parse_table(DataBase, {File, clean, Table, Time, "daily_time"});
parse_table(DataBase, {_, clean, Table, Time, DailyTime}) ->
    TableSql = io_lib:format(<<"SELECT `TABLE_NAME` FROM information_schema.`TABLES` WHERE `TABLE_SCHEMA` = '~s' AND `TABLE_NAME` = '~s';">>, [DataBase, Table]),
    [[TableName]] = maker:select(TableSql),
    %% fetch table fields
    Sql = io_lib:format("DELETE FROM `~s` WHERE `~s` <= ~~w", [TableName, DailyTime]),
    Pattern = "(?m)\\s*\\]\\.",
    Code = io_lib:format(",\n        {<<\"~s\">>, ~w}\n    ].", [Sql, Time]),
    %% delete end code on first, then replace/append code, append end code on the end
    ReplacePattern = io_lib:format(".*`~s`.*\n", [TableName]),
    [{ReplacePattern, ""}, {Pattern, Code}, {"(?<=\\[)\\s*,", ""}];

%% parse per table log
parse_table(DataBase, {File, log, Table}) ->
    %% default time and daily_time field
    parse_table(DataBase, {File, log, Table, "time", "daily_time"});
parse_table(DataBase, {_, log, Table, Time, DailyTime}) ->
    FieldsSql = io_lib:format(<<"SELECT `COLUMN_NAME`, `COLUMN_DEFAULT`, `DATA_TYPE`, `COLUMN_COMMENT`, `ORDINAL_POSITION`, `COLUMN_KEY`, `EXTRA` FROM information_schema.`COLUMNS` WHERE `TABLE_SCHEMA` = '~s' AND `TABLE_NAME` = '~s' ORDER BY `ORDINAL_POSITION`;">>, [DataBase, Table]),
    %% fetch table fields
    RawFields = maker:select(FieldsSql),
    %% make hump name list
    Args = string:join([maker:hump(Name) || [Name, _, _, _, _, _, E] <- RawFields, E =/= <<"auto_increment">> andalso Name =/= type:to_binary(DailyTime)], ", "),
    %% make hump name list and replace zero time
    FF = fun(Name) -> case string:str(Name, type:to_list(DailyTime)) =/= 0 of true -> lists:flatten(io_lib:format("time:zero(~s)", [maker:hump(Time)])); _ -> maker:hump(Name) end end,
    Value = string:join([FF(binary_to_list(Name)) || [Name, _, _, _, _, _, E] <- RawFields, E =/= <<"auto_increment">>], ", "),
    %% match replace
    Pattern = lists:concat(["(?s)(?m)^", Table, ".*?\\.$\n?"]),
    Code = lists:concat([Table, "(", Args, ") ->\n    log_server:log(", Table, ", [", Value, "]).\n"]),
    [{Pattern, Code}].

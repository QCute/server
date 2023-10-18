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
    maker:start(fun parse_file/1, List).

%%%===================================================================
%%% Internal functions
%%%===================================================================
parse_file(#{type := log, meta := Meta}) ->
    parse_log(Meta, [], []);
parse_file(#{type := save, meta := Meta}) ->
    parse_save(Meta, []);
parse_file(#{type := clean, meta := Meta}) ->
    parse_clean(Meta, []);
parse_file(#{type := retain, meta := Meta}) ->
    parse_retain(Meta, []);
parse_file(Item = #{file := File}) ->
    Type = list_to_atom(lists:concat(string:replace(filename:basename(File, ".erl"), "log_sql_", "", all))),
    parse_file(Item#{type => Type}).

%% parse per table log
parse_log([], ExportList, CodeList) ->
    ExportCode = string:join(lists:reverse(ExportList), "\n"),
    FunctionCode = string:join(lists:reverse(CodeList), "\n"),
    Code = io_lib:format(
"%%%-------------------------------------------------------------------
%%% @doc
%%% log
%%% @end
%%%-------------------------------------------------------------------
-module(log).
~s
%%%===================================================================
%%% API functions
%%%===================================================================
~s
", [ExportCode, FunctionCode]),
    [#{pattern => "(?s).*", code => Code}];
parse_log([#{table := Table} | T], ExportList, CodeList) ->
    %% fetch table fields
    AllFields = parser:convert(db:select(<<"SELECT `COLUMN_NAME`, `COLUMN_DEFAULT`, `COLUMN_TYPE`, CASE WHEN `DATA_TYPE` = 'char' THEN '\\'~~s\\'' WHEN `DATA_TYPE` = 'varchar' THEN '\\'~~w\\'' ELSE '~~w' END AS `DATA_TYPE`, `COLUMN_COMMENT`, `ORDINAL_POSITION`, `COLUMN_KEY`, `EXTRA` FROM information_schema.`COLUMNS` WHERE `TABLE_SCHEMA` = DATABASE() AND `TABLE_NAME` = '~s' ORDER BY `ORDINAL_POSITION`;">>, [Table]), field),
    AllFields == [] andalso erlang:throw(lists:flatten(io_lib:format("Table: ~s Not Found ", [Table]))),
    %% make hump name spec list
    SpecArgs = string:join([lists:concat([word:to_hump(Name), " :: ", case Format of <<"~w">> -> "integer()"; <<"'~s'">> -> "binary()"; _ -> "term()" end]) || #field{name = Name, format = Format, extra = Extra} <- AllFields, Extra =/= <<"auto_increment">>], ", "),
    %% make hump name list
    Args = string:join([word:to_hump(Name) || #field{name = Name, extra = Extra} <- AllFields, Extra =/= <<"auto_increment">>], ", "),
    %% make hump name list and replace zero time
    Value = string:join([word:to_hump(binary_to_list(Name)) || #field{name = Name, extra = Extra} <- AllFields, Extra =/= <<"auto_increment">>], ", "),
    %% export
    Export = lists:concat(["-export([", Table, "/", length(AllFields) - 1, "])."]),
    %% code
    Spec = lists:concat(["-spec ", Table, "(", SpecArgs, ") -> ok.\n"]),
    Code = lists:concat([Spec, Table, "(", Args, ") ->\n    log_server:log(", Table, ", [", Value, "]).\n"]),
    parse_log(T, [Export | ExportList], [Code | CodeList]).

%% parse per table sql
parse_save([], CodeList) ->
    FunctionCode = string:join(lists:reverse(CodeList), "\n"),
    Code = io_lib:format(
"%%%-------------------------------------------------------------------
%%% @doc
%%% log sql save
%%% @end
%%%-------------------------------------------------------------------
-module(log_sql_save).
-export([sql/1]).
%%%===================================================================
%%% API functions
%%%===================================================================
%% @doc the log sql
-spec sql(Key :: atom()) -> {InsertSql :: binary(), ValueFormat :: binary()}.
~s
sql(_) ->
    {<<>>, <<>>}.
", [FunctionCode]),
    [#{pattern => "(?s).*", code => Code}];
parse_save([#{table := Table} | T], CodeList) ->
    %% fetch table fields
    AllFields = parser:convert(db:select(<<"SELECT `COLUMN_NAME`, `COLUMN_DEFAULT`, `COLUMN_TYPE`, CASE WHEN `DATA_TYPE` = 'char' THEN '\\'~~s\\'' WHEN `DATA_TYPE` = 'varchar' THEN '\\'~~w\\'' ELSE '~~w' END AS `DATA_TYPE`, `COLUMN_COMMENT`, `ORDINAL_POSITION`, `COLUMN_KEY`, `EXTRA` FROM information_schema.`COLUMNS` WHERE `TABLE_SCHEMA` = DATABASE() AND `TABLE_NAME` = '~s' ORDER BY `ORDINAL_POSITION`;">>, [Table]), field),
    AllFields == [] andalso erlang:throw(lists:flatten(io_lib:format("Table: ~s Not Found ", [Table]))),
    %% convert type to format
    InsertFields = string:join([io_lib:format("`~s`", [Name]) || #field{name = Name, extra = Extra} <- AllFields, Extra =/= <<"auto_increment">>], ", "),
    InsertFormat = string:join([binary_to_list(Format) || #field{format = Format, extra = Extra} <- AllFields, Extra =/= <<"auto_increment">>], ", "),
    Sql = io_lib:format("INSERT INTO `~s` (~s) VALUES ", [Table, InsertFields]),
    Code = io_lib:format("sql(~s) ->\n    {<<\"~s\">>, <<\"(~s)\">>};", [Table, Sql, InsertFormat]),
    parse_save(T, [Code | CodeList]).

%% parse per table clean sql
parse_clean([], CodeList) ->
    FunctionCode = string:join(lists:reverse(CodeList), ",\n"),
    Code = io_lib:format(
"%%%-------------------------------------------------------------------
%%% @doc
%%% log sql clean
%%% @end
%%%-------------------------------------------------------------------
-module(log_sql_clean).
-export([sql/0]).
%%%===================================================================
%%% API functions
%%%===================================================================
%% @doc the log clean sql
-spec sql() -> [{DeleteSql :: binary(), Time :: non_neg_integer()}].
sql() ->
    [
~s
    ].
", [FunctionCode]),
    [#{pattern => "(?s).*", code => Code}];
parse_clean([Item = #{table := Table} | T], CodeList) ->
    ExpireTime = maps:get(expire_time, Item, month),
    CleanExpireTime = maps:get(ExpireTime, #{day => 86400, week => 604800, month => 259200, year => 31536000}, 259200),
    %% fetch table fields
    AllFields = parser:convert(db:select(<<"SELECT `COLUMN_NAME`, `COLUMN_DEFAULT`, `COLUMN_TYPE`, CASE WHEN `DATA_TYPE` = 'char' THEN '\\'~~s\\'' WHEN `DATA_TYPE` = 'varchar' THEN '\\'~~w\\'' ELSE '~~w' END AS `DATA_TYPE`, `COLUMN_COMMENT`, `ORDINAL_POSITION`, `COLUMN_KEY`, `EXTRA` FROM information_schema.`COLUMNS` WHERE `TABLE_SCHEMA` = DATABASE() AND `TABLE_NAME` = '~s' ORDER BY `ORDINAL_POSITION`;">>, [Table]), field),
    AllFields == [] andalso erlang:throw(lists:flatten(io_lib:format("Table: ~s Not Found ", [Table]))),
    %% fetch table fields
    Sql = io_lib:format("DELETE FROM `~s` WHERE `time` < ~~w LIMIT 1000", [Table]),
    %% Pattern = "(?m)\\s*\\]\\.",
    Code = io_lib:format("        {<<\"~s\">>, ~w}", [Sql, CleanExpireTime]),
    parse_clean(T, [Code | CodeList]).

%% parse per table retain sql
parse_retain([], CodeList) ->
    FunctionCode = string:join(lists:reverse(CodeList), ",\n"),
    Code = io_lib:format(
"%%%-------------------------------------------------------------------
%%% @doc
%%% log sql retain
%%% @end
%%%-------------------------------------------------------------------
-module(log_sql_retain).
-export([sql/0]).
%%%===================================================================
%%% API functions
%%%===================================================================
%% @doc the log retain sql
-spec sql() -> [{DeleteReturningSql :: binary(), {ReplaceSql :: binary(), ValueFormat :: binary(), EndTag :: binary()}, Time :: non_neg_integer()}].
sql() ->
    [
~s
    ].
", [FunctionCode]),
    [#{pattern => "(?s).*", code => Code}];
parse_retain([Item = #{table := Table} | T], CodeList) ->
    ExpireTime = maps:get(expire_time, Item, month),
    RetainExpireTime = maps:get(ExpireTime, #{day => 86400, week => 604800, month => 259200, year => 31536000}, 259200),
    %% delete and return data
    DeleteSql = io_lib:format("DELETE FROM `~s` WHERE `time` < ~~w LIMIT 1000 RETURNING *", [Table]),
    %% fetch table fields
    AllFields = parser:convert(db:select(<<"SELECT `COLUMN_NAME`, `COLUMN_DEFAULT`, `COLUMN_TYPE`, CASE WHEN `DATA_TYPE` = 'char' THEN '\\'~~s\\'' WHEN `DATA_TYPE` = 'varchar' THEN '\\'~~w\\'' ELSE '~~w' END AS `DATA_TYPE`, `COLUMN_COMMENT`, `ORDINAL_POSITION`, `COLUMN_KEY`, `EXTRA` FROM information_schema.`COLUMNS` WHERE `TABLE_SCHEMA` = DATABASE() AND `TABLE_NAME` = '~s' ORDER BY `ORDINAL_POSITION`;">>, [Table]), field),
    AllFields == [] andalso erlang:throw(lists:flatten(io_lib:format("Table: ~s Not Found ", [Table]))),
    %% convert type to format
    InsertFields = string:join([io_lib:format("`~s`", [Name]) || #field{name = Name} <- AllFields], ", "),
    ReplaceFormat = string:join([binary_to_list(Format) || #field{format = Format} <- AllFields], ", "),
    ReplaceSql = io_lib:format("REPLACE INTO `~s` (~s) VALUES ", [Table, InsertFields]),
    Code = io_lib:format("        {<<\"~s\">>, {<<\"~s\">>, <<\"(~s)\">>, <<\";\">>}, ~w}", [DeleteSql, ReplaceSql, ReplaceFormat, RetainExpireTime]),
    parse_retain(T, [Code | CodeList]).

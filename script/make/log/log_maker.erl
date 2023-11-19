%%%-------------------------------------------------------------------
%%% @doc
%%% make database fields to log sql(insert/delete/dump)/code
%%% @end
%%%-------------------------------------------------------------------
-module(log_maker).
-export([start/1]).
-record(field, {table = <<>>, name = <<>>, default = <<>>, type = <<>>, format = <<>>, comment = <<>>, key = <<>>, extra = <<>>, expression = <<>>, position = 0}).
%%%===================================================================
%%% API functions
%%%===================================================================
%% @doc for shell
start(List) ->
    maker:connect_database(),
    TableData = db:select(lists:concat([
        "SELECT `TABLE_NAME` FROM information_schema.`TABLES`", " ",
        "WHERE `TABLE_SCHEMA` = DATABASE() AND `TABLE_NAME` LIKE '%_log'" " ",
        "ORDER BY `TABLE_NAME` ASC"
    ])),

    Tables = [type:to_list(Table) || [Table] <- TableData],
    parse_file(List, Tables).

%%%===================================================================
%%% Internal functions
%%%===================================================================
parse_file([], _) ->
    ok;
parse_file([#{file := File, type := log} | T], Tables) ->
    %% log interface
    Log = parse_log(Tables, [], []),
    filelib:ensure_dir(File),
    file:write_file(File, Log),
    parse_file(T, Tables);
parse_file([#{file := File, type := save} | T], Tables) ->
    %% log interface
    Log = parse_save(Tables, []),
    filelib:ensure_dir(File),
    file:write_file(File, Log),
    parse_file(T, Tables);
parse_file([Preset = #{file := File, type := delete} | T], Tables) ->
    %% log interface
    Log = parse_delete(Tables, Preset, []),
    filelib:ensure_dir(File),
    file:write_file(File, Log),
    parse_file(T, Tables);
parse_file([Preset = #{file := File, type := delete_return} | T], Tables) ->
    %% log interface
    Log = parse_delete_return(Tables, Preset, []),
    filelib:ensure_dir(File),
    file:write_file(File, Log),
    parse_file(T, Tables);
parse_file([Preset = #{file := File, type := replace} | T], Tables) ->
    %% log interface
    Log = parse_replace(Tables, Preset, []),
    filelib:ensure_dir(File),
    file:write_file(File, Log),
    parse_file(T, Tables).

%%%===================================================================
%%% log part
%%%===================================================================

%% parse per table log
parse_log([], ExportList, CodeList) ->
    ExportCode = string:join(lists:reverse(ExportList), "\n"),
    FunctionCode = string:join(lists:reverse(CodeList), "\n"),
    io_lib:format(
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
", [ExportCode, FunctionCode]);
parse_log([Table | T], ExportList, CodeList) ->
    Columns = string:join([
        "`TABLE_NAME`",
        "`COLUMN_NAME`",
        "`COLUMN_DEFAULT`",
        "`COLUMN_TYPE`",
        "`DATA_TYPE`",
        "`COLUMN_COMMENT`",
        "`COLUMN_KEY`",
        "`EXTRA`",
        "`GENERATION_EXPRESSION`",
        "`ORDINAL_POSITION`"
    ], ", "),
    
    Sql = lists:concat([
        "SELECT",
        " ", Columns,
        "FROM",
        " ", "information_schema.`COLUMNS`",
        "WHERE",
        " ", "`TABLE_SCHEMA` = DATABASE()",
        "AND", " ",
        "`TABLE_NAME` = '", Table, "'", " ",
        "ORDER BY",
        " ", "ORDINAL_POSITION", " ", "ASC"
    ]),
    
    %% fetch table fields
    AllFields = parser:convert(db:select(Sql), field, fun(Field = #field{type = Type}) -> Field#field{type = lists:last(binary:split(Type, <<" ">>))} end),
    AllFields == [] andalso erlang:throw(lists:flatten(io_lib:format("Table: ~s Not Found ", [Table]))),
    
    %% make hump name spec list
    SpecArgs = string:join([parse_field(Field) || Field = #field{extra = Extra} <- AllFields, Extra =/= <<"auto_increment">>], ", "),
    
    %% make hump name list
    Args = string:join([word:to_hump(Name) || #field{name = Name, extra = Extra} <- AllFields, Extra =/= <<"auto_increment">>], ", "),
    
    %% make hump name list and replace zero time
    Value = string:join([word:to_hump(binary_to_list(Name)) || #field{name = Name, extra = Extra} <- AllFields, Extra =/= <<"auto_increment">>], ", "),
    
    %% export
    Export = lists:concat(["-export([", Table, "/", length(AllFields) - 1, "])."]),
    
    %% code
    Spec = lists:concat(["-spec ", Table, "(", SpecArgs, ") -> ok.\n"]),
    Code = lists:concat([Spec, Table, "(", Args, ") ->\n    log_server:log(", Table, ", {", Value, "}).\n"]),
    
    parse_log(T, [Export | ExportList], [Code | CodeList]).

parse_field(#field{name = Name, format = <<"boolean">>}) ->
    lists:concat([word:to_hump(Name), " :: ", "boolean()"]);

parse_field(#field{name = Name, format = <<"tinyint">>, type = <<"unsigned">>}) ->
    lists:concat([word:to_hump(Name), " :: ", "non_neg_integer()"]);

parse_field(#field{name = Name, format = <<"smallint">>, type = <<"unsigned">>}) ->
    lists:concat([word:to_hump(Name), " :: ", "non_neg_integer()"]);

parse_field(#field{name = Name, format = <<"int">>, type = <<"unsigned">>}) ->
    lists:concat([word:to_hump(Name), " :: ", "non_neg_integer()"]);

parse_field(#field{name = Name, format = <<"bigint">>, type = <<"unsigned">>}) ->
    lists:concat([word:to_hump(Name), " :: ", "non_neg_integer()"]);

parse_field(#field{name = Name, format = <<"tinyint">>}) ->
    lists:concat([word:to_hump(Name), " :: ", "integer()"]);

parse_field(#field{name = Name, format = <<"smallint">>}) ->
    lists:concat([word:to_hump(Name), " :: ", "integer()"]);

parse_field(#field{name = Name, format = <<"int">>}) ->
    lists:concat([word:to_hump(Name), " :: ", "integer()"]);

parse_field(#field{name = Name, format = <<"bigint">>}) ->
    lists:concat([word:to_hump(Name), " :: ", "integer()"]);

parse_field(#field{name = Name, format = <<"varchar">>}) ->
    lists:concat([word:to_hump(Name), " :: ", "term()"]);

parse_field(#field{name = Name, format = <<"char">>}) ->
    lists:concat([word:to_hump(Name), " :: ", "binary()"]);

parse_field(#field{name = Name, format = <<"decimal">>}) ->
    lists:concat([word:to_hump(Name), " :: ", "float()"]).

%%%===================================================================
%%% save part
%%%===================================================================

%% parse per table sql
parse_save([], CodeList) ->
    FunctionCode = string:join(lists:reverse(CodeList), "\n"),
    io_lib:format(
"%%%-------------------------------------------------------------------
%%% @doc
%%% log save
%%% @end
%%%-------------------------------------------------------------------
-module(log_save).
-export([save/2]).
%%%===================================================================
%%% API functions
%%%===================================================================
%% @doc the log save
-spec save(Key :: atom(), Binding :: list()) -> NewList :: list().
~s
save(_, Binding) ->
    Binding.
", [FunctionCode]);
parse_save([Table | T], CodeList) ->
    Columns = string:join([
        "`TABLE_NAME`",
        "`COLUMN_NAME`",
        "`COLUMN_DEFAULT`",
        "`COLUMN_TYPE`",
        "`DATA_TYPE`",
        "`COLUMN_COMMENT`",
        "`COLUMN_KEY`",
        "`EXTRA`",
        "`GENERATION_EXPRESSION`",
        "`ORDINAL_POSITION`"
    ], ", "),

    Sql = lists:concat([
        "SELECT",
        " ", Columns,
        "FROM",
        " ", "information_schema.`COLUMNS`",
        "WHERE",
        " ", "`TABLE_SCHEMA` = DATABASE()",
        "AND", " ",
        "`TABLE_NAME` = '", Table, "'", " ",
        "ORDER BY",
        " ", "ORDINAL_POSITION", " ", "ASC"
    ]),

    AllFields = parser:convert(db:select(Sql), field, fun(Field = #field{type = Type}) -> Field#field{type = lists:last(binary:split(Type, <<" ">>))} end),
    AllFields == [] andalso erlang:throw(lists:flatten(io_lib:format("Table: ~s Not Found ", [Table]))),
    
    %% convert type to format
    InsertFields = string:join([io_lib:format("`~ts`", [Name]) || #field{name = Name, extra = Extra} <- AllFields, Extra =/= <<"auto_increment">>], ", "),
    InsertFormat = string:join([lists:concat([":", Position - 1, ":"]) || #field{extra = Extra, position = Position} <- AllFields, Extra =/= <<"auto_increment">>], ", "),
    
    SaveSql = io_lib:format("INSERT INTO `~s` (~s) VALUES ", [Table, InsertFields]),
    Code = io_lib:format("save(~s, Binding) ->\n    db:save_into(<<\"~s\">>, <<\"(~s)\">>, <<>>, Binding, 0);", [Table, SaveSql, InsertFormat]),
    
    parse_save(T, [Code | CodeList]).


%%%===================================================================
%%% delete part
%%%===================================================================

%% parse per table clean sql
parse_delete([], _, CodeList) ->
    FunctionCode = string:join(lists:reverse(CodeList), "\n"),
    io_lib:format(
"%%%-------------------------------------------------------------------
%%% @doc
%%% log delete
%%% @end
%%%-------------------------------------------------------------------
-module(log_delete).
-export([delete/1]).
%%%===================================================================
%%% API functions
%%%===================================================================
%% @doc the log delete
-spec delete(Key :: atom()) -> AffectRows :: non_neg_integer().
~s
delete(_) ->
    0.
", [FunctionCode]);
parse_delete([Table | T], Preset, CodeList) ->
    ExpireTime = maps:get(expire_time, Preset, month),
    CleanExpireTime = maps:get(ExpireTime, #{day => 86400, week => 604800, month => 259200, year => 31536000}, 259200),

    Columns = string:join([
        "`TABLE_NAME`",
        "`COLUMN_NAME`",
        "`COLUMN_DEFAULT`",
        "`COLUMN_TYPE`",
        "`DATA_TYPE`",
        "`COLUMN_COMMENT`",
        "`COLUMN_KEY`",
        "`EXTRA`",
        "`GENERATION_EXPRESSION`",
        "`ORDINAL_POSITION`"
    ], ", "),

    Sql = lists:concat([
        "SELECT",
        " ", Columns,
        "FROM",
        " ", "information_schema.`COLUMNS`",
        "WHERE",
        " ", "`TABLE_SCHEMA` = DATABASE()",
        "AND", " ",
        "`TABLE_NAME` = '", Table, "'", " ",
        "ORDER BY",
        " ", "ORDINAL_POSITION", " ", "ASC"
    ]),

    AllFields = parser:convert(db:select(Sql), field, fun(Field = #field{type = Type}) -> Field#field{type = lists:last(binary:split(Type, <<" ">>))} end),
    AllFields == [] andalso erlang:throw(lists:flatten(io_lib:format("Table: ~s Not Found ", [Table]))),
    
    %% fetch table fields
    DeleteSql = io_lib:format("DELETE FROM `~s` WHERE `time` < ~~w LIMIT 1000", [Table]),
    
    %% Pattern = "(?m)\\s*\\]\\.",
    Code = io_lib:format("delete(~s) ->\n    db:delete(<<\"~s\">>, [time:now() - ~w]);", [Table, DeleteSql, CleanExpireTime]),
    
    parse_delete(T, Preset, [Code | CodeList]).


%%%===================================================================
%%% delete return part
%%%===================================================================

%% parse per table retain sql
parse_delete_return([], _, CodeList) ->
    FunctionCode = string:join(lists:reverse(CodeList), "\n"),
    io_lib:format(
"%%%-------------------------------------------------------------------
%%% @doc
%%% log delete return
%%% @end
%%%-------------------------------------------------------------------
-module(log_delete_return).
-export([delete_return/1]).
%%%===================================================================
%%% API functions
%%%===================================================================
%% @doc the log retain sql
-spec delete_return(Key :: atom()) -> NewList :: list().
~s
delete_return(_) ->
    [].
", [FunctionCode]);
parse_delete_return([Table | T], Preset, CodeList) ->
    ExpireTime = maps:get(expire, Preset, month),
    RetainExpireTime = maps:get(ExpireTime, #{day => 86400, week => 604800, month => 259200, year => 31536000}, 259200),
    
    %% delete and return data
    DeleteReturnSql = io_lib:format("DELETE FROM `~s` WHERE `time` < ~~w LIMIT 1000 RETURNING *", [Table]),

    Columns = string:join([
        "`TABLE_NAME`",
        "`COLUMN_NAME`",
        "`COLUMN_DEFAULT`",
        "`COLUMN_TYPE`",
        "`DATA_TYPE`",
        "`COLUMN_COMMENT`",
        "`COLUMN_KEY`",
        "`EXTRA`",
        "`GENERATION_EXPRESSION`",
        "`ORDINAL_POSITION`"
    ], ", "),

    Sql = lists:concat([
        "SELECT",
        " ", Columns,
        "FROM",
        " ", "information_schema.`COLUMNS`",
        "WHERE",
        " ", "`TABLE_SCHEMA` = DATABASE()",
        "AND", " ",
        "`TABLE_NAME` = '", Table, "'", " ",
        "ORDER BY",
        " ", "ORDINAL_POSITION", " ", "ASC"
    ]),

    AllFields = parser:convert(db:select(Sql), field, fun(Field = #field{type = Type}) -> Field#field{type = lists:last(binary:split(Type, <<" ">>))} end),
    AllFields == [] andalso erlang:throw(lists:flatten(io_lib:format("Table: ~s Not Found ", [Table]))),
    %% convert type to format
    %% InsertFields = string:join([io_lib:format("`~s`", [Name]) || #field{name = Name} <- AllFields], ", "),
    %% ReplaceFormat = string:join([binary_to_list(Format) || #field{name = Name, format = Format} <- AllFields], ", "),
    %% ReplaceSql = io_lib:format("REPLACE INTO `~s` (~s) VALUES ", [Table, InsertFields]),
    %% Code = io_lib:format("        {<<\"~s\">>, {<<\"~s\">>, <<\"(~s)\">>, <<\";\">>}, ~w}", [DeleteSql, ReplaceSql, ReplaceFormat, RetainExpireTime]),
    Code = io_lib:format("delete_return(~s) ->\n    db:delete(<<\"~s\">>, [time:now() - ~w]);", [Table, DeleteReturnSql, RetainExpireTime]),
    
    parse_delete_return(T, Preset, [Code | CodeList]).


%%%===================================================================
%%% replace part
%%%===================================================================

%% parse per table retain sql
parse_replace([], _, CodeList) ->
    FunctionCode = string:join(lists:reverse(CodeList), "\n"),
    io_lib:format(
"%%%-------------------------------------------------------------------
%%% @doc
%%% log replace
%%% @end
%%%-------------------------------------------------------------------
-module(log_replace).
-export([replace/2]).
%%%===================================================================
%%% API functions
%%%===================================================================
%% @doc the log replace sql
-spec replace(Key :: atom(), Data :: list()) -> Sql :: binary().
~s
replace(_, _) ->
    <<>>.
", [FunctionCode]);
parse_replace([Table | T], Preset, CodeList) ->
    %% replace data

    Columns = string:join([
        "`TABLE_NAME`",
        "`COLUMN_NAME`",
        "`COLUMN_DEFAULT`",
        "`COLUMN_TYPE`",
        "`DATA_TYPE`",
        "`COLUMN_COMMENT`",
        "`COLUMN_KEY`",
        "`EXTRA`",
        "`GENERATION_EXPRESSION`",
        "`ORDINAL_POSITION`"
    ], ", "),

    Sql = lists:concat([
        "SELECT",
        " ", Columns,
        "FROM",
        " ", "information_schema.`COLUMNS`",
        "WHERE",
        " ", "`TABLE_SCHEMA` = DATABASE()",
        "AND", " ",
        "`TABLE_NAME` = '", Table, "'", " ",
        "ORDER BY",
        " ", "ORDINAL_POSITION", " ", "ASC"
    ]),

    AllFields = parser:convert(db:select(Sql), field, fun(Field = #field{type = Type}) -> Field#field{type = lists:last(binary:split(Type, <<" ">>))} end),
    AllFields == [] andalso erlang:throw(lists:flatten(io_lib:format("Table: ~s Not Found ", [Table]))),
    %% convert type to format
    %% InsertFields = string:join([io_lib:format("`~s`", [Name]) || #field{name = Name} <- AllFields], ", "),
    %% ReplaceFormat = string:join([binary_to_list(Format) || #field{name = Name, format = Format} <- AllFields], ", "),
    %% ReplaceSql = io_lib:format("REPLACE INTO `~s` (~s) VALUES ", [Table, InsertFields]),
    %% Code = io_lib:format("        {<<\"~s\">>, {<<\"~s\">>, <<\"(~s)\">>, <<\";\">>}, ~w}", [DeleteSql, ReplaceSql, ReplaceFormat, RetainExpireTime]),

    InsertFields = string:join([io_lib:format("`~ts`", [Name]) || #field{name = Name} <- AllFields], ", "),
    InsertFormat = string:join([lists:concat([":", Position - 1, ":"]) || #field{position = Position} <- AllFields], ", "),
    Code = io_lib:format("replace(~s, Data) ->\n    db:collect(<<\"REPLACE INTO (~s) VALUES \">>, <<\"~s\">>, <<>>, Data, 0);", [Table, InsertFields, InsertFormat]),

    parse_replace(T, Preset, [Code | CodeList]).

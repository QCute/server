%%%-------------------------------------------------------------------
%%% @doc
%%% make database fields to sql code
%%% @end
%%%-------------------------------------------------------------------
-module(sql_maker).
-export([start/1]).
-record(field, {table = <<>>, name = <<>>, default = <<>>, type = <<>>, format = <<>>, comment = <<>>, key = <<>>, extra = <<>>, position = 0, alias = <<>>, value = <<>>, save = false, preset = #{}, except = false}).
%%%===================================================================
%%% API functions
%%%===================================================================
%% @doc for shell
start(List) ->
    maker:start(fun parse_file/1, List).

%%%===================================================================
%%% Internal functions
%%%===================================================================
%% parse per file
parse_file(#{file := File, sql := Sql}) ->
    Order = maker:collect_map_order("script/make/sql/sql_script.erl", File),

    %% collect into or from table
    Tables = listing:unique([maps:get(into, Sets, maps:get(from, Sets, undefined)) || Sets <- Sql, is_map_key(into, Sets) orelse is_map_key(from, Sets)]),
    Includes = string:join(maker:collect_include(Tables), ""),

    %% the main function
    length(Sql) =/= length(Order) andalso erlang:throw(lists:flatten(io_lib:format("Sql has ~w item, but form has ~w item, check the comma if exists between two maps", [length(Sql), length(Order)]))),
    Sets = [parse_sql(File, Sets, MapOrder) || {Sets, MapOrder} <- lists:zip(Sql, Order)],

    Exports = string:join([lists:concat([Export, "\n"]) || #{export := Export} <- Sets], ""),

    Function = string:join([Code || #{code := Code} <- Sets], "\n\n"),

    Code = lists:concat([
        "-module(", filename:basename(File, ".erl"), ").", "\n",
        Exports,
        Includes,
        "\n",
        Function,
        "\n"
    ]),

    %% replace
    [#{pattern => "(?s).*", code => Code}].

%% insert sql with filter
parse_sql(File, SQL = #{insert := Insert, into := Table, filter := Filter, as := FunctionName}, Order) ->
    %% convert table_name to TableName
    HumpName = word:to_hump(Table),
    Fields = collect_fields(File, SQL, Table),

    PresetFields = collect_preset_fields(File, SQL, Table, insert, Insert, Fields, maps:get(except, SQL, []), []),

    ValueFields = parse_value(File, SQL, Table, insert, PresetFields, Fields, maps:get(insert, Order, []), []),

    %% args and spec
    Args = string:join([lists:concat([db:to_snake(Alias), " = ", db:to_hump(Alias)]) || #field{alias = Alias} <- ValueFields], ", "),
    Params = string:join([db:to_hump(Alias) || #field{alias = Alias} <- ValueFields], ", "),

    %% format names INSERT INTO `table` (`key`, `value`, ...) ...
    Names = string:join([type:to_list(Alias) || #field{alias = Alias} <- ValueFields], ", "),
    %% format values INSERT INTO `table` ... VALUES (~w, '~s', ~p, ...)
    Values = string:join([type:to_list(Value) || #field{value = Value} <- ValueFields], ", "),

    %% format duplicate key resolve method
    %% INSERT IGNORE ... 
    Ignore = parse_duplicate_ignore(SQL, Fields),
    %% INSERT INTO ... ON DUPLICATE KEY UPDATE `key` = VALUES(`key`), ...
    Update = parse_duplicate_update(SQL, Fields),

    %% concat data
    List = [
        "INSERT", Ignore, "INTO",
        lists:concat(["`", Table, "`"]), lists:concat(["(", Names, ")"]),
        "VALUES"
    ],

    %% remove unset condition
    Result = string:join([I || I <- List, I =/= []], " "),

    Export = lists:concat(["-export([", FunctionName, "/1])."]),

    %% format code
    Code = lists:concat([
        "%% @doc insert into ", Table, "\n",
        "-spec ", FunctionName, "(", HumpName, "List :: [#", Table, "{}] | ets:tab()) -> New", HumpName, "List :: [#", Table, "{}].", "\n",
        FunctionName, "(", HumpName, "List) ->", "\n",
        "    ", "db:save(<<\"", Result, "\">>, ", "<<\"", lists:concat(["(", Values, ")"]), "\">>, ", "<<\"", Update, "\">>, ", HumpName, "List, ", "fun(#", Table, "{", Args, "}) -> [", Params, "] end, ", "#", Table, ".", Filter, ")."
    ]),

    #{export => Export, code => Code};

%% insert sql without filter
parse_sql(File, SQL = #{insert := Insert, into := Table, as := FunctionName}, Order) ->
    %% convert table_name to TableName
    HumpName = word:to_hump(Table),
    Fields = collect_fields(File, SQL, Table),

    PresetFields = collect_preset_fields(File, SQL, Table, insert, Insert, Fields, maps:get(except, SQL, []), []),

    ValueFields = parse_value(File, SQL, Table, insert, PresetFields, Fields, maps:get(insert, Order, []), []),

    %% args and spec
    Args = string:join([lists:concat([db:to_snake(Alias), " = ", db:to_hump(Alias)]) || #field{alias = Alias} <- ValueFields], ", "),
    Params = string:join([db:to_hump(Alias) || #field{alias = Alias} <- ValueFields], ", "),

    %% format names INSERT INTO `table` (`key`, `value`, ...) ...
    Names = string:join([type:to_list(Alias) || #field{alias = Alias} <- ValueFields], ", "),
    %% format values INSERT INTO `table` ... VALUES (~w, '~s', ~p, ...)
    Values = string:join([type:to_list(Value) || #field{value = Value} <- ValueFields], ", "),

    %% format duplicate key resolve method
    %% INSERT IGNORE ... 
    Ignore = parse_duplicate_ignore(SQL, Fields),
    %% INSERT INTO ... ON DUPLICATE KEY UPDATE `key` = VALUES(`key`), ...
    Update = parse_duplicate_update(SQL, Fields),

    %% concat data
    List = [
        "INSERT", Ignore, "INTO",
        lists:concat(["`", Table, "`"]), lists:concat(["(", Names, ")"]),
        "VALUES", lists:concat(["(", Values, ")"]),
        Update
    ],

    %% remove unset condition
    Result = string:join([I || I <- List, I =/= []], " "),

    Export = lists:concat(["-export([", FunctionName, "/1])."]),

    %% format code
    Code = lists:concat([
        "%% @doc insert into ", Table, "\n",
        "-spec ", FunctionName, "(", HumpName, " :: #", Table, "{}) -> InsertIdOrAffectedRows :: non_neg_integer().", "\n",
        FunctionName, "(#", Table, "{", Args, "}) ->", "\n",
        "    ", "db:insert(<<\"", Result, "\">>, [", Params, "])."
    ]),

    #{export => Export, code => Code};

%% select sql without join
parse_sql(File, SQL = #{select := Select, from := Table, as := FunctionName}, Order) ->
    %% convert table_name to TableName
    %% HumpName = word:to_hump(Table),
    Fields = collect_fields(File, SQL, Table),

    KeyFields = parse_key(File, SQL, Table, select, Fields, maps:get(by, Order, []), maps:get(having, Order, [])),
    Spec = string:join([lists:concat([type:to_list(Alias), " :: ", Type]) || #field{alias = Alias, value = Type} <- KeyFields], ", "),
    Arg = string:join([type:to_list(Alias) || #field{alias = Alias} <- KeyFields], ", "),

    %% format condition
    Where = parse_where(File, SQL, Table, select, Fields, maps:get(by, Order, [])),
    GroupBy = parse_group_by(File, SQL, Table, Fields),
    Having = parse_having(File, SQL, Table, select, Fields, maps:get(having, Order, [])),
    OrderBy = parse_order_by(File, SQL, Table, Fields, maps:get(order_by, Order, [])),
    Limit = parse_limit(File, SQL, Table, Fields),
    Offset = parse_offset(File, SQL, Table, Fields),

    PresetFields = collect_preset_fields(File, SQL, Table, select, Select, Fields, maps:get(except, SQL, []), []),

    ValueFields = parse_value(File, SQL, Table, select, PresetFields, Fields, maps:get(select, Order, []), []),

    %% parse join
    Join = parse_join(SQL, Table, Fields),

    %% format names SELECT `key`, `value`, ...
    TableFields = string:join([type:to_list(Value) || #field{value = Value} <- ValueFields], ", "),
    %% format convert key = parser:to_term(Key), ...
    Convert = parse_convert(SQL, Table, ValueFields, [], []),

    %% concat data
    List = [
        "SELECT", TableFields, "FROM",
        lists:concat(["`", Table, "`"]),
        Join,
        Where,
        GroupBy,
        Having,
        OrderBy,
        Limit,
        Offset
    ],

    %% remove unset condition
    Result = string:join([I || I <- List, I =/= []], " "),

    Export = lists:concat(["-export([", FunctionName, "/", length(KeyFields), "])."]),

    %% format code
    Code = lists:concat([
        "%% @doc select from ", Table, "\n",
        "-spec ", FunctionName, "(", Spec, ") -> Rows :: [#", Table, "{}].", "\n",
        FunctionName, "(", Arg, ") ->", "\n",
        "    ", "Data = db:select(<<\"", Result, "\">>, [", Arg, "]),", "\n",
        "    ", "parser:convert(Data, ", Table, Convert, ")."
    ]),

    #{export => Export, code => Code};

parse_sql(File, SQL = #{update := Update, into := Table, as := FunctionName}, Order) ->
    %% convert table_name to TableName
    %% HumpName = word:to_hump(Table),
    Fields = collect_fields(File, SQL, Table),

    KeyFields = parse_key(File, SQL, Table, update, Fields, maps:get(by, Order, []), maps:get(having, Order, [])),

    %% parse condition
    Where = parse_where(File, SQL, Table, update, Fields, maps:get(by, Order, [])),
    GroupBy = parse_group_by(File, SQL, Table, Fields),
    Having = parse_having(File, SQL, Table, update, Fields, maps:get(having, Order, [])),
    OrderBy = parse_order_by(File, SQL, Table, Fields, maps:get(order_by, Order, [])),
    Limit = parse_limit(File, SQL, Table, Fields),
    Offset = parse_offset(File, SQL, Table, Fields),

    PresetKeyFields = [type:to_atom(Name) || #field{name = Name} <- KeyFields],
    AllPresetFields = lists:flatten([collect_preset_fields(File, SQL, Table, update, Update, Fields, [], []), collect_preset_fields(File, SQL, Table, update, PresetKeyFields, Fields, [], [])]),
    PresetFields = collect_preset_fields(File, SQL, Table, update, Update, Fields, maps:get(except, SQL, []), []),

    AllValueFields = parse_value(File, SQL, Table, update, AllPresetFields, Fields, maps:get(update, Order, []), []),
    ValueFields = parse_value(File, SQL, Table, update, PresetFields, Fields, maps:get(update, Order, []), []),

    %% parse join
    Join = parse_join(SQL, Table, Fields),

    %% args and spec
    Args = string:join([lists:concat([db:to_snake(Alias), " = ", db:to_hump(Alias)]) || #field{alias = Alias} <- AllValueFields], ", "),
    Params = string:join([db:to_hump(Alias) || #field{alias = Alias} <- AllValueFields], ", "),

    %% format values UPDATE `table` SET `key` = ~w, `value` = '~s', ...
    Updates = string:join([lists:concat([type:to_list(Alias), " = ", type:to_list(Value)]) || #field{alias = Alias, value = Value} <- ValueFields], ", "),

    %% concat data
    List = [
        "UPDATE", lists:concat(["`", Table, "`"]),
        Join,
        "SET", Updates,
        Where,
        GroupBy,
        Having,
        OrderBy,
        Limit,
        Offset
    ],

    %% remove unset condition
    Result = string:join([I || I <- List, I =/= []], " "),

    Export = lists:concat(["-export([", FunctionName, "/1])."]),

    %% format code
    Code = lists:concat([
        "%% @doc update into ", Table, "\n",
        "-spec ", FunctionName, "(#", Table, "{}) -> AffectedRows :: non_neg_integer().", "\n",
        FunctionName, "(#", Table, "{", Args, "}) ->", "\n",
        "    ", "db:update(<<\"", Result, "\">>, [", Params, "])."
    ]),

    #{export => Export, code => Code};

parse_sql(File, SQL = #{change := Change, into := Table, as := FunctionName}, Order) ->
    %% convert table_name to TableName
    %% HumpName = word:to_hump(Table),
    Fields = collect_fields(File, SQL, Table),

    KeyFields = parse_key(File, SQL, Table, update, Fields, maps:get(by, Order, []), maps:get(having, Order, [])),
    Spec = string:join([lists:concat(["By", Alias, " :: ", Type]) || #field{alias = Alias, value = Type} <- KeyFields], ", "),
    Arg = string:join([lists:concat(["By", Alias]) || #field{alias = Alias} <- KeyFields], ", "),

    %% parse condition
    Where = parse_where(File, SQL, Table, update, Fields, maps:get(by, Order, [])),
    GroupBy = parse_group_by(File, SQL, Table, Fields),
    Having = parse_having(File, SQL, Table, update, Fields, maps:get(having, Order, [])),
    OrderBy = parse_order_by(File, SQL, Table, Fields, maps:get(order_by, Order, [])),
    Limit = parse_limit(File, SQL, Table, Fields),
    Offset = parse_offset(File, SQL, Table, Fields),

    PresetFields = collect_preset_fields(File, SQL, Table, update, Change, Fields, maps:get(except, SQL, []), []),

    ValueFields = parse_value(File, SQL, Table, update, PresetFields, Fields, maps:get(update, Order, []), []),

    %% parse join
    Join = parse_join(SQL, Table, Fields),

    %% args and spec
    Args = string:join([lists:concat([db:to_snake(Alias), " = ", db:to_hump(Alias)]) || #field{alias = Alias} <- ValueFields], ", "),
    Params = string:join([db:to_hump(Alias) || #field{alias = Alias} <- ValueFields], ", "),

    %% format values UPDATE `table` SET `key` = ~w, `value` = '~s', ...
    Updates = string:join([lists:concat([type:to_list(Alias), " = ", type:to_list(Value)]) || #field{alias = Alias, value = Value} <- ValueFields], ", "),

    %% concat data
    List = [
        "UPDATE", lists:concat(["`", Table, "`"]),
        Join,
        "SET", Updates,
        Where,
        GroupBy,
        Having,
        OrderBy,
        Limit,
        Offset
    ],

    %% remove unset condition
    Result = string:join([I || I <- List, I =/= []], " "),

    Export = lists:concat(["-export([", FunctionName, "/", length(KeyFields) + 1, "])."]),

    %% format code
    Code = lists:concat([
        "%% @doc update into ", Table, "\n",
        "-spec ", FunctionName, "(#", Table, "{}", ", ", Spec, ") -> AffectedRows :: non_neg_integer().", "\n",
        FunctionName, "(#", Table, "{", Args, "}", ", ", Arg, ") ->", "\n",
        "    ", "db:update(<<\"", Result, "\">>, [", Params, ", ", Arg, "])."
    ]),

    #{export => Export, code => Code};

parse_sql(File, SQL = #{delete := [], from := Table, as := FunctionName}, Order) ->
    %% convert table_name to TableName
    %% HumpName = word:to_hump(Table),
    Fields = collect_fields(File, SQL, Table),

    Keys = parse_key(File, SQL, Table, delete, Fields, maps:get(by, Order, []), maps:get(having, Order, [])),
    Spec = string:join([lists:concat([type:to_list(Alias), " :: ", type:to_list(Type)]) || #field{alias = Alias, value = Type} <- Keys], ", "),
    Arg = string:join([type:to_list(Alias) || #field{alias = Alias} <- Keys], ", "),

    %% parse condition
    Where = parse_where(File, SQL, Table, delete, Fields, maps:get(by, Order, [])),
    GroupBy = parse_group_by(File, SQL, Table, Fields),
    Having = parse_having(File, SQL, Table, delete, Fields, maps:get(having, Order, [])),
    OrderBy = parse_order_by(File, SQL, Table, Fields, maps:get(order_by, Order, [])),
    Limit = parse_limit(File, SQL, Table, Fields),
    Offset = parse_offset(File, SQL, Table, Fields),

    %% parse join
    Join = parse_join(SQL, Table, Fields),

    %% concat data
    List = [
        "DELETE", "FROM", lists:concat(["`", Table, "`"]),
        Join,
        Where,
        GroupBy,
        Having,
        OrderBy,
        Limit,
        Offset
    ],

    %% remove unset condition
    Result = string:join([I || I <- List, I =/= []], " "),

    Export = lists:concat(["-export([", FunctionName, "/", length(Keys), "])."]),

    %% format code
    Code = lists:concat([
        "%% @doc delete row from ", Table, "\n",
        "-spec ", FunctionName, "(", Spec, ") -> AffectedRows :: non_neg_integer().", "\n",
        FunctionName, "(", Arg, ") ->", "\n",
        "    ", "db:delete(<<\"", Result, "\">>, [", Arg, "])."
    ]),

    #{export => Export, code => Code};

parse_sql(_, #{truncate := Table, as := FunctionName}, _) ->
    %% convert table_name to TableName
    HumpName = word:to_hump(Table),

    Export = lists:concat(["-export([", FunctionName, "/0])."]),

    %% format code
    Code = lists:concat([
        "%% @doc truncate ", Table, "\n",
        "-spec ", FunctionName, "(", HumpName, " :: #", Table, "{}) -> AffectedRows :: non_neg_integer().", "\n",
        FunctionName, "(", HumpName, ") ->", "\n",
        "    ", "db:query(<<\"TRUNCATE `", Table, "`\">>, ", HumpName, ")."
    ]),

    #{export => Export, code => Code};

parse_sql(File, SQL = #{insert := _, from := _}, _) ->
    Base = filename:basename(File),
    erlang:throw(lists:flatten(io_lib:format("insert table use `into` instead `from`: ~tw in: ~ts", [SQL, Base])));

parse_sql(File, SQL = #{insert := _}, _) ->
    Base = filename:basename(File),
    erlang:throw(lists:flatten(io_lib:format("insert operation missing `into`: ~tw in: ~ts", [SQL, Base])));

parse_sql(File, SQL = #{select := _, into := _}, _) ->
    Base = filename:basename(File),
    erlang:throw(lists:flatten(io_lib:format("select table use `from` instead `into`: ~tw in: ~ts", [SQL, Base])));

parse_sql(File, SQL = #{select := _}, _) ->
    Base = filename:basename(File),
    erlang:throw(lists:flatten(io_lib:format("select operation missing `from`: ~tw in: ~ts", [SQL, Base])));

parse_sql(File, SQL = #{update := _, from := _}, _) ->
    Base = filename:basename(File),
    erlang:throw(lists:flatten(io_lib:format("update table use `into` instead `from`: ~tw in: ~ts", [SQL, Base])));

parse_sql(File, SQL = #{update := _}, _) ->
    Base = filename:basename(File),
    erlang:throw(lists:flatten(io_lib:format("update operation mssing `into`: ~tw in: ~ts", [SQL, Base])));

parse_sql(File, SQL = #{delete := _, from := _}, _) ->
    Base = filename:basename(File),
    erlang:throw(lists:flatten(io_lib:format("delete table use `from` instead `into`: ~tw in: ~ts", [SQL, Base])));

parse_sql(File, SQL = #{delete := _}, _) ->
    Base = filename:basename(File),
    erlang:throw(lists:flatten(io_lib:format("delete operation msssing `from`: ~tw in: ~ts", [SQL, Base])));

parse_sql(File, SQL = #{}, _) when not is_map_key(as, SQL) ->
    Base = filename:basename(File),
    Operate = maps:get(insert, SQL, maps:get(select, SQL, maps:get(update, SQL, maps:get(delete, SQL, maps:get(truncate, SQL, undefined))))),
    erlang:throw(lists:flatten(io_lib:format("Could not found ~ts name from: ~tw in ~ts, use as => your_name to setup it", [Operate, SQL, Base]))).

%%%===================================================================
%%% collect fields part
%%%===================================================================
collect_fields(File, SQL, Table) ->

    Alias = maps:get(is_map_key(join, SQL), #{true => "CONCAT(`TABLE_NAME`, '.', `COLUMN_NAME`) AS `ALIAS`", false => "`COLUMN_NAME` AS `ALIAS`"}),

    Columns = string:join([
        "`TABLE_NAME`",
        "`COLUMN_NAME`",
        "`COLUMN_DEFAULT`",
        "`COLUMN_TYPE`",
        "`DATA_TYPE`",
        "`COLUMN_COMMENT`",
        "`COLUMN_KEY`",
        "`EXTRA`",
        "`ORDINAL_POSITION`",
        Alias,
        "'' AS `VALUE`",
        "'' AS `SAVE`",
        "'' AS `PRESET`",
        "'' AS `EXCEPT`"
    ], ", "),

    %% join table fields
    Tables = string:join([lists:concat(["`TABLE_NAME`", " = ", "'", Name, "'"]) || Name <- [Table | maps:keys(maps:get(join, SQL, #{}))]], " OR "),

    Sql = lists:concat([
        "SELECT",
        " ", Columns,
        "FROM",
        " ", "information_schema.`COLUMNS`",
        "WHERE",
        " ", "`TABLE_SCHEMA` = DATABASE()",
        "AND", " ",
        "(", Tables, ")",
        " ",
        "ORDER BY",
        " ", "ORDINAL_POSITION", " ", "ASC"
    ]),

    %% collect table fields
    Fields = parser:convert(db:select(Sql), field, fun(Field = #field{type = Type}) -> Field#field{type = lists:last(binary:split(Type, <<" ">>))} end),

    %% table not exists
    Base = filename:basename(File),
    length(Fields) == 0 andalso erlang:throw(lists:flatten(io_lib:format("Counot not found table `~s` in: ~ts", [Table, Base]))),

    Fields.


%% use default
collect_preset_fields(_, _, _, _, [], [], _, List) ->
    lists:reverse(List);

%% use preset
collect_preset_fields(_, _, _, _, Preset, [], _, _) ->
    Preset;

%% collect without except
collect_preset_fields(File, SQL, Table, Operation, Preset, [#field{table = Target, alias = Alias} | Fields], [], List) ->
    case Target == type:to_binary(Table) of
        true ->
            collect_preset_fields(File, SQL, Table, Operation, Preset, Fields, [], [type:to_atom(Alias) | List]);
        _ ->
            collect_preset_fields(File, SQL, Table, Operation, Preset, Fields, [], List)
    end;

%% collect without except
collect_preset_fields(File, SQL, Table, Operation, Preset, [#field{table = Target, name = Name, alias = Alias} | Fields], Except, List) when is_list(Except) andalso length(Except) > 0 andalso (is_atom(hd(Except)) orelse is_list(hd(Except))) ->
    case Target == type:to_binary(Table) andalso lists:all(fun(Item) -> type:to_list(Name) =/= type:to_list(Item) end, Except) of
        true ->
            collect_preset_fields(File, SQL, Table, Operation, Preset, Fields, Except, [type:to_atom(Alias) | List]);
        _ ->
            collect_preset_fields(File, SQL, Table, Operation, Preset, Fields, Except, List)
    end;

%% collect without except
collect_preset_fields(File, SQL, Table, Operation, Preset, [#field{table = Target, name = Name, alias = Alias} | Fields], Except, List) when is_atom(Except) ->
    case Target == type:to_binary(Table) andalso type:to_list(Name) =/= type:to_list(Except) of
        true ->
            collect_preset_fields(File, SQL, Table, Operation, Preset, Fields, Except, [type:to_atom(Alias) | List]);
        _ ->
            collect_preset_fields(File, SQL, Table, Operation, Preset, Fields, Except, List)
    end.

%%%===================================================================
%%% collect fields type/format/name part
%%%===================================================================

%% order end
parse_value(_, _, _, _, #{}, _, [], Values) ->
    lists:flatten(lists:reverse(Values));

%% list end
parse_value(_, _, _, _, [], _, _, Values) ->
    lists:flatten(lists:reverse(Values));

%% map
parse_value(File, SQL, Table, Operation, Preset = #{}, Fields, [Name | Order], Values) ->
    Field = parse_field_value(File, SQL, Table, Operation, Fields, Name, maps:get(Name, Preset)),
    parse_value(File, SQL, Table, Operation, Preset, Fields, Order, [Field | Values]);

%% list
parse_value(File, SQL, Table, Operation, [Name | Preset], Fields, Order, Values) when is_atom(Name) orelse is_list(Name) orelse is_binary(Name) ->
    Field = parse_field_value(File, SQL, Table, Operation, Fields, Name, Name),
    parse_value(File, SQL, Table, Operation, Preset, Fields, Order, [Field | Values]);

%% atom
parse_value(File, SQL, Table, Operation, Preset, Fields, _, Values) ->
    Field = parse_field_value(File, SQL, Table, Operation, Fields, Preset, Preset),
    lists:flatten([Field | Values]).


%% collect convert
parse_convert(#{}, _, [], [], []) ->
    lists:concat([]);

parse_convert(#{}, Table, [], Matches, Converts) ->
    HumpName = word:to_hump(Table),
    Match = string:join(lists:reverse(Matches), ", "),
    Convert = string:join(lists:reverse(Converts), ", "),
    lists:concat([", ", "fun(", HumpName, " = ", "#", Table, "{", Match, "}) ->", " ", HumpName, "#", Table, "{", Convert, "}", " ", "end"]);

%% ignore virtual column
parse_convert(SQL = #{}, Table, [#field{extra = <<"VIRTUAL", _/binary>>} | Fields], Matches, Converts) ->
    parse_convert(SQL, Table, Fields, Matches, Converts);

%% except spec column
parse_convert(SQL = #{}, Table, [#field{save = false} | Fields], Matches, Converts) ->
    parse_convert(SQL, Table, Fields, Matches, Converts);

%% the convert column
parse_convert(SQL = #{}, Table, [#field{name = Name, format = <<"varchar">>} | Fields], Matches, Converts) ->
    %% format the match fun(#record{field = Field})
    Match = lists:concat([type:to_list(Name), " = ", word:to_hump(Name)]),
    %% format the convert #record{field = parser:convert(Field)}
    Convert = lists:concat([type:to_list(Name), " = ", "parser:to_term(", word:to_hump(Name), ")"]),
    parse_convert(SQL, Table, Fields, [Match | Matches], [Convert | Converts]);

%% other data column
parse_convert(SQL = #{}, Table, [#field{} | Fields], Matches, Converts) ->
    parse_convert(SQL, Table, Fields, Matches, Converts).


%%%===================================================================
%%% duplicate ignore part
%%%===================================================================

%% with ignore
parse_duplicate_ignore(#{duplicate := Ignore = ignore}, _) ->
    lists:concat([string:to_upper(Ignore)]);

%% without ignore
parse_duplicate_ignore(#{}, _) ->
    lists:concat([]).

%%%===================================================================
%%% duplicate update part
%%%===================================================================

%% with update
parse_duplicate_update(#{duplicate := update}, _) ->
    lists:concat([]);

%% without update
parse_duplicate_update(#{}, _) ->
    lists:concat([]).

%%%===================================================================
%%% join part
%%%===================================================================
%% @todo check join
%% with join
parse_join(#{join := Join}, LocalTable, _) ->
    parse_join_table(maps:to_list(Join), LocalTable, []);

parse_join(#{}, _, _) ->
    lists:concat([]).

%% join table, ...
parse_join_table([], _, List) ->
    string:join(lists:reverse(List), " ");

parse_join_table([{ForeignTable, Pairs} | Preset], LocalTable, List) ->
    Condition = parse_join_condition(maps:to_list(Pairs), ForeignTable, LocalTable, []),
    Join = lists:concat(["INNER JOIN", " `", ForeignTable, "` ", Condition]),
    parse_join_table(Preset, LocalTable, [Join | List]).

%% with join table on ... and ...
parse_join_condition([], _, _, List) ->
    string:join(lists:reverse(List), " ");

%% the first condition concat with on
parse_join_condition([{Foreign, Local} | Preset], ForeignTable, LocalTable, []) ->
    On = lists:concat(["ON", " ", "`", ForeignTable, "`.`", Foreign, "`", " = ", "`", LocalTable, "`", ".", "`", Local, "`"]),
    parse_join_condition(Preset, ForeignTable, LocalTable, [On]);

%% other condition concat with and
parse_join_condition([{Foreign, Local} | Preset], ForeignTable, LocalTable, List) ->
    And = lists:concat(["AND", " ", "`", ForeignTable, "`.`", Foreign, "`", " = ", "`", LocalTable, "`", ".", "`", Local, "`"]),
    parse_join_condition(Preset, ForeignTable, LocalTable, [And | List]).

%%%===================================================================
%%% where part
%%%===================================================================

%% spec where
parse_where(File, SQL = #{by := Preset = #{}}, Table, Operation, Fields, Order) ->
    %% Condition = [parse_where_compare(File, Table, Name, Compare, Fields) || {Name, Compare} <- maps:to_list(Preset)],
    Condition = [parse_where_compare(File, SQL, Table, Operation, Name, maps:get(Name, Preset), Fields) || Name <- Order],
    lists:concat(["WHERE", " ", string:join(Condition, " AND ")]);

%% multi column
parse_where(File, SQL = #{by := Preset = [_ | _]}, Table, Operation, Fields, _) when is_atom(hd(Preset)) orelse is_list(hd(Preset)) orelse is_binary(hd(Preset)) ->
    Condition = [lists:concat(["`", Name, "`", " = ", (parse_field_format(File, SQL, Table, Operation, Fields, Name))#field.value]) || Name <- Preset],
    lists:concat(["WHERE",  " ", string:join(Condition, " AND ")]);

%% single column
parse_where(File, SQL = #{by := Preset}, Table, Operation, Fields, _) when is_atom(Preset) orelse is_list(Preset) orelse is_binary(Preset) ->
    #field{alias = Alias, value = Format} = parse_field_format(File, SQL, Table, Operation, Fields, Preset),
    lists:concat(["WHERE", " ", type:to_list(Alias), " = ", type:to_list(Format)]);

%% without where
parse_where(_, #{}, _, _, _, _) ->
    lists:concat([]).


%% spec compare or literal
parse_where_compare(File, SQL, Table, Operation, Name, Preset = #{}, Fields) ->
    Condition = [parse_where_compare_literal(File, SQL, Table, Operation, Name, Compare, Value, Fields) || {Compare, Value} <- maps:to_list(Preset)],
    string:join(Condition, " AND ");

%% compare only
parse_where_compare(File, SQL, Table, Operation, Name, Compare, Fields) ->
    #field{alias = Alias, value = Format} = parse_field_format(File, SQL, Table, Operation, Fields, Name),
    lists:concat([type:to_list(Alias), " ", string:to_upper(type:to_list(Compare)), " ", type:to_list(Format)]).


%% not literal
parse_where_compare_literal(File, SQL, Table, Operation, Name, Compare, {'$param$', []}, Fields) ->
    #field{alias = Alias, value = Format} = parse_field_format(File, SQL, Table, Operation, Fields, Name),
    lists:concat([type:to_list(Alias), " ", string:to_upper(type:to_list(Compare)), " ", type:to_list(Format)]);

%% literal
parse_where_compare_literal(File, SQL, Table, Operation, Name, Compare, Literal, Fields) ->
    #field{alias = Alias} = parse_field_format(File, SQL, Table, Operation, Fields, Name),
    lists:concat([type:to_list(Alias), " ", string:to_upper(type:to_list(Compare)), " ", "'", Literal, "'"]).

%%%===================================================================
%%% group by part
%%%===================================================================

%% group by multi column
parse_group_by(_, #{group_by := Preset = [_ | _]}, _, _) ->
    GroupBy = [lists:concat(["`", Field, "`"]) || Field <- maps:to_list(Preset)],
    lists:concat(["GROUP BY ", string:join(GroupBy, ", ")]);

%% group by single column
parse_group_by(_, #{group_by := Field}, _, _) ->
    lists:concat(["GROUP BY `", Field, "`"]);

%% without group
parse_group_by(_, #{}, _, _) ->
    lists:concat([]).

%%%===================================================================
%%% having part
%%%===================================================================

%% spec having
parse_having(File, SQL = #{having := Preset = #{}}, Table, Operation, Fields, Order) ->
    %% Condition = [parse_having_compare(File, Table, Name, Compare, Fields) || {Name, Compare} <- maps:to_list(Preset)],
    Condition = [parse_having_compare(File, SQL, Table, Operation, Name, maps:get(Name, Preset), Fields) || Name <- Order],
    lists:concat(["HAVING", " ", string:join(Condition, " AND ")]);

%% multi column
parse_having(File, SQL = #{having := Preset = [_ | _]}, Table, Operation, Fields, _) when is_atom(hd(Preset)) orelse is_list(hd(Preset)) orelse is_binary(hd(Preset)) ->
    Condition = [lists:concat(["`", Name, "`", " = ", (parse_field_format(File, SQL, Table, Operation, Fields, Name))#field.value]) || Name <- Preset],
    lists:concat(["HAVING",  " ", string:join(Condition, " AND ")]);

%% single column
parse_having(File, SQL = #{having := Preset}, Table, Operation, Fields, _) when is_atom(Preset) orelse is_list(Preset) orelse is_binary(Preset) ->
    #field{alias = Alias, value = Format} = parse_field_format(File, SQL, Table, Operation, Fields, Preset),
    lists:concat(["HAVING", " ", type:to_list(Alias), " = ", type:to_list(Format)]);

%% without having
parse_having(_, #{}, _, _, _, _) ->
    lists:concat([]).


%% spec compare or literal
parse_having_compare(File, SQL, Table, Operation, Name, Preset = #{}, Fields) ->
    Condition = [parse_having_compare_literal(File, SQL, Table, Operation, Name, Compare, Value, Fields) || {Compare, Value} <- maps:to_list(Preset)],
    string:join(Condition, " AND ");

%% compare only
parse_having_compare(File, SQL, Table, Operation, Name, Compare, Fields) ->
    #field{alias = Alias, value = Format} = parse_field_format(File, SQL, Table, Operation, Fields, Name),
    lists:concat([type:to_list(Alias), " ", string:to_upper(type:to_list(Compare)), " ", type:to_list(Format)]).


%% not literal
parse_having_compare_literal(File, SQL, Table, Operation, Name, Compare, {'$param$', []}, Fields) ->
    #field{alias = Alias, value = Format} = parse_field_format(File, SQL, Table, Operation, Fields, Name),
    lists:concat([type:to_list(Alias), " ", string:to_upper(type:to_list(Compare)), " ", type:to_list(Format)]);

%% literal
parse_having_compare_literal(File, SQL, Table, Operation, Name, Compare, Literal, Fields) ->
    #field{alias = Alias} = parse_field_format(File, SQL, Table, Operation, Fields, Name),
    lists:concat([type:to_list(Alias), " ", string:to_upper(type:to_list(Compare)), " ", "'", Literal, "'"]).

%%%===================================================================
%%% order by part
%%%===================================================================

%% order by spec order
parse_order_by(_, #{order_by := Preset = #{}}, _, _, Order) ->
    %% OrderBy = [lists:concat(["`", Field, "`", " ", string:to_upper(type:to_list(Sort))]) || {Field, Sort} <- maps:to_list(Preset)],
    OrderBy = [lists:concat(["`", Field, "`", " ", string:to_upper(type:to_list(maps:get(Field, Preset)))]) || Field <- Order],
    lists:concat(["ORDER BY ", string:join(OrderBy, ", ")]);

%% order by multi column
parse_order_by(_, #{order_by := Preset = [_ | _]}, _, _, _) ->
    OrderBy = [lists:concat(["`", Field, "`", " ", "ASC"]) || Field <- maps:to_list(Preset)],
    lists:concat(["GROUP BY ", string:join(OrderBy, ", ")]);

%% order by single column
parse_order_by(_, #{order_by := Field}, _, _, _) ->
    lists:concat(["ORDER BY `", Field, "` ASC"]);

%% without order
parse_order_by(_, #{}, _, _, _) ->
    lists:concat([]).

%%%===================================================================
%%% limit part
%%%===================================================================

%% with limit
parse_limit(_, #{limit := Limit}, _, _) ->
    lists:concat(["LIMIT ", Limit]);

%% without limit
parse_limit(_, #{}, _, _) ->
    lists:concat([]).

%%%===================================================================
%%% offset part
%%%===================================================================

%% with offset
parse_offset(_, #{offset := Offset}, _, _) ->
    lists:concat(["OFFSET ", Offset]);

%% without offset
parse_offset(_, #{}, _, _) ->
    lists:concat([]).

%%%===================================================================
%%% key part
%%%===================================================================

parse_key(File, SQL, Table, Operation, Fields, WhereOrder, HavingOrder) ->
    lists:append([parse_key_where(File, SQL, Table, Operation, Fields, WhereOrder), parse_key_having(File, SQL, Table, Operation, Fields, HavingOrder)]).

%% spec key
parse_key_where(File, SQL = #{by := Preset = #{}}, Table, Operation, Fields, Order) ->
    %% lists:append([parse_key_compare(File, Table, Name, Compare, Fields) || {Name, Compare} <- maps:to_list(Preset)]);
    lists:append([parse_key_compare(File, SQL, Table, Operation, Name, maps:get(Name, Preset), Fields) || Name <- Order]);

%% multi column
parse_key_where(File, SQL = #{by := Preset = [_ | _]}, Table, Operation, Fields, _) when is_atom(hd(Preset)) orelse is_list(hd(Preset)) orelse is_binary(hd(Preset)) ->
    [(parse_field_type(File, SQL, Table, Operation, Fields, Name))#field{alias = word:to_hump(Name)} || Name <- Preset];

%% single column
parse_key_where(File, SQL = #{by := Preset}, Table, Operation, Fields, _) when is_atom(Preset) orelse is_list(Preset) orelse is_binary(Preset) ->
    Field = parse_field_type(File, SQL, Table, Operation, Fields, Preset),
    [Field];

%% without key
parse_key_where(_, #{}, _, _, _, _) ->
    [].

%% spec key
parse_key_having(File, SQL = #{having := Preset = #{}}, Table, Operation, Fields, _) ->
    lists:append([parse_key_compare(File, SQL, Table, Operation, Name, Compare, Fields) || {Name, Compare} <- maps:to_list(Preset)]);

%% multi column
parse_key_having(File, SQL = #{having := Preset = [_ | _]}, Table, Operation, Fields, _) ->
    [(parse_field_type(File, SQL, Table, Operation, Fields, Name))#field{alias = word:to_hump(Name)} || Name <- Preset];

%% single column
parse_key_having(File, SQL = #{having := Name}, Table, Operation, Fields, _) ->
    Field = parse_field_type(File, SQL, Table, Operation, Fields, Name),
    [Field];

%% without key
parse_key_having(_, #{}, _, _, _, _) ->
    [].


%% spec compare or literal
parse_key_compare(File, SQL, Table, Operation, Name, Preset = #{}, Fields) ->
    lists:append([parse_key_compare_literal(File, SQL, Table, Operation, Name, Compare, Value, Fields) || {Compare, Value} <- maps:to_list(Preset)]);

%% compare only
parse_key_compare(File, SQL, Table, Operation, Name, Compare, Fields) ->
    Field = parse_field_type(File, SQL, Table, Operation, Fields, Name),
    [Field#field{preset = Compare}].


%% not literal
parse_key_compare_literal(File, SQL, Table, Operation, Name, Compare, {'$param$', []}, Fields) ->
    Field = parse_field_type(File, SQL, Table, Operation, Fields, Name),
    [Field#field{preset = Compare}];

%% literal
parse_key_compare_literal(_, _, _, _, _, _, _, _) ->
    [].

%%%===================================================================
%%% fields type and format part
%%%===================================================================

%% value
parse_field_value(File, SQL, Table, Operation, Fields, Name, Preset) ->
    Base = filename:basename(File),
    Key = type:to_binary(Name),
    Field = lists:keyfind(Key, #field.alias, Fields),
    is_boolean(Field) andalso erlang:throw(lists:flatten(io_lib:format("Could not found field from `~ts` by name `~ts` on `~ts` in ~ts", [Table, Name, Operation, Base]))),
    %% convert table.field to `table`.`field`
    TableField = db:dot(Name),
    %% SELECT `table`.`field` FROM `table`
    %% INSERT `table` (`table`.`field`) VALUES (?)
    %% UPDATE `table` SET `table`.`field` = ?
    Use = [{Local, Foreign} || {Local, Foreign} <- maps:to_list(maps:get(use, SQL, #{})), type:to_atom(Local) == type:to_atom(Field#field.name) orelse type:to_atom(Local) == type:to_atom(<<(Field#field.table)/binary, ".", (Field#field.name)/binary>>)],
    %% length(Use) > 1 andalso erlang:throw(lists:flatten(io_lib:format("Found many use: ~tp by name `~ts` or `~ts` in ~ts", [Use, Field#field.name, <<(Field#field.table)/binary, ".", (Field#field.name)/binary>>, Base]))),
    %% use replace alias
    Alias = maps:get(Use, #{[] => TableField}, [db:dot(Foreign) || {_, Foreign} <- Use]),
    Value = maps:get(Operation, #{select => Alias}, "?"),
    parse_field_value(Field#field{alias = Alias, value = Value, preset = Preset, save = length(Use) > 0}).

parse_field_value(#field{extra = <<"VIRTUAL", _/binary>>, save = false}) ->
    [];

parse_field_value(Field = #field{preset = {'$raw$', Raw}}) ->
    Field#field{value = Raw};

parse_field_value(#field{preset = {'$all$', All}}) ->
    erlang:throw(lists:flatten(io_lib:format("Sql doest not support `all`: ~tp operation", [All])));

parse_field_value(Field = #field{preset = {'$avg$', Avg}}) ->
    #field{value = SQL} = parse_field_value(Field#field{preset = Avg}),
    Field#field{value = lists:concat(["AVG", "(" , SQL, ")"])};

parse_field_value(Field = #field{preset = {'$bit_and$', BitAnd}}) ->
    #field{value = SQL} = parse_field_value(Field#field{preset = BitAnd}),
    Field#field{value = lists:concat(["BIT_AND", "(" , SQL, ")"])};

parse_field_value(Field = #field{preset = {'$bit_or$', BitOr}}) ->
    #field{value = SQL} = parse_field_value(Field#field{preset = BitOr}),
    Field#field{value = lists:concat(["BIT_OR", "(" , SQL, ")"])};

parse_field_value(Field = #field{preset = {'$bit_xor$', BitXor}}) ->
    #field{value = SQL} = parse_field_value(Field#field{preset = BitXor}),
    Field#field{value = lists:concat(["BIT_XOR", "(" , SQL, ")"])};

parse_field_value(Field = #field{preset = {'$count$', Count}}) ->
    #field{value = SQL} = parse_field_value(Field#field{preset = Count}),
    Field#field{value = lists:concat(["COUNT", "(" , SQL, ")"])};

parse_field_value(Field = #field{preset = {'$max$', Max}}) ->
    #field{value = SQL} = parse_field_value(Field#field{preset = Max}),
    Field#field{value = lists:concat(["MAX", "(" , SQL, ")"])};

parse_field_value(Field = #field{preset = {'$min$', Min}}) ->
    #field{value = SQL} = parse_field_value(Field#field{preset = Min}),
    Field#field{value = lists:concat(["MIN", "(" , SQL, ")"])};

parse_field_value(Field = #field{preset = {'$std$', Std}}) ->
    #field{value = SQL} = parse_field_value(Field#field{preset = Std}),
    Field#field{value = lists:concat(["STD", "(" , SQL, ")"])};

parse_field_value(Field = #field{preset = {'$std_dev$', StdDev}}) ->
    #field{value = SQL} = parse_field_value(Field#field{preset = StdDev}),
    Field#field{value = lists:concat(["STDDEV", "(" , SQL, ")"])};

parse_field_value(Field = #field{preset = {'$std_dev_pop$', StdDevPop}}) ->
    #field{value = SQL} = parse_field_value(Field#field{preset = StdDevPop}),
    Field#field{value = lists:concat(["STDDEV_POP", "(" , SQL, ")"])};

parse_field_value(Field = #field{preset = {'$std_dev_sample$', StdDevSample}}) ->
    #field{value = SQL} = parse_field_value(Field#field{preset = StdDevSample}),
    Field#field{value = lists:concat(["STDDEV_SAMP", "(" , SQL, ")"])};

parse_field_value(Field = #field{preset = {'$sum$', Sum}}) ->
    #field{value = SQL} = parse_field_value(Field#field{preset = Sum}),
    Field#field{value = lists:concat(["SUM", "(" , SQL, ")"])};

parse_field_value(Field = #field{preset = {'$var_pop$', VarPop}}) ->
    #field{value = SQL} = parse_field_value(Field#field{preset = VarPop}),
    Field#field{value = lists:concat(["VAR_POP", "(" , SQL, ")"])};

parse_field_value(Field = #field{preset = {'$var_sample$', VarSample}}) ->
    #field{value = SQL} = parse_field_value(Field#field{preset = VarSample}),
    Field#field{value = lists:concat(["VAR_SAMP", "(" , SQL, ")"])};

parse_field_value(Field = #field{preset = {'$variance$', Variance}}) ->
    #field{value = SQL} = parse_field_value(Field#field{preset = Variance}),
    Field#field{value = lists:concat(["VARIANCE", "(" , SQL, ")"])};

parse_field_value(Field) ->
    Field.


%% format
parse_field_format(File, _, Table, Operation, Fields, Name) ->
    Base = filename:basename(File),
    Key = type:to_binary(Name),
    Field = lists:keyfind(Key, #field.alias, Fields),
    is_boolean(Field) andalso erlang:throw(lists:flatten(io_lib:format("Could not found field from `~ts` by name `~ts` on `~ts` in ~ts", [Table, Name, Operation, Base]))),
    %% convert table.field to `table`.`field`
    TableField = db:dot(Name),
    parse_field_format(Field#field{alias = TableField}).

parse_field_format(Field = #field{}) ->
    Field#field{value = "?"}.

%%parse_field_format(#field{format = <<"boolean">>}) ->
%%    "~w";
%%parse_field_format(#field{format = <<"tinyint">>}) ->
%%    "~w";
%%parse_field_format(#field{format = <<"smallint">>}) ->
%%    "~w";
%%parse_field_format(#field{format = <<"int">>}) ->
%%    "~w";
%%parse_field_format(#field{format = <<"bigint">>}) ->
%%    "~w";
%%parse_field_format(#field{format = <<"varchar">>}) ->
%%    "~w";
%%parse_field_format(#field{format = <<"char">>}) ->
%%    "~s";
%%parse_field_format(#field{format = <<"decimal">>}) ->
%%    "~w".


%% spec type
parse_field_type(File, _, Table, Operation, Fields, Name) ->
    Base = filename:basename(File),
    Key = type:to_binary(Name),
    Field = lists:keyfind(Key, #field.alias, Fields),
    is_boolean(Field) andalso erlang:throw(lists:flatten(io_lib:format("Could not found field from `~ts` by name `~ts` on `~ts` in ~ts", [Table, Name, Operation, Base]))),
    %% convert table.field to TableField
    TableField = db:to_hump(Name),
    parse_field_type(Field#field{alias = TableField}).

parse_field_type(Field = #field{format = <<"boolean">>}) ->
    Field#field{value = "boolean()"};

parse_field_type(Field = #field{format = <<"tinyint">>, type = <<"unsigned">>}) ->
    Field#field{value = "non_neg_integer()"};

parse_field_type(Field = #field{format = <<"smallint">>, type = <<"unsigned">>}) ->
    Field#field{value = "non_neg_integer()"};

parse_field_type(Field = #field{format = <<"int">>, type = <<"unsigned">>}) ->
    Field#field{value = "non_neg_integer()"};

parse_field_type(Field = #field{format = <<"bigint">>, type = <<"unsigned">>}) ->
    Field#field{value = "non_neg_integer()"};

parse_field_type(Field = #field{format = <<"tinyint">>}) ->
    Field#field{value = "integer()"};

parse_field_type(Field = #field{format = <<"smallint">>}) ->
    Field#field{value = "integer()"};

parse_field_type(Field = #field{format = <<"int">>}) ->
    Field#field{value = "integer()"};

parse_field_type(Field = #field{format = <<"bigint">>}) ->
    Field#field{value = "integer()"};

parse_field_type(Field = #field{format = <<"varchar">>}) ->
    Field#field{value = "term()"};

parse_field_type(Field = #field{format = <<"char">>}) ->
    Field#field{value = "binary()"};

parse_field_type(Field = #field{format = <<"decimal">>}) ->
    Field#field{value = "float()"}.

%%%===================================================================
%%% tool part
%%%===================================================================

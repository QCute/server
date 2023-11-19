%%%-------------------------------------------------------------------
%%% @doc
%%% make database data to erlang term
%%% @end
%%%-------------------------------------------------------------------
-module(erl_maker).
-export([start/1]).
-record(field, {table = <<>>, name = <<>>, default = <<>>, type = <<>>, format = <<>>, comment = <<>>, key = <<>>, extra = <<>>, expression = <<>>, position = 0, alias = <<>>, value = <<>>, save = false, preset = #{}, except = false}).
%%%===================================================================
%%% API functions
%%%===================================================================
%% @doc for shell
start(List) ->
    maker:start(fun parse_file/1, List).

%%%===================================================================
%%% Internal functions
%%%===================================================================
%% @doc parse file
parse_file(Item = #{file := File, sql := Sql}) ->
    Order = maker:collect_map_order("script/make/erl/erl_script.erl", File),

    %% collect into or from table
    Tables = listing:unique([maps:get(into, Sets, maps:get(from, Sets, undefined)) || Sets <- Sql, is_map_key(into, Sets) orelse is_map_key(from, Sets)]),
    Includes = string:join(maker:collect_include(Tables), ""),

    %% the main function
    length(Sql) =/= length(Order) andalso erlang:throw(lists:flatten(io_lib:format("Sql has ~w item, but form has ~w item, check the comma if exists between two maps", [length(Sql), length(Order)]))),
    Sets = [parse_sql(File, Sets, MapOrder) || {Sets, MapOrder} <- lists:zip(Sql, Order)],

    Exports = string:join([lists:concat([Export, "\n"]) || #{export := Export} <- Sets], ""),

    Function = string:join([lists:concat([Code, "\n"]) || #{code := Code} <- Sets], "\n"),

    Extra = maps:get(extra, Item, []),
    ExtraExport = [lists:concat(["-export([", Name, "/", Arity, "]).", "\n"]) || {Name, Arity} <- parser:parse_function(lists:flatten(Extra))],

    Code = unicode:characters_to_binary(lists:flatten(lists:concat([
        "-module(", filename:basename(File, ".erl"), ").", "\n",
        Exports,
        ExtraExport,
        Includes,
        "\n",
        Function,
        "\n",
        Extra
    ]))),

    [#{pattern => [], code => Code}].


parse_sql(File, SQL = #{select := Select, from := Table, as := FunctionName}, Order) ->

    Fields = collect_fields(File, SQL, Table),

    KeyFields = parse_key(File, SQL, Table, select, Fields, maps:get(by, Order, []), maps:get(having, Order, [])),

    %% format condition
    Where = parse_where(File, SQL, Table, select, Fields, maps:get(by, Order, [])),
    GroupBy = parse_group_by(File, SQL, Table, Fields),
    Having = parse_having(File, SQL, Table, select, Fields, maps:get(having, Order, [])),
    OrderBy = parse_order_by(File, SQL, Table, Fields, maps:get(order_by, Order, [])),
    Limit = parse_limit(File, SQL, Table, Fields),
    Offset = parse_offset(File, SQL, Table, Fields),

    FilterWhere = parse_filter_where(File, SQL, Table, select, Fields, maps:get(by, Order, [])),
    FilterHaving = parse_filter_having(File, SQL, Table, select, Fields, maps:get(having, Order, [])),
    UniqueBy = parse_unique_by(File, SQL, Table, Fields),
    SortBy = parse_sort_by(File, SQL, Table, Fields, maps:get(sort_by, Order, [])),

    %% parse join
    Join = parse_join(SQL, Table, Fields),

    DefaultFields = listing:collect_into(#field.alias, [Field || Field = #field{table = Target} <- Fields, Target == type:to_binary(Table)], fun erlang:binary_to_atom/1),
    Preset = #{
        [] => DefaultFields,
        {} => DefaultFields,
        {'$list$', []} => DefaultFields,
        {'$tuple$', []} => DefaultFields,
        {'$map$', []} => DefaultFields,
        {'$record$', []} => DefaultFields,
        {'$all$', {'$list$', []}} => DefaultFields,
        {'$all$', []} => DefaultFields,
        {'$all$', {'$tuple$', []}} => DefaultFields,
        {'$all$', {}} => DefaultFields,
        {'$all$', {'$map$', []}} => DefaultFields,
        {'$all$', #{}} => DefaultFields,
        {'$all$', {'$record$', []}} => DefaultFields
    },
    PresetFields = maps:get(Select, Preset, Select),
    ValueFields = parse_value(File, SQL, Table, select, PresetFields, Fields, maps:get(select, Order, []), []),

    %% format names SELECT `key`, `value`, ...
    TableFields = string:join([type:to_list(Value) || #field{value = Value} <- ValueFields], ", "),

    %% concat data
    List = [
        "SELECT", " ", TableFields, " ", "FROM",
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

    parse_key_value(File, SQL, Select, Table, Fields, FunctionName, KeyFields, ValueFields, Result, FilterWhere, FilterHaving, UniqueBy, SortBy).


parse_key_value(File, SQL, Select, Table, Fields, FunctionName, [], ValueFields, ValueSql, _, _, _, _) ->

    RawValue = db:select(ValueSql),
    Value = [[Field#field{value = Column} || {Column, Field} <- lists:zip(Row, ValueFields)] || Row <- RawValue],

    ValueSpec = format_spec(File, SQL, Select, Table, Fields, FunctionName, [parse_field_type(Field) || Field <- ValueFields]),

    Return = format_row(File, SQL, Select, Table, Fields, FunctionName, false, 1, 1, 1, [{[], Value}], []),

    Export = lists:concat([
        "-export([", FunctionName, "/0])."
    ]),

    Code = lists:concat([
        "-spec ", FunctionName, "() -> ", ValueSpec, ".", "\n",
        Return, "."
    ]),

    #{export => Export, code => Code};

parse_key_value(File, SQL, Select, Table, Fields, FunctionName, KeyFields, ValueFields, ValueSql, FilterWhere, FilterHaving, UniqueBy, SortBy) ->

    %% collect keys
    KeyData = [db:select(lists:concat(["SELECT", " ", "DISTINCT", " ", "`", db:to_snake(Alias), "`", " ", "FROM", " ", "`", Table, "`", FilterWhere, UniqueBy, FilterHaving, SortBy])) || #field{alias = Alias} <- KeyFields],

    %% transform row to column
    Matrix = compose(KeyData),
    ValueData = collect_data(Matrix, ValueSql, KeyFields, ValueFields, []),

    Range = lists:any(fun(#field{preset = Preset}) -> listing:is_in(Preset, ['>', '<', '>=', '<=', '=<']) end, KeyFields),

    Return = string:join(format_row(File, SQL, Select, Table, Fields, FunctionName, Range, 1, length(ValueData), 1, ValueData, []), ";\n"),

    Export = lists:concat([
        "-export([", FunctionName, "/", length(KeyFields), "])."
    ]),

    KeySpec = string:join([format_key_spec(File, SQL, Select, Table, Fields, FunctionName, Field) || Field <- KeyFields], ", "),

    ValueSpec = format_spec(File, SQL, Select, Table, Fields, FunctionName, [parse_field_type(Field) || Field <- ValueFields]),

    Default = format_default(File, SQL, Select, Table, KeyFields, FunctionName),

    Code = lists:concat([
        "-spec ", FunctionName, "(", KeySpec, ") -> ", ValueSpec, ".", "\n",
        [[Return, ";", "\n"] || Return =/= []],
        Default
    ]),

    #{export => Export, code => Code}.

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
        "`GENERATION_EXPRESSION`",
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

parse_value(File, SQL, Table, Operation, [Name | Preset], Fields, Order, Values) when is_tuple(Name) ->
    Field = parse_value(File, SQL, Table, Operation, Name, Fields, Order, []),
    parse_value(File, SQL, Table, Operation, Preset, Fields, Order, [Field | Values]);

%% list
parse_value(File, SQL, Table, Operation, {'$list$', Preset}, Fields, Order, Values) ->
    parse_value(File, SQL, Table, Operation, Preset, Fields, Order, Values);

%% tuple
parse_value(File, SQL, Table, Operation, {'$tuple$', Preset}, Fields, Order, Values) ->
    parse_value(File, SQL, Table, Operation, Preset, Fields, Order, Values);

%% map
parse_value(File, SQL, Table, Operation, {'$map$', Preset}, Fields, Order, Values) ->
    parse_value(File, SQL, Table, Operation, Preset, Fields, Order, Values);

%% record
parse_value(File, SQL, Table, Operation, {'$record$', Preset}, Fields, Order, Values) ->
    parse_value(File, SQL, Table, Operation, Preset, Fields, Order, Values);

parse_value(File, SQL, Table, Operation, {'$all$', All}, Fields, Order, Values) ->
    parse_value(File, SQL, Table, Operation, All, Fields, Order, Values);

parse_value(File, SQL, Table, Operation, Preset = {'$avg$', Avg}, Fields, Order, Values) ->
    Field = parse_field_value(File, SQL, Table, Operation, Fields, Avg, Preset),
    parse_value(File, SQL, Table, Operation, [], Fields, Order, [Field | Values]);

parse_value(File, SQL, Table, Operation, Preset = {'$bit_and$', BitAnd}, Fields, Order, Values) ->
    Field = parse_field_value(File, SQL, Table, Operation, Fields, BitAnd, Preset),
    parse_value(File, SQL, Table, Operation, [], Fields, Order, [Field | Values]);

parse_value(File, SQL, Table, Operation, Preset = {'$bit_or$', BitOr}, Fields, Order, Values) ->
    Field = parse_field_value(File, SQL, Table, Operation, Fields, BitOr, Preset),
    parse_value(File, SQL, Table, Operation, [], Fields, Order, [Field | Values]);

parse_value(File, SQL, Table, Operation, Preset = {'$bit_xor$', BitXor}, Fields, Order, Values) ->
    Field = parse_field_value(File, SQL, Table, Operation, Fields, BitXor, Preset),
    parse_value(File, SQL, Table, Operation, [], Fields, Order, [Field | Values]);

parse_value(File, SQL, Table, Operation, Preset = {'$count$', Count}, Fields, Order, Values) ->
    Field = parse_field_value(File, SQL, Table, Operation, Fields, Count, Preset),
    parse_value(File, SQL, Table, Operation, [], Fields, Order, [Field | Values]);

parse_value(File, SQL, Table, Operation, Preset = {'$max$', Max}, Fields, Order, Values) ->
    Field = parse_field_value(File, SQL, Table, Operation, Fields, Max, Preset),
    parse_value(File, SQL, Table, Operation, [], Fields, Order, [Field | Values]);

parse_value(File, SQL, Table, Operation, Preset = {'$min$', Min}, Fields, Order, Values) ->
    Field = parse_field_value(File, SQL, Table, Operation, Fields, Min, Preset),
    parse_value(File, SQL, Table, Operation, [], Fields, Order, [Field | Values]);

parse_value(File, SQL, Table, Operation, Preset = {'$std$', Std}, Fields, Order, Values) ->
    Field = parse_field_value(File, SQL, Table, Operation, Fields, Std, Preset),
    parse_value(File, SQL, Table, Operation, [], Fields, Order, [Field | Values]);

parse_value(File, SQL, Table, Operation, Preset = {'$std_dev$', StdDev}, Fields, Order, Values) ->
    Field = parse_field_value(File, SQL, Table, Operation, Fields, StdDev, Preset),
    parse_value(File, SQL, Table, Operation, [], Fields, Order, [Field | Values]);

parse_value(File, SQL, Table, Operation, Preset = {'$std_dev_pop$', StdDevPop}, Fields, Order, Values) ->
    Field = parse_field_value(File, SQL, Table, Operation, Fields, StdDevPop, Preset),
    parse_value(File, SQL, Table, Operation, [], Fields, Order, [Field | Values]);

parse_value(File, SQL, Table, Operation, Preset = {'$std_dev_sample$', StdDevSample}, Fields, Order, Values) ->
    Field = parse_field_value(File, SQL, Table, Operation, Fields, StdDevSample, Preset),
    parse_value(File, SQL, Table, Operation, [], Fields, Order, [Field | Values]);

parse_value(File, SQL, Table, Operation, Preset = {'$sum$', Sum}, Fields, Order, Values) ->
    Field = parse_field_value(File, SQL, Table, Operation, Fields, Sum, Preset),
    parse_value(File, SQL, Table, Operation, [], Fields, Order, [Field | Values]);

parse_value(File, SQL, Table, Operation, Preset = {'$var_pop$', VarPop}, Fields, Order, Values) ->
    Field = parse_field_value(File, SQL, Table, Operation, Fields, VarPop, Preset),
    parse_value(File, SQL, Table, Operation, [], Fields, Order, [Field | Values]);

parse_value(File, SQL, Table, Operation, Preset = {'$var_sample$', VarSample}, Fields, Order, Values) ->
    Field = parse_field_value(File, SQL, Table, Operation, Fields, VarSample, Preset),
    parse_value(File, SQL, Table, Operation, [], Fields, Order, [Field | Values]);

parse_value(File, SQL, Table, Operation, Preset = {'$variance$', Variance}, Fields, Order, Values) ->
    Field = parse_field_value(File, SQL, Table, Operation, Fields, Variance, Preset),
    parse_value(File, SQL, Table, Operation, [], Fields, Order, [Field | Values]);

parse_value(File, SQL, Table, Operation, Preset, Fields, Order, Values) when is_tuple(Preset) ->
    parse_value(File, SQL, Table, Operation, tuple_to_list(Preset), Fields, Order, Values);

%% atom
parse_value(File, SQL, Table, Operation, Preset, Fields, _, Values) when is_atom(Preset) orelse is_list(Preset) orelse is_binary(Preset) ->
    Field = parse_field_value(File, SQL, Table, Operation, Fields, Preset, Preset),
    lists:flatten([Field | Values]).

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

%% normal compare
parse_where_compare(File, SQL, Table, Operation, Name, Compare, Fields) when Compare == '=' orelse Compare == '>' orelse Compare == '>=' orelse Compare == '<' orelse Compare == '<=' ->
    #field{alias = Alias, value = Format} = parse_field_format(File, SQL, Table, Operation, Fields, Name),
    lists:concat([type:to_list(Alias), " ", string:to_upper(type:to_list(Compare)), " ", type:to_list(Format)]);

%% other compare
parse_where_compare(_, _, _, _, _, Compare, _) ->
    erlang:throw(lists:flatten(io_lib:format("Compare `~ts` parameterize does not supported", [Compare]))).

%% raw
parse_where_compare_literal(File, SQL, Table, Operation, Name, Compare, {'$raw$', Raw}, Fields) ->
    #field{alias = Alias} = parse_field_format(File, SQL, Table, Operation, Fields, Name),
    lists:concat([type:to_list(Alias), " ", Compare, " ", Raw, ""]);

%% normal compare => binding param
parse_where_compare_literal(File, SQL, Table, Operation, Name, Compare, {'$param$', []}, Fields) when Compare == '=' orelse Compare == '>' orelse Compare == '>=' orelse Compare == '<' orelse Compare == '<=' ->
    #field{alias = Alias, value = Format} = parse_field_format(File, SQL, Table, Operation, Fields, Name),
    lists:concat([type:to_list(Alias), " ", string:to_upper(type:to_list(Compare)), " ", "(", type:to_list(Format), ")"]);

%% other compare => binding param
parse_where_compare_literal(_, _, _, _, _, Compare, {'$param$', []}, _) ->
    erlang:throw(lists:flatten(io_lib:format("Compare `~ts` parameterize does not supported", [Compare])));

%% literal
parse_where_compare_literal(File, SQL, Table, Operation, Name, Compare, Literal, Fields) when Compare == in orelse Compare == not_in ->
    #field{alias = Alias} = parse_field_format(File, SQL, Table, Operation, Fields, Name),
    lists:concat([type:to_list(Alias), " ", string:to_upper(type:to_list(Compare)), " ", "(", type:to_list(db:join(Literal)), ")"]);

%% literal
parse_where_compare_literal(File, SQL, Table, Operation, Name, Compare, Literal, Fields) ->
    #field{alias = Alias} = parse_field_format(File, SQL, Table, Operation, Fields, Name),
    lists:concat([type:to_list(Alias), " ", string:to_upper(type:to_list(Compare)), " ", "'", Literal, "'"]).

%%%===================================================================
%%% filter where part
%%%===================================================================

%% spec where
parse_filter_where(File, SQL = #{by := Preset = #{}}, Table, Operation, Fields, Order) ->
    %% Condition = [parse_where_compare(File, Table, Name, Compare, Fields) || {Name, Compare} <- maps:to_list(Preset)],
    Condition = [parse_filter_where_compare(File, SQL, Table, Operation, Name, maps:get(Name, Preset), Fields) || Name <- Order],
    case lists:flatten(Condition) of
        [] ->
            [];
        _ ->
            lists:concat([" ", "WHERE", " ", string:join([Option || Option <- Condition, Option =/= []], " AND ")])
    end;

%% multi column
parse_filter_where(_, #{by := Preset = [_ | _]}, _, _, _, _) when is_atom(hd(Preset)) orelse is_list(hd(Preset)) orelse is_binary(hd(Preset)) ->
    lists:concat([]);

%% single column
parse_filter_where(_, #{by := Preset}, _, _, _, _) when is_atom(Preset) orelse is_list(Preset) orelse is_binary(Preset) ->
    lists:concat([]);

%% without where
parse_filter_where(_, #{}, _, _, _, _) ->
    lists:concat([]).


%% spec compare or literal
parse_filter_where_compare(File, SQL, Table, Operation, Name, Preset = #{}, Fields) ->
    Condition = [parse_filter_where_compare_literal(File, SQL, Table, Operation, Name, Compare, Value, Fields) || {Compare, Value} <- maps:to_list(Preset)],
    string:join([Option || Option <- Condition, Option =/= []], " AND ");

%% normal compare
parse_filter_where_compare(_, _, _, _, _, Compare, _) when Compare == '=' orelse Compare == '>' orelse Compare == '>=' orelse Compare == '<' orelse Compare == '<=' ->
    lists:concat([]);

%% other compare
parse_filter_where_compare(_, _, _, _, _, Compare, _) ->
    erlang:throw(lists:flatten(io_lib:format("Compare `~ts` parameterize does not supported", [Compare]))).

%% raw
parse_filter_where_compare_literal(File, SQL, Table, Operation, Name, Compare, {'$raw$', Raw}, Fields) ->
    #field{alias = Alias} = parse_field_format(File, SQL, Table, Operation, Fields, Name),
    lists:concat([type:to_list(Alias), " ", Compare, " ", Raw, ""]);

%% normal compare => binding param
parse_filter_where_compare_literal(_, _, _, _, _, Compare, {'$param$', []}, _) when Compare == '=' orelse Compare == '>' orelse Compare == '>=' orelse Compare == '<' orelse Compare == '<=' ->
    lists:concat([]);

%% other compare => binding param
parse_filter_where_compare_literal(_, _, _, _, _, Compare, {'$param$', []}, _) ->
    erlang:throw(lists:flatten(io_lib:format("Compare `~ts` parameterize does not supported", [Compare])));

%% literal
parse_filter_where_compare_literal(File, SQL, Table, Operation, Name, Compare, Literal, Fields) when Compare == in orelse Compare == not_in ->
    #field{alias = Alias} = parse_field_format(File, SQL, Table, Operation, Fields, Name),
    lists:concat([type:to_list(Alias), " ", string:to_upper(type:to_list(Compare)), " ", "(", type:to_list(db:join(Literal)), ")"]);

%% literal
parse_filter_where_compare_literal(File, SQL, Table, Operation, Name, Compare, Literal, Fields) ->
    #field{alias = Alias} = parse_field_format(File, SQL, Table, Operation, Fields, Name),
    lists:concat([type:to_list(Alias), " ", string:to_upper(type:to_list(Compare)), " ", "'", Literal, "'"]).

%%%===================================================================
%%% group by part
%%%===================================================================

%% group by multi column
parse_group_by(_, #{group_by := Preset = [_ | _]}, _, _) ->
    GroupBy = [lists:concat(["`", Field, "`"]) || Field <- maps:to_list(Preset)],
    lists:concat([" ", "GROUP BY ", string:join(GroupBy, ", ")]);

%% group by single column
parse_group_by(_, #{group_by := Field}, _, _) ->
    lists:concat([" ", "GROUP BY `", Field, "`"]);

%% without group
parse_group_by(_, #{}, _, _) ->
    lists:concat([]).

%%%===================================================================
%%% group by part
%%%===================================================================

%% unique by multi column
parse_unique_by(_, #{unique_by := Preset = [_ | _]}, _, _) ->
    UniqueBy = [lists:concat(["`", Field, "`"]) || Field <- maps:to_list(Preset)],
    lists:concat([" ", "GROUP BY ", string:join(UniqueBy, ", ")]);

%% unique by single column
parse_unique_by(_, #{unique_by := Field}, _, _) ->
    lists:concat([" ", "GROUP BY `", Field, "`"]);

%% without unique
parse_unique_by(_, #{}, _, _) ->
    lists:concat([]).

%%%===================================================================
%%% having part
%%%===================================================================

%% spec having
parse_having(File, SQL = #{having := Preset = #{}}, Table, Operation, Fields, Order) ->
    %% Condition = [parse_having_compare(File, Table, Name, Compare, Fields) || {Name, Compare} <- maps:to_list(Preset)],
    Condition = [parse_having_compare(File, SQL, Table, Operation, Name, maps:get(Name, Preset), Fields) || Name <- Order],
    case lists:flatten(Condition) of
        [] ->
            [];
        _ ->
            lists:concat([" ", "HAVING", " ", string:join(Condition, " AND ")])
    end;

%% multi column
parse_having(_, #{having := Preset = [_ | _]}, _, _, _, _) when is_atom(hd(Preset)) orelse is_list(hd(Preset)) orelse is_binary(hd(Preset)) ->
    lists:concat([]);

%% single column
parse_having(File, SQL = #{having := Preset}, Table, Operation, Fields, _) when is_atom(Preset) orelse is_list(Preset) orelse is_binary(Preset) ->
    #field{alias = Alias, value = Format} = parse_field_format(File, SQL, Table, Operation, Fields, Preset),
    lists:concat([" ", "HAVING", " ", type:to_list(Alias), " = ", type:to_list(Format)]);

%% without having
parse_having(_, #{}, _, _, _, _) ->
    lists:concat([]).


%% spec compare or literal
parse_having_compare(File, SQL, Table, Operation, Name, Preset = #{}, Fields) ->
    Condition = [parse_having_compare_literal(File, SQL, Table, Operation, Name, Compare, Value, Fields) || {Compare, Value} <- maps:to_list(Preset)],
    string:join(Condition, " AND ");

%% normal compare
parse_having_compare(File, SQL, Table, Operation, Name, Compare, Fields) when Compare == '=' orelse Compare == '>' orelse Compare == '>=' orelse Compare == '<' orelse Compare == '<=' ->
    #field{alias = Alias, value = Format} = parse_field_format(File, SQL, Table, Operation, Fields, Name),
    lists:concat([type:to_list(Alias), " ", string:to_upper(type:to_list(Compare)), " ", type:to_list(Format)]);

%% other compare
parse_having_compare(_, _, _, _, _, Compare, _) ->
    erlang:throw(lists:flatten(io_lib:format("Compare `~ts` parameterize does not supported", [Compare]))).

%% raw
parse_having_compare_literal(File, SQL, Table, Operation, Name, Compare, {'$raw$', Raw}, Fields) ->
    #field{alias = Alias} = parse_field_format(File, SQL, Table, Operation, Fields, Name),
    lists:concat([type:to_list(Alias), " ", Compare, " ", Raw, ""]);

%% normal compare => binding param
parse_having_compare_literal(File, SQL, Table, Operation, Name, Compare, {'$param$', []}, Fields) when Compare == '=' orelse Compare == '>' orelse Compare == '>=' orelse Compare == '<' orelse Compare == '<=' ->
    #field{alias = Alias, value = Format} = parse_field_format(File, SQL, Table, Operation, Fields, Name),
    lists:concat([type:to_list(Alias), " ", string:to_upper(type:to_list(Compare)), " ", "(", type:to_list(Format), ")"]);

%% other compare => binding param
parse_having_compare_literal(_, _, _, _, _, Compare, {'$param$', []}, _) ->
    erlang:throw(lists:flatten(io_lib:format("Compare `~ts` parameterize does not supported", [Compare])));

%% literal
parse_having_compare_literal(File, SQL, Table, Operation, Name, Compare, Literal, Fields) when Compare == in orelse Compare == not_in ->
    #field{alias = Alias} = parse_field_format(File, SQL, Table, Operation, Fields, Name),
    lists:concat([type:to_list(Alias), " ", string:to_upper(type:to_list(Compare)), " ", "(", type:to_list(db:join(Literal)), ")"]);

%% literal
parse_having_compare_literal(File, SQL, Table, Operation, Name, Compare, Literal, Fields) ->
    #field{alias = Alias} = parse_field_format(File, SQL, Table, Operation, Fields, Name),
    lists:concat([type:to_list(Alias), " ", string:to_upper(type:to_list(Compare)), " ", "'", Literal, "'"]).

%%%===================================================================
%%% filter having part
%%%===================================================================

%% spec having
parse_filter_having(File, SQL = #{having := Preset = #{}}, Table, Operation, Fields, Order) ->
    %% Condition = [parse_having_compare(File, Table, Name, Compare, Fields) || {Name, Compare} <- maps:to_list(Preset)],
    Condition = [parse_filter_having_compare(File, SQL, Table, Operation, Name, maps:get(Name, Preset), Fields) || Name <- Order],
    lists:concat([" ", "HAVING", " ", string:join([Option || Option <- Condition, Option =/= []], " AND ")]);

%% multi column
parse_filter_having(File, SQL = #{having := Preset = [_ | _]}, Table, Operation, Fields, _) when is_atom(hd(Preset)) orelse is_list(hd(Preset)) orelse is_binary(hd(Preset)) ->
    Condition = [lists:concat(["`", Name, "`", " = ", (parse_field_format(File, SQL, Table, Operation, Fields, Name))#field.value]) || Name <- Preset],
    lists:concat([" ", "HAVING",  " ", string:join([Option || Option <- Condition, Option =/= []], " AND ")]);

%% without having
parse_filter_having(_, #{}, _, _, _, _) ->
    lists:concat([]).


%% spec compare or literal
parse_filter_having_compare(File, SQL, Table, Operation, Name, Preset = #{}, Fields) ->
    Condition = [parse_filter_having_compare_literal(File, SQL, Table, Operation, Name, Compare, Value, Fields) || {Compare, Value} <- maps:to_list(Preset)],
    string:join([Option || Option <- Condition, Option =/= []], " AND ");

%% normal compare
parse_filter_having_compare(_, _, _, _, _, Compare, _) when Compare == '=' orelse Compare == '>' orelse Compare == '>=' orelse Compare == '<' orelse Compare == '<=' ->
    lists:concat([]);

%% other compare
parse_filter_having_compare(_, _, _, _, _, Compare, _) ->
    erlang:throw(lists:flatten(io_lib:format("Compare `~ts` parameterize does not supported", [Compare]))).

%% raw
parse_filter_having_compare_literal(File, SQL, Table, Operation, Name, Compare, {'$raw$', Raw}, Fields) ->
    #field{alias = Alias} = parse_field_format(File, SQL, Table, Operation, Fields, Name),
    lists:concat([type:to_list(Alias), " ", Compare, " ", Raw, ""]);

%% normal compare => binding param
parse_filter_having_compare_literal(_, _, _, _, _, Compare, {'$param$', []}, _) when Compare == '=' orelse Compare == '>' orelse Compare == '>=' orelse Compare == '<' orelse Compare == '<=' ->
    lists:concat([]);

%% other compare => binding param
parse_filter_having_compare_literal(_, _, _, _, _, Compare, {'$param$', []}, _) ->
    erlang:throw(lists:flatten(io_lib:format("Compare `~ts` parameterize does not supported", [Compare])));

%% literal
parse_filter_having_compare_literal(File, SQL, Table, Operation, Name, Compare, Literal, Fields) when Compare == in orelse Compare == not_in ->
    #field{alias = Alias} = parse_field_format(File, SQL, Table, Operation, Fields, Name),
    lists:concat([type:to_list(Alias), " ", string:to_upper(type:to_list(Compare)), " ", "(", type:to_list(db:join(Literal)), ")"]);

%% literal
parse_filter_having_compare_literal(File, SQL, Table, Operation, Name, Compare, Literal, Fields) ->
    #field{alias = Alias} = parse_field_format(File, SQL, Table, Operation, Fields, Name),
    lists:concat([type:to_list(Alias), " ", string:to_upper(type:to_list(Compare)), " ", "'", Literal, "'"]).

%%%===================================================================
%%% order by part
%%%===================================================================

%% order by spec order
parse_order_by(_, #{order_by := Preset = #{}}, _, _, Order) ->
    %% OrderBy = [lists:concat(["`", Field, "`", " ", string:to_upper(type:to_list(Sort))]) || {Field, Sort} <- maps:to_list(Preset)],
    OrderBy = [lists:concat(["`", Field, "`", " ", string:to_upper(type:to_list(maps:get(Field, Preset)))]) || Field <- Order],
    lists:concat([" ", "ORDER BY ", string:join(OrderBy, ", ")]);

%% order by multi column
parse_order_by(_, #{order_by := Preset = [_ | _]}, _, _, _) ->
    OrderBy = [lists:concat(["`", Field, "`", " ", "ASC"]) || Field <- maps:to_list(Preset)],
    lists:concat([" ", "ORDER BY ", string:join(OrderBy, ", ")]);

%% order by single column
parse_order_by(_, #{order_by := Field}, _, _, _) ->
    lists:concat([" ", "ORDER BY `", Field, "` ASC"]);

%% without order
parse_order_by(_, #{}, _, _, _) ->
    lists:concat([]).

%%%===================================================================
%%% sort by part
%%%===================================================================

%% sort by spec sort
parse_sort_by(_, #{sort_by := Preset = #{}}, _, _, Sort) ->
    %% SortBy = [lists:concat(["`", Field, "`", " ", string:to_upper(type:to_list(Sort))]) || {Field, Sort} <- maps:to_list(Preset)],
    SortBy = [lists:concat(["`", Field, "`", " ", string:to_upper(type:to_list(maps:get(Field, Preset)))]) || Field <- Sort],
    lists:concat([" ", "ORDER BY ", string:join(SortBy, ", ")]);

%% sort by multi column
parse_sort_by(_, #{sort_by := Preset = [_ | _]}, _, _, _) ->
    SortBy = [lists:concat(["`", Field, "`", " ", "ASC"]) || Field <- maps:to_list(Preset)],
    lists:concat([" ", "ORDER BY ", string:join(SortBy, ", ")]);

%% sort by single column
parse_sort_by(_, #{sort_by := Field}, _, _, _) ->
    lists:concat([" ", "ORDER BY `", Field, "` ASC"]);

%% without sort
parse_sort_by(_, #{}, _, _, _) ->
    lists:concat([]).

%%%===================================================================
%%% limit part
%%%===================================================================

%% with limit
parse_limit(_, #{limit := Limit}, _, _) ->
    lists:concat([" ", "LIMIT ", Limit]);

%% without limit
parse_limit(_, #{}, _, _) ->
    lists:concat([]).

%%%===================================================================
%%% offset part
%%%===================================================================

%% with offset
parse_offset(_, #{offset := Offset}, _, _) ->
    lists:concat([" ", "OFFSET ", Offset]);

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
parse_key_having(File, SQL = #{having := Preset = #{}}, Table, Operation, Fields, Order) ->
    %% lists:append([parse_key_compare(File, Table, Name, Compare, Fields) || {Name, Compare} <- maps:to_list(Preset)]);
    lists:append([parse_key_compare(File, SQL, Table, Operation, Name, maps:get(Name, Preset), Fields) || Name <- Order]);

%% multi column
parse_key_having(File, SQL = #{having := Preset = [_ | _]}, Table, Operation, Fields, _) when is_atom(hd(Preset)) orelse is_list(hd(Preset)) orelse is_binary(hd(Preset)) ->
    [(parse_field_type(File, SQL, Table, Operation, Fields, Name))#field{alias = word:to_hump(Name)} || Name <- Preset];

%% single column
parse_key_having(File, SQL = #{having := Preset}, Table, Operation, Fields, _) when is_atom(Preset) orelse is_list(Preset) orelse is_binary(Preset) ->
    Field = parse_field_type(File, SQL, Table, Operation, Fields, Preset),
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

parse_field_value(#field{preset = {'$raw$', Raw}}) ->
    erlang:throw(lists:flatten(io_lib:format("Sql doest not support `raw`: ~tp operation", [Raw])));

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

parse_field_value(Field = #field{alias = Alias}) ->
    Field#field{value = type:to_list(Alias)}.


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
    Field#field{value = "float()"};

parse_field_type(Field = #field{format = <<"enum">>}) ->
    Field#field{value = "atom()"};

parse_field_type(Field = #field{format = <<"set">>}) ->
    Field#field{value = "[atom()]"}.

%%%===================================================================
%%% format spec part
%%%===================================================================
format_key_spec(_, _, _, _, _, _, #field{alias = Alias, value = Value}) ->
    lists:concat([db:to_hump(Alias), " :: ", Value]).

format_spec(File, SQL, {'$all$', All}, Table, Fields, FunctionName, ValueFields) ->
    lists:concat(["[", format_spec(File, SQL, All, Table, Fields, FunctionName, ValueFields), "]"]);

format_spec(_, _, {'$avg$', _}, _, _, _, _) ->
    lists:concat(["number()"]);

format_spec(_, _, {'$bit_and$', _}, _, _, _, _) ->
    lists:concat(["integer()"]);

format_spec(_, _, {'$bit_or$', _}, _, _, _, _) ->
    lists:concat(["integer()"]);

format_spec(_, _, {'$bit_xor$', _}, _, _, _, _) ->
    lists:concat(["integer()"]);

format_spec(_, _, {'$count$', _}, _, _, _, _) ->
    lists:concat(["non_neg_integer()"]);

format_spec(_, _, {'$max$', _}, _, _, _, ValueFields) ->
    Types = listing:unique([Value || #field{value = Value} <- ValueFields]),
    lists:concat(["[", string:join(Types, " | "), "]"]);

format_spec(_, _, {'$min$', _}, _, _, _, ValueFields) ->
    Types = listing:unique([Value || #field{value = Value} <- ValueFields]),
    lists:concat(["[", string:join(Types, " | "), "]"]);

format_spec(_, _, {'$std$', _}, _, _, _, _) ->
    lists:concat(["float()"]);

format_spec(_, _, {'$std_dev$', _}, _, _, _, _) ->
    lists:concat(["float()"]);

format_spec(_, _, {'$std_dev_pop$', _}, _, _, _, _) ->
    lists:concat(["float()"]);

format_spec(_, _, {'$std_dev_sample$', _}, _, _, _, _) ->
    lists:concat(["float()"]);

format_spec(_, _, {'$sum$', _}, _, _, _, _) ->
    lists:concat(["float()"]);

format_spec(_, _, {'$var_pop$', _}, _, _, _, _) ->
    lists:concat(["float()"]);

format_spec(_, _, {'$var_sample$', _}, _, _, _, _) ->
    lists:concat(["float()"]);

format_spec(_, _, {'$variance$', _}, _, _, _, _) ->
    lists:concat(["float()"]);


%% the list spec
format_spec(_, _, {'$list$', _}, _, _, _, ValueFields) ->
    Types = listing:unique([Value || #field{value = Value} <- ValueFields]),
    lists:concat(["[", string:join(Types, " | "), "]"]);

%% the list spec
format_spec(_, _, Select, _, _, _, ValueFields) when is_list(Select) ->
    Types = listing:unique([Value || #field{value = Value} <- ValueFields]),
    lists:concat(["[", string:join(Types, " | "), "]"]);

%% the map spec
format_spec(_, _, {'$map$', _}, _, _, _, ValueFields) ->
    Types = listing:unique([lists:concat(["#{atom() => ", Value, "}"]) || #field{value = Value} <- ValueFields]),
    string:join(Types, " | ");

%% the record spec
format_spec(_, _, {'$record$', _}, Table, _, _, _) ->
    lists:concat(["#", Table, "{}"]);

%% the tuple spec
format_spec(_, _, {'$tuple$', _}, _, _, _, ValueFields) ->
    Types = [Value || #field{value = Value} <- ValueFields],
    lists:concat(["{", string:join(Types, ", "), "}"]);

%% the tuple spec
format_spec(_, _, Select, _, _, _, ValueFields) when is_tuple(Select) ->
    Types = [Value || #field{value = Value} <- ValueFields],
    lists:concat(["{", string:join(Types, ", "), "}"]);

format_spec(_, _, _, _, _, _, ValueFields) ->
    Types = listing:unique([Value || #field{value = Value} <- ValueFields]),
    string:join(Types, " | ").

%%%===================================================================
%%% format value part
%%%===================================================================

%% value row
format_row(_, _, _, _, _, _, _, _, _, _, [], List) ->
    lists:reverse(List);

%% recursive
format_row(File, SQL, Select, Table, Fields, FunctionName, Range, Depth, Total, Number, [{Key, Value = [{_, _} | _]} | Rows], List) ->
    ValueList = format_row(File, SQL, Select, Table, Fields, FunctionName, Range, Depth + 1, length(Value), 1, Value, []),
    ValueData = lists:concat([
        string:join(ValueList, ",\n")
    ]),
    Pair = format_key(File, SQL, Select, Table, Fields, FunctionName, Range, Depth, false, Total, Number, Key, ValueData, [], []),
    format_row(File, SQL, Select, Table, Fields, FunctionName, Range, Depth, Total, Number + 1, Rows, [Pair | List]);

format_row(File, SQL, Select = {'$all$', _}, Table, Fields, FunctionName, Range, Depth, Total, Number, [{Key, Value} | Rows], List) ->
    %% maybe has many
    ValueList = [format_column(File, SQL, Select, Table, Fields, Depth + 1, Sub, []) || Sub <- Value],
    %% concat with [ ... ]
    case lists:flatlength(ValueList) =< 120 of
        true ->
            ValueData = lists:concat(["[", string:join(ValueList, ", "), "]"]);
        false ->
            FrontPadding = lists:duplicate(Depth + 1, "    "),
            BackPadding = lists:duplicate(Depth, "    "),
            ValueData = lists:concat(["[", "\n", FrontPadding, string:join(ValueList, ",\n" ++ FrontPadding), "\n", BackPadding, "]"])
    end,
    Pair = format_key(File, SQL, Select, Table, Fields, FunctionName, Range, Depth, true, Total, Number, Key, ValueData, [], []),
    format_row(File, SQL, Select, Table, Fields, FunctionName, Range, Depth, Total, Number + 1, Rows, [Pair | List]);

format_row(File, SQL, Select, Table, Fields, FunctionName, Range, Depth, Total, Number, [{Key, Value} | Rows], List) ->
    %% maybe has many
    ValueList = [format_column(File, SQL, Select, Table, Fields, Depth, Sub, []) || Sub <- Value],
    %% concat with [ ... ]
    ValueData = string:join(ValueList, ", "),
    Pair = format_key(File, SQL, Select, Table, Fields, FunctionName, Range, Depth, true, Total, Number, Key, ValueData, [], []),
    format_row(File, SQL, Select, Table, Fields, FunctionName, Range, Depth, Total, Number + 1, Rows, [Pair | List]).


%% key
format_key(_, _, _, _, _, FunctionName, _, Depth, _, _, _, [], Value, Match, []) ->
    Args = string:join(lists:reverse(Match), ", "),
    %% padding
    Align = lists:duplicate(Depth, "    "),
    lists:concat([
        FunctionName, "(", Args, ") ->", "\n",
        Align, Value
    ]);

format_key(_, _, _, _, _, FunctionName, _, Depth, _, _, _, [], Value, Match, Guard) ->
    Args = string:join(lists:reverse(Match), ", "),
    Condition = string:join(lists:reverse(Guard), " andalso "),
    %% padding
    Align = lists:duplicate(Depth, "    "),
    lists:concat([
        FunctionName, "(", Args, ") when ", Condition, " ", "->", "\n",
        Align, Value
    ]);


format_key(File, SQL, Select, Table, Fields, FunctionName, Range, Depth, Leaf, Total, Number, [Key = #field{alias = Alias, preset = '>'} | Keys], Value, Match, Guard) ->
    HumpName = db:to_hump(Alias),
    KeyData = format_term([], Key),
    Arg = lists:concat([HumpName, " > ", KeyData]),
    format_key(File, SQL, Select, Table, Fields, FunctionName, Range, Depth, Leaf, Total, Number, Keys, Value, [HumpName | Match], [Arg | Guard]);

format_key(File, SQL, Select, Table, Fields, FunctionName, Range, Depth, Leaf, Total, Number, [Key = #field{alias = Alias, preset = '<'} | Keys], Value, Match, Guard) ->
    HumpName = db:to_hump(Alias),
    KeyData = format_term([], Key),
    Arg = lists:concat([HumpName, " < ", KeyData]),
    format_key(File, SQL, Select, Table, Fields, FunctionName, Range, Depth, Leaf, Total, Number, Keys, Value, [HumpName | Match], [Arg | Guard]);

format_key(File, SQL, Select, Table, Fields, FunctionName, Range, Depth, Leaf, Total, Number, [Key = #field{alias = Alias, preset = '>='} | Keys], Value, Match, Guard) ->
    HumpName = db:to_hump(Alias),
    KeyData = format_term([], Key),
    Arg = lists:concat([HumpName, " >= ", KeyData]),
    format_key(File, SQL, Select, Table, Fields, FunctionName, Range, Depth, Leaf, Total, Number, Keys, Value, [HumpName | Match], [Arg | Guard]);

format_key(File, SQL, Select, Table, Fields, FunctionName, Range, Depth, Leaf, Total, Number, [Key = #field{alias = Alias, preset = '<='} | Keys], Value, Match, Guard) ->
    HumpName = db:to_hump(Alias),
    KeyData = format_term([], Key),
    Arg = lists:concat([HumpName, " =< ", KeyData]),
    format_key(File, SQL, Select, Table, Fields, FunctionName, Range, Depth, Leaf, Total, Number, Keys, Value, [HumpName | Match], [Arg | Guard]);

format_key(File, SQL, Select, Table, Fields, FunctionName, Range, Depth, Leaf, Total, Number, [Key = #field{alias = Alias, preset = '=<'} | Keys], Value, Match, Guard) ->
    HumpName = db:to_hump(Alias),
    KeyData = format_term([], Key),
    Arg = lists:concat([HumpName, " =< ", KeyData]),
    format_key(File, SQL, Select, Table, Fields, FunctionName, Range, Depth, Leaf, Total, Number, Keys, Value, [HumpName | Match], [Arg | Guard]);

format_key(File, SQL, Select, Table, Fields, FunctionName, Range, Depth, Leaf, Total, Number, [Key = #field{} | Keys], Value, Match, Guard) ->
    KeyData = format_term([], Key),
    format_key(File, SQL, Select, Table, Fields, FunctionName, Range, Depth, Leaf, Total, Number, Keys, Value, [KeyData | Match], Guard).


%% format column
format_column(_, _, {'$all$', _}, _, _, _, [], List) ->
    string:join(lists:reverse(List), ", ");

format_column(_, _, {'$avg$', _}, _, _, _, [], List) ->
    string:join(lists:reverse(List), ", ");

format_column(_, _, {'$bit_and$', _}, _, _, _, [], List) ->
    string:join(lists:reverse(List), ", ");

format_column(_, _, {'$bit_or$', _}, _, _, _, [], List) ->
    string:join(lists:reverse(List), ", ");

format_column(_, _, {'$bit_xor$', _}, _, _, _, [], List) ->
    string:join(lists:reverse(List), ", ");

format_column(_, _, {'$count$', _}, _, _, _, [], List) ->
    string:join(lists:reverse(List), ", ");

format_column(_, _, {'$max$', _}, _, _, _, [], List) ->
    string:join(lists:reverse(List), ", ");

format_column(_, _, {'$min$', _}, _, _, _, [], List) ->
    string:join(lists:reverse(List), ", ");

format_column(_, _, {'$std$', _}, _, _, _, [], List) ->
    string:join(lists:reverse(List), ", ");

format_column(_, _, {'$std_dev$', _}, _, _, _, [], List) ->
    string:join(lists:reverse(List), ", ");

format_column(_, _, {'$std_dev_pop$', _}, _, _, _, [], List) ->
    string:join(lists:reverse(List), ", ");

format_column(_, _, {'$std_dev_sample$', _}, _, _, _, [], List) ->
    string:join(lists:reverse(List), ", ");

format_column(_, _, {'$sum$', _}, _, _, _, [], List) ->
    string:join(lists:reverse(List), ", ");

format_column(_, _, {'$var_sample$', _}, _, _, _, [], List) ->
    string:join(lists:reverse(List), ", ");

format_column(_, _, {'$variance$', _}, _, _, _, [], List) ->
    string:join(lists:reverse(List), ", ");

format_column(_, _, {'$list$', _}, _, _, _, [], List) ->
    lists:concat(["[", string:join(lists:reverse(List), ", "), "]"]);

format_column(_, _, Select, _, _, _, [], List) when is_list(Select) ->
    lists:concat(["[", string:join(lists:reverse(List), ", "), "]"]);

format_column(_, _, {'$map$', _}, _, _, _, [], List) ->
    lists:concat(["#", "{", string:join(lists:reverse(List), ", "), "}"]);

format_column(_, _, Select, _, _, _, [], List) when is_map(Select) ->
    lists:concat(["#", "{", string:join(lists:reverse(List), ", "), "}"]);

format_column(_, _, {'$record$', _}, Table, _, _, [], List) ->
    lists:concat(["#", Table, "{", string:join(lists:reverse(List), ", "), "}"]);

format_column(_, _, {'$tuple$', _}, _, _, _, [], List) ->
    lists:concat(["{", string:join(lists:reverse(List), ", "), "}"]);

format_column(_, _, Select, _, _, _, [], List) when is_tuple(Select) ->
    lists:concat(["{", string:join(lists:reverse(List), ", "), "}"]);

format_column(_, _, _, _, _, _, [], List) ->
    string:join(List, ", ");

%% all
format_column(File, SQL, {'$all$', All}, Table, Fields, Depth, Columns, List) ->
    format_column(File, SQL, All, Table, Fields, Depth, Columns, List);

format_column(File, SQL, Select = {'$avg$', _}, Table, Fields, Depth, [Column | Columns], List) ->
    Value = lists:concat([format_term(Select, Column)]),
    format_column(File, SQL, Select, Table, Fields, Depth, Columns, [Value | List]);

format_column(File, SQL, Select = {'$bit_and$', _}, Table, Fields, Depth, [Column | Columns], List) ->
    Value = lists:concat([format_term(Select, Column)]),
    format_column(File, SQL, Select, Table, Fields, Depth, Columns, [Value | List]);

format_column(File, SQL, Select = {'$bit_or$', _}, Table, Fields, Depth, [Column | Columns], List) ->
    Value = lists:concat([format_term(Select, Column)]),
    format_column(File, SQL, Select, Table, Fields, Depth, Columns, [Value | List]);

format_column(File, SQL, Select = {'$bit_xor$', _}, Table, Fields, Depth, [Column | Columns], List) ->
    Value = lists:concat([format_term(Select, Column)]),
    format_column(File, SQL, Select, Table, Fields, Depth, Columns, [Value | List]);

format_column(File, SQL, Select = {'$count$', _}, Table, Fields, Depth, [Column | Columns], List) ->
    Value = lists:concat([format_term(Select, Column)]),
    format_column(File, SQL, Select, Table, Fields, Depth, Columns, [Value | List]);

format_column(File, SQL, Select = {'$max$', _}, Table, Fields, Depth, [Column | Columns], List) ->
    Value = lists:concat([format_term(Select, Column)]),
    format_column(File, SQL, Select, Table, Fields, Depth, Columns, [Value | List]);

format_column(File, SQL, Select = {'$min$', _}, Table, Fields, Depth, [Column | Columns], List) ->
    Value = lists:concat([format_term(Select, Column)]),
    format_column(File, SQL, Select, Table, Fields, Depth, Columns, [Value | List]);

format_column(File, SQL, Select = {'$std$', _}, Table, Fields, Depth, [Column | Columns], List) ->
    Value = lists:concat([format_term(Select, Column)]),
    format_column(File, SQL, Select, Table, Fields, Depth, Columns, [Value | List]);

format_column(File, SQL, Select = {'$std_dev$', _}, Table, Fields, Depth, [Column | Columns], List) ->
    Value = lists:concat([format_term(Select, Column)]),
    format_column(File, SQL, Select, Table, Fields, Depth, Columns, [Value | List]);

format_column(File, SQL, Select = {'$std_dev_pop$', _}, Table, Fields, Depth, [Column | Columns], List) ->
    Value = lists:concat([format_term(Select, Column)]),
    format_column(File, SQL, Select, Table, Fields, Depth, Columns, [Value | List]);

format_column(File, SQL, Select = {'$std_dev_sample$', _}, Table, Fields, Depth, [Column | Columns], List) ->
    Value = lists:concat([format_term(Select, Column)]),
    format_column(File, SQL, Select, Table, Fields, Depth, Columns, [Value | List]);

format_column(File, SQL, Select = {'$sum$', _}, Table, Fields, Depth, [Column | Columns], List) ->
    Value = lists:concat([format_term(Select, Column)]),
    format_column(File, SQL, Select, Table, Fields, Depth, Columns, [Value | List]);

format_column(File, SQL, Select = {'$var_sample$', _}, Table, Fields, Depth, [Column | Columns], List) ->
    Value = lists:concat([format_term(Select, Column)]),
    format_column(File, SQL, Select, Table, Fields, Depth, Columns, [Value | List]);

format_column(File, SQL, Select = {'$variance$', _}, Table, Fields, Depth, [Column | Columns], List) ->
    Value = lists:concat([format_term(Select, Column)]),
    format_column(File, SQL, Select, Table, Fields, Depth, Columns, [Value | List]);

%% the map type
format_column(File, SQL, Select = {'$map$', _}, Table, Fields, Depth, [Column = #field{alias = Alias} | Columns], List) ->
    Value = format_term(Select, Column),
    %% concat with key => value
    Pair = lists:concat([db:to_snake(Alias), " => ", Value]),
    format_column(File, SQL, Select, Table, Fields, Depth, Columns, [Pair | List]);

%% the record type
format_column(File, SQL, Select = {'$record$', _}, Table, Fields, Depth, [Column = #field{alias = Alias} | Columns], List) ->
    Value = format_term(Select, Column),
    %% concat with key = value
    Pair = lists:concat([db:to_snake(Alias), " = ", Value]),
    format_column(File, SQL, Select, Table, Fields, Depth, Columns, [Pair | List]);

%% other type
format_column(File, SQL, Select, Table, Fields, Depth, [Column | Columns], List) ->
    Value = lists:concat([format_term(Select, Column)]),
    format_column(File, SQL, Select, Table, Fields, Depth, Columns, [Value | List]).


%% value to term
format_term({'$all$', All}, Field) ->
    format_term(All, Field);

format_term({'$avg$', _}, #field{value = Value}) ->
    lists:concat([type:to_integer(Value)]);

format_term({'$bit_and$', _}, #field{value = Value}) ->
    lists:concat([type:to_integer(Value)]);

format_term({'$bit_or$', _}, #field{value = Value}) ->
    lists:concat([type:to_integer(Value)]);

format_term({'$bit_xor$', _}, #field{value = Value}) ->
    lists:concat([type:to_integer(Value)]);

format_term({'$count$', _}, #field{value = Value}) ->
    lists:concat([type:to_integer(Value)]);

format_term({'$max$', Max}, Field) ->
    format_term(Max, Field);

format_term({'$min$', Min}, Field) ->
    format_term(Min, Field);

format_term({'$std$', _}, #field{value = Value}) ->
    lists:concat([type:to_float(Value)]);

format_term({'$std_dev$', _}, #field{value = Value}) ->
    lists:concat([type:to_float(Value)]);

format_term({'$std_dev_pop$', _}, #field{value = Value}) ->
    lists:concat([type:to_float(Value)]);

format_term({'$std_dev_sample$', _}, #field{value = Value}) ->
    lists:concat([type:to_float(Value)]);

format_term({'$var_pop$', _}, #field{value = Value}) ->
    lists:concat([type:to_float(Value)]);

format_term({'$var_sample$', _}, #field{value = Value}) ->
    lists:concat([type:to_float(Value)]);

format_term({'$variance$', _}, #field{value = Value}) ->
    lists:concat([type:to_float(Value)]);

format_term(_, #field{format = <<"boolean">>, value = Value}) ->
    lists:concat([type:to_boolean(Value)]);

format_term(_, #field{format = <<"tinyint">>, type = <<"unsigned">>, value = Value}) ->
    lists:concat([type:to_integer(Value)]);

format_term(_, #field{format = <<"smallint">>, type = <<"unsigned">>, value = Value}) ->
    lists:concat([type:to_integer(Value)]);

format_term(_, #field{format = <<"int">>, type = <<"unsigned">>, value = Value}) ->
    lists:concat([type:to_integer(Value)]);

format_term(_, #field{format = <<"bigint">>, type = <<"unsigned">>, value = Value}) ->
    lists:concat([type:to_integer(Value)]);

format_term(_, #field{format = <<"tinyint">>, value = Value}) ->
    lists:concat([type:to_integer(Value)]);

format_term(_, #field{format = <<"smallint">>, value = Value}) ->
    lists:concat([type:to_integer(Value)]);

format_term(_, #field{format = <<"int">>, value = Value}) ->
    lists:concat([type:to_integer(Value)]);

format_term(_, #field{format = <<"bigint">>, value = Value}) ->
    lists:concat([type:to_integer(Value)]);

format_term(_, #field{format = <<"decimal">>, value = Value}) ->
    lists:concat([type:to_float(Value)]);

format_term(_, #field{format = <<"char">>, value = Value}) ->
    lists:concat(["<<\"", unicode:characters_to_list(type:to_binary(Value)), "\"/utf8>>"]);

format_term(_, #field{format = <<"varchar">>, value = Value}) ->
    unicode:characters_to_list(type:to_binary(Value));

format_term(_, #field{format = <<"enum">>, value = Value}) ->
    lists:concat([type:to_list(Value)]);

format_term(_, #field{format = <<"set">>, value = Value}) ->
    lists:concat(["[", [type:to_list(Option) || Option <- Value], "]"]).

%%%===================================================================
%%% format default part
%%%===================================================================

%% no default
format_default(_, SQL, _, _, KeyFields, FunctionName) when not is_map_key(default, SQL) ->
    Args = string:join(lists:duplicate(length(KeyFields), "_"), ", "),
    lists:concat([
        FunctionName, "(", Args, ") ->", "\n",
        "    ", "undefined", "."
    ]);

format_default(_, #{default := {'$list$', _}}, _, _, KeyFields, FunctionName) ->
    Args = string:join(lists:duplicate(length(KeyFields), "_"), ", "),
    lists:concat([
        FunctionName, "(", Args, ") ->", "\n",
        "    ", "[]", "."
    ]);

format_default(_, #{default := {'$map$', _}}, _, _, KeyFields, FunctionName) ->
    Args = string:join(lists:duplicate(length(KeyFields), "_"), ", "),
    lists:concat([
        FunctionName, "(", Args, ") ->", "\n",
        "    ", "#", "{}", "."
    ]);

format_default(_, #{default := {'$record$', _}}, _, Table, KeyFields, FunctionName) ->
    Args = string:join(lists:duplicate(length(KeyFields), "_"), ", "),
    lists:concat([
        FunctionName, "(", Args, ") ->", "\n",
        "    ", "#", Table, "{}", "."
    ]);

format_default(_, #{default := {'$tuple$', _}}, _, _, KeyFields, FunctionName) ->
    Args = string:join(lists:duplicate(length(KeyFields), "_"), ", "),
    lists:concat([
        FunctionName, "(", Args, ") ->", "\n",
        "    ", "{}", "."
    ]);

format_default(_, #{default := Default}, _, _, KeyFields, FunctionName) ->
    Args = string:join(lists:duplicate(length(KeyFields), "_"), ", "),
    lists:concat([
        FunctionName, "(", Args, ") ->", "\n",
        "    ", type:to_list(Default), "."
    ]).

%%%===================================================================
%%% collect data part
%%%===================================================================

%% n degree array product
compose([Head]) ->
    [lists:flatten([X]) || X <- Head];

compose([Head, Tail]) ->
    [lists:flatten([X, Y]) || X <- Head, Y <- Tail];

compose([Head | Tail]) ->
    [lists:flatten([X, Y]) || X <- Head, Y <- compose(Tail)].


%% collect value by key
collect_data([], _, _, _, List) ->
    lists:reverse(List);

collect_data([Binding | Tail], Sql, KeyFields, ValueFields, List) ->
    RawValue = db:select(Sql, Binding),
    %% filter empty value
    case RawValue of
        [] ->
            collect_data(Tail, Sql, KeyFields, ValueFields, List);
        _ ->
            Key = [Field#field{value = Column} || {Column, Field} <- lists:zip(Binding, KeyFields)],
            Value = [[Field#field{value = Column} || {Column, Field} <- lists:zip(Row, ValueFields)] || Row <- RawValue],
            collect_data(Tail, Sql, KeyFields, ValueFields, [{Key, Value} | List])
    end.

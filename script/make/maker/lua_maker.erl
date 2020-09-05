%%%-------------------------------------------------------------------
%%% @doc
%%% make database data to lua table
%%% @end
%%%-------------------------------------------------------------------
-module(lua_maker).
-export([start/1]).
-record(field, {name, default, type, format, comment, position, key, extra}).
%%%===================================================================
%%% API functions
%%%===================================================================
%% @doc for shell
start(List) ->
    maker:start(fun parse_table/2, List).

%%%===================================================================
%%% Internal functions
%%%===================================================================
%% @doc parse table
parse_table(DataBase, {File, List}) ->
    parse_table(DataBase, {File, [], List});
parse_table(DataBase, {File, _, List}) ->
    Code = lists:flatten(string:join([parse_code(DataBase, Sql, Name) || {Sql, Name} <- List], ",\n")),
    Name = word:to_lower_hump(filename:basename(File, ".lua")),
    All = lists:concat(["local ", Name, " = {\n", Code, "\n}"]),
    [{"(?s).*", All}].

parse_code(DataBase, Sql, Name) ->
    [ValueBlock, TableBlock, KeyBlock, GroupBlock, OrderBlock, LimitBlock] = parse_sql(Sql),
    %% collect data
    Table = extract(TableBlock, "\\w+"),
    %% parse value type
    {Value, Type, TypeLeft, TypeRight} = parse_type(Table, ValueBlock),
    %% collect table fields
    AllFields = collect_fields(DataBase, Table),
    %% parse key and make key format
    KeyFormat = parse_keys(AllFields, KeyBlock),
    %% collect value fields
    ValueFields = collect_value_fields(AllFields, Value),
    %% parse values and make values format
    ValueFormat = parse_values(ValueFields, Type, TypeLeft, TypeRight),
    %% collect from database
    {KeyData, ValueData} = collect_data(TableBlock, KeyFormat, GroupBlock, OrderBlock, LimitBlock, ValueFields),
    %% format data code
    format_code(Name, KeyFormat, KeyData, ValueFormat, ValueData, string:strip(GroupBlock)).

parse_sql(Sql) ->
    %% parse sql sentence
    [ValueBlock]   = extract(Sql, "(?i)(?<=\\bSELECT\\b).*?(?=\\bAS\\b|\\bFROM\\b)"),
    %% [FormatBlock]  = extract(Sql, "(?i)(?<=\\bAS\\b).*?(?=\\bFROM\\b|\\bWHERE\\b|\\bGROUP BY\\b|\\bORDER BY\\b|\\bLIMIT\\b|\\bMULTI\\b|\\bDEFAULT\\b|;|$)"),
    [TableBlock]   = extract(Sql, "(?i)(?<=\\bFROM\\b).*?(?=\\bWHERE\\b|\\bGROUP BY\\b|\\bORDER BY\\b|\\bLIMIT\\b|\\bMULTI\\b|\\bDEFAULT\\b|;|$)"),
    [KeyBlock]     = extract(Sql, "(?i)(?<=\\bWHERE\\b).*?(?=\\bGROUP BY\\b|\\bORDER BY\\b|\\bLIMIT\\b|\\bMULTI\\b|\\bDEFAULT\\b|;|$)"),
    [GroupBlock]   = extract(Sql, "(?i)\\bGROUP BY\\b.*?(?=\\bORDER BY\\b|\\bLIMIT|\\bMULTI\\b|\\bDEFAULT\\b|;|$)"),
    [OrderBlock]   = extract(Sql, "(?i)\\bORDER BY\\b.*?(?=\\bLIMIT|\\bMULTI\\b|\\bDEFAULT\\b|;|$)"),
    [LimitBlock]   = extract(Sql, "(?i)\\bLIMIT\\b.*?(?=\\bMULTI\\b|\\bDEFAULT\\b|;|$)"),
    [string:strip(ValueBlock), string:strip(TableBlock), string:strip(KeyBlock), string:strip(GroupBlock), string:strip(OrderBlock), string:strip(LimitBlock)].

%% @doc parse value type
parse_type(_Table, String) ->
    TypeList = [{table, "{", "}", "(?<=\\{).*?(?=\\})"}],
    parse_type_loop(TypeList, String).

parse_type_loop([], String) ->
    {string:strip(String), origin, "", ""};
parse_type_loop([{Type, TypeLeft, TypeRight, RegEx} | T], String) ->
    case re:run(String, RegEx, [{capture, all, list}]) of
        nomatch ->
            parse_type_loop(T, String);
        {match, [Match]} ->
            {Match, Type, TypeLeft, TypeRight}
    end.

%% @doc collect value fields
collect_value_fields(Fields, "*") ->
    [Field || Field = #field{comment = Comment} <- Fields, string:str(Comment, "(server)") == 0];
collect_value_fields(Fields, Value) ->
    {match, List} = re:run(Value, "\\w+", [global, {capture, all, list}]),
    collect_value_fields_loop(List, Fields, []).

collect_value_fields_loop([], _Fields, List) ->
    lists:reverse(List);
collect_value_fields_loop([[Name] | T], Fields, List) ->
    case lists:keyfind(Name, #field.name, Fields) of
        false ->
            erlang:error(lists:flatten(io_lib:format("invalid key expression, no such value filed: ~s", [Name])));
        Field ->
            collect_value_fields_loop(T, Fields, [Field | List])
    end.

%% @doc get table field list
collect_fields(DataBase, Table) ->
    %% make fields sql
    FieldsSql = io_lib:format(<<"SELECT `COLUMN_NAME`, `COLUMN_DEFAULT`, `COLUMN_TYPE`, `DATA_TYPE`, `COLUMN_COMMENT`, `ORDINAL_POSITION`, `COLUMN_KEY`, `EXTRA` FROM information_schema.`COLUMNS` WHERE `TABLE_SCHEMA` = '~s' AND `TABLE_NAME` = '~s' ORDER BY `ORDINAL_POSITION`;">>, [DataBase, Table]),
    %% data revise
    Revise = fun
        (FieldInfo = #field{name = Name, format = <<"char">>, comment = Comment}) ->
            FieldInfo#field{name = binary_to_list(Name), format = "\"~s\"", default = "\"\"", comment = binary_to_list(Comment)};
        (FieldInfo = #field{name = Name, format = <<"varchar">>, comment = Comment}) ->
            FieldInfo#field{name = binary_to_list(Name), format = "~s", default = "{}", comment = binary_to_list(Comment)};
        (FieldInfo = #field{name = Name, comment = Comment}) ->
            FieldInfo#field{name = binary_to_list(Name), format = "~w", comment = binary_to_list(Comment)}
    end,
    %% fetch table fields
    parser:convert(maker:select(FieldsSql), field, Revise).

%% @doc parse key format
parse_keys(_, []) ->
    [];
parse_keys(Fields, KeyBlock) ->
    List = re:split(KeyBlock, "(?i)AND", [{return, list}]),
    [parse_key_expression(Expression, Fields) || Expression <- List].
parse_key_expression(Expression, Fields) ->
    [_, Operate, _] = re:split(Expression, "`?\\w+`?|'?\\w+'?", [{return, list}]),
    {match, [[Left], [Right]]} = re:run(Expression, "\\w+", [global, {capture, all, list}]),
    case {lists:keyfind(Left, #field.name, Fields), lists:keyfind(Right, #field.name, Fields)} of
        {#field{format = Format}, false} ->
            {Format, Left, string:strip(Operate), Right};
        {false, #field{format = Format}} ->
            %% compare operator revise
            Reserve = fun("<") -> ">";("<=") -> ">=";(">") -> "<";(">=") -> "=<";(O) -> O end,
            {Format, Right, Reserve(string:strip(Operate)), Left};
        _ ->
            erlang:error("invalid key expression, no such key filed: " ++ Expression)
    end.

%% @doc parse value format
parse_values(Fields, Type, Left, Right) ->
    lists:flatten(lists:concat([Left, string:join([parse_value_expression(Name, Format, Type) || #field{name = Name, format = Format} <- Fields], ", "), Right])).
parse_value_expression(Field, Format, table) ->
    lists:concat(["[\"", Field, "\"]", " = ", Format]);
parse_value_expression(_Field, Format, _) ->
    %% tuple/list/origin
    Format.

%% @doc collect value data group by key
collect_data(TableBlock, [], GroupBlock, OrderBlock, LimitBlock, ValueFields) ->
    %% replace erlang tuple to lua table
    %% case when length(`award`) = 0 then '[]' else replace(replace(`award`, '[', '{'), ']', '}') end as `award`
    %% IF(length(trim(`~s`)), replace(replace(`~s`, '[', '{'), ']', '}'), '{}') AS `~s`
    %% data revise empty string '' to empty table '{}'
    ValueBlock = string:join(lists:map(fun(#field{format = "~s", name = Name}) -> io_lib:format(" IF(length(trim(`~s`)), replace(replace(`~s`, '[', '{'), ']', '}'), '{}') AS `~s` ", [Name, Name, Name]); (#field{name = Name}) -> io_lib:format("`~s`", [Name]) end, ValueFields), ", "),
    ValueData = maker:select(io_lib:format("SELECT ~s FROM ~s ~s ~s ~s;", [ValueBlock, TableBlock, GroupBlock, OrderBlock, LimitBlock])),
    {[], ValueData};
collect_data(TableBlock, KeyFormat, GroupBlock, OrderBlock, LimitBlock, ValueFields) ->
    %% collect key data
    KeyFields = string:join(["`" ++ Name ++ "`" || {_, Name, _, _} <- KeyFormat], ", "),
    KeyData = maker:select(io_lib:format("SELECT ~s FROM ~s ~s ~s", [KeyFields, TableBlock, GroupBlock, OrderBlock])),
    %% collect value data
    KeyBlock = string:join([lists:concat(["`", Name, "` = '", hd(extract(Format, "~\\w")), "'"]) || {Format, Name, _, _} <- KeyFormat], " AND "),
    %% data revise empty string '' to empty table '{}'
    ValueBlock = string:join(lists:map(fun(#field{format = "~s", name = Name}) -> io_lib:format(" IF(length(trim(`~s`)), replace(replace(`~s`, '[', '{'), ']', '}'), '{}') AS `~s` ", [Name, Name, Name]); (#field{name = Name}) -> io_lib:format("`~s`", [Name]) end, ValueFields), ", "),
    ValueData = [listing:unique(maker:select(io_lib:format("SELECT ~s FROM ~s WHERE " ++ KeyBlock ++ " ~s ~s", [ValueBlock, TableBlock] ++ Key ++ [OrderBlock, LimitBlock]))) || Key <- KeyData],
    %% convert erl atom to lua string
    ReviseValueData = [[lists:zipwith(fun(#field{format = Format}, Field) -> revise(Format, Field) end, ValueFields, Row) || Row <- Values] || Values <- ValueData],
    {KeyData, ReviseValueData}.

%% revise erl atom to lua string
revise("~s", String) ->
    revise_loop(String, <<>>, false);
revise(_, String) ->
    String.

%% wrap word with double quote
revise_loop(<<>>, String, false) ->
    String;
revise_loop(<<>>, String, true) ->
    <<String/binary, $">>;
revise_loop(<<Word:8, Rest/binary>>, String, false) when ($a =< Word andalso Word =< $z) orelse ($A =< Word andalso Word =< $Z) orelse Word == $_ orelse Word == $- ->
    revise_loop(Rest, <<String/binary, $", Word:8>>, true);
revise_loop(<<Word:8, Rest/binary>>, String, true) when ($a =< Word andalso Word =< $z) orelse ($A =< Word andalso Word =< $Z) orelse Word == $_ orelse Word == $- ->
    revise_loop(Rest, <<String/binary, Word:8>>, true);
revise_loop(<<Word:8, Rest/binary>>, String, true) when ($0 =< Word andalso Word =< $9) ->
    revise_loop(Rest, <<String/binary, Word:8>>, true);
revise_loop(<<Word:8, Rest/binary>>, String, true) ->
    revise_loop(Rest, <<String/binary, $", Word:8>>, false);
revise_loop(<<Word:8, Rest/binary>>, String, Flag) ->
    revise_loop(Rest, <<String/binary, Word:8>>, Flag).

%% @doc format code
format_code(Name, _KeyFormat, [], ValueFormat, ValueData, _GroupBlock) ->
    %% no key make value data as list type
    io_lib:format("    [\"~s\"] = {~s}", [Name, format_value(ValueFormat, ValueData)]);
format_code(Name, KeyFormat, KeyData, ValueFormat, ValueData, GroupBlock) ->
    _ = length(KeyData) =/= length(ValueData) andalso erlang:error("data key/value set has different length"),
    List = lists:zipwith(fun(K, V) -> K ++ [V] end, KeyData, ValueData),
    KeyFormatList = [Format || {Format, _, _, _} <- KeyFormat],
    tree(List, KeyFormatList, ValueFormat, Name, GroupBlock).

%% tree code(lua table key/value type)
tree(List, KeyFormatList, Format, Name, Group) ->
    Result = tree(List, KeyFormatList, Format, 2, Group, []),
    io_lib:format("    [\"~s\"] = {~n~s~n    }", [Name, Result]).
tree([], _KeyFormatList, _Format, _Depth, _Group, Result) ->
    string:join(lists:reverse(Result), ",\n");
tree([[_, _] | _] = List, [KeyFormat | _], Format, Depth, [], _Result) ->
    Padding = lists:concat(lists:duplicate(Depth, "    ")),
    %% lua key [] revise
    string:join([io_lib:format(Padding ++ "[" ++ KeyFormat ++ "]" ++ " = ~s", [K, format_value(Format, V)]) || [K, V] <- List], ",\n");
tree([[_, _] | _] = List, [KeyFormat | _], Format, Depth, _Group, _Result) ->
    Padding = lists:concat(lists:duplicate(Depth, "    ")),
    %% group collect value as table type
    %% lua key [] revise
    string:join([io_lib:format(Padding ++ "[" ++ KeyFormat ++ "]" ++ " = {~s}", [K, format_value(Format, V)]) || [K, V] <- List], ",\n");
tree([[K | _] | _] = List, [KeyFormat | RemainFormatList] = KeyFormatList, Format, Depth, Group, Result) ->
    %% filter same key set
    {Target, Remain} = lists:partition(fun([X | _]) -> X == K end, List),
    %% next key
    Tree = tree([X || [_ | X] <- Target], RemainFormatList, Format, Depth + 1, Group, []),
    %% tree align padding
    Padding = lists:concat(lists:duplicate(Depth, "    ")),
    %% lua key [] revise
    New = io_lib:format(Padding ++ "[" ++ KeyFormat ++ "]" ++ " = {~n~s~n~s}", [K, Tree, Padding]),
    tree(Remain, KeyFormatList, Format, Depth, Group, [New | Result]).

%% format lua value
format_value(Format, ValueData) ->
    string:join([io_lib:format(Format, Value) || Value <- ValueData], ", ").

%%%===================================================================
%%% Common Tool
%%%===================================================================
%% extract string by regex
extract(String, RegEx) ->
    extract(String, RegEx, [{capture, all, list}]).
extract(String, RegEx, Option) ->
    case re:run(String, RegEx, Option) of
        nomatch ->
            [""];
        {match, Match} ->
            Match
    end.

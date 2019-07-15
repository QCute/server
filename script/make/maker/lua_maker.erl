%%%-------------------------------------------------------------------
%%% @doc
%%% module lua maker
%%% database data to lua term tool
%%% @end
%%%-------------------------------------------------------------------
-module(lua_maker).
-export([start/1]).
-export([parse/2]).
%% ------------------------ user guide -------------------------------
%%
%% sql      :: auto group by key(when key reduplicated)
%% varchar/char convert to list/string by default
%% use string specified to make bit string
%% (string) varchar/char -> <<"">>
%% text -> <<"">>
%%%===================================================================
%%% API
%%%===================================================================
%% @doc for shell
start(List) ->
    maker:start(fun parse_table/2, [List]).

%% @doc parse
parse(DataBase, One) ->
    parse_table(DataBase, One).
%% ====================================================================
%% Internal functions
%% ====================================================================
%% @doc parse table
parse_table(DataBase, {File, List}) ->
    Code = lists:flatten([parse_code(DataBase, Sql, Name) || {Sql, Name} <- List]),
    Name = filename:basename(File, ".lua"),
    All = lists:concat(["ConfigManager.InitConfig(\"", Name, "\",\n{\n", Code, "})"]),
    [{"(?s).*", All}].

parse_code(DataBase, Sql, Name) ->
    {TableBlock, KeyBlock, ValueBlock, OrderBlock, LimitBlock, Type} = parse_sql(Sql),
    FieldList = parse_field(DataBase, TableBlock),
    KeyFormat = parse_key(KeyBlock, FieldList),
    ValueFormat = parse_value(ValueBlock, FieldList),
    {KeyData, ValueData} = collect_data(TableBlock, KeyFormat, ValueBlock, OrderBlock, LimitBlock),
    %% map type only
    Format = parse_type_format(Type, ValueFormat),
    case length(KeyData) == length(ValueData) of
        true ->
            %% k/v type
            List = lists:zipwith(fun(K, V) -> K ++ [V] end, KeyData, ValueData),
            tree(List, ValueFormat, Format, Name);
        _ when KeyBlock == [] ->
            %% collect type
            KeyData ++ "\n    " ++ ValueData ++ ".\n\n"
    end.

%% @doc parse sql expression
parse_sql(Sql) ->
    {match, [ValueBlock]} = re:run(Sql, "(?i)(?<=\\bSELECT).*?(?=\\bFROM\\b)", [{capture, all, list}]),
    {match, [TableBlock]} = re:run(Sql, "(?i)(?<=\\bFROM\\b).*?(?=\\bWHERE\\b|\\bGROUP BY\\b|\\bORDER BY\\b|\\bLIMIT\\b|;|$)", [{capture, all, list}]),
    {match, [KeyBlock]} = max(re:run(Sql, "(?i)(?<=\\bWHERE\\b).*?(?=\\bGROUP BY\\b|\\bORDER BY\\b|\\bLIMIT\\b|;|$)", [{capture, all, list}]), {match, [""]}),
    {match, [OrderBlock]} = max(re:run(Sql, "(?i)\\bORDER BY\\b.*?(?=\\bLIMIT\\b|;|$)", [{capture, all, list}]), {match, [""]}),
    {match, [LimitBlock]} = max(re:run(Sql, "(?i)\\bLIMIT\\b.*?(?=;|$)", [{capture, all, list}]), {match, [""]}),
    {Type, Value} = parse_type(string:strip(ValueBlock)),
    {string:strip(TableBlock), string:strip(KeyBlock), Value, OrderBlock, LimitBlock, Type}.

%% @doc parse data type
parse_type(ValueBlock) ->
    List = [{"(?<=\\[).*?(?=\\])", list}, {"(?<=#record\\{).*?(?=\\})", record}, {"#\\w+\\{(.*?)\\}", record}, {"(?<=#\\{).*?(?=\\})", maps}, {"(?<=\\{).*?(?=\\})", tuple}, {"(?<=\\().*?(?=\\))", record}],
    parse_type(ValueBlock, List).
parse_type(Value, []) ->
    {origin, Value};
parse_type(ValueBlock, [{Pattern, Type} | T]) ->
    case re:run(ValueBlock, Pattern, [{capture, all, list}]) of
        {match, [Value]} ->
            {Type, Value};
        {match, [_, Value]} ->
            {Type, Value};
        _ ->
            parse_type(ValueBlock, T)
    end.

%% @doc get table FieldList
parse_field(DataBase, TableBlock) ->
    {match, [Table]} = re:run(TableBlock, "(?i)\\w+", [{capture, all, list}]),
    FieldList = maker:select(io_lib:format(<<"SELECT `COLUMN_NAME`, `COLUMN_DEFAULT`, `DATA_TYPE`, `COLUMN_COMMENT`, `ORDINAL_POSITION`, `COLUMN_KEY`, `EXTRA` FROM information_schema.`COLUMNS` WHERE `TABLE_SCHEMA` = '~s' AND TABLE_NAME = '~s' ORDER BY ORDINAL_POSITION;">>, [DataBase, Table])),
    %% parse per field
    [parse_field_one(Field) || Field <- FieldList].

parse_field_one([N, D, <<"char">>, C, P, K, E]) ->
    parse_field_one([N, D, <<"varchar">>, C, P, K, E]);
parse_field_one([N, D, <<"varchar">>, C, P, K, E]) ->
    case string:str(binary_to_list(C), "(string)") =/= 0 of
        true ->
            %% string specified format
            {binary_to_list(N), D, "\"~s\"", C, P, K, E};
        _ ->
            {binary_to_list(N), D, "~s", C, P, K, E}
    end;
parse_field_one([N, D, _, C, P, K, E]) ->
    {binary_to_list(N), D, "~w", C, P, K, E}.

%% @doc parse key format
parse_key([], _) ->
    [];
parse_key(KeyBlock, FieldList) ->
    List = re:split(KeyBlock, "(?i)AND", [{return, list}]),
    [parse_key_expression(Expression, FieldList) || Expression <- List].
parse_key_expression(Expression, FieldList) ->
    [_, Operate, _] = re:split(Expression, "`?\\w+`?|'?\\w+'?", [{return, list}]),
    {match, [[Left], [Right]]} = re:run(Expression, "\\w+", [global, {capture, all, list}]),
    case {lists:keymember(Left, 1, FieldList), lists:keymember(Right, 1, FieldList)} of
        {true, false} ->
            Type = element(3, lists:keyfind(Left, 1, FieldList)),
            {Type, {Left, string:strip(Operate), Right}};
        {false, true} ->
            Type = element(3, lists:keyfind(Right, 1, FieldList)),
            Reserve = fun("<") -> ">";("<=") -> ">=";(O) -> O end,
            {Type, {Right, Reserve(string:strip(Operate)), Left}};
        _ ->
            erlang:error(binary_to_list(list_to_binary(io_lib:format("invail key expression, no such key filed: ~s", [Expression]))))
    end.

%% @doc parse value format
parse_value("*", FieldList) ->
    ValueBlock = string:join([N || {N, _, _, C, _, _, _} <- FieldList, string:str(binary_to_list(C), "(server)") == 0], ","),
    parse_value(ValueBlock, FieldList);
parse_value(ValueBlock, FieldList) ->
    {match, List} = re:run(ValueBlock, "\\w+", [global, {capture, all, list}]),
    [parse_value_expression(Field, FieldList) || [Field] <- List].
parse_value_expression(Field, FieldList) ->
    case lists:keyfind(Field, 1, FieldList) of
        {_, _, T, _, _, _, _} ->
            {Field, T};
        _ ->
            erlang:error(binary_to_list(list_to_binary(io_lib:format("invail key expression, no such key filed: ~s", [Field]))))
    end.

%% @doc collect value data group by key
collect_data(TableBlock, [], ValueBlock, OrderBlock, LimitBlock) ->
    ValueData = maker:select(io_lib:format("SELECT ~s FROM ~s ~s ~s;", [ValueBlock, TableBlock, OrderBlock, LimitBlock])),
    {[], [ValueData]};
collect_data(TableBlock, KeyFormat, ValueBlock, OrderBlock, LimitBlock) ->
    KeyFieldList = string:join(["`" ++ K ++ "`"|| {_, {K, _, _}} <- KeyFormat], ", "),
    KeyData = maker:select(io_lib:format("SELECT ~s FROM ~s GROUP BY ~s ~s", [KeyFieldList, TableBlock, KeyFieldList, OrderBlock])),
    %% bit string key convert
    Convert = fun("<<\"~s\">>") -> "'~s'"; ("~s") -> "'~s'"; ("~w") -> "'~w'"; (Other) -> Other end,
    ReviseKeyFieldList = string:join([lists:concat(["`", K, "` = ", Convert(Type)]) || {Type, {K, _, _}} <- KeyFormat], " AND "),
    ValueData = [maker:select(io_lib:format("SELECT ~s FROM ~s WHERE " ++ ReviseKeyFieldList ++ " ~s ~s;", [ValueBlock, TableBlock | K] ++ [OrderBlock, LimitBlock])) || K <- KeyData],
    {KeyData, ValueData}.

%% @doc format code by format
parse_type_format(Type, ValueFormat) ->
    case Type of
        origin ->
            Format = [T || {_, T} <- ValueFormat],
            {"", "", "", Format};
        _ ->
            Prefix = "",
            TypeLeft = "{",
            TypeRight = "}",
            Format = [lists:concat([F, " = ", T]) || {F, T} <- ValueFormat],
            {Prefix, TypeLeft, TypeRight, Format}
    end.

%% tree code(lua k/v type)
tree(List, ValueFormat, Format, []) ->
    tree(List, ValueFormat, Format, [], 1) ++ "\n";
tree(List, ValueFormat, Format, Name) ->
    Result = tree(List, ValueFormat, Format, [], 2),
    io_lib:format("    [~p] = ~n    {~n~s~n    }\n", [Name, Result]).
tree([], _ValueFormat, _Format, List, _Depth) ->
    string:join(lists:reverse(List), ",\n");
tree([[_, _] | _] = List, ValueFormat, Format, _Result, Depth) ->
    Padding = lists:concat(lists:duplicate(Depth, "    ")),
    string:join([io_lib:format("~s[~p] = ~s", [Padding, K, format_value(Padding, ValueFormat, Format, [V])]) || [K, V] <- List], ",\n");
tree([[H | _] | _] = List, ValueFormat, Format, Result, Depth) ->
    {Target, Remain} = lists:splitwith(fun([X | _]) -> H == X end, List),
    Tree = tree([X || [_ | X] <- Target], ValueFormat, Format, [], Depth + 1),
    Padding = lists:concat(lists:duplicate(Depth, "    ")),
    New = io_lib:format("~s[~p] = ~n~s{~n~s~n~s}", [Padding, H, Padding, Tree, Padding]),
    tree(Remain, ValueFormat, Format, [New | Result], Depth).

%% @doc format code by format
format_value(Padding, ValueFormat, {Prefix, TypeLeft, TypeRight, Format}, ValueData) ->
    [format_value_list(Padding, ValueFormat, Format, Prefix, TypeLeft, TypeRight, Value) || Value <- ValueData].

%% @doc format per list
%% origin/list/tuple only format with ,
format_value_list(Padding, ValueFormat, Format, Prefix, TypeLeft, TypeRight, Value = [_]) ->
    WithAlignFormat = string:join(Format, ", "),
    format_value_item(Padding, ValueFormat, WithAlignFormat, Prefix, TypeLeft, TypeRight, Value, ", ");
format_value_list(Padding, ValueFormat, Format, Prefix, TypeLeft, TypeRight, Value) ->
    WithAlignFormat = string:join(Format, ", "),
    %% add list quote
    "\n" ++ Padding ++ "{\n" ++ Padding ++ "    " ++ format_value_item(Padding, ValueFormat, WithAlignFormat, Prefix, TypeLeft, TypeRight, Value, ", ") ++ "\n" ++ Padding ++ "}".

%% format per item
format_value_item(Padding, ValueFormat, WithAlignFormat, Prefix, TypeLeft, TypeRight, Value, Align) ->
    %% trans empty string to empty list []
    %% revise erlang list to lua list [] => {}
    R = fun(S) -> [case C of $[ -> ${; $] -> $}; _ -> C end || C <- binary_to_list(S)] end,
    %% field (string) specified will format to empty bit string <<"">>
    F = fun(<<>>, {_, <<"~s">>}) -> <<"\"\"">>; (<<>>, {_, "~s"}) -> <<"\"\"">>; (String, {_, <<"~s">>}) -> R(String); (String, {_, "~s"}) -> R(String); (Other, _) -> Other end,
    Data = [io_lib:format("~s~s" ++ WithAlignFormat ++ "~s", [Prefix, TypeLeft | lists:zipwith(F, Row, ValueFormat)] ++ [TypeRight]) || Row <- Value],
    string:join(Data, Align ++ "\n" ++ Padding ++ "    ").

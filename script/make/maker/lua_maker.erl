%%%-------------------------------------------------------------------
%%% @doc
%%% module lua maker
%%% database data to lua term tool
%%% @end
%%%-------------------------------------------------------------------
-module(lua_maker).
-export([start/1]).
%% ------------------------ user guide -------------------------------
%%
%% sql      :: auto group by key(when key reduplicated)
%%
%% string type term guide
%% varchar                                   => term
%% varchar with default(<<>>) in comment     => ""
%% char                                      => ""
%% text                                      => ""
%%
%%%===================================================================
%%% API
%%%===================================================================
%% @doc for shell
start(List) ->
    maker:start(fun parse_table/2, List).

%% ====================================================================
%% Internal functions
%% ====================================================================
%% @doc parse table
parse_table(DataBase, {File, List}) ->
    Code = string:join([parse_code(DataBase, Sql, Name) || {Sql, Name} <- List], ",\n"),
    Name = maker:lower_hump(filename:basename(File, ".lua")),
    All = lists:concat(["ConfigManager.InitConfig(\"", Name, "\",\n{\n", Code, "\n})"]),
    [{"(?s).*", All}].

parse_code(DataBase, Sql, Name) ->
    {TableBlock, KeyBlock, ValueBlock, OrderBlock, LimitBlock, Type, Multi} = parse_sql(Sql),
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
            tree(List, ValueFormat, Format, Name, Multi);
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
    {Type, Multi, Value} = parse_type(string:strip(ValueBlock)),
    {string:strip(TableBlock), string:strip(KeyBlock), Value, OrderBlock, LimitBlock, Type, Multi}.

%% @doc parse data type
parse_type(ValueBlock) ->
    %% parse value type
    Sharp = string:str(ValueBlock, "#"),
    SharpMaps = string:str(ValueBlock, "#{"),
    TupleLeft = string:str(ValueBlock, "{"),
    TupleRight = string:str(ValueBlock, "}"),
    ListLeft = string:str(ValueBlock, "["),
    ListRight = string:str(ValueBlock, "]"),
    %% parse value type
    parse_type_one(Sharp, SharpMaps, TupleLeft, TupleRight, ListLeft, ListRight, ValueBlock).

parse_type_one(0, 0, 0, 0, 0, 0, ValueBlock) ->
    {origin, false, ValueBlock};
parse_type_one(0, 0, TupleLeft, TupleRight, 0, 0, ValueBlock) ->
    Value = string:sub_string(ValueBlock, TupleLeft + 1, TupleRight - 1),
    %% lua tuple as k/v type value {k = v}
    {tuple, false, Value};
parse_type_one(0, 0, 0, 0, ListLeft, ListRight, ValueBlock) ->
    Value = string:sub_string(ValueBlock, ListLeft + 1, ListRight - 1),
    {list, false, Value};
parse_type_one(0, 0, TupleLeft, TupleRight, _ListLeft, _ListRight, ValueBlock) ->
    Value = string:sub_string(ValueBlock, TupleLeft + 1, TupleRight - 1),
    %% lua tuple as k/v type value {k = v}
    {tuple, true, Value}.

%% @doc get table FieldList
parse_field(DataBase, TableBlock) ->
    {match, [Table]} = re:run(TableBlock, "(?i)\\w+", [{capture, all, list}]),
    FieldList = maker:select(io_lib:format(<<"SELECT `COLUMN_NAME`, `COLUMN_DEFAULT`, `DATA_TYPE`, `COLUMN_COMMENT`, `ORDINAL_POSITION`, `COLUMN_KEY`, `EXTRA` FROM information_schema.`COLUMNS` WHERE `TABLE_SCHEMA` = '~s' AND TABLE_NAME = '~s' ORDER BY ORDINAL_POSITION;">>, [DataBase, Table])),
    %% parse per field
    [parse_field_one(Field) || Field <- FieldList].

parse_field_one([N, D, <<"char">>, C, P, K, E]) ->
    %% char as binary format
    {binary_to_list(N), D, "\"~s\"", C, P, K, E};
parse_field_one([N, D, <<"varchar">>, C, P, K, E]) ->
    %% varchar as binary format
    {binary_to_list(N), D, "~s", C, P, K, E};
parse_field_one([N, D, <<"text">>, C, P, K, E]) ->
    %% text as binary format
    {binary_to_list(N), D, "\"~s\"", C, P, K, E};
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
    KeyFieldData = maker:select(io_lib:format("SELECT ~s FROM ~s GROUP BY ~s ~s", [KeyFieldList, TableBlock, KeyFieldList, OrderBlock])),
	%% lua key integer or string will keep origin data type
    KeyData = [lists:map(fun(V = <<_/binary>>) -> type:to_list(<<$", V/binary, $">>); (V) -> type:to_list(V) end, Row) || Row <- KeyFieldData],
    %% bit string key convert
    Convert = fun("<<\"~s\">>") -> "'~s'"; ("~s") -> "'~s'"; ("~w") -> "'~w'"; (Other) -> Other end,
    ReviseKeyFieldList = string:join([lists:concat(["`", K, "` = ", Convert(Type)]) || {Type, {K, _, _}} <- KeyFormat], " AND "),
    ValueData = [maker:select(io_lib:format("SELECT ~s FROM ~s WHERE " ++ ReviseKeyFieldList ++ " ~s ~s;", [ValueBlock, TableBlock | K] ++ [OrderBlock, LimitBlock])) || K <- KeyFieldData],
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
tree(List, ValueFormat, Format, [], Multi) ->
    tree(List, ValueFormat, Format, [], 1, Multi);
tree(List, ValueFormat, Format, Name, Multi) ->
    Result = tree(List, ValueFormat, Format, [], 2, Multi),
    io_lib:format("    [~p] = ~n    {~n~s~n    }", [Name, Result]).
tree([], _ValueFormat, _Format, List, _Depth, _Multi) ->
    string:join(lists:reverse(List), ",\n");
tree([[_, _] | _] = List, ValueFormat, Format, _Result, Depth, Multi) ->
    Padding = lists:concat(lists:duplicate(Depth, "    ")),
    %% if one key maps multi value
    %% value must as a list collection
    case lists:any(fun([_, Value]) -> erlang:length(Value) > 1 end, List) of
        true ->
            ListReviseLeft = "\n" ++ Padding ++ "{\n" ++ Padding ++ "    ",
            ListReviseRight = "\n" ++ Padding ++ "}";
        false when Multi ->
            %% tuple/maps/record as list type ignore single data status
            ListReviseLeft = "\n" ++ Padding ++ "{\n" ++ Padding ++ "    ",
            ListReviseRight = "\n" ++ Padding ++ "}";
        false ->
            ListReviseLeft = "",
            ListReviseRight = ""
    end,
    string:join([io_lib:format("~s[~s] = ~s", [Padding, K, format_value(Padding, ValueFormat, Format, [V], ListReviseLeft, ListReviseRight)]) || [K, V] <- List], ",\n");
tree([[H | _] | _] = List, ValueFormat, Format, Result, Depth, Multi) ->
    {Target, Remain} = lists:splitwith(fun([X | _]) -> H == X end, List),
    Tree = tree([X || [_ | X] <- Target], ValueFormat, Format, [], Depth + 1, Multi),
    Padding = lists:concat(lists:duplicate(Depth, "    ")),
    New = io_lib:format("~s[~s] = ~n~s{~n~s~n~s}", [Padding, H, Padding, Tree, Padding]),
    tree(Remain, ValueFormat, Format, [New | Result], Depth, Multi).

%% @doc format code by format
format_value(Padding, ValueFormat, {Prefix, TypeLeft, TypeRight, Format}, ValueData, ListReviseLeft, ListReviseRight) ->
    [format_value_list(Padding, ValueFormat, Format, Prefix, TypeLeft, TypeRight, Value, ListReviseLeft, ListReviseRight) || Value <- ValueData].

%% @doc format per list
%% origin/list/tuple only format with ,
format_value_list(Padding, ValueFormat, Format, Prefix, TypeLeft, TypeRight, Value, ListReviseLeft, ListReviseRight) ->
    WithAlignFormat = string:join(Format, ", "),
    ListReviseLeft ++ format_value_item(Padding, ValueFormat, WithAlignFormat, Prefix, TypeLeft, TypeRight, Value, ", ") ++ ListReviseRight.

%% format per item
format_value_item(Padding, ValueFormat, WithAlignFormat, Prefix, TypeLeft, TypeRight, Value, Align) ->
    %% trans empty string to empty list []
    %% revise erlang list to lua list [] => {}
    R = fun(S) -> [case C of $[ -> ${; $] -> $}; _ -> C end || C <- binary_to_list(S)] end,
    %% field (string) specified will format to empty bit string <<"">>
    F = fun(<<>>, {_, <<"~s">>}) -> <<"\"\"">>; (<<>>, {_, "~s"}) -> <<"\"\"">>; (String, {_, <<"~s">>}) -> R(String); (String, {_, "~s"}) -> R(String); (Other, _) -> Other end,
    Data = [io_lib:format("~s~s" ++ WithAlignFormat ++ "~s", [Prefix, TypeLeft | lists:zipwith(F, Row, ValueFormat)] ++ [TypeRight]) || Row <- Value],
    string:join(Data, Align ++ "\n" ++ Padding ++ "    ").

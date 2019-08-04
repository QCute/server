%%%-------------------------------------------------------------------
%%% @doc
%%% module data maker
%%% database data to erlang term tool
%%% @end
%%%-------------------------------------------------------------------
-module(data_maker).
-export([start/1]).
%% ------------------------ user guide -------------------------------
%% 
%% sql      :: auto group by key(when key reduplicated)
%% type     :: [] | record | maps | tuple | list | origin
%% default  :: [] | record | maps | tuple | list | (specified value)
%% includes :: split with ,

%% string type term guide
%% varchar                                   => term
%% varchar with default(<<>>) in comment     => <<>>
%% char                                      => <<>>
%% text                                      => <<>>
%%
%%%===================================================================
%%% API
%%%===================================================================
%% @doc for shell
start(List) ->
    maker:start(fun parse_table/2, [List]).

%% ====================================================================
%% Internal functions
%% ====================================================================
%% @doc parse table
parse_table(DataBase, {File, Includes, List}) ->
    Code = lists:flatten([parse_code(DataBase, Sql, Name, Default) || {Sql, Name, Default} <- List]),
    Include = [lists:flatten(io_lib:format("-include(\"~s\").\n", [X])) || X <- Includes],
    Module = filename:basename(File, ".erl"),
    Head = io_lib:format("-module(~s).\n-compile(nowarn_export_all).\n-compile(export_all).\n~s\n\n", [Module, Include]),
    [{"(?s).*", Head ++ Code}].

parse_code(DataBase, Sql, Name, Default) ->
    {TableBlock, KeyBlock, ValueBlock, OrderBlock, LimitBlock, Type, Multi, Record} = parse_sql(Sql),
    FieldList = parse_field(DataBase, TableBlock),
    KeyFormat = parse_key(KeyBlock, FieldList),
    ValueFormat = parse_value(ValueBlock, FieldList),
    {KeyData, ValueData} = collect_data(TableBlock, KeyFormat, ValueBlock, OrderBlock, LimitBlock),
    KeyCode = format_key(Name, KeyFormat, KeyData),
    ValueCode = format_value(Type, Multi, Default, Record, KeyFormat, ValueFormat, ValueData),
    case length(KeyCode) == length(ValueCode) of
        true ->
            %% k/v type
            lists:concat([string:join(lists:zipwith(fun(K, V) -> K ++ "\n    " ++ V end, KeyCode, ValueCode), ";\n"), ".\n\n"]);
        _ when KeyBlock == [] ->
            %% collect type
            KeyCode ++ "\n    " ++ ValueCode ++ ".\n\n"
    end.

%% @doc parse sql expression
parse_sql(Sql) ->
    {match, [ValueBlock]} = re:run(Sql, "(?i)(?<=\\bSELECT).*?(?=\\bFROM\\b)", [{capture, all, list}]),
    {match, [TableBlock]} = re:run(Sql, "(?i)(?<=\\bFROM\\b).*?(?=\\bWHERE\\b|\\bGROUP BY\\b|\\bORDER BY\\b|\\bLIMIT\\b|;|$)", [{capture, all, list}]),
    {match, [KeyBlock]} = max(re:run(Sql, "(?i)(?<=\\bWHERE\\b).*?(?=\\bGROUP BY\\b|\\bORDER BY\\b|\\bLIMIT\\b|;|$)", [{capture, all, list}]), {match, [""]}),
    {match, [OrderBlock]} = max(re:run(Sql, "(?i)\\bORDER BY\\b.*?(?=\\bLIMIT\\b|;|$)", [{capture, all, list}]), {match, [""]}),
    {match, [LimitBlock]} = max(re:run(Sql, "(?i)\\bLIMIT\\b.*?(?=;|$)", [{capture, all, list}]), {match, [""]}),
    {Type, Multi, Value, Record} = parse_type(string:strip(TableBlock), string:strip(ValueBlock)),
    {string:strip(TableBlock), string:strip(KeyBlock), Value, OrderBlock, LimitBlock, Type, Multi, Record}.

%% @doc parse data type
parse_type(TableBlock, ValueBlock) ->
    %% table name
    {match, [TableName]} = re:run(TableBlock, "\\w+", [{capture, first, list}]),
    %% parse value type
    Sharp = string:str(ValueBlock, "#"),
    SharpMaps = string:str(ValueBlock, "#{"),
    TupleLeft = string:str(ValueBlock, "{"),
    TupleRight = string:str(ValueBlock, "}"),
    ListLeft = string:str(ValueBlock, "["),
    ListRight = string:str(ValueBlock, "]"),
    %% parse value type
    parse_type_one(Sharp, SharpMaps, TupleLeft, TupleRight, ListLeft, ListRight, ValueBlock, TableName).

parse_type_one(0, 0, 0, 0, 0, 0, ValueBlock, TableName) ->
    {origin, false, ValueBlock, TableName};
parse_type_one(0, 0, TupleLeft, TupleRight, 0, 0, ValueBlock, TableName) ->
    Value = string:sub_string(ValueBlock, TupleLeft + 1, TupleRight - 1),
    {tuple, false, Value, TableName};
parse_type_one(Sharp, 0, TupleLeft, TupleRight, 0, 0, ValueBlock, TableName) ->
    Value = string:sub_string(ValueBlock, TupleLeft + 1, TupleRight - 1),
    case string:sub_string(ValueBlock, Sharp + 1, TupleLeft - 1) of
        "record" ->
            Record = TableName;
        Other ->
            Record = Other
    end,
    {record, false, Value, Record};
parse_type_one(_Sharp, _SharpMaps, TupleLeft, TupleRight, 0, 0, ValueBlock, TableName) ->
    Value = string:sub_string(ValueBlock, TupleLeft + 1, TupleRight - 1),
    {maps, false, Value, TableName};
parse_type_one(0, 0, 0, 0, ListLeft, ListRight, ValueBlock, TableName) ->
    Value = string:sub_string(ValueBlock, ListLeft + 1, ListRight - 1),
    {list, false, Value, TableName};
parse_type_one(0, 0, TupleLeft, TupleRight, _ListLeft, _ListRight, ValueBlock, TableName) ->
    Value = string:sub_string(ValueBlock, TupleLeft + 1, TupleRight - 1),
    {tuple, true, Value, TableName};
parse_type_one(Sharp, 0, TupleLeft, TupleRight, _ListLeft, _ListRight, ValueBlock, TableName) ->
    Value = string:sub_string(ValueBlock, TupleLeft + 1, TupleRight - 1),
    case string:sub_string(ValueBlock, Sharp + 1, TupleLeft - 1) of
        "record" ->
            Record = TableName;
        Other ->
            Record = Other
    end,
    {record, true, Value, Record};
parse_type_one(_Sharp, _SharpMaps, TupleLeft, TupleRight, _ListLeft, _ListRight, ValueBlock, TableName) ->
    Value = string:sub_string(ValueBlock, TupleLeft + 1, TupleRight - 1),
    {maps, true, Value, TableName}.

%% @doc get table FieldList
parse_field(DataBase, TableBlock) ->
    {match, [Table]} = re:run(TableBlock, "(?i)\\w+", [{capture, all, list}]),
    FieldList = maker:select(io_lib:format(<<"SELECT `COLUMN_NAME`, `COLUMN_DEFAULT`, `DATA_TYPE`, `COLUMN_COMMENT`, `ORDINAL_POSITION`, `COLUMN_KEY`, `EXTRA` FROM information_schema.`COLUMNS` WHERE `TABLE_SCHEMA` = '~s' AND TABLE_NAME = '~s' ORDER BY ORDINAL_POSITION;">>, [DataBase, Table])),
    %% parse per field
    [parse_field_one(Field) || Field <- FieldList].

parse_field_one([N, D, <<"char">>, C, P, K, E]) ->
    %% char as binary format
    {binary_to_list(N), D, "<<\"~s\">>", C, P, K, E};
parse_field_one([N, D, <<"varchar">>, C, P, K, E]) ->
    case re:run(C, "(?<=default\\().*?(?=\\))", [{capture, first, list}]) of
        {match, ["<<>>"]} ->
            %% specified default binary format
            {binary_to_list(N), D, "<<\"~s\">>", C, P, K, E};
        _ ->
            %% term format by default
            {binary_to_list(N), D, "~s", C, P, K, E}
    end;
parse_field_one([N, D, <<"text">>, C, P, K, E]) ->
    %% text as binary format
    {binary_to_list(N), D, "<<\"~s\">>", C, P, K, E};
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
    ValueBlock = string:join([N || {N, _, _, C, _, _, _} <- FieldList, string:str(binary_to_list(C), "(client)") == 0], ","),
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
    RawKeyData = maker:select(io_lib:format("SELECT ~s FROM ~s GROUP BY ~s ~s", [KeyFieldList, TableBlock, KeyFieldList, OrderBlock])),
    %% bit string key convert
    Convert = fun("<<\"~s\">>") -> "'~s'"; ("~s") -> "'~s'"; ("~w") -> "'~w'"; (Other) -> Other end,
    ReviseKeyFieldList = string:join([lists:concat(["`", K, "` = ", Convert(Type)]) || {Type, {K, _, _}} <- KeyFormat], " AND "),
    ValueData = [maker:select(io_lib:format("SELECT ~s FROM ~s WHERE " ++ ReviseKeyFieldList ++ " ~s ~s;", [ValueBlock, TableBlock | K] ++ [OrderBlock, LimitBlock])) || K <- RawKeyData],
    {RawKeyData, ValueData}.

%% @doc format code by key
format_key(Name, [], _) ->
    [binary_to_list(list_to_binary(io_lib:format("~s() ->", [Name])))];
format_key(Name, KeyFormat, KeyData) ->
    %% 保留参数顺序
    {P, G} = lists:foldr(fun({T, {_, "=", _}}, {P, G}) -> {[T | P], G};({T, {_, "<=", A}}, {P, G}) -> {[A | P], [T ++ " =< " ++ A | G]};({T, {_, ">", A}}, {P, G}) -> {[A | P], [A ++ " < " ++ T | G]};({T, {_, ">=", A}}, {P, G}) -> {[A | P], [A ++ " =< " ++ T | G]};({T, {_, O, A}}, {P, G}) -> {[A | P], [T ++ " " ++ O ++ " " ++ A | G]} end, {[], []}, KeyFormat),
    Param = lists:foldr(fun(X, A) -> case {string:str(X, "~") == 0, lists:member(X, A) == false} of {true, true} -> [X | A]; {false, _} -> [X | A]; _ -> A end end, [], P),
    case G of
        [] ->
            Guard = "";
        _ ->
            Guard = " when " ++ string:join(G, " andalso ")
    end,
    Format = binary_to_list(list_to_binary(io_lib:format("~s(~s)~s ->", [Name, string:join(Param, ", "), Guard]))),
    Default = binary_to_list(list_to_binary(io_lib:format("~s(~s) -> ", [Name, string:join(lists:duplicate(erlang:length(Param), "_"), ", ")]))),
    [io_lib:format(Format, K) || K <- KeyData] ++ [Default].

%% @doc format code by format
format_value(Type, Multi, Default, Record, KeyFormat, ValueFormat, ValueData) ->
    case Type of
        record ->
            Prefix = "#" ++ Record,
            TypeLeft = "{\n~s",
            TypeRight = "\n~s}",
            Format = [lists:concat([F, " = ", T]) || {F, T} <- ValueFormat];
        maps ->
            Prefix = "#",
            TypeLeft = "{\n~s",
            TypeRight = "\n~s}",
            Format = [lists:concat([F, " => ", T]) || {F, T} <- ValueFormat];
        tuple ->
            Prefix = "",
            TypeLeft = "{",
            TypeRight = "}",
            Format = [T || {_, T} <- ValueFormat];
        list ->
            Prefix = "",
            TypeLeft = "[",
            TypeRight = "]",
            Format = [T || {_, T} <- ValueFormat];
        origin ->
            Prefix = "",
            TypeLeft = "",
            TypeRight = "",
            Format = [T || {_, T} <- ValueFormat];
        _ ->
            Prefix = "",
            TypeLeft = "",
            TypeRight = "",
            Format = "",
            erlang:error(binary_to_list(list_to_binary(io_lib:format("invail value type, no such type support: ~w~n", [Type]))))
    end,
    case Default of
        _ when KeyFormat == [] ->
            DefaultValue = "";
        _ when Default == list orelse Default == "" orelse Default == "[]" ->
            DefaultValue = ["[]"];
        tuple ->
            DefaultValue = ["{}"];
        record ->
            DefaultValue = ["#" ++ Record ++ "{}"];
        _ ->
            DefaultValue = [binary_to_list(list_to_binary(io_lib:format("~w", [Default])))]
    end,
    %% if one key maps multi value
    %% value must as a list collection
    case lists:any(fun(Value) -> erlang:length(Value) > 1 end, ValueData) of
        true when Type == record orelse Type == maps ->
            ListReviseLeft = "[\n",
            ListReviseRight = "\n    ]";
        true ->
            ListReviseLeft = "[",
            ListReviseRight = "]";
        false when Multi ->
            %% tuple/maps/record as list type ignore single data status
            ListReviseLeft = "[\n",
            ListReviseRight = "\n    ]";
        false ->
            ListReviseLeft = "",
            ListReviseRight = ""
    end,
    [format_value_list(ValueFormat, Format, Prefix, TypeLeft, TypeRight, Value, Type, ListReviseLeft, ListReviseRight) || Value <- ValueData] ++ DefaultValue.

%% @doc format per list
%% record/maps format pretty
format_value_list(ValueFormat, Format, Prefix, TypeLeft, TypeRight, Value, Type, ListReviseLeft, ListReviseRight) when Type == record orelse Type == maps ->
    WithAlignFormat = string:join(Format, lists:concat([",\n", lists:duplicate(2, "    ")])),
    WithAlignTypeLeft = io_lib:format(TypeLeft, [lists:concat(lists:duplicate(2, "    "))]),
    WithAlignTypeRight = io_lib:format(TypeRight, [lists:concat(lists:duplicate(1, "    "))]),
    Align = lists:concat([",", lists:duplicate(1, "    ")]),
    ListReviseLeft ++ format_value_item(ValueFormat, WithAlignFormat, Prefix, WithAlignTypeLeft, WithAlignTypeRight, Value, Align) ++ ListReviseRight;

%% origin/list/tuple only format with ,
format_value_list(ValueFormat, Format, Prefix, TypeLeft, TypeRight, Value, _Type, ListReviseLeft, ListReviseRight) ->
    WithAlignFormat = string:join(Format, ", "),
    ListReviseLeft ++ format_value_item(ValueFormat, WithAlignFormat, Prefix, TypeLeft, TypeRight, Value, ", ") ++ ListReviseRight.

%% format per item
format_value_item(Format, WithAlignFormat, Prefix, TypeLeft, TypeRight, Value, Align) ->
    %% trans empty string to empty list []
    %% field default(<<>>) specified will format to empty bit string <<"">>
    F = fun(<<>>, {_, <<"~s">>}) -> <<"[]">>; (<<>>, {_, "~s"}) -> <<"[]">>; (Other, _) -> Other end,
    Data = [io_lib:format("~s~s" ++ WithAlignFormat ++ "~s", [Prefix, TypeLeft | lists:zipwith(F, Row, Format)] ++ [TypeRight]) || Row <- Value],
    string:join(Data, Align).

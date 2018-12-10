%%%-------------------------------------------------------------------
%%% @doc
%%% module database data to erlang term tool
%%% @end
%%%-------------------------------------------------------------------
-module(data_maker).
-export([start/1]).
-export([parse/2]).
%% ------------------------ user guide -------------------------------
%% 
%% sql      :: auto group by key(when key reduplicated)
%% type     :: [] | record | maps | tuple | list | origin
%% default  :: [] | record | maps | tuple | list | (specified value)
%% includes :: split with ,

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
parse_table(DataBase, {File, Includes, List}) ->
    Code = lists:flatten([parse_code(DataBase, Sql, Name, Type, Default) || {Sql, Name, Type, Default} <- List]),
    Include = [lists:flatten(io_lib:format("-include(\"~s\").\n", [X])) || X <- Includes],
    [Module | _] = string:tokens(hd(lists:reverse(string:tokens(File, "/"))), "."),
    Head = io_lib:format("-module(~s).\n-compile(nowarn_export_all).\n-compile(export_all).\n~s\n\n", [Module, Include]),
    [{"(?s).*", Head ++ Code}].

parse_code(DataBase, Sql, Name, Type, Default) ->
    {TableBlock, KeyBlock, ValueBlock, OrderBlock} = parse_sql(Sql),
    Fields = parse_field(DataBase, TableBlock),
    KeyFormat = parse_key(KeyBlock, Fields),
    ValueFormat = parse_value(ValueBlock, Fields),
    {KeyData, ValueData} = collect_data(DataBase, TableBlock, KeyFormat, ValueBlock, OrderBlock),
    KeyCode = format_key(Name, KeyFormat, KeyData),
    ValueCode = format_value(Type, Default, TableBlock, KeyFormat, ValueFormat, ValueData),
    case length(KeyCode) == length(ValueCode) of
        true ->
            %% final data code
            lists:concat([string:join(lists:zipwith(fun(K, V) -> K ++ "\n    " ++ V end, KeyCode, ValueCode), ";\n"), ".\n\n"]);
        _ when KeyBlock == [] ->
            %% final data code
            KeyCode ++ "\n    " ++ ValueCode ++ ".\n\n"
    end.

%% @doc parse sql expression
parse_sql(Sql) ->
    {match, [[ValueBlock]]} = re:run(Sql, "(?i)(?<=SELECT).*?(?=FROM)", [global, {capture, all, list}]),
    {match, [[TableBlock]]} = re:run(Sql, "(?i)(?<=FROM).*?(?=WHERE|GROUP BY|ORDER BY|;|$)", [global, {capture, all, list}]),
    {match, [[KeyBlock]]} = max(re:run(Sql, "(?i)(?<=WHERE).*?(?=GROUP BY|ORDER BY|;|$)", [global, {capture, all, list}]), {match,[[""]]}),
    {match, [[OrderBlock]]} = max(re:run(Sql, "(?i)ORDER BY.*?(?=;|$)", [global, {capture, all, list}]), {match,[[""]]}),
    {string:strip(TableBlock), string:strip(KeyBlock), string:strip(ValueBlock), OrderBlock}.

%% @doc get table fields
parse_field(DataBase, TableBlock) ->
    {match, [[Table]]} = re:run(TableBlock, "(?i)\\w+", [global, {capture, all, list}]),
    RawFields = sql:select(DataBase, Table, io_lib:format(<<"SELECT `COLUMN_NAME`, `COLUMN_DEFAULT`, `DATA_TYPE`, `COLUMN_COMMENT`, `ORDINAL_POSITION`, `COLUMN_KEY`, `EXTRA` FROM information_schema.`COLUMNS` WHERE `TABLE_SCHEMA` = '~s' AND TABLE_NAME = '~s' ORDER BY ORDINAL_POSITION;">>, [DataBase, Table])),
    [begin F = fun(TT, CC) when TT == <<"varchar">> orelse TT == <<"char">> -> case string:str(binary_to_list(CC), "(convert)") == 0 of true -> "~s"; _ -> "~p" end;(_, _) -> "~p" end, {binary_to_list(N), D, F(T, C), C, P, K, E} end || [N, D, T, C, P, K, E] <- RawFields].

%% @doc parse key format
parse_key([], _) ->
    [];
parse_key(KeyBlock, Fields) ->
    List = re:split(KeyBlock, "(?i)AND", [{return, list}]),
    [parse_key_expression(Expression, Fields) || Expression <- List].
parse_key_expression(Expression, Fields) ->
    [_, Operate, _] = re:split(Expression, "`?\\w+`?|'?\\w+'?", [{return, list}]),
    {match, [[Left], [Right]]} = re:run(Expression, "\\w+", [global, {capture, all, list}]),
    case {lists:keymember(Left, 1, Fields), lists:keymember(Right, 1, Fields)} of
        {true, false} ->
            Type = element(3, lists:keyfind(Left, 1, Fields)),
            {Type, {Left, string:strip(Operate), Right}};
        {false, true} ->
            Type = element(3, lists:keyfind(Right, 1, Fields)),
            Reserve = fun("<") -> ">";("<=") -> ">=";(O) -> O end,
            {Type, {Right, Reserve(string:strip(Operate)), Left}};
        _ ->
            erlang:error(binary_to_list(list_to_binary(io_lib:format("invail key expression, no such key filed: ~s~n", [Expression]))))
    end.

%% @doc parse value format
parse_value("*", Fields) ->
    ValueBlock = string:join([N || {N, _, _, _, _, _, _} <- Fields], ","),
    parse_value(ValueBlock, Fields);
parse_value(ValueBlock, Fields) ->
    {match, List} = re:run(ValueBlock, "\\w+", [global, {capture, all, list}]),
    [parse_value_expression(Expression, Fields) || [Expression] <- List].
parse_value_expression(Expression, Fields) ->
    case lists:keyfind(Expression, 1, Fields) of
        {_, _, T, _, _, _, _} ->
            {Expression, T};
        _ ->
            erlang:error(binary_to_list(list_to_binary(io_lib:format("invail key expression, no such key filed: ~s~n", [Expression]))))
    end.

%% @doc collect value data group by key
collect_data(DataBase, TableBlock, [], ValueBlock, OrderBlock) ->
    ValueData = sql:select(DataBase, TableBlock, io_lib:format("SELECT ~s FROM ~s ~s;", [ValueBlock, TableBlock, OrderBlock])),
    {[], [ValueData]};
collect_data(DataBase, TableBlock, KeyFormat, ValueBlock, OrderBlock) ->
    KeyFields = string:join(["`" ++ K ++ "`"|| {_, {K, _, _}} <- KeyFormat], ", "),
    RawKeyData = sql:select(DataBase, TableBlock, io_lib:format("SELECT ~s FROM ~s GROUP BY ~s ~s", [KeyFields, TableBlock, KeyFields, OrderBlock])),
    KeyData = [[maker:term(T) || T <- R] || R <- RawKeyData],
    KeyFieldList = string:join([lists:concat(["`", K, "` = '~w'"]) || {_, {K, _, _}} <- KeyFormat], " AND "),
    ValueData = [sql:select(DataBase, TableBlock, io_lib:format("SELECT ~s FROM ~s WHERE " ++ KeyFieldList ++ " ~s;", [ValueBlock, TableBlock | K] ++ [OrderBlock])) || K <- KeyData],
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
format_value(Type, Default, TableBlock, KeyFormat, ValueFormat, ValueData) ->
    {match, [[Name]]} = re:run(TableBlock, "\\w+", [global, {capture, all, list}]),
    case Type of
        _ when Type == [] orelse Type == record ->
            Prefix = "#" ++ Name,
            TypeLeft = "{",
            TypeRight = "}",
            Format = string:join([lists:concat([E, " = ", T]) || {E, T} <- ValueFormat], ", ");
        maps ->
            Prefix = "#",
            TypeLeft = "{",
            TypeRight = "}",
            Format = string:join([lists:concat([E, " => ", T]) || {E, T} <- ValueFormat], ", ");
        tuple ->
            Prefix = "",
            TypeLeft = "{",
            TypeRight = "}",
            Format = string:join([T || {_, T} <- ValueFormat], ", ");
        list ->
            Prefix = "",
            TypeLeft = "[",
            TypeRight = "]",
            Format = string:join([T || {_, T} <- ValueFormat], ", ");
        origin ->
            Prefix = "",
            TypeLeft = "",
            TypeRight = "",
            Format = string:join([T || {_, T} <- ValueFormat], ", ");
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
            DefaultValue = ["#" ++ Name ++ "{}"];
        _ ->
            DefaultValue = [binary_to_list(list_to_binary(io_lib:format("~w", [Default])))]
    end,
    [begin C  = string:join([io_lib:format("~s~s" ++ Format ++ "~s", [Prefix, TypeLeft | VV] ++ [TypeRight]) || VV <- V], ", "), case length(V) == 1 of true -> C; _-> "[" ++ C ++ "]" end end || V <- ValueData] ++ DefaultValue.

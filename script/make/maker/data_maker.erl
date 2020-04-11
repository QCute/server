%%%-------------------------------------------------------------------
%%% @doc
%%% module data maker
%%% database data to erlang term tool
%%% @end
%%%-------------------------------------------------------------------
-module(data_maker).
-export([start/1]).
-record(field, {name = [], default = [], type = [], format = [], comment = [], position = 0, key = [], extra = []}).
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
parse_table(DataBase, {File, Includes, List}) ->
    Code = lists:flatten([parse_code(DataBase, Sql, Name) || {Sql, Name} <- List]),
    Include = [lists:flatten(io_lib:format("-include(\"~s\").\n", [X])) || X <- Includes],
    Module = filename:basename(File, ".erl"),
    Head = io_lib:format("-module(~s).\n-compile(nowarn_export_all).\n-compile(export_all).\n~s\n\n", [Module, Include]),
    [{"(?s).*", Head ++ Code}].

parse_code(DataBase, Sql, Name) ->
    [ValueBlock, TableBlock, KeyBlock, GroupBlock, OrderBlock, LimitBlock, DefaultBlock] = parse_sql(Sql),
    %% collect data
    Table = extract(TableBlock, "\\w+"),
    %% parse value type
    {Value, Type, TypeLeft, TypeRight} = parse_type(Table, ValueBlock),
    %% collect table fields
    AllFields = collect_fields(DataBase, Table),
    %% parse key and make key format
    KeyFormat = parse_keys(AllFields, KeyBlock),
    %% collect value fields
    {ValueMode, ValueFields} = collect_value_fields(AllFields, Value),
    %% parse values and make values format
    ValueFormat = parse_values(ValueMode, ValueFields, Type, TypeLeft, TypeRight),
    %% collect from database
    {KeyData, ValueData} = collect_data(TableBlock, KeyFormat, GroupBlock, OrderBlock, LimitBlock, ValueMode, ValueFields),
    %% parse default value
    {DefaultArgs, DefaultValue} = parse_default(Table, KeyFormat, string:strip(DefaultBlock)),
    %% format data code
    format_code(Name, KeyFormat, KeyData, ValueFormat, ValueData, string:strip(GroupBlock), DefaultArgs, DefaultValue, ValueMode).

parse_sql(Sql) ->
    %% parse sql sentence
    [ValueBlock]   = extract(Sql, "(?i)(?<=\\bSELECT\\b).*?(?=\\bAS\\b|\\bFROM\\b)"),
    %% [FormatBlock]  = extract(Sql, "(?i)(?<=\\bAS\\b).*?(?=\\bFROM\\b|\\bWHERE\\b|\\bGROUP BY\\b|\\bORDER BY\\b|\\bLIMIT\\b|\\bMULTI\\b|\\bDEFAULT\\b|;|$)"),
    [TableBlock]   = extract(Sql, "(?i)(?<=\\bFROM\\b).*?(?=\\bWHERE\\b|\\bGROUP BY\\b|\\bORDER BY\\b|\\bLIMIT\\b|\\bMULTI\\b|\\bDEFAULT\\b|;|$)"),
    [KeyBlock]     = extract(Sql, "(?i)(?<=\\bWHERE\\b).*?(?=\\bGROUP BY\\b|\\bORDER BY\\b|\\bLIMIT\\b|\\bMULTI\\b|\\bDEFAULT\\b|;|$)"),
    [GroupBlock]   = extract(Sql, "(?i)\\bGROUP BY\\b.*?(?=\\bORDER BY\\b|\\bLIMIT|\\bMULTI\\b|\\bDEFAULT\\b|;|$)"),
    [OrderBlock]   = extract(Sql, "(?i)\\bORDER BY\\b.*?(?=\\bLIMIT|\\bMULTI\\b|\\bDEFAULT\\b|;|$)"),
    [LimitBlock]   = extract(Sql, "(?i)\\bLIMIT\\b.*?(?=\\bMULTI\\b|\\bDEFAULT\\b|;|$)"),
    [DefaultBlock] = extract(Sql, "(?i)(?<=\\bDEFAULT\\b).*?(?=;|$)"),
    [string:strip(ValueBlock), string:strip(TableBlock), string:strip(KeyBlock), string:strip(GroupBlock), string:strip(OrderBlock), string:strip(LimitBlock), string:strip(DefaultBlock)].

%% @doc parse value type
parse_type(Table, String) ->
    TypeList = [{record, "#" ++ Table ++ "{", "}", "(?<=#record\\{).*?(?=\\})"}, {maps, "#{", "}", "(?<=#\\{).*?(?=\\})"}, {tuple, "{", "}", "(?<=\\{).*?(?=\\})"}, {list, "[", "]", "(?<=\\[).*?(?=\\])"}],
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

%% @doc get table field list
collect_fields(DataBase, Table) ->
    %% make fields sql
    FieldsSql = io_lib:format(<<"SELECT `COLUMN_NAME`, `COLUMN_DEFAULT`, `COLUMN_TYPE`, `DATA_TYPE`, `COLUMN_COMMENT`, `ORDINAL_POSITION`, `COLUMN_KEY`, `EXTRA` FROM information_schema.`COLUMNS` WHERE `TABLE_SCHEMA` = '~s' AND `TABLE_NAME` = '~s' ORDER BY `ORDINAL_POSITION`;">>, [DataBase, Table]),
    %% R or OTP
    IsOtp = erlang:list_to_float(erlang:system_info(version)) > 6,
    %% data revise
    Revise = fun
        (FieldInfo = #field{name = Name, format = <<"char">>, comment = Comment}) when IsOtp ->
            %% OTP 17 or later, utf8 bit string use ""/utf8 to declare
            FieldInfo#field{name = binary_to_list(Name), format = "<<\"~s\"/utf8>>", default = "<<>>", comment = binary_to_list(Comment)};
        (FieldInfo = #field{name = Name, format = <<"char">>, comment = Comment}) ->
            %% OTP R16B03-1 or earlier, utf8 bit string do not use ""/utf8 to declare
            FieldInfo#field{name = binary_to_list(Name), format = "<<\"~s\">>", default = "<<>>", comment = binary_to_list(Comment)};
        (FieldInfo = #field{name = Name, format = <<"varchar">>, comment = Comment}) ->
            FieldInfo#field{name = binary_to_list(Name), format = "~s", default = "[]", comment = binary_to_list(Comment)};
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
    %% [_, Operate, _] = re:split(Expression, "`?\\w+`?|'?\\w+'?", [{return, list}]),
    %% {match, [[Left], [Right]]} = re:run(Expression, "\\w+", [global, {capture, all, list}]),
    %% force it match
    {match, [Compare]} = re:run(Expression, "<=|=>|=<|>=|<|>|==|=", [{capture, first, list}]),
    [Left, Right] = [string:strip(string:strip(string:strip(string:strip(Slice), both, $`), left, $(), right, $))|| Slice <- string:split(Expression, Compare)],
    case {lists:keyfind(Left, #field.name, Fields), lists:keyfind(Right, #field.name, Fields)} of
        {#field{format = Format}, false} ->
            Revise = fun("<=") -> ">=";("=>") -> ">=";("==") -> "=";(O) -> O end,
            {Format, Left, Revise(string:strip(Compare)), Right};
        {false, #field{format = Format}} ->
            %% compare operator revise, all to erlang compare expression
            Revise = fun("<=") -> ">=";("=>") -> ">=";("==") -> "=";(O) -> O end,
            %% reverse compare direction
            Reverse = fun("<") -> ">";("<=") -> ">=";(">") -> "<";(">=") -> "=<";("=>") -> "=<";(O) -> O end,
            {Format, Right, Reverse(Revise(string:strip(Compare))), Left};
        _ ->
            erlang:error(lists:flatten(io_lib:format("invalid key expression, no such key filed: ~s", [Expression])))
    end.

%% @doc collect value fields
collect_value_fields(Fields, "*") ->
    {field, [Field || Field = #field{comment = Comment} <- Fields, string:str(Comment, "(client)") == 0]};
collect_value_fields(Fields, Value) ->
    case re:run(Value, "^\\w+(?=\\(.*?\\)$)",[{capture, all, list}]) of
        {match, _} ->
            %% window function mode
            {window, Value};
        _ ->
            {match, List} = re:run(Value, "\\w+", [global, {capture, all, list}]),
            {field, collect_value_fields_loop(List, Fields, [])}
    end.

collect_value_fields_loop([], _Fields, List) ->
    lists:reverse(List);
collect_value_fields_loop([[Name] | T], Fields, List) ->
    case lists:keyfind(Name, #field.name, Fields) of
        false ->
            erlang:error(lists:flatten(io_lib:format("invalid key expression, no such value filed: ~s", [Name])));
        Field ->
            collect_value_fields_loop(T, Fields, [Field | List])
    end.

%% @doc parse value format
parse_values(window, _Fields, _Type, _Left, _Right) ->
    "~1024p";
parse_values(_, Fields, Type, Left, Right) ->
    lists:flatten(lists:concat([Left, string:join([parse_value_expression(Name, Format, Type) || #field{name = Name, format = Format} <- Fields], ", "), Right])).

%% parse value expression format
parse_value_expression(Field, Format, record) ->
    lists:concat([Field, " = ", Format]);
parse_value_expression(Field, Format, maps) ->
    lists:concat([Field, " => ", Format]);
parse_value_expression(_Field, Format, _) ->
    %% tuple/list/origin
    Format.

%% @doc collect value data group by key
collect_data(TableBlock, [], GroupBlock, OrderBlock, LimitBlock, window, ValueFields) ->
    ValueData = maker:select(io_lib:format("SELECT ~s FROM ~s ~s ~s ~s;", [ValueFields, TableBlock, GroupBlock, OrderBlock, LimitBlock])),
    {[], ValueData};
collect_data(TableBlock, KeyFormat, GroupBlock, OrderBlock, LimitBlock, window, ValueFields) ->
    %% collect key data
    KeyFields = string:join(["`" ++ Name ++ "`" || {_, Name, _, _} <- KeyFormat], ", "),
    KeyData = maker:select(io_lib:format("SELECT ~s FROM ~s ~s ~s", [KeyFields, TableBlock, GroupBlock, OrderBlock])),
    %% collect value data, convert char format <<"~s">> to ~s
    KeyBlock = string:join([lists:concat(["`", Name, "` = '", hd(extract(Format, "~\\w")), "'"]) || {Format, Name, _, _} <- KeyFormat], " AND "),
    %% data revise empty string '' to empty list '[]'
    ValueData = [listing:unique(maker:select(io_lib:format("SELECT ~s FROM ~s WHERE " ++ KeyBlock ++ " ~s ~s", [ValueFields, TableBlock] ++ Key ++ [OrderBlock, LimitBlock]))) || Key <- KeyData],
    {KeyData, ValueData};
collect_data(TableBlock, [], GroupBlock, OrderBlock, LimitBlock, _, ValueFields) ->
    %% case when length(`award`) = 0 then '[]' else `award` end as `award`
    %% IF(length(trim(`~s`)), `~s`, '[]') AS `~s`
    %% data revise empty string '' to empty list '[]'
    ValueBlock = string:join(lists:map(fun(#field{format = "~s", name = Name}) -> io_lib:format(" IF(length(trim(`~s`)), `~s`, '[]') AS `~s` ", [Name, Name, Name]); (#field{name = Name}) -> io_lib:format("`~s`", [Name]) end, ValueFields), ", "),
    ValueData = maker:select(io_lib:format("SELECT ~s FROM ~s ~s ~s ~s;", [ValueBlock, TableBlock, GroupBlock, OrderBlock, LimitBlock])),
    {[], ValueData};
collect_data(TableBlock, KeyFormat, GroupBlock, OrderBlock, LimitBlock, _, ValueFields) ->
    %% collect key data
    KeyFields = string:join(["`" ++ Name ++ "`" || {_, Name, _, _} <- KeyFormat], ", "),
    KeyData = maker:select(io_lib:format("SELECT ~s FROM ~s ~s ~s", [KeyFields, TableBlock, GroupBlock, OrderBlock])),
    %% collect value data, convert char format <<"~s">> to ~s
    KeyBlock = string:join([lists:concat(["`", Name, "` = '", hd(extract(Format, "~\\w")), "'"]) || {Format, Name, _, _} <- KeyFormat], " AND "),
    %% data revise empty string '' to empty list '[]'
    ValueBlock = string:join(lists:map(fun(#field{format = "~s", name = Name}) -> io_lib:format(" IF(length(trim(`~s`)), `~s`, '[]') AS `~s` ", [Name, Name, Name]); (#field{name = Name}) -> io_lib:format("`~s`", [Name]) end, ValueFields), ", "),
    ValueData = [listing:unique(maker:select(io_lib:format("SELECT ~s FROM ~s WHERE " ++ KeyBlock ++ " ~s ~s", [ValueBlock, TableBlock] ++ Key ++ [OrderBlock, LimitBlock]))) || Key <- KeyData],
    {KeyData, ValueData}.

%% @doc parse default value
parse_default(_Table, KeyFormat, []) ->
    Args = string:join(lists:duplicate(length(KeyFormat), "_"), ", "),
    {Args, "[]"};
parse_default(_Table, KeyFormat, "KEY") ->
    Value = string:join([Arg || {_, _, _, Arg} <- KeyFormat], ", "),
    {Value, Value};
parse_default(_Table, KeyFormat, "{KEY}") ->
    Value = string:join([Arg || {_, _, _, Arg} <- KeyFormat], ", "),
    {Value, "{" ++ Value ++ "}"};
parse_default(_Table, KeyFormat, "[KEY]") ->
    Value = string:join([Arg || {_, _, _, Arg} <- KeyFormat], ", "),
    {Value, "[" ++ Value ++ "]"};
parse_default(Table, KeyFormat, "#record{}") ->
    Args = string:join(lists:duplicate(length(KeyFormat), "_"), ", "),
    {Args, "#" ++ Table ++ "{}"};
parse_default(_, KeyFormat, Value) ->
    Args = string:join(lists:duplicate(length(KeyFormat), "_"), ", "),
    {Args, Value}.

%% @doc format code
format_code(Name, _KeyFormat, [], _ValueFormat, [], _GroupBlock, Args, DefaultValue, _) ->
    %% no key no value data (empty table)
    format_function_end(Name, Args, DefaultValue);
format_code(Name, _KeyFormat, [], ValueFormat, ValueData, _GroupBlock, _Args, _DefaultValue, window) ->
    %% window function mode make data as single term
    format_function_end(Name, [], format_value(ValueFormat, ValueData));
format_code(Name, _KeyFormat, [], ValueFormat, ValueData, _GroupBlock, _Args, _DefaultValue, _) ->
    %% no key make value data as list type
    format_function_end(Name, [], "[" ++ format_value(ValueFormat, ValueData) ++ "]");
format_code(Name, KeyFormat, KeyData, ValueFormat, ValueData, GroupBlock, Args, DefaultValue, ValueMode) ->
    %% data set is key value pair
    _ = length(KeyData) =/= length(ValueData) andalso erlang:error("data key/value set has different length"),
    ArgsFormat = parse_args_format(Name, KeyFormat),
    %% make key/value function
    Code = lists:zipwith(fun(K, V) -> format_function(ArgsFormat, K, ValueFormat, V, GroupBlock, ValueMode) end, KeyData, ValueData),
    io_lib:format("~s~s", [Code, format_function_end(Name, Args, DefaultValue)]).

%% format function
format_function(ArgsFormat, K, ValueFormat, V, [], _) ->
    io_lib:format("~s~s;\n", [io_lib:format(ArgsFormat, K), format_value(ValueFormat, V)]);
%% window function mode make data as single term
format_function(ArgsFormat, K, ValueFormat, V, _GroupBlock, window) ->
    io_lib:format("~s~s;\n", [io_lib:format(ArgsFormat, K), format_value(ValueFormat, V)]);
%% group by key make value data as list type
format_function(ArgsFormat, K, ValueFormat, V, _GroupBlock, _) ->
    io_lib:format("~s[~s];\n", [io_lib:format(ArgsFormat, K), format_value(ValueFormat, V)]).

%% format function end clause
format_function_end(Name, Args, DefaultValue) ->
    io_lib:format("~s(~s) ->\n    ~s.\n\n\n", [Name, Args, DefaultValue]).

%% format function value
format_value(Format, ValueData) ->
    string:join([io_lib:format(Format, Value) || Value <- ValueData], ", ").

%% parse function args format
parse_args_format(Name, Format) ->
    lists:flatten(io_lib:format("~s~s", [Name, parse_args_format_loop(Format, [], [])])).

%% without any key
parse_args_format_loop([], [], []) ->
    "() ->\n    ";
%% only equals
parse_args_format_loop([], Equals, []) ->
    io_lib:format("(~s) ->\n    ", [string:join(lists:reverse(Equals), ", ")]);
%% only ranges
parse_args_format_loop([], [], Ranges) ->
    io_lib:format("() when ~s ->\n    ", [string:join(lists:reverse(Ranges), " andalso ")]);
%% equals and range
parse_args_format_loop([], Equals, Ranges) ->
    io_lib:format("(~s) when ~s ->\n    ", [string:join(lists:reverse(Equals), ", "), string:join(lists:reverse(Ranges), " andalso ")]);
%% equal in arg directly
parse_args_format_loop([{Format, _, "=", _} | Args], Equals, Ranges) ->
    parse_args_format_loop(Args, [Format | Equals], Ranges);
%% range in when guard
parse_args_format_loop([{Format, _, "=<", Arg} | Args], Equals, Ranges) ->
    parse_args_format_loop(Args, [Arg | Equals], [io_lib:format("~s =< ~s", [Format, Arg]) | Ranges]);
%% range in when guard
parse_args_format_loop([{Format, _, "<", Arg} | Args], Equals, Ranges) ->
    parse_args_format_loop(Args, [Arg | Equals], [io_lib:format("~s < ~s", [Format, Arg]) | Ranges]);
%% range in when guard
parse_args_format_loop([{Format, _, ">=", Arg} | Args], Equals, Ranges) ->
    parse_args_format_loop(Args, [Arg | Equals], [io_lib:format("~s =< ~s", [Arg, Format]) | Ranges]);
%% range in when guard
parse_args_format_loop([{Format, _, ">", Arg} | Args], Equals, Ranges) ->
    parse_args_format_loop(Args, [Arg | Equals], [io_lib:format("~s < ~s", [Arg, Format]) | Ranges]).

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


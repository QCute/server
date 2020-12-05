%%%-------------------------------------------------------------------
%%% @doc
%%% make database data to erlang term
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
    maker:start(fun parse_table/1, List).

%%%===================================================================
%%% Internal functions
%%%===================================================================
%% @doc parse table
parse_table({File, Includes, List}) ->
    parse_table({File, Includes, List, []});
parse_table({File, Includes, List, Extra}) ->
    Code = lists:flatten([parse_code(Sql, Name) || {Sql, Name} <- List]),
    Include = [lists:flatten(io_lib:format("-include(\"~s\").\n", [X])) || X <- Includes],
    Module = filename:basename(File, ".erl"),
    Head = io_lib:format("-module(~s).\n-compile(nowarn_export_all).\n-compile(export_all).\n~s\n\n", [Module, Include]),
    [{"(?s).*", lists:concat([Head, Code, Extra])}].

parse_code(Sql, Name) ->
    %% parse sql syntax
    [ValueBlock, AllBlock, TableBlock, KeyBlock, Option, Default] = parse_sql(Sql),
    %% parse
    [Table] = extract(TableBlock, "\\w+"),
    {KeyFormat, Keys} = parse_key_block(Name, KeyBlock),
    {ValueFormat, Values} = parse_value_block(Table, ValueBlock),
    collect_data(Table, AllBlock, KeyFormat, Keys, ValueFormat, Values, Option, Name, Default).

parse_sql(Sql) ->
    %% parse sql syntax
    [ValueBlock] = extract(Sql, "(?i)(?<=\\bSELECT\\b|\\bALL\\b).*?(?=\\bAS\\b|\\bFROM\\b)"),
    [AllBlock] = extract(Sql, "(?i)(?<=\\bSELECT\\b)\\s*\\bALL\\b"),
    [TableBlock] = extract(Sql, "(?i)(?<=\\bFROM\\b).*?(?=\\bWHERE\\b|\\bGROUP BY\\b|\\bORDER BY\\b|\\bLIMIT\\b|\\bAS\\b|\\bDEFAULT\\b|;|$)"),
    [KeyBlock] = extract(Sql, "(?i)(?<=\\bWHERE\\b).*?(?=\\bGROUP BY\\b|\\bORDER BY\\b|\\bLIMIT\\b|\\bAS\\b|\\bDEFAULT\\b|;|$)"),
    [GroupBlock] = extract(Sql, "(?i)\\bGROUP BY\\b.*?(?=\\bORDER BY\\b|\\bLIMIT|\\bAS\\b|\\bDEFAULT\\b|;|$)"),
    [OrderBlock] = extract(Sql, "(?i)\\bORDER BY\\b.*?(?=\\bLIMIT|\\bAS\\b|\\bDEFAULT\\b|;|$)"),
    [LimitBlock] = extract(Sql, "(?i)\\bLIMIT\\b.*?(?=\\bAS\\b|\\bDEFAULT\\b|;|$)"),
    %% [NameBlock] = extract(Sql, "(?i)(?<=\\bAS\\b).*?(?=\\bDEFAULT\\b|;|$)"),
    [DefaultBlock] = extract(Sql, "(?i)(?<=\\bDEFAULT\\b).*?(?=\\bAS\\b|;|$)"),
    [ValueBlock -- AllBlock, string:strip(AllBlock), TableBlock, KeyBlock, lists:concat([GroupBlock, " ", OrderBlock, " ", LimitBlock]), string:strip(DefaultBlock)].

%% parse key block
parse_key_block(_Name, "") ->
    {{'KEY', "", ""}, []};
parse_key_block(Name, KeyBlock) ->
    ConditionList = lists:concat(extract(KeyBlock, "(?i)AND|OR", [global, {capture, all, list}])),
    ExpressionList = re:split(KeyBlock, "(?i)AND|OR", [trim, {return, list}]),
    length(ConditionList) + 1 =/= length(ExpressionList) andalso error(lists:flatten(io_lib:format("Invalid Key Format: ~s", [KeyBlock]))),
    %% todo or/orelse expression implement
    parse_key_loop(ExpressionList, Name, [], [], []).

parse_key_loop([], SetsName, Inner, [], List) ->
    {{'KEY', lists:concat([SetsName, "(", string:join(Inner, ", "), ") ->\n    "]), ";\n"}, lists:reverse(List)};
parse_key_loop([], SetsName, Inner, Outer, List) ->
    {{'KEY', lists:concat([SetsName, "(", string:join(Inner, ", "), ") when ", string:join(Outer, " andalso "), " ->\n    "]), ";\n"}, lists:reverse(List)};
parse_key_loop([Left | Expression], SetsName, Inner, Outer, List) ->
    case re:run(Left, "<\\s*=|=\\s*>|=\\s*<|>\\s*=|<|>", [{capture, all, list}]) of
        {match, [Match]} ->
            is_tuple(re:run(Match, "\\s+")) andalso error(lists:flatten(io_lib:format("Unknown Compare Format: ~s", [Match]))),
            [NameLeft, NameRight] = [string:trim(Item) || Item <- re:split(Left, Match, [trim, {return, binary}])],
            %% value name
            Value = binary_to_list(min(NameLeft, NameRight)),
            %% when guard
            Name = max(NameLeft, NameRight),
            When = case Name of NameRight -> lists:concat([binary_to_list(NameLeft), " ", Match, " ", "~s"]); NameLeft -> lists:concat(["~s", " ", Match, " ", binary_to_list(NameRight)]) end,
            parse_key_loop(Expression, SetsName, [Value | Inner], [When | Outer], [Name | List]);
        _ ->
            [NameLeft, NameRight] = [string:trim(Item) || Item <- re:split(Left, "=", [trim, {return, list}])],
            Name = max(NameLeft, NameRight),
            parse_key_loop(Expression, SetsName, ["~s" | Inner], Outer, [Name | List])
    end.

%% parse value block
parse_value_block(Table, ValueBlock) ->
    TypeList = [{'RECORD', lists:concat(["#", Table, "{"]), "}", "(?<=#record\\{).*?(?=\\})"}, {'MAPS', "#{", "}", "(?<=#\\{).*?(?=\\})"}, {'TUPLE', "{", "}", "(?<=\\{).*?(?=\\})"}, {'LIST', "[", "]", "(?<=\\[).*?(?=\\])"}],
    parse_value_loop(TypeList, ValueBlock).

%% parse value type
parse_value_loop([], String) ->
    Values = [list_to_binary(string:trim(Item)) || Item <- re:split(String, ",", [trim, {return, list}])],
    {{'ORIGIN', "", ""}, Values};
parse_value_loop([{Type, TypeLeft, TypeRight, RegEx} | T], String) ->
    case re:run(String, RegEx, [{capture, all, list}]) of
        {match, [Match]} ->
            Values = [list_to_binary(string:trim(Item)) || Item <- re:split(Match, ",", [trim, {return, list}])],
            {{Type, TypeLeft, TypeRight}, Values};
        _ ->
            parse_value_loop(T, String)
    end.

%% split key value
split(Value, 0, [], _, _) ->
    {[], Value};
split(Value, 0, _, _, _) ->
    {[], [Value]};
split([], _, _, NewKeyList, NewValueList) ->
    {lists:reverse(NewKeyList), lists:reverse(NewValueList)};
split([Raw | T], Length, [], KeyData, ValueData) ->
    {Key, Value} = lists:split(Length, Raw),
    split(T, Length, [], [Key | KeyData], [Value | ValueData]);
split([Raw | T], Length, Multi, KeyData, ValueData) ->
    {Key, FirstValue} = lists:split(Length, Raw),
    {GroupValueData, Other} = lists:partition(fun(Value) -> lists:sublist(Value, length(Key)) == Key end, T),
    {_, Value} = split(GroupValueData, Length, [], [], []),
    split(Other, Length, Multi, [Key | KeyData], [[FirstValue | lists:reverse(Value)] | ValueData]).

format_row([], _, _, _, List) ->
    lists:reverse(List);
format_row([Row | T], Format, Multi, Type = {ValueType = 'KEY', KeyFormat, Tail}, List) ->
    %% format key data
    Data = format_field(Row, Format, ValueType, []),
    %% format data in function
    Code = lists:flatten(lists:concat([io_lib:format(KeyFormat, Data), "~s", Tail])),
    format_row(T, Format, Multi, Type, [Code | List]);
format_row([Rows = [_ | _] | T], Format, Multi = [_ | _], Type = {ValueType, Left, Right}, List) ->
    %% group merge as separated list
    Code = lists:flatten(lists:concat(["[", string:join([lists:concat([Left, string:join(format_field(Row, Format, ValueType, []), ", "), Right]) || Row <- Rows], ", "), "]"])),
    format_row(T, Format, Multi, Type, [Code | List]);
format_row([Row | T], Format, Multi, Type = {ValueType, Left, Right}, List) ->
    %% concat left and right brackets to value
    Code = lists:flatten(lists:concat([Left, string:join(format_field(Row, Format, ValueType, []), ", "), Right])),
    format_row(T, Format, Multi, Type, [Code | List]).

format_field([], [], _, List) ->
    lists:reverse(List);
format_field([Field | T], [#field{name = Name, format = Format} | OtherFormat], Type, List) ->
    Code = lists:flatten(format_value(Type, Name, convert_format(Format, Field), Field)),
    format_field(T, OtherFormat, Type, [Code | List]).

convert_format(_Format, Value) when is_number(Value) ->
    <<"~w">>;
convert_format(Format, _Value) ->
    Format.

%% parse value expression format
format_value('RECORD', Name, Format, Value) ->
    io_lib:format(<<Name/binary, " = ", Format/binary>>, [Value]);
format_value('MAPS', Name, Format, Value) ->
    io_lib:format(<<Name/binary, " => ", Format/binary>>, [Value]);
format_value(_, _Name, Format, Value) ->
    io_lib:format(Format, [Value]).

%% format default key value
format_default(_Table, SetsName, Fields, []) ->
    Args = string:join(lists:duplicate(length(Fields), "_"), ", "),
    {io_lib:format("~s(~s) ->", [SetsName, Args]), "[]"};
format_default(_Table, SetsName, Fields, "KEY") ->
    Args = string:join([word:to_hump(binary_to_list(Name)) || #field{name = Name} <- Fields], ", "),
    {io_lib:format("~s(~s) ->", [SetsName, Args]), Args};
format_default(_Table, SetsName, Fields, "{KEY}") ->
    Args = string:join([word:to_hump(binary_to_list(Name)) || #field{name = Name} <- Fields], ", "),
    {io_lib:format("~s(~s) ->", [SetsName, Args]), "{" ++ Args ++ "}"};
format_default(_Table, SetsName, Fields, "[KEY]") ->
    Args = string:join([word:to_hump(binary_to_list(Name)) || #field{name = Name} <- Fields], ", "),
    {io_lib:format("~s(~s) ->", [SetsName, Args]), "[" ++ Args ++ "]"};
format_default(Table, SetsName, Fields, "#record{}") ->
    Args = string:join(lists:duplicate(length(Fields), "_"), ", "),
    {io_lib:format("~s(~s) ->", [SetsName, Args]), "#" ++ Table ++ "{}"};
format_default(_Table, SetsName, Fields, Value) ->
    Args = string:join([lists:concat(["_", word:to_hump(binary_to_list(Name))]) || #field{name = Name} <- Fields], ", "),
    {io_lib:format("~s(~s) ->", [SetsName, Args]), Value}.

%% collect fields info
collect_fields([], _, _, List) ->
    _ = [Field || Field = #field{name = Name, comment = Comment} <- List, string:str(binary_to_list(Comment), "(client)") =/= 0 andalso error(lists:flatten(io_lib:format("Field ~s Marked as (client) Field", [Name])))],
    lists:reverse(List);
collect_fields([<<"*">>], Table, FullFields, []) ->
    collect_fields([<<"`", Name/binary, "`">> || #field{name = Name} <- FullFields], Table, FullFields, []);
collect_fields([Name | T], Table, FullFields, List) ->
    PureName = re:replace(Name, "`|\\s*", "", [global, {return, binary}]),
    case lists:keyfind(PureName, #field.name, FullFields) of
        Field = #field{type = Type, format = <<"~s">>} ->
            NewField = Field#field{type = lists:flatten(io_lib:format(Type, [Name, Name]))},
            collect_fields(T, Table, FullFields, [NewField | List]);
        Field = #field{type = Type} ->
            NewField = Field#field{type = lists:flatten(io_lib:format(Type, [Name]))},
            collect_fields(T, Table, FullFields, [NewField | List]);
        false ->
            %% window function
            %% Inner = hd(lists:reverse(lists:concat(extract(Name, "\\w+\\s*\\(`?\\b(\\w+)\\b`?\\)$", [global, {capture, all, binary}], [[<<"">>, <<"">>]])))),
            Inner = re:replace(PureName, "\\w+\\(|\\)$", "", [global, {return, binary}]),
            case lists:keyfind(Inner, #field.name, FullFields) of
                false ->
                    error(lists:flatten(io_lib:format("Unknown Value Field: ~s in ~s", [Name, Table])));
                Field = #field{type = Type, format = <<"~s">>} ->
                    NewField = Field#field{type = lists:flatten(io_lib:format(Type, [Name, Name]))},
                    collect_fields(T, Table, FullFields, [NewField | List]);
                Field = #field{type = Type} ->
                    NewField = Field#field{type = lists:flatten(io_lib:format(Type, [Name]))},
                    collect_fields(T, Table, FullFields, [NewField | List])
            end
    end.

%% collect data
collect_data(Table, Multi, _KeyFormat, [], ValueFormat, Values, Option, SetsName, _Default) ->
    FieldsSql = io_lib:format(<<"SELECT `COLUMN_NAME`, IF(`EXTRA` = 'auto_increment', 0, IF(`COLUMN_DEFAULT` != 'NULL', `COLUMN_DEFAULT`, `GENERATION_EXPRESSION`)) AS `COLUMN_DEFAULT`, IF(`DATA_TYPE` = 'varchar', CONCAT('IF(LENGTH(TRIM(~~s)), ~~s, \\'[]\\') AS `', `COLUMN_NAME`, '`'), '~~s') AS `COLUMN_TYPE`, CASE WHEN `DATA_TYPE` = 'char' THEN '<<\"~~s\"/utf8>>' WHEN `DATA_TYPE` = 'varchar' THEN '~~s' ELSE '~~w' END AS `DATA_TYPE`, `COLUMN_COMMENT`, `ORDINAL_POSITION`, `COLUMN_KEY`, `EXTRA` FROM information_schema.`COLUMNS` WHERE `TABLE_SCHEMA` = DATABASE() AND `TABLE_NAME` = '~s' ORDER BY `ORDINAL_POSITION`;">>, [Table]),
    FullFields = parser:convert(sql:select(FieldsSql), field),
    length(FullFields) == 0 andalso error(lists:flatten(io_lib:format("Could Not Found Table: ~s", [Table]))),
    %% collect key and value fields
    NeedValueFields = collect_fields(Values, Table, FullFields, []),
    %% select data
    NeedFieldsFormat = [Type || #field{type = Type} <- NeedValueFields],
    RawData = sql:select(lists:concat(["SELECT ", string:join(NeedFieldsFormat, ", "), " FROM `", Table, "` ", Option])),
    %% format key value
    {_, MergeValueData} = split(RawData, 0, Multi, [], []),
    ValueData = format_row(MergeValueData, NeedValueFields, Multi, ValueFormat, []),
    %% parse default value
    {DefaultKey, _} = format_default(Table, SetsName, [], ""),
    %% no key
    io_lib:format("~s\n    ~s.\n\n\n", [DefaultKey, string:join(ValueData, ", ")]);
collect_data(Table, Multi, KeyFormat, Keys, ValueFormat, Values, Option, SetsName, Default) ->
    FieldsSql = io_lib:format(<<"SELECT `COLUMN_NAME`, IF(`EXTRA` = 'auto_increment', 0, IF(`COLUMN_DEFAULT` != 'NULL', `COLUMN_DEFAULT`, `GENERATION_EXPRESSION`)) AS `COLUMN_DEFAULT`, IF(`DATA_TYPE` = 'varchar', CONCAT('IF(LENGTH(TRIM(~~s)), ~~s, \\'[]\\') AS `', `COLUMN_NAME`, '`'), '~~s') AS `COLUMN_TYPE`, CASE WHEN `DATA_TYPE` = 'char' THEN '<<\"~~s\"/utf8>>' WHEN `DATA_TYPE` = 'varchar' THEN '~~s' ELSE '~~w' END AS `DATA_TYPE`, `COLUMN_COMMENT`, `ORDINAL_POSITION`, `COLUMN_KEY`, `EXTRA` FROM information_schema.`COLUMNS` WHERE `TABLE_SCHEMA` = DATABASE() AND `TABLE_NAME` = '~s' ORDER BY `ORDINAL_POSITION`;">>, [Table]),
    FullFields = parser:convert(sql:select(FieldsSql), field),
    length(FullFields) == 0 andalso error(lists:flatten(io_lib:format("Could Not Found Table: ~s", [Table]))),
    %% collect key and value fields
    NeedKeyFields = collect_fields(Keys, Table, FullFields, []),
    NeedValueFields = collect_fields(Values, Table, FullFields, []),
    %% select data
    NeedFieldsFormat = [Type || #field{type = Type} <- NeedKeyFields ++ NeedValueFields],
    RawData = sql:select(lists:concat(["SELECT ", string:join(NeedFieldsFormat, ", "), " FROM `", Table, "` ", Option])),
    %% ALL group merge
    {MergeKeyData, MergeValueData} = split(RawData, length(NeedKeyFields), Multi, [], []),
    %% format key value
    KeyData = format_row(MergeKeyData, NeedKeyFields, Multi, KeyFormat, []),
    ValueData = format_row(MergeValueData, NeedValueFields, Multi, ValueFormat, []),
    %% parse default value
    {DefaultKey, DefaultValue} = format_default(Table, SetsName, NeedKeyFields, Default),
    %% format code
    _ = length(KeyData) =/= length(ValueData) andalso erlang:error("data key/value set has different length"),
    %% make key/value function
    Code = string:join(lists:zipwith(fun(Key, Value) -> io_lib:format(Key, [Value]) end, KeyData, ValueData), ""),
    io_lib:format("~s~s\n    ~s.\n\n\n", [Code, DefaultKey, DefaultValue]).

%%%===================================================================
%%% Common Tool
%%%===================================================================
%% extract string by regex
extract(String, RegEx) ->
    extract(String, RegEx, [{capture, all, list}]).
extract(String, RegEx, Option) ->
    extract(String, RegEx, Option, [""]).
extract(String, RegEx, Option, Default) ->
    case re:run(String, RegEx, Option) of
        {match, Match} ->
            Match;
        _ ->
            Default
    end.

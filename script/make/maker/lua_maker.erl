%%%-------------------------------------------------------------------
%%% @doc
%%% make database data to lua table
%%% @end
%%%-------------------------------------------------------------------
-module(lua_maker).
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
parse_table({File, _, List}) ->
    parse_table({File, List});
parse_table({File, List}) ->
    Code = lists:flatten(string:join([parse_code(Sql, Name) || {Sql, Name} <- List], ",\n")),
    Name = word:to_lower_hump(filename:basename(File, ".lua")),
    All = lists:concat(["local ", Name, " = {\n", Code, "\n}"]),
    [{"(?s).*", All}].

parse_code(Sql, Name) ->
    %% parse sql syntax
    [ValueBlock, AllBlock, TableBlock, KeyBlock, Option] = parse_sql(Sql),
    %% parse
    [Table] = extract(TableBlock, "\\w+"),
    {KeyFormat, Keys} = parse_key_block(Name, KeyBlock),
    {ValueFormat, Values} = parse_value_block(Table, ValueBlock),
    collect_data(Table, AllBlock, KeyFormat, Keys, ValueFormat, Values, Option, Name).

parse_sql(Sql) ->
    %% parse sql syntax
    [ValueBlock]   = extract(Sql, "(?i)(?<=\\bSELECT\\b|\\bALL\\b).*?(?=\\bAS\\b|\\bFROM\\b)"),
    [AllBlock]     = extract(Sql, "(?i)(?<=\\bSELECT\\b)\\s*\\bALL\\b"),
    [TableBlock]   = extract(Sql, "(?i)(?<=\\bFROM\\b).*?(?=\\bWHERE\\b|\\bGROUP BY\\b|\\bORDER BY\\b|\\bLIMIT\\b|\\bAS\\b|\\bDEFAULT\\b|;|$)"),
    [KeyBlock]     = extract(Sql, "(?i)(?<=\\bWHERE\\b).*?(?=\\bGROUP BY\\b|\\bORDER BY\\b|\\bLIMIT\\b|\\bAS\\b|\\bDEFAULT\\b|;|$)"),
    [GroupBlock]   = extract(Sql, "(?i)\\bGROUP BY\\b.*?(?=\\bORDER BY\\b|\\bLIMIT|\\bAS\\b|\\bDEFAULT\\b|;|$)"),
    [OrderBlock]   = extract(Sql, "(?i)\\bORDER BY\\b.*?(?=\\bLIMIT|\\bAS\\b|\\bDEFAULT\\b|;|$)"),
    [LimitBlock]   = extract(Sql, "(?i)\\bLIMIT\\b.*?(?=\\bAS\\b|\\bDEFAULT\\b|;|$)"),
    [ValueBlock -- AllBlock, string:strip(AllBlock), TableBlock, KeyBlock, lists:concat([GroupBlock, " ", OrderBlock, " ", LimitBlock])].

%% parse key block
parse_key_block(_Name, "") ->
    {{'KEY', "", ""}, []};
parse_key_block(Name, KeyBlock) ->
    ConditionList = [string:to_upper(Item) || Item <- lists:concat(extract(KeyBlock, "(?i)\\bAND\\b|\\bOR\\b", [global, {capture, all, list}]))],
    ExpressionList = re:split(KeyBlock, "(?i)\\bAND|\\bOR\\b", [trim, {return, list}]),
    length(ConditionList) + 1 =/= length(ExpressionList) andalso error(lists:flatten(io_lib:format("Invalid Key Format: ~s", [KeyBlock]))),
    parse_key_loop(ExpressionList, Name, [], [], []).

parse_key_loop([], _SetsName, Inner, [], List) ->
    {{'KEY', Inner, lists:duplicate(length(Inner) - 1, " }")}, lists:reverse(List)};
parse_key_loop([Left | Expression], SetsName, Inner, Outer, List) ->
    case re:run(Left, "<\\s*=|=\\s*>|=\\s*<|>\\s*=|<|>", [{capture, all, list}]) of
        {match, [Match]} ->
            error(lists:flatten(io_lib:format("Compare Format: ~s Not Supported", [Match])));
        _ ->
            [NameLeft, NameRight] = [string:trim(Item) || Item <- re:split(Left, "=", [trim, {return, binary}])],
            Name = max(NameLeft, NameRight),
            parse_key_loop(Expression, SetsName, ["[~s] = " | Inner], Outer, [Name | List])
    end.

%% parse value block
parse_value_block(_Table, ValueBlock) ->
    TypeList = [{'MAPS_TABLE', "{ ", " }", "(?<=\\{).*?(?=\\})"}, {'TABLE', "[", "]", "(?<=\\[).*?(?=\\])"}],
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
arrange(Value, 0, _) ->
    Value;
arrange([], _, List) ->
    lists:reverse(List);
arrange([[H | T] | R], Length, List) ->
    {Group, Other} = lists:partition(fun(Value) -> hd(Value) == H end, R),
    SubGroup = [T | [tl(Row) || Row <- Group]],
    SubList = arrange(SubGroup, Length - 1, []),
    arrange(Other, Length, [{[H], SubList} | List]).

format_row([], _, _, _, List) ->
    lists:reverse(List);
format_row([{Key, Value = [{_, _} | _]} | T], Format = [FirstFormat | SubFormat], Multi, Type, List) ->
    %% format key data
    First = format_field(Key, [FirstFormat], 'ORIGIN', []),
    SubList = format_row(Value, SubFormat, Multi, Type, []),
    %% format data in key
    Code = lists:flatten(lists:concat(["[", First, "] = { ", string:join(SubList, ", "), " }"])),
    format_row(T, Format, Multi, Type, [Code | List]);
format_row([{Key, Value} | T], Format = [FirstFormat | SubFormat], Multi = [], Type, List) ->
    %% format key data
    First = format_field(Key, [FirstFormat], 'ORIGIN', []),
    SubList = format_row(Value, SubFormat, Multi, Type, []),
    %% format data in key
    Code = lists:flatten(lists:concat(["[", First, "] = ", string:join(SubList, ", "), ""])),
    format_row(T, Format, Multi, Type, [Code | List]);
format_row([{Key, Value} | T], Format = [FirstFormat | SubFormat], Multi, Type, List) ->
    %% format key data
    First = format_field(Key, [FirstFormat], 'ORIGIN', []),
    SubList = format_row(Value, SubFormat, Multi, Type, []),
    %% format data in key
    Code = lists:flatten(lists:concat(["[", First, "] = {", string:join(SubList, ", "), "}"])),
    format_row(T, Format, Multi, Type, [Code | List]);
format_row([Row | T], Format, Multi, Type = {ValueType, Left, Right}, List) ->
    %% concat left and right brackets to value
    Code = lists:concat([Left, string:join(format_field(Row, Format, ValueType, []), ", "), Right]),
    format_row(T, Format, Multi, Type, [Code | List]).

format_field([], [], _, List) ->
    lists:reverse(List);
format_field([Field | T], [#field{name = Name, format = Format = <<"~s">>} | OtherFormat], Type, List) ->
    %% replace atom to string
    String = re:replace(Field, "[a-zA-Z]\\w+", "\"&\"", [global, {return, list}]),
    %% replace erlang list [] to lua table {}
    Value = re:replace(re:replace(String, "\\[", "\\{", [global, {return, list}]), "\\]", "\\}", [global, {return, list}]),
    Code = lists:flatten(format_value(Type, Name, Format, Value)),
    format_field(T, OtherFormat, Type, [Code | List]);
format_field([Field | T], [#field{name = Name, format = Format} | OtherFormat], Type, List) ->
    Code = lists:flatten(format_value(Type, Name, convert_format(Format, Field), Field)),
    format_field(T, OtherFormat, Type, [Code | List]).

convert_format(_Format, Value) when is_number(Value) ->
    <<"~w">>;
convert_format(Format, _Value) ->
    Format.

%% parse value expression format
format_value('MAPS_TABLE', Name, Format, Value) ->
    io_lib:format(<<"[\"", Name/binary, "\"]", <<" = ">>/binary, Format/binary>>, [Value]);
format_value(_, _Name, Format, Value) ->
    io_lib:format(Format, [Value]).

%% collect fields info
collect_fields([], _, _, List) ->
    _ = [Field || Field = #field{name = Name, comment = Comment} <- List, string:str(binary_to_list(Comment), "(server)") =/= 0 andalso error(lists:flatten(io_lib:format("Field ~s Marked as (server) Field", [Name])))],
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
collect_data(Table, Multi, _KeyFormat, [], ValueFormat, Values, Option, SetsName) ->
    FieldsSql = io_lib:format(<<"SELECT `COLUMN_NAME`, IF(`EXTRA` = 'auto_increment', 0, IF(`COLUMN_DEFAULT` != 'NULL', `COLUMN_DEFAULT`, `GENERATION_EXPRESSION`)) AS `COLUMN_DEFAULT`, IF(`DATA_TYPE` = 'varchar', CONCAT('IF(LENGTH(TRIM(~~s)), ~~s, \\'{}\\') AS `', `COLUMN_NAME`, '`'), '~~s') AS `COLUMN_TYPE`, CASE WHEN `DATA_TYPE` = 'char' THEN '\"~~s\"' WHEN `DATA_TYPE` = 'varchar' THEN '~~s' ELSE '~~w' END AS `DATA_TYPE`, `COLUMN_COMMENT`, `ORDINAL_POSITION`, `COLUMN_KEY`, `EXTRA` FROM information_schema.`COLUMNS` WHERE `TABLE_SCHEMA` = DATABASE() AND `TABLE_NAME` = '~s' ORDER BY `ORDINAL_POSITION`;">>, [Table]),
    FullFields = parser:convert(sql:select(FieldsSql), field),
    length(FullFields) == 0 andalso error(lists:flatten(io_lib:format("Could Not Found Table: ~s", [Table]))),
    %% collect key and value fields
    NeedValueFields = collect_fields(Values, Table, FullFields, []),
    %% select data
    NeedFieldsFormat = [Type || #field{type = Type} <- NeedValueFields],
    RawData = sql:select(lists:concat(["SELECT ", string:join(NeedFieldsFormat, ", "), " FROM `", Table, "` ", Option])),
    %% format key value
    ArrangeData = arrange(RawData, 0, []),
    Code = string:join(format_row(ArrangeData, NeedValueFields, Multi, ValueFormat, []), ", "),
    {Left, Right} = case Multi of [] -> {"", ""}; _ -> {"{", "}"} end,
    %% no key
    io_lib:format("    [\"~s\"] = ~s~s~s", [SetsName, Left, Code, Right]);
collect_data(Table, Multi, _KeyFormat, Keys, ValueFormat, Values, Option, SetsName) ->
    FieldsSql = io_lib:format(<<"SELECT `COLUMN_NAME`, IF(`EXTRA` = 'auto_increment', 0, IF(`COLUMN_DEFAULT` != 'NULL', `COLUMN_DEFAULT`, `GENERATION_EXPRESSION`)) AS `COLUMN_DEFAULT`, IF(`DATA_TYPE` = 'varchar', CONCAT('IF(LENGTH(TRIM(~~s)), ~~s, \\'{}\\') AS `', `COLUMN_NAME`, '`'), '~~s') AS `COLUMN_TYPE`, CASE WHEN `DATA_TYPE` = 'char' THEN '\"~~s\"' WHEN `DATA_TYPE` = 'varchar' THEN '~~s' ELSE '~~w' END AS `DATA_TYPE`, `COLUMN_COMMENT`, `ORDINAL_POSITION`, `COLUMN_KEY`, `EXTRA` FROM information_schema.`COLUMNS` WHERE `TABLE_SCHEMA` = DATABASE() AND `TABLE_NAME` = '~s' ORDER BY `ORDINAL_POSITION`;">>, [Table]),
    FullFields = parser:convert(sql:select(FieldsSql), field),
    length(FullFields) == 0 andalso error(lists:flatten(io_lib:format("Could Not Found Table: ~s", [Table]))),
    %% collect key and value fields
    NeedKeyFields = collect_fields(Keys, Table, FullFields, []),
    NeedValueFields = collect_fields(Values, Table, FullFields, []),
    %% select data
    NeedFieldsFormat = [Type || #field{type = Type} <- NeedKeyFields ++ NeedValueFields],
    RawData = sql:select(lists:concat(["SELECT ", string:join(NeedFieldsFormat, ", "), " FROM `", Table, "` ", Option])),
    ArrangeData = arrange(RawData, length(NeedKeyFields), []),
    Code = string:join(format_row(ArrangeData, NeedKeyFields ++ NeedValueFields, Multi, ValueFormat, []), ",\n        "),
    io_lib:format("    [\"~s\"] = {\n        ~s\n    }", [SetsName, Code]).

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

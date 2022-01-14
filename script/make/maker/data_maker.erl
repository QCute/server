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
parse_table({File, Includes, _, List}) ->
    parse_table({File, Includes, [], List, []});
parse_table({File, Includes, _, List, Extra}) ->
    {Export, Code} = parse_code(List, [], []),
    Include = [lists:flatten(io_lib:format("-include(\"~s\").\n", [X])) || X <- Includes],
    Module = filename:basename(File, ".erl"),
    ExtraExport = parse_extra_export(Extra, []),
    Head = io_lib:format("-module(~s).\n~s~s\n~s\n", [Module, Export, ExtraExport, Include]),
    [{"(?s).*", lists:concat([Head, Code, Extra])}].

parse_extra_export([], List) ->
    lists:reverse(List);
parse_extra_export([Extra | T], List) ->
    [H | Other] = re:split(Extra, "(?<=\\.)\\s|$", [trim, {return, list}]),
    {ok, Tokens, _} = erl_scan:string(H),
    case erl_parse:parse_form(Tokens) of
        {ok, {function, _, Name, Args, _}} ->
            parse_extra_export(lists:append(Other, T), [lists:concat(["-export([", Name, "/", Args, "]).\n"]) | List]);
        _ ->
            parse_extra_export(lists:append(Other, T), List)
    end.

parse_code([], Export, Code) ->
    {lists:reverse(Export), lists:reverse(Code)};
parse_code([{Sql, Name} | T], Export, Code) ->
    %% parse sql syntax
    [ValueBlock, AllBlock, TableBlock, KeyBlock, Option, Default] = parse_sql(Sql),
    %% parse
    [Table] = extract(TableBlock, "\\w+"),
    {KeyFormat, Keys} = parse_key_block(Name, KeyBlock),
    {ValueFormat, Values} = parse_value_block(Table, ValueBlock),
    %% export
    ExportName = lists:concat(["-export([", Name, "/", length(Keys), "]).\n"]),
    Data = collect_data(Table, AllBlock, KeyFormat, Keys, ValueFormat, Values, Option, Name, Default),
    parse_code(T, [ExportName | Export], [Data | Code]).

parse_sql(Sql) ->
    %% parse sql syntax
    [ValueBlock] = extract(Sql, "(?i)(?<=\\bSELECT\\b|\\bALL\\b).*?(?=\\bAS\\b|\\bFROM\\b)"),
    [AllBlock] = extract(Sql, "(?i)(?<=\\bSELECT\\b)\\s*\\bALL\\b"),
    [TableBlock] = extract(Sql, "(?i)(?<=\\bFROM\\b).*?(?=\\bWHERE\\b|\\bGROUP BY\\b|\\bORDER BY\\b|\\bLIMIT\\b|\\bAS\\b|\\bDEFAULT\\b|;|$)"),
    [KeyBlock] = extract(Sql, "(?i)(?<=\\bWHERE\\b).*?(?=\\bGROUP BY\\b|\\bHAVING\\b|\\bORDER BY\\b|\\bLIMIT\\b|\\bAS\\b|\\bDEFAULT\\b|;|$)"),
    [GroupBlock] = extract(Sql, "(?i)\\bGROUP BY\\b.*?(?=\\bHAVING\\b|\\bORDER BY\\b|\\bLIMIT|\\bAS\\b|\\bDEFAULT\\b|;|$)"),
    [HavingBlock] = extract(Sql, "(?i)\\bHAVING\\b.*?(?=\\bORDER BY\\b|\\bLIMIT|\\bAS\\b|\\bDEFAULT\\b|;|$)"),
    [OrderBlock] = extract(Sql, "(?i)\\bORDER BY\\b.*?(?=\\bLIMIT|\\bAS\\b|\\bDEFAULT\\b|;|$)"),
    [LimitBlock] = extract(Sql, "(?i)\\bLIMIT\\b.*?(?=\\bAS\\b|\\bDEFAULT\\b|;|$)"),
    [DefaultBlock] = extract(Sql, "(?i)(?<=\\bDEFAULT\\b).*?(?=\\bAS\\b|;|$)"),
    [ValueBlock -- AllBlock, string:strip(AllBlock), TableBlock, KeyBlock, lists:concat([GroupBlock, "", HavingBlock, " ", OrderBlock, " ", LimitBlock]), string:strip(DefaultBlock)].

%% parse key block
parse_key_block(_Name, "") ->
    {{'KEY', "", ""}, []};
parse_key_block(Name, KeyBlock) ->
    ConditionList = lists:concat(extract(KeyBlock, "(?i)AND|OR", [global, {capture, all, list}])),
    ExpressionList = re:split(KeyBlock, "(?i)AND|OR", [trim, {return, list}]),
    length(ConditionList) + 1 =/= length(ExpressionList) andalso erlang:throw(lists:flatten(io_lib:format("Invalid Key Format: ~s", [KeyBlock]))),
    %% todo or/orelse expression implement
    parse_key_loop(ExpressionList, Name, [], [], []).

parse_key_loop([], SetsName, Inner, [], List) ->
    {{'KEY', lists:concat([SetsName, "(", string:join(Inner, ", "), ") ->\n    "]), ";\n"}, lists:reverse(List)};
parse_key_loop([], SetsName, Inner, Outer, List) ->
    {{'KEY', lists:concat([SetsName, "(", string:join(Inner, ", "), ") when ", string:join(Outer, " andalso "), " ->\n    "]), ";\n"}, lists:reverse(List)};
parse_key_loop([Condition | Expression], SetsName, Inner, Outer, List) ->
    case re:run(Condition, "=<|>=|<|>", [{capture, all, list}]) of
        {match, [Match]} ->
            %% is_tuple(re:run(Match, "\\s+")) andalso erlang:throw(lists:flatten(io_lib:format("Unknown Compare Format: ~s", [Match]))),
            [NameLeft, NameRight] = [string:trim(Item) || Item <- re:split(Condition, Match, [trim, {return, list}])],
            %% value name
            Name = case {string:to_lower(NameLeft), string:to_lower(NameRight)} of {NameLeft, _} -> NameLeft; {_, NameRight} -> NameRight end,
            Value = case {string:to_lower(NameLeft), string:to_lower(NameRight)} of {NameLeft, _} -> NameRight; {_, NameRight} -> NameLeft end,
            %% when guard
            When = case Name of NameRight -> lists:concat([NameLeft, " ", Match, " ", "~s"]); NameLeft -> lists:concat(["~s", " ", Match, " ", NameRight]) end,
            parse_key_loop(Expression, SetsName, [Value | Inner], [When | Outer], [Name | List]);
        _ ->
            [NameLeft, NameRight] = [string:trim(Item) || Item <- re:split(Condition, "=", [trim, {return, list}])],
            Name = case {string:to_lower(NameLeft), string:to_lower(NameRight)} of {NameLeft, _} -> NameLeft; {_, NameRight} -> NameRight end,
            parse_key_loop(Expression, SetsName, ["~s" | Inner], Outer, [Name | List])
    end.

%% parse value block
parse_value_block(Table, ValueBlock) ->
    TypeList = [{'RECORD', lists:concat(["#", Table, "{"]), "}", "(?<=#record\\{).*?(?=\\})"}, {'MAPS', "#{", "}", "(?<=#\\{).*?(?=\\})"}, {'TUPLE', "{", "}", "(?<=\\{).*?(?=\\})"}, {'LIST', "[", "]", "(?<=\\[).*?(?=\\])"}],
    parse_value_loop(TypeList, ValueBlock).

%% parse value type
parse_value_loop([], String) ->
    Values = [string:trim(Item) || Item <- re:split(String, ",", [trim, {return, list}])],
    {{'ORIGIN', "", ""}, Values};
parse_value_loop([{Type, TypeLeft, TypeRight, RegEx} | T], String) ->
    case re:run(String, RegEx, [{capture, all, list}]) of
        {match, [Match]} ->
            Values = [string:trim(Item) || Item <- re:split(Match, ",", [trim, {return, list}])],
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
    split(Other, Length, Multi, [Key | KeyData], [[FirstValue | Value] | ValueData]).

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
    Args = string:join([word:to_hump(Name) || #field{name = Name} <- Fields], ", "),
    {io_lib:format("~s(~s) ->", [SetsName, Args]), Args};
format_default(_Table, SetsName, Fields, "{KEY}") ->
    Args = string:join([word:to_hump(Name) || #field{name = Name} <- Fields], ", "),
    {io_lib:format("~s(~s) ->", [SetsName, Args]), "{" ++ Args ++ "}"};
format_default(_Table, SetsName, Fields, "[KEY]") ->
    Args = string:join([word:to_hump(Name) || #field{name = Name} <- Fields], ", "),
    {io_lib:format("~s(~s) ->", [SetsName, Args]), "[" ++ Args ++ "]"};
format_default(Table, SetsName, Fields, "#record{}") ->
    Args = string:join(lists:duplicate(length(Fields), "_"), ", "),
    {io_lib:format("~s(~s) ->", [SetsName, Args]), "#" ++ Table ++ "{}"};
format_default(_Table, SetsName, Fields, Value) ->
    Args = string:join([lists:concat(["_", word:to_hump(Name)]) || #field{name = Name} <- Fields], ", "),
    {io_lib:format("~s(~s) ->", [SetsName, Args]), Value}.

%% format function key spec
format_key_spec(Keys, []) ->
    format_spec(Keys);
format_key_spec(Keys, RawData) ->
    %% analyzer data
    analyze_field_data(1, length(Keys), Keys, RawData).

%% single mode
format_value_spec(Type, Table, [], Keys, _Fields, _RawData, _SetsName, Default) ->
    format_value_spec(Type, Table, "", "", Keys, _Fields, _RawData, _SetsName, Default);
%% list mode
format_value_spec(Type, Table, _, Keys, _Fields, _RawData, _SetsName, Default) ->
    format_value_spec(Type, Table, "[", "]", Keys, _Fields, _RawData, _SetsName, Default).
%% format
format_value_spec('RECORD', Table, Left, Right, Keys, _Fields, RawData, _SetsName, Default) ->
    io_lib:format("~s :: ~s#~s{}~s~s", [word:to_hump(Table), Left, Table, Right, format_default_spec(Keys, RawData, Default)]);
format_value_spec('MAPS', Table, Left, Right, Keys, _Fields, RawData, _SetsName, Default) ->
    io_lib:format("~s :: ~s#{}~s~s", [word:to_hump(Table), Left, Right, format_default_spec(Keys, RawData, Default)]);
format_value_spec('TUPLE', _Table, Left, Right, Keys, Fields, RawData, SetsName, Default) ->
    io_lib:format("~s :: ~s{~s}~s~s", [word:to_hump(SetsName), Left, analyze_field_data(length(Keys) + 1, length(Keys) + length(Fields), Fields, RawData), Right, format_default_spec(Keys, RawData, Default)]);
format_value_spec('LIST', Table, Left, Right, Keys, _Fields, RawData, _SetsName, Default) ->
    io_lib:format("~s :: ~slist()~s~s", [word:to_hump(Table), Left, Right, format_default_spec(Keys, RawData, Default)]);
format_value_spec('ORIGIN', _Table, Left, Right, Keys, Fields, RawData, _SetsName, Default) ->
    io_lib:format("~s~s~s~s", [Left, analyze_field_data(length(Keys) + 1, length(Keys) + length(Fields), Fields, RawData), Right, format_default_spec(Keys, RawData, Default)]).

%% format default key value
format_default_spec(_Keys, _, undefined) ->
    "";
format_default_spec(_Keys, _, []) ->
    " | Default :: []";
format_default_spec(Keys, _, "KEY") ->
    io_lib:format(" | Default :: ~s", [format_spec(Keys)]);
format_default_spec(Keys, _, "{KEY}") ->
    io_lib:format(" | Default :: {~s}", [format_spec(Keys)]);
format_default_spec(_Keys, _, "[KEY]") ->
    " | Default :: list()";
format_default_spec(_Keys, _, "#record{}") ->
    "";
format_default_spec(_Keys, _, Value) ->
    io_lib:format(" | Default :: ~s", [type:spec(parser:to_term(Value))]).

analyze_field_data(_, _, Fields, []) ->
    format_spec(Fields);
analyze_field_data(Start, End, Fields, RawData) ->
    List = analyze_field_data_loop(Start, End, Fields, RawData, []),
    %% format field name into spec
    Text = lists:zipwith(fun(#field{name = []}, Spec) -> string:join(Spec, " | "); (#field{name = Name}, Spec) -> io_lib:format("~s :: ~s", [word:to_hump(Name), string:join(Spec, " | ")]) end, Fields, List),
    string:join(Text, ", ").

analyze_field_data_loop(_, _, _, [], List) ->
    List;
analyze_field_data_loop(Start, End, Fields, [H | RawData], List) ->
    %% format is string and value is binary
    %% check spec from data
    Next = lists:zipwith(fun(#field{format = <<"<<\"~s\"/utf8>>">>}, <<_/binary>>) -> ["binary()"]; (_, Data) -> [type:spec(parser:to_term(Data))] end, Fields, lists:sublist(H, Start, End)),
    case List of
        [] ->
            analyze_field_data_loop(Start, End, Fields, RawData, Next);
        _ ->
            New = lists:zipwith(fun(N, P) -> listing:unique(listing:merge(N, P)) end, Next, List),
            analyze_field_data_loop(Start, End, Fields, RawData, New)
    end.

%% format spec type
format_spec(Fields) when is_list(Fields) ->
    string:join([format_spec(Field) || Field <- Fields], ", ");
format_spec(#field{name = Name, format = Format}) ->
    %% data type spec
    io_lib:format("~s :: ~s", [word:to_hump(Name), case Format of <<"~w">> -> "integer()"; <<"<<\"~s\"/utf8>>">> -> "binary()"; _ ->  "term()" end]).

%% collect fields info
collect_fields([], _, _, List) ->
    lists:foreach(fun(#field{name = Name, comment = Comment}) -> is_tuple(binary:match(Comment, <<"(client)">>)) andalso erlang:throw(lists:flatten(io_lib:format("Field ~s Marked as (client) Field", [Name]))) end, List),
    lists:reverse(List);
collect_fields(["*"], Table, FullFields, []) ->
    collect_fields([<<"`", Name/binary, "`">> || #field{name = Name} <- FullFields], Table, FullFields, []);
collect_fields([Sql | T], Table, FullFields, List) ->
    case re:run(Sql, "(\\w+)\\(\\s*(`?\\w+`?)\\s*\\)|`?\\w+`?", [global, {capture, all, binary}]) of
        {match, [[SqlName, Function, FieldName]]} ->
            %% in window function
            case lists:keyfind(binary:replace(FieldName, <<"`">>, <<>>, [global]), #field.name, FullFields) of
                Field = #field{name = Name, type = Type, format = <<"~s">>} ->
                    %% combine function and field as name
                    %% default as empty
                    %% format field into predefine sql
                    NewField = Field#field{name = <<(<<<<(string:to_lower(Word)):8>> || <<Word:8>> <= Function>>)/binary, "_", Name/binary>>, type = io_lib:format(Type, [SqlName, SqlName])},
                    collect_fields(T, Table, FullFields, [NewField | List]);
                Field = #field{name = Name, type = Type} ->
                    %% combine function and field as name
                    %% default as empty
                    %% format field into predefine sql
                    NewField = Field#field{name = << (<<<<(string:to_lower(Word)):8>> || <<Word:8>> <= Function>>)/binary, "_", Name/binary>>, type = io_lib:format(Type, [SqlName])},
                    collect_fields(T, Table, FullFields, [NewField | List]);
                false ->
                    erlang:throw(lists:flatten(io_lib:format("Unknown Value Field: ~s in ~s", [Sql, Table])))
            end;
        {match, [[FieldName]]} ->
            case lists:keyfind(binary:replace(FieldName, <<"`">>, <<>>, [global]), #field.name, FullFields) of
                Field = #field{type = Type, format = <<"~s">>} ->
                    %% format field into predefine sql
                    NewField = Field#field{type = io_lib:format(Type, [FieldName, FieldName])},
                    collect_fields(T, Table, FullFields, [NewField | List]);
                Field = #field{type = Type} ->
                    %% format field into predefine sql
                    NewField = Field#field{type = io_lib:format(Type, [FieldName])},
                    collect_fields(T, Table, FullFields, [NewField | List]);
                false ->
                    erlang:throw(lists:flatten(io_lib:format("Unknown Value Field: ~s in ~s", [Sql, Table])))
            end;
        _ ->
            erlang:throw(lists:flatten(io_lib:format("Could not Found any Field World: ~s", [Sql])))
    end.

%% collect data
collect_data(Table, Multi, _KeyFormat, [], ValueFormat, Values, Option, SetsName, _Default) ->
    %% FullFields = parser:convert(db:select(<<"SELECT `COLUMN_NAME`, IF(`EXTRA` = 'auto_increment', 0, IF(`COLUMN_DEFAULT` != 'NULL', `COLUMN_DEFAULT`, `GENERATION_EXPRESSION`)) AS `COLUMN_DEFAULT`, CASE WHEN `DATA_TYPE` = 'char' THEN 'CONCAT(\\'<<\"\\', ~~s, \\'\"/utf8>>\\')' WHEN `DATA_TYPE` = 'varchar' THEN CONCAT('IF(LENGTH(TRIM(~~s)), ~~s, \\'[]\\') AS `', `COLUMN_NAME`, '`') ELSE '~~s' END AS `COLUMN_TYPE`, CASE WHEN `DATA_TYPE` = 'char' THEN '<<\"~~s\"/utf8>>' WHEN `DATA_TYPE` = 'varchar' THEN '~~s' ELSE '~~w' END AS `DATA_TYPE`, `COLUMN_COMMENT`, `ORDINAL_POSITION`, `COLUMN_KEY`, `EXTRA` FROM information_schema.`COLUMNS` WHERE `TABLE_SCHEMA` = DATABASE() AND `TABLE_NAME` = '~s' ORDER BY `ORDINAL_POSITION`;">>, [Table]), field),
    FullFields = parser:convert(db:select(<<"SELECT `COLUMN_NAME`, IF(`EXTRA` = 'auto_increment', 0, IF(`COLUMN_DEFAULT` != 'NULL', `COLUMN_DEFAULT`, `GENERATION_EXPRESSION`)) AS `COLUMN_DEFAULT`, IF(`DATA_TYPE` = 'varchar', CONCAT('IF(LENGTH(TRIM(~~s)), ~~s, \\'[]\\') AS `', `COLUMN_NAME`, '`'), '~~s') AS `COLUMN_TYPE`, CASE WHEN `DATA_TYPE` = 'char' THEN '<<\"~~s\"/utf8>>' WHEN `DATA_TYPE` = 'varchar' THEN '~~s' ELSE '~~w' END AS `DATA_TYPE`, `COLUMN_COMMENT`, `ORDINAL_POSITION`, `COLUMN_KEY`, `EXTRA` FROM information_schema.`COLUMNS` WHERE `TABLE_SCHEMA` = DATABASE() AND `TABLE_NAME` = '~s' ORDER BY `ORDINAL_POSITION`;">>, [Table]), field),
    length(FullFields) == 0 andalso erlang:throw(lists:flatten(io_lib:format("Could Not Found Table: ~s", [Table]))),
    %% collect key and value fields
    NeedValueFields = collect_fields(Values, Table, FullFields, []),
    %% select data
    NeedFieldsFormat = [Type || #field{type = Type} <- NeedValueFields],
    RawData = db:select(lists:flatten(["SELECT ", string:join(NeedFieldsFormat, ", "), " FROM `", Table, "` ", Option])),
    %% spec
    Spec = io_lib:format("-spec ~s() -> ~s.", [SetsName, format_value_spec(element(1, ValueFormat), Table, Multi, [], NeedValueFields, RawData, SetsName, undefined)]),
    %% format key value
    {_, MergeValueData} = split(RawData, 0, Multi, [], []),
    ValueData = format_row(MergeValueData, NeedValueFields, Multi, ValueFormat, []),
    %% parse default value
    {DefaultKey, _} = format_default(Table, SetsName, [], ""),
    %% no key
    io_lib:format("~s~n~s\n    ~s.\n\n\n", [Spec, DefaultKey, string:join(ValueData, ", ")]);
collect_data(Table, Multi, KeyFormat, Keys, ValueFormat, Values, Option, SetsName, Default) ->
    %% FullFields = parser:convert(db:select(<<"SELECT `COLUMN_NAME`, IF(`EXTRA` = 'auto_increment', 0, IF(`COLUMN_DEFAULT` != 'NULL', `COLUMN_DEFAULT`, `GENERATION_EXPRESSION`)) AS `COLUMN_DEFAULT`, CASE WHEN `DATA_TYPE` = 'char' THEN 'CONCAT(\\'<<\"\\', ~~s, \\'\"/utf8>>\\')' WHEN `DATA_TYPE` = 'varchar' THEN CONCAT('IF(LENGTH(TRIM(~~s)), ~~s, \\'[]\\') AS `', `COLUMN_NAME`, '`') ELSE '~~s' END AS `COLUMN_TYPE`, CASE WHEN `DATA_TYPE` = 'char' THEN '<<\"~~s\"/utf8>>' WHEN `DATA_TYPE` = 'varchar' THEN '~~s' ELSE '~~w' END AS `DATA_TYPE`, `COLUMN_COMMENT`, `ORDINAL_POSITION`, `COLUMN_KEY`, `EXTRA` FROM information_schema.`COLUMNS` WHERE `TABLE_SCHEMA` = DATABASE() AND `TABLE_NAME` = '~s' ORDER BY `ORDINAL_POSITION`;">>, [Table]), field),
    FullFields = parser:convert(db:select(<<"SELECT `COLUMN_NAME`, IF(`EXTRA` = 'auto_increment', 0, IF(`COLUMN_DEFAULT` != 'NULL', `COLUMN_DEFAULT`, `GENERATION_EXPRESSION`)) AS `COLUMN_DEFAULT`, IF(`DATA_TYPE` = 'varchar', CONCAT('IF(LENGTH(TRIM(~~s)), ~~s, \\'[]\\') AS `', `COLUMN_NAME`, '`'), '~~s') AS `COLUMN_TYPE`, CASE WHEN `DATA_TYPE` = 'char' THEN '<<\"~~s\"/utf8>>' WHEN `DATA_TYPE` = 'varchar' THEN '~~s' ELSE '~~w' END AS `DATA_TYPE`, `COLUMN_COMMENT`, `ORDINAL_POSITION`, `COLUMN_KEY`, `EXTRA` FROM information_schema.`COLUMNS` WHERE `TABLE_SCHEMA` = DATABASE() AND `TABLE_NAME` = '~s' ORDER BY `ORDINAL_POSITION`;">>, [Table]), field),
    length(FullFields) == 0 andalso erlang:throw(lists:flatten(io_lib:format("Could Not Found Table: ~s", [Table]))),
    %% collect key and value fields
    NeedKeyFields = collect_fields(Keys, Table, FullFields, []),
    NeedValueFields = collect_fields(Values, Table, FullFields, []),
    %% select data
    NeedFieldsFormat = [Type || #field{type = Type} <- NeedKeyFields ++ NeedValueFields],
    RawData = db:select(lists:flatten(["SELECT ", string:join(NeedFieldsFormat, ", "), " FROM `", Table, "` ", Option])),
    %% spec
    Spec = io_lib:format("-spec ~s(~s) -> ~s.", [SetsName, format_key_spec(NeedKeyFields, RawData), format_value_spec(element(1, ValueFormat), Table, Multi, NeedKeyFields, NeedValueFields, RawData, SetsName, Default)]),
    %% ALL group merge
    {MergeKeyData, MergeValueData} = split(RawData, length(NeedKeyFields), Multi, [], []),
    %% format key value
    KeyData = format_row(MergeKeyData, NeedKeyFields, Multi, KeyFormat, []),
    ValueData = format_row(MergeValueData, NeedValueFields, Multi, ValueFormat, []),
    %% parse default value
    {DefaultKey, DefaultValue} = format_default(Table, SetsName, NeedKeyFields, Default),
    %% format code
    _ = length(KeyData) =/= length(ValueData) andalso erlang:throw(lists:flatten(io_lib:format("data key:(~w)/value:(~w) set has different length", [length(KeyData), length(ValueData)]))),
    %% make key/value function
    Code = string:join(lists:zipwith(fun(Key, Value) -> io_lib:format(Key, [Value]) end, KeyData, ValueData), ""),
    io_lib:format("~s~n~s~s\n    ~s.\n\n\n", [Spec, Code, DefaultKey, DefaultValue]).

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

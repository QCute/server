%%%-------------------------------------------------------------------
%%% @doc
%%% make database data to lua table
%%% @end
%%%-------------------------------------------------------------------
-module(lua_maker).
-export([start/1]).
-export([parse_sql/1]).
-export([parse_tag/1]).
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
parse_table(#{file := File, meta := Meta}) ->
    TableCode = lists:flatten(string:join(parse_code(Meta, []), ",\n")),
    Name = word:to_lower_hump(filename:basename(File, ".lua")),
    Code = lists:concat([Name, " = {\n", TableCode, "\n}"]),
    [#{pattern => "(?s).*", code => Code}].

parse_code([], Code) ->
    lists:reverse(Code);
parse_code([#{name := Name, sql := Sql} | T], Code) ->
    %% parse sql syntax
    Token = parse_sql(Sql, Sql, 0, 0, [], [], []),
    Form = parse_tag(Token, [], []),
    AllBlock = element(3, listing:key_find('ALL', 1, Form, {'ALL', [], []})),
    All = string:trim(AllBlock),
    %% table block
    FromBlock = element(3, listing:key_find('FROM', 1, Form, {'FROM', [], []})),
    Table = string:trim(string:replace(FromBlock, "`", "", all)),
    %% value block
    ValueBlock = element(3, listing:key_find('SELECT', 1, Form, {'SELECT', [], []})),
    %% parse field to value
    {ValueFormat, Values} = parse_value(parse_fields(ValueBlock, Table, []), All),
    %% key block
    KeyBlock = element(3, listing:key_find('WHERE', 1, Form, {'WHERE', [], []})),
    %% parse condition to key
    {KeyFormat, Keys} = parse_key(parse_condition(KeyBlock, [], []), Name, [], [], []),
    %% extra option
    {_, Group, GroupBlock} = listing:key_find('GROUP', 1, Form, {'GROUP', [], []}),
    {_, Having, HavingBlock} = listing:key_find('HAVING', 1, Form, {'HAVING', [], []}),
    {_, Order, OrderBlock} = listing:key_find('ORDER', 1, Form, {'ORDER', [], []}),
    {_, Limit, LimitBlock} = listing:key_find('LIMIT', 1, Form, {'LIMIT', [], []}),
    %% other option
    Option = [Group, GroupBlock, Having, HavingBlock, Order, OrderBlock, Limit, LimitBlock],
    Data = collect_data(Table, KeyFormat, Keys, ValueFormat, Values, Option, Name),
    parse_code(T, [Data | Code]).

%%%===================================================================
%%% parse sql part
%%%===================================================================
parse_sql(Sql) ->
    parse_sql(Sql, Sql, 0, 0, [], [], []).

parse_sql([], Sql, Skip, _, _, _, List) ->
    %% <<_:Skip/binary, Part/binary>> = Sql,
    Part = lists:sublist(Sql, Skip + 1, length(Sql)),
    lists:reverse([Part | List]);
%% back quote
parse_sql([$` | Rest], Sql, Skip, Length, [], Stack, List) ->
    %% <<_:Skip/binary, Part:Length/binary, _/binary>> = Sql,
    Part = lists:sublist(Sql, Skip + 1, Length),
    %% start
    parse_sql(Rest, Sql, Skip + Length, 1, ['BACK_QUOTE'], Stack, [Part | List]);
parse_sql([$\\, $` | Rest], Sql, Skip, Length, ['BACK_QUOTE' | Match], Stack, List) ->
    %% escape
    parse_sql(Rest, Sql, Skip, Length + 1, ['BACK_QUOTE' | Match], Stack, List);
parse_sql([$` | Rest], Sql, Skip, Length, ['BACK_QUOTE' | Match], Stack, List) ->
    %% PartLength = Length + 1,
    %% <<_:Skip/binary, Part:PartLength/binary, _/binary>> = Sql,
    Part = lists:sublist(Sql, Skip + 1, Length + 1),
    %% end
    parse_sql(Rest, Sql, Skip + Length + 1, 0, Match, Stack, [Part | List]);
%% single quote
parse_sql([$' | Rest], Sql, Skip, Length, [], Stack, List) ->
    %% <<_:Skip/binary, Part:Length/binary, _/binary>> = Sql,
    Part = lists:sublist(Sql, Skip + 1, Length),
    %% start
    parse_sql(Rest, Sql, Skip + Length, 1, ['SINGLE_QUOTE'], Stack, [Part | List]);
parse_sql([$\\, $' | Rest], Sql, Skip, Length, ['SINGLE_QUOTE' | Match], Stack, List) ->
    %% escape
    parse_sql(Rest, Sql, Skip, Length + 1, ['SINGLE_QUOTE' | Match], Stack, List);
parse_sql([$' | Rest], Sql, Skip, Length, ['SINGLE_QUOTE' | Match], Stack, List) ->
    %% PartLength = Length + 1,
    %% <<_:Skip/binary, Part:PartLength/binary, _/binary>> = Sql,
    Part = lists:sublist(Sql, Skip + 1, Length + 1),
    %% end
    parse_sql(Rest, Sql, Skip + Length + 1, 0, Match, Stack, [Part | List]);
%% double quote
parse_sql([$" | Rest], Sql, Skip, Length, [], Stack, List) ->
    %% <<_:Skip/binary, Part:Length/binary, _/binary>> = Sql,
    Part = lists:sublist(Sql, Skip + 1, Length),
    %% start
    parse_sql(Rest, Sql, Skip + Length, 1, ['DOUBLE_QUOTE'], Stack, [Part | List]);
parse_sql([$\\, $" | Rest], Sql, Skip, Length, ['DOUBLE_QUOTE' | Match], Stack, List) ->
    %% escape
    parse_sql(Rest, Sql, Skip, Length + 1, ['DOUBLE_QUOTE' | Match], Stack, List);
parse_sql([$" | Rest], Sql, Skip, Length, ['DOUBLE_QUOTE' | Match], Stack, List) ->
    %% PartLength = Length + 1,
    %% <<_:Skip/binary, Part:PartLength/binary, _/binary>> = Sql,
    Part = lists:sublist(Sql, Skip + 1, Length + 1),
    %% end
    parse_sql(Rest, Sql, Skip + Length + 1, 0, Match, Stack, [Part | List]);
%% parenthesis
parse_sql([$( | Rest], Sql, Skip, Length, [], Stack, List) ->
    %% <<_:Skip/binary, Part:Length/binary, _/binary>> = Sql,
    Part = lists:sublist(Sql, Skip + 1, Length),
    %% start
    parse_sql(Rest, Sql, Skip + Length, 1, [], ['PARENTHESIS' | Stack], [Part | List]);
parse_sql([$) | Rest], Sql, Skip, Length, [], ['PARENTHESIS' | Stack], List) ->
    %% PartLength = Length + 1,
    %% <<_:Skip/binary, Part:PartLength/binary, _/binary>> = Sql,
    Part = lists:sublist(Sql, Skip + 1, Length + 1),
    {Inner, [[$( | Left] | Root]} = lists:splitwith(fun([$( | _]) -> false; (_) -> true end, List),
    %% end
    parse_sql(Rest, Sql, Skip + Length + 1, 0, [], Stack, [[$(, Left | lists:reverse(Inner)] ++ Part | Root]);
%% tuple
parse_sql([${ | Rest], Sql, Skip, Length, [], Stack, List) ->
    %% <<_:Skip/binary, Part:Length/binary, _/binary>> = Sql,
    Part = lists:sublist(Sql, Skip + 1, Length),
    %% start
    parse_sql(Rest, Sql, Skip + Length, 1, [], ['BRACE' | Stack], [Part | List]);
parse_sql([$} | Rest], Sql, Skip, Length, [], ['BRACE' | Stack], List) ->
    %% PartLength = Length + 1,
    %% <<_:Skip/binary, Part:PartLength/binary, _/binary>> = Sql,
    Part = lists:sublist(Sql, Skip + 1, Length + 1),
    {Inner, [[${ | Left] | Root]} = lists:splitwith(fun([${ | _]) -> false; (_) -> true end, List),
    %% end
    parse_sql(Rest, Sql, Skip + Length + 1, 0, [], Stack, [[${, Left | lists:reverse(Inner)] ++ Part | Root]);
%% list
parse_sql([$[ | Rest], Sql, Skip, Length, [], Stack, List) ->
    %% <<_:Skip/binary, Part:Length/binary, _/binary>> = Sql,
    Part = lists:sublist(Sql, Skip + 1, Length),
    %% start
    parse_sql(Rest, Sql, Skip + Length, 1, [], ['BRACKET' | Stack], [Part | List]);
parse_sql([$] | Rest], Sql, Skip, Length, [], ['BRACKET' | Stack], List) ->
    %% PartLength = Length + 1,
    %% <<_:Skip/binary, Part:PartLength/binary, _/binary>> = Sql,
    Part = lists:sublist(Sql, Skip + 1, Length + 1),
    {Inner, [[$[ | Left] | Root]} = lists:splitwith(fun([$[ | _]) -> false; (_) -> true end, List),
    %% end
    parse_sql(Rest, Sql, Skip + Length + 1, 0, [], Stack, [[$[, Left | lists:reverse(Inner)] ++ Part | Root]);
%% comma
parse_sql([$, | Rest], Sql, Skip, Length, [], Stack, List) ->
    %% PartLength = Length + 1,
    %% <<_:Skip/binary, Part:PartLength/binary, _/binary>> = Sql,
    Part = lists:sublist(Sql, Skip + 1, Length),
    %% space
    parse_sql(Rest, Sql, Skip + Length + 1, 0, [], Stack, [$,, Part | List]);
%% asterisk
parse_sql([$* | Rest], Sql, Skip, Length, [], Stack, List) ->
    %% PartLength = Length + 1,
    %% <<_:Skip/binary, Part:PartLength/binary, _/binary>> = Sql,
    Part = lists:sublist(Sql, Skip + 1, Length),
    %% asterisk
    parse_sql(Rest, Sql, Skip + Length + 1, 0, [], Stack, [$*, Part | List]);
%% space
parse_sql([32 | Rest], Sql, Skip, Length, [], Stack, List) ->
    %% PartLength = Length + 1,
    %% <<_:Skip/binary, Part:Length/binary, Space:1/binary, _/binary>> = Sql,
    Part = lists:sublist(Sql, Skip + 1, Length),
    %% space
    parse_sql(Rest, Sql, Skip + Length + 1, 0, [], Stack, [32, Part | List]);
%% other
parse_sql([_ | Rest], Sql, Skip, Length, Match, Stack, List) ->
    %% in stack
    parse_sql(Rest, Sql, Skip, Length + 1, Match, Stack, List).

parse_tag(Token) ->
    parse_tag(Token, [], []).

%% parse tag
parse_tag([H | T], [], List) ->
    case string:to_upper(H) of
        "SELECT" ->
            parse_tag(T, [{'SELECT', H, []}], []);
        _ ->
            parse_tag(T, [], [H | List])
    end;
parse_tag([H | T], [{'SELECT', Origin, []} | Stack], List) ->
    case string:to_upper(H) of
        "ALL" ->
            parse_tag(T, [{'ALL', H, [H]}, {'SELECT', Origin, []} | Stack], []);
        "FROM" ->
            parse_tag(T, [{'FROM', H, []}, {'SELECT', Origin, lists:reverse(List)} | Stack], []);
        _ ->
            parse_tag(T, [{'SELECT', Origin, []} | Stack], [H | List])
    end;
parse_tag([H | T], [{'ALL', AllOrigin, AllBlock}, {'SELECT', SelectOrigin, []} | Stack], List) ->
    case string:to_upper(H) of
        "FROM" ->
            parse_tag(T, [{'FROM', H, []}, {'ALL', AllOrigin, AllBlock}, {'SELECT', SelectOrigin, lists:reverse(List)} | Stack], []);
        _ ->
            parse_tag(T, [{'ALL', AllOrigin, AllBlock}, {'SELECT', SelectOrigin, []} | Stack], [H | List])
    end;
parse_tag([H | T], [{Tag, Origin, []} | Stack], List) ->
    case string:to_upper(H) of
        "WHERE" ->
            parse_tag(T, [{'WHERE', H, []}, {Tag, Origin, lists:reverse(List)} | Stack], []);
        "GROUP" ->
            parse_tag(T, [{'GROUP', H, []}, {Tag, Origin, lists:reverse(List)} | Stack], []);
        "HAVING" ->
            parse_tag(T, [{'HAVING', H, []}, {Tag, Origin, lists:reverse(List)} | Stack], []);
        "ORDER" ->
            parse_tag(T, [{'ORDER', H, []}, {Tag, Origin, lists:reverse(List)} | Stack], []);
        "LIMIT" ->
            parse_tag(T, [{'LIMIT', H, []}, {Tag, Origin, lists:reverse(List)} | Stack], []);
        "DEFAULT" ->
            parse_tag(T, [{'DEFAULT', H, []}, {Tag, Origin, lists:reverse(List)} | Stack], []);
        _ ->
            parse_tag(T, [{Tag, Origin, []} | Stack], [H | List])
    end;
parse_tag([], [{Tag, Origin, []} | Stack], List) ->
    lists:reverse([{Tag, Origin, lists:reverse(List)} | Stack]).

%% parse fields
parse_fields(["" | T], Table, List) ->
    %% empty tag
    parse_fields(T, Table, List);
parse_fields([32 | T], Table, List) ->
    %% space
    parse_fields(T, Table, List);
parse_fields([$, | T], Table, List) ->
    %% comma tag
    parse_fields(T, Table, List);
parse_fields([N, P = [$( | H] | T], Table, List) ->
    %% maybe window function
    %% in function, single field by default
    parse_fields(T, Table, [#{type => value, format => 'ORIGIN', left => "", name => lists:droplast(H), right => "", function => (string:to_lower(N)), sql => [N | P]} | List]);
parse_fields([[${ | H] | T], Table, List) ->
    %% maps table
    parse_fields(T, Table, [#{type => value, format => 'MAPS_TABLE', left => "{ ", values => parse_fields(lists:droplast(H), Table, []), right => " }"} | List]);
parse_fields([[$[ | H] | T], Table, List) ->
    %% table
    parse_fields(T, Table, [#{type => value, format => 'TABLE', left => "{", values => parse_fields(lists:droplast(H), Table, []), right => "}"} | List]);
parse_fields([H | T], Table, List) ->
    %% maybe origin
    parse_fields(T, Table, [#{type => value, format => 'ORIGIN', left => "", name => H, function => "", right => "", sql => H} | List]);
parse_fields([], _, List) ->
    List.

parse_value([ValueFormat = #{values := Values}], All) ->
    %% multi field
    {maps:merge(ValueFormat, parse_all(All)), lists:reverse(Values)};
parse_value([ValueFormat = #{type := value, format := 'ORIGIN'}], All) ->
    %% single field
    {maps:merge(ValueFormat, parse_all(All)), [ValueFormat]}.

parse_all([]) ->
    %% single mode
    #{array => false, array_left => "", array_right => ""};
parse_all(_) ->
    %% array mode
    #{array => true, array_left => "{", array_right => "}"}.

%% parse condition
parse_condition(["" | T], Segment, List) ->
    parse_condition(T, Segment, List);
parse_condition([32 | T], Segment, List) ->
    parse_condition(T, Segment, List);
parse_condition([H = "=" | T], Segment, List) ->
    parse_condition(T, [H | Segment], List);
parse_condition([H = ">=" | T], Segment, List) ->
    parse_condition(T, [H | Segment], List);
parse_condition([H = "<=" | T], Segment, List) ->
    parse_condition(T, [H | Segment], List);
parse_condition([H = "=>" | T], Segment, List) ->
    parse_condition(T, [H | Segment], List);
parse_condition([H = "=<" | T], Segment, List) ->
    parse_condition(T, [H | Segment], List);
parse_condition([H | T], Segment, List) ->
    case string:to_upper(H) of
        "AND" ->
            parse_condition(T, [], [lists:reverse(Segment) | List]);
        _ ->
            parse_condition(T, [H | Segment], List)
    end;
parse_condition([], Segment = [_ | _], List) ->
    parse_condition([], [], [lists:reverse(Segment) | List]);
parse_condition([], [], List) ->
    lists:reverse(List).

%% parse key
parse_key([], _, Inner, [], List) ->
    {#{type => key, format => lists:reverse(Inner), head => [], tail => []}, lists:reverse(List)};
parse_key([], _, Inner, _, List) ->
    %% concat if clause condition
    Arguments = [string:trim(string:replace(Name, "`", "", all)) || Name <- lists:reverse(List)],
    {#{type => key, mode => range, format => lists:reverse(Inner), head => Arguments, tail => []}, lists:reverse(List)};
parse_key([[Left, "=", Right] | T], SetsName, Inner, Outer, List) ->
    case {string:to_lower(Left), string:to_lower(Right)} of
        {Left, _} ->
            When = lists:concat(["~s", " ", "==", " ", word:to_lower_hump(Right)]),
            parse_key(T, SetsName, [When | Inner], Outer, [Left | List]);
        {_, Right} ->
            When = lists:concat(["~s", " ", "==", " ", word:to_lower_hump(Left)]),
            parse_key(T, SetsName, [When | Inner], Outer, [Right | List])
    end;
parse_key([[Left, Op, Right] | T], SetsName, Inner, Outer, List) ->
    case {string:to_lower(Left), string:to_lower(Right)} of
        {Left, _} ->
            When = lists:concat(["~s", " ", Op, " ", word:to_lower_hump(Right)]),
            parse_key(T, SetsName, [When | Inner], [When | Outer], [Left | List]);
        {_, Right} ->
            When = lists:concat([word:to_lower_hump(Left), " ", Op, " ", "~s"]),
            parse_key(T, SetsName, [When | Inner], [When | Outer], [Right | List])
    end;
parse_key([H | _], _, _, _, _) ->
    erlang:throw(lists:flatten(io_lib:format("Invalid Condition Format: ~s", [H]))).

%%%===================================================================
%%% parse code part
%%%===================================================================
%% collect data
collect_data(Table, KeyFormat, [], ValueFormat, Values, Option, SetsName) ->
    Sql = <<"SELECT `COLUMN_NAME`, IF(`EXTRA` = 'auto_increment', 0, IF(`COLUMN_DEFAULT` != 'NULL', `COLUMN_DEFAULT`, `GENERATION_EXPRESSION`)) AS `COLUMN_DEFAULT`, IF(`DATA_TYPE` = 'varchar', CONCAT('IF(LENGTH(TRIM(~~s)), ~~s, \\'{}\\') AS `', `COLUMN_NAME`, '`'), '~~s') AS `COLUMN_TYPE`, CASE WHEN `DATA_TYPE` = 'char' THEN '\"~~s\"' WHEN `DATA_TYPE` = 'varchar' THEN '~~s' ELSE '~~w' END AS `DATA_TYPE`, `COLUMN_COMMENT`, `ORDINAL_POSITION`, `COLUMN_KEY`, `EXTRA` FROM information_schema.`COLUMNS` WHERE `TABLE_SCHEMA` = DATABASE() AND `TABLE_NAME` = '~s' ORDER BY `ORDINAL_POSITION`;">>,
    FullFields = parser:convert(db:select(Sql, [Table]), field, fun(Field = #field{name = Name, type = Type, format = Format, comment = Comment}) -> Field#field{name = binary_to_list(Name), type = binary_to_list(Type), format = binary_to_list(Format), comment = binary_to_list(Comment)} end),
    length(FullFields) == 0 andalso erlang:throw(lists:flatten(io_lib:format("Could Not Found Table: ~s", [Table]))),
    %% collect key and value fields
    NeedValueFields = collect_fields(Values, Table, FullFields, []),
    %% select data
    NeedFieldsFormat = [Type || #field{type = Type} <- NeedValueFields],
    RawData = db:select(lists:concat(["SELECT ", string:join(NeedFieldsFormat, ", "), " FROM `", Table, "` ", Option])),
    %% format key value
    ArrangeData = arrange(RawData, 0, []),
    CodeList = format_row(ArrangeData, NeedValueFields, KeyFormat, ValueFormat, []),
    format_code(CodeList, KeyFormat, ValueFormat, SetsName);
collect_data(Table, KeyFormat, Keys, ValueFormat, Values, Option, SetsName) ->
    Sql = <<"SELECT `COLUMN_NAME`, IF(`EXTRA` = 'auto_increment', 0, IF(`COLUMN_DEFAULT` != 'NULL', `COLUMN_DEFAULT`, `GENERATION_EXPRESSION`)) AS `COLUMN_DEFAULT`, IF(`DATA_TYPE` = 'varchar', CONCAT('IF(LENGTH(TRIM(~~s)), ~~s, \\'{}\\') AS `', `COLUMN_NAME`, '`'), '~~s') AS `COLUMN_TYPE`, CASE WHEN `DATA_TYPE` = 'char' THEN '\"~~s\"' WHEN `DATA_TYPE` = 'varchar' THEN '~~s' ELSE '~~w' END AS `DATA_TYPE`, `COLUMN_COMMENT`, `ORDINAL_POSITION`, `COLUMN_KEY`, `EXTRA` FROM information_schema.`COLUMNS` WHERE `TABLE_SCHEMA` = DATABASE() AND `TABLE_NAME` = '~s' ORDER BY `ORDINAL_POSITION`;">>,
    FullFields = parser:convert(db:select(Sql, [Table]), field, fun(Field = #field{name = Name, type = Type, format = Format, comment = Comment}) -> Field#field{name = binary_to_list(Name), type = binary_to_list(Type), format = binary_to_list(Format), comment = binary_to_list(Comment)} end),
    length(FullFields) == 0 andalso erlang:throw(lists:flatten(io_lib:format("Could Not Found Table: ~s", [Table]))),
    %% collect key and value fields
    NeedKeyFields = collect_fields(Keys, Table, FullFields, []),
    NeedValueFields = collect_fields(Values, Table, FullFields, []),
    %% select data
    NeedFieldsFormat = [Type || #field{type = Type} <- NeedKeyFields ++ NeedValueFields],
    RawData = db:select(lists:concat(["SELECT ", string:join(NeedFieldsFormat, ", "), " FROM `", Table, "` ", Option])),
    ArrangeData = arrange(RawData, length(NeedKeyFields), []),
    CodeList = format_row(ArrangeData, NeedKeyFields ++ NeedValueFields, KeyFormat, ValueFormat, []),
    format_code(CodeList, KeyFormat, ValueFormat, SetsName).

%% collect fields info
collect_fields([], _, _, List) ->
    %% check fields restrict
    lists:foreach(fun(#field{name = Name, comment = Comment}) -> string:str(Comment, "(server)") =/= 0 andalso erlang:throw(lists:flatten(io_lib:format("Field ~s Marked as (server) Field", [Name]))) end, List),
    lists:reverse(List);
collect_fields([#{name := $*}], Table, FullFields, []) ->
    %% use all fields as values
    Values = [lists:concat(["`", Name, "`"]) || #field{name = Name} <- FullFields],
    %% collect again
    collect_fields(Values, Table, FullFields, []);
collect_fields([#{type := value, name := SqlName, function := Function, sql := Sql} | T], Table, FullFields, List) ->
    %% for value use
    %% in window function
    RawName = string:trim(string:replace(SqlName, "`", "", all)),
    case lists:keyfind(RawName, #field.name, FullFields) of
        Field = #field{type = Type, format = "~s"} ->
            %% combine function and field as name
            %% empty string as empty list
            %% format field into predefine sql
            Name = word:to_snake(lists:concat([Function, word:to_hump(RawName)])),
            NewField = Field#field{name = Name, type = io_lib:format(Type, [Sql, Sql])},
            collect_fields(T, Table, FullFields, [NewField | List]);
        Field = #field{type = Type} ->
            %% combine function and field as name
            %% format field into predefine sql
            Name = word:to_snake(lists:concat([Function, word:to_hump(RawName)])),
            NewField = Field#field{name = Name, type = io_lib:format(Type, [Sql])},
            collect_fields(T, Table, FullFields, [NewField | List]);
        false ->
            erlang:throw(lists:flatten(io_lib:format("Unknown Value Field: ~s in ~s", [Sql, Table])))
    end;
collect_fields([SqlName | T], Table, FullFields, List) ->
    %% for key use
    case lists:keyfind(string:trim(string:replace(SqlName, "`", "", all)), #field.name, FullFields) of
        Field = #field{type = Type, format = "~s"} ->
            %% empty string as empty list
            %% format field into predefine sql
            NewField = Field#field{type = io_lib:format(Type, [SqlName, SqlName])},
            collect_fields(T, Table, FullFields, [NewField | List]);
        Field = #field{type = Type} ->
            %% format field into predefine sql
            NewField = Field#field{type = io_lib:format(Type, [SqlName])},
            collect_fields(T, Table, FullFields, [NewField | List]);
        false ->
            erlang:throw(lists:flatten(io_lib:format("Unknown Value Field: ~s in ~s", [SqlName, Table])))
    end.

%%%===================================================================
%%% format value part
%%%===================================================================
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
format_row([{Key, Value = [{_, _} | _]} | T], Format = [FirstFormat | SubFormat], KeyType = #{mode := range, format := KeyFormat}, ValueType, List) ->
    %% format key data
    First = format_field(Key, [FirstFormat], 'ORIGIN', []),
    SubList = format_row(Value, SubFormat, KeyType#{format => tl(KeyFormat)}, ValueType, []),
    %% format data in key
    Condition = io_lib:format(hd(KeyFormat), [First]),
    Clause = if List == [] -> "if"; true -> "elseif" end,
    Code = io_lib:format("~s ~s then ~s", [Clause, Condition, string:join(SubList, " ")]),
    Next = if T == [] -> ["else return nil end", Code]; true -> [Code] end,
    format_row(T, Format, KeyType, ValueType, Next ++ List);
format_row([{Key, Value} | T], Format = [FirstFormat | SubFormat], KeyType = #{mode := range, format := KeyFormat}, ValueType = #{array_left := ArrayLeft, array_right := ArrayRight}, List) ->
    %% format key data
    First = format_field(Key, [FirstFormat], 'ORIGIN', []),
    SubList = format_row(Value, SubFormat, KeyType#{format => tl(KeyFormat)}, ValueType, []),
    %% format data in key
    Condition = io_lib:format(hd(KeyFormat), [First]),
    Clause = if List == [] -> "if"; true -> "elseif" end,
    Code = io_lib:format("~s ~s then return ~s~s~s", [Clause, Condition, ArrayLeft, string:join(SubList, " "), ArrayRight]),
    Next = if T == [] -> ["else return nil end", Code]; true -> [Code] end,
    format_row(T, Format, KeyType, ValueType, Next ++ List);

format_row([{Key, Value = [{_, _} | _]} | T], Format = [FirstFormat | SubFormat], KeyType, ValueType, List) ->
    %% format key data
    First = format_field(Key, [FirstFormat], 'ORIGIN', []),
    SubList = format_row(Value, SubFormat, KeyType, ValueType, []),
    %% format data in key
    Code = lists:flatten(lists:concat(["[", First, "] = { ", string:join(SubList, ", "), " }"])),
    format_row(T, Format, KeyType, ValueType, [Code | List]);
format_row([{Key, Value} | T], Format = [FirstFormat | SubFormat], KeyType, ValueType = #{array_left := ArrayLeft, array_right := ArrayRight}, List) ->
    %% format key data
    First = format_field(Key, [FirstFormat], 'ORIGIN', []),
    SubList = format_row(Value, SubFormat, KeyType, ValueType, []),
    %% format data in key
    Code = lists:flatten(lists:concat(["[", First, "] = ", ArrayLeft, string:join(SubList, ", "), ArrayRight])),
    format_row(T, Format, KeyType, ValueType, [Code | List]);
format_row([Row | T], Format, KeyType, ValueType = #{type := value, format := ValueFormat, left := Left, right := Right}, List) ->
    %% concat left and right brackets to value
    Code = lists:concat([Left, string:join(format_field(Row, Format, ValueFormat, []), ", "), Right]),
    format_row(T, Format, KeyType, ValueType, [Code | List]).

format_field([], [], _, List) ->
    lists:reverse(List);
format_field([Field | T], [#field{name = Name, format = Format = "~s"} | OtherFormat], 'MAPS_TABLE', List) ->
    %% replace atom to string
    String = re:replace(Field, "(?!\\btrue\\b)\\b[a-zA-Z]\\w+\\b(?<!\\bfalse\\b)", "\"&\"", [global, {return, list}]),
    %% replace erlang list [] to lua table {}
    Value = re:replace(re:replace(String, "\\[", "\\{", [global, {return, list}]), "\\]", "\\}", [global, {return, list}]),
    Code = lists:flatten(io_lib:format(lists:concat(["[\"", Name, "\"]", " = ", Format]), [Value])),
    format_field(T, OtherFormat, 'MAPS_TABLE', [Code | List]);
format_field([Field | T], [#field{format = Format = "~s"} | OtherFormat], Type, List) ->
    %% replace atom to string
    String = re:replace(Field, "(?!\\btrue\\b)\\b[a-zA-Z]\\w+\\b(?<!\\bfalse\\b)", "\"&\"", [global, {return, list}]),
    %% replace erlang list [] to lua table {}
    Value = re:replace(re:replace(String, "\\[", "\\{", [global, {return, list}]), "\\]", "\\}", [global, {return, list}]),
    Code = lists:flatten(io_lib:format(Format, [Value])),
    format_field(T, OtherFormat, Type, [Code | List]);
format_field([Field | T], [#field{name = Name} | OtherFormat], 'MAPS_TABLE', List) when is_number(Field) ->
    %% object type, number format
    Code = lists:flatten(io_lib:format(lists:concat(["[\"", Name, "\"]", " = ", "~w"]), [Field])),
    format_field(T, OtherFormat, 'MAPS_TABLE', [Code | List]);
format_field([Field | T], [#field{name = Name, format = Format} | OtherFormat], 'MAPS_TABLE', List) ->
    %% object type, other format
    Code = lists:flatten(io_lib:format(lists:concat(["[\"", Name, "\"]", " = ", Format]), [Field])),
    format_field(T, OtherFormat, 'MAPS_TABLE', [Code | List]);
format_field([Field | T], [#field{} | OtherFormat], Type, List) when is_number(Field) ->
    %% other type, number format
    Code = lists:flatten(io_lib:format("~w", [Field])),
    format_field(T, OtherFormat, Type, [Code | List]);
format_field([Field | T], [#field{format = Format} | OtherFormat], Type, List) ->
    %% other type, other format
    Code = lists:flatten(io_lib:format(Format, [Field])),
    format_field(T, OtherFormat, Type, [Code | List]).

format_code(CodeList, #{mode := range, head := Arguments}, _ValueFormat, SetsName) ->
    %% range code
    Code = string:join(CodeList, "\n        "),
    io_lib:format("    [\"~s\"] = function(~s) \n        ~s\n    end", [SetsName, string:join(Arguments, ", "), Code]);
format_code(CodeList, #{format := []}, #{array := true}, SetsName) ->
    %% array value code
    Code = string:join(CodeList, ",\n        "),
    io_lib:format("    [\"~s\"] = {\n        ~s\n    }", [SetsName, Code]);
format_code(CodeList, #{format := []}, #{}, SetsName) ->
    %% array value code
    Code = string:join(CodeList, ",\n        "),
    io_lib:format("    [\"~s\"] = ~s", [SetsName, Code]);
format_code(CodeList, _KeyFormat, _ValueFormat, SetsName) ->
    %% normal code
    Code = string:join(CodeList, ",\n        "),
    io_lib:format("    [\"~s\"] = {\n        ~s\n    }", [SetsName, Code]).

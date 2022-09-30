%%%-------------------------------------------------------------------
%%% @doc
%%% make database data to erlang term
%%% @end
%%%-------------------------------------------------------------------
-module(data_maker).
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
    DefaultBlock = element(3, listing:key_find('DEFAULT', 1, Form, {'DEFAULT', [], []})),
    Default = string:trim(DefaultBlock),
    %% export
    ExportName = lists:concat(["-export([", Name, "/", length(Keys), "]).\n"]),
    Data = collect_data(Table, KeyFormat, Keys, ValueFormat, Values, Option, Name, Default),
    parse_code(T, [ExportName | Export], [Data | Code]).

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
%% record
parse_sql([$#, $r, $e, $c, $o, $r, $d, ${ | Rest], Sql, Skip, Length, [], Stack, List) ->
    %% <<_:Skip/binary, Part:Length/binary, _/binary>> = Sql,
    Part = lists:sublist(Sql, Skip + 1, Length),
    %% start
    parse_sql(Rest, Sql, Skip + Length, 8, [], ['RECORD' | Stack], [Part | List]);
parse_sql([$} | Rest], Sql, Skip, Length, [], ['RECORD' | Stack], List) ->
    %% PartLength = Length + 1,
    %% <<_:Skip/binary, Part:PartLength/binary, _/binary>> = Sql,
    Part = lists:sublist(Sql, Skip + 1, Length + 1),
    {Inner, [[$#, $r, $e, $c, $o, $r, $d, ${ | Left] | Root]} = lists:splitwith(fun([$#, $r, $e, $c, $o, $r, $d, ${ | _]) -> false; (_) -> true end, List),
    %% end
    parse_sql(Rest, Sql, Skip + Length + 1, 0, [], Stack, [[$#, $r, $e, $c, $o, $r, $d, ${, Left | lists:reverse(Inner)] ++ Part | Root]);
%% maps
parse_sql([$#, ${ | Rest], Sql, Skip, Length, [], Stack, List) ->
    %% <<_:Skip/binary, Part:Length/binary, _/binary>> = Sql,
    Part = lists:sublist(Sql, Skip + 1, Length),
    %% start
    parse_sql(Rest, Sql, Skip + Length, 2, [], ['MAPS' | Stack], [Part | List]);
parse_sql([$} | Rest], Sql, Skip, Length, [], ['MAPS' | Stack], List) ->
    %% PartLength = Length + 1,
    %% <<_:Skip/binary, Part:PartLength/binary, _/binary>> = Sql,
    Part = lists:sublist(Sql, Skip + 1, Length + 1),
    {Inner, [[$#, ${ | Left] | Root]} = lists:splitwith(fun([$#, ${ | _]) -> false; (_) -> true end, List),
    %% end
    parse_sql(Rest, Sql, Skip + Length + 1, 0, [], Stack, [[$#, ${, Left | lists:reverse(Inner)] ++ Part | Root]);
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
    parse_fields(T, Table, [#{type => value, format => 'ORIGIN', left => "", name => lists:droplast(H), right => "", function => word:to_hump(string:to_lower(N)), sql => [N | P]} | List]);
parse_fields([[$#, $r, $e, $c, $o, $r, $d, ${ | H] | T], Table, List) ->
    %% record
    parse_fields(T, Table, [#{type => value, format => 'RECORD', left => ["#", Table, "{"], values => parse_fields(lists:droplast(H), Table, []), right => "}"} | List]);
parse_fields([[$#, ${ | H] | T], Table, List) ->
    %% maps
    parse_fields(T, Table, [#{type => value, format => 'MAPS', left => "#{", values => parse_fields(lists:droplast(H), Table, []), right => "}"} | List]);
parse_fields([[${ | H] | T], Table, List) ->
    %% tuple
    parse_fields(T, Table, [#{type => value, format => 'TUPLE', left => "{", values => parse_fields(lists:droplast(H), Table, []), right => "}"} | List]);
parse_fields([[$[ | H] | T], Table, List) ->
    %% list
    parse_fields(T, Table, [#{type => value, format => 'LIST', left => "[", values => parse_fields(lists:droplast(H), Table, []), right => "]"} | List]);
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
    #{array => true, array_left => "[", array_right => "]"}.

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
parse_key([], SetsName, Inner, [], List) ->
    Head = lists:concat([SetsName, "(", string:join(lists:reverse(Inner), ", "), ") ->\n    "]),
    Tail = ";\n",
    {#{type => key, head => Head, tail => Tail}, lists:reverse(List)};
parse_key([], SetsName, Inner, Outer, List) ->
    Head = lists:concat([SetsName, "(", string:join(lists:reverse(Inner), ", "), ") when ", string:join(lists:reverse(Outer), " andalso "), " ->\n    "]),
    Tail = ";\n",
    {#{type => key, head => Head, tail => Tail}, lists:reverse(List)};
parse_key([[Left, "=", Right] | T], SetsName, Inner, Outer, List) ->
    case {string:to_lower(Left), string:to_lower(Right)} of
        {Left, _} ->
            parse_key(T, SetsName, ["~s" | Inner], Outer, [Left | List]);
        {_, Right} ->
            parse_key(T, SetsName, ["~s" | Inner], Outer, [Right | List])
    end;
parse_key([[Left, Op, Right] | T], SetsName, Inner, Outer, List) ->
    case {string:to_lower(Left), string:to_lower(Right)} of
        {Left, _} ->
            When = lists:concat(["~s", " ", Op, " ", Right]),
            parse_key(T, SetsName, [Right | Inner], [When | Outer], [Left | List]);
        {_, Right} ->
            When = lists:concat([Left, " ", Op, " ", "~s"]),
            parse_key(T, SetsName, [Left | Inner], [When | Outer], [Right | List])
    end;
parse_key([H | _], _, _, _, _) ->
    erlang:throw(lists:flatten(io_lib:format("Invalid Condition Format: ~s", [H]))).

%%%===================================================================
%%% parse code part
%%%===================================================================
%% collect data
collect_data(Table, _KeyFormat, [], ValueFormat, Values, Option, SetsName, _Default) ->
    %% FullFields = parser:convert(db:select(<<"SELECT `COLUMN_NAME`, IF(`EXTRA` = 'auto_increment', 0, IF(`COLUMN_DEFAULT` != 'NULL', `COLUMN_DEFAULT`, `GENERATION_EXPRESSION`)) AS `COLUMN_DEFAULT`, CASE WHEN `DATA_TYPE` = 'char' THEN 'CONCAT(\\'<<\"\\', ~~s, \\'\"/utf8>>\\')' WHEN `DATA_TYPE` = 'varchar' THEN CONCAT('IF(LENGTH(TRIM(~~s)), ~~s, \\'[]\\') AS `', `COLUMN_NAME`, '`') ELSE '~~s' END AS `COLUMN_TYPE`, CASE WHEN `DATA_TYPE` = 'char' THEN '<<\"~~s\"/utf8>>' WHEN `DATA_TYPE` = 'varchar' THEN '~~s' ELSE '~~w' END AS `DATA_TYPE`, `COLUMN_COMMENT`, `ORDINAL_POSITION`, `COLUMN_KEY`, `EXTRA` FROM information_schema.`COLUMNS` WHERE `TABLE_SCHEMA` = DATABASE() AND `TABLE_NAME` = '~s' ORDER BY `ORDINAL_POSITION`;">>, [Table]), field),
    Sql = <<"SELECT `COLUMN_NAME`, IF(`EXTRA` = 'auto_increment', 0, IF(`COLUMN_DEFAULT` != 'NULL', `COLUMN_DEFAULT`, `GENERATION_EXPRESSION`)) AS `COLUMN_DEFAULT`, IF(`DATA_TYPE` = 'varchar', CONCAT('IF(LENGTH(TRIM(~~s)), ~~s, \\'[]\\') AS `', `COLUMN_NAME`, '`'), '~~s') AS `COLUMN_TYPE`, CASE WHEN `DATA_TYPE` = 'char' THEN '<<\"~~s\"/utf8>>' WHEN `DATA_TYPE` = 'varchar' THEN '~~s' ELSE '~~w' END AS `DATA_TYPE`, `COLUMN_COMMENT`, `ORDINAL_POSITION`, `COLUMN_KEY`, `EXTRA` FROM information_schema.`COLUMNS` WHERE `TABLE_SCHEMA` = DATABASE() AND `TABLE_NAME` = '~s' ORDER BY `ORDINAL_POSITION`;">>,
    FullFields = parser:convert(db:select(Sql, [Table]), field, fun(Field = #field{name = Name, type = Type, format = Format, comment = Comment}) -> Field#field{name = binary_to_list(Name), type = binary_to_list(Type), format = binary_to_list(Format), comment = binary_to_list(Comment)} end),
    length(FullFields) == 0 andalso erlang:throw(lists:flatten(io_lib:format("Could Not Found Table: ~s", [Table]))),
    %% collect key and value fields
    NeedValueFields = collect_fields(Values, Table, FullFields, []),
    %% select data
    NeedFieldsFormat = [Type || #field{type = Type} <- NeedValueFields],
    RawData = db:select(lists:flatten(["SELECT ", string:join(NeedFieldsFormat, ", "), " FROM `", Table, "` ", Option])),
    %% spec
    Spec = io_lib:format("-spec ~s() -> ~s.", [SetsName, format_value_spec(ValueFormat, Table, [], NeedValueFields, RawData, SetsName, undefined)]),
    %% format key value
    {_, MergeValueData} = split(RawData, 0, ValueFormat, [], []),
    ValueData = format_row(MergeValueData, NeedValueFields, ValueFormat, []),
    %% parse default value
    {DefaultKey, _} = format_default(Table, SetsName, [], ""),
    %% no key
    io_lib:format("~s~n~s\n    ~s.\n\n\n", [Spec, DefaultKey, string:join(ValueData, ", ")]);
collect_data(Table, KeyFormat, Keys, ValueFormat, Values, Option, SetsName, Default) ->
    %% FullFields = parser:convert(db:select(<<"SELECT `COLUMN_NAME`, IF(`EXTRA` = 'auto_increment', 0, IF(`COLUMN_DEFAULT` != 'NULL', `COLUMN_DEFAULT`, `GENERATION_EXPRESSION`)) AS `COLUMN_DEFAULT`, CASE WHEN `DATA_TYPE` = 'char' THEN 'CONCAT(\\'<<\"\\', ~~s, \\'\"/utf8>>\\')' WHEN `DATA_TYPE` = 'varchar' THEN CONCAT('IF(LENGTH(TRIM(~~s)), ~~s, \\'[]\\') AS `', `COLUMN_NAME`, '`') ELSE '~~s' END AS `COLUMN_TYPE`, CASE WHEN `DATA_TYPE` = 'char' THEN '<<\"~~s\"/utf8>>' WHEN `DATA_TYPE` = 'varchar' THEN '~~s' ELSE '~~w' END AS `DATA_TYPE`, `COLUMN_COMMENT`, `ORDINAL_POSITION`, `COLUMN_KEY`, `EXTRA` FROM information_schema.`COLUMNS` WHERE `TABLE_SCHEMA` = DATABASE() AND `TABLE_NAME` = '~s' ORDER BY `ORDINAL_POSITION`;">>, [Table]), field),
    Sql = <<"SELECT `COLUMN_NAME`, IF(`EXTRA` = 'auto_increment', 0, IF(`COLUMN_DEFAULT` != 'NULL', `COLUMN_DEFAULT`, `GENERATION_EXPRESSION`)) AS `COLUMN_DEFAULT`, IF(`DATA_TYPE` = 'varchar', CONCAT('IF(LENGTH(TRIM(~~s)), ~~s, \\'[]\\') AS `', `COLUMN_NAME`, '`'), '~~s') AS `COLUMN_TYPE`, CASE WHEN `DATA_TYPE` = 'char' THEN '<<\"~~s\"/utf8>>' WHEN `DATA_TYPE` = 'varchar' THEN '~~s' ELSE '~~w' END AS `DATA_TYPE`, `COLUMN_COMMENT`, `ORDINAL_POSITION`, `COLUMN_KEY`, `EXTRA` FROM information_schema.`COLUMNS` WHERE `TABLE_SCHEMA` = DATABASE() AND `TABLE_NAME` = '~s' ORDER BY `ORDINAL_POSITION`;">>,
    FullFields = parser:convert(db:select(Sql, [Table]), field, fun(Field = #field{name = Name, type = Type, format = Format, comment = Comment}) -> Field#field{name = binary_to_list(Name), type = binary_to_list(Type), format = binary_to_list(Format), comment = binary_to_list(Comment)} end),
    length(FullFields) == 0 andalso erlang:throw(lists:flatten(io_lib:format("Could Not Found Table: ~s", [Table]))),
    %% collect key and value fields
    NeedKeyFields = collect_fields(Keys, Table, FullFields, []),
    NeedValueFields = collect_fields(Values, Table, FullFields, []),
    %% select data
    NeedFieldsFormat = [Type || #field{type = Type} <- NeedKeyFields ++ NeedValueFields],
    RawData = db:select(lists:flatten(["SELECT ", string:join(NeedFieldsFormat, ", "), " FROM `", Table, "` ", Option])),
    %% spec
    Spec = io_lib:format("-spec ~s(~s) -> ~s.", [SetsName, format_key_spec(NeedKeyFields, RawData), format_value_spec(ValueFormat, Table, NeedKeyFields, NeedValueFields, RawData, SetsName, Default)]),
    %% ALL group merge
    {MergeKeyData, MergeValueData} = split(RawData, length(NeedKeyFields), ValueFormat, [], []),
    %% format key value
    KeyData = format_row(MergeKeyData, NeedKeyFields, KeyFormat, []),
    ValueData = format_row(MergeValueData, NeedValueFields, ValueFormat, []),
    %% parse default value
    {DefaultKey, DefaultValue} = format_default(Table, SetsName, NeedKeyFields, Default),
    %% format code
    _ = length(KeyData) =/= length(ValueData) andalso erlang:throw(lists:flatten(io_lib:format("data key:(~w)/value:(~w) set has different length", [length(KeyData), length(ValueData)]))),
    %% make key/value function
    Code = string:join(lists:zipwith(fun(Key, Value) -> io_lib:format(Key, [Value]) end, KeyData, ValueData), ""),
    io_lib:format("~s~n~s~s\n    ~s.\n\n\n", [Spec, Code, DefaultKey, DefaultValue]).

%% collect fields info
collect_fields([], _, _, List) ->
    %% check fields restrict
    lists:foreach(fun(#field{name = Name, comment = Comment}) -> string:str(Comment, "(client)") =/= 0 andalso erlang:throw(lists:flatten(io_lib:format("Field ~s Marked as (client) Field", [Name]))) end, List),
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
%%% format spec part
%%%===================================================================
%% format function key spec
format_key_spec(Keys, []) ->
    %% without key data
    format_spec(Keys);
format_key_spec(Keys, RawData) ->
    %% analyzer key data
    analyze_field_data(1, length(Keys), Keys, RawData).

%% format
format_value_spec(#{format := 'RECORD', array_left := ArrayLeft, left := Left, right := Right, array_right := ArrayRight}, Table, Keys, _Fields, RawData, _SetsName, Default) ->
    %% record spec
    DefaultSpec = format_default_spec(Keys, RawData, Default),
    [word:to_hump(Table), " :: ", ArrayLeft, Left, Right, ArrayRight, DefaultSpec];
format_value_spec(#{format := 'MAPS', array_left := ArrayLeft, left := Left, right := Right, array_right := ArrayRight}, Table, Keys, _Fields, RawData, _SetsName, Default) ->
    %% maps spec
    DefaultSpec = format_default_spec(Keys, RawData, Default),
    [word:to_hump(Table), " :: ", ArrayLeft, Left, Right, ArrayRight, DefaultSpec];
format_value_spec(#{format := 'TUPLE', array_left := ArrayLeft, left := Left, right := Right, array_right := ArrayRight}, _Table, Keys, Fields, RawData, SetsName, Default) ->
    %% tuple spec, with element spec
    ElementSpec = analyze_field_data(length(Keys) + 1, length(Keys) + length(Fields), Fields, RawData),
    DefaultSpec = format_default_spec(Keys, RawData, Default),
    [word:to_hump(SetsName), " :: ", ArrayLeft, Left, ElementSpec, Right, ArrayRight, DefaultSpec];
format_value_spec(#{format := 'LIST', array_left := ArrayLeft, array_right := ArrayRight}, Table, Keys, _Fields, RawData, _SetsName, Default) ->
    %% list spec
    DefaultSpec = format_default_spec(Keys, RawData, Default),
    [word:to_hump(Table), " :: ", ArrayLeft, "list()", ArrayRight, DefaultSpec];
format_value_spec(#{format := 'ORIGIN', array_left := ArrayLeft, array_right := ArrayRight}, _Table, Keys, Fields, RawData, _SetsName, Default) ->
    %% origin value spec
    ValueSpec = analyze_field_data(length(Keys) + 1, length(Keys) + length(Fields), Fields, RawData),
    DefaultSpec = format_default_spec(Keys, RawData, Default),
    [ArrayLeft, ValueSpec, ArrayRight, DefaultSpec].

%% format default key value
format_default_spec(_Keys, _, undefined) ->
    %% empty default
    "";
format_default_spec(_Keys, _, []) ->
    %% empty list as default
    " | Default :: []";
format_default_spec(_Keys, _, "#record{}") ->
    %% record as default
    "";
format_default_spec(Keys, _, "KEY") ->
    %% keys as default
    [" | Default :: ", format_spec(Keys)];
format_default_spec(Keys, _, "{KEY}") ->
    %% tuple keys as default
    [" | Default :: {", format_spec(Keys), "}"];
format_default_spec(_Keys, _, "[KEY]") ->
    %% list keys as default
    " | Default :: list()";
format_default_spec(_Keys, _, Value) ->
    %% other value as default
    [" | Default :: ", type:spec(parser:to_term(Value))].

analyze_field_data(_, _, Fields, []) ->
    format_spec(Fields);
analyze_field_data(Start, End, Fields, RawData) ->
    List = analyze_field_data_loop(Start, End, Fields, RawData, []),
    %% format field name into spec
    Text = lists:zipwith(fun(#field{name = []}, Spec) -> string:join(Spec, " | "); (#field{name = Name}, Spec) -> [word:to_hump(Name), " :: ", string:join(Spec, " | ")] end, Fields, List),
    string:join(Text, ", ").

analyze_field_data_loop(_, _, _, [], List) ->
    List;
analyze_field_data_loop(Start, End, Fields, [H | RawData], []) ->
    %% check spec from data
    Next = lists:zipwith(fun(#field{format = "<<\"~s\"/utf8>>"}, <<_/binary>>) -> ["binary()"]; (_, Data) -> [type:spec(parser:to_term(Data))] end, Fields, lists:sublist(H, Start, End)),
    analyze_field_data_loop(Start, End, Fields, RawData, Next);
analyze_field_data_loop(Start, End, Fields, [H | RawData], List) ->
    %% format is string and value is binary
    %% check spec from data
    Next = lists:zipwith(fun(#field{format = "<<\"~s\"/utf8>>"}, <<_/binary>>) -> ["binary()"]; (_, Data) -> [type:spec(parser:to_term(Data))] end, Fields, lists:sublist(H, Start, End)),
    New = lists:zipwith(fun(N, P) -> listing:unique(listing:merge(N, P)) end, Next, List),
    analyze_field_data_loop(Start, End, Fields, RawData, New).

%% format spec type
format_spec(Fields) when is_list(Fields) ->
    string:join([format_spec(Field) || Field <- Fields], ", ");
format_spec(#field{name = Name, format = Format}) ->
    %% data type spec
    [word:to_hump(Name), " :: ", case Format of "~w" -> "integer()"; "<<\"~s\"/utf8>>" -> "binary()"; _ ->  "term()" end].

%%%===================================================================
%%% format value part
%%%===================================================================
%% split key value
split(Value, 0, #{array := false}, _, _) ->
    {[], Value};
split(Value, 0, _, _, _) ->
    {[], [Value]};
split([], _, _, NewKeyList, NewValueList) ->
    {lists:reverse(NewKeyList), lists:reverse(NewValueList)};
split([Raw | T], Length, ValueFormat = #{array := false}, KeyData, ValueData) ->
    %% split key value
    {Key, Value} = lists:split(Length, Raw),
    split(T, Length, ValueFormat, [Key | KeyData], [Value | ValueData]);
split([Raw | T], Length, ValueFormat, KeyData, ValueData) ->
    %% split key value
    {Key, FirstValue} = lists:split(Length, Raw),
    %% group by key value list
    {GroupValueData, Other} = lists:partition(fun(Value) -> lists:sublist(Value, length(Key)) == Key end, T),
    %% merge key value list
    {_, Value} = split(GroupValueData, Length, [], [], []),
    split(Other, Length, ValueFormat, [Key | KeyData], [[FirstValue | Value] | ValueData]).

%% format row value
format_row([], _, _, List) ->
    lists:reverse(List);
format_row([Row | T], FieldFormat, Type = #{type := key, head := KeyFormat, tail := Tail}, List) ->
    %% format key data
    Data = format_field(Row, FieldFormat, 'ORIGIN', []),
    %% format data in function
    Code = lists:flatten(lists:concat([io_lib:format(KeyFormat, Data), "~s", Tail])),
    format_row(T, FieldFormat, Type, [Code | List]);
format_row([Rows = [_ | _] | T], FieldFormat, Type = #{type := value, format := ValueFormat, array := true, left := Left, right := Right}, List) ->
    %% group merge as separated list
    Code = lists:flatten(lists:concat(["[", string:join([lists:concat([Left, string:join(format_field(Row, FieldFormat, ValueFormat, []), ", "), Right]) || Row <- Rows], ", "), "]"])),
    format_row(T, FieldFormat, Type, [Code | List]);
format_row([Row | T], FieldFormat, Type = #{type := value, format := ValueFormat, left := Left, right := Right}, List) ->
    %% concat left and right brackets to value
    Code = lists:flatten(lists:concat([Left, string:join(format_field(Row, FieldFormat, ValueFormat, []), ", "), Right])),
    format_row(T, FieldFormat, Type, [Code | List]).

%% format field value
format_field([], [], _, List) ->
    lists:reverse(List);
format_field([Field | T], [#field{name = Name} | OtherFormat], 'RECORD', List) when is_number(Field) ->
    %% type record, number value
    Code = lists:flatten(io_lib:format(lists:concat([Name, " = ", "~w"]), [Field])),
    format_field(T, OtherFormat, 'RECORD', [Code | List]);
format_field([Field | T], [#field{name = Name, format = Format} | OtherFormat], 'RECORD', List) ->
    %% type record, other value
    Code = lists:flatten(io_lib:format(lists:concat([Name, " = ", Format]), [Field])),
    format_field(T, OtherFormat, 'RECORD', [Code | List]);
format_field([Field | T], [#field{name = Name} | OtherFormat], 'MAPS', List) when is_number(Field) ->
    %% type maps, number value
    Code = lists:flatten(io_lib:format(lists:concat([Name, " => ", "~w"]), [Field])),
    format_field(T, OtherFormat, 'MAPS', [Code | List]);
format_field([Field | T], [#field{name = Name, format = Format} | OtherFormat], 'MAPS', List) ->
    %% type maps, other value
    Code = lists:flatten(io_lib:format(lists:concat([Name, " => ", Format]), [Field])),
    format_field(T, OtherFormat, 'MAPS', [Code | List]);
format_field([Field | T], [#field{} | OtherFormat], Type, List) when is_number(Field) ->
    %% type raw, number format
    Code = lists:flatten(io_lib:format("~w", [Field])),
    format_field(T, OtherFormat, Type, [Code | List]);
format_field([Field | T], [#field{format = Format} | OtherFormat], Type, List) ->
    %% type raw, other format
    Code = lists:flatten(io_lib:format(Format, [Field])),
    format_field(T, OtherFormat, Type, [Code | List]).

%% format default key value
format_default(_Table, SetsName, Fields, []) ->
    %% without default, empty list as default
    Args = string:join(lists:duplicate(length(Fields), "_"), ", "),
    {io_lib:format("~s(~s) ->", [SetsName, Args]), "[]"};
format_default(_Table, SetsName, Fields, "KEY") ->
    %% the key as default
    Args = string:join([word:to_hump(Name) || #field{name = Name} <- Fields], ", "),
    {io_lib:format("~s(~s) ->", [SetsName, Args]), Args};
format_default(_Table, SetsName, Fields, "{KEY}") ->
    %% the tuple key as default
    Args = string:join([word:to_hump(Name) || #field{name = Name} <- Fields], ", "),
    {io_lib:format("~s(~s) ->", [SetsName, Args]), lists:concat(["{", Args, "}"])};
format_default(_Table, SetsName, Fields, "[KEY]") ->
    %% the list key as default
    Args = string:join([word:to_hump(Name) || #field{name = Name} <- Fields], ", "),
    {io_lib:format("~s(~s) ->", [SetsName, Args]), lists:concat(["{", Args, "}"])};
format_default(Table, SetsName, Fields, "#record{}") ->
    %% the record as default
    Args = string:join(lists:duplicate(length(Fields), "_"), ", "),
    {io_lib:format("~s(~s) ->", [SetsName, Args]), lists:concat(["#", Table, "{}"])};
format_default(_Table, SetsName, Fields, Value) ->
    %% other value as default
    Args = string:join([lists:concat(["_", word:to_hump(Name)]) || #field{name = Name} <- Fields], ", "),
    {io_lib:format("~s(~s) ->", [SetsName, Args]), Value}.

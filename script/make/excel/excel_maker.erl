%%%-------------------------------------------------------------------
%%% @doc
%%% make database data to excel and excel to database data
%%% @end
%%%-------------------------------------------------------------------
-module(excel_maker).
-export([to_book/2]).
-export([to_sheet/2]).
-export([to_collection/2]).
-export([to_table/2]).
-record(field, {table = <<>>, name = <<>>, default = <<>>, type = <<>>, format = <<>>, comment = <<>>, key = <<>>, extra = <<>>, expression = <<>>, position = 0}).
%%%===================================================================
%%% Table to File
%%%===================================================================
%% @doc table to book
-spec to_book(Table :: string(), Path :: file:filename()) -> ok.
to_book(Table, Path) ->
    %% connect database
    maker:connect_database(),

    %% take configure from data script
    {ErlFlag, ErlForm} = epp:parse_file(maker:relative_path("script/make/erl/erl_script.erl"), [], []),
    ErlFlag =/= ok andalso erlang:throw(lists:flatten(io_lib:format("cound not found erl script: ~p", [ErlForm]))),
    {function, _, data, _, [{clause, _, [], [], ErlCons}]} = lists:keyfind(data, 3, ErlForm),
    ErlDataConfigureList = parser:evaluate(erl_prettypr:format(erl_syntax:form_list(ErlCons))),
    %% ErlConfigure = [#{comment => Comment, sql => Sql} || #{file := File, comment := Comment, sql := Sql} <- ErlDataConfigureList, filename:basename(File) == Table orelse filename:basename(File, ".erl") == Table],

    %% take configure from js script
    {JsFlag, JsForm} = epp:parse_file(maker:relative_path("script/make/js/js_script.erl"), [], []),
    JsFlag =/= ok andalso erlang:throw(lists:flatten(io_lib:format("cound not found js script: ~p", [JsForm]))),
    {function, _, js, _, [{clause, _, [], [], JsCons}]} = lists:keyfind(js, 3, JsForm),
    JsDataConfigureList = parser:evaluate(erl_prettypr:format(erl_syntax:form_list(JsCons))),
    %% JsConfigure = [#{comment => Comment, sql => Sql} || #{file := File, comment := Comment, sql := Sql} <- JsDataConfigureList, filename:basename(File) == Table orelse filename:basename(File, ".js") == Table],

    %% take configure from lua script
    {LuaFlag, LuaForm} = epp:parse_file(maker:relative_path("script/make/lua/lua_script.erl"), [], []),
    LuaFlag =/= ok andalso erlang:throw(lists:flatten(io_lib:format("cound not found lua script: ~p", [LuaForm]))),
    {function, _, lua, _, [{clause, _, [], [], LuaCons}]} = lists:keyfind(lua, 3, LuaForm),
    LuaDataConfigureList = parser:evaluate(erl_prettypr:format(erl_syntax:form_list(LuaCons))),
    %% LuaConfigure = [#{comment => Comment, sql => Sql} || #{file := File, comment := Comment, sql := Sql} <- LuaDataConfigureList, filename:basename(File) == Table orelse filename:basename(File, ".lua") == Table],

    %% all configure list
    Configure = [Configure || Configure = #{file := File} <- ErlDataConfigureList ++ JsDataConfigureList ++ LuaDataConfigureList, filename:basename(File) == Table],
    #{comment := Comment, sql := Sql} = (length(Configure) =/= 1 andalso erlang:throw(lists:flatten(io_lib:format("cound not found file `~ts` in erl/js/lua script", [Table])))) orelse hd(Configure),
    %% TableList = listing:unique([lists:concat(string:replace(string:trim(element(3, lists:keyfind('FROM', 1, Form))), "`", "", all)) || Form <- Configure]),
    %% take table from sql
    TableList = listing:unique([From || #{from := From} <- Sql]),
    TableList == [] andalso erlang:throw(lists:flatten(io_lib:format("cound not found table in erl/js/lua script", []))),

    %% Because of system compatibility problems
    %% because of the utf8/gbk character set problem, use table name as file name
    %% load table list data
    lists:foreach(fun(TableName) -> export_file(Path, Comment, parse_table(TableName)) end, TableList).


%% @doc table to sheet file
-spec to_sheet(Table :: string(), Path :: file:filename()) -> ok.
to_sheet(Table, Path) ->
    %% connect database
    maker:connect_database(),
    %% Because of system compatibility problems
    %% because of the utf8/gbk character set problem, use table name as file name
    #{file := Name, sheet := Sheet} = parse_table(Table),
    export_file(Path, Name, Sheet).


export_file(Path, Name, Sheet) ->
    %% remove duplicate sheet, set the validation and reference behind the sheet
    %% UniqueSortData = lists:sort(fun({_, _, V}, _) -> is_list(V) end, listing:key_unique(1, Data)),
    %% specific path
    SpecificPath = lists:concat([filename:absname(Path), "/"]),
    %% !!! different os shell need different encode type
    %% !!! such windows nt with gbk need characters list/binary int
    %% !!! the unix shell with utf8 need characters list/binary
    %% characters list int
    FileName = unicode:characters_to_binary(lists:concat([SpecificPath, Name, ".xlsm"])),
    filelib:ensure_dir(SpecificPath),
    %% export sheet
    Binary = excel_maker_xls:write(Name, Sheet),
    %% remove old
    file:delete(FileName),
    file:write_file(FileName, Binary).


%%%===================================================================
%%% parse table part
%%%===================================================================

parse_table(Table) ->
    %% fetch table comment
    TableNameComment = db:select(lists:concat(["SELECT `TABLE_NAME`, `TABLE_COMMENT` FROM information_schema.`TABLES` WHERE `TABLE_SCHEMA` = DATABASE() AND `TABLE_NAME` = '", Table, "'"])),
    TableNameComment == [] andalso erlang:throw(lists:flatten(io_lib:format("no such table: ~s", [Table]))),
    [[TableName, TableComment]] = TableNameComment,
    SheetName = lists:concat([unicode:characters_to_list(TableComment), "-", unicode:characters_to_list(TableName)]),

    %% fetch table fields
    Columns = string:join([
        "`TABLE_NAME`",
        "`COLUMN_NAME`",
        "`COLUMN_DEFAULT`",
        "`COLUMN_TYPE`",
        "`DATA_TYPE`",
        "`COLUMN_COMMENT`",
        "`COLUMN_KEY`",
        "`EXTRA`",
        "`GENERATION_EXPRESSION`",
        "`ORDINAL_POSITION`"
    ], ", "),

    Sql = lists:concat([
        "SELECT",
        " ", Columns,
        "FROM",
        " ", "information_schema.`COLUMNS`",
        "WHERE",
        " ", "`TABLE_SCHEMA` = DATABASE()",
        "AND", " ",
        "`TABLE_NAME` = '", Table, "'", " ",
        "ORDER BY",
        " ", "ORDINAL_POSITION", " ", "ASC"
    ]),

    %% fetch table fields
    Fields = parser:convert(db:select(Sql), field, fun(Field = #field{type = Type}) -> Field#field{type = lists:last(binary:split(Type, <<" ">>))} end),
    Fields == [] andalso erlang:throw(lists:flatten(io_lib:format("Table: ~s Not Found ", [Table]))),

    %% parse enum/set validation
    Keys = string:join([db:dot(lists:concat([Table, ".", type:to_list(Name)])) || #field{name = Name, key = <<"PRI">>} <- Fields], ", "),
    Meta = parse_validation_field(Fields, Table, "validation_data", key, value, 1, #{}),
    MetaList = maps:values(Meta),
    Select = string:join([Select || #{select := Select} <- MetaList], ", "),
    Join = string:join([Join || #{join := Join} <- MetaList, Join =/= []], " "),

    %% fetch all data
    SourceData = db:select(lists:concat([
        "SELECT", " ",
        Select, " ",
        "FROM", " ", type:to_list(Table), " ",
        Join, " ",
        "GROUP BY", " ", Keys
    ])),

    %% add column comment and validation data
    ColumnName = [unicode:characters_to_list(Name) || #field{name = Name} <- Fields],
    ColumnType = [unicode:characters_to_list(Format) || #field{format = Format} <- Fields],
    ColumnComment = [unicode:characters_to_list(Comment) || #field{comment = Comment} <- Fields],
    #{data := TableData, meta := TableMeta} = split([ColumnName, ColumnType, ColumnComment | SourceData], Meta),
    Sheet = #{name => SheetName, state => "", protected => false, data => TableData, meta => TableMeta},

    %% validation sheet
    ValidationSheet = maps:from_list([{Name, #{name => Name, state => "hidden", protected => true, data => Validation, meta => #{}}} || #{type := Type, sheet := Name, validation := Validation} <- maps:values(TableMeta), Type == enum orelse Type == set]),

    %% all sheet data
    %% convert unicode binary list to binary
    #{file => SheetName, sheet => ValidationSheet#{SheetName => Sheet}}.

%% load validation data
parse_validation_field([], _, _, _, _, _, Map) ->
    Map;

parse_validation_field([#field{name = Name, format = Format = <<"enum">>, type = <<"enum", Type/binary>>} | T], Table, Foreign, Key, Value, Index, Map) ->
    Alias = lists:concat([
        Foreign, "_", Index
    ]),

    Select = lists:concat([
        "IFNULL(`", Alias, "`.`", Value, "`, `", Table, "`.", "`", type:to_list(Name), "`) AS `", type:to_list(Name), "`"
    ]),

    Join = lists:concat([
        "LEFT JOIN `", Foreign, "` AS ", "`", Alias, "`", " ",
        "ON ", "(",  "`", Alias, "`", ".", "`type` = '", Table, ".", binary_to_list(Name), "'", " OR ", "`", Alias, "`", ".", "`type` = '", binary_to_list(Name), "'", " OR ", "`", Alias, "`", ".", "`type` = ''", ")", " ",
        "AND ", "`", Alias, "`", ".", "`", Key, "` = `", Table, "`", ".", "`", type:to_list(Name), "`"
    ]),

    Length = byte_size(Type) - 2,
    <<"(", OptionString:Length/binary, ")">> = Type,
    OptionList = [unicode:characters_to_list(Option) || Option <- binary:split(OptionString, <<",">>, [global])],

    ValidationData = db:select(lists:concat([
        "SELECT", " `", Value, "`", ",", "`", Key, "` ",
        "FROM", " `", Foreign, "` ",
        "WHERE",  " `", Key, "` ", "IN(", string:join(OptionList, ","), ")", " ",
        "AND ", "(", "`type` = '", Table, ".", binary_to_list(Name), "'", " OR ", "`type` = '", binary_to_list(Name), "'", " OR ", "`type` = ''", ")"
    ])),

    ValidationData == [] andalso erlang:throw(lists:flatten(io_lib:format("cound not found any validation by field name: ~ts", [Name]))),

    Validation = [[unicode:characters_to_list(ValueData), unicode:characters_to_list(KeyData)] || [ValueData, KeyData] <- ValidationData],

    Item = #{
        table => Table,
        name => binary_to_list(Name),
        sheet => lists:concat([Table, "-", binary_to_list(Name), "-", binary_to_list(Format)]),
        type => binary_to_atom(Format),
        alias => Alias,
        select => Select,
        join => Join,
        column => Index,
        validation => Validation
    },

    parse_validation_field(T, Table, Foreign, Key, Value, Index + 1, Map#{Index => Item});

parse_validation_field([#field{name = Name, format = Format = <<"set">>, type = <<"set", Type/binary>>} | T], Table, Foreign, Key, Value, Index, Map) ->
    Alias = lists:concat([
        Foreign, "_", Index
    ]),

    Select = lists:concat([
        "IFNULL(GROUP_CONCAT(`", Alias, "`.`", Value, "`), `", type:to_list(Name), "`) AS `", type:to_list(Name), "`"
    ]),

    Join = lists:concat([
        "LEFT JOIN `", Foreign, "` AS ", "`", Alias, "`", " ",
        "ON ", "(",  "`", Alias, "`", ".", "`type` = '", Table, ".", binary_to_list(Name), "'", " OR ", "`", Alias, "`", ".", "`type` = '", binary_to_list(Name), "'", " OR ", "`", Alias, "`", ".", "`type` = ''", ")", " ",
        "AND FIND_IN_SET(`", Alias, "`", ".", "`", Key, "`, `", Table, "`", ".", "`", type:to_list(Name), "`)"
    ]),

    Length = byte_size(Type) - 2,
    <<"(", OptionString:Length/binary, ")">> = Type,
    OptionList = [unicode:characters_to_list(Option) || Option <- binary:split(OptionString, <<",">>, [global])],

    ValidationData = db:select(lists:concat([
        "SELECT", " `", Value, "`", ",", "`", Key, "` ",
        "FROM", " `", Foreign, "` ",
        "WHERE",  " `", Key, "` ", "IN(", string:join(OptionList, ","), ")", " ",
        "AND ", "(", "`type` = '", Table, ".", binary_to_list(Name), "'", " OR ", "`type` = '", binary_to_list(Name), "'", " OR ", "`type` = ''", ")"
    ])),

    ValidationData == [] andalso erlang:throw(lists:flatten(io_lib:format("cound not found any validation by field name: ~ts", [Name]))),

    Validation = [[unicode:characters_to_list(ValueData), unicode:characters_to_list(KeyData)] || [ValueData, KeyData] <- ValidationData],

    Item = #{
        table => Table,
        name => binary_to_list(Name),
        sheet => lists:concat([Table, "-", binary_to_list(Name), "-", binary_to_list(Format)]),
        type => binary_to_atom(Format),
        alias => Alias,
        select => Select,
        join => Join,
        column => Index,
        validation => Validation
    },

    parse_validation_field(T, Table, Foreign, Key, Value, Index + 1, Map#{Index => Item});

parse_validation_field([#field{name = Name, format = Format} | T], Table, Foreign, Key, Value, Index, Map) ->

    Select = lists:concat([
        "`", Table, "`.`", type:to_list(Name), "`"
    ]),

    Item = #{
        table => Table,
        name => binary_to_list(Name),
        sheet => lists:concat([Table, "-", binary_to_list(Name), "-", binary_to_list(Format)]),
        type => binary_to_atom(Format),
        alias => "",
        select => Select,
        join => "",
        column => Index,
        validation => []
    },

    parse_validation_field(T, Table, Foreign, Key, Value, Index + 1, Map#{Index => Item}).


%%%===================================================================
%%% split table part
%%%===================================================================
split(TableData, Map) ->
    NewMap = find_max_row(TableData, 1, Map),
    NewMeta = revise_validation(maps:values(NewMap), 0, #{}),
    NewTableData = split_row(TableData, 1, NewMap, []),
    #{data => NewTableData, meta => NewMeta}.


%% find empty header column
find_max_row([], _, Map) ->
    Map;

find_max_row([RowData | T], Row, Map) ->
    NewMap = find_max_column(RowData, Row, 1, Map),
    find_max_row(T, Row + 1, NewMap).


find_max_column([], _, _, Map) ->
    Map;

find_max_column([ColumnData | T], Row, Column, Map) ->
    case maps:get(Column, Map) of
        _ when Row == 1 orelse Row == 2 orelse Row == 3 ->
            find_max_column(T, Row, Column + 1, Map);
        Item = #{type := varchar} ->
            Length = length(binary:matches(ColumnData, <<"},">>)) + 1,
            Max = maps:get(max, Item, Length),
            find_max_column(T, Row, Column + 1, Map#{Column => Item#{max => max(Max, Length)}});
        _ ->
            find_max_column(T, Row, Column + 1, Map)
    end.


revise_validation([], _, Map) ->
    Map;
revise_validation([Item = #{column := Column, max := Max} | T], Offset, Map) ->
    NewMap = lists:foldl(fun(Seq, Acc) -> Acc#{Seq => Item#{column => Seq}} end, Map, lists:seq(Column, Column + Max - 1)),
    revise_validation(T, Offset + Max - 1, NewMap);
revise_validation([Item = #{column := Column} | T], Offset, Map) ->
    ThisOffset = Column + Offset,
    revise_validation(T, Offset, Map#{ThisOffset => Item#{column => ThisOffset}}).


%% split row column
split_row([], _, _, List) ->
    lists:reverse(List);

split_row([RowData | T], Row, Map, List) ->
    NewRow = split_column(RowData, Row, 1, Map, []),
    split_row(T, Row + 1, Map, [NewRow | List]).


split_column([], _, _, _, List) ->
    lists:reverse(List);

split_column([ColumnData | T], Row, Column, Map, List) ->
    case maps:get(Column, Map) of
        #{type := varchar, max := Max} when Row == 1 orelse Row == 2 orelse Row == 3 ->
            Padding = lists:duplicate(Max - 1, []),
            split_column(T, Row, Column + 1, Map, lists:append(Padding, [ColumnData | List]));
        #{type := varchar, max := Max} ->
            Term = lists:reverse(parser:to_term(ColumnData)),
            Sub = split_cell_format(Term, []),
            Padding = lists:duplicate(Max - length(Sub), []),
            split_column(T, Row, Column + 1, Map, lists:append(lists:append(Padding, Sub), List));
        _ ->
            split_column(T, Row, Column + 1, Map, [ColumnData | List])
    end.


split_cell_format([], List) ->
    lists:reverse(List);

split_cell_format([H = #{} | T], List) ->
    Sub = string:join([lists:concat([Key, "=", Value]) || {Key, Value} <- maps:to_list(H)], ","),
    split_cell_format(T, [Sub | List]);

split_cell_format([H | T], List) when is_tuple(H) ->
    Sub = string:join([type:to_list(Element) || Element <- tuple_to_list(H)], ","),
    split_cell_format(T, [Sub | List]);

split_cell_format([H | T], List) ->
    split_cell_format(T, [H | List]).

%%%===================================================================
%%% File to Table
%%%===================================================================
%% @doc file to table collection
-spec to_collection(File :: file:filename(), Name :: file:filename()) -> ok.
to_collection(File, _) ->
    %% take book data
    {ok, Binary} = file:read_file(File),
    SheetData = excel_maker_xls:read(Binary),
    %% import all sheet
    lists:foreach(fun({Name, Sheet}) -> import_table(Name, join(Sheet)) end, maps:to_list(SheetData)).


%% @doc file to table
-spec to_table(File :: file:filename(), Name :: file:filename()) -> ok.
to_table(File, Name) ->
    %% take book data
    {ok, Binary} = file:read_file(File),
    SheetData = excel_maker_xls:read(Binary),
    not is_map_key(Name, SheetData) andalso erlang:throw(lists:flatten(io_lib:format("cound not found sheet by file name: ~ts", [Name]))),
    Sheet = maps:get(Name, SheetData, #{}),
    %% join column
    JoinSheet = join(Sheet),
    %% import
    import_table(Name, JoinSheet).


%%%===================================================================
%%% import part
%%%===================================================================

%% hidden table
import_table(_, #{state := "hidden"}) ->
    ok;

%% normal table
import_table(Name, #{data := [Head, _, _ | SheetData]}) ->
    %% Name must characters binary or characters list
    %% binary format with ~s will convert to characters list,  一  => [228, 184, 128]
    %% binary format with ~ts will convert to unicode list,    一  => [19968]
    %% connect database
    maker:connect_database(),
    [_, TableName] = string:tokens(Name, "-"),
    %% construct value format
    ColumnNames = [db:dot(Field) || Field <- Head],
    Names = unicode:characters_to_binary(lists:concat(["(", string:join(ColumnNames, ", "), ")"])),
    Format = unicode:characters_to_binary(lists:concat(["(", string:join(lists:duplicate(length(ColumnNames), "?"), ", "), ")"])),
    UpdateColumnNames = [lists:concat([Field, " = VALUES(", Field, ")"]) || Field <- ColumnNames],
    Updates = unicode:characters_to_binary(string:join(UpdateColumnNames, ", ")),
    db:save_into(<<"INSERT INTO ", "`", (list_to_binary(TableName))/binary, "` ", Names/binary, " VALUES ">>, Format, <<"ON DUPLICATE KEY UPDATE ", Updates/binary>>, SheetData, 0),
    ok.

%%%===================================================================
%%% join part
%%%===================================================================

join(Sheet = #{data := [_, Type | _] = TableData}) ->
    JoinColumn = find_join_column(Type, 1, 0, 0, []),
    NewData = join_row(TableData, 1, JoinColumn, []),
    Sheet#{data => NewData}.


%% find empty header column
find_join_column([], _, 0, _, List) ->
    lists:reverse(List);

find_join_column([], Index, Column, Acc, List) when Column > 0 ->
    find_join_column([], Index + 1, 0, Acc + (Index - Column - 1), [#{column => Column - Acc, length => Index - Column} | List]);

find_join_column(["varchar" | T], Index, Column, Acc, List) when Column > 0 ->
    find_join_column(T, Index + 1, Index, Acc + (Index - Column - 1), [#{column => Column - Acc, length => Index - Column} | List]);

find_join_column(["varchar" | T], Index, 0, Acc, List) ->
    find_join_column(T, Index + 1, Index, Acc, List);

find_join_column(["" | T], Index, Column, Acc, List) ->
    find_join_column(T, Index + 1, Column, Acc, List);

find_join_column([_ | T], Index, Column, Acc, List) when Column > 0 ->
    find_join_column(T, Index + 1, 0, Acc + (Index - Column - 1), [#{column => Column - Acc, length => Index - Column} | List]);

find_join_column([_ | T], Index, Column, Acc, List) ->
    find_join_column(T, Index + 1, Column, Acc, List).

%% shrink column by index and length
join_row([], _, _, List) ->
    lists:reverse(List);

join_row([RowData | T], Row, Meta, List) ->
    NewRow = join_column(Meta, Row, 1, RowData, []),
    join_row(T, Row + 1, Meta, [NewRow | List]).


%% shrink column by index and length
join_column([], _, _, [], List) ->
    lists:reverse(List);

join_column([#{column := Column, length := Length} | T], Row, Column, RowData, List) when Row == 1 orelse Row == 2 orelse Row == 3 ->
    {[Head | _], SubRowData} = lists:split(Length, RowData),
    join_column(T, Row, Column + 1, SubRowData, [Head | List]);

join_column([#{column := Column, length := Length} | T], Row, Column, RowData, List) when Row =/= 1 andalso Row =/= 2 orelse Row =/= 3 ->
    {SubColumnData, SubRowData} = lists:split(Length, RowData),
    Sub = join_column_convert(SubColumnData, false, []),
    join_column(T, Row, Column + 1, SubRowData, [Sub | List]);

join_column(Meta, Row, Column, [H | T], List) ->
    join_column(Meta, Row, Column + 1, T, [H | List]).


%% #{key => value}/{element, ...}
join_column_convert([], _, List) ->
    lists:flatten(lists:concat(["[", string:join(lists:reverse(List), ", "), "]"]));

join_column_convert([[] | T], Object, List) ->
    join_column_convert(T, Object, List);

join_column_convert([Column | T], Object, List) ->
    case string:str(Column, "=") of
        0 ->
            %% {element, ...}
            NewColumn = lists:concat(["{", Column, "}"]),
            join_column_convert(T, Object, [NewColumn | List]);
        _ ->
            %% #{key => value}
            NewColumn = lists:concat(["#{", string:replace(Column, "=", "=>"), "}"]),
            join_column_convert(T, true, [NewColumn | List])

    end.

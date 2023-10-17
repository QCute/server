%%%-------------------------------------------------------------------
%%% @doc
%%% make database data to excel and excel to database data
%%% @end
%%%-------------------------------------------------------------------
-module(excel_maker).
-export([to_sheet/2]).
-export([to_xml/2]).
-export([to_collection/2]).
-export([to_table/2]).
-include_lib("xmerl/include/xmerl.hrl").
-record(field, {name = [], default = [], type = [], format = [], comment = [], position = 0, key = [], extra = []}).
%%%===================================================================
%%% Table to XML
%%%===================================================================
%% @doc to sheet collection
to_sheet(Table, Path) ->
    %% connect database
    maker:connect_database(),

    %% take configure from data script
    {ErlFlag, ErlForm} = epp:parse_file(maker:relative_path("script/make/data/data_script.erl"), [], []),
    ErlFlag =/= ok andalso erlang:throw(lists:flatten(io_lib:format("cound not found data script: ~p", [ErlForm]))),
    {function, _, data, _, [{clause, _, [], [], ErlCons}]} = lists:keyfind(data, 3, ErlForm),
    ErlDataConfigureList = parser:evaluate(erl_prettypr:format(erl_syntax:form_list(ErlCons))),
    ErlConfigure = [#{comment => Comment, forms => [data_maker:parse_tag(data_maker:parse_sql(Sql)) || #{sql := Sql} <- Meta]} || #{file := File, comment := Comment, meta := Meta} <- ErlDataConfigureList, filename:basename(File) == Table orelse filename:basename(File, ".erl") == Table],

    %% take configure from js script
    {JsFlag, JsForm} = epp:parse_file(maker:relative_path("script/make/js/js_script.erl"), [], []),
    JsFlag =/= ok andalso erlang:throw(lists:flatten(io_lib:format("cound not found js script: ~p", [JsForm]))),
    {function, _, js, _, [{clause, _, [], [], JsCons}]} = lists:keyfind(js, 3, JsForm),
    JsDataConfigureList = parser:evaluate(erl_prettypr:format(erl_syntax:form_list(JsCons))),
    JsConfigure = [#{comment => Comment, forms => [js_maker:parse_tag(js_maker:parse_sql(Sql)) || #{sql := Sql} <- Meta]} || #{file := File, comment := Comment, meta := Meta} <- JsDataConfigureList, filename:basename(File) == Table orelse filename:basename(File, ".js") == Table],

    %% take configure from lua script
    {LuaFlag, LuaForm} = epp:parse_file(maker:relative_path("script/make/lua/lua_script.erl"), [], []),
    LuaFlag =/= ok andalso erlang:throw(lists:flatten(io_lib:format("cound not found lua script: ~p", [LuaForm]))),
    {function, _, lua, _, [{clause, _, [], [], LuaCons}]} = lists:keyfind(lua, 3, LuaForm),
    LuaDataConfigureList = parser:evaluate(erl_prettypr:format(erl_syntax:form_list(LuaCons))),
    LuaConfigure = [#{comment => Comment, forms => [lua_maker:parse_tag(lua_maker:parse_sql(Sql)) || #{sql := Sql} <- Meta]} || #{file := File, comment := Comment, meta := Meta} <- LuaDataConfigureList, filename:basename(File) == Table orelse filename:basename(File, ".lua") == Table],

    %% all configure list
    Configure = ErlConfigure ++ JsConfigure ++ LuaConfigure,
    #{comment := Comment, forms := Forms} = (length(Configure) =/= 1 andalso erlang:throw(lists:flatten(io_lib:format("cound not found file `~ts` in data/js/lua script", [Table])))) orelse hd(Configure),
    %% TableList = listing:unique([lists:concat(string:replace(string:trim(element(3, lists:keyfind('FROM', 1, Form))), "`", "", all)) || Form <- Configure]),
    %% take table from sql
    TableList = listing:unique([lists:concat(string:replace(string:trim(element(3, lists:keyfind('FROM', 1, Form))), "`", "", all)) || Form <- Forms]),
    TableList == [] andalso erlang:throw(lists:flatten(io_lib:format("cound not found table in data/js/lua script", []))),

    %% Because of system compatibility problems
    %% because of the utf8/gbk character set problem, use table name as file name
    %% load table list data
    Data = lists:append([element(2, parse_table(TableName)) || TableName <- TableList]),
    export_file(Path, Comment, Data).

%% @doc table to xml
to_xml(Table, Path) ->
    %% connect database
    maker:connect_database(),
    %% Because of system compatibility problems
    %% because of the utf8/gbk character set problem, use table name as file name
    {Name, Data} = parse_table(Table),
    export_file(Path, Name, Data).

export_file(Path, Name, Data) ->
    %% remove duplicate sheet, set the validation and reference behind the sheet
    UniqueSortData = lists:sort(fun({_, _, V}, _) -> is_list(V) end, listing:key_unique(1, Data)),
    %% specific path
    SpecificPath = lists:concat([filename:absname(Path), "/"]),
    %% !!! different os shell need different encode type
    %% !!! such windows nt with gbk need characters list/binary int
    %% !!! the unix shell with utf8 need characters list/binary
    %% characters list int
    FileName = unicode:characters_to_binary(lists:concat([SpecificPath, Name, ".xml"])),
    filelib:ensure_dir(SpecificPath),
    %% xml sheet head
    XmlData = append_sheet_style(FileName, filelib:is_file(FileName)),
    Book = unicode:characters_to_binary(xmerl:export_simple(make_book(XmlData, UniqueSortData), xmerl_xml)),
    %% Book = io_lib:format("<?xml version=\"1.0\" encoding=\"utf-8\"?><?mso-application progid=\"Excel.Sheet\"?>~ts", [make_book(XmlData, UniqueSortData)]),
    %% remove old
    file:delete(FileName),
    file:write_file(FileName, Book).

append_sheet_style(_, false) ->
    #xmlElement{};
append_sheet_style(File, true) ->
    {XmlData, Reason} = xmerl_scan:file(File),
    XmlData == error andalso erlang:throw(lists:flatten(io_lib:format("cound not open file: ~p", [Reason]))),
    XmlData.

parse_table(Table) ->
    %% load table data
    %% [{Type, [{Key, Value}, ...]}, ...]
    ValidationData = [{Type, [[Key, unicode:characters_to_list(Value), unicode:characters_to_list(Description)] || [Key, Value, Description] <- db:select(<<"SELECT `key`, `value`, `description` FROM `validation_data` WHERE `type` = '~s'">>, [Type])]} || [Type] <- db:select("SELECT DISTINCT `type` FROM `validation_data`")],
    ReferenceData = [{Key, [[Value, unicode:characters_to_list(Description)] || [Value, Description] <- db:select(<<"SELECT `value`, `description` FROM `reference_data` WHERE `key` = '~s'">>, [Key])]} || [Key] <- db:select("SELECT DISTINCT `key` FROM `reference_data`")],
    %% fetch table comment
    TableComment = lists:append(db:select(<<"SELECT `TABLE_COMMENT` FROM information_schema.`TABLES` WHERE `TABLE_SCHEMA` = DATABASE() AND `TABLE_NAME` = '~s';">>, [Table])),
    TableComment == [] andalso erlang:throw(lists:flatten(io_lib:format("no such table: ~s", [Table]))),
    Name = unicode:characters_to_list(hd(TableComment)),
    %% fetch table fields
    Fields = parser:convert(db:select(<<"SELECT `COLUMN_NAME`, `COLUMN_DEFAULT`, `COLUMN_TYPE`, `DATA_TYPE`, `COLUMN_COMMENT`, `ORDINAL_POSITION`, `COLUMN_KEY`, `EXTRA` FROM information_schema.`COLUMNS` WHERE `TABLE_SCHEMA` = DATABASE() AND `TABLE_NAME` = '~s' ORDER BY `ORDINAL_POSITION`;">>, [Table]), field),
    %% target table all data
    SourceData = db:select(<<"SELECT * FROM `~s`">>, [Table]),
    %% validation
    {Validation, ValidationSheetData} = load_validation(Fields, ValidationData, 1, [], []),
    %% reference
    ReferenceSheetData = load_reference(Fields, ReferenceData, []),
    %% add column comment and validation data
    ColumnComment = [unicode:characters_to_list(Comment) || #field{comment = Comment} <- Fields],
    NewSourceData = transform_data(Validation, ValidationSheetData, SourceData),
    SheetData = {Name, [ColumnComment | NewSourceData], Validation},
    %% all sheet data
    %% convert unicode binary list to binary
    {Name, [SheetData] ++ ValidationSheetData ++ ReferenceSheetData}.

%% load validation data
load_validation([], _, _, Validation, ValidationSheetData) ->
    {lists:reverse(Validation), lists:reverse(ValidationSheetData)};
load_validation([#field{name = Name, comment = Comment} | T], ValidationData, Index, Validation, ValidationSheetData) ->
    %% CommentName = re:replace(Comment, "validate\\(.*?\\)|\\(|\\)|\\[|\\]|\\{|\\}", "", [global, {return, binary}]),
    %% excel table name contain comma(,) could not validation column data problem
    %% Comment = [X || X <- CommentName, X =/= $, andalso X =/= $( andalso X =/= $) andalso X =/= $[ andalso X =/= $] andalso X =/= ${ andalso X =/= $}],
    %% convert unicode binary list to characters list
    %% ValidationSheetName = unicode:characters_to_list(re:replace(Comment, "\\w+\\(.*?\\)|\\(|\\)|\\[|\\]|\\{|\\}", "", [global, {return, binary}])),
    ValidationSheetName = unicode:characters_to_list(list_to_binary(lists:takewhile(fun(C) when C == $( orelse C == $) -> false; (_) -> true end, binary_to_list(Comment)))),
    %% @deprecated old mode
    %% capture (`table`.`key`,`table`.`value`)
    %% "(?<=validate\\()(`?\\w+`?)\\.`?\\w+`?\\s*,\\s*(`?\\w+`?)\\.`?\\w+`?(?=\\))"
    %% @recommend new mode
    %% read validation data from table validation_data
    case re:run(Comment, "(?<=validate\\().*?(?=\\))", [global, {capture, all, binary}]) of
        {match, [[Type]]} ->
            %% fetch table k,v data
            %% read from script instead of database
            Data = element(2, listing:key_find(Type, 1, ValidationData, {Type, []})),
            Data == [] andalso erlang:throw(lists:flatten(io_lib:format("could not found validation option: ~s in field: ~s", [Type, Name]))),
            %% NewSourceData = lists:map(fun(Row) -> {_, Value, _} = listing:key_find(lists:nth(Index, Row), 1, Data, {<<>>, [], []}), listing:set(Index, Row, Value) end, SourceData),
            %% column comment as sheet name
            %% {SheetName}!R{Row}C{Column}:R1048576C{Column}
            %% Validation
            %% |--- Range: C Index(C1/C2/...)
            %% |--- Value: Comment!C2(k/v's data v)
            AppendHeadData = [[[38190], [20540], [25551, 36848]] | Data],
            load_validation(T, ValidationData, Index + 1, [{Index, ValidationSheetName, 2} | Validation], [{ValidationSheetName, AppendHeadData, validation} | ValidationSheetData]);
        _ ->
            %% ensure zip function data list length equal column length
            load_validation(T, ValidationData, Index + 1, Validation, ValidationSheetData)
    end.

%% transform database data to excel data
transform_data([], [], SourceData) ->
    SourceData;
transform_data([{Index, _, _} | Validation], [{_, [_ | Data], validation} | ValidationSheetData], SourceData) ->
    %% transform column data
    NewSourceData = [begin {_, Value, _} = listing:key_find(type:to_binary(lists:nth(Index, Row)), 1, Data, {<<>>, [], []}), listing:set(Index, Row, Value) end || Row <- SourceData],
    transform_data(Validation, ValidationSheetData, NewSourceData).

load_reference([], _, ReferenceSheetData) ->
    lists:reverse(ReferenceSheetData);
load_reference([#field{name = Name, comment = Comment} | T], ReferenceData, ReferenceSheetData) ->
    %% ReferenceSheetName = unicode:characters_to_list(re:replace(Comment, "\\w+\\(.*?\\)|\\(|\\)|\\[|\\]|\\{|\\}", "", [global, {return, binary}])),
    ReferenceSheetName = unicode:characters_to_list(list_to_binary(lists:takewhile(fun(C) when C == $( orelse C == $) -> false; (_) -> true end, binary_to_list(Comment)))),
    case re:run(Comment, "(?<=reference\\(|ref\\().*?(?=\\))", [global, {capture, all, binary}]) of
        {match, [[Type]]} ->
            %% fetch table k,v data
            %% read from script instead of database
            Data = element(2, listing:key_find(Type, 1, ReferenceData, {Type, []})),
            Data == [] andalso erlang:throw(lists:flatten(io_lib:format("could not found reference option: ~s in field: ~s", [Type, Name]))),
            %% column comment as sheet name
            AppendHeadData = [[[25968, 20540], [25551, 36848], []] | Data],
            load_reference(T, ReferenceData, [{ReferenceSheetName ++ [21442, 32771], AppendHeadData, reference} | ReferenceSheetData]);
        _ ->
            %% ensure zip function data list length equal column length
            load_reference(T, ReferenceData, ReferenceSheetData)
    end.

%% make xml
make_book(XmlData, DataList) ->
    %% use MicroSoftYaHei as default style font
    %% Font = io_lib:format("<Style ss:ID=\"s01\"><Font ss:FontName=\"~ts\"/></Style>", [[24494, 36719, 38597, 40657]]),
    %% number format
    %% NumberFormat = io_lib:format("<Style ss:ID=\"s02\"><NumberFormat ss:Format=\"yyyy/mm/dd\\ hh:mm:ss\"/></Style>", []),
    %% Style = lists:concat(["<Styles>", Font, NumberFormat, "</Styles>"]),
    %% export styles and remove xml tag "<?xml version=\"1.0\"?>"
    %% Style = tl(xmerl:export_simple(xmerl_xpath:string("//Workbook//Styles", XmlData), xmerl_xml)),
    Styles = xmerl_xpath:string("//Workbook//Styles", XmlData),
    Sheet = make_sheet(XmlData, DataList, []),
    Namespace = #xmlNamespace{
        default = "urn:schemas-microsoft-com:office:spreadsheet",
        nodes = [
            {"o", "urn:schemas-microsoft-com:office:office"},
            {"x", "urn:schemas-microsoft-com:office:excel"},
            {"ss", "urn:schemas-microsoft-com:office:spreadsheet"},
            {"html", "http://www.w3.org/TR/REC-html40"},
            {"dt", "uuid:C2F41010-65B3-11d1-A29F-00AA00C14882"}
        ]
    },
    Attributes = [
        #xmlAttribute{name = 'xmlns', value = "urn:schemas-microsoft-com:office:spreadsheet"},
        #xmlAttribute{name = 'xmlns:o', value = "urn:schemas-microsoft-com:office:office"},
        #xmlAttribute{name = 'xmlns:x', value = "urn:schemas-microsoft-com:office:excel"},
        #xmlAttribute{name = 'xmlns:ss', value = "urn:schemas-microsoft-com:office:spreadsheet"},
        #xmlAttribute{name = 'xmlns:html', value = "http://www.w3.org/TR/REC-html40"},
        #xmlAttribute{name = 'xmlns:dt', value = "uuid:C2F41010-65B3-11d1-A29F-00AA00C14882"}
    ],
    %% io_lib:format("<Workbook xmlns=\"urn:schemas-microsoft-com:office:spreadsheet\" xmlns:ss=\"urn:schemas-microsoft-com:office:spreadsheet\">~ts~ts</Workbook>", [Style, Sheet]).
    [#xmlElement{name = 'Workbook', namespace = Namespace, attributes = Attributes, content = lists:merge(Styles, Sheet)}].

make_sheet(_, [], List) ->
    lists:reverse(List);
make_sheet(XmlData, [{Name, Data, validation} | T], List) ->
    %% lock validation
    %% Protect = "<TabColorIndex>10</TabColorIndex><ProtectObjects>True</ProtectObjects><ProtectScenarios>False</ProtectScenarios>",
    %% hide validation
    %% Hidden = "<Visible>SheetHidden</Visible>",
    OptionContent = [
        #xmlElement{name = 'TabColorIndex', content = [#xmlText{value = "10"}]},
        #xmlElement{name = 'ProtectObjects', content = [#xmlText{value = "True"}]},
        #xmlElement{name = 'ProtectScenarios', content = [#xmlText{value = "False"}]}
        #xmlElement{name = 'Visible', content = [#xmlText{value = "SheetHidden"}]}
    ],
    %% Option = ["<WorksheetOptions xmlns=\"urn:schemas-microsoft-com:office:excel\">", Protect, Hidden, "</WorksheetOptions>"],
    Namespace = #xmlNamespace{default = "urn:schemas-microsoft-com:office:excel"},
    OptionAttributes = [
        #xmlAttribute{namespace = 'xmlns', value = "urn:schemas-microsoft-com:office:excel"}
    ],
    Option = [#xmlElement{name = 'WorksheetOptions', namespace = Namespace, attributes = OptionAttributes, content = OptionContent}],
    %% take column style
    %% io_lib:format("<Worksheet ss:Name=\"~ts\" ss:Protected=\"1\">~ts~ts</Worksheet>", [Name, make_table(make_table_prepend(XmlData, Name), Data), Option]);
    Attributes = [
        #xmlAttribute{name = 'ss:Name', value = Name}, #xmlAttribute{name = 'ss:Protected', value = "1"}
    ],
    %% [Element || Element = #xmlElement{attributes = Attributes} <- xmerl_xpath:string("//Workbook//Worksheet", XmlData), (listing:key_find('ss:Name', #xmlAttribute.name, Attributes, #xmlAttribute{}))#xmlAttribute.value == Name].
    Prepend = find_sheet_table(xmerl_xpath:string("//Workbook//Worksheet", XmlData), Name),
    Table = make_table(Prepend, Data),
    Sheet = #xmlElement{name = 'Worksheet', attributes = Attributes, content = lists:merge(Table, Option)},
    make_sheet(XmlData, T, [Sheet | List]);
make_sheet(XmlData, [{Name, Data, reference} | T], List) ->
    %% lock reference
    %% Protect = "<TabColorIndex>10</TabColorIndex><ProtectObjects>True</ProtectObjects><ProtectScenarios>False</ProtectScenarios>",
    %% Option = ["<WorksheetOptions xmlns=\"urn:schemas-microsoft-com:office:excel\">", Protect, "</WorksheetOptions>"],
    Namespace = #xmlNamespace{default = "urn:schemas-microsoft-com:office:excel"},
    OptionAttributes = [
        #xmlAttribute{namespace = 'xmlns', value = "urn:schemas-microsoft-com:office:excel"}
    ],
    OptionContent = [
        #xmlElement{name = 'TabColorIndex', content = [#xmlText{value = "10"}]},
        #xmlElement{name = 'ProtectObjects', content = [#xmlText{value = "True"}]},
        #xmlElement{name = 'ProtectScenarios', content = [#xmlText{value = "False"}]}
    ],
    Option = [#xmlElement{name = 'WorksheetOptions', namespace = Namespace, attributes = OptionAttributes, content = OptionContent}],
    %% io_lib:format("<Worksheet ss:Name=\"~ts\" ss:Protected=\"1\">~ts~ts</Worksheet>", [Name, make_table(make_table_prepend(XmlData, Name), Data), Option]);
    Attributes = [
        #xmlAttribute{name = 'ss:Name', value = Name},
        #xmlAttribute{name = 'ss:Protected', value = "1"}
    ],
    %% [Element || Element = #xmlElement{attributes = Attributes} <- xmerl_xpath:string("//Workbook//Worksheet", XmlData), (listing:key_find('ss:Name', #xmlAttribute.name, Attributes, #xmlAttribute{}))#xmlAttribute.value == Name].
    Prepend = find_sheet_table(xmerl_xpath:string("//Workbook//Worksheet", XmlData), Name),
    Table = make_table(Prepend, Data),
    Sheet = #xmlElement{name = 'Worksheet', attributes = Attributes, content = lists:merge(Table, Option)},
    make_sheet(XmlData, T, [Sheet | List]);
make_sheet(XmlData, [{Name, TableData, ValidationData} | T], List) ->
    %% Attributes = [io_lib:format(" ~ts=\"~ts\"", [Name, Value]) || #xmlAttribute{name = Name, value = Value} <- Attributes],
    %% io_lib:format("<Worksheet~ts>~ts~ts</Worksheet>", [Attributes, Name, make_table(make_table_prepend(XmlData, Name), Data), ValidationContent]).
    Attributes = [
        #xmlAttribute{name = 'ss:Name', value = Name}
    ],
    %% [Element || Element = #xmlElement{attributes = Attributes} <- xmerl_xpath:string("//Workbook//Worksheet", XmlData), (listing:key_find('ss:Name', #xmlAttribute.name, Attributes, #xmlAttribute{}))#xmlAttribute.value == Name].
    Prepend = find_sheet_table(xmerl_xpath:string("//Workbook//Worksheet", XmlData), Name),
    Table = make_table(Prepend, TableData),
    Validation = [make_data_validation(Range, SheetName, Value) || {Range, SheetName, Value} <- ValidationData],
    Sheet = #xmlElement{name = 'Worksheet', attributes = Attributes, content = lists:merge(Table, Validation)},
    make_sheet(XmlData, T, [Sheet | List]).

find_sheet_table([], _) ->
    #xmlElement{};
find_sheet_table([#xmlElement{attributes = Attributes, content = Content} | T], Name) ->
    case lists:keyfind('ss:Name', #xmlAttribute.name, Attributes) of
        #xmlAttribute{value = Name} ->
            %% first sheet table
            listing:key_find('Table', #xmlElement.name, Content, #xmlElement{});
        _ ->
            find_sheet_table(T, Name)
    end.

make_table(Prepend = #xmlElement{attributes = Attributes}, Table) ->
    Column = xmerl_xpath:string("//Table//Column", Prepend),
    Row = make_row(Table, xmerl_xpath:string("//Table//Row", Prepend), []),
    %% Column = tl(xmerl:export_simple(xmerl_xpath:string("//Table//Column", XmlData), xmerl_xml)),
    %% Row = lists:concat([make_row(Row) || Row <- Table]),
    %% io_lib:format("<Table>~ts~ts</Table>", [Column, Row]).
    [#xmlElement{name = 'Table', attributes = Attributes, content = lists:merge(Column, Row)}].

make_row([], _, List) ->
    lists:reverse(List);
make_row([Row | T], [#xmlElement{attributes = Attributes, content = Content} | Prepend], List) ->
    Children = [Element || Element = #xmlElement{} <- Content],
    Cell = make_cell(Row, Children, []),
    %% PrependAttributes = [io_lib:format(" ~ts=\"~ts\"", [Name, Value]) || #xmlAttribute{name = Name, value = Value} <- Attributes],
    %% RowData = io_lib:format("<Row~ts>~ts</Row>", [PrependAttributes, Cell]),
    RowData = #xmlElement{name = 'Row', attributes = Attributes, content = Cell},
    make_row(T, Prepend, [RowData | List]);
make_row([Row | T], [], List) ->
    Cell = make_cell(Row, [], []),
    %% RowData = io_lib:format("<Row>~ts</Row>", [Cell]),
    RowData = #xmlElement{name = 'Row', content = Cell},
    make_row(T, [], [RowData | List]).

make_cell([], _, List) ->
    lists:reverse(List);
make_cell([Cell | T], [#xmlElement{attributes = Attributes} | Prepend], List) ->
    Data = make_data(Cell),
    %% PrependAttributes = [io_lib:format(" ~ts=\"~ts\"", [Name, Value]) || #xmlAttribute{name = Name, value = Value} <- Attributes],
    %% CellData = io_lib:format("<Cell~ts>~ts</Cell>", [PrependAttributes, Data]),
    CellData = #xmlElement{name = 'Cell', attributes = Attributes, content = Data},
    make_cell(T, Prepend, [CellData | List]);
make_cell([Cell | T], [], List) ->
    Data = make_data(Cell),
    %% CellData = io_lib:format("<Cell>~ts</Cell>", [Data]),
    CellData = #xmlElement{name = 'Cell', content = Data},
    make_cell(T, [], [CellData | List]).

make_data(Text) when is_number(Text) ->
    %% io_lib:format("<Data ss:Type=\"Number\">~w</Data>", [Text]);
    Attributes = [#xmlAttribute{name = 'ss:Type', value = "Number"}],
    Content = [#xmlText{value = lists:flatten(io_lib:format("~w", [Text]))}],
    [#xmlElement{name = 'Data', attributes = Attributes, content = Content}];
make_data(Text) when is_binary(Text) orelse is_atom(Text) orelse is_list(Text) ->
    %% io_lib:format("<Data ss:Type=\"String\">~ts</Data>", [Text]);
    Attributes = [#xmlAttribute{name = 'ss:Type', value = "String"}],
    Content = [#xmlText{value = lists:flatten(io_lib:format("~ts", [Text]))}],
    [#xmlElement{name = 'Data', attributes = Attributes, content = Content}].

%% data validation
make_data_validation(Range, SheetName, Value) ->
    %% io_lib:format("<DataValidation xmlns=\"urn:schemas-microsoft-com:office:excel\">~ts~ts~ts~ts</DataValidation>", [make_range(Range), make_type(), make_value(SheetName, Value), make_throw_style()]).
    Namespace = #xmlNamespace{default = "urn:schemas-microsoft-com:office:excel"},
    Attributes = [
        #xmlAttribute{namespace = 'xmlns', value = "urn:schemas-microsoft-com:office:excel"}
    ],
    Content = [make_range(Range), make_type(), make_value(SheetName, Value), make_throw_style()],
    #xmlElement{name = 'DataValidation', namespace = Namespace, attributes = Attributes, content = Content}.

make_range(Range) ->
    %% except first row
    %% io_lib:format("<Range>R2C~w:R1048576C~w</Range>", [Range,  Range]).
    Content = [#xmlText{value = io_lib:format("R2C~w:R1048576C~w", [Range, Range])}],
    #xmlElement{name = 'Range', content = Content}.

make_type() ->
    %% "<Type>List</Type>".
    Content = [#xmlText{value = "List"}],
    #xmlElement{name = 'Type', content = Content}.

make_value(SheetName, Value) ->
    %% io_lib:format("<Value>~ts!R2C~w:R1048576C~w</Value>", [SheetName, Value, Value]).
    Content = [#xmlText{value = io_lib:format("~ts!R2C~w:R1048576C~w", [SheetName, Value, Value])}],
    #xmlElement{name = 'Value', content = Content}.

make_throw_style() ->
    %% "<throwStyle>Stop</throwStyle>".
    Content = [#xmlText{value = "Stop"}],
    #xmlElement{name = 'throwStyle', content = Content}.

%%%===================================================================
%%% XML to Table
%%%===================================================================
%% @doc to table collection
to_collection(File, _) ->
    %% restore data
    {XmlData, Reason} = xmerl_scan:file(File),
    XmlData == error andalso erlang:throw(lists:flatten(io_lib:format("cound not open file: ~p", [Reason]))),
    SheetList = xmerl_xpath:string("/Workbook/Worksheet", XmlData),
    BookSheetData = [take_sheet(Sheet) || Sheet <- SheetList],
    RestoreBookSheetData = restore_sheet(BookSheetData, BookSheetData, []),
    lists:foreach(fun({Name, _, _, SheetData, _}) -> import_table(Name, SheetData) end, RestoreBookSheetData).

%% @doc xml to table
to_table(File, Name) ->
    %% restore data
    {XmlData, Reason} = xmerl_scan:file(File),
    XmlData == error andalso erlang:throw(lists:flatten(io_lib:format("cound not open file: ~p", [Reason]))),
    SheetList = xmerl_xpath:string("/Workbook/Worksheet", XmlData),
    BookSheetData = [take_sheet(Sheet) || Sheet <- SheetList],
    RestoreBookSheetData = restore_sheet(BookSheetData, BookSheetData, []),
    {_, _, _, SheetData, _} = listing:key_find(Name, 1, RestoreBookSheetData, {Name, [], [], [], []}),
    %% check sheet exists
    SheetData == [] andalso erlang:throw(lists:flatten(io_lib:format("cound not found sheet by file name: ~ts", [Name]))),
    import_table(Name, SheetData).

import_table(Name, SheetData) ->
    %% Name must characters binary or characters list
    %% binary format with ~s will convert to characters list,  一  => [228, 184, 128]
    %% binary format with ~ts will convert to unicode list,    一  => [19968]
    %% connect database
    maker:connect_database(),
    case db:select_column(<<"SELECT `TABLE_NAME` FROM information_schema.`TABLES` WHERE `TABLE_SCHEMA` = DATABASE() AND `TABLE_COMMENT` = '~s';">>, [Name]) of
        [Table] ->
            %% insert before truncate
            db:query(parser:format(<<"TRUNCATE `~s`">>, [Table])),
            %% construct value format
            Format = list_to_binary(lists:concat(["(", string:join(lists:duplicate(length(hd(SheetData)), "'~s'"), ","), ")"])),
            db:insert(parser:collect(tl(SheetData), {<<"INSERT INTO `", Table/binary, "` VALUES ">>, Format})),
            ok;
        [] ->
            erlang:throw(lists:flatten(io_lib:format("could not found table by comment: ~ts", [Name])));
        More ->
            erlang:throw(lists:flatten(io_lib:format("found multiple table: ~p", [More])))
    end.

restore_sheet([], _, List) ->
    List;
restore_sheet([{_SheetName, [_ | _], _Hidden, _Table, _Validation} | T], BookSheetData, List) ->
    %% drop reference sheet (protected)
    restore_sheet(T, BookSheetData, List);
restore_sheet([{_SheetName, _, [_ | _], _Table, _Validation} | T], BookSheetData, List) ->
    %% drop validation sheet (protected) or (hidden)
    restore_sheet(T, BookSheetData, List);
restore_sheet([{SheetName, Protected, Hidden, Table, Validation} | T], BookSheetData, List) ->
    %% normal sheet data
    NewTable = restore_table(Validation, BookSheetData, Table),
    restore_sheet(T, BookSheetData, [{SheetName, Protected, Hidden, NewTable, Validation} | List]).

restore_table([], _, Table) ->
    Table;
restore_table([{Range, SheetName, Column} | T], BookSheetData, Table) ->
    {_, _, _, Data, _} = listing:key_find(SheetName, 1, BookSheetData, {SheetName, [], []}),
    %% check validation data sheet exists
    Data == [] andalso erlang:throw(lists:flatten(io_lib:format("count not found validation data by sheet name: ~ts", [SheetName]))),
    RestoreData = [list_to_tuple(Row) || Row <- Data],
    %% replace slot with validation data
    NewTable = [listing:set(Range, Row, element(1, listing:key_find(lists:nth(Range, Row), Column, RestoreData, {"", ""}))) || Row <- Table],
    restore_table(T, BookSheetData, NewTable).

take_sheet(Sheet = #xmlElement{attributes = Attributes}) ->
    %% single table in sheet
    #xmlAttribute{value = Name} = listing:key_find('ss:Name', #xmlAttribute.name, Attributes, #xmlAttribute{}),
    %% ss:Protected="1"
    #xmlAttribute{value = Protected} = listing:key_find('ss:Protected', #xmlAttribute.name, Attributes, #xmlAttribute{}),
    %% SheetHidden
    Hidden = lists:append([Value || #xmlElement{content = Content} <- xmerl_xpath:string("//Visible", Sheet), #xmlText{value = Value} <- Content]),
    %% check sheet name exists
    Name == undefined andalso erlang:throw("cound not take sheet name"),
    Table = hd(xmerl_xpath:string("//Table", Sheet)),
    DataValidation = [take_validation(Validation) || Validation <- xmerl_xpath:string("//DataValidation", Sheet)],
    {Name, Protected, Hidden, take_table(Table), DataValidation}.

take_table(Table) ->
    take_row(xmerl_xpath:string("//Row", Table)).

take_row([Head | Tail]) ->
    First = take_row(xmerl_xpath:string("//Cell", Head), 1, array:new([{default, []}])),
    Array = array:from_list(lists:duplicate(length(First), []), []),
    [First | [take_row(xmerl_xpath:string("//Cell", Row), 1, Array) || Row <- Tail]].

take_row([], _, Array) ->
    array:to_list(Array);
take_row([Cell = #xmlElement{attributes = Attributes} | T], Index, Array) ->
    %% take index offset
    #xmlAttribute{value = Value} = listing:key_find('ss:Index', #xmlAttribute.name, Attributes, #xmlAttribute{value = integer_to_list(Index)}),
    %% single data in cell
    Data = hd([take_text(Data) || Data <- xmerl_xpath:string("//Data", Cell)]),
    NewIndex = list_to_integer(Value),
    NewArray = array:set(NewIndex - 1, Data, Array),
    take_row(T, NewIndex + 1, NewArray).

take_validation(Validation) ->
    %% single range in data validation
    ValidationRange = hd([take_text(Data) || Data <- xmerl_xpath:string("//Range", Validation)]),
    %% single value in data validation
    ValidationValue = hd([take_text(Data) || Data <- xmerl_xpath:string("//Value", Validation)]),
    Range = take_validation_column(ValidationRange),
    %% data column, sheet name, sheet column
    SheetName = hd(string:tokens(ValidationValue, "!")),
    Column = take_validation_column(ValidationValue),
    {Range, SheetName, Column}.

take_validation_column(RowColumn) ->
    {match, [Column]} = re:run(unicode:characters_to_binary(RowColumn), "(?<=C)\\d+", [{capture, first, binary}]),
    binary_to_integer(Column).

take_text(#xmlElement{content = []}) ->
    [""];
take_text(#xmlElement{content = Content}) ->
    %% single text in data
    hd([Value || #xmlText{value = Value} <- Content]).

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
-record(field, {table = <<>>, name = <<>>, default = <<>>, type = <<>>, format = <<>>, comment = <<>>, key = <<>>, extra = <<>>, position = 0}).
%%%===================================================================
%%% Table to XML
%%%===================================================================
%% @doc to sheet collection
-spec to_sheet(Table :: string(), Path :: file:filename()) -> ok.
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
-spec to_xml(Table :: string(), Path :: file:filename()) -> ok.
to_xml(Table, Path) ->
    %% connect database
    maker:connect_database(),
    %% Because of system compatibility problems
    %% because of the utf8/gbk character set problem, use table name as file name
    {Name, Data} = parse_table(Table),
    export_file(Path, Name, Data).

export_file(Path, Name, Data) ->
    %% remove duplicate sheet, set the validation and reference behind the sheet
    %% UniqueSortData = lists:sort(fun({_, _, V}, _) -> is_list(V) end, listing:key_unique(1, Data)),
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
    Book = unicode:characters_to_binary(xmerl:export_simple(make_book(XmlData, Data), xmerl_xml)),
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


%%%===================================================================
%%% parse table part
%%%===================================================================

parse_table(Table) ->
    %% fetch table comment
    TableComment = lists:append(db:select(lists:concat(["SELECT `TABLE_COMMENT` FROM information_schema.`TABLES` WHERE `TABLE_SCHEMA` = DATABASE() AND `TABLE_NAME` = '", Table, "';"]))),
    TableComment == [] andalso erlang:throw(lists:flatten(io_lib:format("no such table: ~s", [Table]))),
    SheetName = unicode:characters_to_list(hd(TableComment)),

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
    MetaList = parse_validation_field(Fields, Table, "validation_data", key, value, 1, []),

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
    ColumnComment = [unicode:characters_to_list(Comment) || #field{comment = Comment} <- Fields],
    SheetData = #{name => SheetName, data => [ColumnComment | SourceData], meta => MetaList},
    %% all sheet data
    %% convert unicode binary list to binary
    {SheetName, [SheetData]}.

%% load validation data
parse_validation_field([], _, _, _, _, _, List) ->
    lists:reverse(List);

parse_validation_field([#field{name = Name, format = <<"enum">>, type = <<"enum", Type/binary>>} | T], Table, Foreign, Key, Value, Index, List) ->
    Alias = lists:concat([
        Foreign, "_", Index
    ]),

    Select = lists:concat([
        "IFNULL(GROUP_CONCAT(`", Alias, "`.`", Value, "`), `", type:to_list(Name), "`) AS `", type:to_list(Name), "`"
    ]),

    Join = lists:concat([
        "LEFT JOIN `", Foreign, "` AS ", "`", Alias, "`", " ON `", Alias, "`.`", Key, "` = `", Table, "`.`", type:to_list(Name), "`"
    ]),

    
    <<"(", OptionString:(byte_size(Type))/binary, ")">> = Type,
    OptionList = [unicode:characters_to_list(Option) || Option <- binary:split(OptionString, <<",">>, [global])],

    ValidationData = db:select(lists:concat([
        "SELECT", " `", Key, "` ", " `", Value, "` ",
        "FROM", " `", Foreign, "` ",
        "WHERE",  " `", Key, "` ", "IN(", string:join(OptionList, ","), ")"
    ])),

    Validation = [{unicode:characters_to_list(KeyData), unicode:characters_to_list(ValueData)} || [KeyData, ValueData] <- ValidationData],

    Item = #{type => enum, alias => Alias, select => Select, join => Join, column => Index, validation => Validation},

    parse_validation_field(T, Table, Foreign, Key, Value, Index + 1, [Item | List]);

parse_validation_field([#field{name = Name, format = <<"set">>, type = <<"set", Type/binary>>} | T], Table, Foreign, Key, Value, Index, List) ->
    Alias = lists:concat([
        Foreign, "_", Index
    ]),

    Select = lists:concat([
        "IFNULL(GROUP_CONCAT(`", Alias, "`.`", Value, "`), `", type:to_list(Name), "`) AS `", type:to_list(Name), "`"
    ]),

    Join = lists:concat([
        "LEFT JOIN `", Foreign, "` AS ", "`", Alias, "`", " ON FIND_IN_SET(`", Alias, "`.`", Key, "`, `", Table, "`.`", type:to_list(Name), "`)"
    ]),

    <<"(", OptionString:(byte_size(Type))/binary, ")">> = Type,
    OptionList = [unicode:characters_to_list(Option) || Option <- binary:split(OptionString, <<",">>, [global])],

    ValidationData = db:select(lists:concat([
        "SELECT", " `", Value, "` ", " `", Key, "` ",
        "FROM", " `", Foreign, "` ",
        "WHERE",  " `", Key, "` ", "IN(", string:join(OptionList, ","), ")"
    ])),

    Validation = [{unicode:characters_to_list(ValueData), unicode:characters_to_list(KeyData)} || [ValueData, KeyData] <- ValidationData],

    Item = #{type => set, alias => Alias, select => Select, join => Join, column => Index, validation => Validation},

    parse_validation_field(T, Table, Foreign, Key, Value, Index + 1, [Item | List]);

parse_validation_field([#field{name = Name} | T], Table, Foreign, Key, Value, Index, List) ->

    Select = lists:concat([
        "`", Table, "`.`", type:to_list(Name), "`"
    ]),

    Item = #{alias => "", select => Select, join => "", column => Index, validation => []},

    parse_validation_field(T, Table, Foreign, Key, Value, Index + 1, [Item | List]).


%%%===================================================================
%%% book part
%%%===================================================================

%% make xml
make_book(XmlData, DataList) ->
    Style = make_style(XmlData),
    CustomDocumentProperties = make_custom_property(XmlData, DataList, []),
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

    Content = lists:flatten([
        Style,
        CustomDocumentProperties,
        Sheet
    ]),

    [#xmlElement{name = 'Workbook', namespace = Namespace, attributes = Attributes, content = Content}].


%%%===================================================================
%%% style part
%%%===================================================================
make_style(XmlData) ->
    %% use MicroSoftYaHei as default style font
    %% Font = io_lib:format("<Style ss:ID=\"s01\"><Font ss:FontName=\"~ts\"/></Style>", [[24494, 36719, 38597, 40657]]),
    %% number format
    %% NumberFormat = io_lib:format("<Style ss:ID=\"s02\"><NumberFormat ss:Format=\"yyyy/mm/dd\\ hh:mm:ss\"/></Style>", []),
    %% Style = lists:concat(["<Styles>", Font, NumberFormat, "</Styles>"]),
    %% export styles and remove xml tag "<?xml version=\"1.0\"?>"
    %% Style = tl(xmerl:export_simple(xmerl_xpath:string("//Workbook//Styles", XmlData), xmerl_xml)),
    Styles = xmerl_xpath:string("//Workbook//Styles", XmlData),
    %% todo replace default styles with prepend

    DefaultStyle = #xmlElement{
        name = 'Style',
        content = [
            #xmlElement{
                name = 'Alignment',
                attributes = [
                    #xmlAttribute{name = 'ss:Vertical', value = "Center"}
                ]
            },
            #xmlElement{
                name = 'Borders'
            },
            #xmlElement{
                name = 'Interior'
            },
            #xmlElement{
                name = 'NumberFormat'
            },
            #xmlElement{
                name = 'Font',
                attributes = [
                    #xmlAttribute{name = 'ss:FontName', value = "Microsoft YaHei"},
                    #xmlAttribute{name = 'ss:CharSet', value = "134"},
                    #xmlAttribute{name = 'ss:Size', value = "13"},
                    #xmlAttribute{name = 'ss:Color', value = "#000000"}
                ]
            }
        ]
    },

    DefaultStyles = [
        #xmlElement{
            name = 'Styles',
            attributes = [
                #xmlAttribute{name = 'ss:ID', value = "Default"},
                #xmlAttribute{name = 'ss:Name', value = "Normal"}
            ],
            content = [
                DefaultStyle
            ]
        }
    ],

    lists:merge(DefaultStyles, Styles).

%%%===================================================================
%%% custom option part
%%%===================================================================

%% <CustomDocumentProperties></CustomDocumentProperties>
make_custom_property(_, [], List) ->
    [#xmlElement{name = 'CustomDocumentProperties', content = lists:reverse(List)}];

make_custom_property(XmlData, [#{name := Name, validation := ValidationData} | T], List) ->
    CustomData = make_origin_validation(XmlData, Name, ValidationData, List),
    make_custom_property(XmlData, T, CustomData).


%% <SheetNameC6></SheetNameC6>
make_origin_validation(_, _, [], List) ->
    lists:reverse(List);

make_origin_validation(XmlData, SheetName, [#{type := enum, column := Column, validation := ValidationData} | T], List) ->
    Name = type:to_atom(lists:concat([SheetName, "-", Column])),
    Value = lists:flatten(io_lib:format("~tw", [maps:from_list(ValidationData)])),
    Property = #xmlElement{
        name = Name,
        attributes = [
            #xmlAttribute{name = 'dt:dt', value = "String"}
        ],
        content = [
            #xmlText{value = Value}
        ]
    },
    make_origin_validation(XmlData, SheetName, T, [Property | List]);

make_origin_validation(XmlData, SheetName, [#{type := set, column := Column, validation := ValidationData} | T], List) ->
    Name = type:to_atom(lists:concat([SheetName, "-", Column])),
    Value = lists:flatten(io_lib:format("~tw", [maps:from_list(ValidationData)])),
    Property = #xmlElement{
        name = Name,
        attributes = [
            #xmlAttribute{name = 'dt:dt', value = "String"}
        ],
        content = [
            #xmlText{value = Value}
        ]
    },
    make_origin_validation(XmlData, SheetName, T, [Property | List]).

%%%===================================================================
%%% sheet part
%%%===================================================================

%% <Worksheet></Worksheet>
make_sheet(_, [], List) ->
    lists:reverse(List);

make_sheet(XmlData, [#{name := Name, data := TableData, meta := MetaList} | T], List) ->
    %% Attributes = [io_lib:format(" ~ts=\"~ts\"", [Name, Value]) || #xmlAttribute{name = Name, value = Value} <- Attributes],
    Attributes = [
        #xmlAttribute{name = 'ss:Name', value = Name}
    ],

    Prepend = find_sheet_table(xmerl_xpath:string("//Workbook//Worksheet", XmlData), Name),
    Table = make_table(Prepend, TableData),
    Property = make_sheet_property(MetaList, []),

    Sheet = #xmlElement{name = 'Worksheet', attributes = Attributes, content = lists:merge(Table, Property)},
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


%% worksheet property
make_sheet_property([], List) ->
    lists:reverse(List);
%% <DataValidation></DataValidation>
make_sheet_property([#{type := enum, validation := ValidationData} | T], List) ->
    Validation = [make_data_validation(Validation) || Validation <- ValidationData],
    make_sheet_property(T, lists:merge(Validation, List));
%% other column
make_sheet_property([_ | T], List) ->
    make_sheet_property(T, List).

%%%===================================================================
%%% table part
%%%===================================================================

%% <Table></Table>
make_table(Prepend = #xmlElement{attributes = Attributes}, Table) ->
    Column = xmerl_xpath:string("//Table//Column", Prepend),
    Row = make_row(Table, xmerl_xpath:string("//Table//Row", Prepend), []),
    %% Column = tl(xmerl:export_simple(xmerl_xpath:string("//Table//Column", XmlData), xmerl_xml)),
    %% Row = lists:concat([make_row(Row) || Row <- Table]),
    %% ss:ExpandedColumnCount="3" ss:ExpandedRowCount="2" x:FullColumns="1" x:FullRows="1" ss:DefaultColumnWidth="66" ss:DefaultRowHeight="18.75"
    %% todo replace default with prepend
    DefaultAttributes = [
        #xmlAttribute{name = 'ss:ExpandedColumnCount', value = "1"},
        #xmlAttribute{name = 'ss:ExpandedRowCount', value = "1"},
        #xmlAttribute{name = 'ss:FullColumns', value = "1"},
        #xmlAttribute{name = 'ss:FullRows', value = "1"},
        #xmlAttribute{name = 'ss:DefaultColumnWidth', value = "92"},
        #xmlAttribute{name = 'ss:DefaultRowHeight', value = "22"}
    ],
    [#xmlElement{name = 'Table', attributes = lists:merge(DefaultAttributes, Attributes), content = lists:merge(Column, Row)}].


%% <Row></Row>
make_row([], _, List) ->
    lists:reverse(List);
%% use prepend
make_row([Row | T], [#xmlElement{attributes = Attributes, content = Content} | Prepend], List) ->
    Children = [Element || Element = #xmlElement{} <- Content],
    Cell = make_cell(Row, Children, []),
    %% PrependAttributes = [io_lib:format(" ~ts=\"~ts\"", [Name, Value]) || #xmlAttribute{name = Name, value = Value} <- Attributes],
    %% RowData = io_lib:format("<Row~ts>~ts</Row>", [PrependAttributes, Cell]),
    RowData = #xmlElement{name = 'Row', attributes = Attributes, content = Cell},
    make_row(T, Prepend, [RowData | List]);
%% no prepend
make_row([Row | T], [], List) ->
    Cell = make_cell(Row, [], []),
    %% RowData = io_lib:format("<Row>~ts</Row>", [Cell]),
    RowData = #xmlElement{name = 'Row', content = Cell},
    make_row(T, [], [RowData | List]).


%% <Cell></Cell>
make_cell([], _, List) ->
    lists:reverse(List);
%% use prepend
make_cell([Cell | T], [#xmlElement{attributes = Attributes} | Prepend], List) ->
    Data = make_data(Cell),
    %% PrependAttributes = [io_lib:format(" ~ts=\"~ts\"", [Name, Value]) || #xmlAttribute{name = Name, value = Value} <- Attributes],
    %% CellData = io_lib:format("<Cell~ts>~ts</Cell>", [PrependAttributes, Data]),
    CellData = #xmlElement{name = 'Cell', attributes = Attributes, content = Data},
    make_cell(T, Prepend, [CellData | List]);
%% no prepend
make_cell([Cell | T], [], List) ->
    Data = make_data(Cell),
    %% CellData = io_lib:format("<Cell>~ts</Cell>", [Data]),
    CellData = #xmlElement{name = 'Cell', content = Data},
    make_cell(T, [], [CellData | List]).


%% <Data ss:Type="Number"></Data>
make_data(Text) when is_number(Text) ->
    Attributes = [#xmlAttribute{name = 'ss:Type', value = "Number"}],
    Content = [#xmlText{value = lists:flatten(io_lib:format("~w", [Text]))}],
    [#xmlElement{name = 'Data', attributes = Attributes, content = Content}];
%% <Data ss:Type="String"></Data>
make_data(Text) when is_binary(Text) orelse is_atom(Text) orelse is_list(Text) ->
    Attributes = [#xmlAttribute{name = 'ss:Type', value = "String"}],
    Content = [#xmlText{value = lists:flatten(io_lib:format("~ts", [Text]))}],
    [#xmlElement{name = 'Data', attributes = Attributes, content = Content}].


%%%===================================================================
%%% validation part
%%%===================================================================

%% <DataValidation></DataValidation>
make_data_validation(Validation = #{column := Column}) ->
    Namespace = #xmlNamespace{default = "urn:schemas-microsoft-com:office:excel"},
    Attributes = [
        #xmlAttribute{name = 'xmlns', value = "urn:schemas-microsoft-com:office:excel"}
    ],
    Content = [
        make_range(Column),
        make_type(),
        make_value(Validation),
        make_throw_style()
    ],
    #xmlElement{name = 'DataValidation', namespace = Namespace, attributes = Attributes, content = Content}.


%% <Range></Range>
make_range(Range) ->
    %% except first row
    Content = [#xmlText{value = io_lib:format("R2C~w:R1048576C~w", [Range, Range])}],
    #xmlElement{name = 'Range', content = Content}.


%% <Type>List</Type>
make_type() ->
    Content = [#xmlText{value = "List"}],
    #xmlElement{name = 'Type', content = Content}.


%% <Value></Value>
make_value(#{type := value, validation := Value}) ->
    Content = [#xmlText{value = io_lib:format("\"~ts\"", [Value])}],
    #xmlElement{name = 'Value', content = Content};
%% <Value></Value>
make_value(#{type := sheet, name := SheetName}) ->
    Content = [#xmlText{value = io_lib:format("~ts!R2C~w:R1048576C~w", [SheetName, 2, 2])}],
    #xmlElement{name = 'Value', content = Content}.


%% <throwStyle>Stop</throwStyle>
make_throw_style() ->
    Content = [#xmlText{value = "Stop"}],
    #xmlElement{name = 'throwStyle', content = Content}.


%%%===================================================================
%%% XML to Table
%%%===================================================================

%% @doc to table collection
-spec to_collection(File :: file:filename(), Name :: file:filename()) -> ok.
to_collection(File, _) ->
    %% parse file data
    {XmlData, Reason} = xmerl_scan:file(File),
    XmlData == error andalso erlang:throw(lists:flatten(io_lib:format("cound not open file: ~p", [Reason]))),

    %% take book data
    Book = #{sheet := SheetList} = take_book(XmlData),

    %% import all sheet
    lists:foreach(fun({Name, Sheet}) -> import_table(Name, join(restore(Book, Name, Sheet))) end, SheetList).


%% @doc xml to table
-spec to_table(File :: file:filename(), Name :: file:filename()) -> ok.
to_table(File, Name) ->
    %% parse file data
    {XmlData, Reason} = xmerl_scan:file(File),
    XmlData == error andalso erlang:throw(lists:flatten(io_lib:format("cound not open file: ~p", [Reason]))),

    %% take book data
    Book = #{sheet := SheetList} = take_book(XmlData),
    not is_map_key(Name, SheetList) andalso erlang:throw(lists:flatten(io_lib:format("cound not found sheet by file name: ~ts", [Name]))),
    Sheet = maps:get(Name, Book, #{}),

    %% restore validation data
    RestoreSheet = restore(Book, Name, Sheet),

    %% join column
    JoinSheet = join(RestoreSheet),

    %% import
    import_table(Name, JoinSheet).


%%%===================================================================
%%% import part
%%%===================================================================

%% protected table
import_table(_, #{property := #{protected := [_ | _]}}) ->
    ok;

%% hidden table
import_table(_, #{property := #{hidden := [_ | _]}}) ->
    ok;

%% normal table
import_table(Name, #{data := SheetData}) ->
    %% Name must characters binary or characters list
    %% binary format with ~s will convert to characters list,  一  => [228, 184, 128]
    %% binary format with ~ts will convert to unicode list,    一  => [19968]
    %% connect database
    maker:connect_database(),
    case db:select_column(lists:concat(["SELECT `TABLE_NAME` FROM information_schema.`TABLES` WHERE `TABLE_SCHEMA` = DATABASE() AND `TABLE_COMMENT` = '", Name, "';"])) of
        [Table] ->
            %% insert before truncate
            db:query(db:format(<<"TRUNCATE `~s`">>, [Table])),
            %% construct value format
            Format = list_to_binary(lists:concat(["(", string:join(lists:duplicate(length(hd(SheetData)), "'~s'"), ","), ")"])),
            db:save(<<"INSERT INTO `", Table/binary, "` VALUES ">>, Format, <<>>, tl(SheetData), fun(A) -> A end, 0),
            ok;
        [] ->
            erlang:throw(lists:flatten(io_lib:format("could not found table by comment: ~ts", [Name])));
        More ->
            erlang:throw(lists:flatten(io_lib:format("found multiple table: ~p", [More])))
    end.

%%%===================================================================
%%% restore part
%%%===================================================================

restore(Book, SheetName, Sheet = #{data := [_ | RowData], property := #{validation := ValidationData}}) ->
    NewTableData = restore_row_validation(RowData, Book, SheetName, ValidationData, 1, []),
    Sheet#{data => NewTableData}.


restore_row_validation([], _, _, _, _, List) ->
    lists:reverse(List);
restore_row_validation([Row | RowData], Book, SheetName, ValidationData, Index, List) ->
    NewRow = restore_cell_validation(Row, Book, SheetName, ValidationData, 1, []),
    restore_row_validation(RowData, SheetName, Book, ValidationData, Index + 1, [NewRow | List]).


restore_cell_validation([], _, _, _, _, List) ->
    lists:reverse(List);
restore_cell_validation([Cell | CellData], Book, SheetName, ValidationData, Index, List) ->
    NewCell = restore_validation(ValidationData, Book, SheetName, Index, Cell),
    restore_cell_validation(CellData, Book, SheetName, ValidationData, Index + 1, [NewCell | List]).


restore_validation([], _, _, _, Cell) ->
    Cell;
%% <SheetName6></SheetName6>
restore_validation([#{type := list, column := Column} | T], Book = #{property := CustomProperty}, SheetName, Column, Cell) ->
    NewCell = string:join([maps:get(Option, lists:flatte(lists:concat([SheetName, "-", Column])), CustomProperty) || Option <- string:tokens(Cell, ",")], ","),
    restore_validation(T, Book, SheetName, Column, NewCell);

%% SheetName!R2C6:R1048576C6
restore_validation([#{type := sheet, column := Column, name := Name} | T], Book, SheetName, Column, Cell) ->
    #{data := Data} = maps:get(Name, Book),
    NewCell = find_loop(Data, Cell),
    restore_validation(T, Book, SheetName, Column, NewCell).


find_loop([], Value) ->
    Value;
find_loop([[Key, Value] | _], Value) ->
    Key;
find_loop([_ | T], Value) ->
    find_loop(T, Value).


%%%===================================================================
%%% join part
%%%===================================================================

join(Sheet = #{data := [Head | RowData]}) ->
    JoinColumn = find_join_column(Head, 1, 0, []),
    NewData = [join_column(JoinColumn, 1, Row, []) || Row <- RowData],
    Sheet#{data => NewData}.


%% find empty header column
find_join_column([], _, _, List) ->
    lists:reverse(List);

find_join_column([], Index, Column, List) when Column > 0 ->
    find_join_column([], Index + 1, 0, [#{column => Column, length => Index - Column} | List]);

find_join_column(["" | T], Index, _, List) ->
    find_join_column(T, Index + 1, Index, List);

find_join_column([_ | T], Index, Column, List) when Column > 0 ->
    find_join_column(T, Index + 1, 0, [#{column => Column, length => Index - Column} | List]);

find_join_column([_ | T], Index, Column, List) ->
    find_join_column(T, Index + 1, Column, List).


%% shrink column by index and length
join_column([], _, [], List) ->
    lists:reverse(List);

join_column([#{column := Index, length := Length} | T], Index, Row, List) ->
    SubColumn = lists:sublist(Row, Length),
    Sub = join_column_convert(SubColumn, false, []),
    join_column(T, Index + 1, Row, [Sub | List]);

join_column(Column, Index, [H | T], List) ->
    join_column(Column, Index + 1, T, [H | List]).


%% #{key => value}/{element, ...}
join_column_convert([], true, List) ->
    lists:flatten(lists:concat(["[", string:join(lists:reverse(List), ", "), "]"]));

join_column_convert([Column | T], Object, List) ->
    case string:find(Column, "=") of
        true ->
            %% #{key => value}
            NewColumn = lists:concat(["#{", string:replace(Column, "=", "=>"), "}"]),
            join_column_convert(T, true, [NewColumn | List]);
        false ->
            %% {element, ...}
            NewColumn = lists:concat(["{", Column, "}"]),
            join_column_convert(T, Object, [NewColumn | List])
    end.


%%%===================================================================
%%% take book part
%%%===================================================================

%% <Workbook></Workbook>
take_book(XmlData) ->
    %% sheet data
    SheetList = xmerl_xpath:string("/Workbook/Worksheet", XmlData),
    SheetData = take_sheet(SheetList, #{}),
    
    %% property
    PropertyList = xmerl_xpath:string("/Workbook/CustomDocumentProperties", XmlData),
    CustomProperty = take_custom_property(PropertyList, #{}),

    #{sheet => SheetData, property => CustomProperty}.

%%%===================================================================
%%% take custom property part
%%%===================================================================

take_custom_property([], Validation) ->
    Validation;
take_custom_property([#xmlElement{name = Name, content = Content} | T], Validation) ->
    NewValidation = take_validation_property(Content, Name, Validation),
    take_custom_property(T, NewValidation).


take_validation_property([], _, Validation) ->
    Validation;
take_validation_property([H | T], Property, Validation) ->
    case catch parser:evaluate(take_text(H)) of
        {'EXIT', _} ->
            take_validation_property(T, Property, Validation);
        ValueKeyMap ->
            %% [SheetName, Column] = string:tokens(type:to_list(Property), "-"),
            NewValidation = Validation#{Property => ValueKeyMap},
            take_validation_property(T, Property, NewValidation)
    end.

%%%===================================================================
%%% take sheet part
%%%===================================================================

%% <Worksheet></Worksheet>
take_sheet([], Map) ->
    Map;
take_sheet([Sheet = #xmlElement{attributes = Attributes} | T], Map) ->
    %% single table in sheet
    #xmlAttribute{value = Name} = listing:key_find('ss:Name', #xmlAttribute.name, Attributes, #xmlAttribute{}),
    %% ss:Protected="1"
    #xmlAttribute{value = Protected} = listing:key_find('ss:Protected', #xmlAttribute.name, Attributes, #xmlAttribute{}),

    %% check sheet name exists
    Name == undefined andalso erlang:throw("cound not take sheet name"),
    Table = hd(xmerl_xpath:string("//Table", Sheet)),

    %% SheetDataValidation
    DataValidation = [take_validation(Validation) || Validation <- xmerl_xpath:string("//DataValidation", Sheet)],

    %% SheetHidden
    Hidden = lists:append([Value || #xmlElement{content = Content} <- xmerl_xpath:string("//Visible", Sheet), #xmlText{value = Value} <- Content]),

    Property = #{
        protected => Protected, 
        hidden => Hidden, 
        validation => DataValidation
    },

    take_sheet(T, Map#{Name => #{data => take_table(Table), property => Property}}).


%% <Table></Table>
take_table(Table) ->
    take_row(xmerl_xpath:string("//Row", Table)).


%% <Row></Row>
take_row([Head | Tail]) ->
    First = take_row(xmerl_xpath:string("//Cell", Head), 1, array:new([{default, []}])),
    Array = array:from_list(lists:duplicate(length(First), []), []),
    [First | [take_row(xmerl_xpath:string("//Cell", Row), 1, Array) || Row <- Tail]].


%% <Row></Row>
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


%% ...
take_text(#xmlElement{content = []}) ->
    [""];
%% <Text></Text>
take_text(#xmlElement{content = Content}) ->
    %% single text in data
    hd([Value || #xmlText{value = Value} <- Content]).


%%%===================================================================
%%% take validation part
%%%===================================================================

take_validation(Validation) ->
    %% single range in data validation
    ValidationRange = hd([take_text(Data) || Data <- xmerl_xpath:string("//Range", Validation)]),
    %% single value in data validation
    ValidationValue = hd([take_text(Data) || Data <- xmerl_xpath:string("//Value", Validation)]),
    Column = take_validation_column(ValidationRange),
    %% data column, sheet name, sheet column
    take_validation_value(Column, ValidationValue).


%% SheetName!R2C6:R1048576C6
take_validation_column(RowColumn) ->
    {match, [Column]} = re:run(unicode:characters_to_binary(RowColumn), "(?<=C)\\d+", [{capture, first, binary}]),
    binary_to_integer(Column).


%% "option,..."
take_validation_value(Column, [$" | ValidationValue]) ->
    Value = string:tokens(lists:droplast(ValidationValue), ","),
    #{type => list, column => Column, value => Value};
%% sheet!column
take_validation_value(Column, ValidationValue) ->
    Name = hd(string:tokens(ValidationValue, "!")),
    #{type => sheet, column => Column, name => Name}.

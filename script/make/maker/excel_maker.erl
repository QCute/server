%%%-------------------------------------------------------------------
%%% @doc
%%% make database data to excel and excel to database data
%%% @end
%%%-------------------------------------------------------------------
-module(excel_maker).
-export([to_xml/2]).
-export([to_table/2]).
-include_lib("xmerl/include/xmerl.hrl").
-record(field, {name = [], default = [], type = [], format = [], comment = [], position = 0, key = [], extra = []}).
%%%===================================================================
%%% Table to XML
%%%===================================================================
%% @doc make xml sheet part
to_xml(Table, Path) ->
    %% connect database
    maker:connect_database(),
    %% Because of system compatibility problems
    %% because of the utf8/gbk character set problem, use table name as file name
    %% load table data
    %% [{Type, [{Key, Value}, ...]}, ...]
    SourceValidateData = [{Type, [{Key, unicode:characters_to_list(Value)} || [Key, Value] <- db:select(<<"SELECT `key`, `value` FROM `validate_data` WHERE `type` = '~s'">>, [Type])]} || [Type] <- db:select("SELECT DISTINCT `type` FROM `validate_data`")],
    SourceReferenceData = [{Key, [{Value, unicode:characters_to_list(Description)} || [Value, Description] <- db:select(<<"SELECT `value`, `description` FROM `reference_data` WHERE `key` = '~s'">>, [Key])]} || [Key] <- db:select("SELECT DISTINCT `key` FROM `reference_data`")],
    {Name, Data} = parse_table(Table, SourceValidateData, SourceReferenceData),
    %% xml sheet head
    Book = io_lib:format("<?xml version=\"1.0\" encoding=\"utf-8\"?><?mso-application progid=\"Excel.Sheet\"?>~ts", [make_book(Data)]),
    %% specific path
    SpecificPath = filename:absname(Path) ++ "/",
    %% !!! different os shell need different encode type
    %% !!! such windows nt with gbk need characters list/binary int
    %% !!! the unix shell with utf8 need characters list/binary
    %% characters list int
    FileName = lists:concat([SpecificPath, Name, ".xml"]),
    filelib:ensure_dir(SpecificPath),
    file:delete(FileName),
    file:write_file(FileName, unicode:characters_to_binary(Book)).

parse_table(Table, SourceValidateData, SourceReferenceData) ->
    %% fetch table comment
    TableComment = lists:append(db:select(<<"SELECT `TABLE_COMMENT` FROM information_schema.`TABLES` WHERE `TABLE_SCHEMA` = DATABASE() AND `TABLE_NAME` = '~s';">>, [Table])),
    TableComment == [] andalso erlang:throw(lists:flatten(io_lib:format("no such table: ~s", [Table]))),
    Name = unicode:characters_to_list(hd(TableComment)),
    %% fetch table fields
    Fields = parser:convert(db:select(<<"SELECT `COLUMN_NAME`, `COLUMN_DEFAULT`, `COLUMN_TYPE`, `DATA_TYPE`, `COLUMN_COMMENT`, `ORDINAL_POSITION`, `COLUMN_KEY`, `EXTRA` FROM information_schema.`COLUMNS` WHERE `TABLE_SCHEMA` = DATABASE() AND `TABLE_NAME` = '~s' ORDER BY `ORDINAL_POSITION`;">>, [Table]), field),
    {ColumnComment, Validation, ValidationData} = load_validation(Fields, SourceValidateData, 1, [], [], []),
    ReferenceData = load_reference(Fields, SourceReferenceData, []),
    %% target table all data
    SourceData = db:select(<<"SELECT * FROM `~s`">>, [Table]),
    %% transform data with ValidationData
    TransformData = transform_data(SourceData, Validation, ValidationData, []),
    %% add column comment and validation data
    %% convert unicode binary list to characters list
    SheetData = {Name, [ColumnComment | TransformData], Validation},
    %% all sheet data
    %% convert unicode binary list to binary
    {Name, [SheetData] ++ ValidationData ++ ReferenceData}.

%% load validation data
load_validation([], _, _, ColumnComment, Validation, DataList) ->
    {lists:reverse(ColumnComment), lists:reverse(Validation), lists:reverse(DataList)};
load_validation([#field{name = Name, comment = Comment} | T], SourceValidationData, Index, ColumnComment, Validation, DataList) ->
    %% remove (.*?) from comment
    CommentName = unicode:characters_to_list(Comment),
    %% CommentName = re:replace(binary_to_list(Comment), "validate\\(.*?\\)|\\(|\\)|\\[|\\]|\\{|\\}", "", [global, {return, binary}]),
    %% excel table name contain comma(,) could not validation column data problem
    %% Comment = [X || X <- CommentName, X =/= $, andalso X =/= $( andalso X =/= $) andalso X =/= $[ andalso X =/= $] andalso X =/= ${ andalso X =/= $}],
    %% convert unicode binary list to characters list
    ValidationSheetName = unicode:characters_to_list(re:replace(binary_to_list(Comment), "\\w+\\(.*?\\)|\\(|\\)|\\[|\\]|\\{|\\}", "", [global, {return, binary}])),
    %% @deprecated old mode
    %% capture (`table`.`key`,`table`.`value`)
    %% "(?<=validate\\()(`?\\w+`?)\\.`?\\w+`?\\s*,\\s*(`?\\w+`?)\\.`?\\w+`?(?=\\))"
    %% @recommend new mode
    %% read validation data from table validate_data
    case re:run(Comment, "(?<=validate\\().*?(?=\\))", [global, {capture, all, binary}]) of
        {match, [[Type]]} ->
            %% fetch table k,v data
            %% read from script instead of database
            Data = element(2, listing:key_find(Type, 1, SourceValidationData, {Type, []})),
            Data == [] andalso erlang:throw(lists:flatten(io_lib:format("could not found validation option: ~s in field: ~s", [Type, Name]))),
            %% column comment as sheet name
            %% Validation
            %% |--- Range: C Index(C1/C2/...)
            %% |--- Value: Comment!C2(kv's data v)
            load_validation(T, SourceValidationData, Index + 1, [CommentName | ColumnComment], [{Index, ValidationSheetName, 2} | Validation], [{ValidationSheetName, Data, validation} | DataList]);
        _ ->
            %% ensure zip function data list length equal column length
            load_validation(T, SourceValidationData, Index + 1, [CommentName | ColumnComment], Validation, DataList)
    end.

load_reference([], _, DataList) ->
    lists:reverse(DataList);
load_reference([#field{name = Name, comment = Comment} | T], SourceReferenceData, DataList) ->
    %% remove (.*?) from comment
    %% CommentName = re:replace(binary_to_list(Comment), "reference\\(.*?\\)|\\(|\\)|\\[|\\]|\\{|\\}", "", [global, {return, binary}]),
    %% excel table name contain comma(,) could not Reference column data problem
    %% Comment = [X || X <- CommentName, X =/= $, andalso X =/= $( andalso X =/= $) andalso X =/= $[ andalso X =/= $] andalso X =/= ${ andalso X =/= $}],
    %% convert unicode binary list to characters list
    ReferenceSheetName = unicode:characters_to_list(re:replace(binary_to_list(Comment), "reference\\(.*?\\)|ref\\(.*?\\)|\\(|\\)|\\[|\\]|\\{|\\}", "", [global, {return, binary}])),
    %% @deprecated old mode
    %% capture (`table`.`key`,`table`.`value`)
    %% "(?<=reference\\()(`?\\w+`?)\\.`?\\w+`?\\s*,\\s*(`?\\w+`?)\\.`?\\w+`?(?=\\))"
    %% @recommend new mode
    %% read Reference data from table Reference_data
    case re:run(Comment, "(?<=reference\\(|ref\\().*?(?=\\))", [global, {capture, all, binary}]) of
        {match, [[Type]]} ->
            %% fetch table k,v data
            %% read from script instead of database
            Data = element(2, listing:key_find(Type, 1, SourceReferenceData, {Type, []})),
            Data == [] andalso erlang:throw(lists:flatten(io_lib:format("could not found reference option: ~s in field: ~s", [Type, Name]))),
            %% column comment as sheet name
            %% Validation
            %% |--- Range: C Index(C1/C2/...)
            %% |--- Value: Comment!C2(kv's data v)
            AppendHeadData = [{[25968, 20540], [25551, 36848]} | Data],
            load_reference(T, SourceReferenceData, [{ReferenceSheetName ++ [21442, 32771], AppendHeadData, reference} | DataList]);
        _ ->
            %% ensure zip function data list length equal column length
            load_reference(T, SourceReferenceData, DataList)
    end.

transform_data([], _, _, List) ->
    lists:reverse(List);
transform_data([Row | T], Validation, ValidationData, List) ->
    NewRow = transform_validation(Validation, ValidationData, Row),
    transform_data(T, Validation, ValidationData, [NewRow | List]).

transform_validation([], _, Row) ->
    Row;
transform_validation([{Index, SheetName, _} | T], ValidationData, Row) ->
    %% find validation data by sheet name
    Data = element(2, listing:key_find(SheetName, 1, ValidationData, {[], []})),
    %% find data map by slot
    {_, Value} = listing:key_find(lists:nth(Index, Row), 1, Data, {"", ""}),
    transform_validation(T, ValidationData, listing:set(Index, Row, Value)).

%% make xml
make_book(DataList) ->
    %% use MicroSoftYaHei as default style font
    Style = io_lib:format("<Styles><Style ss:ID=\"s01\"><Font ss:FontName=\"~ts\"/></Style></Styles>", [[24494, 36719, 38597, 40657]]),
    Sheet = lists:concat([make_sheet(Data) || Data <- DataList]),
    io_lib:format("<Workbook xmlns=\"urn:schemas-microsoft-com:office:spreadsheet\" xmlns:ss=\"urn:schemas-microsoft-com:office:spreadsheet\">~ts~ts</Workbook>", [Style, Sheet]).

make_sheet({Name, Data, validation}) ->
    %% hide validation
    Hidden = "<WorksheetOptions xmlns=\"urn:schemas-microsoft-com:office:excel\"><Visible>SheetHidden</Visible></WorksheetOptions>",
    io_lib:format("<Worksheet ss:Name=\"~ts\">~ts~ts</Worksheet>", [Name, make_table(Data), Hidden]);
make_sheet({Name, Data, reference}) ->
    io_lib:format("<Worksheet ss:Name=\"~ts\">~ts</Worksheet>", [Name, make_table(Data)]);
make_sheet({Name, Data, Validation}) ->
    ValidationContent = lists:concat([make_data_validation(Range, SheetName, Value) || {Range, SheetName, Value} <- Validation]),
    io_lib:format("<Worksheet ss:Name=\"~ts\">~ts~ts</Worksheet>", [Name, make_table(Data), ValidationContent]).

make_table(Table) when is_tuple(Table) ->
    make_table(tuple_to_list(Table));
make_table(Table) when is_list(Table) ->
    Row = lists:concat([make_row(Row) || Row <- Table]),
    io_lib:format("<Table>~ts</Table>", [Row]).

make_row(Row) when is_tuple(Row) ->
    make_row(tuple_to_list(Row));
make_row(Row) when is_list(Row) ->
    Cell = lists:concat([make_cell(Cell) || Cell <- Row]),
    io_lib:format("<Row>~ts</Row>", [Cell]).

make_cell(Text) ->
    io_lib:format("<Cell>~ts</Cell>", [make_data(Text)]).

make_data(Text) when is_number(Text) ->
    io_lib:format("<Data ss:Type=\"Number\">~w</Data>", [Text]);
make_data(Text) when is_binary(Text) ->
    io_lib:format("<Data ss:Type=\"String\">~ts</Data>", [Text]);
make_data(Text) when is_atom(Text) ->
    io_lib:format("<Data ss:Type=\"String\">~ts</Data>", [Text]);
make_data(Text) when is_list(Text) ->
    io_lib:format("<Data ss:Type=\"String\">~ts</Data>", [Text]).

%% data validation
make_data_validation(Range, SheetName, Value) ->
    io_lib:format("<DataValidation xmlns=\"urn:schemas-microsoft-com:office:excel\">~ts~ts~ts~ts</DataValidation>", [make_range(Range), make_type(), make_value(SheetName, Value), make_throw_style()]).

make_range(Range) ->
    io_lib:format("<Range>C~w</Range>", [Range]).

make_type() ->
    "<Type>List</Type>".

make_value(SheetName, Value) ->
    io_lib:format("<Value>~ts!C~w</Value>", [SheetName, Value]).

make_throw_style() ->
    "<throwStyle>Stop</throwStyle>".

%%%===================================================================
%%% XML to Table
%%%===================================================================
%% @doc restore database part
to_table(File, Name) ->
    %% restore data
    {XmlData, Reason} = xmerl_scan:file(File),
    XmlData == error andalso erlang:throw(lists:flatten(io_lib:format("cound not open file: ~p", [Reason]))),
    SheetList = xmerl_xpath:string("/Workbook/Worksheet", XmlData),
    SheetData = [take_sheet(Sheet) || Sheet <- SheetList],
    RestoreData = restore_sheet(SheetData, SheetData, []),
    {_, TableData, _} = listing:key_find(Name, 1, RestoreData, {Name, [], []}),
    %% check sheet exists
    TableData == [] andalso erlang:throw(lists:flatten(io_lib:format("cound not found sheet by file name: ~ts", [Name]))),
    [Head | Data] = TableData,
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
            Format = list_to_binary(lists:concat(["(", string:join(lists:duplicate(length(Head), "'~s'"), ","), ")"])),
            db:insert(parser:collect(Data, {<<"INSERT INTO `", Table/binary, "` VALUES ">>, Format})),
            ok;
        [] ->
            erlang:throw(lists:flatten(io_lib:format("could not found table by comment: ~ts", [Name])));
        More ->
            erlang:throw(lists:flatten(io_lib:format("found multiple table: ~p", [More])))
    end.

restore_sheet([], _, List) ->
    List;
restore_sheet([{SheetName, Table, Validation} | T], SheetData, List) ->
    NewTable = restore_table(Validation, SheetData, Table),
    restore_sheet(T, SheetData, [{SheetName, NewTable, Validation} | List]).

restore_table([], _, Table) ->
    Table;
restore_table([{Range, SheetName, Column} | T], SheetData, Table) ->
    {_, Data, _} = listing:key_find(SheetName, 1, SheetData, {SheetName, [], []}),
    %% check validation data sheet exists
    Data == [] andalso erlang:throw(lists:flatten(io_lib:format("count not found validation data by sheet name: ~ts", [SheetName]))),
    RestoreData = [list_to_tuple(Row) || Row <- Data],
    %% replace slot with validation data
    NewTable = [listing:set(Range, Row, element(1, listing:key_find(lists:nth(Range, Row), Column, RestoreData, {"", ""}))) || Row <- Table],
    restore_table(T, SheetData, NewTable).

take_sheet(Sheet = #xmlElement{attributes = Attributes}) ->
    %% single table in sheet
    #xmlAttribute{value = Value} = listing:key_find('ss:Name', #xmlAttribute.name, Attributes, #xmlAttribute{}),
    %% check sheet name exists
    Value == undefined andalso erlang:throw("cound not take sheet name"),
    Table = hd(xmerl_xpath:string("//Table", Sheet)),
    DataValidation = [take_validation(Validation) || Validation <- xmerl_xpath:string("//DataValidation", Sheet)],
    {Value, take_table(Table), DataValidation}.

take_table(Table) ->
    [Head | Tail] = [take_row(Row) || Row <- xmerl_xpath:string("//Row", Table)],
    Length = length(Head),
    %% fill data tail empty slot
    [Head | [case Length - length(Row) of Diff when Diff > 0 -> Row ++ lists:duplicate(Diff, []); _ -> Row end || Row <- Tail]].

take_row(Row) ->
    %% remove slot 0
    tl(take_row(xmerl_xpath:string("//Cell", Row), 1, array:new([{default, ""}]))).

take_row([], _, Array) ->
    array:to_list(Array);
take_row([Cell = #xmlElement{attributes = Attributes} | T], Index, Array) ->
    %% take index offset
    #xmlAttribute{value = Value} = listing:key_find('ss:Index', #xmlAttribute.name, Attributes, #xmlAttribute{value = integer_to_list(Index)}),
    %% single data in cell
    Data = hd([take_text(Data) || Data <- xmerl_xpath:string("//Data", Cell)]),
    NewIndex = list_to_integer(Value),
    NewArray = array:set(NewIndex, Data, Array),
    take_row(T, NewIndex + 1, NewArray).

take_validation(Validation) ->
    %% single range in data validation
    Range = hd([take_text(Data) || Data <- xmerl_xpath:string("//Range", Validation)]),
    %% single value in data validation
    Value = hd([take_text(Data) || Data <- xmerl_xpath:string("//Value", Validation)]),
    %% data column, sheet name, sheet column
    [SheetName, Column] = string:tokens(Value, "!"),
    {list_to_integer(hd(string:tokens(Range, "C"))), SheetName, list_to_integer(hd(string:tokens(Column, "C")))}.

take_text(#xmlElement{content = []}) ->
    [""];
take_text(#xmlElement{content = Content}) ->
    %% single text in data
    hd([Value || #xmlText{value = Value} <- Content]).

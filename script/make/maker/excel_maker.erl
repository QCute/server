%%%-------------------------------------------------------------------
%%% @doc
%%% make database data to excel and excel to database data
%%% @end
%%%-------------------------------------------------------------------
-module(excel_maker).
-export([to_xml/1, to_xml/2]).
-export([to_table/1]).
-include_lib("xmerl/include/xmerl.hrl").
-record(field, {name = [], default = [], type = [], format = [], comment = [], position = 0, key = [], extra = []}).
%%%===================================================================
%%% Table to XML
%%%===================================================================
%% @doc make xml sheet part
to_xml(Table) ->
    to_xml(Table, "").
to_xml(Table, Path) ->
    %% connect database
    maker:connect_database(),
    %% Because of system compatibility problems
    %% because of the utf8/gbk character set problem, use table name as file name
    %% load table data
    %% [{Type, [{Key, Value}, ...]}, ...]
    SourceValidateData = [{type:to_atom(Type), [{parser:to_term(Key), unicode:characters_to_list(Value)} || [Key, Value] <- db:select(<<"SELECT `key`, `value` FROM `validate_data` WHERE `type` = '~s'">>, [Type])]} || [Type] <- db:select("SELECT DISTINCT `type` FROM `validate_data`")],
    SourceReferenceData = [{type:to_atom(Key), [{unicode:characters_to_list(Value), unicode:characters_to_list(Description)} || [Value, Description] <- db:select(<<"SELECT `value`, `description` FROM `reference_data` WHERE `key` = '~s'">>, [Key])]} || [Key] <- db:select("SELECT DISTINCT `key` FROM `reference_data`")],
    {Name, Data} = parse_table(Table, SourceValidateData, SourceReferenceData),
    %% make work book element
    Element = make_book(Data),
    %% export to characters list
    List = xmerl:export_element(Element, xmerl_xml),
    %% xml sheet head
    Head = <<"<?xml version=\"1.0\" encoding=\"utf-8\"?><?mso-application progid=\"Excel.Sheet\"?>">>,
    WorkBook = unicode:characters_to_binary(lists:flatten(List)),
    %% specific path
    SpecificPath = filename:absname(Path) ++ "/",
    %% !!! different os shell need different encode type
    %% !!! such windows nt with gbk need characters list/binary int
    %% !!! the unix shell with utf8 need characters list/binary
    %% characters list int
    FileName = lists:concat([SpecificPath, Name, ".xml"]),
    file:delete(FileName),
    case file:write_file(FileName, <<Head/binary, WorkBook/binary>>) of
        {error, Reason} ->
            erlang:throw(Reason);
        ok ->
            ok
    end.

make_book(Data) ->
    Attributes = [
        #xmlAttribute{name = xmlns, value = "urn:schemas-microsoft-com:office:spreadsheet"},
        #xmlAttribute{name = 'xmlns:ss', value = "urn:schemas-microsoft-com:office:spreadsheet"}
    ],
    Style = make_style(),
    #xmlElement{name = 'Workbook', attributes = Attributes, content = [Style | make_sheet(Data, [])]}.

make_style() ->
    %% use MicroSoftYaHei as default style font
    MicrosoftYaHei = [24494, 36719, 38597, 40657],
    Font = #xmlElement{name = 'Font', attributes = [#xmlAttribute{name = 'ss:FontName', value = MicrosoftYaHei}], content = []},
    Style = #xmlElement{name = 'Style', attributes = [#xmlAttribute{name = 'ss:ID', value = "s01"}], content = [Font]},
    #xmlElement{name = 'Styles', attributes = [], content = [Style]}.

make_sheet([], List) ->
    lists:reverse(List);
make_sheet([{Name, Data, validation} | T], List) ->
    Sheet = #xmlElement{name = 'Worksheet', attributes = [#xmlAttribute{name = 'ss:Name', value = Name}], content = [make_table(Data), make_sheet_option()]},
    make_sheet(T, [Sheet | List]);
make_sheet([{Name, Data, reference} | T], List) ->
    Sheet = #xmlElement{name = 'Worksheet', attributes = [#xmlAttribute{name = 'ss:Name', value = Name}], content = [make_table(Data)]},
    make_sheet(T, [Sheet | List]);
make_sheet([{Name, Data, Validation} | T], List) ->
    ValidationContent = make_data_validation(Validation, []),
    Sheet = #xmlElement{name = 'Worksheet', attributes = [#xmlAttribute{name = 'ss:Name', value = Name}], content = [make_table(Data) | ValidationContent]},
    make_sheet(T, [Sheet | List]).

make_table(Data) when is_tuple(Data) ->
    #xmlElement{name = 'Table', content = [make_row(Row) || Row <- tuple_to_list(Data)]};

make_table(Data) when is_list(Data) ->
    #xmlElement{name = 'Table', content = [make_row(Row) || Row <- Data]}.

make_row(Row) when is_tuple(Row) ->
    #xmlElement{name = 'Row', content = [make_cell(Text) || Text <- tuple_to_list(Row)]};

make_row(Row) when is_list(Row) ->
    #xmlElement{name = 'Row', content = [make_cell(Text) || Text <- Row]}.

make_cell(Text) ->
    #xmlElement{name = 'Cell', content = [make_data(Text)]}.

make_data(Text) when is_integer(Text) ->
    #xmlElement{name = 'Data', attributes = [#xmlAttribute{name = 'ss:Type', value = "Number"}], content = [make_text(integer_to_list(Text))]};
make_data(Text) when is_atom(Text) ->
    #xmlElement{name = 'Data', attributes = [#xmlAttribute{name = 'ss:Type', value = "String"}], content = [make_text(atom_to_list(Text))]};
make_data(Text) when is_list(Text) ->
    #xmlElement{name = 'Data', attributes = [#xmlAttribute{name = 'ss:Type', value = "String"}], content = [make_text(Text)]}.

make_sheet_option() ->
    #xmlElement{name = 'WorksheetOptions', attributes = [#xmlAttribute{name = xmlns, value = "urn:schemas-microsoft-com:office:excel"}], content = [make_visible()]}.

make_visible() ->
    #xmlElement{name = 'Visible', content = [make_text("SheetHidden")]}.

make_data_validation([], List) ->
    List;
make_data_validation([{Range, Value} | T], List) ->
    Validation = #xmlElement{
        name = 'DataValidation',
        attributes = [#xmlAttribute{name = xmlns, value = "urn:schemas-microsoft-com:office:excel"}],
        content = [make_range(Range), make_type(), make_value(Value), make_throw_style()]
    },
    make_data_validation(T, [Validation | List]).

make_range(Range) ->
    #xmlElement{name = 'Range', content = [make_text(Range)]}.

make_type() ->
    #xmlElement{name = 'Type', content = [make_text("List")]}.

make_value(Value) ->
    #xmlElement{name = 'Value', content = [make_text(Value)]}.

make_throw_style() ->
    #xmlElement{name = 'throwStyle', content = [make_text("Stop")]}.

make_text(Text) ->
    #xmlText{value = Text}.

%%%===================================================================
%%% parse table data part
%%%===================================================================
parse_table(Table, SourceValidateData, SourceReferenceData) ->
    %% fetch table comment
    TableComment = lists:append(db:select(<<"SELECT `TABLE_COMMENT` FROM information_schema.`TABLES` WHERE `TABLE_SCHEMA` = DATABASE() AND `TABLE_NAME` = '~s';">>, [Table])),
    TableComment == [] andalso erlang:throw(lists:flatten(io_lib:format("no such table: ~s", [Table]))),
    Name = unicode:characters_to_list(hd(TableComment)),
    %% fetch table fields
    Fields = parser:convert(db:select(<<"SELECT `COLUMN_NAME`, `COLUMN_DEFAULT`, `COLUMN_TYPE`, `DATA_TYPE`, `COLUMN_COMMENT`, `ORDINAL_POSITION`, `COLUMN_KEY`, `EXTRA` FROM information_schema.`COLUMNS` WHERE `TABLE_SCHEMA` = DATABASE() AND `TABLE_NAME` = '~s' ORDER BY `ORDINAL_POSITION`;">>, [Table]), field),
    {ColumnComment, Validation, ValidateData} = load_validation(Fields, SourceValidateData, 1, [], [], []),
    ReferenceData = load_reference(Fields, SourceReferenceData, []),
    %% target table all data
    DataBaseData = db:select(<<"SELECT * FROM `~s`">>, [Table]),
    %% transform data with ValidateData
    TransformData = transform_data(DataBaseData, ValidateData),
    %% remove empty data validate data
    RemoveEmpty = [X || {_, [_ | _], _} = X <- ValidateData],
    %% add column comment and validate data
    %% convert unicode binary list to characters list
    SheetData = {Name, [ColumnComment | TransformData], Validation},
    %% all sheet data
    %% convert unicode binary list to binary
    {Name, [SheetData] ++ RemoveEmpty ++ ReferenceData}.

%% load validate data
load_validation([], _, _, ColumnComment, Validation, DataList) ->
    {lists:reverse(ColumnComment), lists:reverse(Validation), lists:reverse(DataList)};
load_validation([#field{name = Name, comment = Comment} | T], SourceValidateData, Index, ColumnComment, Validation, DataList) ->
    %% remove (.*?) from comment
    CommentName = unicode:characters_to_list(Comment),
    %% CommentName = re:replace(binary_to_list(Comment), "validate\\(.*?\\)|\\(|\\)|\\[|\\]|\\{|\\}", "", [global, {return, binary}]),
    %% excel table name contain comma(,) could not validate column data problem
    %% Comment = [X || X <- CommentName, X =/= $, andalso X =/= $( andalso X =/= $) andalso X =/= $[ andalso X =/= $] andalso X =/= ${ andalso X =/= $}],
    %% convert unicode binary list to characters list
    ValidateSheetName = unicode:characters_to_list(re:replace(binary_to_list(Comment), "\\w+\\(.*?\\)|\\(|\\)|\\[|\\]|\\{|\\}", "", [global, {return, binary}])),
    %% @deprecated old mode
    %% capture (`table`.`key`,`table`.`value`)
    %% "(?<=validate\\()(`?\\w+`?)\\.`?\\w+`?\\s*,\\s*(`?\\w+`?)\\.`?\\w+`?(?=\\))"
    %% @recommend new mode
    %% read validate data from table validate_data
    case re:run(Comment, "(?<=validate\\().*?(?=\\))", [global, {capture, all, list}]) of
        {match, [[Type]]} ->
            %% fetch table k,v data
            %% read from script instead of database
            Data = element(2, listing:key_find(list_to_atom(Type), 1, SourceValidateData, {Type, []})),
            Data == [] andalso erlang:throw(lists:flatten(io_lib:format("could not found validate option: ~s in field: ~s", [Type, Name]))),
            %% column comment as sheet name
            %% Validation
            %% |--- Range: C Index(C1/C2/...)
            %% |--- Value: Comment!C2(kv's data v)
            load_validation(T, SourceValidateData, Index + 1, [CommentName | ColumnComment], [{"C" ++ integer_to_list(Index), ValidateSheetName ++ "!C2"} | Validation], [{ValidateSheetName, Data, validation} | DataList]);
        _ ->
            %% ensure zip function data list length equal column length
            load_validation(T, SourceValidateData, Index + 1, [CommentName | ColumnComment], Validation, [{ValidateSheetName, [], []} | DataList])
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
    case re:run(Comment, "(?<=reference\\(|ref\\().*?(?=\\))", [global, {capture, all, list}]) of
        {match, [[Type]]} ->
            %% fetch table k,v data
            %% read from script instead of database
            Data = element(2, listing:key_find(list_to_atom(Type), 1, SourceReferenceData, {Type, []})),
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

%% transform database data to excel data
transform_data(DataBaseData, RawValidateData) ->
    ValidateData = [List || {_, List, _} <- RawValidateData],
    [zip(Row, ValidateData, []) || Row <- DataBaseData].

zip([], [], List) ->
    %% reverse column order
    lists:reverse(List);
zip([Value | ValueT], [[] | ValidationT], List) when is_integer(Value) ->
    %% not validate row
    zip(ValueT, ValidationT, [Value | List]);
zip([Value | ValueT], [[] | ValidationT], List) ->
    %% not validate row
    Result = unicode:characters_to_list(type:to_binary(Value)),
    zip(ValueT, ValidationT, [Result | List]);
zip([<<>> | ValueT], [Validation | ValidationT], List) ->
    %% empty string
    Result = element(2, listing:key_find("", 1, Validation, listing:key_find('', 1, Validation, {[], []}))),
    zip(ValueT, ValidationT, [Result | List]);
zip([Value | ValueT], [Validation | ValidationT], List) when is_integer(Value) ->
    %% validate row
    Result = element(2, listing:key_find(Value, 1, Validation, {[], []})),
    zip(ValueT, ValidationT, [Result | List]);
zip([Value | ValueT], [Validation | ValidationT], List) ->
    %% validate row
    Result = element(2, listing:key_find(type:to_atom(Value), 1, Validation, {[], []})),
    zip(ValueT, ValidationT, [Result | List]).
%%%===================================================================
%%% XML to Table
%%%===================================================================
%% @doc restore database part
to_table(File) ->
    %% connect database
    maker:connect_database(),
    %% restore data
    {Name, Data} = restore(File),
    %% Name must characters binary or characters list
    %% binary format with ~s will convert to characters list,  一  => [228, 184, 128]
    %% binary format with ~ts will convert to unicode list,    一  => [19968]
    case db:select(<<"SELECT `TABLE_NAME` FROM information_schema.`TABLES` WHERE `TABLE_SCHEMA` = DATABASE() AND `TABLE_COMMENT` = '~s';">>, [Name]) of
        [[Table]] ->
            AllData = ["(" ++ string:join([lists:concat(["'", Cell, "'"]) || Cell <- Row], ",") ++ ")" || Row <- Data],
            %% ensure data order
            db:query(parser:format(<<"TRUNCATE `~s`">>, [Table])),
            %% convert sql(unicode) to list
            db:insert(<<"INSERT INTO `~s` VALUES ~s">>, [binary_to_list(Table), string:join(lists:reverse(AllData), ", ")]),
            ok;
        [] ->
            erlang:throw(lists:flatten(io_lib:format("could not found table by comment: ~ts", [Name])));
        More ->
            erlang:throw(lists:flatten(io_lib:format("found multiple table: ~p", [More])))
    end.

%% load excel sheet data part
%% !!! different os shell will encode to different type
%% !!! unicode file name pass by shell as characters list
%% !!! unicode file name pass by erlang shell as characters list list
restore(File) ->
    SheetName = filename:basename(File, ".xml"),
    %% convert unicode list to binary
    %% different characters encode compatible
    {XmlData, Reason} = xmerl_scan:file(File),
    XmlData == error andalso erlang:throw(lists:flatten(io_lib:format("cound not open file: ~p", [Reason]))),
    %% if file name use utf8 character set, need to convert file name(table name) to sheet name(table comment)
    %% file name to sheet name (table comment)
    %% [[TableComment]] = db:select(<<"SELECT `TABLE_COMMENT` FROM information_schema.`TABLES` WHERE `TABLE_SCHEMA` = DATABASE() AND `TABLE_NAME` = '~s';">>, [Name]),
    %% trim first row (name row)
    [Header | SourceData] = work_book_data(XmlData, SheetName),
    Validation = work_book_data_validation(XmlData, SheetName),
    Data = restore_data(XmlData, SourceData, Validation),
    %% convert unicode list to binary
    ReviseData = revise_row(length(Header), Data, []),
    {SheetName, ReviseData}.

restore_data(_, SourceData, []) ->
    SourceData;
restore_data(XmlData, SourceData, [{Column, Validation} | T]) ->
    SheetName = hd(string:tokens(Validation, "!")),
    ValidateData = [list_to_tuple(X) || X <- work_book_data(XmlData, SheetName)],
    %% validation data sample
    %% Worksheet -> WorksheetOptions -> DataValidation -> Range
    %% R2C2
    %% R4C3,R1C3:R3C3,R5C3:R1048576C3
    {match, [Index]} = re:run(Column, "(?<=C)\\d+", [{capture, first, list}]),
    NewSourceData = restore_row(SourceData, list_to_integer(Index), ValidateData, []),
    restore_data(XmlData, NewSourceData, T).

restore_row([], _, _, List) ->
    List;
restore_row([Row | T], Index, ValidateData, List) ->
    {Key, _} = lists:keyfind(lists:nth(Index, Row), 2, ValidateData),
    %% replace element from list by pos
    New = array:to_list(array:set(Index - 1, Key, array:from_list(Row))),
    restore_row(T, Index, ValidateData, [New | List]).

%% revise tail empty cell/data
revise_row(_, [], List) ->
    lists:reverse(List);
revise_row(Length, [Row | T], List) ->
    %% empty cell/data default empty string
    New = Row ++ lists:duplicate(Length - length(Row), ''),
    revise_row(Length, T, [New | List]).

%% read excel data
%% Sheet must characters list int
work_book_data(#xmlElement{name = 'Workbook', content = Content}, SheetName) ->
    hd([work_sheet(X) || X = #xmlElement{name = 'Worksheet', attributes = Attributes} <- Content, lists:keyfind(SheetName, #xmlAttribute.value, Attributes) =/= false]).

work_sheet(#xmlElement{name = 'Worksheet', content = Content}) ->
    hd([table(X) || X = #xmlElement{name = 'Table'} <- Content]).

table(#xmlElement{name = 'Table', content = Content}) ->
    [row(X) || X = #xmlElement{name = 'Row'} <- Content].

row(#xmlElement{name = 'Row', content = Content}) ->
    row_loop(Content, []).

row_loop([], List) ->
    lists:reverse(List);
row_loop([X = #xmlElement{name = 'Cell', attributes = Attributes} | T], List) ->
    case lists:keyfind('ss:Index', #xmlAttribute.name, Attributes) of
        false ->
            row_loop(T, [cell(X) | List]);
        #xmlAttribute{value = Value} ->
            Column = type:to_integer(Value),
            row_loop(T, [cell(X) | lists:duplicate(Column - length(List) - 1, '')] ++ List)
    end;
row_loop([_ | T], List) ->
    row_loop(T, List).

cell(#xmlElement{name = 'Cell', content = []}) ->
    '';
cell(#xmlElement{name = 'Cell', content = Content}) ->
    hd([data(X) || X = #xmlElement{name = 'Data'} <- Content]).

data(#xmlElement{name = 'Data', content = []}) ->
    '';
data(#xmlElement{name = 'Data', content = Content, attributes = Attributes}) ->
    hd([text(X, Attributes) || X <- Content]).

%% read excel data validation
%% Sheet must characters list int
work_book_data_validation(#xmlElement{name = 'Workbook', content = Content}, Sheet) ->
    hd([work_sheet_data_validation(X) || X = #xmlElement{name = 'Worksheet', attributes = Attributes} <- Content, lists:keyfind(Sheet, #xmlAttribute.value, Attributes) =/= false]).

work_sheet_data_validation(#xmlElement{name = 'Worksheet', content = Content}) ->
    [data_validation(X) || X = #xmlElement{name = 'DataValidation'} <- Content].

data_validation(#xmlElement{name = 'DataValidation', content = Content}) ->
    Range = hd([range(X) || X = #xmlElement{name = 'Range'} <- Content]),
    Value = hd([value(X) || X = #xmlElement{name = 'Value'} <- Content]),
    {Range, Value}.

range(#xmlElement{name = 'Range', content = Content, attributes = Attributes}) ->
    hd([text(X, Attributes) || X <- Content]).

value(#xmlElement{name = 'Value', content = Content, attributes = Attributes}) ->
    hd([text(X, Attributes) || X <- Content]).

%% extract text and format
text(#xmlText{value = Value}, Attributes) ->
    format(Value, Attributes).

format(Text, Attributes) ->
    case lists:keyfind('ss:Type', #xmlAttribute.name, Attributes) of
        #xmlAttribute{value = "Number"} ->
            list_to_integer(Text);
        _ ->
            Text
    end.

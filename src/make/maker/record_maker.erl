%%%-------------------------------------------------------------------
%%% @doc
%%% module database fields to record tool
%%% @end
%%%-------------------------------------------------------------------
-module(record_maker).
-export([start/1]).
-export([parse/2]).
%%%===================================================================
%%% API
%%%===================================================================
%% @doc for shell
start(List) ->
    maker:start(fun parse_table/2, List).

%% @doc parse
parse(DataBase, One) ->
    parse_table(DataBase, One).
%% ====================================================================
%% Internal functions
%% ====================================================================
%% parse per table
parse_table(DataBase, {File, Table}) ->
    parse_table(DataBase, {File, Table, Table});
parse_table(DataBase, {_, Table, Record}) ->
    CommentSql = io_lib:format(<<"SELECT `TABLE_COMMENT` FROM information_schema.`TABLES` WHERE `TABLE_SCHEMA` = '~s' AND `TABLE_NAME` = '~s';">>, [DataBase, Table]),
    FieldsSql = io_lib:format(<<"SELECT `COLUMN_NAME`, `COLUMN_DEFAULT`, `DATA_TYPE`, `COLUMN_COMMENT`, `ORDINAL_POSITION`, `COLUMN_KEY`, `EXTRA` FROM information_schema.`COLUMNS` WHERE `TABLE_SCHEMA` = '~s' AND `TABLE_NAME` = '~s' ORDER BY `ORDINAL_POSITION`;">>, [DataBase, Table]),
    %% fetch table comment
    [[CommentData]] = maker:select(CommentSql),
    %% fetch table fields
    FieldsData = maker:select(FieldsSql),
    %% parse fields
    Total = length(FieldsData),
    Fields = [parse_field(Field, Total) || Field = [_, _, _, C, _, _, _] <- FieldsData, string:str(binary_to_list(C), "(client)") == 0],
    %% write record data and table comment
    Comment = io_lib:format("%% ~s\n%% ~s =====> ~s", [CommentData, Table, Record]),
    Head = io_lib:format("-record(~s, {\n", [Record]),
    RecordData = lists:concat([Comment, "\n", Head, Fields, "}).\n\n"]),
    %% return data
    RecordPatten = io_lib:format("~s\n(?m)(?s)(?<!\\S)(-record\\s*\\(\\s*~s\\s*,.+?)(?=\\.$|\\.\\%)\\.\n?\n?", [Comment, Record]),
    [{RecordPatten, RecordData}].

%% parse per field

parse_field([Name, Default, Type, Comment, Position, _, _], Total) ->
    %% only parse varchar and tinyint, smallint, int, bigint
    MatchDefaultType = re:run(Comment, "(#\\w+\\{\\})", [{capture, first, list}]),
    IsConvert = string:str(binary_to_list(Comment), "(convert)") =/= 0,
    IsUndefined = string:str(binary_to_list(Comment), "(null)") =/= 0,
    SpecifiedValue = re:run(Comment, "(?<=\\()\\d+(?=\\))", [{capture, first, list}]),
    case Type of
        <<"char">> when IsConvert == true ->
            FiledDefault = " = []";
        <<"varchar">> when IsConvert == true ->
            FiledDefault = " = []";
        <<"char">> when Default == undefined orelse IsUndefined == true ->
            FiledDefault = " = undefined";
        <<"varchar">> when Default == undefined orelse IsUndefined == true ->
            FiledDefault = " = undefined";
        <<"char">> when SpecifiedValue =/= nomatch ->
            {match, [DefaultType]} = SpecifiedValue,
            FiledDefault = " = " ++ DefaultType;
        <<"varchar">> when SpecifiedValue =/= nomatch ->
            {match, [DefaultType]} = SpecifiedValue,
            FiledDefault = " = " ++ DefaultType;
        <<"char">> ->
            FiledDefault = " = <<>>";
        <<"varchar">> ->
            FiledDefault = " = <<>>";
        _ when MatchDefaultType =/= nomatch ->
            {match, [DefaultType]} = MatchDefaultType,
            FiledDefault = lists:concat([" = ", DefaultType]);
        _ when is_binary(Default) ->
            FiledDefault = lists:concat([" = ", binary_to_list(Default)]);
        _ ->
            FiledDefault = lists:concat([" = ", Default])
    end,
    %% record field end comma
    case Position of
        Total ->
            Comma = "";
        _ ->
            Comma = ","
    end,
    %% format record field expression
    Expression = io_lib:format("~s~s~s", [Name, FiledDefault, Comma]),
    %% calculate alignment space
    Alignment = lists:duplicate(50 - length(lists:flatten(Expression)), " "),
    %% align comment
    io_lib:format("    ~s~s%% ~s \n", [Expression, Alignment, Comment]).
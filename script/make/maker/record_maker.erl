%%%-------------------------------------------------------------------
%%% @doc
%%% make database fields to record
%%% @end
%%%-------------------------------------------------------------------
-module(record_maker).
-export([start/1]).
%%%===================================================================
%%% API functions
%%%===================================================================
%% @doc for shell
start(List) ->
    maker:start(fun parse_table/1, List).

%%%===================================================================
%%% Internal functions
%%%===================================================================
%% parse per table
parse_table({_, Table}) ->
    CommentSql = io_lib:format(<<"SELECT `TABLE_COMMENT` FROM information_schema.`TABLES` WHERE `TABLE_SCHEMA` = DATABASE() AND `TABLE_NAME` = '~s';">>, [Table]),
    FieldsSql = io_lib:format(<<"SELECT `COLUMN_NAME`, `COLUMN_DEFAULT`, `DATA_TYPE`, `COLUMN_COMMENT`, `ORDINAL_POSITION`, `COLUMN_KEY`, `EXTRA`, `GENERATION_EXPRESSION` FROM information_schema.`COLUMNS` WHERE `TABLE_SCHEMA` = DATABASE() AND `TABLE_NAME` = '~s' ORDER BY `ORDINAL_POSITION`;">>, [Table]),
    %% fetch table comment
    [[CommentData]] = sql:select(CommentSql),
    %% fetch table fields
    FieldsData = sql:select(FieldsSql),
    %% parse fields
    Total = length(FieldsData),
    Fields = [parse_field(Field, Total) || Field = [_, _, _, C, _, _, _, _] <- FieldsData, string:str(binary_to_list(C), "(client)") == 0],
    %% write record data and table comment
    Comment = io_lib:format("%% ~s\n%% ~s =====> ~s", [CommentData, Table, Table]),
    Head = io_lib:format("-record(~s, {\n", [Table]),
    RecordData = lists:concat([Comment, "\n", Head, Fields, "}).\n\n"]),
    %% return data
    RecordPattern = io_lib:format("~s\n(?m)(?s)(?<!\\S)(-record\\s*\\(\\s*~s\\s*,.+?)(?=\\.$|\\.\\%)\\.\n?\n?", [Comment, Table]),
    [{RecordPattern, RecordData}].

%% parse per field
parse_field([Name, Default, Type, Comment, Position, _, Extra, GenerationExpression], Total) ->
    %% only parse varchar, char, tinyint, smallint, int, bigint
    SpecifiedValue = parse_field_default(Comment),
    case Type of
        _ when Extra == <<"auto_increment">> ->
            FiledDefault = " = 0";
        _ when SpecifiedValue =/= [] ->
            FiledDefault = lists:concat([" = ", SpecifiedValue]);
        _ when GenerationExpression =/= undefined ->
            FiledDefault = lists:concat([" = ", binary_to_list(GenerationExpression)]);
        <<"varchar", _/binary>> ->
            FiledDefault = " = []";
        <<"char", _/binary>> ->
            FiledDefault = " = <<>>";
        _ ->
            FiledDefault = lists:concat([" = ", binary_to_list(Default)])
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
    io_lib:format("    ~s~s%% ~s\n", [Expression, Alignment, Comment]).

%% field default
parse_field_default(Comment) ->
    case re:run(Comment, "(?<=default\\().*?(?=\\))", [{capture, first, list}]) of
        {match, [SpecifiedValue]} ->
            SpecifiedValue;
        _ ->
            []
    end.

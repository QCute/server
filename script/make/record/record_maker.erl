%%%-------------------------------------------------------------------
%%% @doc
%%% make database fields to record
%%% @end
%%%-------------------------------------------------------------------
-module(record_maker).
-export([start/1]).
-record(field, {table = <<>>, name = <<>>, default = <<>>, type = <<>>, format = <<>>, comment = <<>>, key = <<>>, extra = <<>>, expression = <<>>, position = 0}).
%%%===================================================================
%%% API functions
%%%===================================================================
%% @doc for shell
start(List) ->
    maker:connect_database(),
    maker:start(fun(#{result := Pattern}) -> Pattern end, parse_file(List, [])).

%%%===================================================================
%%% Internal functions
%%%===================================================================
%% parse per table
parse_file([], List) ->
    lists:reverse(List);
parse_file([#{file := File, client := Client} | T], List) ->
    %% fetch table and comment
    Name = filename:basename(File, ".hrl"),

    File = lists:concat(["include/", Name, ".hrl"]),
    %% create new
    not filelib:is_file(File) andalso file:write_file(File, <<>>),

    Tables = db:select(lists:concat([
        "SELECT `TABLE_NAME`, `TABLE_COMMENT`", " ",
        "FROM information_schema.`TABLES`", " ",
        "WHERE `TABLE_SCHEMA` = DATABASE()", " ",
        "AND `TABLE_NAME` LIKE '", Name, "%'", " ",
        "AND `TABLE_NAME` NOT LIKE '%_log'", " ",
        "ORDER BY `TABLE_NAME`"
    ])),

    Macro = string:to_upper(lists:concat([Name, "_HRL"])),
    MacroHeadPattern = lists:concat(["^(-ifndef\\(", Macro, "\\)\\.", "\n", "-define\\(", Macro, ", '",  Macro, "'\\)\\.", "\n)?\n*"]),
    MacroHeadCode = lists:concat(["-ifndef(", Macro, ").", "\n", "-define(", Macro, ", '", Macro, "').", "\n", "\n"]),
    MacroTailPattern = lists:concat(["(-endif\\.)?\n*$"]),
    MacroTailCode = lists:concat(["\n", "\n", "-endif."]),

    RemoveMacroHead = #{pattern => MacroHeadPattern, code => ""},
    AddMacroHead = #{pattern => MacroHeadPattern, code => MacroHeadCode},
    RemoveMacroTail = #{pattern => MacroTailPattern, code => ""},
    AddMacroTail = #{pattern => MacroTailPattern, code => MacroTailCode},

    RecordPattern = parse_table(Tables, Name, Client, []),
    Pattern = lists:append([RemoveMacroHead, RemoveMacroTail], lists:append(RecordPattern, [AddMacroHead, AddMacroTail])),

    parse_file(T, [#{file => File, result => Pattern} | List]);
parse_file([Name | T], List) ->
    File = lists:concat(["include/", Name, ".hrl"]),
    %% create new
    not filelib:is_file(File) andalso file:write_file(File, <<>>),

    %% fetch table and comment
    Tables = db:select(lists:concat([
        "SELECT `TABLE_NAME`, `TABLE_COMMENT`", " ",
        "FROM information_schema.`TABLES`", " ",
        "WHERE `TABLE_SCHEMA` = DATABASE()", " ",
        "AND `TABLE_NAME` LIKE '", Name, "%'", " ",
        "AND `TABLE_NAME` NOT LIKE '%_log'", " ",
        "ORDER BY `TABLE_NAME`"
    ])),

    Macro = string:to_upper(lists:concat([Name, "_HRL"])),
    MacroHeadPattern = lists:concat(["^(-ifndef\\(", Macro, "\\)\\.", "\n", "-define\\(", Macro, ", '",  Macro, "'\\)\\.", "\n)?\n*"]),
    MacroHeadCode = lists:concat(["-ifndef(", Macro, ").", "\n", "-define(", Macro, ", '", Macro, "').", "\n", "\n"]),
    MacroTailPattern = lists:concat(["(-endif\\.)?\n*$"]),
    MacroTailCode = lists:concat(["\n", "\n", "-endif."]),

    RemoveMacroHead = #{pattern => MacroHeadPattern, code => ""},
    AddMacroHead = #{pattern => MacroHeadPattern, code => MacroHeadCode},
    RemoveMacroTail = #{pattern => MacroTailPattern, code => ""},
    AddMacroTail = #{pattern => MacroTailPattern, code => MacroTailCode},

    RecordPattern = parse_table(Tables, Name, #{}, []),
    Pattern = lists:append([RemoveMacroHead, RemoveMacroTail], lists:append(RecordPattern, [AddMacroHead, AddMacroTail])),

    parse_file(T, [#{file => File, result => Pattern} | List]).

%% parse per table
parse_table([], _, _, List) ->
    lists:reverse(List);
parse_table([[Table, Comment] | Tables], FileName, Preset, List) ->
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
        "`TABLE_NAME` = '", type:to_list(Table), "'", " ",
        "ORDER BY",
        " ", "ORDINAL_POSITION", " ", "ASC"
    ]),

    Fields = parser:convert(db:select(Sql), field, fun(Field = #field{type = Type}) -> Field#field{type = lists:last(binary:split(Type, <<" ">>))} end),
    Fields == [] andalso erlang:throw(lists:flatten(io_lib:format("Table: ~s Not Found ", [Table]))),

    %% format fields
    Total = length(Fields),
    ClientFields = maps:get(type:to_atom(Table), Preset, []),
    FieldsCode = [parse_field(Field, Total) || Field = #field{name = Name} <- Fields, not listing:is_in(binary_to_atom(Name), ClientFields)],

    %% replace pattern
    Pattern = unicode:characters_to_binary(lists:flatten(io_lib:format("%%.*\n(?m)(?s)(?<!\\S)(-record\\s*\\(\\s*~s\\s*,.+?)(?=\\.$|\\.\\%)\\.\n?\n?", [Table]))),
    %% erl_scan:string(element(1, file:read_file(maker:relative_path(File)), 1, [return_white_spaces, return_comments]),

    %% write record data and table comment
    Code = unicode:characters_to_binary(lists:flatten(lists:concat([
        "%% ", unicode:characters_to_list(Comment), "\n",
        "-record(", type:to_list(Table), ", {", "\n",
        FieldsCode, "}).", "\n",
        "\n"
    ]))),

    %% return pattern data
    parse_table(Tables, FileName, Preset, [#{pattern => Pattern, code => Code} | List]).

%% format per field
parse_field(#field{name = Name, default = Default, type = Type, comment = Comment, position = Position, extra = Extra, expression = Expression}, Total) ->
    (Default == undefined andalso Extra =/= <<"auto_increment">>) andalso erlang:throw(lists:flatten(io_lib:format("Field: ~s does not found default value", [Name]))),
    %% only parse varchar, char, tinyint, smallint, int, bigint
    case Type of
        _ when Extra == <<"auto_increment">> ->
            FiledDefault = " = 0";
        <<"char", _/binary>> ->
            FiledDefault = " = <<>>";
        <<"varchar", _/binary>> when Default == <<"''"/utf8>> orelse Default == <<"\"\""/utf8>> ->
            FiledDefault = " = []";
        <<"varchar", _/binary>> ->
            FiledDefault = lists:concat([" = ", Default]);
        <<"enum", _/binary>> ->
            FiledDefault = lists:concat([" = ", binary_to_list(binary:replace(Default, [<<"'">>, <<"\"">>], <<>>, [global]))]);
        <<"set", _/binary>> ->
            FiledDefault = lists:concat([" = [", string:replace(binary_to_list(binary:replace(Default, [<<"'">>, <<"\"">>], <<>>, [global])), ",", ", "), "]"]);
        _ when Expression =/= undefined ->
            FiledDefault = lists:concat([" = ", binary_to_list(Expression)]);
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
    Row = io_lib:format("~s~s~s", [Name, FiledDefault, Comma]),
    %% calculate alignment space
    Alignment = lists:duplicate(50 - length(lists:flatten(Row)), " "),
    %% align comment
    lists:concat(["    ", Row, Alignment, "%%", " ", unicode:characters_to_list(Comment), "\n"]).

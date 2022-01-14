%%%-------------------------------------------------------------------
%%% @doc
%%% make database fields to sql code
%%% @end
%%%-------------------------------------------------------------------
-module(sql_maker).
-export([start/1]).
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
%% parse per table
parse_table({File, Table, Includes}) ->
    parse_table({File, Table, Includes, []});
parse_table({File, Table, Includes, Modes}) ->
    %% data revise
    Revise = fun(Field = #field{name = Name, format = Format, default = Default, comment = Comment}) -> Field#field{name = binary_to_list(Name), format = binary_to_list(Format), default = binary_to_list(Default), comment = binary_to_list(Comment)} end,
    %% fetch table fields
    Fields = parser:convert(db:select(<<"SELECT `COLUMN_NAME`, IF(`EXTRA` = 'auto_increment', 0, IF(`COLUMN_DEFAULT` != 'NULL', `COLUMN_DEFAULT`, `GENERATION_EXPRESSION`)) AS `COLUMN_DEFAULT`, `COLUMN_TYPE`, CASE WHEN `EXTRA` LIKE 'VIRTUAL%' THEN '~~i' WHEN `DATA_TYPE` = 'char' THEN '\\'~~s\\'' WHEN `DATA_TYPE` = 'varchar' THEN '\\'~~w\\'' ELSE '~~w' END AS `DATA_TYPE`, `COLUMN_COMMENT`, `ORDINAL_POSITION`, `COLUMN_KEY`, `EXTRA` FROM information_schema.`COLUMNS` WHERE `TABLE_SCHEMA` = DATABASE() AND `TABLE_NAME` = '~s' ORDER BY `ORDINAL_POSITION`;">>, [Table]), field, Revise),
    %% primary key fields
    StoreFields = [X || X = #field{extra = Extra} <- Fields, is_atom(binary:match(Extra, <<"VIRTUAL">>))],
    PrimaryFields = [X || X = #field{key = <<"PRI">>} <- Fields],
    NormalFields = [X || X = #field{key = Key, extra = <<>>} <- Fields, Key =/= <<"PRI">>],
    EmptyFields = [X || X = #field{extra = <<"VIRTUAL", _/binary>>} <- Fields],
    %% no primary key, could not do anything
    PrimaryFields == [] andalso erlang:throw(lists:flatten(io_lib:format("could not found any primary key in table: ~s", [Table]))),
    %% file data
    Module = io_lib:format("-module(~s).\n", [filename:basename(File, ".erl")]),
    %% include
    Include = [lists:flatten(io_lib:format("-include(\"~s\").\n", [X])) || X <- Includes],
    [Export, Define, Code] = parse_code(type:to_list(Table), type:to_list(Table), Fields, StoreFields, PrimaryFields, NormalFields, EmptyFields, Modes),
    [{"(?s).*", lists:concat([Module, Export, Include, "\n", Define, Code])}].

%% parse all code
parse_code(TableName, Record, FullFields, StoreFields, PrimaryFields, NormalFields, EmptyFields, Modes) ->

    %% insert define part
    InsertFields = [X || X = #field{extra = Extra} <- StoreFields, Extra =/= <<"auto_increment">>],
    InsertDefine = parse_define_insert(TableName, FullFields, InsertFields),

    %% select table key
    SelectKeys = proplists:get_value(select, Modes, PrimaryFields),
    SelectDefine = parse_define_select(TableName, SelectKeys, FullFields),

    %% update define part, no update, primary key as update key by default
    UpdateDefine = parse_define_update(TableName, FullFields, PrimaryFields),

    %% delete define part, no delete, primary key as delete key by default
    DeleteDefine = parse_define_delete(TableName, PrimaryFields),

    %% insert update
    InsertUpdateFlag = [Name || #field{name = Name, comment = Comment} <- FullFields, contain(Comment, "(flag)")],
    InsertUpdateDefine = parse_define_insert_update(TableName, FullFields, StoreFields, NormalFields, InsertUpdateFlag),

    %% select join
    %% in store fields, join_on(`table`.`field`)
    SelectJoinKeys = [{extract(Comment, "(?<=join_on\\()`?\\w+`?(?=\\.)"), extract(Comment, "(?<=join_on\\()(`?\\w+`?\\.`?\\w+`?)(?=\\))"), FieldInfo} || FieldInfo = #field{comment = Comment} <- StoreFields, extract(Comment, "(?<=join_on\\()`?\\w+`?(?=\\.)") =/= []],
    %% in virtual fields, join(`table`.`field`)
    %% join key field use inner table name field
    %% join field must add table name
    %% type revise IF_NULL/AS(name alias)
    SelectJoinFields = [case lists:keymember(Name, #field.name, EmptyFields) of false -> lists:concat(["`", TableName, "`", ".", "`", Name, "`"]); true -> lists:concat(["IFNULL(", extract(Comment, "(?<=join\\()(`?\\w+`?\\.`?\\w+`?)(?=\\))", lists:concat(["`", TableName, "`", ".", "`", Name, "`"])), ", ", Default, ") AS ", "`", Name, "`"]) end || #field{name = Name, comment = Comment, default = Default} <- FullFields],
    SelectJoinDefine = parse_define_select_join(TableName, SelectKeys, SelectJoinKeys, SelectJoinFields),

    %% select (keys) group
    SelectGroupFields = ([{X, extract(Comment, "(?<=\\(select_)\\w+(?=\\))")} || X = #field{comment = Comment} <- StoreFields]),
    SelectGroupList = lists:append([[{Group, Field} || Group <- Groups] || {Field, Groups} <- SelectGroupFields]),
    SelectMergeGroupList = lists:reverse(listing:key_group(1, SelectGroupList, fun({_, Field}, List) -> [Field | List] end)),
    SelectGroupDefine = [parse_define_select_group(TableName, FieldName, Fields, FullFields) || {FieldName, Fields} <- SelectMergeGroupList],
    SelectJoinGroupDefine = [parse_define_select_join(TableName, Fields, SelectJoinKeys, SelectJoinFields, FieldName) || {FieldName, Fields} <- SelectMergeGroupList],

    %% update (fields) group
    UpdateGroupFields = ([{X, extract(Comment, "(?<=\\(update_)\\w+(?=\\))")} || X = #field{comment = Comment} <- StoreFields]),
    UpdateGroupList = lists:append([[{Group, Field} || Group <- Groups] || {Field, Groups} <- UpdateGroupFields]),
    UpdateMergeGroupList = lists:reverse(listing:key_group(1, UpdateGroupList, fun({_, Field}, List) -> [Field | List] end)),
    UpdateGroupDefine = [parse_define_update_group(TableName, FieldName, PrimaryFields, Fields) || {FieldName, Fields} <- UpdateMergeGroupList],

    %% delete (keys) group
    DeleteGroupFields = ([{X, extract(Comment, "(?<=\\(delete_)\\w+(?=\\))")} || X = #field{comment = Comment} <- StoreFields]),
    DeleteGroupList = lists:append([[{Group, Field} || Group <- Groups] || {Field, Groups} <- DeleteGroupFields]),
    DeleteMergeGroupList = lists:reverse(listing:key_group(1, DeleteGroupList, fun({_, Field}, List) -> [Field | List] end)),
    DeleteGroupDefine = [parse_define_delete_group(TableName, FieldName, Fields, []) || {FieldName, Fields} <- DeleteMergeGroupList],

    %% delete in
    AutoIncrementKeys = [X || X = #field{extra = <<"auto_increment">>} <- StoreFields],
    DeleteInDefine = parse_define_delete_in(TableName, AutoIncrementKeys),

    %% truncate code
    TruncateDefine = parse_define_truncate(TableName, Modes),

    Define = lists:concat([InsertDefine, SelectDefine, UpdateDefine, DeleteDefine, InsertUpdateDefine, SelectJoinDefine, SelectGroupDefine, SelectJoinGroupDefine, UpdateGroupDefine, DeleteGroupDefine, DeleteInDefine, TruncateDefine]),

    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    %%%%%%%%%%%%%%%%%%%%%%%%%%%% Separator %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    %% insert code
    InsertArgs = chose_style(direct, Record, InsertFields),
    InsertCode = parse_code_insert(TableName, InsertArgs),

    %% select code
    %% filter varchar convert format fields
    SelectConvertFields = [Field || Field = #field{format = "'~w'"} <- FullFields],
    SelectCode = parse_code_select(TableName, SelectKeys, SelectConvertFields),

    %% update code
    UpdateFields = case [X || X = #field{extra = Extra} <- NormalFields, Extra =/= <<"auto_increment">>] of [] -> PrimaryFields; ThisUpdateFields -> ThisUpdateFields end,
    UpdateCodeFieldsArgs = chose_style(direct, Record, UpdateFields),
    UpdateCodeKeysArgs = chose_style(direct, Record, PrimaryFields),
    UpdateCode = parse_code_update(TableName, UpdateCodeKeysArgs, UpdateCodeFieldsArgs),

    %% delete code
    DeleteCode = parse_code_delete(TableName, PrimaryFields, []),

    %% insert update code
    InsertUpdateArgs = chose_style(direct, Record, StoreFields),
    InsertUpdateCode = [parse_code_insert_update(TableName, Record, InsertUpdateArgs, InsertUpdateFlag) || InsertUpdateFlag =/= []],

    %% select join code
    SelectJoinCode = [parse_code_select_join(TableName, SelectKeys, SelectConvertFields) || SelectJoinKeys =/= []],

    %% select (keys) group code
    SelectGroupCode = [parse_code_select_group(TableName, Keys, SelectConvertFields, Name) || {Name, Keys} <- SelectMergeGroupList],

    %% select join group code
    SelectJoinGroupCode = [parse_code_select_join_group(TableName, Keys, SelectConvertFields, Name) || {Name, Keys} <- SelectMergeGroupList, SelectJoinKeys =/= []],

    %% update (fields) group code
    UpdateGroupCode = [parse_code_update_group(TableName, Name, PrimaryFields, Fields) || {Name, Fields} <- UpdateMergeGroupList],

    %% delete (keys) group code
    DeleteGroupCode = [parse_code_delete_group(TableName, Name, Fields) || {Name, Fields} <- DeleteMergeGroupList],

    %% delete in code
    DeleteInCode = parse_code_delete_in(TableName, AutoIncrementKeys),

    %% truncate code
    TruncateCode = parse_code_truncate(TableName, TruncateDefine),

    %% collect all code
    Code = lists:concat([InsertCode, SelectCode, UpdateCode, DeleteCode, InsertUpdateCode, SelectJoinCode, SelectGroupCode, SelectJoinGroupCode, UpdateGroupCode, DeleteGroupCode, DeleteInCode, TruncateCode]),

    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    %%%%%%%%%%%%%%%%%%%%%%%%%%%% Separator %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    %% insert export
    InsertExport = lists:concat(["-export([insert/1]).\n"]),

    %% select export
    SelectExport = lists:concat(["-export([select/", length(SelectKeys), "]).\n"]),

    %% update export
    UpdateExport = lists:concat(["-export([update/1]).\n"]),

    %% delete export
    DeleteExport = lists:concat(["-export([delete/", length(PrimaryFields),"]).\n"]),

    %% insert update export
    InsertUpdateExport = [lists:concat(["-export([insert_update/1]).\n"]) || InsertUpdateFlag =/= []],

    %% select join export
    SelectJoinExport = [lists:concat(["-export([select_join/", length(SelectKeys), "]).\n"]) || SelectJoinKeys =/= []],

    %% select (keys) group export
    SelectGroupExport = [lists:concat(["-export([select_", Name, "/", length(Keys), "]).\n"]) || {Name, Keys} <- SelectMergeGroupList],

    %% select join group export
    SelectJoinGroupExport = [lists:concat(["-export([select_join_", Name, "/", length(Keys), "]).\n"]) || {Name, Keys} <- SelectMergeGroupList, SelectJoinKeys =/= []],

    %% update (fields) group export
    UpdateGroupExport = [lists:concat(["-export([update_", Name, "/", length(Fields) + length(PrimaryFields), "]).\n"]) || {Name, Fields} <- UpdateMergeGroupList],

    %% delete (keys) group export
    DeleteGroupExport = [lists:concat(["-export([delete_", Name, "/", length(Fields), "]).\n"]) || {Name, Fields} <- DeleteMergeGroupList],

    %% delete in export
    DeleteInExport = [lists:concat(["-export([delete_in_", Name, "/1]).\n"]) || #field{name = Name} <- AutoIncrementKeys],

    %% truncate export
    TruncateExport = [lists:concat(["-export([truncate/1]).\n"]) || TruncateCode =/= []],

    Export = [InsertExport, SelectExport, UpdateExport, DeleteExport, InsertUpdateExport, SelectJoinExport, SelectGroupExport, SelectJoinGroupExport, UpdateGroupExport, DeleteGroupExport, DeleteInExport, TruncateExport],

    [Export, Define, Code].

%%%===================================================================
%%% define part
%%%===================================================================
%% insert define
parse_define_insert(TableName, FullFields, StoreFields) ->
    %% field
    UpperTableName = string:to_upper(TableName),
    InsertFields = string:join(listing:collect_into(#field.name, StoreFields, fun(Name) -> lists:concat(["`", Name, "`"]) end), ", "),
    InsertFieldsFormat = join([case Extra of <<"auto_increment">> -> "~i"; _ -> Format end || #field{format = Format, extra = Extra} <- FullFields], ", "),
    %% insert single row data
    io_lib:format("-define(INSERT_~s, <<\"INSERT INTO `~s` (~s) VALUES (~s~s)\">>).\n", [UpperTableName, TableName, InsertFields, "~i", InsertFieldsFormat]).

%% select define
parse_define_select(TableName, [], Fields) ->
    parse_define_select(TableName, [], [], Fields);
parse_define_select(TableName, Keys, Fields) ->
    %% with select filter add where clause
    parse_define_select(TableName, " WHERE ", Keys, Fields).
parse_define_select(TableName, Where, Keys, Fields) ->
    UpperTableName = string:to_upper(TableName),
    %% field
    SelectFields = string:join([fun(#field{name = Name, default = Default, extra = <<"VIRTUAL", _/binary>>}) -> lists:concat([Default, " AS ", "`", Name, "`"]); (#field{name = Name}) -> lists:concat(["`", Name, "`"]) end(Field) || Field <- Fields], ", "),
    %% key
    SelectKeys = listing:collect(#field.name, Keys),
    SelectKeysFormat = listing:collect(#field.format, Keys),
    SelectKeysClause = string:join(lists:zipwith(fun(Key, Format) -> lists:concat(["`", Key, "`", " = ", Format]) end, SelectKeys, SelectKeysFormat), " AND "),
    %% select without key allow
    io_lib:format("-define(SELECT_~s, <<\"SELECT ~s FROM `~s`~s~s\">>).\n", [UpperTableName, SelectFields, TableName, Where, SelectKeysClause]).

%% update define
parse_define_update(TableName, FullFields, Keys) ->
    UpperTableName = string:to_upper(TableName),
    %% field empty use keys as fields
    UpdateFieldsClause = join(lists:map(fun(#field{format = "~i"}) -> "~i"; (#field{name = Name, format = Format}) -> case lists:keyfind(Name, #field.name, Keys) of false -> lists:concat(["`", Name, "`", " = ", Format]); _ -> "~i" end end, FullFields), ", "),
    %% key
    UpdateKeys = listing:collect(#field.name, Keys),
    UpdateKeysFormat = listing:collect(#field.format, Keys),
    UpdateKeysClause = string:join(lists:zipwith(fun(Key, Format) -> lists:concat(["`", Key, "`", " = ", Format]) end, UpdateKeys, UpdateKeysFormat), " AND "),
    %% update operation must be use key restrict
    %% where clause is necessary always
    io_lib:format("-define(UPDATE_~s, {<<\"UPDATE `~s` SET ~s~s \">>, <<\"WHERE ~s\">>}).\n", [UpperTableName, TableName, "~i", UpdateFieldsClause, UpdateKeysClause]).

%% delete define
parse_define_delete(TableName, Keys) ->
    UpperTableName = string:to_upper(TableName),
    %% key
    DeleteKeys = listing:collect(#field.name, Keys),
    DeleteKeysFormat = listing:collect(#field.format, Keys),
    DeleteKeysClause = string:join(lists:zipwith(fun(Key, Format) -> lists:concat(["`", Key, "`", " = ", Format]) end, DeleteKeys, DeleteKeysFormat), " AND "),
    %% delete operation must be use key restrict
    %% where clause is necessary always
    io_lib:format("-define(DELETE_~s, <<\"DELETE FROM `~s` WHERE ~s\">>).\n", [UpperTableName, TableName, DeleteKeysClause]).

%% insert update
parse_define_insert_update(_TableName, _FullFields, _FieldsInsert, _FieldsUpdate, []) ->
    %% no update flag, do not make insert update define
    [];
parse_define_insert_update(TableName, FullFields, FieldsInsert, FieldsUpdate, _Flag) ->
    %% insert field
    UpperTableName = string:to_upper(TableName),
    InsertFields = string:join(listing:collect_into(#field.name, FieldsInsert, fun(Name) -> lists:concat(["`", Name, "`"]) end), ", "),
    InsertFieldsFormat = join(listing:collect(#field.format, FullFields), ", "),
    %% update field (not include key)
    UpdateFieldsClause = string:join([lists:concat(["`", Name, "`", " = ", "VALUES(`", Name, "`)"]) || #field{name = Name} <- FieldsUpdate], ", "),
    %% split 3 part sql for parser use
    InsertDefine = io_lib:format("-define(INSERT_UPDATE_~s, {<<\"INSERT INTO `~s` (~s) VALUES \">>, ", [UpperTableName, TableName, InsertFields]),
    ValueDefine = io_lib:format("<<\"(~s~s)\">>", ["~i", InsertFieldsFormat]), %% add tag ignore
    UpdateDefine = io_lib:format(", <<\" ON DUPLICATE KEY UPDATE ~s\">>}).\n", [UpdateFieldsClause]),
    lists:concat([InsertDefine, ValueDefine, UpdateDefine]).

%% select join
parse_define_select_join(_TableName, _, [], _Fields) ->
    %% no join key, do not make select join define
    [];
parse_define_select_join(TableName, KeysFilter, Keys, Fields) ->
    %% default table name
    parse_define_select_join(TableName, KeysFilter, Keys, Fields, TableName).

parse_define_select_join(TableName, KeysFilter, Keys, Fields, Name) ->
    UpperName = string:to_upper(Name),
    SelectJoinFields = string:join(Fields, ", "),
    %% join key must add table name
    %% multi table join supported
    SelectJoinKeys = lists:append([lists:append(lists:zipwith(fun(Table, OuterField) -> lists:concat([" LEFT JOIN ", Table, " ON ", "`", TableName, "`", ".", "`", InnerField, "`", " = ", OuterField]) end, OuterTables, OuterFields)) || {OuterTables, OuterFields, #field{name = InnerField}} <- Keys]),
    %% select filter key must add table name
    SelectKeys = listing:collect(#field.name, KeysFilter),
    SelectKeysFormat = listing:collect(#field.format, KeysFilter),
    SelectKeysClause = string:join(lists:zipwith(fun(Key, Format) -> lists:concat(["`", TableName, "`", ".", "`", Key, "`", " = ", Format]) end, SelectKeys, SelectKeysFormat), " AND "),
    %% add where clause when with select filter
    Where = case KeysFilter of [] -> ""; _ -> " WHERE " end,
    %% select join key must primary and unique
    io_lib:format("-define(SELECT_JOIN_~s, <<\"SELECT ~s FROM `~s`~s~s~s\">>).\n", [UpperName, SelectJoinFields, TableName, SelectJoinKeys, Where, SelectKeysClause]).

%% update group define
parse_define_update_group(TableName, FieldName, Keys, Fields) ->
    UpperTableName = string:to_upper(FieldName),
    %% field empty use keys as fields
    UpdateFields = listing:collect(#field.name, Fields, Keys),
    UpdateFieldsFormat = listing:collect(#field.format, Fields, Keys),
    UpdateFieldsClause = string:join(lists:zipwith(fun(Field, Format) -> lists:concat(["`", Field, "`", " = ", Format]) end, UpdateFields, UpdateFieldsFormat), ", "),
    %% key
    UpdateKeys = listing:collect(#field.name, Keys),
    UpdateKeysFormat = listing:collect(#field.format, Keys),
    UpdateKeysClause = string:join(lists:zipwith(fun(Key, Format) -> lists:concat(["`", Key, "`", " = ", Format]) end, UpdateKeys, UpdateKeysFormat), " AND "),
    %% update operation must be use key restrict
    %% where clause is necessary always
    io_lib:format("-define(UPDATE_~s, <<\"UPDATE `~s` SET ~s WHERE ~s\">>).\n", [UpperTableName, TableName, UpdateFieldsClause, UpdateKeysClause]).

%% delete group define
parse_define_delete_group(TableName, FieldName, Keys, _Fields) ->
    UpperFieldName = string:to_upper(FieldName),
    %% field
    %% DeleteFields = string:join(listing:collect(#field.field, Fields), ", "),
    %% key
    DeleteKeys = listing:collect(#field.name, Keys),
    DeleteKeysFormat = listing:collect(#field.format, Keys),
    DeleteKeysClause = string:join(lists:zipwith(fun(Key, Format) -> lists:concat(["`", Key, "`", " = ", Format]) end, DeleteKeys, DeleteKeysFormat), " AND "),
    %% update must has key restrict
    %% where clause is necessary always
    io_lib:format("-define(DELETE_~s, <<\"DELETE FROM `~s` WHERE ~s\">>).\n", [UpperFieldName, TableName, DeleteKeysClause]).

%% select group define
parse_define_select_group(TableName, FieldName, Keys, Fields) ->
    UpperName = string:to_upper(FieldName),
    %% field
    SelectFields = string:join([fun(#field{name = Name, default = Default, extra = <<"VIRTUAL", _/binary>>}) -> lists:concat([Default, " AS ", "`", Name, "`"]); (#field{name = Name}) -> lists:concat(["`", Name, "`"]) end(Field) || Field <- Fields], ", "),
    %% key
    SelectKeys = listing:collect(#field.name, Keys),
    SelectKeysFormat = listing:collect(#field.format, Keys),
    SelectKeysClause = string:join(lists:zipwith(fun(Key, Format) -> lists:concat(["`", Key, "`", " = ", Format]) end, SelectKeys, SelectKeysFormat), " AND "),
    %% where clause is necessary always
    io_lib:format("-define(SELECT_~s, <<\"SELECT ~s FROM `~s` WHERE ~s\">>).\n", [UpperName, SelectFields, TableName, SelectKeysClause]).

%% delete in define
parse_define_delete_in(_TableName, []) ->
    %% no auto increment field, do not make define in define
    [];
parse_define_delete_in(TableName, [#field{name = FieldName, format = Format}]) ->
    UpperFieldName = string:to_upper(FieldName),
    %% update operation must be use key restrict
    %% where clause is necessary always
    io_lib:format("-define(DELETE_IN_~s, {<<\"DELETE FROM `~s` WHERE `~s` in (\">>, <<\"~s\">>, <<\")\">>}).\n", [UpperFieldName, TableName, FieldName, Format]).

%% truncate code
parse_define_truncate(TableName, Mode) ->
    case lists:keymember(truncate, 1, Mode) of
        true ->
            io_lib:format("-define(TRUNCATE, <<\"TRUNCATE TABLE `~s`\">>).\n", [TableName]);
        false ->
            []
    end.

%%%===================================================================
%%% code style part
%%%===================================================================
%% get arg directly (Record#record.field)
chose_style(direct, Record, Fields) ->
    string:join(listing:collect_into(#field.name, Fields, fun(Name) -> lists:concat([word:to_hump(Record), "#", Record, ".", Name]) end), ", ").

%% spec type format
format_spec(Fields) when is_list(Fields) ->
    string:join([format_spec(Field) || Field <- Fields], ", ");
format_spec(#field{name = Name, format = Format}) ->
    lists:concat([word:to_hump(Name), " :: ", case Format of "~w" -> "integer()"; "'~s'" -> "binary()"; _ -> "term()" end]).

%%%===================================================================
%%% code part
%%%===================================================================
%% insert codeN
parse_code_insert(TableName, _Fields) ->
    UpperName = string:to_upper(TableName),
    HumpName = word:to_hump(TableName),
    io_lib:format("\n%% @doc insert\n-spec insert(~s :: #~s{}) -> InsertIdOrAffectedRows :: non_neg_integer().\ninsert(~s) ->
    Sql = parser:format(?INSERT_~s, ~s),
    db:insert(Sql).\n\n", [HumpName, TableName, HumpName, UpperName, HumpName]).

%% select code
parse_code_select(TableName, Keys, []) ->
    UpperName = string:to_upper(TableName),
    HumpName = word:to_hump(TableName),
    KeysSpec = format_spec(Keys),
    KeysCode = string:join(listing:collect_into(#field.name, Keys, fun(Name) -> word:to_hump(Name) end), ", "),
    io_lib:format("%% @doc select\n-spec select(~s) -> ~sList :: [#~s{}].\nselect(~s) ->
    Sql = parser:format(?SELECT_~s, [~s]),
    Data = db:select(Sql),
    parser:convert(Data, ~s).\n\n", [KeysSpec, HumpName, TableName, KeysCode, UpperName, KeysCode, TableName]);
parse_code_select(TableName, Keys, ConvertFields) ->
    UpperName = string:to_upper(TableName),
    HumpName = word:to_hump(TableName),
    KeysSpec = format_spec(Keys),
    KeysCode = string:join(listing:collect_into(#field.name, Keys, fun(Name) -> word:to_hump(Name) end), ", "),
    MatchCode = string:join(listing:collect_into(#field.name, ConvertFields, fun(FieldName) -> lists:concat([FieldName, " = ", word:to_hump(FieldName)]) end), ", "),
    ConvertCode = string:join(listing:collect_into(#field.name, ConvertFields, fun(FieldName) -> lists:concat([FieldName, " = ", "parser:to_term(", word:to_hump(FieldName), ")"]) end), ", "),
    io_lib:format("%% @doc select\n-spec select(~s) -> ~sList :: [#~s{}].\nselect(~s) ->
    Sql = parser:format(?SELECT_~s, [~s]),
    Data = db:select(Sql),
    F = fun(~s = #~s{~s}) -> ~s#~s{~s} end,
    parser:convert(Data, ~s, F).\n\n", [KeysSpec, HumpName, TableName, KeysCode, UpperName, KeysCode, HumpName, TableName, MatchCode, HumpName, TableName, ConvertCode, TableName]).

%% update code
parse_code_update(TableName, Keys, _Fields) ->
    UpperName = string:to_upper(TableName),
    HumpName = word:to_hump(TableName),
    io_lib:format("%% @doc update\n-spec update(~s :: #~s{}) -> AffectedRows :: non_neg_integer().\nupdate(~s) ->
    Sql = <<(parser:format(element(1, ?UPDATE_~s), ~s))/binary, (parser:format(element(2, ?UPDATE_~s), [~s]))/binary>>,
    db:update(Sql).\n\n", [HumpName, TableName, HumpName, UpperName, HumpName, UpperName, Keys]).

%% delete code
parse_code_delete(TableName, Keys, Fields) ->
    UpperName = string:to_upper(TableName),
    KeysSpec = format_spec(Keys),
    KeysCode = string:join(listing:collect_into(#field.name, Keys, fun(Name) -> word:to_hump(Name) end), ", "),
    io_lib:format("%% @doc delete\n-spec delete(~s) -> AffectedRows :: non_neg_integer().\ndelete(~s) ->
    Sql = parser:format(?DELETE_~s, [~s]),
    db:delete(Sql).\n\n", [KeysSpec, KeysCode ++ Fields, UpperName, KeysCode ++ Fields]).

%% batch insert code (with the flag)
parse_code_insert_update(TableName, Record, _Fields, [Flag | _]) ->
    UpperName = string:to_upper(TableName),
    HumpName = word:to_hump(TableName),
    io_lib:format("\n%% @doc insert_update\n-spec insert_update(~sList :: [#~s{}] | ets:tab()) -> New~sList :: [#~s{}].\ninsert_update(~sList) ->
    {Sql, New~sList} = parser:collect_into(~sList, ?INSERT_UPDATE_~s, #~s.~s),
    db:insert(Sql),
    New~sList.\n\n", [HumpName, TableName, HumpName, TableName, HumpName, HumpName, HumpName, UpperName, Record, Flag, HumpName]).

%% select join other table
parse_code_select_join(TableName, ArgKeys, []) ->
    UpperName = string:to_upper(TableName),
    HumpName = word:to_hump(TableName),
    KeysSpec = format_spec(ArgKeys),
    ArgKeysCode = string:join(listing:collect_into(#field.name, ArgKeys, fun(Name) -> word:to_hump(Name) end), ", "),
    io_lib:format("%% @doc select join\n-spec select_join(~s) -> ~sList :: [#~s{}].\nselect_join(~s) ->
    Sql = parser:format(?SELECT_JOIN_~s, [~s]),
    Data = db:select(Sql),
    parser:convert(Data, ~s).\n\n", [KeysSpec, HumpName, TableName, ArgKeysCode, UpperName, ArgKeysCode, TableName]);
parse_code_select_join(TableName, ArgKeys, ConvertFields) ->
    UpperName = string:to_upper(TableName),
    HumpName = word:to_hump(TableName),
    KeysSpec = format_spec(ArgKeys),
    ArgKeysCode = string:join(listing:collect_into(#field.name, ArgKeys, fun(Name) -> word:to_hump(Name) end), ", "),
    MatchCode = string:join(listing:collect_into(#field.name, ConvertFields, fun(FieldName) -> lists:concat([FieldName, " = ", word:to_hump(FieldName)]) end), ", "),
    ConvertCode = string:join(listing:collect_into(#field.name, ConvertFields, fun(FieldName) -> lists:concat([FieldName, " = ", "parser:to_term(", word:to_hump(FieldName), ")"]) end), ", "),
    io_lib:format("%% @doc select join\n-spec select_join(~s) -> ~sList :: [#~s{}].\nselect_join(~s) ->
    Sql = parser:format(?SELECT_JOIN_~s, [~s]),
    Data = db:select(Sql),
    F = fun(~s = #~s{~s}) -> ~s#~s{~s} end,
    parser:convert(Data, ~s, F).\n\n", [KeysSpec, HumpName, TableName, ArgKeysCode, UpperName, ArgKeysCode, HumpName, TableName, MatchCode, HumpName, TableName, ConvertCode, TableName]).

parse_code_select_join_group(TableName, ArgKeys, [], Name) ->
    UpperName = string:to_upper(Name),
    HumpName = word:to_hump(TableName),
    KeysSpec = format_spec(ArgKeys),
    ArgKeysCode = string:join(listing:collect_into(#field.name, ArgKeys, fun(FieldName) -> word:to_hump(FieldName) end), ", "),
    io_lib:format("%% @doc select join\n-spec select_join_~s(~s) -> ~sList :: [#~s{}].\nselect_join_~s(~s) ->
    Sql = parser:format(?SELECT_JOIN_~s, [~s]),
    Data = db:select(Sql),
    parser:convert(Data, ~s).\n\n", [Name, KeysSpec, HumpName, TableName, Name, ArgKeysCode, UpperName, ArgKeysCode, TableName]);
parse_code_select_join_group(TableName, ArgKeys, ConvertFields, Name) ->
    UpperName = string:to_upper(Name),
    HumpName = word:to_hump(TableName),
    KeysSpec = format_spec(ArgKeys),
    ArgKeysCode = string:join(listing:collect_into(#field.name, ArgKeys, fun(FieldName) -> word:to_hump(FieldName) end), ", "),
    MatchCode = string:join(listing:collect_into(#field.name, ConvertFields, fun(FieldName) -> lists:concat([FieldName, " = ", word:to_hump(FieldName)]) end), ", "),
    ConvertCode = string:join(listing:collect_into(#field.name, ConvertFields, fun(FieldName) -> lists:concat([FieldName, " = ", "parser:to_term(", word:to_hump(FieldName), ")"]) end), ", "),
    io_lib:format("%% @doc select join\n-spec select_join_~s(~s) -> ~sList :: [#~s{}].\nselect_join_~s(~s) ->
    Sql = parser:format(?SELECT_JOIN_~s, [~s]),
    Data = db:select(Sql),
    F = fun(~s = #~s{~s}) -> ~s#~s{~s} end,
    parser:convert(Data, ~s, F).\n\n", [Name, KeysSpec, HumpName, TableName, Name, ArgKeysCode, UpperName, ArgKeysCode, HumpName, TableName, MatchCode, HumpName, TableName, ConvertCode, TableName]).

%% select group code
parse_code_select_group(TableName, Keys, [], Name) ->
    UpperName = string:to_upper(Name),
    HumpName = word:to_hump(TableName),
    KeysSpec = format_spec(Keys),
    KeysCode = string:join(listing:collect_into(#field.name, Keys, fun(FieldName) -> word:to_hump(FieldName) end), ", "),
    io_lib:format("%% @doc select\n-spec select_~s(~s) -> ~sList :: [#~s{}].\nselect_~s(~s) ->
    Sql = parser:format(?SELECT_~s, [~s]),
    Data = db:select(Sql),
    parser:convert(Data, ~s).\n\n", [Name, KeysSpec, HumpName, TableName, Name, KeysCode, UpperName, KeysCode, TableName]);
parse_code_select_group(TableName, Keys, ConvertFields, Name) ->
    UpperName = string:to_upper(Name),
    HumpName = word:to_hump(TableName),
    KeysSpec = format_spec(Keys),
    KeysCode = string:join(listing:collect_into(#field.name, Keys, fun(FieldName) -> word:to_hump(FieldName) end), ", "),
    MatchCode = string:join(listing:collect_into(#field.name, ConvertFields, fun(FieldName) -> lists:concat([FieldName, " = ", word:to_hump(FieldName)]) end), ", "),
    ConvertCode = string:join(listing:collect_into(#field.name, ConvertFields, fun(FieldName) -> lists:concat([FieldName, " = ", "parser:to_term(", word:to_hump(FieldName), ")"]) end), ", "),
    io_lib:format("%% @doc select\n-spec select_~s(~s) -> ~sList :: [#~s{}].\nselect_~s(~s) ->
    Sql = parser:format(?SELECT_~s, [~s]),
    Data = db:select(Sql),
    F = fun(~s = #~s{~s}) -> ~s#~s{~s} end,
    parser:convert(Data, ~s, F).\n\n", [Name, KeysSpec, HumpName, TableName, Name, KeysCode, UpperName, KeysCode, HumpName, TableName, MatchCode, HumpName, TableName, ConvertCode, TableName]).

%% update group code
parse_code_update_group(_TableName, Name, PrimaryFields, Fields) ->
    ReviseNameFields = [Field#field{name = lists:concat(["update", "_", FieldName])} || Field = #field{name = FieldName} <- Fields],
    KeysSpec = format_spec(lists:append(ReviseNameFields, PrimaryFields)),
    FieldsNames = listing:collect_into(#field.name, ReviseNameFields, fun(FieldName) -> word:to_hump(FieldName) end),
    PrimaryFieldsNames = listing:collect_into(#field.name, PrimaryFields, fun(FieldName) -> word:to_hump(FieldName) end),
    FieldsCode = string:join(lists:append(FieldsNames, PrimaryFieldsNames), ", "),
    UpperName = string:to_upper(Name),
    io_lib:format("%% @doc update\n-spec update_~s(~s) -> non_neg_integer().\nupdate_~s(~s) ->
    Sql = parser:format(?UPDATE_~s, [~s]),
    db:update(Sql).\n\n", [Name, KeysSpec, Name, FieldsCode, UpperName, FieldsCode]).

%% delete group code
parse_code_delete_group(_TableName, Name, Fields) ->
    UpperName = string:to_upper(Name),
    FieldsSpec = format_spec(Fields),
    FieldsCode = string:join(listing:collect_into(#field.name, Fields, fun(FieldName) -> word:to_hump(FieldName) end), ", "),
    io_lib:format("%% @doc delete\n-spec delete_~s(~s) -> AffectedRows :: non_neg_integer().\ndelete_~s(~s) ->
    Sql = parser:format(?DELETE_~s, [~s]),
    db:delete(Sql).\n\n", [Name, FieldsSpec, Name, FieldsCode, UpperName, FieldsCode]).

%% delete in code
parse_code_delete_in(_TableName, []) ->
    %% no auto increment field, do not make define in code
    [];
parse_code_delete_in(_TableName, [Field = #field{name = FieldName}]) ->
    UpperName = string:to_upper(FieldName),
    HumpName = word:to_hump(FieldName),
    FieldSpec = format_spec([Field]),
    io_lib:format("%% @doc delete\n-spec delete_in_~s(~sList :: [~s]) -> AffectedRows :: non_neg_integer().\ndelete_in_~s(~sList) ->
    Sql = parser:collect(~sList, ?DELETE_IN_~s),
    db:delete(Sql).\n\n", [FieldName, HumpName, FieldSpec, FieldName, HumpName, HumpName, UpperName]).

%% truncate code
parse_code_truncate(_TableName, []) ->
    [];
parse_code_truncate(_TableName, _Code) ->
    io_lib:format("%% @doc truncate\n-spec truncate() -> non_neg_integer().\ntruncate() ->
    Sql = parser:format(?TRUNCATE, []),
    db:query(Sql).\n\n", []).

%%%===================================================================
%%% Common Tool
%%%===================================================================
%% contain
contain(Content, What) ->
    string:str(Content, What) =/= 0.

%% extract
extract(Content, Match) ->
    extract(Content, Match, []).
extract(Content, Match, Default) ->
    extract(Content, Match, Default, [global, {capture, all, list}]).
extract(Content, Match, Default, Option) ->
    case re:run(Content, Match, Option) of
        {match, Result} ->
            lists:usort(lists:append(Result));
        _ ->
            Default
    end.

%% join with separator ignore
join(List, Separator) ->
    join_with_loop(List, Separator, "~i", "").
join_with_loop([], _Separator, _Ignore, String) ->
    String;
join_with_loop([H = Ignore | T], Separator, Ignore, String) ->
    join_with_loop(T, Separator, Ignore, lists:concat([String, H]));
join_with_loop([H | T], Separator, Ignore, String) ->
    case lists:any(fun(Item) -> Item =/= Ignore end, T) of
        true ->
            join_with_loop(T, Separator, Ignore, lists:concat([String, H, Separator]));
        false ->
            join_with_loop(T, Separator, Ignore, lists:concat([String, H]))
    end.

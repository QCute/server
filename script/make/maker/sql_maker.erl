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
    Fields = parser:convert(db:select(<<"SELECT `COLUMN_NAME`, IF(`EXTRA` = 'auto_increment', 0, IF(`COLUMN_DEFAULT` != 'NULL', `COLUMN_DEFAULT`, `GENERATION_EXPRESSION`)) AS `COLUMN_DEFAULT`, `COLUMN_TYPE`, CASE WHEN `EXTRA` = 'VIRTUAL GENERATED' THEN '~~i' WHEN `DATA_TYPE` = 'char' THEN '\\'~~s\\'' WHEN `DATA_TYPE` = 'varchar' THEN '\\'~~w\\'' ELSE '~~w' END AS `DATA_TYPE`, `COLUMN_COMMENT`, `ORDINAL_POSITION`, `COLUMN_KEY`, `EXTRA` FROM information_schema.`COLUMNS` WHERE `TABLE_SCHEMA` = DATABASE() AND `TABLE_NAME` = '~s' ORDER BY `ORDINAL_POSITION`;">>, [Table]), field, Revise),
    %% erlang:display(Revise),
    %% primary key fields
    PrimaryFields = [X || X = #field{key = <<"PRI">>} <- Fields],
    NormalFields = [X || X = #field{key = Key, extra = <<>>} <- Fields, Key =/= <<"PRI">>],
    EmptyFields = [X || X = #field{extra = <<"VIRTUAL GENERATED">>} <- Fields],
    %% no primary key, could not do anything
    PrimaryFields == [] andalso erlang:throw(lists:flatten(io_lib:format("could not found any primary key in table: ~s", [Table]))),
    %% return data
    Head = parse_head(File, Includes),
    Code = parse_code(type:to_list(Table), type:to_list(Table), Fields, PrimaryFields ++ NormalFields, PrimaryFields, NormalFields, EmptyFields, Modes),
    [{"(?s).*", Head ++ Code}].

%% sql code head
parse_head(File, Includes) ->
    %% base file name is module name
    Module = filename:basename(File, ".erl"),
    %% head
    Head = io_lib:format("-module(~s).\n-compile(nowarn_export_all).\n-compile(export_all).\n", [Module]),
    %% include
    Include = [lists:flatten(io_lib:format("-include(\"~s\").\n", [X])) || X <- Includes],
    %% collect head and include
    lists:concat([Head, Include]).

%% parse all code
parse_code(TableName, Record, FullFields, StoreFields, PrimaryFields, NormalFields, EmptyFields, Modes) ->

    %% insert define part
    InsertFields = [X || X = #field{extra = Extra} <- lists:keysort(#field.position, StoreFields), Extra =/= <<"auto_increment">>],
    InsertDefine = parse_define_insert(TableName, FullFields, InsertFields),

    %% select table key
    SelectKeys = case lists:keyfind(select, 1, Modes) of {select, []} -> []; false -> PrimaryFields end,
    SelectDefine = parse_define_select(TableName, SelectKeys, FullFields),

    %% update define part, no update, primary key as update key by default
    UpdateFields = case [X || X = #field{extra = Extra} <- NormalFields, Extra =/= <<"auto_increment">>] of [] -> PrimaryFields; ThisUpdateFields -> ThisUpdateFields end,
    UpdateDefine = parse_define_update(TableName, FullFields, PrimaryFields),

    %% delete define part, no delete, primary key as delete key by default
    DeleteDefine = parse_define_delete(TableName, PrimaryFields, []),

    %% insert update
    InsertUpdateFields = lists:keysort(#field.position, StoreFields),
    InsertUpdateFlag = [Name || #field{name = Name, comment = Comment} <- lists:keysort(#field.position, FullFields), contain(Comment, "(flag)")],
    InsertUpdateDefine = parse_define_insert_update(TableName, FullFields, InsertUpdateFields, NormalFields, InsertUpdateFlag),

    %% select join
    SelectJoinKeys = parse_select_join_keys(StoreFields),
    SelectJoinFields = parse_select_join_fields(FullFields, EmptyFields, TableName),
    SelectJoinDefine = parse_define_select_join(TableName, SelectKeys, SelectJoinKeys, SelectJoinFields),

    %% select (keys) group
    SelectGroupFields = ([{X, extract(Comment, "(?<=\\(select_)\\w+(?=\\))")} || X = #field{comment = Comment} <- lists:keysort(#field.position, StoreFields)]),
    SelectGroupList = lists:append([[{Group, Field} || Group <- Groups] || {Field, Groups} <- SelectGroupFields]),
    SelectMergeGroupList = lists:reverse(listing:key_merge(1, SelectGroupList, fun({_, Field}, {_, List}) -> [Field | List] end)),
    SelectGroupDefine = [parse_define_select_group(TableName, FieldName, Fields, FullFields) || {FieldName, Fields} <- SelectMergeGroupList],
    SelectJoinGroupDefine = [parse_define_select_join(TableName, Fields, SelectJoinKeys, SelectJoinFields, FieldName) || {FieldName, Fields} <- SelectMergeGroupList],

    %% update (fields) group
    UpdateGroupFields = ([{X, extract(Comment, "(?<=\\(update_)\\w+(?=\\))")} || X = #field{comment = Comment} <- lists:keysort(#field.position, StoreFields)]),
    UpdateGroupList = lists:append([[{Group, Field} || Group <- Groups] || {Field, Groups} <- UpdateGroupFields]),
    UpdateMergeGroupList = lists:reverse(listing:key_merge(1, UpdateGroupList, fun({_, Field}, {_, List}) -> [Field | List] end)),
    UpdateGroupDefine = [parse_define_update_group(TableName, FieldName, PrimaryFields, Fields) || {FieldName, Fields} <- UpdateMergeGroupList],

    %% delete (keys) group
    DeleteGroupFields = ([{X, extract(Comment, "(?<=\\(delete_)\\w+(?=\\))")} || X = #field{comment = Comment} <- lists:keysort(#field.position, StoreFields)]),
    DeleteGroupList = lists:append([[{Group, Field} || Group <- Groups] || {Field, Groups} <- DeleteGroupFields]),
    DeleteMergeGroupList = lists:reverse(listing:key_merge(1, DeleteGroupList, fun({_, Field}, {_, List}) -> [Field | List] end)),
    DeleteGroupDefine = [parse_define_delete_group(TableName, FieldName, Fields, []) || {FieldName, Fields} <- DeleteMergeGroupList],

    %% delete in
    AutoIncrementKeys = [X || X = #field{extra = <<"auto_increment">>} <- lists:keysort(#field.position, StoreFields)],
    DeleteInDefine = parse_define_delete_in(TableName, AutoIncrementKeys, []),

    %% truncate code
    TruncateDefine = parse_define_truncate(TableName, Modes),

    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    %%%%%%%%%%%%%%%%%%%%%%%%%%%% Separator %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    %% insert code
    InsertArgs = chose_style(direct, Record, [], InsertFields),
    InsertCode = parse_code_insert(TableName, InsertArgs),

    %% select code
    SelectCodeKeysArgs = string:join(listing:collect_into(#field.name, SelectKeys, fun(Name) -> word:to_hump(Name) end), ", "),
    SelectConvertFields = [Field || Field = #field{format = "'~w'"} <- FullFields],
    SelectCode = parse_code_select(TableName, SelectCodeKeysArgs, SelectConvertFields),

    %% update code
    UpdateCodeFieldsArgs = chose_style(direct, Record, UpdateFields, []),
    UpdateCodeKeysArgs = chose_style(direct, Record, PrimaryFields, []),
    UpdateCode = parse_code_update(TableName, UpdateCodeKeysArgs, UpdateCodeFieldsArgs),

    %% delete code
    DeleteCodeKeyArgs = string:join(listing:collect_into(#field.name, PrimaryFields, fun(Name) -> word:to_hump(Name) end), ", "),
    DeleteCode = parse_code_delete(TableName, DeleteCodeKeyArgs, []),

    %% insert update code
    InsertUpdateArgs = chose_style(direct, Record, [], InsertUpdateFields),
    InsertUpdateCode = parse_code_insert_update(TableName, Record, InsertUpdateArgs, InsertUpdateFlag),

    %% select join code
    SelectJoinCode = parse_code_select_join(TableName, SelectCodeKeysArgs, SelectJoinKeys, SelectJoinFields, SelectConvertFields),

    %% select (keys) group code
    SelectGroupCode = [parse_code_select_group(TableName, string:join(listing:collect_into(#field.name, Fields, fun(FieldName) -> word:to_hump(FieldName) end), ", "), SelectConvertFields, Name) || {Name, Fields} <- SelectMergeGroupList],

    %% select join group code
    SelectJoinGroupCode = [parse_code_select_join_group(TableName, string:join(listing:collect_into(#field.name, Fields, fun(FieldName) -> word:to_hump(FieldName) end), ", "), SelectJoinKeys, SelectJoinFields, SelectConvertFields, Name) || {Name, Fields} <- SelectMergeGroupList],

    %% update (fields) group code
    UpdateGroupCode = [parse_code_update_group(TableName, Name, string:join(listing:collect_into(#field.name, Fields, fun(FieldName) -> "This" ++ word:to_hump(FieldName) end) ++ listing:collect_into(#field.name, PrimaryFields, fun(FieldName) -> word:to_hump(FieldName) end), ", ")) || {Name, Fields} <- UpdateMergeGroupList],

    %% delete (keys) group code
    DeleteGroupCode = [parse_code_delete_group(TableName, Name, string:join(listing:collect_into(#field.name, Fields, fun(FieldName) -> word:to_hump(FieldName) end), ", ")) || {Name, Fields} <- DeleteMergeGroupList],

    %% delete in code
    DeleteInCodeKeys = listing:collect(#field.name, AutoIncrementKeys),
    DeleteInCode = parse_code_delete_in(TableName, DeleteInCodeKeys),

    %% truncate code
    TruncateCode = parse_code_truncate(TableName, TruncateDefine),

    %% collect all code
    lists:concat([InsertDefine, SelectDefine, UpdateDefine, DeleteDefine, InsertUpdateDefine, SelectJoinDefine, SelectGroupDefine, SelectJoinGroupDefine, UpdateGroupDefine, DeleteGroupDefine, DeleteInDefine, TruncateDefine, InsertCode, SelectCode, UpdateCode, DeleteCode, InsertUpdateCode, SelectJoinCode, SelectGroupCode, SelectJoinGroupCode, UpdateGroupCode, DeleteGroupCode, DeleteInCode, TruncateCode]).

%%%===================================================================
%%% key/field part
%%%===================================================================
parse_select_join_keys(StoreFields) ->
    [
        %% extract table and field
        {extract(Comment, "(?<=join_on\\()`?\\w+`?(?=\\.)"), extract(Comment, "(?<=join_on\\()(`?\\w+`?\\.`?\\w+`?)(?=\\))"), FieldInfo}
        ||
        FieldInfo = #field{comment = Comment} <- lists:keysort(#field.position, StoreFields),
        %% contain join spec
        extract(Comment, "(?<=join_on\\()`?\\w+`?(?=\\.)") =/= []
    ].

parse_select_join_fields(FullFields, EmptyFields, TableName) ->
    [
        case lists:keymember(Name, #field.name, EmptyFields) of
            false ->
                %% not empty field, select inner table field
                {false, lists:concat(["`", TableName, "`", ".", "`", Name, "`"]), FieldInfo};
            true ->
                %% is empty field, join outer table field
                {true, extract(Comment, "(?<=join\\()(`?\\w+`?\\.`?\\w+`?)(?=\\))", lists:concat(["`", TableName, "`", ".", "`", Name, "`"])), FieldInfo}
        end
        ||
        FieldInfo = #field{name = Name, comment = Comment} <- lists:keysort(#field.position, FullFields)
    ].

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
    Revise = fun(#field{name = Name, extra = Extra}) when Extra =/= <<"VIRTUAL GENERATED">> -> lists:concat(["`", Name, "`"]); (#field{name = Name, default = Default}) -> lists:concat([Default, " AS ", "`", Name, "`"]) end,
    SelectFields = string:join([Revise(Field) || Field <- Fields], ", "),
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
parse_define_delete(TableName, Keys, Fields) ->
    UpperTableName = string:to_upper(TableName),
    %% field
    DeleteFields = string:join(listing:collect_into(#field.name, Fields, fun(Name) -> lists:concat(["`", Name, "`"]) end), ", "),
    %% key
    DeleteKeys = listing:collect(#field.name, Keys),
    DeleteKeysFormat = listing:collect(#field.format, Keys),
    DeleteKeysClause = string:join(lists:zipwith(fun(Key, Format) -> lists:concat(["`", Key, "`", " = ", Format]) end, DeleteKeys, DeleteKeysFormat), " AND "),
    %% delete operation must be use key restrict
    %% where clause is necessary always
    io_lib:format("-define(DELETE_~s, <<\"DELETE ~s FROM `~s` WHERE ~s\">>).\n", [UpperTableName, DeleteFields, TableName, DeleteKeysClause]).

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
    %% join key field use inner table name field
    %% join field must add table name
    %% type revise IF_NULL/AS(name alias)
    Revise = fun({true, OuterField, #field{name = InnerField, default = Default}}) -> lists:concat(["IFNULL(", OuterField, ", ", Default, ") AS ", "`", InnerField, "`"]); ({false, OuterField, _}) -> OuterField end,
    %% SelectJoinFields = string:join(listing:collect_into(1, Fields, Revise), ", "),
    SelectJoinFields = string:join([Revise(Field) || Field <- Fields], ", "),
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
    Revise = fun(#field{name = Name, extra = Extra}) when Extra =/= <<"VIRTUAL GENERATED">> -> lists:concat(["`", Name, "`"]); (#field{name = Name, default = Default}) -> lists:concat([Default, " AS ", "`", Name, "`"]) end,
    SelectFields = string:join([Revise(Field) || Field <- Fields], ", "),
    %% key
    SelectKeys = listing:collect(#field.name, Keys),
    SelectKeysFormat = listing:collect(#field.format, Keys),
    SelectKeysClause = string:join(lists:zipwith(fun(Key, Format) -> lists:concat(["`", Key, "`", " = ", Format]) end, SelectKeys, SelectKeysFormat), " AND "),
    %% where clause is necessary always
    io_lib:format("-define(SELECT_~s, <<\"SELECT ~s FROM `~s` WHERE ~s\">>).\n", [UpperName, SelectFields, TableName, SelectKeysClause]).

%% delete in define
parse_define_delete_in(_TableName, [], _Fields) ->
    %% no auto increment field, do not make define in define
    [];
parse_define_delete_in(TableName, [#field{name = FieldName, format = Format}], Fields) ->
    UpperFieldName = string:to_upper(FieldName),
    %% field
    DeleteFields = string:join(listing:collect_into(#field.name, Fields, fun(Name) -> lists:concat(["`", Name, "`"]) end), ", "),
    %% update operation must be use key restrict
    %% where clause is necessary always
    io_lib:format("-define(DELETE_IN_~s, {<<\"DELETE ~s FROM `~s` WHERE `~s` in (\">>, <<\"~s\">>, <<\")\">>}).\n", [UpperFieldName, DeleteFields, TableName, FieldName, Format]).

%% truncate code
parse_define_truncate(TableName, Mode) ->
    case lists:member(truncate, Mode) of
        true ->
            io_lib:format("-define(TRUNCATE, <<\"TRUNCATE TABLE `~s`\">>).\n", [TableName]);
        false ->
            []
    end.

%%%===================================================================
%%% code style part
%%%===================================================================
%% code style
chose_style(direct, Record, Keys, Fields) ->
    parse_code_fields_style_direct(Record, Keys, Fields).

%% get arg directly (Record#record.field)
parse_code_fields_style_direct(Record, Keys, Fields) ->
    %% lists:concat(["\n        ", string:join(listing:collect_into(#field.name, Keys ++ Fields, fun(Name) -> lists:concat([word:to_hump(Record), "#", Record, ".", Name]) end), ",\n        "), "\n    "]).
    string:join(listing:collect_into(#field.name, Keys ++ Fields, fun(Name) -> lists:concat([word:to_hump(Record), "#", Record, ".", Name]) end), ", ").

%%%===================================================================
%%% code part
%%%===================================================================
%% insert codeN
parse_code_insert(TableName, _Fields) ->
    UpperName = string:to_upper(TableName),
    HumpName = word:to_hump(TableName),
    io_lib:format("\n%% @doc insert\ninsert(~s) ->
    Sql = parser:format(?INSERT_~s, ~s),
    db:insert(Sql).\n\n", [HumpName, UpperName, HumpName]).

%% select code
parse_code_select(TableName, Keys, []) ->
    UpperName = string:to_upper(TableName),
    io_lib:format("%% @doc select\nselect(~s) ->
    Sql = parser:format(?SELECT_~s, [~s]),
    Data = db:select(Sql),
    parser:convert(Data, ~s).\n\n", [Keys, UpperName, Keys, TableName]);
parse_code_select(TableName, Keys, ConvertFields) ->
    UpperName = string:to_upper(TableName),
    HumpName = word:to_hump(TableName),
    MatchCode = string:join(listing:collect_into(#field.name, ConvertFields, fun(FieldName) -> lists:concat([FieldName, " = ", word:to_hump(FieldName)]) end), ", "),
    ConvertCode = string:join(listing:collect_into(#field.name, ConvertFields, fun(FieldName) -> lists:concat([FieldName, " = ", "parser:to_term(", word:to_hump(FieldName), ")"]) end), ", "),
    io_lib:format("%% @doc select\nselect(~s) ->
    Sql = parser:format(?SELECT_~s, [~s]),
    Data = db:select(Sql),
    F = fun(~s = #~s{~s}) -> ~s#~s{~s} end,
    parser:convert(Data, ~s, F).\n\n", [Keys, UpperName, Keys, HumpName, TableName, MatchCode, HumpName, TableName, ConvertCode, TableName]).

%% update code
parse_code_update(TableName, Keys, _Fields) ->
    UpperName = string:to_upper(TableName),
    HumpName = word:to_hump(TableName),
    io_lib:format("%% @doc update\nupdate(~s) ->
    Sql = <<(parser:format(element(1, ?UPDATE_~s), ~s))/binary, (parser:format(element(2, ?UPDATE_~s), [~s]))/binary>>,
    db:update(Sql).\n\n", [HumpName, UpperName, HumpName, UpperName, Keys]).

%% delete code
parse_code_delete(TableName, Keys, Fields) ->
    UpperName = string:to_upper(TableName),
    io_lib:format("%% @doc delete\ndelete(~s) ->
    Sql = parser:format(?DELETE_~s, [~s]),
    db:delete(Sql).\n\n", [Keys ++ Fields, UpperName, Keys ++ Fields]).

%% batch insert code (with the flag)
parse_code_insert_update(_TableName, _, _Fields, []) ->
    %% no update flag, do not make insert update code
    [];
parse_code_insert_update(TableName, Record, _Fields, [Flag | _]) ->
    UpperName = string:to_upper(TableName),
    %% HumpName = word:to_hump(TableName),
    io_lib:format("\n%% @doc insert_update\ninsert_update(Data) ->
    {Sql, NewData} = parser:collect_into(Data, ?INSERT_UPDATE_~s, #~s.~s),
    db:insert(Sql),
    NewData.\n\n", [UpperName, Record, Flag]).

%% select join other table
parse_code_select_join(_TableName, _, [], _, _) ->
    %% no join key, do not make select join code
    [];
parse_code_select_join(_TableName, _, _, [], _) ->
    %% no join fields, do not make select join code
    [];
parse_code_select_join(TableName, ArgKeys, _, _Fields, []) ->
    UpperName = string:to_upper(TableName),
    io_lib:format("%% @doc select join\nselect_join(~s) ->
    Sql = parser:format(?SELECT_JOIN_~s, [~s]),
    Data = db:select(Sql),
    parser:convert(Data, ~s).\n\n", [ArgKeys, UpperName, ArgKeys, TableName]);
parse_code_select_join(TableName, ArgKeys, _, _Fields, ConvertFields) ->
    UpperName = string:to_upper(TableName),
    HumpName = word:to_hump(TableName),
    MatchCode = string:join(listing:collect_into(#field.name, ConvertFields, fun(FieldName) -> lists:concat([FieldName, " = ", word:to_hump(FieldName)]) end), ", "),
    ConvertCode = string:join(listing:collect_into(#field.name, ConvertFields, fun(FieldName) -> lists:concat([FieldName, " = ", "parser:to_term(", word:to_hump(FieldName), ")"]) end), ", "),
    io_lib:format("%% @doc select join\nselect_join(~s) ->
    Sql = parser:format(?SELECT_JOIN_~s, [~s]),
    Data = db:select(Sql),
    F = fun(~s = #~s{~s}) -> ~s#~s{~s} end,
    parser:convert(Data, ~s, F).\n\n", [ArgKeys, UpperName, ArgKeys, HumpName, TableName, MatchCode, HumpName, TableName, ConvertCode, TableName]).

parse_code_select_join_group(_TableName, _, [], _, _, _) ->
    %% no join key, do not make select join code
    [];
parse_code_select_join_group(_TableName, _, _, [], _, _) ->
    %% no join fields, do not make select join code
    [];
parse_code_select_join_group(TableName, ArgKeys, _Keys, _Fields, [], Name) ->
    UpperName = string:to_upper(Name),
    io_lib:format("%% @doc select join\nselect_join_~s(~s) ->
    Sql = parser:format(?SELECT_JOIN_~s, [~s]),
    Data = db:select(Sql),
    parser:convert(Data, ~s).\n\n", [Name, ArgKeys, UpperName, ArgKeys, TableName]);
parse_code_select_join_group(TableName, ArgKeys, _Keys, _Fields, ConvertFields, Name) ->
    UpperName = string:to_upper(Name),
    HumpName = word:to_hump(TableName),
    MatchCode = string:join(listing:collect_into(#field.name, ConvertFields, fun(FieldName) -> lists:concat([FieldName, " = ", word:to_hump(FieldName)]) end), ", "),
    ConvertCode = string:join(listing:collect_into(#field.name, ConvertFields, fun(FieldName) -> lists:concat([FieldName, " = ", "parser:to_term(", word:to_hump(FieldName), ")"]) end), ", "),
    io_lib:format("%% @doc select join\nselect_join_~s(~s) ->
    Sql = parser:format(?SELECT_JOIN_~s, [~s]),
    Data = db:select(Sql),
    F = fun(~s = #~s{~s}) -> ~s#~s{~s} end,
    parser:convert(Data, ~s, F).\n\n", [Name, ArgKeys, UpperName, ArgKeys, HumpName, TableName, MatchCode, HumpName, TableName, ConvertCode, TableName]).

%% select group code
parse_code_select_group(TableName, Keys, [], Name) ->
    UpperName = string:to_upper(Name),
    io_lib:format("%% @doc select\nselect_~s(~s) ->
    Sql = parser:format(?SELECT_~s, [~s]),
    Data = db:select(Sql),
    parser:convert(Data, ~s).\n\n", [Name, Keys, UpperName, Keys, TableName]);
parse_code_select_group(TableName, Keys, ConvertFields, Name) ->
    UpperName = string:to_upper(Name),
    HumpName = word:to_hump(TableName),
    MatchCode = string:join(listing:collect_into(#field.name, ConvertFields, fun(FieldName) -> lists:concat([FieldName, " = ", word:to_hump(FieldName)]) end), ", "),
    ConvertCode = string:join(listing:collect_into(#field.name, ConvertFields, fun(FieldName) -> lists:concat([FieldName, " = ", "parser:to_term(", word:to_hump(FieldName), ")"]) end), ", "),
    io_lib:format("%% @doc select\nselect_~s(~s) ->
    Sql = parser:format(?SELECT_~s, [~s]),
    Data = db:select(Sql),
    F = fun(~s = #~s{~s}) -> ~s#~s{~s} end,
    parser:convert(Data, ~s, F).\n\n", [Name, Keys, UpperName, Keys, HumpName, TableName, MatchCode, HumpName, TableName, ConvertCode, TableName]).

%% update group code
parse_code_update_group(_TableName, Name, Fields) ->
    UpperName = string:to_upper(Name),
    io_lib:format("%% @doc update\nupdate_~s(~s) ->
    Sql = parser:format(?UPDATE_~s, [~s]),
    db:update(Sql).\n\n", [Name, Fields, UpperName, Fields]).

%% delete group code
parse_code_delete_group(_TableName, Name, Fields) ->
    UpperName = string:to_upper(Name),
    io_lib:format("%% @doc delete\ndelete_~s(~s) ->
    Sql = parser:format(?DELETE_~s, [~s]),
    db:delete(Sql).\n\n", [Name, Fields, UpperName, Fields]).

%% delete in code
parse_code_delete_in(_TableName, []) ->
    %% no auto increment field, do not make define in code
    [];
parse_code_delete_in(_TableName, [FieldName]) ->
    UpperName = string:to_upper(FieldName),
    HumpName = word:to_hump(FieldName),
    io_lib:format("%% @doc delete\ndelete_in_~s(~sList) ->
    Sql = parser:collect(~sList, ?DELETE_IN_~s),
    db:delete(Sql).\n\n", [FieldName, HumpName, HumpName, UpperName]).

%% truncate code
parse_code_truncate(_TableName, []) ->
    [];
parse_code_truncate(_TableName, _Code) ->
    io_lib:format("%% @doc truncate\ntruncate() ->
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

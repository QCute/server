%%%-------------------------------------------------------------------
%%% @doc
%%% module sql maker
%%% database fields to sql code tool
%%% @end
%%%-------------------------------------------------------------------
-module(sql_maker).
-export([start/1]).
-record(field, {name = [], default = [], type = [], comment = [], position = 0, key = [], extra = [], expression = []}).
%%%===================================================================
%%% API functions
%%%===================================================================
%% @doc for shell
start(List) ->
    maker:start(fun parse_table/2, List).

%%%===================================================================
%%% Internal functions
%%%===================================================================
%% parse per table
parse_table(DataBase, {File, Table, Includes}) ->
    parse_table(DataBase, {File, Table, Table, Includes, []});
parse_table(DataBase, {File, Table, Includes, Modes}) ->
    parse_table(DataBase, {File, Table, Table, Includes, Modes});
parse_table(DataBase, {File, Table, Record, Includes, Modes}) ->
    %% make fields sql
    FieldsSql = io_lib:format(<<"SELECT `COLUMN_NAME`, `COLUMN_DEFAULT`, `DATA_TYPE`, `COLUMN_COMMENT`, `ORDINAL_POSITION`, `COLUMN_KEY`, `EXTRA`, `GENERATION_EXPRESSION` FROM information_schema.`COLUMNS` WHERE `TABLE_SCHEMA` = '~s' AND `TABLE_NAME` = '~s' ORDER BY `ORDINAL_POSITION`;">>, [DataBase, Table]),
    %% data revise
    Revise = fun
        (FieldInfo = #field{name = Name, type = <<"char">>, comment = Comment}) ->
            FieldInfo#field{name = binary_to_list(Name), type = "'~s'", default = "''", comment = binary_to_list(Comment)};
        %% (FieldInfo = #field{name = Name, type = <<"varchar(0)">>, comment = Comment}) ->
        %%     Default = hd(tool:default(extract(Comment, "(?<=default\\().*?(?=\\))"), ["0"])),
        %%     FieldInfo#field{name = binary_to_list(Name), format = "'~s'", default = Default, comment = binary_to_list(Comment)};
        (FieldInfo = #field{name = Name, type = <<"varchar">>, comment = Comment}) ->
            FieldInfo#field{name = binary_to_list(Name), type = "'~w'", default = "''", comment = binary_to_list(Comment)};
        (FieldInfo = #field{name = Name, comment = Comment}) ->
            FieldInfo#field{name = binary_to_list(Name), type = "~w", default = "0", comment = binary_to_list(Comment)}
    end,
    %% fetch table fields
    Fields = parser:convert(maker:select(FieldsSql), field, Revise),
    %% primary key fields
    PrimaryFields = [X || X = #field{key = <<"PRI">>} <- Fields],
    %% ValidateFields = [X || X = #field{key = Key, type = Type, expression = Expression} <- Fields, Key =/= <<"PRI">> andalso Type =/= <<"char(0)">> andalso Type =/= <<"varchar(0)">> andalso Expression == undefined],
    ValidateFields = [X || X = #field{key = Key, expression = Expression} <- Fields, Key =/= <<"PRI">> andalso Expression == undefined],
    %% EmptyFields = [X || X = #field{type = Type, expression = Expression} <- Fields, Type == <<"char(0)">> orelse Type == <<"varchar(0)">> orelse Expression == undefined],
    EmptyFields = [X || X = #field{expression = Expression} <- Fields, Expression =/= undefined],
    %% no primary key, cannot do anything
    PrimaryFields == [] andalso erlang:error(lists:flatten(io_lib:format("table: ~s, could not find any primary key", [Table]))),
    %% return data
    Head = parse_head(File, Includes),
    Code = parse_code(type:to_list(Table), type:to_list(Record), PrimaryFields, ValidateFields, EmptyFields, Modes),
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
parse_code(TableName, Record, PrimaryFields, ValidateFields, EmptyFields, Modes) ->
    
    %% insert define part
    InsertFields = [X || X = #field{extra = Extra} <- lists:keysort(#field.position, PrimaryFields ++ ValidateFields), Extra =/= <<"auto_increment">>],
    InsertDefine = parse_define_insert(TableName, InsertFields),

    %% select define part, no select, primary key as select key by default
    DefaultSelectKey = tool:default([X || X = #field{comment = Comment} <- lists:keysort(#field.position, PrimaryFields ++ ValidateFields), contain(Comment, "(select)")], PrimaryFields),
    {_, SelectKeys} = listing:key_find(select, 1, Modes, {select, DefaultSelectKey}),
    SelectFields = lists:keysort(#field.position, PrimaryFields ++ ValidateFields ++ EmptyFields),
    SelectDefine = parse_define_select(TableName, SelectKeys, SelectFields),
    
    %% update define part, no update, primary key as update key by default
    UpdateKeys = tool:default([X || X = #field{comment = Comment} <- lists:keysort(#field.position, PrimaryFields ++ ValidateFields), contain(Comment, "(update)")], PrimaryFields),
    UpdateFields = tool:default([X || X = #field{comment = Comment, extra = Extra} <- ValidateFields, Extra =/= <<"auto_increment">> andalso not contain(Comment, "(once)")], UpdateKeys),
    UpdateDefine = parse_define_update(TableName, UpdateKeys, UpdateFields),

    %% delete define part, no delete, primary key as delete key by default
    DeleteKeys = tool:default([X || X = #field{comment = Comment} <- lists:keysort(#field.position, PrimaryFields ++ ValidateFields), contain(Comment, "(delete)")], PrimaryFields),
    DeleteDefine = parse_define_delete(TableName, DeleteKeys, []),

    %% insert update
    InsertUpdateFields = lists:keysort(#field.position, PrimaryFields ++ ValidateFields),
    InsertUpdateFlag = [Name || #field{name = Name, comment = Comment} <- lists:keysort(#field.position, PrimaryFields ++ ValidateFields ++ EmptyFields), contain(Comment, "(flag)")],
    InsertUpdateDefine = parse_define_insert_update(TableName, InsertUpdateFields, ValidateFields, InsertUpdateFlag),
    
    %% select join
    SelectJoinKeys = [{extract(Comment, "(?<=join\\()`?\\w+`?(?=\\.)"), extract(Comment, "(?<=join\\()(`?\\w+`?\\.`?\\w+`?)(?=\\))"), FieldInfo} || FieldInfo = #field{comment = Comment} <- lists:keysort(#field.position, PrimaryFields ++ ValidateFields), extract(Comment, "(?<=join\\()`?\\w+`?(?=\\.)") =/= []],
    %% SelectJoinFields = [case lists:keymember(extract(Comment, "(?<=join\\()`?\\w+`?(?=\\.)"), 1, SelectJoinKeys) of false -> {lists:keymember(Name, #field.name, EmptyFields), contain(Comment, "(flag)"), hd(tool:default(extract(Comment, "(?<=join\\()(`?\\w+`?\\.`?\\w+`?)(?=\\))"), [lists:concat(["`", TableName, "`", ".", "`", Name, "`"])])), FieldInfo}; true -> {lists:keymember(Name, #field.name, EmptyFields), contain(Comment, "(flag)"), hd(tool:default(extract(Comment, "(?<=join\\()(`?\\w+`?\\.`?\\w+`?)(?=\\))"), [lists:concat(["`", TableName, "`", ".", "`", Name, "`"])])), FieldInfo} end || FieldInfo = #field{name = Name, comment = Comment} <- lists:keysort(#field.position, PrimaryFields ++ ValidateFields ++ EmptyFields)],
    SelectJoinFields = [case lists:keymember(extract(Comment, "(?<=join\\()`?\\w+`?(?=\\.)"), 1, SelectJoinKeys) of false -> {lists:keymember(Name, #field.name, EmptyFields), hd(tool:default(extract(Comment, "(?<=join\\()(`?\\w+`?\\.`?\\w+`?)(?=\\))"), [lists:concat(["`", TableName, "`", ".", "`", Name, "`"])])), FieldInfo}; true -> {lists:keymember(Name, #field.name, EmptyFields), hd(tool:default(extract(Comment, "(?<=join\\()(`?\\w+`?\\.`?\\w+`?)(?=\\))"), [lists:concat(["`", TableName, "`", ".", "`", Name, "`"])])), FieldInfo} end || FieldInfo = #field{name = Name, comment = Comment} <- lists:keysort(#field.position, PrimaryFields ++ ValidateFields ++ EmptyFields)],
    SelectJoinDefine = parse_define_select_join(TableName, SelectKeys, SelectJoinKeys, SelectJoinFields),

    %% select (keys) group
    SelectGroupFields = ([{X, extract(Comment, "(?<=\\(select_)\\w+(?=\\))")} || X = #field{comment = Comment} <- lists:keysort(#field.position, PrimaryFields ++ ValidateFields)]),
    SelectGroupList = lists:append([[{Group, Field} || Group <- Groups] || {Field, Groups} <- SelectGroupFields]),
    SelectMergeGroupList = listing:key_merge(1, SelectGroupList, fun({_, Field}, {_, List}) -> [Field | List] end),
    SelectGroupDefine = [parse_define_select_group(TableName, FieldName, Fields, []) || {FieldName, Fields} <- SelectMergeGroupList],

    %% update (fields) group
    UpdateGroupFields = ([{X, extract(Comment, "(?<=\\(update_)\\w+(?=\\))")} || X = #field{comment = Comment} <- lists:keysort(#field.position, PrimaryFields ++ ValidateFields)]),
    UpdateGroupList = lists:append([[{Group, Field} || Group <- Groups] || {Field, Groups} <- UpdateGroupFields]),
    UpdateMergeGroupList = listing:key_merge(1, UpdateGroupList, fun({_, Field}, {_, List}) -> [Field | List] end),
    UpdateGroupDefine = [parse_define_update_group(TableName, FieldName, UpdateKeys, Fields) || {FieldName, Fields} <- UpdateMergeGroupList],

    %% delete (keys) group
    DeleteGroupFields = ([{X, extract(Comment, "(?<=\\(delete_)\\w+(?=\\))")} || X = #field{comment = Comment} <- lists:keysort(#field.position, PrimaryFields ++ ValidateFields)]),
    DeleteGroupList = lists:append([[{Group, Field} || Group <- Groups] || {Field, Groups} <- DeleteGroupFields]),
    DeleteMergeGroupList = listing:key_merge(1, DeleteGroupList, fun({_, Field}, {_, List}) -> [Field | List] end),
    DeleteGroupDefine = [parse_define_delete_group(TableName, FieldName, Fields, []) || {FieldName, Fields} <- DeleteMergeGroupList],

    %% delete in
    AutoIncrementKeys = [X || X = #field{extra = <<"auto_increment">>} <- lists:keysort(#field.position, PrimaryFields ++ ValidateFields)],
    DeleteInDefine = parse_define_delete_in(TableName, AutoIncrementKeys, []),

    %% truncate code
    TruncateDefine = parse_define_truncate(TableName),

    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    %%%%%%%%%%%%%%%%%%%%%%%%%%%% Separator %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    
    %% insert code
    InsertArgs = chose_style(direct, Record, [], InsertFields),
    InsertCode = parse_code_insert(TableName, InsertArgs),

    %% select code
    SelectCodeKeysArgs = string:join(listing:collect_into(#field.name, SelectKeys, fun(Name) -> word:to_hump(Name) end), ", "),
    SelectConvertFields = [Field || Field = #field{type = "'~w'"} <- SelectFields],
    SelectCode = parse_code_select(TableName, SelectCodeKeysArgs, SelectFields, SelectConvertFields),
    
    %% update code
    UpdateCodeFieldsArgs = chose_style(direct, Record, UpdateFields ++ UpdateKeys, []),
    UpdateCode = parse_code_update(TableName, UpdateCodeFieldsArgs),

    %% delete code
    DeleteCodeKeyArgs = string:join(listing:collect_into(#field.name, DeleteKeys, fun(Name) -> word:to_hump(Name) end), ", "),
    DeleteCode = parse_code_delete(TableName, DeleteCodeKeyArgs, []),

    %% insert update code
    InsertUpdateArgs = chose_style(direct, Record, [], InsertUpdateFields),
    InsertUpdateCode = parse_code_insert_update(TableName, Record, InsertUpdateArgs, InsertUpdateFlag),
    
    %% select join code
    SelectJoinCode = parse_code_select_join(TableName, SelectCodeKeysArgs, SelectJoinKeys, SelectJoinFields, SelectConvertFields),

    %% select (keys) group code
    SelectGroupCode = [parse_code_select_group(Name, string:join(listing:collect_into(#field.name, Fields, fun(FieldName) -> word:to_hump(FieldName) end), ", ")) || {Name, Fields} <- SelectMergeGroupList],

    %% update (fields) group code
    UpdateGroupCode = [parse_code_update_group(Name, string:join(listing:collect_into(#field.name, Fields, fun(FieldName) -> "This" ++ word:to_hump(FieldName) end) ++ listing:collect_into(#field.name, UpdateKeys, fun(FieldName) -> word:to_hump(FieldName) end), ", ")) || {Name, Fields} <- UpdateMergeGroupList],

    %% delete (keys) group code
    DeleteGroupCode = [parse_code_delete_group(Name, string:join(listing:collect_into(#field.name, Fields, fun(FieldName) -> word:to_hump(FieldName) end), ", ")) || {Name, Fields} <- DeleteMergeGroupList],

    %% delete in code
    DeleteInCodeKeys = listing:collect(#field.name, AutoIncrementKeys),
    DeleteInCode = parse_code_delete_in(TableName, DeleteInCodeKeys),

    %% truncate code
    TruncateCode = parse_code_truncate(TableName),

    %% collect all code
    lists:concat([InsertDefine, SelectDefine, UpdateDefine, DeleteDefine, InsertUpdateDefine, SelectJoinDefine, SelectGroupDefine, UpdateGroupDefine, DeleteGroupDefine, DeleteInDefine, TruncateDefine, InsertCode, SelectCode, UpdateCode, DeleteCode, InsertUpdateCode, SelectJoinCode, SelectGroupCode, UpdateGroupCode, DeleteGroupCode, DeleteInCode, TruncateCode]).


%%%===================================================================
%%% define part
%%%===================================================================
%% insert define
parse_define_insert(TableName, Fields) ->
    %% field
    UpperTableName = string:to_upper(TableName),
    InsertFields = string:join(listing:collect_into(#field.name, Fields, fun(Name) -> lists:concat(["`", Name, "`"]) end), ", "),
    InsertFieldsFormat = string:join(listing:collect(#field.type, Fields), ", "),
    %% insert single row data
    io_lib:format("-define(INSERT_~s, <<\"INSERT INTO `~s` (~s) VALUES (~s)\">>).\n", [UpperTableName, TableName, InsertFields, InsertFieldsFormat]).

%% select define
parse_define_select(TableName, [], Fields) ->
    parse_define_select(TableName, [], [], Fields);
parse_define_select(TableName, Keys, Fields) ->
    %% with select filter add where clause
    parse_define_select(TableName, " WHERE ", Keys, Fields).
parse_define_select(TableName, Where, Keys, Fields) ->
    UpperTableName = string:to_upper(TableName),
    %% field
    %% SelectFields = tool:default(string:join(listing:collect(#field.field, Fields), ", "), "*"),
    %% SelectFields = string:join([case contain(Comment, "(flag)") of true -> "IF(" ++ Field ++ ", 0, 1) AS " ++ Field; false -> Field end || #field{field = Field, comment = Comment} <- Fields], ", "),
    %% SelectFields = string:join([case contain(Comment, "(flag)") of true -> lists:concat(["0 AS ", "`", Name, "`"]); false -> lists:concat(["`", Name, "`"]) end || #field{name = Name, comment = Comment} <- Fields], ", "),
    SelectFields = string:join([lists:concat(["`", Name, "`"]) || #field{name = Name} <- Fields], ", "),
    %% key
    SelectKeys = listing:collect(#field.name, Keys),
    SelectKeysFormat = listing:collect(#field.type, Keys),
    SelectKeysClause = string:join(lists:zipwith(fun(Key, Format) -> lists:concat(["`", Key, "`", " = ", Format]) end, SelectKeys, SelectKeysFormat), " AND "),
    %% select without key allow
    io_lib:format("-define(SELECT_~s, <<\"SELECT ~s FROM `~s`~s~s\">>).\n", [UpperTableName, SelectFields, TableName, Where, SelectKeysClause]).

%% update define
parse_define_update(TableName, Keys, Fields) ->
    UpperTableName = string:to_upper(TableName),
    %% field empty use keys as fields
    UpdateFields = listing:collect(#field.name, Fields, Keys),
    UpdateFieldsFormat = listing:collect(#field.type, Fields, Keys),
    UpdateFieldsClause = string:join(lists:zipwith(fun(Field, Format) -> lists:concat(["`", Field, "`", " = ", Format]) end, UpdateFields, UpdateFieldsFormat), ", "),
    %% key
    UpdateKeys = listing:collect(#field.name, Keys),
    UpdateKeysFormat = listing:collect(#field.type, Keys),
    UpdateKeysClause = string:join(lists:zipwith(fun(Key, Format) -> lists:concat(["`", Key, "`", " = ", Format]) end, UpdateKeys, UpdateKeysFormat), " AND "),
    %% update operation must be use key restrict
    %% where clause is necessary always
    io_lib:format("-define(UPDATE_~s, <<\"UPDATE `~s` SET ~s WHERE ~s\">>).\n", [UpperTableName, TableName, UpdateFieldsClause, UpdateKeysClause]).

%% delete define
parse_define_delete(TableName, Keys, Fields) ->
    UpperTableName = string:to_upper(TableName),
    %% field
    DeleteFields = tool:default(string:join(listing:collect_into(#field.name, Fields, fun(Name) -> lists:concat(["`", Name, "`"]) end), ", "), ""),
    %% key
    DeleteKeys = listing:collect(#field.name, Keys),
    DeleteKeysFormat = listing:collect(#field.type, Keys),
    DeleteKeysClause = string:join(lists:zipwith(fun(Key, Format) -> lists:concat(["`", Key, "`", " = ", Format]) end, DeleteKeys, DeleteKeysFormat), " AND "),
    %% delete operation must be use key restrict
    %% where clause is necessary always
    io_lib:format("-define(DELETE_~s, <<\"DELETE ~s FROM `~s` WHERE ~s\">>).\n", [UpperTableName, DeleteFields, TableName, DeleteKeysClause]).

%% insert update
parse_define_insert_update(_TableName, _FieldsInsert, _FieldsUpdate, []) ->
    %% no update flag, do not make insert update define
    [];
parse_define_insert_update(TableName, FieldsInsert, FieldsUpdate, _Flag) ->
    %% insert field
    UpperTableName = string:to_upper(TableName),
    InsertFields = string:join(listing:collect_into(#field.name, FieldsInsert, fun(Name) -> lists:concat(["`", Name, "`"]) end), ", "),
    InsertFieldsFormat = string:join(listing:collect(#field.type, FieldsInsert), ", "),
    %% update field (not include key)
    UpdateFieldsClause = string:join([lists:concat(["`", Name, "`", " = ", "VALUES(`", Name, "`)"]) || #field{name = Name} <- FieldsUpdate], ", "),
    %% split 3 part sql for parser use
    InsertDefine = io_lib:format("-define(INSERT_UPDATE_~s, {<<\"INSERT INTO `~s` (~s) VALUES \">>, ", [UpperTableName, TableName, InsertFields]),
    ValueDefine = io_lib:format("<<\"(~s)\">>", [InsertFieldsFormat]),
    UpdateDefine = io_lib:format(", <<\" ON DUPLICATE KEY UPDATE ~s\">>}).\n", [UpdateFieldsClause]),
    lists:concat([InsertDefine, ValueDefine, UpdateDefine]).

%% select join
parse_define_select_join(_TableName, _, [], _Fields) ->
    %% no join key, do not make select join define
    [];
parse_define_select_join(TableName, [], Keys, Fields) ->
    parse_define_select_join(TableName, [], [], Keys, Fields);
parse_define_select_join(TableName, KeysFilter, Keys, Fields) ->
    %% with select filter add where clause
    parse_define_select_join(TableName, " WHERE ", KeysFilter, Keys, Fields).
parse_define_select_join(TableName, Where, KeysFilter, Keys, Fields) ->
    UpperTableName = string:to_upper(TableName),
    %% join key field use inner table name field
    %% join field must add table name
    %% type revise IF_NULL/AS(name alias)
    %% Revise = fun({true, false, OuterField, #field{name = InnerField, default = Default}}) -> lists:concat(["IFNULL(", OuterField, ", ", Default, ") AS ", "`", InnerField, "`"]); ({_, true, _, #field{name = Field}}) -> lists:concat(["0 AS ", "`", Field, "`"]); ({_, _, OuterField, _}) -> OuterField end,
    Revise = fun({true, OuterField, #field{name = InnerField, default = Default}}) -> lists:concat(["IFNULL(", OuterField, ", ", Default, ") AS ", "`", InnerField, "`"]); ({_, OuterField, _}) -> OuterField end,
    %% SelectJoinFields = string:join(listing:collect_into(1, Fields, Revise), ", "),
    SelectJoinFields = string:join([Revise(Field) || Field <- Fields], ", "),
    %% join key must add table name
    %% multi table join supported
    SelectJoinKeys = lists:append([lists:append(lists:zipwith(fun(Table, OuterField) -> lists:concat([" LEFT JOIN ", Table, " ON ", "`", TableName, "`", ".", "`", InnerField, "`", " = ", OuterField]) end, OuterTables, OuterFields)) || {OuterTables, OuterFields, #field{name = InnerField}} <- Keys]),
    %% select filter key must add table name
    SelectKeys = listing:collect(#field.name, KeysFilter),
    SelectKeysFormat = listing:collect(#field.type, KeysFilter),
    SelectKeysClause = string:join(lists:zipwith(fun(Key, Format) -> lists:concat(["`", TableName, "`", ".", "`", Key, "`",  " = ", Format]) end, SelectKeys, SelectKeysFormat), " AND "),
    %% select join key must primary and unique
    io_lib:format("-define(SELECT_JOIN_~s, <<\"SELECT ~s FROM `~s`~s~s~s\">>).\n", [UpperTableName, SelectJoinFields, TableName, SelectJoinKeys, Where, SelectKeysClause]).

%% update group define
parse_define_update_group(TableName, FieldName, Keys, Fields) ->
    UpperTableName = string:to_upper(FieldName),
    %% field empty use keys as fields
    UpdateFields = listing:collect(#field.name, Fields, Keys),
    UpdateFieldsFormat = listing:collect(#field.type, Fields, Keys),
    UpdateFieldsClause = string:join(lists:zipwith(fun(Field, Format) -> lists:concat(["`", Field, "`", " = ", Format]) end, UpdateFields, UpdateFieldsFormat), ", "),
    %% key
    UpdateKeys = listing:collect(#field.name, Keys),
    UpdateKeysFormat = listing:collect(#field.type, Keys),
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
    DeleteKeysFormat = listing:collect(#field.type, Keys),
    DeleteKeysClause = string:join(lists:zipwith(fun(Key, Format) -> lists:concat(["`", Key, "`", " = ", Format]) end, DeleteKeys, DeleteKeysFormat), " AND "),
    %% update must has key restrict
    %% where clause is necessary always
    io_lib:format("-define(DELETE_~s, <<\"DELETE FROM `~s` WHERE ~s\">>).\n", [UpperFieldName, TableName, DeleteKeysClause]).

%% select group define
parse_define_select_group(TableName, FieldName, Keys, _Fields) ->
    UpperName = string:to_upper(FieldName),
    %% field
    %% SelectFields = string:join(listing:collect(#field.field, Fields), ", "),
    %% key
    SelectKeys = listing:collect(#field.name, Keys),
    SelectKeysFormat = listing:collect(#field.type, Keys),
    SelectKeysClause = string:join(lists:zipwith(fun(Key, Format) -> lists:concat(["`", Key, "`", " = ", Format]) end, SelectKeys, SelectKeysFormat), " AND "),
    %% where clause is necessary always
    io_lib:format("-define(SELECT_~s, <<\"SELECT * FROM `~s` WHERE ~s\">>).\n", [UpperName, TableName, SelectKeysClause]).

%% delete in define
parse_define_delete_in(_TableName, [], _Fields) ->
    %% no auto increment field, do not make define in define
    [];
parse_define_delete_in(TableName, [#field{name = FieldName, type = Format}], Fields) ->
    UpperFieldName = string:to_upper(FieldName),
    %% field
    DeleteFields = tool:default(string:join(listing:collect_into(#field.name, Fields, fun(Name) -> lists:concat(["`", Name, "`"]) end), ", "), ""),
    %% update operation must be use key restrict
    %% where clause is necessary always
    io_lib:format("-define(DELETE_IN_~s, {<<\"DELETE ~s FROM `~s` WHERE `~s` in (\">>, <<\"~s\">>, <<\")\">>}).\n", [UpperFieldName, DeleteFields, TableName, FieldName, Format]).

%% truncate code
parse_define_truncate(TableName) ->
    io_lib:format("-define(TRUNCATE, <<\"TRUNCATE TABLE `~s`\">>).\n", [TableName]).
%%%===================================================================
%%% code style part
%%%===================================================================
%% code style
chose_style(direct, Record, Keys, Fields) ->
    parse_code_fields_style_direct(Record, Keys, Fields).

%% get arg directly (Record#record.field)
parse_code_fields_style_direct(Record, Keys, Fields) ->
    "\n        " ++ string:join(listing:collect_into(#field.name, Keys ++ Fields, fun(Name) -> lists:concat([word:to_hump(Record), "#", Record, ".", Name]) end), ",\n        ") ++ "\n    ".

%%%===================================================================
%%% code part
%%%===================================================================
%% insert codeN
parse_code_insert(TableName, Fields) ->
    UpperName = string:to_upper(TableName),
    HumpName = word:to_hump(TableName),
    io_lib:format("\n%% @doc insert\ninsert(~s) ->
    Sql = parser:format(?INSERT_~s, [~s]),
    sql:insert(Sql).\n\n", [HumpName, UpperName, Fields]).

%% select code
parse_code_select(TableName, Keys, _Fields, []) ->
    UpperName = string:to_upper(TableName),
    io_lib:format("%% @doc select\nselect(~s) ->
    Sql = parser:format(?SELECT_~s, [~s]),
    Data = sql:select(Sql),
    parser:convert(Data, ~s).\n\n", [Keys, UpperName, Keys, TableName]);
parse_code_select(TableName, Keys, _Fields, ConvertFields) ->
    UpperName = string:to_upper(TableName),
    HumpName = word:to_hump(TableName),
    MatchCode = string:join(listing:collect_into(#field.name, ConvertFields, fun(FieldName) -> lists:concat([FieldName, " = ", word:to_hump(FieldName)]) end), ", "),
    ConvertCode = string:join(listing:collect_into(#field.name, ConvertFields, fun(FieldName) -> lists:concat([FieldName, " = ", "parser:to_term(", word:to_hump(FieldName), ")"]) end), ", "),
    io_lib:format("%% @doc select\nselect(~s) ->
    Sql = parser:format(?SELECT_~s, [~s]),
    Data = sql:select(Sql),
    F = fun(~s = #~s{~s}) -> ~s#~s{~s} end,
    parser:convert(Data, ~s, F).\n\n", [Keys, UpperName, Keys, HumpName, TableName, MatchCode, HumpName, TableName, ConvertCode, TableName]).

%% update code
parse_code_update(TableName, Fields) ->
    UpperName = string:to_upper(TableName),
    HumpName = word:to_hump(TableName),
    io_lib:format("%% @doc update\nupdate(~s) ->
    Sql = parser:format(?UPDATE_~s, [~s]),
    sql:update(Sql).\n\n", [HumpName, UpperName, Fields]).

%% delete code
parse_code_delete(TableName, Keys, Fields) ->
    UpperName = string:to_upper(TableName),
    io_lib:format("%% @doc delete\ndelete(~s) ->
    Sql = parser:format(?DELETE_~s, [~s]),
    sql:delete(Sql).\n\n", [Keys ++ Fields, UpperName, Keys ++ Fields]).

%% batch insert code (with the flag)
parse_code_insert_update(_TableName, _, _Fields, []) ->
    %% no update flag, do not make insert update code
    [];
parse_code_insert_update(TableName, Record, Fields, [Flag | _]) ->
    UpperName = string:to_upper(TableName),
    HumpName = word:to_hump(TableName),
    io_lib:format("\n%% @doc insert_update\ninsert_update(Data) ->
    F = fun(~s) -> [~s] end,
    {Sql, NewData} = parser:collect_into(Data, F, ?INSERT_UPDATE_~s, #~s.~s),
    sql:insert(Sql),
    NewData.\n\n", [HumpName, Fields, UpperName, Record, Flag]).

%% select join other table
parse_code_select_join(_TableName, _, [], _, _) ->
    %% no join key, do not make select join code
    [];
parse_code_select_join(_TableName, _, _, [], _) ->
    %% no join key, do not make select join code
    [];
parse_code_select_join(TableName, Keys, _, _Fields, []) ->
    UpperName = string:to_upper(TableName),
    io_lib:format("%% @doc select join\nselect_join(~s) ->
    Sql = parser:format(?SELECT_JOIN_~s, [~s]),
    Data = sql:select(Sql),
    parser:convert(Data, ~s).\n\n", [Keys, UpperName, Keys, TableName]);
parse_code_select_join(TableName, Keys, _, _Fields, ConvertFields) ->
    UpperName = string:to_upper(TableName),
    HumpName = word:to_hump(TableName),
    MatchCode = string:join(listing:collect_into(#field.name, ConvertFields, fun(FieldName) -> lists:concat([FieldName, " = ", word:to_hump(FieldName)]) end), ", "),
    ConvertCode = string:join(listing:collect_into(#field.name, ConvertFields, fun(FieldName) -> lists:concat([FieldName, " = ", "parser:to_term(", word:to_hump(FieldName), ")"]) end), ", "),
    io_lib:format("%% @doc select join\nselect_join(~s) ->
    Sql = parser:format(?SELECT_JOIN_~s, [~s]),
    Data = sql:select(Sql),
    F = fun(~s = #~s{~s}) -> ~s#~s{~s} end,
    parser:convert(Data, ~s, F).\n\n", [Keys, UpperName, Keys, HumpName, TableName, MatchCode, HumpName, TableName, ConvertCode, TableName]).

%% select group code
parse_code_select_group(TableName, Fields) ->
    UpperName = string:to_upper(TableName),
    io_lib:format("%% @doc select\nselect_~s(~s) ->
    Sql = parser:format(?SELECT_~s, [~s]),
    sql:select(Sql).\n\n", [TableName, Fields, UpperName, Fields]).

%% update group code
parse_code_update_group(TableName, Fields) ->
    UpperName = string:to_upper(TableName),
    io_lib:format("%% @doc update\nupdate_~s(~s) ->
    Sql = parser:format(?UPDATE_~s, [~s]),
    sql:update(Sql).\n\n", [TableName, Fields, UpperName, Fields]).

%% delete group code
parse_code_delete_group(TableName, Fields) ->
    UpperName = string:to_upper(TableName),
    io_lib:format("%% @doc delete\ndelete_~s(~s) ->
    Sql = parser:format(?DELETE_~s, [~s]),
    sql:delete(Sql).\n\n", [TableName, Fields, UpperName, Fields]).

%% delete in code
parse_code_delete_in(_TableName, []) ->
    %% no auto increment field, do not make define in code
    [];
parse_code_delete_in(_TableName, [FieldName]) ->
    UpperName = string:to_upper(FieldName),
    HumpName = word:to_hump(FieldName),
    io_lib:format("%% @doc delete\ndelete_in_~s(~sList) ->
    F = fun(~s) -> [~s] end,
    Sql = parser:collect(~sList, F, ?DELETE_IN_~s),
    sql:delete(Sql).\n\n", [FieldName, HumpName, HumpName, HumpName, HumpName, UpperName]).

%% truncate code
parse_code_truncate(_TableName) ->
    io_lib:format("%% @doc truncate\ntruncate() ->
    Sql = parser:format(?TRUNCATE, []),
    sql:query(Sql).\n\n", []).

%%%===================================================================
%%% Common Tool
%%%===================================================================
%% contain
contain(Content, What) ->
    string:str(Content, What) =/= 0.

%% extract
extract(Content, Match) ->
    lists:usort(lists:append(extract(Content, Match, [global, {capture, all, list}]))).
extract(Content, Match, Option) ->
    case re:run(Content, Match, Option) of
        {match, Result} ->
            Result;
        _ ->
            []
    end.

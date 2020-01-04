%%%------------------------------------------------------------------
%%% @doc
%%% module sql maker
%%% database fields to sql code tool
%%% @end
%%%------------------------------------------------------------------
-module(sql_maker).
-export([start/1]).
-record(field, {name, field, default, type, format, comment, position, key, extra}).
%%%==================================================================
%%% API functions
%%%==================================================================
%% @doc for shell
start(List) ->
    maker:start(fun parse_table/2, List).

%%%==================================================================
%%% Internal functions
%%%==================================================================
%% parse per table
parse_table(DataBase, {File, Table, Includes}) ->
    parse_table(DataBase, {File, Table, Table, Includes, []});
parse_table(DataBase, {File, Table, Includes, Modes}) ->
    parse_table(DataBase, {File, Table, Table, Includes, Modes});
parse_table(DataBase, {File, Table, Record, Includes, Modes}) ->
    %% make fields sql
    FieldsSql = io_lib:format(<<"SELECT `COLUMN_NAME`, CONCAT('`', `COLUMN_NAME`, '`'), `COLUMN_DEFAULT`, `COLUMN_TYPE`, `DATA_TYPE`, `COLUMN_COMMENT`, `ORDINAL_POSITION`, `COLUMN_KEY`, `EXTRA` FROM information_schema.`COLUMNS` WHERE `TABLE_SCHEMA` = '~s' AND `TABLE_NAME` = '~s' ORDER BY `ORDINAL_POSITION`;">>, [DataBase, Table]),
    %% data revise
    Revise = fun
        (FieldInfo = #field{name = Name, field = Field, format = <<"char">>}) ->
            FieldInfo#field{name = type:to_list(Name), field = type:to_list(Field), format = "'~s'", default = "''"};
        (FieldInfo = #field{name = Name, field = Field, type = <<"varchar(0)">>, comment = Comment}) ->
            Default = hd(tool:default(extract("(?<=default\\().*?(?=\\))", Comment), ["0"])),
            FieldInfo#field{name = type:to_list(Name), field = type:to_list(Field), format = "'~s'", default = Default};
        (FieldInfo = #field{name = Name, field = Field, format = <<"varchar">>}) ->
            FieldInfo#field{name = type:to_list(Name), field = type:to_list(Field), format = "'~w'", default = "''"};
        (FieldInfo = #field{name = Name, field = Field}) ->
            FieldInfo#field{name = type:to_list(Name), field = type:to_list(Field), format = "~w", default = "0"}
    end,
    %% fetch table fields
    Fields = parser:convert(maker:select(FieldsSql), field, Revise),
    %% primary key fields
    PrimaryFields = [X || X = #field{key = <<"PRI">>} <- Fields],
    ValidateFields = [X || X = #field{key = Key, type = Type} <- Fields, Key =/= <<"PRI">> andalso Type =/= <<"char(0)">> andalso Type =/= <<"varchar(0)">>],
    EmptyFields = [X || X = #field{type = Type} <- Fields, Type =:= <<"char(0)">> orelse Type =:= <<"varchar(0)">>],
    %% no primary key, cannot do anything
    PrimaryFields == [] andalso erlang:error("table: " ++ type:to_list(Table) ++ ", could not found any primary key"),
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
    SelectDefine = parse_define_select(TableName, SelectKeys, lists:keysort(#field.position, PrimaryFields ++ ValidateFields ++ EmptyFields)),
    
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
    SelectJoinFields = [case lists:keymember(extract(Comment, "(?<=join\\()`?\\w+`?(?=\\.)"), 1, SelectJoinKeys) of false -> {lists:keymember(Field, #field.field, EmptyFields), contain(Comment, "(flag)"), hd(tool:default(extract(Comment, "(?<=join\\()(`?\\w+`?\\.`?\\w+`?)(?=\\))"), [lists:concat(["`", TableName, "`", ".", Field])])), FieldInfo}; true -> {lists:keymember(Field, #field.field, EmptyFields), contain(Comment, "(flag)"), hd(tool:default(extract(Comment, "(?<=join\\()(`?\\w+`?\\.`?\\w+`?)(?=\\))"), [lists:concat(["`", TableName, "`", ".", Field])])), FieldInfo} end || FieldInfo = #field{field = Field, comment = Comment} <- lists:keysort(#field.position, PrimaryFields ++ ValidateFields ++ EmptyFields)],
    SelectJoinDefine = parse_define_select_join(TableName, SelectKeys, SelectJoinKeys, SelectJoinFields),

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

    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    %%%%%%%%%%%%%%%%%%%%%%%%%%%% Separator %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    
    %% insert code
    InsertArgs = chose_style(direct, Record, [], InsertFields),
    InsertCode = parse_code_insert(TableName, InsertArgs),

    %% select code
    SelectCodeKeysArgs = string:join(listing:collect_into(#field.name, SelectKeys, fun(Name) -> maker:hump(Name) end), ", "),
    SelectCode = parse_code_select(TableName, SelectCodeKeysArgs, SelectCodeKeysArgs),
    
    %% update code
    UpdateCodeFieldsArgs = chose_style(direct, Record, UpdateFields ++ UpdateKeys, []),
    UpdateCode = parse_code_update(TableName, UpdateCodeFieldsArgs),

    %% delete code
    DeleteCodeKeyArgs = string:join(listing:collect_into(#field.name, DeleteKeys, fun(Name) -> maker:hump(Name) end), ", "),
    DeleteCode = parse_code_delete(TableName, DeleteCodeKeyArgs, []),

    %% insert update code
    InsertUpdateArgs = chose_style(direct, Record, [], InsertUpdateFields),
    InsertUpdateCode = parse_code_insert_update(TableName, Record, InsertUpdateArgs, InsertUpdateFlag),
    
    %% select join code
    SelectJoinCode = parse_code_select_join(TableName, SelectJoinKeys, SelectCodeKeysArgs),

    %% update (fields) group code
    UpdateGroupCode = [parse_code_update_group(Name, string:join(listing:collect_into(#field.name, Fields ++ UpdateKeys, fun(FieldName) -> maker:hump(FieldName) end), ", ")) || {Name, Fields} <- UpdateMergeGroupList],

    %% delete (keys) group code
    DeleteGroupCode = [parse_code_delete_group(Name, string:join(listing:collect_into(#field.name, Fields, fun(FieldName) -> maker:hump(FieldName) end), ", ")) || {Name, Fields} <- DeleteMergeGroupList],

    %% delete in code
    DeleteInCodeKeys = listing:collect(#field.name, AutoIncrementKeys),
    DeleteInCode = parse_code_delete_in(DeleteInCodeKeys),

    %% collect all code
    lists:concat([InsertDefine, SelectDefine, UpdateDefine, DeleteDefine, InsertUpdateDefine, SelectJoinDefine, UpdateGroupDefine, DeleteGroupDefine, DeleteInDefine, InsertCode, SelectCode, UpdateCode, DeleteCode, InsertUpdateCode, SelectJoinCode, UpdateGroupCode, DeleteGroupCode, DeleteInCode]).


%%%==================================================================
%%% define part
%%%==================================================================
%% insert define
parse_define_insert(Name, Fields) ->
    %% field
    UpperName = string:to_upper(Name),
    InsertFields = string:join(listing:collect(#field.field, Fields), ", "),
    InsertFieldsFormat = string:join(listing:collect(#field.format, Fields), ", "),
    %% insert single row data
    io_lib:format("-define(INSERT_~s, <<\"INSERT INTO `~s` (~s) VALUES (~s)\">>).\n", [UpperName, Name, InsertFields, InsertFieldsFormat]).

%% select define
parse_define_select(Name, [], Fields) ->
    parse_define_select(Name, [], [], Fields);
parse_define_select(Name, Keys, Fields) ->
    %% with select filter add where clause
    parse_define_select(Name, " WHERE ", Keys, Fields).
parse_define_select(Name, Where, Keys, Fields) ->
    UpperName = string:to_upper(Name),
    %% field
    %% SelectFields = tool:default(string:join(listing:collect(#field.field, Fields), ", "), "*"),
    %% SelectFields = string:join([case contain(Comment, "(flag)") of true -> "IF(" ++ Field ++ ", 0, 1) AS " ++ Field; false -> Field end || #field{field = Field, comment = Comment} <- Fields], ", "),
    SelectFields = string:join([case contain(Comment, "(flag)") of true -> "0 AS " ++ Field; false -> Field end || #field{field = Field, comment = Comment} <- Fields], ", "),
    %% key
    SelectKeys = listing:collect(#field.field, Keys),
    SelectKeysFormat = listing:collect(#field.format, Keys),
    SelectKeysClause = string:join(lists:zipwith(fun(Key, Format) -> lists:concat([Key, " = ", Format]) end, SelectKeys, SelectKeysFormat), " AND "),
    %% select without key allow
    io_lib:format("-define(SELECT_~s, <<\"SELECT ~s FROM `~s`~s~s\">>).\n", [UpperName, SelectFields, Name, Where, SelectKeysClause]).

%% update define
parse_define_update(Name, Keys, Fields) ->
    UpperName = string:to_upper(Name),
    %% field empty use keys as fields
    UpdateFields = listing:collect(#field.field, Fields, Keys),
    UpdateFieldsFormat = listing:collect(#field.format, Fields, Keys),
    UpdateFieldsClause = string:join(lists:zipwith(fun(Field, Format) -> lists:concat([Field, " = ", Format]) end, UpdateFields, UpdateFieldsFormat), ", "),
    %% key
    UpdateKeys = listing:collect(#field.field, Keys),
    UpdateKeysFormat = listing:collect(#field.format, Keys),
    UpdateKeysClause = string:join(lists:zipwith(fun(Key, Format) -> lists:concat([Key, " = ", Format]) end, UpdateKeys, UpdateKeysFormat), " AND "),
    %% update must has key restrict
    %% where clause always need
    io_lib:format("-define(UPDATE_~s, <<\"UPDATE `~s` SET ~s WHERE ~s\">>).\n", [UpperName, Name, UpdateFieldsClause, UpdateKeysClause]).

%% delete define
parse_define_delete(Name, Keys, Fields) ->
    UpperName = string:to_upper(Name),
    %% field
    DeleteFields = tool:default(string:join(listing:collect(#field.field, Fields), ", "), ""),
    %% key
    DeleteKeys = listing:collect(#field.field, Keys),
    DeleteKeysFormat = listing:collect(#field.format, Keys),
    DeleteKeysClause = string:join(lists:zipwith(fun(Key, Format) -> lists:concat([Key, " = ", Format]) end, DeleteKeys, DeleteKeysFormat), " AND "),
    %% update must has key restrict
    %% where clause always need
    io_lib:format("-define(DELETE_~s, <<\"DELETE ~s FROM `~s` WHERE ~s\">>).\n", [UpperName, DeleteFields, Name, DeleteKeysClause]).

%% insert update
parse_define_insert_update(_Name, _FieldsInsert, _FieldsUpdate, []) ->
    %% no update flag, do not make insert update define
    [];
parse_define_insert_update(Name, FieldsInsert, FieldsUpdate, _Flag) ->
    %% insert field
    UpperName = string:to_upper(Name),
    InsertFields = string:join(listing:collect(#field.field, FieldsInsert), ", "),
    InsertFieldsFormat = string:join(listing:collect(#field.format, FieldsInsert), ", "),
    %% update field (not include key)
    UpdateFieldsClause = string:join([lists:concat([Field, " = ", "VALUES(", Field, ")"]) || #field{field = Field} <- FieldsUpdate], ", "),
    %% split 3 part sql for parser use
    InsertDefine = io_lib:format("-define(INSERT_UPDATE_~s, {<<\"INSERT INTO `~s` (~s) VALUES \">>, ", [UpperName, Name, InsertFields]),
    ValueDefine = io_lib:format("<<\"(~s)\">>", [InsertFieldsFormat]),
    UpdateDefine = io_lib:format(", <<\" ON DUPLICATE KEY UPDATE ~s\">>}).\n", [UpdateFieldsClause]),
    lists:concat([InsertDefine, ValueDefine, UpdateDefine]).

%% select join
parse_define_select_join(_Name, _, [], _Fields) ->
    %% no join key, do not make select join define
    [];
parse_define_select_join(Name, [], Keys, Fields) ->
    parse_define_select_join(Name, [], [], Keys, Fields);
parse_define_select_join(Name, KeysFilter, Keys, Fields) ->
    %% with select filter add where clause
    parse_define_select_join(Name, " WHERE ", KeysFilter, Keys, Fields).
parse_define_select_join(Name, Where, KeysFilter, Keys, Fields) ->
    UpperName = string:to_upper(Name),
    %% join key field use inner table name field
    %% join field must add table name
    %% type revise IF_NULL/AS(name alias)
    Revise = fun({true, false, OuterField, #field{field = InnerField, default = Default}}) -> lists:concat(["IFNULL(", OuterField, ", ", Default, ") AS ", InnerField]); ({_, true, _, #field{field = Field}}) -> lists:concat(["0 AS ", Field]); ({_, _, OuterField, _}) -> OuterField end,
    %% SelectJoinFields = string:join(listing:collect_into(1, Fields, Revise), ", "),
    SelectJoinFields = string:join([Revise(Field) || Field <- Fields], ", "),
    %% join key must add table name
    %% multi table join supported
    SelectJoinKeys = lists:append([lists:append(lists:zipwith(fun(Table, OuterField) -> lists:concat(["LEFT JOIN ", Table, " ON ", "`", Name, "`", ".", InnerField, " = ", OuterField]) end, OuterTables, OuterFields)) || {OuterTables, OuterFields, #field{field = InnerField}} <- Keys]),
    %% select filter key must add table name
    SelectKeys = listing:collect(#field.field, KeysFilter),
    SelectKeysFormat = listing:collect(#field.format, KeysFilter),
    SelectKeysClause = string:join(lists:zipwith(fun(Key, Format) -> lists:concat(["`", Name, "`.", Key, " = ", Format]) end, SelectKeys, SelectKeysFormat), " AND "),
    %% select join key must primary and unique
    io_lib:format("-define(SELECT_JOIN_~s, <<\"SELECT ~s FROM `~s`~s~s~s\">>).\n", [UpperName, SelectJoinFields, Name, SelectJoinKeys, Where, SelectKeysClause]).

%% update group define
parse_define_update_group(Name, FieldName, Keys, Fields) ->
    UpperName = string:to_upper(FieldName),
    %% field empty use keys as fields
    UpdateFields = listing:collect(#field.field, Fields, Keys),
    UpdateFieldsFormat = listing:collect(#field.format, Fields, Keys),
    UpdateFieldsClause = string:join(lists:zipwith(fun(Field, Format) -> lists:concat([Field, " = ", Format]) end, UpdateFields, UpdateFieldsFormat), ", "),
    %% key
    UpdateKeys = listing:collect(#field.field, Keys),
    UpdateKeysFormat = listing:collect(#field.format, Keys),
    UpdateKeysClause = string:join(lists:zipwith(fun(Key, Format) -> lists:concat([Key, " = ", Format]) end, UpdateKeys, UpdateKeysFormat), " AND "),
    %% update must has key restrict
    %% where clause always need
    io_lib:format("-define(UPDATE_~s, <<\"UPDATE `~s` SET ~s WHERE ~s\">>).\n", [UpperName, Name, UpdateFieldsClause, UpdateKeysClause]).

%% delete group define
parse_define_delete_group(Name, FieldName, Keys, Fields) ->
    UpperName = string:to_upper(FieldName),
    %% field
    DeleteFields = string:join(listing:collect(#field.field, Fields), ", "),
    %% key
    DeleteKeys = listing:collect(#field.field, Keys),
    DeleteKeysFormat = listing:collect(#field.format, Keys),
    DeleteKeysClause = string:join(lists:zipwith(fun(Key, Format) -> lists:concat([Key, " = ", Format]) end, DeleteKeys, DeleteKeysFormat), " AND "),
    %% update must has key restrict
    %% where clause always need
    io_lib:format("-define(DELETE_~s, <<\"DELETE ~s FROM `~s` WHERE ~s\">>).\n", [UpperName, DeleteFields, Name, DeleteKeysClause]).


%% delete in define
parse_define_delete_in(_Name, [], _Fields) ->
    %% no auto increment field, do not make define in define
    [];
parse_define_delete_in(Name, [#field{name = FieldName, field = Field, format = Format}], Fields) ->
    UpperName = string:to_upper(FieldName),
    %% field
    DeleteFields = tool:default(string:join(listing:collect(#field.field, Fields), ", "), ""),
    %% update must has key restrict
    %% where clause always need
    io_lib:format("-define(DELETE_IN_~s, {<<\"DELETE ~s FROM `~s` WHERE ~s in (\">>, <<\"~s\">>, <<\")\">>}).\n", [UpperName, DeleteFields, Name, Field, Format]).

%%%==================================================================
%%% code style part
%%%==================================================================
%% code style
chose_style(direct, Record, Keys, Fields) ->
    parse_code_fields_style_direct(Record, Keys, Fields).

%% get arg directly (Record#record.field)
parse_code_fields_style_direct(Record, Keys, Fields) ->
    "\n        " ++ string:join(listing:collect_into(#field.name, Keys ++ Fields, fun(Name) -> lists:concat([maker:hump(Record), "#", Record, ".", Name]) end), ",\n        ") ++ "\n    ".

%%%==================================================================
%%% code part
%%%==================================================================
%% insert codeN
parse_code_insert(Name, Fields) ->
    UpperName = string:to_upper(Name),
    HumpName = maker:hump(Name),
    io_lib:format("\n%% @doc insert\ninsert(~s) ->
    Sql = parser:format(?INSERT_~s, [~s]),
    sql:insert(Sql).\n\n", [HumpName, UpperName, Fields]).

%% select code
parse_code_select(Name, Keys, Fields) ->
    UpperName = string:to_upper(Name),
    io_lib:format("%% @doc select\nselect(~s) ->
    Sql = parser:format(?SELECT_~s, [~s]),
    sql:select(Sql).\n\n", [Keys, UpperName, Fields]).

%% update code
parse_code_update(Name, Fields) ->
    UpperName = string:to_upper(Name),
    HumpName = maker:hump(Name),
    io_lib:format("%% @doc update\nupdate(~s) ->
    Sql = parser:format(?UPDATE_~s, [~s]),
    sql:update(Sql).\n\n", [HumpName, UpperName, Fields]).

%% delete code
parse_code_delete(Name, Keys, Fields) ->
    UpperName = string:to_upper(Name),
    io_lib:format("%% @doc delete\ndelete(~s) ->
    Sql = parser:format(?DELETE_~s, [~s]),
    sql:delete(Sql).\n\n", [Keys ++ Fields, UpperName, Keys ++ Fields]).

%% batch insert code (with flag)
parse_code_insert_update(_Name, _, _Fields, []) ->
    %% no update flag, do not make insert update code
    [];
parse_code_insert_update(Name, Record, Fields, [Flag | _]) ->
    UpperName = string:to_upper(Name),
    HumpName = maker:hump(Name),
    io_lib:format("\n%% @doc insert_update\ninsert_update(Data) ->
    F = fun(~s) -> [~s] end,
    {Sql, NewData} = parser:collect_into(Data, F, ?INSERT_UPDATE_~s, #~s.~s),
    sql:insert(Sql),
    NewData.\n\n", [HumpName, Fields, UpperName, Record, Flag]).

%% select join other table
parse_code_select_join(_Name, [], _) ->
    %% no join key, do not make select join code
    [];
parse_code_select_join(Name, _, Fields) ->
    UpperName = string:to_upper(Name),
    io_lib:format("%% @doc select join\nselect_join(~s) ->
    Sql = parser:format(?SELECT_JOIN_~s, [~s]),
    sql:select(Sql).\n\n", [Fields, UpperName, Fields]).

%% update group code
parse_code_update_group(Name, Fields) ->
    UpperName = string:to_upper(Name),
    io_lib:format("%% @doc update\nupdate_~s(~s) ->
    Sql = parser:format(?UPDATE_~s, [~s]),
    sql:update(Sql).\n\n", [Name, Fields, UpperName, Fields]).

%% delete group code
parse_code_delete_group(Name, Fields) ->
    UpperName = string:to_upper(Name),
    io_lib:format("%% @doc delete\ndelete_~s(~s) ->
    Sql = parser:format(?DELETE_~s, [~s]),
    sql:delete(Sql).\n\n", [Name, Fields, UpperName, Fields]).

%% delete in code
parse_code_delete_in([]) ->
    %% no auto increment field, do not make define in code
    [];
parse_code_delete_in([FieldName]) ->
    UpperName = string:to_upper(FieldName),
    HumpName = maker:hump(FieldName),
    io_lib:format("%% @doc delete\ndelete_in_~s(~sList) ->
    F = fun(~s) -> [~s] end,
    Sql = parser:collect(~sList, F, ?DELETE_IN_~s),
    sql:delete(Sql).\n\n", [FieldName, HumpName, HumpName, HumpName, HumpName, UpperName]).

%%%==================================================================
%%% Common Tool
%%%==================================================================
%% contain
contain(Content, What) ->
    string:str(type:to_list(Content), What) =/= 0.

%% extract
extract(Content, Match) ->
    lists:usort(lists:append(extract(Content, Match, [global, {capture, all, list}]))).
extract(Content, Match, Option) ->
    case re:run(type:to_list(Content), Match, Option) of
        {match, Result} ->
            Result;
        _ ->
            []
    end.

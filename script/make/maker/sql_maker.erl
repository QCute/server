%%%-------------------------------------------------------------------
%%% @doc
%%% module sql maker
%%% database fields to sql code tool
%%% @end
%%%-------------------------------------------------------------------
-module(sql_maker).
-export([start/1]).
%% ------------------------ user guide -------------------------------
%% fields property/comment specified
%% insert fields not contain auto_increment/(ignore)/char(0)/varchar(0) property
%% no (update) property, use primary key to update, update fields not contain auto_increment/(once)/(ignore)/char(0)/varchar(0) property
%% (select) select all fields by default
%% (delete) delete this row by default
%%
%% update/delete group support
%% (update_???)/(delete_???)
%% sql group will group by same group name, multi group supported
%%

-record(field, {name, field, default, type, format, comment, position, key, extra}).
%%%===================================================================
%%% API
%%%===================================================================
%% @doc for shell
start(List) ->
	maker:start(fun parse_table/2, List).

%%%====================================================================
%%% Internal functions
%%%====================================================================
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
			FieldInfo#field{name = type:to_list(Name), field = type:to_list(Field), format = "'~s'"};
		(FieldInfo = #field{name = Name, field = Field}) ->
			FieldInfo#field{name = type:to_list(Name), field = type:to_list(Field), format = "'~w'"}
	end,
	%% fetch table fields
	Fields = parser:convert(maker:select(FieldsSql), field, Revise),
	%% primary key fields
	PrimaryFields = [X || X = #field{key = <<"PRI">>} <- Fields],
	ValidateFields = [X || X = #field{key = Key, type = Type} <- Fields, Key =/= <<"PRI">> andalso Type =/= <<"char(0)">> andalso Type =/= <<"varchar(0)">>],
	EmptyFields = [X || X = #field{type = Type} <- Fields, Type =:= <<"char(0)">> orelse Type =:= <<"varchar(0)">>],
	%% no primary key, cannot do anything
	PrimaryFields == [] andalso erlang:error("could not found any primary key"),
	%% return data
	Head = parse_head(File, Includes),
	Code = parse_code(type:to_list(Table), type:to_list(Record), PrimaryFields, ValidateFields, EmptyFields, Modes),
	[{"(?s).*", Head ++ Code}].

%% sql code head
parse_head(File, Includes) ->
	Module = filename:basename(File, ".erl"),
	%% head
	Head = io_lib:format("-module(~s).\n-compile(nowarn_export_all).\n-compile(export_all).\n", [Module]),
	%% include
	Include = [lists:flatten(io_lib:format("-include(\"~s\").\n", [X])) || X <- Includes],
	lists:concat([Head, Include]).

%% parse all code
parse_code(TableName, Record, PrimaryFields, ValidateFields, EmptyFields, Modes) ->
	
	%% insert define part
	InsertFields = [X || X = #field{extra = Extra} <- PrimaryFields ++ ValidateFields, Extra =/= <<"auto_increment">>],
	InsertDefine = parse_define_insert(TableName, InsertFields),
	
	%% delete define part, no delete, primary key as delete key by default
	DeleteKeys = tool:default([X || X = #field{comment = Comment} <- ValidateFields, contain(Comment, "(delete)")], PrimaryFields),
	DeleteDefine = parse_define_delete(TableName, DeleteKeys, []),
	
	%% select define part, no select, primary key as select key by default
	{_, SelectKeys} = listing:key_find(select, 1, Modes, {select, tool:default([X || X = #field{comment = Comment} <- PrimaryFields, contain(Comment, "(select)")], PrimaryFields)}),
	SelectDefine = parse_define_select(TableName, SelectKeys, []),
	
	%% update define part, no update, primary key as update key by default
	UpdateKeys = tool:default([X || X = #field{comment = Comment} <- PrimaryFields, contain(Comment, "(update)")], PrimaryFields),
	UpdateFields = [X || X = #field{comment = Comment, extra = Extra} <- ValidateFields, Extra =/= <<"auto_increment">> andalso not contain(Comment, "(once)")],
	UpdateDefine = parse_define_update(TableName, UpdateKeys, UpdateFields),
	
	%% insert update
	InsertUpdateDefine = parse_define_insert_update(TableName, InsertFields, UpdateFields),
	
	%% select join
	RawSelectJoinKeys = [{Name, extract(Comment, "(?<=join\\()`?\\w+`?(?=\\.)"), lists:concat(["`", TableName, "`.", Field]), extract(Comment, "(?<=join\\()(`?\\w+`?\\.`?\\w+`?)(?=\\))")} || #field{name = Name, field = Field, comment = Comment} <- PrimaryFields],
	{_, SelectJoinKeys} = listing:key_find(join, 1, Modes, {select, [X || X = {_, _, _, [_ | _]} <- RawSelectJoinKeys]}),
	SelectJoinFields = [{Name, extract(Comment, "(?<=join\\()`?\\w+`?(?=\\.)"), lists:concat(["`", TableName, "`.", Field]), extract(Comment, "(?<=join\\()(`?\\w+`?\\.`?\\w+`?)(?=\\))")} || #field{name = Name, field = Field, comment = Comment} <- PrimaryFields ++ ValidateFields ++ EmptyFields],
	SelectJoinFilterKeys = [X#field{field = lists:concat(["`", TableName, "`.", Field])} || X = #field{field = Field} <- SelectKeys],
	SelectJoinDefine = parse_define_select_join(TableName, SelectJoinFilterKeys, SelectJoinKeys, SelectJoinFields),
	
	%% update group
	%% UpdateGroupFields = ([{Name, lists:append(extract(Comment, "(?<=\\()update_\\w+(?=\\))", [global, {capture, all, list}]))} || #field{field = Name, comment = Comment} <- PrimaryFields ++ ValidateFields]),
	
	%% delete group
	%% DeleteGroupFields = ([{Name, lists:append(extract(Comment, "(?<=\\()delete_\\w+(?=\\))", [global, {capture, all, list}]))} || #field{field = Name, comment = Comment} <- PrimaryFields ++ ValidateFields]),
	
	
	%%%====================================================================
	
	%% insert code
	InsertCode = parse_code_insert(TableName, chose_style(direct, Record, [], InsertFields)),
	
	%% delete code
	DeleteCodeArgs = string:join(listing:collect_into(#field.name, DeleteKeys, fun(Name) -> maker:hump(Name) end), ", "),
	DeleteCode = parse_code_delete(TableName, DeleteCodeArgs, DeleteCodeArgs),
	
	%% select code
	SelectCodeArgs = string:join(listing:collect_into(#field.name, SelectKeys, fun(Name) -> maker:hump(Name) end), ", "),
	SelectCode = parse_code_select(TableName, SelectCodeArgs, SelectCodeArgs),
	
	%% update code
	UpdateCode = parse_code_update(TableName, chose_style(direct, Record, UpdateFields, [])),
	
	%% insert update code
	InsertUpdateFlag = [Name || #field{name = Name, comment = Comment} <- PrimaryFields ++ ValidateFields ++ EmptyFields, contain(Comment, "(flag)")],
	InsertUpdateArgs = chose_style(direct, Record, [], InsertFields),
	InsertUpdateCode = parse_code_insert_update(TableName, InsertUpdateArgs, InsertUpdateFlag),
	
	%% delete in code
	
	%% select join code
	SelectJoinCodeKeys = string:join(listing:collect_into(#field.name, SelectKeys, fun(Name) -> maker:hump(Name) end), ", "),
	SelectJoinCode = parse_code_select_join(TableName, SelectJoinCodeKeys, SelectJoinCodeKeys),
	
	%% collect
	lists:concat([InsertDefine, DeleteDefine, SelectDefine, UpdateDefine, InsertUpdateDefine, SelectJoinDefine, InsertCode, DeleteCode, SelectCode, UpdateCode, InsertUpdateCode, SelectJoinCode]).




%%%====================================================================
%%% define part
%%%====================================================================
%% insert define
parse_define_insert(Name, Fields) ->
	%% field
	UpperName = string:to_upper(Name),
	InsertFields = string:join(listing:collect(#field.field, Fields), ", "),
	InsertFieldsFormat = string:join(listing:collect(#field.format, Fields), ", "),
	%% insert single row data
	io_lib:format("-define(INSERT_~s, <<\"INSERT INTO `~s` (~s) VALUES (~s)\">>).\n", [UpperName, Name, InsertFields, InsertFieldsFormat]).

%% delete define
parse_define_delete(Name, Keys, Fields) ->
	UpperName = string:to_upper(Name),
	%% field
	DeleteFields = string:join(listing:collect(#field.field, Fields), ", "),
	%% key
	DeleteKeys = listing:collect(#field.field, Keys),
	DeleteKeysFormat = listing:collect(#field.format, Keys),
	DeleteKeysClause = string:join(lists:zipwith(fun(Key, Format) -> lists:concat([Key, " = ", Format]) end, DeleteKeys, DeleteKeysFormat), " AND "),
	%% update must has key restrict
	%% where clause always need
	io_lib:format("-define(DELETE_~s, <<\"DELETE ~s FROM `~s` WHERE ~s\">>).\n", [UpperName, DeleteFields, Name, DeleteKeysClause]).

%% select define
parse_define_select(Name, [], Fields) ->
	parse_define_select(Name, [], [], Fields);
parse_define_select(Name, Keys, Fields) ->
	parse_define_select(Name, " WHERE ", Keys, Fields).
parse_define_select(Name, Where, Keys, Fields) ->
	UpperName = string:to_upper(Name),
	%% field
	SelectFields = tool:default(string:join(listing:collect(#field.field, Fields), ", "), "*"),
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
	UpdateFields = listing:collect(#field.field, tool:default(Fields, Keys)),
	UpdateFieldsFormat = listing:collect(#field.format, tool:default(Fields, Keys)),
	UpdateFieldsClause = string:join(lists:zipwith(fun(Field, Format) -> lists:concat([Field, " = ", Format]) end, UpdateFields, UpdateFieldsFormat), ", "),
	%% key
	UpdateKeys = listing:collect(#field.field, Keys),
	UpdateKeysFormat = listing:collect(#field.format, Keys),
	UpdateKeysClause = string:join(lists:zipwith(fun(Key, Format) -> lists:concat([Key, " = ", Format]) end, UpdateKeys, UpdateKeysFormat), " AND "),
	%% update must has key restrict
	%% where clause always need
	io_lib:format("-define(UPDATE_~s, <<\"UPDATE `~s` SET ~s WHERE ~s\">>).\n", [UpperName, Name, UpdateFieldsClause, UpdateKeysClause]).

%% insert update
parse_define_insert_update(Name, FieldsInsert, FieldsUpdate) ->
	%% insert field
	UpperName = string:to_upper(Name),
	InsertFields = string:join(listing:collect(#field.field, FieldsInsert), ", "),
	InsertFieldsFormat = string:join(listing:collect(#field.format, FieldsInsert), ", "),
	%% update field
	UpdateFields = listing:collect(#field.field, FieldsUpdate),
	UpdateFieldsFormat = listing:collect(#field.format, FieldsUpdate),
	UpdateFieldsClause = string:join(lists:zipwith(fun(Field, Format) -> lists:concat([Field, " = ", Format]) end, UpdateFields, UpdateFieldsFormat), ", "),
	%% split 3 part sql for parser use
	InsertDefine = io_lib:format("-define(INSERT_UPDATE_~s, {<<\"INSERT INTO `~s` (~s) VALUES \">>, ", [UpperName, Name, InsertFields]),
	ValueDefine = io_lib:format("<<\"(~s)\">>", [InsertFieldsFormat]),
	UpdateDefine = io_lib:format(", <<\" ON DUPLICATE KEY UPDATE ~s\">>}).\n", [UpdateFieldsClause]),
	lists:concat([InsertDefine, ValueDefine, UpdateDefine]).

%% select join
parse_define_select_join(_Name, _, [], _Fields) ->
	[];
parse_define_select_join(Name, KeysFilter, Keys, Fields) ->
	parse_define_select_join(Name, " WHERE ", KeysFilter, Keys, Fields).
parse_define_select_join(Name, Where, KeysFilter, Keys, Fields) ->
	UpperName = string:to_upper(Name),
	%% field @todo type revise IF_NULL
	SelectJoinFields = string:join([tool:default(OuterField, InnerField) || {_, _, InnerField, OuterField} <- Fields], ", "),
	%% key
	SelectJoinKeys = [lists:concat(["LEFT JOIN ", Table, " ON", InnerField, " = ", OuterField]) || {_, Table, InnerField, OuterField} <- Keys],
	%% key
	SelectKeys = listing:collect(#field.field, KeysFilter),
	SelectKeysFormat = listing:collect(#field.format, KeysFilter),
	SelectKeysClause = string:join(lists:zipwith(fun(Key, Format) -> lists:concat([Key, " = ", Format]) end, SelectKeys, SelectKeysFormat), " AND "),
	%% select join key must primary and unique
	io_lib:format("-define(SELECT_JOIN_~s, <<\"SELECT ~s FROM `~s`~s~s~s\">>).\n", [UpperName, SelectJoinFields, Name, SelectJoinKeys, Where, SelectKeysClause]).

%%%====================================================================
%%% code style part
%%%====================================================================
%% code style
chose_style(direct, Record, Keys, Fields) ->
	parse_code_fields_style_direct(Record, Keys, Fields).

%% get arg directly (Record#record.field)
parse_code_fields_style_direct(Record, Keys, Fields) ->
	Args = string:join(listing:collect_into(#field.name, Keys ++ Fields, fun(Name) -> lists:concat([maker:hump(Record), "#", Record, ".", Name]) end), ",\n        "),
	"\n        " ++ Args ++ "\n    ".


%%%====================================================================
%%% code part
%%%====================================================================
%% insert codeN
parse_code_insert(Name, Fields) ->
	UpperName = string:to_upper(Name),
	HumpName = maker:hump(Name),
	io_lib:format("\n%% @doc insert\ninsert(~s) ->
    Sql = parser:format(?INSERT_~s, [~s]),
    sql:insert(Sql).\n\n", [HumpName, UpperName, Fields]).

%% delete code
parse_code_delete(Name, Keys, Fields) ->
	UpperName = string:to_upper(Name),
	io_lib:format("%% @doc delete\ndelete(~s) ->
    Sql = parser:format(?DELETE_~s, [~s]),
    sql:delete(Sql).\n\n", [Keys, UpperName, Fields]).

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

%% batch insert code (with flag)
parse_code_insert_update(Name, Fields, Flag) ->
	UpperName = string:to_upper(Name),
	HumpName = maker:hump(Name),
	io_lib:format("\n%% @doc insert_update\ninsert_update(Data) ->
    F = fun(~s) -> [~s] end,
    {Sql, NewData} = parser:collect(Data, F, ?INSERT_UPDATE_~s, ~s),
    sql:insert(Sql),
    NewData.\n\n", [HumpName, Fields, UpperName, hd(tool:default(Flag, [[]]))]).

%% select join other table
parse_code_select_join(_Name, [], _Fields) ->
	[];
parse_code_select_join(Name, Args, Fields) ->
	UpperName = string:to_upper(Name),
	io_lib:format("%% @doc select join\nselect_join(~s) ->
    Sql = parser:format(?SELECT_JOIN_~s, [~s]),
    sql:select(Sql).\n\n", [Args, UpperName, Fields]).

%%%====================================================================
%%% tool part
%%%====================================================================

%% content contain substring check
contain(Content, What) when is_binary(Content) ->
	contain(binary_to_list(Content), What);
contain(Content, What) ->
	string:str(Content, What) =/= 0.

%% extract
extract(Content, Match) when is_binary(Content) ->
	extract(binary_to_list(Content), Match);
extract(Content, Match) ->
	extract(Content, Match, [{capture, first, list}]).
extract(Content, Match, Option) ->
	case re:run(Content, Match, Option) of
		{match, Result} ->
			Result;
		_ ->
			[]
	end.


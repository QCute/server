%%%-------------------------------------------------------------------
%%% @doc
%%% module sql maker
%%% database fields to sql code tool
%%% @end
%%%-------------------------------------------------------------------
-module(sql_maker).
-export([start/1]).
-export([parse/2]).
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
%% extra shell param : (select/select join all data without key constraint)
%%     select all
%%     join all
%%
-define(MATCH_OPTION, [{capture, first, list}]).
-define(MATCH_ALL_OPTION, [global, {capture, first, list}]).
-define(MATCH_JOIN, "(?<=on\\()(`?\\w+`?\\.`?\\w+`?)(?=\\))").
-define(MATCH_JOIN_TABLE, "(?<=on\\()`?\\w+`?(?=\\.)").
%%%===================================================================
%%% API
%%%===================================================================
%% @doc for shell
start(List) ->
	maker:start(fun parse_table/2, [List]).

%% @doc parse
parse(DataBase, One) ->
	parse_table(DataBase, One).
%%%====================================================================
%%% Internal functions
%%%====================================================================
%% parse per table
parse_table(DataBase, {File, Table, Includes}) ->
	parse_table(DataBase, {File, Table, Table, Includes});
parse_table(DataBase, {File, Table, Record, Includes}) ->
	FieldsSql = io_lib:format(<<"SELECT `COLUMN_NAME`, `COLUMN_DEFAULT`, `COLUMN_TYPE`, `COLUMN_COMMENT`, `ORDINAL_POSITION`, `COLUMN_KEY`, `EXTRA` FROM information_schema.`COLUMNS` WHERE `TABLE_SCHEMA` = '~s' AND `TABLE_NAME` = '~s' ORDER BY `ORDINAL_POSITION`;">>, [DataBase, Table]),
	%% fetch table fields
	RawFields = maker:select(FieldsSql),
	%% convert type to format
	F = fun(<<"char", _/binary>>) -> "'~s'"; (_) -> "'~w'" end,
	%% char(0)/varchar(0) same as (ignore) in comment
	FF = fun(C, <<"char(0)", _/binary>>) -> <<C/binary, <<"(ignore)">>/binary>>; (C, <<"varchar(0)", _/binary>>) -> <<C/binary, <<"(ignore)">>/binary>>; (C, _) -> C end,
	AllFields = [[N, D, F(T), FF(C, T), P, K, E] || [N, D, T, C, P, K, E] <- RawFields],
	%% primary key fields
	Primary = [X || X = [_, _, _, _, _, K, _] <- AllFields, K == <<"PRI">>],
	%% non primary key fields
	Normal = [X || X = [_, _, _, _, _, K, _] <- AllFields, K =/= <<"PRI">>],
	%% upper name
	UpperName = string:to_upper(lists:concat([Table])),
	case Primary of
		[] ->
			%% no primary key, cannot do anything
			erlang:error("could not found any primary key");
		_ ->
			%% return data
			Head = parse_head(File, Includes),
			Code = parse_code(UpperName, Table, Record, AllFields, Primary, Normal),
			Head ++ Code
	end.

%% sql code head
parse_head(File, Includes) ->
	[Module | _] = string:tokens(hd(lists:reverse(string:tokens(File, "/"))), "."),
	%% head
	Head = io_lib:format("-module(~s).\n-compile(nowarn_export_all).\n-compile(export_all).\n", [Module]),
	HeadPattern = io_lib:format("-module\\(~s\\)\\.\n-compile\\(nowarn_export_all\\)\\.\n-compile\\(export_all\\)\\.\n?", [Module]),
	%% include
	Include = [lists:flatten(io_lib:format("-include(\"~s\").\n", [X])) || X <- Includes],
	IncludePattern = [{"-include\\(.*\\)\\.\\n?\\n?", "-include-"}, {"-include\\(.*\\)\\.\n?\n?", "", [global]}, {"-include-", Include ++ "\n"}],
	[{HeadPattern, Head} | IncludePattern].

%% parse all code
parse_code(UpperName, Name, Record, AllFields, Primary, Normal) ->
	%% select specified
	SelectKeys = [X || [_, _, _, C, _, _, _] = X <- AllFields, contain(C, "(select)")],
	DeleteKeys = [X || [_, _, _, C, _, _, _] = X <- AllFields, contain(C, "(delete)")],
	UpdateKeys = [X || [_, _, _, C, _, _, _] = X <- AllFields, contain(C, "(update)")],
	JoinKeys = [X || [_, _, _, C, _, _, _] = X <- AllFields, not contain(C, "(ignore)") andalso me(C, ?MATCH_JOIN)],
	%% fields
	UpdateFields = [X || [_, _, _, C, _, _, E] = X <- Normal, E =/= <<"auto_increment">> andalso not contain(C, "(ignore)") andalso not contain(C, "(once)")],
	InsertFields = [X || [_, _, _, C, _, _, E] = X <- AllFields, E =/= <<"auto_increment">> andalso not contain(C, "(ignore)")],
	UpdateIntoFields = [X || [_, _, _, C, _, _, _] = X <- AllFields, not contain(C, "(ignore)")],
	%% SelectFields = [X || [_, _, _, C, _, _, _] = X <- AllFields, contain(C, "(ignore)") orelse me(C, ?MATCH_JOIN)],
	%% JoinFields = [X || [_, _, _, C, _, _, _] = X <- AllFields, contain(C, "(ignore)") andalso me(C, ?MATCH_JOIN)],

	%% batch insert/update flag (for multi data use)
	UpdateIntoExtra = parse_update_into_define_extra(AllFields, Record),

	%%%%% sql define
	%% regular insert/update/select/delete define
	InsertDefine = parse_define_insert(UpperName, Name, Primary, [], InsertFields),
	UpdateDefine = parse_define_update(UpperName, Name, Primary, UpdateKeys, UpdateFields),
	SelectDefine = parse_define_select(UpperName, Name, Primary, SelectKeys, []),
	DeleteDefine = parse_define_delete(UpperName, Name, Primary, DeleteKeys, []),
	%% batch insert define
	UpdateIntoDefine = parse_define_update_into(UpperName, Name, Primary, [], UpdateIntoFields, UpdateFields, UpdateIntoExtra),
	%% join other table
	JoinDefine = parse_define_join(UpperName, Name, Primary, SelectKeys, parse_define_join_fields(Name, AllFields), parse_define_join_keys(Name, JoinKeys)),

	%%%%% sql code
	%% regular insert/update/select/delete code
	{InsertHumpName, NeedInsertFields} = chose_style(direct, insert, Record, [], [], InsertFields),
	InsertCode = parse_code_insert(Name, InsertHumpName, UpperName, NeedInsertFields),
	{UpdateHumpName, NeedUpdateFields} = chose_style(direct, update, Record, Primary, UpdateKeys, UpdateFields),
	UpdateCode = parse_code_update(Name, UpdateHumpName, UpperName, NeedUpdateFields),
	{SelectHumpName, NeedSelectFields} = chose_style(arity, select, Record, Primary, SelectKeys, []),
	SelectCode = parse_code_select(Name, SelectHumpName, UpperName, NeedSelectFields),
	{DeleteHumpName, NeedDeleteFields} = chose_style(arity, delete, Record, Primary, DeleteKeys, []),
	DeleteCode = parse_code_delete(Name, DeleteHumpName, UpperName, NeedDeleteFields),
	%% batch insert
	{UpdateIntoHumpName, NeedUpdateIntoFields} = chose_style(direct, into, Record, [], [], UpdateIntoFields),
	UpdateIntoCode = parse_code_update_into(Name, UpdateIntoHumpName, UpperName, NeedUpdateIntoFields, UpdateIntoExtra),
	%% join other table
	{SelectJoinHumpName, NeedSelectJoinFields} = chose_style(arity, join, Record, Primary, SelectKeys, []),
	SelectJoinCode = parse_code_select_join(Name, SelectJoinHumpName, UpperName, NeedSelectJoinFields, JoinDefine),

	%%%%% group define/code
	%% update group
	{UpdateGroupDefineList, UpdateGroupCodeList} = parse_group("update", UpperName, Name, Record, Primary, UpdateFields, UpdateKeys),
	%% delete group
	{DeleteGroupDefineList, DeleteGroupCodeList} = parse_group("delete", UpperName, Name, Record, Primary, Primary, UpdateKeys),

	%% sql overloaded code
	%%    {InsertOverloadedArity, OverloadedInsertFields} = chose_style(direct, Record, Primary, [], InsertFields),
	%%    InsertOverloaded = parse_code_insert(Name, InsertOverloadedArity, UpperName, OverloadedInsertFields),
	%%    {UpdateOverloadedArity, OverloadedUpdateFields} = chose_style(direct, Record, Primary, UpdateKeys, UpdateFields),
	%%    UpdateOverloaded = parse_code_update(Name, UpdateOverloadedArity, UpperName, OverloadedUpdateFields),
	%%    {SelectOverloadedArity, OverloadedSelectFields} = chose_style(direct, Record, Primary, SelectKeys, []),
	%%    SelectOverloaded = parse_code_select(Name, SelectOverloadedArity, UpperName, OverloadedSelectFields),
	%%    {DeleteOverloadedArity, OverloadedDeleteFields} = chose_style(direct, Record, Primary, DeleteKeys, []),
	%%    DeleteOverloaded = parse_code_delete(Name, DeleteOverloadedArity, UpperName, OverloadedDeleteFields),

	%% return all replace pattern and code
	[InsertDefine, UpdateDefine, SelectDefine, DeleteDefine, UpdateIntoDefine, JoinDefine] ++ UpdateGroupDefineList ++ DeleteGroupDefineList ++ [UpdateIntoCode, InsertCode, UpdateCode, SelectCode, DeleteCode, SelectJoinCode] ++ UpdateGroupCodeList ++ DeleteGroupCodeList.

	%% [InsertDefine, UpdateDefine, SelectDefine, DeleteDefine] ++ DefineList ++ [InsertCode, InsertOverloaded, UpdateCode, UpdateOverloaded, SelectCode, SelectOverloaded, DeleteCode, DeleteOverloaded] ++ CodeList.


%%%====================================================================
%%% define part
%%%====================================================================
%% insert define
parse_define_insert(UpperName, Name, _Primary, _Keys, Fields) ->
	%% fields without auto increment for insert
	InsertFields = parse_define_fields_name(Fields),
	InsertDataFormat = parse_define_fields_type(Fields),
	InsertDefine = io_lib:format("-define(INSERT_~s, <<\"INSERT INTO `~s` (~s) VALUES (~s)\">>).\n", [UpperName, Name, InsertFields, InsertDataFormat]),
	InsertPattern = io_lib:format("(?m)(^-define\\s*\\(\\s*INSERT_~s\\s*,.+?)(?=\\.$|\\.\\%)\\.\n?", [UpperName]),
	{InsertPattern, InsertDefine}.

%% update define
parse_define_update(UpperName, Name, Primary, [], Fields) ->
	parse_define_update(UpperName, Name, Primary, Primary, Fields);
parse_define_update(UpperName, Name, Primary, Keys, Fields) ->
	%% key
	CollectKeys = collect_default_key(update, Primary, Keys),
	PrimaryFields = parse_define_keys(CollectKeys),
	%% fields (update primary if update fields empty)
	UpdateDataFormat = parse_define_update_fields(Fields, Primary),
	UpdateDefine = io_lib:format("-define(UPDATE_~s, <<\"UPDATE `~p` SET ~s ~s\">>).\n", [UpperName, Name, UpdateDataFormat, PrimaryFields]),
	UpdatePattern = io_lib:format("(?m)(^-define\\s*\\(\\s*UPDATE_~s\\s*,.+?)(?=\\.$|\\.\\%)\\.\n?", [UpperName]),
	{UpdatePattern, UpdateDefine}.

%% select define
parse_define_select(UpperName, Name, Primary, Keys, Fields) ->
	%% key
	CollectKeys = collect_default_key(select, Primary, Keys),
	PrimaryFields = parse_define_keys(CollectKeys),
	%% fields
	SelectFields = erlang:max(parse_define_fields_name(Fields), "*"),
	SelectDefine = io_lib:format("-define(SELECT_~s, <<\"SELECT ~s FROM `~s` ~s\">>).\n", [UpperName, SelectFields, Name, PrimaryFields]),
	SelectPattern = io_lib:format("(?m)(^-define\\s*\\(\\s*SELECT_~s\\s*,.+?)(?=\\.$|\\.\\%)\\.\n?", [UpperName]),
	{SelectPattern, SelectDefine}.

%% delete define
parse_define_delete(UpperName, Name, Primary, Keys, Fields) ->
	%% key
	CollectKeys = collect_default_key(delete, Primary, Keys),
	PrimaryFields = parse_define_keys(CollectKeys),
	%% fields
	DeleteFields = erlang:max(parse_define_fields_name(Fields), ""),
	DeleteDefine = io_lib:format("-define(DELETE_~s, <<\"DELETE ~s FROM `~s` ~s\">>).\n", [UpperName, DeleteFields, Name, PrimaryFields]),
	DeletePattern = io_lib:format("(?m)(^-define\\s*\\(\\s*DELETE_~s\\s*,.+?)(?=\\.$|\\.\\%)\\.\n?", [UpperName]),
	{DeletePattern, DeleteDefine}.

%% batch insert define, with update (flag) in comment
parse_define_update_into(_UpperName, _Name, _Primary, _Keys, _FieldsInsert, _FieldsUpdate, []) ->
	[];
parse_define_update_into(UpperName, Name, _Primary, _Keys, FieldsInsert, FieldsUpdate, _Extra) ->
	InsertFields = parse_define_fields_name(FieldsInsert),
	InsertDataFormat = parse_define_fields_type(FieldsInsert),
	InsertDefine = io_lib:format("-define(UPDATE_INTO_~s, {<<\"INSERT INTO `~s` (~s) VALUES \">>, ", [UpperName, Name, InsertFields]),
	ValueDefine = io_lib:format("<<\"(~s)\">>", [InsertDataFormat]),
	UpdateDefine = io_lib:format(", <<\" ON DUPLICATE KEY UPDATE ~s\">>}).\n", [parse_update_into_define_fields(FieldsUpdate)]),
	InsertPattern = io_lib:format("(?m)(^-define\\s*\\(\\s*UPDATE_INTO_~s\\s*,.+?)(?=\\.$|\\.\\%)\\.\n?", [UpperName]),
	{InsertPattern, InsertDefine ++ ValueDefine ++ UpdateDefine}.

%% select join other table define, with join on() other table in comment
parse_define_join(_UpperName, _Name, _Primary, _Keys, _Fields, []) ->
	[];
parse_define_join(UpperName, Name, Primary, Keys, Fields, Extra) ->
	%% key
	CollectKeys = collect_default_key(join, Primary, Keys),
	PrimaryFields = parse_define_keys(CollectKeys, Name),
	%% fields
	SelectFields = erlang:max(parse_define_fields_name(Fields), "*"),
	SelectDefine = io_lib:format("-define(SELECT_JOIN_~s, <<\"SELECT ~s FROM `~s` ~s ~s\">>).\n", [UpperName, SelectFields, Name, Extra, PrimaryFields]),
	SelectPattern = io_lib:format("(?m)(^-define\\s*\\(\\s*SELECT_JOIN_~s\\s*,.+?)(?=\\.$|\\.\\%)\\.\n?", [UpperName]),
	{SelectPattern, SelectDefine}.

%%%====================================================================
%%% key data collection part
%%%====================================================================
%% parse define key name
parse_define_keys(Primary) ->
	parse_define_keys(Primary, []).

%% parse define key name with table name (join other table use)
parse_define_keys([], _) ->
	[];
parse_define_keys(Primary, []) ->
	%% key field without table
	"WHERE " ++ string:join([io_lib:format("`~s`", [N]) ++ " = " ++ T || [N, _, T, _, _, _, _] <- Primary], " AND ");
parse_define_keys(Primary, Table) ->
	%% key field with table (join other table need)
	"WHERE " ++ string:join([io_lib:format("`~s`.`~s`", [Table, N]) ++ " = " ++ T || [N, _, T, _, _, _, _] <- Primary], " AND ").

%% parse define fields name
parse_define_fields_name(Fields) ->
	F = fun(N) -> case contain(type:to_list(N), ".") of true -> N; _ -> io_lib:format("`~s`", [N]) end end,
	string:join([F(N) || [N, _, _, _, _, _, _] <- Fields], ", ").

%% parse define fields data type (~p, ~w, ~s)
parse_define_fields_type(Fields) ->
	string:join([T || [_, _, T, _, _, _, _] <- Fields], ", ").

%% parse define fields name, for update define use
parse_define_update_fields([], Primary) ->
	%% only keys (without fields), keys as fields
	parse_define_update_fields(Primary, []);
parse_define_update_fields(Fields, _) ->
	%% collect and format fields name
	F = fun(N) -> case contain(type:to_list(N), ".") of true -> N; _ -> io_lib:format("`~s`", [N]) end end,
	UpdateFields = [F(N) || [N, _, _, _, _, _, _] <- Fields],
	%% collect all data types
	Types = [T || [_, _, T, _, _, _, _] <- Fields],
	%% zip fields name and type
	string:join(lists:zipwith(fun(N, T) -> N ++ " = " ++ T end, UpdateFields, Types), ", ").

%% parse define fields name, for batch insert define use
parse_update_into_define_fields(Fields) ->
	string:join([io_lib:format("`~s` = VALUES(`~s`)", [N, N]) || [N, _, _, _, _, _, _] <- Fields], ", ").

%% parse define extra fields name, for batch insert define use
parse_update_into_define_extra(AllFields, Record) ->
	[io_lib:format("#~s.~s", [Record, N]) || [N, _, _, C, _, _, _] <- AllFields, contain(C, "(flag)")].

%% parse define keys, for select join
parse_define_join_keys(Name, JoinKeys) ->
	%% collect join table (multi table select join support)
	Tables = lists:usort([T || [_, _, _, C, _, _, _] <- JoinKeys, T <- rea(C, ?MATCH_JOIN_TABLE)]),
	%% collect join key expr (multi table select join support)
	Keys = [binary_to_list(list_to_binary(io_lib:format("`~s`.`~s` = ~s", [Name, N, K]))) || [N, _, _, C, _, _, _] <- JoinKeys, K <- rea(C, ?MATCH_JOIN)],
	%% collect all select join key
	string:join([" LEFT JOIN " ++ X ++ " ON " ++ string:join([J || J <- Keys, contain(J, X)], " AND ") || X <- Tables], "").

%% parse define fields, select join
parse_define_join_fields(Name, AllFields) ->
	%% join field give default value if null
	TC = fun("'~s'") -> "''"; (_) -> "0" end,
	F = fun(N, C, T) -> case contain(C, "(ignore)") =/= 0 andalso me(C, ?MATCH_JOIN) of true -> "IFNULL(" ++ re(C, ?MATCH_JOIN) ++ ", " ++ TC(T) ++ ")"; _ -> io_lib:format("`~s`.`~s`", [Name, N]) end end,
	[[F(N, C, T), D, T, C, P, K, E] || [N, D, T, C, P, K, E] <- AllFields].

%% with default key
collect_default_key(Type, Primary, []) ->
	case maker:check_param(Type, "all") of
		true ->
			[];
		_ ->
			Primary
	end;
collect_default_key(_Type, _Primary, Keys) ->
	Keys.

collect_default_key(update, Primary, [], []) ->
	%% update primary when update fields empty
	Primary ++ Primary;
collect_default_key(Type, Primary, [], Fields) ->
	case maker:check_param(Type, "all") of
		true ->
			Fields;
		_ ->
			Fields ++ Primary
	end;
collect_default_key(_Type, _Primary, Keys, Fields) ->
	Fields ++ Keys.

%%%====================================================================
%%% code part
%%%====================================================================
%% insert code
parse_code_insert(Table, HumpName, UpperName, Fields) ->
	parse_code_insert("", Table, HumpName, UpperName, Fields).
parse_code_insert(CodeName, _Table, HumpName, UpperName, Fields) ->
	Insert = io_lib:format("\n%% @doc insert\ninsert~s(~s) ->
    Sql = parser:format(?INSERT_~s, [~s]),
    sql:insert(Sql).\n\n", [CodeName, HumpName, UpperName, Fields]),
	InsertPattern = io_lib:format("(?m)(?s)(?<!\\S)(\n?%% @doc insert\ninsert~s\\s*\\(.+?)(?=\\.$|\\%)\\.\n?\n?", [CodeName]),
	{InsertPattern, Insert}.

%% update code
parse_code_update(Table, HumpName, UpperName, Fields) ->
	parse_code_update("", Table, HumpName, UpperName, Fields).
parse_code_update(CodeName, _Table, HumpName, UpperName, Fields) ->
	Update = io_lib:format("%% @doc update\nupdate~s(~s) ->
    Sql = parser:format(?UPDATE_~s, [~s]),
    sql:update(Sql).\n\n", [CodeName, HumpName, UpperName, Fields]),
	UpdatePattern = io_lib:format("(?m)(?s)(?<!\\S)(%% @doc update\nupdate~s\\s*\\(.+?)(?=\\.$|\\%)\\.\n?\n?", [CodeName]),
	{UpdatePattern, Update}.

%% select code
parse_code_select(Table, HumpName, UpperName, Fields) ->
	parse_code_select("", Table, HumpName, UpperName, Fields).
parse_code_select(CodeName, _Table, HumpName, UpperName, Fields) ->
	Select = io_lib:format("%% @doc select\nselect~s(~s) ->
    Sql = parser:format(?SELECT_~s, [~s]),
    sql:select(Sql).\n\n", [CodeName, HumpName, UpperName, Fields]),
	SelectPattern = io_lib:format("(?m)(?s)(?<!\\S)(%% @doc select\nselect~s\\s*\\(.+?)(?=\\.$|\\%)\\.\n?\n?", [CodeName]),
	{SelectPattern, Select}.

%% delete code
parse_code_delete(Table, HumpName, UpperName, Fields) ->
	parse_code_delete("", Table, HumpName, UpperName, Fields).
parse_code_delete(CodeName, _Table, HumpName, UpperName, Fields) ->
	Delete = io_lib:format("%% @doc delete\ndelete~s(~s) ->
    Sql = parser:format(?DELETE_~s, [~s]),
    sql:delete(Sql).\n\n", [CodeName, HumpName, UpperName, Fields]),
	DeletePattern = io_lib:format("(?m)(?s)(?<!\\S)(%% @doc delete\ndelete~s\\s*\\(.+?)(?=\\.$|\\%)\\.\n?\n?", [CodeName]),
	{DeletePattern, Delete}.

%% batch insert code (with flag)
parse_code_update_into(_Table, _HumpName, _UpperName, _Fields, []) ->
	[];
parse_code_update_into(Table, HumpName, UpperName, Fields, Extra) ->
	parse_code_update_into("", Table, HumpName, UpperName, Fields, Extra).
parse_code_update_into(CodeName, _Table, HumpName, UpperName, Fields, [Extra | _]) ->
	UpdateInto = io_lib:format("\n%% @doc update_into\nupdate_into~s(DataList) ->
    F = fun(~s) -> [~s] end,
    {Sql, NewData} = parser:collect(DataList, F, ?UPDATE_INTO_~s, ~s),
    sql:insert(Sql),
    NewData.\n\n", [CodeName, HumpName, Fields, UpperName, Extra]),
	UpdateIntoPattern = io_lib:format("(?m)(?s)(?<!\\S)(\n?%% @doc update_into\nupdate_into~s\\s*\\(.+?)(?=\\.$|\\%)\\.\n?\n?", [CodeName]),
	{UpdateIntoPattern, UpdateInto}.

%% select join other table
parse_code_select_join(_Table, _HumpName, _UpperName, _Fields, []) ->
	[];
parse_code_select_join(Table, HumpName, UpperName, Fields, JoinDefine) ->
	parse_code_select_join("", Table, HumpName, UpperName, Fields, JoinDefine).
parse_code_select_join(CodeName, _Table, HumpName, UpperName, Fields, _JoinDefine) ->
	SelectJoin = io_lib:format("%% @doc select join\nselect_join~s(~s) ->
    Sql = parser:format(?SELECT_JOIN_~s, [~s]),
    sql:select(Sql).\n\n", [CodeName, HumpName, UpperName, Fields]),
	SelectJoinPattern = io_lib:format("(?m)(?s)(?<!\\S)(%% @doc select join\nselect_join~s\\s*\\(.+?)(?=\\.$|\\%)\\.\n?\n?", [CodeName]),
	{SelectJoinPattern, SelectJoin}.

%%%====================================================================
%%% code style part
%%%====================================================================
%% code style
chose_style(direct, Type, Record, Primary, Keys, Fields) ->
	parse_code_fields_style_direct(Type, Record, Primary, Keys, Fields);
chose_style(match, Type, Record, Primary, Keys, Fields) ->
	parse_code_fields_style_match(Type, Record, Primary, Keys, Fields);
chose_style(arity, Type, Record, Primary, Keys, Fields) ->
	parse_code_fields_style_arity(Type, Record, Primary, Keys, Fields).

%% get arg directly (Record#record.field)
parse_code_fields_style_direct(Type, Record, Primary, Keys, Fields) ->
	RawFields = collect_default_key(Type, Primary, Keys, Fields),
	%% key
	Hump = maker:hump(Record),
	Args = string:join([io_lib:format("~s#~s.~s", [Hump, Record, N]) || [N, _, _, _, _, _, _] <- RawFields], ",\n        "),
	{Hump, "\n        " ++ Args ++ "\n    "}.

%% match in args (Record = #record{field = Field})
parse_code_fields_style_match(Type, Record, Primary, Keys, Fields) ->
	RawFields = collect_default_key(Type, Primary, Keys, Fields),
	%% key
	HumpFields = [[{N, maker:hump(N)}, D, T, C, P, K, E] || [N, D, T, C, P, K, E] <- RawFields],
	Content = string:join([io_lib:format("~s = ~s", [N, H]) || [{N, H}, _, _, _, _, _, _] <- HumpFields], ", "),
	Arity = io_lib:format("#~s{~s} = ~s", [Record, Content, maker:hump(Record)]),
	Args = string:join([io_lib:format("~s", [H]) || [{_, H}, _, _, _, _, _, _] <- HumpFields], ",\n        "),
	{Arity, "\n        " ++ Args ++ "\n    "}.

%% match in args ([Field, ...])
parse_code_fields_style_arity(Type, _Record, Primary, Keys, Fields) ->
	RawFields = collect_default_key(Type, Primary, Keys, Fields),
	%% key
	HumpFields = [[maker:hump(N), D, T, C, P, K, E] || [N, D, T, C, P, K, E] <- RawFields],
	Content = string:join([io_lib:format("~s", [N]) || [N, _, _, _, _, _, _] <- HumpFields], ", "),
	Arity = io_lib:format("~s", [Content]),
	Args = string:join([io_lib:format("~s", [N]) || [N, _, _, _, _, _, _] <- HumpFields], ",\n        "),
	{Arity, "\n        " ++ Args ++ "\n    "}.

%%%====================================================================
%%% update group part (big table use)
%%%====================================================================
%% parse one group (select/update/delete)
parse_group(Type, UpperName, Name, Record, Primary, Normal, Keys) ->
	List = parse_comment(Normal, Type, []),
	make_group(List, Type, UpperName, Name, Record, Primary, Keys, [], []).

%% find all group and merge same group
parse_comment([], _, List) ->
	List;
parse_comment([[_, _, _, C, _, _, _] = H | T], Type, List) ->
	case re:run(C, "(?<=\\()" ++ Type ++ "_\\w+(?=\\))", [global, {capture, all, list}]) of
		{match, Result} ->
			Group = merge_group(Result, H, List),
			parse_comment(T, Type, Group);
		_ ->
			parse_comment(T, Type, List)
	end.

%% merge same group define
merge_group([], _, List) ->
	List;
merge_group([[K] | T], Data, List) ->
	case lists:keyfind(K, 1, List) of
		{_, Group} ->
			New = lists:keystore(K, 1, List, {K, [Data | Group]}),
			merge_group(T, Data, New);
		_ ->
			merge_group(T, Data, [{K, [Data]} | List])
	end.

%% make group code
make_group([], _, _, _, _, _, _, DefineList, CodeList) ->
	{DefineList, CodeList};
make_group([{K, All} | Tail], Type, UpperName, Name, Record, Primary, Keys, DefineList, CodeList) ->
	[_ | Suffix] = string:tokens(string:to_upper(K), "_"),
	%% trim update/delete_
	SuffixName = string:join(Suffix, "_"),
	%% upper suffix
	DefineName = lists:concat([UpperName, "_", SuffixName]),
	%% upper define name with suffix
	case Type of
		"update" ->
			Define = parse_define_update(DefineName, Name, Primary, Keys, All),
			%% chose code style
			{HumpName, Fields} = chose_style(arity, update, Record, Primary, Keys, All),
			%% lower code with suffix
			Code = parse_code_update(string:to_lower("_" ++ SuffixName), Name, HumpName, DefineName, Fields);
		"delete" ->
			Define = parse_define_delete(DefineName, Name, Primary, All, []),
			%% chose code style
			{HumpName, Fields} = chose_style(arity, delete, Record, Primary, All, []),
			%% lower code with suffix
			Code = parse_code_delete(string:to_lower("_" ++ SuffixName), Name, HumpName, DefineName, Fields)
	end,
	make_group(Tail, Type, UpperName, Name, Record, Primary, Keys, [Define | DefineList], [Code | CodeList]).

%%{Arity, Overloaded} = chose_style(arity, Record, Primary, Keys, []),
%% OverloadedCode = parse_code_delete(Name, Arity, UpperName, Overloaded),
%% make_group(Tail, UpperName, Name, Record, Primary, Keys, [Define | DefineList], [Code, OverloadedCode | CodeList]).

%%%====================================================================
%%% tool part
%%%====================================================================
%% regexp check
me(S, M) ->
	me(S, M, ?MATCH_OPTION).
me(S, M, O) ->
	re:run(binary_to_list(S), M, O) =/= nomatch.

%% regexp extract
re(S, M) ->
	re(S, M, ?MATCH_OPTION).
re(S, M, O) ->
	hd(element(2, re:run(binary_to_list(S), M, O))).

%% regexp extract all
rea(S, M) ->
	rea(S, M, ?MATCH_ALL_OPTION).
rea(S, M, O) ->
	lists:append(element(2, re:run(binary_to_list(S), M, O))).

%% content contain substring check
contain(Content, What) when is_binary(Content) ->
	contain(binary_to_list(Content), What);
contain(Content, What) ->
	string:str(Content, What) =/= 0.

%%% sql filter
%% DEFAULT PRIMARY KEY
%"SELECT `COLUMN_NAME`, `COLUMN_DEFAULT`, `DATA_TYPE`, `COLUMN_COMMENT`, `ORDINAL_POSITION`, `COLUMN_KEY`, `EXTRA` FROM information_schema.`COLUMNS` WHERE `TABLE_SCHEMA` = '~s' AND TABLE_NAME = '~s' AND `COLUMN_KEY` = 'PRI' ORDER BY ORDINAL_POSITION;",
%% select (select key specified)
%"SELECT `COLUMN_NAME`, `COLUMN_DEFAULT`, `DATA_TYPE`, `COLUMN_COMMENT`, `ORDINAL_POSITION`, `COLUMN_KEY`, `EXTRA` FROM information_schema.`COLUMNS` WHERE `TABLE_SCHEMA` = '~s' AND TABLE_NAME = '~s' AND `COLUMN_COMMENT` LIKE '%(select)%' ORDER BY ORDINAL_POSITION;",
%% delete (delete key specified)
%"SELECT `COLUMN_NAME`, `COLUMN_DEFAULT`, `DATA_TYPE`, `COLUMN_COMMENT`, `ORDINAL_POSITION`, `COLUMN_KEY`, `EXTRA` FROM information_schema.`COLUMNS` WHERE `TABLE_SCHEMA` = '~s' AND TABLE_NAME = '~s' AND `COLUMN_COMMENT` LIKE '%(delete)%' ORDER BY ORDINAL_POSITION;",
%% insert (without ignore/auto_increment)
%"SELECT `COLUMN_NAME`, `COLUMN_DEFAULT`, `DATA_TYPE`, `COLUMN_COMMENT`, `ORDINAL_POSITION`, `COLUMN_KEY`, `EXTRA` FROM information_schema.`COLUMNS` WHERE `TABLE_SCHEMA` = '~s' AND TABLE_NAME = '~s' AND `COLUMN_COMMENT` NOT LIKE '%(ignore%)%' AND `EXTRA` <> 'auto_increment' ORDER BY ORDINAL_POSITION;",
%% update (without ignore/once)
%"SELECT `COLUMN_NAME`, `COLUMN_DEFAULT`, `DATA_TYPE`, `COLUMN_COMMENT`, `ORDINAL_POSITION`, `COLUMN_KEY`, `EXTRA` FROM information_schema.`COLUMNS` WHERE `TABLE_SCHEMA` = '~s' AND TABLE_NAME = '~s' AND `COLUMN_COMMENT` NOT LIKE '%(ignore)%' AND `COLUMN_COMMENT` NOT LIKE '%(once)%' AND `COLUMN_KEY` <> 'PRI' ORDER BY ORDINAL_POSITION;",
%% update group (without ignore/once, use update% specified)
%"SELECT `COLUMN_NAME`, `COLUMN_DEFAULT`, `DATA_TYPE`, `COLUMN_COMMENT`, `ORDINAL_POSITION`, `COLUMN_KEY`, `EXTRA` FROM information_schema.`COLUMNS` WHERE `TABLE_SCHEMA` = '~s' AND TABLE_NAME = '~s' AND `COLUMN_COMMENT` NOT LIKE '%(ignore)%' AND `COLUMN_COMMENT` NOT LIKE '%(once)%' AND `COLUMN_KEY` <> 'PRI' AND `COLUMN_COMMENT` LIKE '%(update%)%' ORDER BY ORDINAL_POSITION;",

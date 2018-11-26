%%%-------------------------------------------------------------------
%%% @doc
%%% module database fields to sql code tool
%%% @end
%%%-------------------------------------------------------------------
-module(sql_maker).
-export([start/1]).
-export([parse/2]).
%% 全局数据更新替换
%% 插入字段不包含自增/(ignore)指定
%% 没有指定(update)使用主键更新，更新字段不包含自增/(once)/(ignore)指定
%% 没有指定(select)使用主键查询，查询全部
%% 没有指定(delete)使用主键删除，删除全部
%% 使用(update_???)可自定义更新组(同名一组，可多组)

%%%===================================================================
%%% API
%%%===================================================================
%% @doc for shell
start(List) ->
    maker:start(fun parse_table/2, [List]).

%% @doc parse
parse(DataBase, One) ->
    parse_table(DataBase, One).
%% ====================================================================
%% Internal functions
%% ====================================================================
%% parse per table
parse_table(DataBase, {File, Table, Includes}) ->
    parse_table(DataBase, {File, Table, Table, Includes});
parse_table(DataBase, {File, Table, Record, Includes}) ->
    FieldsSql = io_lib:format(<<"SELECT `COLUMN_NAME`, `COLUMN_DEFAULT`, `DATA_TYPE`, `COLUMN_COMMENT`, `ORDINAL_POSITION`, `COLUMN_KEY`, `EXTRA` FROM information_schema.`COLUMNS` WHERE `TABLE_SCHEMA` = '~s' AND TABLE_NAME = '~s' ORDER BY ORDINAL_POSITION;">>, [DataBase, Table]),
    %% fetch table fields
    RawFields = sql:select(DataBase, Table, FieldsSql),
    %% convert type to format
    F = fun(<<"char">>) -> "'~s'";(_) -> "'~w'" end,
    AllFields = [[N, D, F(T), C, P, K, E] || [N, D, T, C, P, K, E] <- RawFields],
    %% primary key fields
    Primary = [X || X = [_, _, _, _, _, K, _] <- AllFields, K == <<"PRI">>],
    %% non primary key fields
    Normal = [X || X = [_, _, _, _, _, K, _] <- AllFields, K =/= <<"PRI">>],
    
    UpperName = string:to_upper(lists:concat([Table])),
    case Primary of
        [] ->
            %% forbid
            [];
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
    HeadPatten = io_lib:format("-module\\(~s\\)\\.\n-compile\\(nowarn_export_all\\)\\.\n-compile\\(export_all\\)\\.\n?", [Module]),
    %% include
    Include = [lists:flatten(io_lib:format("-include(\"~s\").\n", [X])) || X <- Includes],
    %Include = ["-include(\"" ++ I ++ "\").\n" || I <- lists:usort(string:tokens(string:join(Includes, ","), ","))],
    IncludePatten = [{"-include\\(.*\\)\\.\\n?", "-include-"}, {"-include\\(.*\\)\\.\n?\n?", "", [global]}, {"-include-", Include ++ "\n"}],
    [{HeadPatten, Head} | IncludePatten].

parse_code(UpperName, Name, Record, AllFields, Primary, Normal) ->
    %% select specified
    SelectKeys = [X || [_, _, _, C, _, _, _] = X <- AllFields, string:str(binary_to_list(C), "(select)") =/= 0],
    DeleteKeys = [X || [_, _, _, C, _, _, _] = X <- AllFields, string:str(binary_to_list(C), "(delete)") =/= 0],
    UpdateKeys = [X || [_, _, _, C, _, _, _] = X <- AllFields, string:str(binary_to_list(C), "(update)") =/= 0],
    UpdateFields = [X || [_, _, _, C, _, _, E] = X <- Normal, E =/= <<"auto_increment">> andalso string:str(binary_to_list(C), "(ignore)") == 0 andalso string:str(binary_to_list(C), "(once)") == 0],
    InsertFields = [X || [_, _, _, C, _, _, E] = X <- AllFields, E =/= <<"auto_increment">> andalso string:str(binary_to_list(C), "(ignore)") == 0],

    %% sql define
    InsertDefine = parse_define_insert(UpperName, Name, Primary, [], InsertFields),
    UpdateDefine = parse_define_update(UpperName, Name, Primary, UpdateKeys, UpdateFields),
    SelectDefine = parse_define_select(UpperName, Name, Primary, SelectKeys, []),
    DeleteDefine = parse_define_delete(UpperName, Name, Primary, DeleteKeys, []),

    %% sql code
    {InsertHumpName, NeedInsertFields} = chose_style(direct, Record, [], [], InsertFields),
    InsertCode = parse_code_insert(Name, InsertHumpName, UpperName, NeedInsertFields),
    {UpdateHumpName, NeedUpdateFields} = chose_style(direct, Record, Primary, UpdateKeys, UpdateFields),
    UpdateCode = parse_code_update(Name, UpdateHumpName, UpperName, NeedUpdateFields),
    {SelectHumpName, NeedSelectFields} = chose_style(arity, Record, Primary, SelectKeys, []),
    SelectCode = parse_code_select(Name, SelectHumpName, UpperName, NeedSelectFields),
    {DeleteHumpName, NeedDeleteFields} = chose_style(arity, Record, Primary, DeleteKeys, []),
    DeleteCode = parse_code_delete(Name, DeleteHumpName, UpperName, NeedDeleteFields),
    %% update group
    {DefineList, CodeList} = parse_group(UpperName, Name, Record, Primary, UpdateFields, UpdateKeys),

    %% sql overloaded code
    %%    {InsertOverloadedArity, OverloadedInsertFields} = chose_style(direct, Record, Primary, [], InsertFields),
    %%    InsertOverloaded = parse_code_insert(Name, InsertOverloadedArity, UpperName, OverloadedInsertFields),
    %%    {UpdateOverloadedArity, OverloadedUpdateFields} = chose_style(direct, Record, Primary, UpdateKeys, UpdateFields),
    %%    UpdateOverloaded = parse_code_update(Name, UpdateOverloadedArity, UpperName, OverloadedUpdateFields),
    %%    {SelectOverloadedArity, OverloadedSelectFields} = chose_style(direct, Record, Primary, SelectKeys, []),
    %%    SelectOverloaded = parse_code_select(Name, SelectOverloadedArity, UpperName, OverloadedSelectFields),
    %%    {DeleteOverloadedArity, OverloadedDeleteFields} = chose_style(direct, Record, Primary, DeleteKeys, []),
    %%    DeleteOverloaded = parse_code_delete(Name, DeleteOverloadedArity, UpperName, OverloadedDeleteFields),

    %% update group
    {DefineList, CodeList} = parse_group(UpperName, Name, Record, Primary, UpdateFields, UpdateKeys),

    [InsertDefine, UpdateDefine, SelectDefine, DeleteDefine] ++ DefineList ++ [InsertCode, UpdateCode, SelectCode, DeleteCode] ++ CodeList.

    %% [InsertDefine, UpdateDefine, SelectDefine, DeleteDefine] ++ DefineList ++ [InsertCode, InsertOverloaded, UpdateCode, UpdateOverloaded, SelectCode, SelectOverloaded, DeleteCode, DeleteOverloaded] ++ CodeList.

%%%
%%% define part
%%%

parse_define_insert(UpperName, Name, _Primary, _Keys, Fields) ->
    %% fields without auto increment for insert
    InsertFields = parse_define_fields_name(Fields),
    InsertDataFormat = parse_define_fields_type(Fields),
    InsertDefine = io_lib:format("-define(INSERT_~s, <<\"INSERT INTO `~s` (~s) VALUES (~s)\">>).\n", [UpperName, Name, InsertFields, InsertDataFormat]),
    InsertPatten = io_lib:format("(?m)(^-define\\s*\\(\\s*INSERT_~s\\s*,.+?)(?=\\.|\\%)\\.\n?", [UpperName]),
    {InsertPatten, InsertDefine}.

parse_define_update(UpperName, Name, Primary, [], Fields) ->
    parse_define_update(UpperName, Name, Primary, Primary, Fields);
parse_define_update(UpperName, Name, _Primary, Keys, Fields) ->
    %% key
    PrimaryFields = parse_define_primary(Keys),
    %% fields
    UpdateKeyFields = parse_define_fields_name(Fields),
    UpdateDataFormat = parse_define_fields_type(Fields),
    UpdateDefine = io_lib:format("-define(UPDATE_~s, <<\"UPDATE `~p` SET (~s) VALUES (~s) WHERE ~s\">>).\n", [UpperName, Name, UpdateKeyFields, UpdateDataFormat, PrimaryFields]),
    UpdatePatten = io_lib:format("(?m)(^-define\\s*\\(\\s*UPDATE_~s\\s*,.+?)(?=\\.|\\%)\\.\n?", [UpperName]),
    {UpdatePatten, UpdateDefine}.

parse_define_select(UpperName, Name, Primary, [], Fields) ->
    parse_define_select(UpperName, Name, Primary, Primary, Fields);
parse_define_select(UpperName, Name, _Primary, Keys, Fields) ->
    %% key
    PrimaryFields = parse_define_primary(Keys),
    %% fields
    SelectFields = erlang:max(parse_define_fields_name(Fields), "*"),
    SelectDefine = io_lib:format("-define(SELECT_~s, <<\"SELECT ~s FROM `~s` WHERE ~s\">>).\n", [UpperName, SelectFields, Name, PrimaryFields]),
    SelectPatten = io_lib:format("(?m)(^-define\\s*\\(\\s*SELECT_~s\\s*,.+?)(?=\\.|\\%)\\.\n?", [UpperName]),
    {SelectPatten, SelectDefine}.

parse_define_delete(UpperName, Name, Primary, [], Fields) ->
    parse_define_delete(UpperName, Name, Primary, Primary, Fields);
parse_define_delete(UpperName, Name, _Primary, Keys, Fields) ->
    %% key
    PrimaryFields = parse_define_primary(Keys),
    %% fields
    DeleteFields = erlang:max(parse_define_fields_name(Fields), "*"),
    DeleteDefine = io_lib:format("-define(DELETE_~s, <<\"DELETE ~s FROM `~s` WHERE ~s\">>).\n", [UpperName, DeleteFields, Name, PrimaryFields]),
    DeletePatten = io_lib:format("(?m)(^-define\\s*\\(\\s*DELETE_~s\\s*,.+?)(?=\\.|\\%)\\.\n?", [UpperName]),
    {DeletePatten, DeleteDefine}.

%% key
parse_define_primary(Primary) ->
    string:join([io_lib:format("`~s`", [N]) ++ " = " ++ T || [N, _, T, _, _, _, _] <- Primary], " AND ").
%% fields name
parse_define_fields_name([]) ->
    [];
parse_define_fields_name(Fields) ->
    string:join([io_lib:format("`~s`", [N]) || [N, _, _, _, _, _, _] <- Fields], ", ").
%% fields type
parse_define_fields_type([]) ->
    [];
parse_define_fields_type(Fields) ->
    string:join([T || [_, _, T, _, _, _, _] <- Fields], ", ").

%%%
%%% code part
%%%
parse_code_insert(Table, HumpName, UpperName, Fields) ->
    parse_code_insert("", Table, HumpName, UpperName, Fields).
parse_code_insert(CodeName, Table, HumpName, UpperName, Fields) ->
    Insert = io_lib:format("\n%% @doc insert\ninsert~s(~s) ->
    Sql = io_lib:format(?INSERT_~s, [~s]),
    sql:execute(?POOL, ~s, Sql).\n\n", [CodeName, HumpName, UpperName, Fields, Table]),
    InsertPatten = io_lib:format("(?m)(?s)(?<!\\S)(\n?%% @doc insert\ninsert~s\\s*\\(.+?)(?=\\.$|\\%)\\.\n?\n?", [CodeName]),
    {InsertPatten, Insert}.

parse_code_update(Table, HumpName, UpperName, Fields) ->
    parse_code_update("", Table, HumpName, UpperName, Fields).
parse_code_update(CodeName, Table, HumpName, UpperName, Fields) ->
    Update = io_lib:format("%% @doc update\nupdate~s(~s) ->
    Sql = io_lib:format(?UPDATE_~s, [~s]),
    sql:execute(?POOL, ~s, Sql).\n\n", [CodeName, HumpName, UpperName, Fields, Table]),
    UpdatePatten = io_lib:format("(?m)(?s)(?<!\\S)(%% @doc update\nupdate~s\\s*\\(.+?)(?=\\.$|\\%)\\.\n?\n?", [CodeName]),
    {UpdatePatten, Update}.

parse_code_select(Table, HumpName, UpperName, Fields) ->
    parse_code_select("", Table, HumpName, UpperName, Fields).
parse_code_select(CodeName, Table, HumpName, UpperName, Fields) ->
    Select = io_lib:format("%% @doc select\nselect~s(~s) ->
    Sql = io_lib:format(?SELECT_~s, [~s]),
    sql:execute(?POOL, ~s, Sql).\n\n", [CodeName, HumpName, UpperName, Fields, Table]),
    SelectPatten = io_lib:format("(?m)(?s)(?<!\\S)(%% @doc select\nselect~s\\s*\\(.+?)(?=\\.$|\\%)\\.\n?\n?", [CodeName]),
    {SelectPatten, Select}.

parse_code_delete(Table, HumpName, UpperName, Fields) ->
    parse_code_delete("", Table, HumpName, UpperName, Fields).
parse_code_delete(CodeName, Table, HumpName, UpperName, Fields) ->
    Delete = io_lib:format("%% @doc delete\ndelete~s(~s) ->
    Sql = io_lib:format(?DELETE_~s, [~s]),
    sql:execute(?POOL, ~s, Sql).\n\n", [CodeName, HumpName, UpperName, Fields, Table]),
    DeletePatten = io_lib:format("(?m)(?s)(?<!\\S)(%% @doc delete\ndelete~s\\s*\\(.+?)(?=\\.$|\\%)\\.\n?\n?", [CodeName]),
    {DeletePatten, Delete}.


%%%
%%% code style part
%%%

%% code style
chose_style(direct, Record, Primary, Keys, Fields) ->
    parse_code_fields_style_direct(Record, Primary, Keys, Fields);
chose_style(match, Record, Primary, Keys, Fields) ->
    parse_code_fields_style_match(Record, Primary, Keys, Fields);
chose_style(arity, Record, Primary, Keys, Fields) ->
    parse_code_fields_style_arity(Record, Primary, Keys, Fields).

%% get arg directly
parse_code_fields_style_direct(Record, Primary, Keys, Fields) ->
    case Keys of
        [] ->
            RawFields =  Fields ++ Primary;
        _ ->
            RawFields =  Fields ++ Keys
    end,
    %% key
    Hump = hump(Record),
    %%Args = string:join([begin F = fun("'~s'") -> io_lib:format("data:convert(~s#~s.~s)", [Hump, Record, N]);(_) -> io_lib:format("~s#~s.~s", [Hump, Record, N]) end, F(T) end || [N, _, T, _, _, _, _] <- RawFields], ",\n        "),
    Args = string:join([io_lib:format("~s#~s.~s", [Hump, Record, N]) || [N, _, _, _, _, _, _] <- RawFields], ",\n        "),
    {Hump, "\n        " ++ Args ++ "\n    "}.

%% match in args
parse_code_fields_style_match(Record, Primary, Keys, Fields) ->
    case Keys of
        [] ->
            RawFields =  Fields ++ Primary;
        _ ->
            RawFields =  Fields ++ Keys
    end,
    HumpFields = [[{N, hump(N)}, D, T, C, P, K, E] || [N, D, T, C, P, K, E] <- RawFields],
    Content = string:join([io_lib:format("~s = ~s", [N, H]) || [{N, H}, _, _, _, _, _, _] <- HumpFields], ", "),
    Arity = io_lib:format("#~s{~s} = ~s", [Record, Content, hump(Record)]),
    %%Args = string:join([begin F = fun("'~s'") -> io_lib:format("data:convert(~s)", [N]);(_) -> io_lib:format("~s", [N]) end, F(T) end || [{_, N}, _, T, _, _, _, _] <- HumpFields], ",\n        "),
    Args = string:join([io_lib:format("~s", [H]) || [{_, H}, _, _, _, _, _, _] <- HumpFields], ",\n        "),
    {Arity, "\n        " ++ Args ++ "\n    "}.

%% match in args
parse_code_fields_style_arity(_Record, Primary, Keys, Fields) ->
    case Keys of
        [] ->
            RawFields =  Fields ++ Primary;
        _ ->
            RawFields =  Fields ++ Keys
    end,
    HumpFields = [[hump(N), D, T, C, P, K, E] || [N, D, T, C, P, K, E] <- RawFields],
    Content = string:join([io_lib:format("~s", [N]) || [N, _, _, _, _, _, _] <- HumpFields], ", "),
    Arity = io_lib:format("~s", [Content]),
    %%Args = string:join([begin F = fun("'~s'") -> io_lib:format("data:convert(~s)", [N]);(_) -> io_lib:format("~s", [N]) end, F(T) end || [N, _, T, _, _, _, _] <- HumpFields], ",\n        "),
    Args = string:join([io_lib:format("~s", [N]) || [N, _, _, _, _, _, _] <- HumpFields], ",\n        "),
    {Arity, "\n        " ++ Args ++ "\n    "}.


hump(Binary) when is_binary(Binary) ->
    hump(binary_to_list(Binary));
hump(Atom) when is_atom(Atom) ->
    hump(atom_to_list(Atom));
hump(Name) ->
    lists:concat([[case 96 < H andalso H < 123 of true -> H - 32; _ -> H end | T] || [H | T] <- string:tokens(Name, "_")]).


%%%
%%% update group (big table use)
%%%
parse_group(UpperName, Name, Record, Primary, Normal, Keys) ->
    List = parse_comment(Normal, []),
    make_group(List, UpperName, Name, Record, Primary, Keys, [], []).

%% for update group
parse_comment([], List) ->
    List;
parse_comment([[_, _, _, C, _, _, _] = H | T], List) ->
    case re:run(C, "(?<=\\()update_\\w+(?=\\))", [global, {capture, all, list}]) of
        {match, Result} ->
            Group = parse_group(Result, H, List),
            parse_comment(T, Group);
        _ ->
            parse_comment(T, List)
    end.

parse_group([], _, List) ->
    List;
parse_group([[K] | T], Data, List) ->
    %% K = [C || C <- H, C =/= $( andalso C =/= $)],
    case lists:keyfind(K, 1, List) of
        {_, Group} ->
            New = lists:keystore(K, 1, List, {K, [Data | Group]}),
            parse_group(T, Data, New);
        _ ->
            parse_group(T, Data, [{K, [Data]} | List])
    end.

make_group([], _, _, _, _, _, DefineList, CodeList) ->
    {DefineList, CodeList};
make_group([{K, All} | Tail], UpperName, Name, Record, Primary, Keys, DefineList, CodeList) ->
    [_ | Suffix] = string:tokens(string:to_upper(K), "_"),
    %% upper suffix
    DefineName = lists:concat([UpperName, "_", lists:flatten(Suffix)]),
    %% upper define name with suffix
    Define = parse_define_update(DefineName, Name, Primary, Keys, All),
    %% chose code style
    {InsertHumpName, UpdateFields} = chose_style(arity, Record, Primary, Keys, All),
    %% lower code with suffix
    Code = parse_code_update(string:to_lower("_" ++ DefineName), Name, InsertHumpName, DefineName, UpdateFields),
    make_group(Tail, UpperName, Name, Record, Primary, Keys, [Define | DefineList], [Code | CodeList]).
    %%{Arity, Overloaded} = chose_style(arity, Record, Primary, Keys, []),
    %% OverloadedCode = parse_code_delete(Name, Arity, UpperName, Overloaded),
    %% make_group(Tail, UpperName, Name, Record, Primary, Keys, [Define | DefineList], [Code, OverloadedCode | CodeList]).



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

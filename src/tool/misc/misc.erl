%%%-------------------------------------------------------------------
%%% @doc
%%% misc code
%%% @end
%%%-------------------------------------------------------------------
-module(misc).
-compile(nowarn_deprecated_function).
-compile(nowarn_export_all).
-compile(export_all).
%% Includes

%% +-----------------------------------------------------------------------------------------+
%% | process flag and exit operation                                                         |
%% | trap_exit flag will converted exit message to {'EXIT', From, Reason}                    |
%% +-----------+--------+--------------------------------------------------------------------+
%% | trap_exit | signal | action                                                             |
%% +-----------+--------+--------------------------------------------------------------------+
%% | true      | kill   | Die: Broadcast the exit signal killed to the linked processes.     |
%% | true      | X      | Add {'EXIT', Pid, X} to the mailbox.                               |
%% | false     | normal | Continue: Do-nothing signal vanishes                               |
%% | false     | kill   | Die: Broadcast the exit signal killed to the linked processes.     |
%% | false     | X      | Die: Broadcast the exit signal X to the linked processes           |
%% +-----------+--------+--------------------------------------------------------------------+


%%%===================================================================
%%% API functions
%%%===================================================================

%% @doc escript entry
-spec main([list() | atom()]) -> term().
main([]) ->
    make();
main(["clean"]) ->
    clean();
main(["maker"]) ->
    maker();
main(["beam"]) ->
    beam();
main(["pt", File]) ->
    main(["protocol", File]);
main(["protocol", Name | T]) ->
    protocol(Name, T);
main([Name | T]) ->
    script(Name, T).

%% compile file
make() ->
    file:set_cwd(script_path()),
    make:all(),
    ok.

%% clean beam file
clean() ->
    file:set_cwd(script_path() ++ "../beam"),
    case os:type() of
        {win32, _} ->
            Cmd = "powershell rm ";
        _ ->
            Cmd = "rm "
    end,
    os:cmd(Cmd ++ "*.beam"),
    ok.

%% compile maker
maker() ->
    file:set_cwd(script_path() ++ "../script/make/*/*maker"),
    make:all(),
    ok.

%% update user_default file include
beam() ->
    Path = script_path(),
    update_include(),
    os:cmd("erlc +debug_info -o " ++ Path ++ "../beam/ " ++ Path ++ "../src/debug/user_default.erl"),
    ok.

%% make protocol file
protocol(Name, T) ->
    Path = script_path(),
    Cmd = lists:flatten(lists:concat(["escript ", Path, "../src/make/protocol/protocol_script_", Name, ".erl ", T])),
    os:cmd(Cmd),
    ok.

%% get script file path
script(Name, T) ->
    Path = script_path(),
    Cmd = lists:flatten(lists:concat(["escript ", Path, "../src/make/script/", Name, "_script.erl ", T])),
    os:cmd(Cmd),
    ok.

%% get script path
script_path() ->
    Name = escript:script_name(),
    string:sub_string(Name, 1, max(string:rstr(Name, "/"), string:rstr(Name, "\\"))).

%% update all include
update_include() ->
    %% src/debug dir by default
    Name = escript:script_name(),
    Path = string:sub_string(Name, 1, max(string:rstr(Name, "/"), string:rstr(Name, "\\"))),
    update_include(Path ++ "user_default.erl", Path, "../../include/").
update_include(FilePath, ScriptPath, IncludePath) ->
    %% list all file
    {ok, LineList} = file:list_dir_all(ScriptPath ++ IncludePath),
    %% extract file name from file path
    Name = filename:basename(FilePath, ".erl"),
    %% construct include line
    Include = ["-include(\"" ++ IncludePath ++ Line ++ "\").\n" || Line <- LineList, string:str(Line, ".hrl") =/= 0],
    IncludePattern = "(?m)(^-include.+?)(?=\\.$)\\.\n?",
    %% construct data and pattern
    %% module declare
    Module = "-module(" ++ Name ++ ").\n",
    ModulePattern = "-module\\(" ++ Name ++ "\\)\\.\n",
    %% no warn declare
    NoWarn = "-compile(nowarn_export_all).\n",
    NoWarnPattern = "-compile\\(nowarn_export_all\\)\\.\n",
    %% export declare
    Export = "-compile(export_all).\n",
    ExportPattern = "-compile\\(export_all\\)\\.\n",
    %% read file data
    Data = binary_to_list(max(element(2, file:read_file(FilePath)), <<>>)),
    %% remove old data
    NewData = lists:foldr(fun(P, L) -> re:replace(L, P, "", [global, {return, list}]) end, Data, [ModulePattern, NoWarnPattern, ExportPattern, IncludePattern]),
    %% concat head include and other origin code
    file:write_file(FilePath, Module ++ NoWarn ++ Export ++ Include ++ NewData),
    ok.

%% format config file
format_config(File) ->
    case file:consult(File) of
        {ok, [Config]} ->
            file:write_file(File, lists:concat([format_config(Config, 1, []), "."]));
        Error ->
            Error
    end.
format_config([], Depth, String) ->
    Padding = lists:duplicate((Depth - 1) * 4, 16#20),
    ["[\n", string:join(lists:reverse(String), ",\n"), "\n", Padding, "]"];
format_config([{Key, Value} | T], Depth, String) ->
    case is_atom(Value) orelse is_number(Value) orelse io_lib:printable_list(Value) of
        true ->
            Padding = lists:duplicate(Depth * 4, 16#20),
            Align = lists:duplicate(52 - (Depth * 4 + 1 + length(lists:concat([Key]))), 16#20),
            NewString = io_lib:format("~s{~p,~s~tp}", [Padding, Key, Align, Value]),
            format_config(T, Depth, [NewString | String]);
        false ->
            Padding = lists:duplicate(Depth * 4, 16#20),
            NewString = io_lib:format("~s{~p, ~ts}", [Padding, Key, format_config(Value, Depth + 1, [])]),
            format_config(T, Depth, [NewString | String])
    end.

%%%===================================================================
%%% console debug assist
%%%===================================================================
%% @doc clear console
cls() ->
    cmd(clear).

%% @doc recompile and reload module
cc() ->
    cc(?MODULE, [debug_info, {d, 'DEBUG', true}]).
cc(Module) ->
    cc(Module, [debug_info, {d, 'DEBUG', true}]).
cc(Module, Option) ->
    %% in config dir by default
    cc(Module, "src/", "include/", "beam/", Option).
cc(Module, SrcPath, IncludePath, BeamPath, Option) ->
    %% locate file
    case cmd(find, [SrcPath, lists:concat([Module, ".erl"])]) of
        [] ->
            {error, nofile};
        [Result | _] ->
            %% recompile and reload it
            c:c(Result, [{i, IncludePath}, {outdir, BeamPath} | Option])
    end.

%% @doc shell command
cmd(Type) ->
    cmd(Type, []).
cmd(Type, Args) ->
    cmd(Type, Args, os:type()).
cmd(clear, _, {win32, _}) ->
    spawn(fun() -> os:cmd("powershell clear") end);
cmd(clear, _, {unix, _}) ->
    %% the wsl
    spawn(fun() -> (lists:any(fun(Path) -> string:str(Path, "/Windows/system32") =/= 0 end, string:tokens(os:getenv("PATH"), ":")) andalso os:cmd("powershell.exe clear") == []) orelse io:format("\e[H\e[J") end);
cmd(list, [Path], {win32, _}) ->
    string:tokens(os:cmd(lists:concat(["dir /b ", Path])), cmd(line));
cmd(list, [Path], {unix, _}) ->
    string:tokens(os:cmd(lists:concat(["ls ", Path])), cmd(line));
cmd(remove, _, {win32, _}) ->
    "del ";
cmd(remove, _, {unix, _}) ->
    "rm ";
cmd(line, _, {win32, _}) ->
    "\r\n";
cmd(line, _, {unix, _}) ->
    "\n";
cmd(path, [], {win32, _}) ->
    $\\;
cmd(path, [], {unix, _}) ->
    $/;
cmd(path, [Path], {win32, _}) ->
    lists:foldr(fun($/, A) -> [$\\ | A];(C, A) -> [C | A] end, [], Path);
cmd(path, [Path], {unix, _}) ->
    lists:foldr(fun($\\, A) -> [$/ | A];(C, A) -> [C | A] end, [], Path);
cmd(find, [Path, Target], {win32, _}) ->
    string:tokens(os:cmd(lists:concat(["where /R ", cmd(path, [Path]), " ", Target, " 2>nul"])), cmd(line));
cmd(find, [Path, Target], {unix, _}) ->
    string:tokens(os:cmd(lists:concat(["find ", cmd(path, [Path]), " -name ", Target, " 2>/dev/null"])), cmd(line)).


%%%===================================================================
%%% performance tool
%%%===================================================================


ts_exception() ->

    {LoopOnly, _} = timer:tc(?MODULE, loop_only, [1000000, none, undefined]),

    {LoopInTryCatchOnly, _} = timer:tc(?MODULE, loop_in_try_catch_only, [1000000, none, undefined]),
    {LoopInTryCatchStacktraceOnly, _} = timer:tc(?MODULE, loop_in_try_catch_stacktrace_only, [1000000, none, undefined, undefined]),
    {LoopInCatchOnly, _} = timer:tc(?MODULE, loop_in_catch_only, [1000000, none]),

    {TryCatchThrow, _} = timer:tc(?MODULE, loop_try_catch, [1000000, throw, undefined]),
    {TryCatchError, _} = timer:tc(?MODULE, loop_try_catch, [1000000, error, undefined]),
    {TryCatchExit, _} = timer:tc(?MODULE, loop_try_catch, [1000000, exit, undefined]),

    {TryCatchStacktraceThrow, _} = timer:tc(?MODULE, loop_try_catch_stacktrace, [1000000, throw, undefined, undefined]),
    {TryCatchStacktraceError, _} = timer:tc(?MODULE, loop_try_catch_stacktrace, [1000000, error, undefined, undefined]),
    {TryCatchStacktraceExit, _} = timer:tc(?MODULE, loop_try_catch_stacktrace, [1000000, exit, undefined, undefined]),

    {CatchStacktraceThrow, _} = timer:tc(?MODULE, loop_catch, [1000000, throw]),
    {CatchStacktraceError, _} = timer:tc(?MODULE, loop_catch, [1000000, error]),
    {CatchStacktraceExit, _} = timer:tc(?MODULE, loop_catch, [1000000, exit]),


    io:format("LoopOnly:~tp~n", [LoopOnly]),
    io:format("LoopInTryCatchOnly:~tp~n", [LoopInTryCatchOnly]),
    io:format("LoopInTryCatchStacktraceOnly:~tp~n", [LoopInTryCatchStacktraceOnly]),
    io:format("LoopInCatchStacktraceOnly:~tp~n", [LoopInCatchOnly]),

    io:format("TryCatchThrow:~tp TryCatchError:~tp TryCatchExit:~tp~n", [TryCatchThrow, TryCatchError, TryCatchExit]),
    io:format("TryCatchStacktraceThrow:~tp TryCatchStacktraceError:~tp TryCatchStacktraceExit:~tp~n", [TryCatchStacktraceThrow, TryCatchStacktraceError, TryCatchStacktraceExit]),
    io:format("CatchStacktraceThrow:~tp CatchStacktraceError:~tp CatchStacktraceExit:~tp~n", [CatchStacktraceThrow, CatchStacktraceError, CatchStacktraceExit]).

loop_only(0, _, _) -> ok;
loop_only(Times, Class, _) ->
    X = make_an_exception(Class),
    loop_only(Times - 1, Class, X).

loop_in_try_catch_only(0, _, _) -> ok;
loop_in_try_catch_only(Times, Class, _) ->
    try
        X = make_an_exception(Class),
        loop_in_try_catch_only(Times - 1, Class, X)
    catch
        Class:Reason ->
            loop_in_try_catch_only(Times - 1, Class, Reason)
    end.

loop_in_try_catch_stacktrace_only(0, _, _, _) -> ok;
loop_in_try_catch_stacktrace_only(Times, Class, _, _) ->
    try
        X = make_an_exception(Class),
        loop_in_try_catch_stacktrace_only(Times - 1, Class, X, X)
    catch
        Class:Reason:Stacktrace ->
            loop_in_try_catch_stacktrace_only(Times - 1, Class, Reason, Stacktrace)
    end.

loop_in_catch_only(0, _) -> ok;
loop_in_catch_only(Times, Class) ->
    catch make_an_exception(Class),
    loop_in_catch_only(Times - 1, Class).

loop_try_catch(0, _, _) -> ok;
loop_try_catch(Times, Class, _) ->
    try
        make_an_exception(Class)
    catch
        Class:Reason ->
            loop_try_catch(Times - 1, Class, Reason)
    end.

loop_try_catch_stacktrace(0, _, _, _) -> ok;
loop_try_catch_stacktrace(Times, Class, _, _) ->
    try
        make_an_exception(Class)
    catch
        Class:Reason:Stacktrace ->
            loop_try_catch_stacktrace(Times - 1, Class, Reason, Stacktrace)
    end.

loop_catch(0, _) -> ok;
loop_catch(Times, Class) ->
    catch make_an_exception(Class),
    loop_catch(Times - 1, Class).

make_an_exception(Class) ->
    case Class of
        throw -> erlang:throw(ladies);
        error -> erlang:error(ladies);
        exit -> erlang:exit(ladies);
        _ -> nothing_wrong
    end.


%%%===================================================================
%%% performance tool
%%%===================================================================
%% test on OTP 21.3
%%
%% function(Arg)
%% module:function(arg)
%%
%% apply(Module, Function, Args)
%% Module:Function(Arg)
%% apply(fun Function/arity, Args)
%%
%% 2.x times slow
%% fun() -> ok end
%% fun Function/arity(Arg)
%%
%% performance
%% local > remote >  apply(f,a) ~= apply(m,f,a) ~= execute(m,f,a) >> execute(f,a)(lambda) > execute(f,a)
%%

test_apply() ->
    L = lists:seq(1, 10000000),
    LocalTime = os:timestamp(),
    [jp() || _ <- L],
    RemoteTime = os:timestamp(),
    [?MODULE:jp() || _ <- L],
    ApplyTime = os:timestamp(),
    [apply(?MODULE, jp, []) || _ <- L],
    ExecuteTime = os:timestamp(),
    Module = ?MODULE,
    Function = jp,
    [Module:Function() || _ <- L],
    ApplyFunctionTime = os:timestamp(),
    [apply(fun ?MODULE:jp/0, []) || _ <- L],
    ExecuteFunctionTime = os:timestamp(),
    F = fun ?MODULE:jp/0,
    [F() || _ <- L],
    LambdaFunctionTime = os:timestamp(),
    FF = fun() -> "にほんご" end,
    [apply(FF, []) || _ <- L],
    EndTime = os:timestamp(),
    io:format("Local:~p Remote:~p Apply(M,F,A):~p M:F(A):~p Apply(fun,A):~p F:(A):~p FF(A):~p~n", [timer:now_diff(RemoteTime, LocalTime), timer:now_diff(ApplyTime, RemoteTime), timer:now_diff(ExecuteTime, ApplyTime), timer:now_diff(ApplyFunctionTime, ExecuteTime), timer:now_diff(ExecuteFunctionTime, ApplyFunctionTime), timer:now_diff(LambdaFunctionTime, ExecuteFunctionTime), timer:now_diff(EndTime, LambdaFunctionTime)]),
    ok.

more_test() ->
    L = lists:seq(1, 1000),
    [test() || _ <- L],
    ok.
test() ->
    O = 10000000,
    L = lists:seq(1, O),
    Begin = os:timestamp(),
    %% first
    F = fun jp/0,
    [F() || _ <- L],
    %% [?MODULE:jp() || _ <- L],
    %% middle
    Middle = os:timestamp(),
    %% middle
    [apply(fun jp/0, []) || _ <- L],
    %% [apply(?MODULE, jp, []) || _ <- L],
    %% [?MODULE:jp() || _ <- L],
    %% second
    End = os:timestamp(),
    %% diff
    First = timer:now_diff(Middle, Begin) div 1000,
    Second = timer:now_diff(End, Middle) div 1000,
    io:format("First:~p   Second:~p~n", [First, Second]),
    ok.

qs(Pid, Sql) ->
    erlang:send(Pid, {query, self(), Sql}),
    receive
        {Pid, Result} ->
            Result
    end.

qd() ->
    receive
        {query, From, Sql} ->
            Self = erlang:self(),
            erlang:send(From, {Self, Sql}),
            qd()
    end.

%% にほんご
jp() ->
    "にほんご".
jps() ->
    <<"にほんご"/utf8>>.


%%%===================================================================
%%% shell script evaluate part
%%%===================================================================


%% @doc 取整 大于X的最小整数
-spec ceil(number()) -> integer().
ceil(X) ->
    case trunc(X) of
        X ->
            X;
        T when X > 0 ->
            T + 1;
        T ->
            T
    end.

%% @doc 取整 小于X的最大整数
-spec floor(number()) -> integer().
floor(X) ->
    case trunc(X) of
        X ->
            X;
        T when X > 0 ->
            T;
        T ->
            T - 1
    end.

%%%===================================================================
%%% general server
%%%===================================================================
start() ->
    start([]).
start(Args) ->
    gen_server:start({local, ?MODULE}, ?MODULE, Args, []).
start_link() ->
    start_link([]).
start_link(Args) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Args, []).
init(_) ->
    %% erlang:process_flag(trap_exit, true),
    {ok, []}.
handle_call(_Request, _From, State) ->
    {reply, ok, State}.
handle_cast(_Request, State) ->
    {noreply, State}.
handle_info(normal, State) ->
    {stop, normal, State};
handle_info(shutdown, State) ->
    {stop, shutdown, State};
handle_info({shutdown, Reason}, State) ->
    {stop, {shutdown, Reason}, State};
handle_info(_Request, State) ->
    io:format("handle_info:~p~n", [_Request]),
    {noreply, State}.
terminate(_Reason, State) ->
    io:format("terminate:~p~n", [_Reason]),
    {ok, State}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% characters test tool
%%%===================================================================
%% 一
%% <<228,184,128>>  .utf8      228*256*256 + 184*256 + 128   [14989440]
%% <<78,0>>         .unicode   78*256 + 0                    [19968]
%% <<210,187>>      .gbk       210*256+187                   [53947]

%% @doc file encoding test
ts() ->
    "一".

ts(String) ->
    io:setopts([{encoding, unicode}]),
    io:setopts(standard_error, [{encoding, unicode}]),
    io:format("\"~ts\"~n", [String]).

%%%===================================================================
%%% misc code
%%%===================================================================
map_reduce(F, L) ->
    Parent = self(),
    [spawn(fun() -> erlang:send(Parent, catch F(I)) end) || I <- L],
    [receive R -> R end || _ <- L].

%% not tail recursive function
append([H|T], Tail) ->
    [H|append(T, Tail)];
append([], Tail) ->
    Tail.



%%%===================================================================
%%% console debug assist
%%%===================================================================

%% @doc find module file from source path
-spec locate(Path :: string(), File :: file:filename()) -> [file:filename()].
locate(Path, File) ->
    {ok, FileList} = file:list_dir_all(Path),
    locate_loop(FileList, Path, File, []).

%% depth first search
locate_loop([], _, _, List) ->
    List;
locate_loop([Name | T], Path, File, List) ->
    SubFile = Path ++ "/" ++ Name,
    case filelib:is_dir(SubFile) of
        true ->
            %% sub dir recursion
            Result = locate(SubFile, File),
            locate_loop(T, Path, File, List ++ Result);
        false when Name == File ->
            locate_loop(T, Path, File, [SubFile | List]);
        _ ->
            locate_loop(T, Path, File, List)
    end.

-record(priority_queue, {size, key, left, right, queue}).

new(Key) ->
    #priority_queue{key = Key, size = 0, left = [], right = [], queue = []}.

query(Item, PriorityQueue = #priority_queue{key = Key, queue = Queue}) ->
    NewQueue = push_loop(Queue, Key, Item, []),
    PriorityQueue#priority_queue{queue = NewQueue}.

push_loop([], _, Item, List) ->
    lists:reverse([Item | List]);
push_loop([H | T], Key, Item, List) when element(Key, Item) >= element(Key, H) ->
    lists:reverse([H | List], T);
push_loop([H | T], Key, Item, List) ->
    push_loop(T, Key, Item, [H | List]).

priority_queue() ->
    Q = new(2),
    Q1 = query({1, 1}, Q),
    Q1 = query({2, 2}, Q),
    ok.

%% insert table initialized data
initialize_table(Id, Table) ->
    Sql = io_lib:format("
    SELECT
        GROUP_CONCAT('`', information_schema.`COLUMNS`.`COLUMN_NAME`, '`'),
        GROUP_CONCAT('''', information_schema.`COLUMNS`.`COLUMN_DEFAULT`, '''')
    FROM
        information_schema.`COLUMNS`
    WHERE
        information_schema.`COLUMNS`.`TABLE_SCHEMA` = DATABASE()
        AND
        information_schema.`COLUMNS`.`TABLE_NAME` = '~s'", [Table]),
    %% collect all fields and default value
    [[Fields, Default]] = db:select(Sql),
    %% strong match insert id equals given id
    Id = db:insert(io_lib:format("INSERT INTO `~s` (~s) VALUES ('~w', ~s)", [Table, Fields, Id, Default])).

%% @doc transform list data to record
transform(Table, CallBack) ->
    %% table name same as record name
    Sql = lists:concat(["SELECT * FROM `", Table, "`"]),
    transform(Sql, Table, Table, CallBack).
transform(Sql, Table, CallBack) ->
    %% table name same as record name
    transform(Sql, Table, Table, CallBack).
transform(Sql, Table, Record, CallBack) ->
    Data = db:select(Sql),
    %% load data delete first
    catch ets:delete_all_objects(Table),
    %% use callback transform data
    List = lists:foldl(fun(E, Acc) -> catch CallBack(list_to_tuple([Record | E]), Acc) end, [], Data),
    %% save to ets
    ets:insert(Table, List).

%%%===================================================================
%%% fix part, develop environment use
%%%===================================================================
%% fix sql
%% only add table, add field state
fix_sql(Sql, Code, Status, Message) ->
    fix_sql({mysql_error, binary_to_list(iolist_to_binary(Sql)), {Code, Status, Message}}).
fix_sql(Error = {mysql_error, Sql, {Code, _Status, Message}}) ->
    {Method, Table} = explain_sql_sentence(binary_to_list(iolist_to_binary(Sql))),
    case Code of
        1054 ->
            %% no field
            Field = parse_error_message(binary_to_list(iolist_to_binary(Message))),
            case find_alter_sentence(Table, Field) of
                {ok, Fix} ->
                    db:query(Fix),
                    ?MODULE:Method(Sql);
                _ ->
                    erlang:throw(Error)
            end;
        1146 ->
            %% no table
            case find_create_sentence(Table) of
                {ok, Fix} ->
                    db:query(Fix),
                    ?MODULE:Method(Sql);
                _ ->
                    erlang:throw(Error)
            end;
        _ ->
            erlang:throw(Error)
    end;
fix_sql(Result) ->
    Result.

%% explain sql sentence
explain_sql_sentence(Sql) ->
    case string:tokens(string:to_lower(Sql), " ") of
        ["insert", "into", Table | _] ->
            {execute, Table};
        ["insert", Table | _] ->
            {execute, Table};
        ["update", Table | _] ->
            {execute, Table};
        ["select" | T = [_ | _]] ->
            {get_all, lists:nth(listing:index("from", T) + 1, T)};
        _ ->
            {error, unknown_sql_sentence}
    end.

%% parse error message
parse_error_message(Message) ->
    case string:tokens(Message, " ") of
        ["Table", Table, "doesn't", "exist" | _] ->
            %% "Table 'main.test' doesn't exist"
            string:strip(hd(tl(string:tokens(Table, "."))), right, $');
        ["Unknown", "column", Field, "in", "'field", "list'" | _] ->
            %% "Unknown column 'field' in 'field list'"
            string:strip(Field, both, $');
        ["Incorrect", Type, "value:", Value, "for", "column", Field, "at", "row" | _] ->
            {Type, Value, Field};
        ["Out", "of", "range", "value", "for", "column", Field, "at", "row" | _] ->
            Field;
        _ ->
            {error, unknown_message_sentence}
    end.

%% create table sentence
find_create_sentence(Table) ->
    find_sql([Table, "CREATE TABLE"]).

%% alter table sentence
find_alter_sentence(Table, Field) ->
    case string:tokens(Field, ".") of
        [_, Name | _] ->
            Name;
        [Name] ->
            Name
    end,
    find_sql([Table, "ALTER TABLE", Field]).

%% read revise sql file
find_sql(Contain) ->
    %% update sql file
    find_sql("script/sql/update.sql", Contain).
find_sql(SqlFile, Contain) ->
    {ok, Binary} = file:read_file(SqlFile),
    String = binary_to_list(Binary),
    List = re:split(String, ";\\s*$", [{return, list}]),
    find_sql_loop(List, Contain).

%% find revise sql sentence
find_sql_loop([], _Contain) ->
    {error, no_such_sql};
find_sql_loop([H | T], Contain) ->
    case lists:all(fun(X) -> string:str(H, X) =/= 0 end, Contain) of
        true ->
            %% add separator
            {ok, H ++ ";"};
        false ->
            find_sql_loop(T, Contain)
    end.

%%%===================================================================
%%% take protocol result string translate text
%%%===================================================================
take() ->
    TextList = lists:flatten([take_text(File) || File <- filelib:wildcard("script/make/protocol/*.erl")]),
    Data = [[Protocol, Key, proplists:get_value(en, StringList, ""), proplists:get_value(zhCN, StringList, "")] || {{Protocol, Key}, StringList} <- TextList],
    db:query(<<"TRUNCATE `error_text_data`">>),
    Sql = parser:collect(Data, {<<"INSERT INTO `error_text_data` VALUES ">>, <<"(~w, '~s', '~s', '~s')">>}),
    db:insert(Sql).

take_text(File) ->
    {ok, Form} = epp:parse_file(File, [], []),
    IoForm = [Cons || {function, _, protocol, 0, [{clause, _, _, _, [{record, _, protocol, Fields} | _]} | _]} <- Form, {record_field, _, {atom, _, io}, Cons} <- Fields],
    take_io_text(hd(IoForm), []).

%% take io text
take_io_text({nil, _}, List) ->
    lists:reverse(List);
take_io_text({cons, _, {record, _, io, Fields}, Cons}, List) ->
    Protocol = hd([Protocol || {record_field, _, {atom, _, protocol}, {integer, _, Protocol}} <- Fields]),
    Text = [{Name, Key, String} || {record_field, _, {atom, _, text}, {map, _, Text}} <- Fields, {map_field_assoc, _, {atom, _, Name}, {map, _, Translate}} <- Text, {map_field_assoc, _, {atom, _, Key}, {string, _, String}} <- Translate],
    Map = [{{Protocol, Key}, [erlang:delete_element(2, String) || String <- StringList]} || {Key, StringList} <- listing:key_group(2, Text)],
    take_io_text(Cons, [Map | List]).

%%%===================================================================
%%% fill protocol result string translate text
%%%===================================================================
fill() ->
    Text = format_text(),
    [file:write_file(File, replace(take_text_line(File), Text, 0, element(2, file:read_file(File)))) || File <- filelib:wildcard("script/make/protocol/*.erl")],
    ok.

format_text() ->
    Padding = lists:duplicate(4, "    "),
    [{Protocol, unicode:characters_to_binary(io_lib:format("~stext = #{\n~ts\n~s},\n", [Padding, format_language(Protocol, 5), Padding]))} || Protocol <- db:select_column("SELECT DISTINCT `protocol` FROM `error_code_data`")].

format_language(Protocol, Depth) ->
    Padding = lists:flatten(lists:duplicate(Depth, "    ")),
    Data = db:select_column("SELECT `COLUMN_NAME` FROM information_schema.`COLUMNS` WHERE `TABLE_SCHEMA` = DATABASE() AND `TABLE_NAME` = 'error_code_data' AND `COLUMN_NAME` != 'protocol' AND `COLUMN_NAME` != 'key'"),
    lists:concat([Padding, string:join([io_lib:format("~s => #{\n~ts\n~s}", [N, format_translate(Protocol, N, Depth + 1), Padding]) || N <- Data], lists:concat([",\n", Padding]))]).

format_translate(Protocol, Name, Depth) ->
    Padding = lists:flatten(lists:duplicate(Depth, "    ")),
    Data = db:select("SELECT `key`, ~s FROM `error_code_data` WHERE `protocol` = ~w", [Name, Protocol]),
    lists:concat([Padding, string:join([io_lib:format("~s => \"~ts\"", [K, T]) || [K, T] <- Data], lists:concat([",\n", Padding]))]).

take_text_line(File) ->
    {ok, Form} = epp:parse_file(File, [], []),
    IoForm = [Cons || {function, _, protocol, 0, [{clause, _, _, _, [{record, _, protocol, Fields} | _]} | _]} <- Form, {record_field, _, {atom, _, io}, Cons} <- Fields],
    take_io_text_line(hd(IoForm), []).

%% take io text
take_io_text_line({nil, _}, List) ->
    lists:reverse(List);
take_io_text_line({cons, _, {record, _, io, Fields}, Cons}, List) ->
    Protocol = hd([Protocol || {record_field, _, {atom, _, protocol}, {integer, _, Protocol}} <- Fields]),
    case [Line || {record_field, Line, {atom, _, text}, _} <- Fields] of
        [Start] ->
            %% previous line
            End = lists:min([Line || {record_field, Line, _, _} <- Fields, Start =/= [], Start < Line]) - 1,
            take_io_text_line(Cons, [{Protocol, Start, End} | List]);
        [] ->
            take_io_text_line(Cons, List)
    end.

%% replace text data
replace([], _, _, Data) ->
    Data;
replace([{Protocol, Start, End} | T], Text, Revise, Data) ->
    {_, Replace} = lists:keyfind(Protocol, 1, Text),
    {NewRevise, NewData} = replace(Data, Start, End, Revise, Replace),
    replace(T, Text, NewRevise, NewData).

replace(Data, Start, End, Revise, Replace) ->
    List = array:from_list([{-1, -1} | binary:matches(Data, <<"\n">>)]),
    %% previous line start offset
    StartLine = Start - 1 + Revise,
    StartOffset = element(1, array:get(StartLine, List)) + 1,
    %% this line end offset
    EndLine = End + Revise,
    EndOffset = element(1, array:get(EndLine, List)),
    Length = EndOffset - StartOffset + 1,
    %% replace
    <<Head:StartOffset/binary, _:Length/binary, Tail/binary>> = Data,
    Line = length(binary:matches(Replace, <<"\n">>)) - (EndLine - StartLine),
    {Line + Revise, <<Head/binary, Replace/binary, Tail/binary>>}.

%%%===================================================================
%%% ip tool
%%%===================================================================
ip(4) ->
    [IP | _] = [Address || {_, Opts} <- element(2, inet:getifaddrs()), {addr, Address} <- Opts, tuple_size(Address) == 4 andalso Address =/= {127, 0, 0, 1}],
    string:join([integer_to_list(I) || I <- tuple_to_list(IP)], ".");
ip(6) ->
    [IP | _] = [Address || {_, Opts} <- element(2, inet:getifaddrs()), {addr, Address} <- Opts, tuple_size(Address) == 8 andalso Address =/= {0, 0, 0, 0, 0, 0, 0, 1}],
    string:join([integer_to_list(I, 16) || I <- tuple_to_list(IP)], ":").

%% local ipv4 address
ipv4() ->
    [IP | _] = [Address || {_, Opts} <- element(2, inet:getifaddrs()), {addr, Address} <- Opts, tuple_size(Address) == 4 andalso Address =/= {127, 0, 0, 1}],
    string:join([integer_to_list(I) || I <- tuple_to_list(IP)], ".").

%% local ipv6 address
ipv6() ->
    [IP | _] = [Address || {_, Opts} <- element(2, inet:getifaddrs()), {addr, Address} <- Opts, tuple_size(Address) == 8 andalso Address =/= {0, 0, 0, 0, 0, 0, 0, 1}],
    string:join([integer_to_list(I, 16) || I <- tuple_to_list(IP)], ":").

%%%===================================================================
%%% code assist
%%%===================================================================
list(Table) ->
    FieldsSql = io_lib:format(<<"SELECT `COLUMN_NAME`, `COLUMN_DEFAULT`, `DATA_TYPE`, `COLUMN_COMMENT`, `ORDINAL_POSITION`, `COLUMN_KEY`, `EXTRA` FROM information_schema.`COLUMNS` WHERE `TABLE_SCHEMA` = DATABASE() AND `TABLE_NAME` = '~s'">>, [Table]),
    Fields = db:select(FieldsSql),
    string:join([binary_to_list(Name) || [Name, _, _, _, _, _, _] <- Fields], ", ").

%% @doc fields to hump name
hump(Table) ->
    FieldsSql = io_lib:format(<<"SELECT `COLUMN_NAME`, `COLUMN_DEFAULT`, `DATA_TYPE`, `COLUMN_COMMENT`, `ORDINAL_POSITION`, `COLUMN_KEY`, `EXTRA` FROM information_schema.`COLUMNS` WHERE `TABLE_SCHEMA` = DATABASE() AND TABLE_NAME = '~s' ORDER BY ORDINAL_POSITION;">>, [Table]),
    Fields = db:select(FieldsSql),
    F = fun(Name) -> lists:concat([[case 96 < H andalso H < 123 of true -> H - 32; _ -> H end | T] || [H | T] <- string:tokens(Name, "_")]) end,
    string:join([F(binary_to_list(Name)) || [Name, _, _, _, _, _, _] <- Fields], ", ").

%% @doc code construct
make(Table) ->
    FieldsSql = io_lib:format(<<"SELECT `COLUMN_NAME`, `COLUMN_DEFAULT`, `DATA_TYPE`, `COLUMN_COMMENT`, `ORDINAL_POSITION`, `COLUMN_KEY`, `EXTRA` FROM information_schema.`COLUMNS` WHERE `TABLE_SCHEMA` = DATABASE() AND TABLE_NAME = '~s' ORDER BY ORDINAL_POSITION;">>, [Table]),
    Fields = db:select(FieldsSql),
    F = fun(Name) -> lists:concat([[case 96 < H andalso H < 123 of true -> H - 32; _ -> H end | T] || [H | T] <- string:tokens(Name, "_")]) end,
    Args = string:join([F(binary_to_list(Name)) || [Name, _, _, _, _, _, _] <- Fields], ", "),
    Fill = string:join([lists:concat(["        ", binary_to_list(Name), " = ", F(binary_to_list(Name))]) || [Name, _, _, _, _, _, _] <- Fields], ",\n"),
    Code = lists:concat(["make_", Table, "(", Args, ") ->\n    #", Table, "{\n", Fill, "\n    }."]),
    io:format("~s~n", [Code]).


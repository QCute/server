%%%-------------------------------------------------------------------
%%% @doc
%%% module misc use for erlang script
%%% @end
%%%-------------------------------------------------------------------
-module(misc).
-compile(nowarn_deprecated_function).
-compile(nowarn_export_all).
-compile(export_all).
%% Includes
-include("../../../include/activity.hrl").
-include("../../../include/asset.hrl").
-include("../../../include/attribute.hrl").
-include("../../../include/auction.hrl").
-include("../../../include/boss.hrl").
-include("../../../include/buff.hrl").
-include("../../../include/common.hrl").
-include("../../../include/count.hrl").
-include("../../../include/dungeon.hrl").
-include("../../../include/effect.hrl").
-include("../../../include/event.hrl").
-include("../../../include/friend.hrl").
-include("../../../include/guild.hrl").
-include("../../../include/item.hrl").
-include("../../../include/key.hrl").
-include("../../../include/lucky_money.hrl").
-include("../../../include/mail.hrl").
-include("../../../include/map.hrl").
-include("../../../include/monster.hrl").
-include("../../../include/notice.hrl").
-include("../../../include/online.hrl").
-include("../../../include/protocol.hrl").
-include("../../../include/quest.hrl").
-include("../../../include/rank.hrl").
-include("../../../include/recharge.hrl").
-include("../../../include/role.hrl").
-include("../../../include/serialize.hrl").
-include("../../../include/shop.hrl").
-include("../../../include/skill.hrl").
-include("../../../include/socket.hrl").
-include("../../../include/sorter.hrl").
-include("../../../include/title.hrl").
-include("../../../include/user.hrl").
-include("../../../include/vip.hrl").

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
    file:set_cwd(script_path() ++ "../script/make/maker/"),
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


%%%===================================================================
%%% performance tool
%%%===================================================================


r() ->

    c().

rs() ->
    c().

c() ->
    {C, _} = timer:tc(test_app, loop1, [10000000, test, macro_catch, [error, true]]),
    {CC, _} = timer:tc(test_app, loop2, [10000000, test, macro_catch, [error, true]]),
    {CCC, _} = timer:tc(test_app, loop3, [10000000, test, macro_catch, [error, true]]),
    {CcCc, _} = timer:tc(test_app, loop4, [10000000, test, macro_catch, [error, true]]),
    {C, CC, CCC, CcCc}.

cc() ->

    {ThrowTC, _} = timer:tc(test_app, loop, [10000000, test, try_catch_test, [throw, true]]),
    {ErrorTC, _} = timer:tc(test_app, loop, [10000000, test, try_catch_test, [error, true]]),
    {ExitTC, _} = timer:tc(test_app, loop, [10000000, test, try_catch_test, [exit, true]]),

    {TRC, _} = timer:tc(test_app, loop, [10000000, test, catch_test, [throw, true]]),
    {ERC, _} = timer:tc(test_app, loop, [10000000, test, catch_test, [error, true]]),
    {EXC, _} = timer:tc(test_app, loop, [10000000, test, catch_test, [exit, true]]),

    {ThrowCO, _} = timer:tc(test_app, loop, [10000000, test, catch_only_test, [throw, true]]),
    {ErrorCO, _} = timer:tc(test_app, loop, [10000000, test, catch_only_test, [error, true]]),
    {ExitCO, _} = timer:tc(test_app, loop, [10000000, test, catch_only_test, [exit, true]]),

    {ExitTCT, _} = timer:tc(test_app, loop, [10000000, test, try_catch_test, [0, true]]),
    {ExitCOU, _} = timer:tc(test_app, loop, [10000000, test, catch_only_test, [0, true]]),
    {ErrorMC, _} = timer:tc(test_app, loop, [10000000, test, macro_catch, [error, true]]),

    {ThrowTC, ErrorTC, ExitTC, TRC, ERC, EXC, ThrowCO, ErrorCO, ExitCO, ExitTCT, ExitCOU, ErrorMC}.

loop1(0, _, _, _) -> ok;
loop1(Times, M, F, A) ->
    _ = catch_only_test(0, 0),
    loop1(Times - 1, M, F, A).

loop2(0, _, _, _) -> ok;
loop2(Times, M, F, A) ->
    loop2(Times - 1, M, F, A).

loop3(0, _, _, _) -> ok;
loop3(Times, M, F, [A0, A1] = A) ->
    _ = M:F(A0, A1),
    loop3(Times - 1, M, F, A).

loop4(0, _, _, _) -> ok;
loop4(Times, M, F, A) ->
    _ = apply(M, F, A),
    loop4(Times - 1, M, F, A).

try_catch_test(Type) -> try_catch_test(Type, false).

try_catch_test(Type, TestLoop) ->
    try
        make_an_exception(Type)
    catch
        % Class:Type:Reason ->
        %     case TestLoop of
        %         true -> ok;
        %         false -> io:format("try .. catch block caught exception of ~p: ~p ~p~n", [Class, Type, Reason])
        %     end;
        Type:Reason ->
            case TestLoop of
                true -> ok;
                false -> io:format("try .. catch block caught exception of ~p: ~p ~p~n", [Type, Reason, erlang:get_stacktrace()])
            end
    end.

catch_test(Type) -> catch_test(Type, false).

catch_test(Type, TestLoop) ->
    case catch make_an_exception(Type) of
        nothing_wrong -> nothing_wrong;
        Exception ->
            case TestLoop of
                true -> ok;
                false -> io:format("catch block caught exception of ~p~n", [Exception])
            end
    end.

catch_only_test(Type, _) ->
    catch make_an_exception(Type),
    ok.

macro_catch(Type, _) ->
    try make_an_exception(Type) catch ?EXCEPTION(_, _, _) -> ok end.

make_an_exception(Type) ->
    case Type of
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
    [apply(fun jp/0, []) || _ <- L],
    ExecuteFunctionTime = os:timestamp(),
    F = fun jp/0,
    [F() || _ <- L],
    LambdaFunctionTime = os:timestamp(),
    FF = fun() -> "にほんご" end,
    [FF() || _ <- L],
    EndTime = os:timestamp(),
    io:format("~p ~p ~p ~p ~p ~p ~p~n", [timer:now_diff(RemoteTime, LocalTime), timer:now_diff(ApplyTime, RemoteTime), timer:now_diff(ExecuteTime, ApplyTime), timer:now_diff(ApplyFunctionTime, ExecuteTime), timer:now_diff(ExecuteFunctionTime, ApplyFunctionTime), timer:now_diff(LambdaFunctionTime, ExecuteFunctionTime), timer:now_diff(EndTime, LambdaFunctionTime)]),
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
    case os:type() of
        {win32, nt} ->
            io:format("\"~ts\"~n", [encoding:to_list(String)]);
        {unix, linux} ->
            io:format("\"~ts\"~n", [encoding:to_list_int(String)])
    end.

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
%%% console test
%%%===================================================================
ct() ->
    console:print(?MODULE, ?LINE, "~s~n", [<<"print">>]),
    console:debug(?MODULE, ?LINE, "~p~n", [<<"debug">>]),
    console:info(?MODULE, ?LINE, "~p~n", [info]),
    console:warming(?MODULE, ?LINE, "~p~n", [warming]),
    console:error(?MODULE, ?LINE, "~p~n", [error]).


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
        false when Name =:= File ->
            locate_loop(T, Path, File, [SubFile | List]);
        _ ->
            locate_loop(T, Path, File, List)
    end.

%%%===================================================================
%%% randomness test
%%%===================================================================
test_randomness() ->
    F = fun(_) -> test_randomness_loop(lists:duplicate(1000, 0), dict:new()) end,
    All = map_reduce(F, lists:seq(1, 1000)),
    String = lists:flatten(["[" ++ string:join([io_lib:format("{~p:~p}", [X, N]) || {X, N} <- List], ", ") ++ "]\n" || List <- All]),
    file:write_file("sample.json", String).

test_randomness_loop([], Dict) ->
    lists:sort(dict:to_list(Dict));
test_randomness_loop([_ | T], Dict) ->
    X = randomness:rand(),
    test_randomness_loop(T, dict:update_counter(X, 1, Dict)).

ac(X) ->
    activity:continue(#activity{show_time = 10, start_time = 10, over_time = 30, award_time = 30, stop_time = 30}, X).


%%%===================================================================
%%% sorter test
%%%===================================================================
tx() ->
    SortList = [
        #rank{type = 1, key = 1, value = 1, order = 2},
        #rank{type = 1, key = 1, value = 1, order = 3},
        #rank{type = 1, key = 1, value = 1, order = 1},
        #rank{type = 1, key = 1, value = 1, order = 4},
        #rank{type = 1, key = 1, value = 1, order = 5}
    ],
    Sorter = sorter:new(wow, share, replace, 100, #rank.key, #rank.value, #rank.time, #rank.order, SortList),
    sorter:update(#rank{type = 1, order = 0}, Sorter),
    sorter:data(Sorter).

%%%===================================================================
%%% parser test
%%%===================================================================
ds() ->
    State = [{a, 0}, {b, 0}, {c, 0}, {d, x}],
    %% batch save only at server close
    Format = {<<"INSERT INTO `increment` (`name`, `value`) VALUES ">>, <<"('~s', '~w')">>, <<" ON DUPLICATE KEY UPDATE `value` = VALUES(`value`)">>},
    %% rename table, avoid other process update sequence after save value
    %% F = fun({Name, _}) -> NewName = type:to_atom(erlang:make_ref()), ets:rename(Name, NewName), Value = ets:lookup_element(NewName, sequence, 2), ets:delete(NewName), {Name, Value} end,
    {Sql, _} = parser:collect_into(State, fun erlang:tuple_to_list/1, Format, 2),
    Sql.

do() ->
    F = fun({A, B, C, _})  -> [A, B, C] end,
    L = [
        {1,2,3,x},
        {4,5,6,x},
        {7,8,9,x},
        {10,11,12,x}
    ],
    parser:collect_into(L, F, {<<"insert into `test` (`a`, `b`, `c`) values ">>, <<"(~w, ~w, ~w)">>, <<" on duplicate key update `type` = VALUES(`type`), `type` = VALUES(`type`), `type` = VALUES(`type`)">>}, 4).

doo() ->
    catch ets:delete(test),
    catch ets:new(test, [named_table,ordered_set, {keypos, 1}]),

    F = fun({A, B, C, _})  -> [A, B, C] end,
    L = [
        {1,2,3,0},
        {4,5,6,0},
        {7,8,9,x},
        {10,11,12,x}
    ],
    ets:insert(test, L),
    {Sql, _} = parser:collect_into(test, F, {<<"insert into `test` (`a`, `b`, `c`) values ">>, <<"(~w, ~w, ~w)">>, <<" on duplicate key update `type` = VALUES(`type`), `type` = VALUES(`type`), `type` = VALUES(`type`)">>}, 4),
    Sql.


-record(priority_queue, {size, key, left, right, queue}).

new(Key) ->
    #priority_queue{key = Key, size = 0, left = [], right = [], queue = []}.

query(Item, PriorityQueue = #priority_queue{key = Key, queue = Queue}) ->
    NewQueue = push_loop(Queue, Key, Item, []),
    PriorityQueue#priority_queue{queue = NewQueue}.

push_loop([], _, Item, List) ->
    lists:reverse([Item | List]);
push_loop([H | T], Key, Item, List) when erlang:element(Key, Item) >= erlang:element(Key, H)->
    lists:reverse([H | List], T);
push_loop([H | T], Key, Item, List) ->
    push_loop(T, Key, Item, [H | List]).

priority_queue() ->
    Q = new(2),
    Q1 = query({1, 1}, Q),
    Q1 = query({2, 2}, Q),
    ok.



hp(Old, New) ->
    HPLevel = (Old div 10),
    case (HPLevel) =/= New div 10 of
        true ->
            hp(HPLevel);
        false ->
            ok
    end.

hp(1) ->
    1;
hp(2) ->
    1;
hp(3) ->
    2;
hp(4) ->
    3;
hp(5) ->
    4;
hp(6) ->
    5;
hp(7) ->
    6;
hp(8) ->
    7;
hp(9) ->
    8;
hp(10) ->
    9;
hp(_) ->
    ok.


%% @doc remote reload module
reload(Module) ->
    [IP | _] = [Address || {_, Opts} <- element(2, inet:getifaddrs()), {addr, Address} <- Opts, tuple_size(Address) == 4 andalso Address =/= {127, 0, 0, 1}],
    LocalIP = string:join([integer_to_list(F) || F <- tuple_to_list(IP)], "."),
    {ok, NameList} = erl_epmd:names(),
    [Self | _] = string:tokens(atom_to_list(node()), "@"),
    [reload(Module, type:to_atom(Name ++ "@" ++ LocalIP)) || {Name, _} <- NameList, Name =/= Self].
reload(Module, Node) ->
    reload(Module, Node, []).
reload(Module, Node, data) ->
    Extra = file:read_file(lists:concat(["ebin/", filename:rootname(Module, ".beam"), ".beam"])),
    reload(Module, Node, Extra);
reload(Module, Node, Extra) ->
    case net_adm:ping(Node) of
        pong ->
            LocalVsn = checksum(Module),
            case rpc:call(Node, ?MODULE, hot_load, [Module, Extra]) of
                {Node, {Flag, Atom}, RemoteVsn} ->
                    io:format("response from :~p result:~p ~p checksum:~p ~n", [Node, Flag, Atom, LocalVsn == RemoteVsn]);
                Error ->
                    io:format("reload error on node: ~p error:~p~n", [Node, Error])
            end;
        pang ->
            io:format("cannot connect node: ~p, plaease check your cookie and connect privilege~n", [Node])
    end.

%% @doc reload remote callback
hot_load(Module, {ok, Data}) ->
    BeamName = filename:rootname(Module, ".beam"),
    file:write_file("ebin/" ++ BeamName ++ ".beam", Data),
    hot_load(Module, []);
hot_load(Module, []) ->
    Result = c:l(Module),
    Vsn = checksum(Module),
    {node(), Result, Vsn};
hot_load(_, Extra) ->
    {node(), Extra, nomatch}.

%% @doc beam checksum
checksum(Module) ->
    case catch Module:module_info(attributes) of
        {'EXIT', _} ->
            [];
        Attributes ->
            {vsn, Vsn} = lists:keyfind(vsn, 1, Attributes),
            Vsn
    end.

%% insert table initialized data
initialize_table(Id, Database, Table) ->
    Sql = io_lib:format("
    SELECT
        GROUP_CONCAT('`', information_schema.`COLUMNS`.`COLUMN_NAME`, '`'),
        GROUP_CONCAT('''', information_schema.`COLUMNS`.`COLUMN_DEFAULT`, '''')
    FROM
        information_schema.`COLUMNS`
    WHERE
        information_schema.`COLUMNS`.`TABLE_SCHEMA` = '~s'
        AND
        information_schema.`COLUMNS`.`TABLE_NAME` = '~s'", [Database, Table]),
    %% collect all fields and default value
    [[Fields, Default]] = sql:select(Sql),
    %% strong match insert id equals given id
    Id = sql:insert(io_lib:format("INSERT INTO `~s` (~s) VALUES ('~w', ~s)", [Table, Fields, Id, Default])).



%% @doc load module for all node, shell execute compatible
-spec load(atom() | [atom()]) -> ok.
load(Modules) ->
    load(proplists:get_value('BEAM_LOADER_NODES', init:get_arguments(), []), Modules).

%% @doc load module (local call)
-spec load(atom() | [atom()], atom() | [atom()]) -> ok.
load(Nodes, Modules) ->
    execute_load(Nodes, Modules, soft_purge).

%% @doc force load module for all node, shell execute compatible
-spec force_load(atom() | [atom()]) -> ok.
force_load(Modules) ->
    force_load(proplists:get_value('BEAM_LOADER_NODES', init:get_arguments(), []), Modules).

%% @doc force load module (local call)
-spec force_load(atom() | [atom()], atom() | [atom()]) -> ok.
force_load(Nodes, Modules) ->
    execute_load(Nodes, Modules, purge).

%% @doc load module (local call)
-spec execute_load(atom() | [atom()], atom() | [atom()], atom()) -> ok.
execute_load(Node, Modules, Mode) when is_atom(Node) ->
    execute_load([Node], Modules, Mode);
execute_load(Nodes, Module, Mode) when is_atom(Module) ->
    execute_load(Nodes, [Module], Mode);
execute_load(Node, Module, Mode) when is_atom(Node) andalso is_atom(Module) ->
    execute_load([Node], [Module], Mode);
execute_load(Nodes, Modules, Mode) ->
    execute_load_loop(Nodes, [{type:to_atom(Module), checksum(type:to_atom(Module))} || Module <- Modules], Mode).

execute_load_loop([], _, _) ->
    ok;
execute_load_loop([Node | T], Modules, Mode) ->
    case rpc:call(type:to_atom(Node), ?MODULE, load_callback, [Modules, Mode]) of
        {ok, Result} ->
            handle_result(Result),
            execute_load_loop(T, Modules, Mode);
        _ ->
            io:format(standard_error, "cannot connect to node:~p~n", [Node]),
            execute_load_loop(T, Modules, Mode)
    end.

%% handle remote result
handle_result([]) ->
    io:format("~n~n");
handle_result([{Node, Module, _, {error, Error}, _} | T]) ->
    NodePadding = lists:duplicate(32 - length(lists:concat([Node])), " "),
    ModulePadding = lists:duplicate(24 - length(lists:concat([Module])), " "),
    io:format("node:~p~s module:~p~s result:~p~n", [Node, NodePadding, Module, ModulePadding, Error]),
    handle_result(T);
handle_result([{Node, Module, _, {_, _}, true} | T]) ->
    NodePadding = lists:duplicate(32 - length(lists:concat([Node])), " "),
    ModulePadding = lists:duplicate(24 - length(lists:concat([Module])), " "),
    io:format("node:~p~s module:~p~s result:~p~n", [Node, NodePadding, Module, ModulePadding, true]),
    handle_result(T).

%% @doc soft/purge and load module (remote call)
load_callback(Modules, Mode) ->
    load_callback_loop(Modules, Mode, []).

load_callback_loop([], _, Result) ->
    {ok, lists:reverse(Result)};
load_callback_loop([{Module, Vsn} | T], Mode, Result) ->
    case code:is_loaded(Module) of
        false ->
            load_callback_loop(T, Mode, [{node(), Module, true, {error, unloaded}, false} | Result]);
        _ ->
            Purge = code:Mode(Module),
            Load = code:load_file(Module),
            Checksum = checksum(Module),
            load_callback_loop(T, Mode, [{node(), Module, Purge, Load, Checksum == Vsn} | Result])
    end.


%% @doc transform list data to record
transform(Table, CallBack) ->
    %% table name same as record name
    Sql = lists:concat(["SELECT * FROM `", Table, "`"]),
    transform(Sql, Table, Table, CallBack).
transform(Sql, Table, CallBack) ->
    %% table name same as record name
    transform(Sql, Table, Table, CallBack).
transform(Sql, Table, Record, CallBack) ->
    Data = sql:select(Sql),
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
fix(Sql, Code, Message) ->
    fix({sql_error, {binary_to_list(iolist_to_binary(Sql)), Code, Message}}).
fix(Throw = {sql_error, {Sql, Code, Message}}) ->
    {Method, Table} = explain_sql_sentence(Sql),
    case Code of
        1054 ->
            %% no field
            Field = parse_error_message(Message),
            case find_alter_sentence(Table, Field) of
                {ok, Fix} ->
                    sql:query(Fix),
                    ?MODULE:Method(Sql);
                _ ->
                    erlang:throw(Throw)
            end;
        1146 ->
            %% no table
            case find_create_sentence(Table) of
                {ok, Fix} ->
                    sql:query(Fix),
                    ?MODULE:Method(Sql);
                _ ->
                    erlang:throw(Throw)
            end;
        _ ->
            erlang:throw(Throw)
    end;
fix(Result) ->
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
    find_sql("script/db_update.txt", Contain).
find_sql(SqlFile, Contain) ->
    {ok, Binary} = file:read_file(SqlFile),
    String = binary_to_list(Binary),
    List = string:tokens(String, ";"),
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
%%% protocol test
%%%===================================================================

x() ->
%%    F = fun(Binary) ->
%%        {Id, Binary1} = protocol:read_unsigned(Binary, 32),
%%        {Nick, Binary2} = protocol:read_string(Binary1),
%%        {Name, Remain} = protocol:read_bit_string(Binary2),
%%        %% protocol:read_bit_string(protocol:read_string(protocol:read_unsigned(Binary, 32))),
%%        {{Id, Nick, Name}, Remain}
%%    end,
    Binary = <<1:32, 1:16, "1", 1:16, "1", 2:32, 1:16, "2", 1:16, "2">>,
    protocol:read_list(fun(BinaryData) -> protocol:revise(protocol:read(string, protocol:read(string, protocol:read(32, BinaryData)))) end, Binary).


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
    list(main, Table).
list(DataBase, Table) ->
    FieldsSql = io_lib:format(<<"SELECT `COLUMN_NAME`, `COLUMN_DEFAULT`, `DATA_TYPE`, `COLUMN_COMMENT`, `ORDINAL_POSITION`, `COLUMN_KEY`, `EXTRA` FROM information_schema.`COLUMNS` WHERE `TABLE_SCHEMA` = '~s' AND `TABLE_NAME` = '~s'">>, [DataBase, Table]),
    Fields = sql:select(FieldsSql),
    string:join([binary_to_list(Name) || [Name, _, _, _, _, _, _] <- Fields], ", ").

%% @doc fields to hump name
hump(Table) ->
    hump(main, Table).
hump(DataBase, Table) ->
    FieldsSql = io_lib:format(<<"SELECT `COLUMN_NAME`, `COLUMN_DEFAULT`, `DATA_TYPE`, `COLUMN_COMMENT`, `ORDINAL_POSITION`, `COLUMN_KEY`, `EXTRA` FROM information_schema.`COLUMNS` WHERE `TABLE_SCHEMA` = '~s' AND TABLE_NAME = '~s' ORDER BY ORDINAL_POSITION;">>, [DataBase, Table]),
    Fields = sql:select(FieldsSql),
    F = fun(Name) -> lists:concat([[case 96 < H andalso H < 123 of true -> H - 32; _ -> H end | T] || [H | T] <- string:tokens(Name, "_")]) end,
    string:join([F(binary_to_list(Name)) || [Name, _, _, _, _, _, _] <- Fields], ", ").

%% @doc code construct
make(Table) ->
    make(main, Table).
make(DataBase, Table) ->
    FieldsSql = io_lib:format(<<"SELECT `COLUMN_NAME`, `COLUMN_DEFAULT`, `DATA_TYPE`, `COLUMN_COMMENT`, `ORDINAL_POSITION`, `COLUMN_KEY`, `EXTRA` FROM information_schema.`COLUMNS` WHERE `TABLE_SCHEMA` = '~s' AND TABLE_NAME = '~s' ORDER BY ORDINAL_POSITION;">>, [DataBase, Table]),
    Fields = sql:select(FieldsSql),
    F = fun(Name) -> lists:concat([[case 96 < H andalso H < 123 of true -> H - 32; _ -> H end | T] || [H | T] <- string:tokens(Name, "_")]) end,
    Args = string:join([F(binary_to_list(Name)) || [Name, _, _, _, _, _, _] <- Fields], ", "),
    Fill = string:join([lists:concat(["        ", binary_to_list(Name), " = ", F(binary_to_list(Name))]) || [Name, _, _, _, _, _, _] <- Fields], ",\n"),
    Code = lists:concat(["make_", Table, "(", Args, ") ->\n    #", Table, "{\n", Fill, "\n    }."]),
    io:format("~s~n", [Code]).

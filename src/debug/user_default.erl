-module(user_default).
-compile(nowarn_export_all).
-compile(export_all).
-include("../../include/assets.hrl").
-include("../../include/common.hrl").
-include("../../include/ets.hrl").
-include("../../include/extra.hrl").
-include("../../include/fashion.hrl").
-include("../../include/guild.hrl").
-include("../../include/item.hrl").
-include("../../include/key.hrl").
-include("../../include/notice.hrl").
-include("../../include/player.hrl").
-include("../../include/protocol.hrl").
-include("../../include/quest.hrl").
-include("../../include/rank.hrl").
-include("../../include/record.hrl").
-include("../../include/serialize.hrl").
-include("../../include/socket.hrl").
-include("../../include/sorter.hrl").
-include("../../include/table.hrl").
-include("../../include/trigger.hrl").
-include("../../include/vip.hrl").



%%====================================================================
%% API functions
%%====================================================================



%%====================================================================
%% erlang script entry
%%====================================================================
%% @doc for e script
main(Env) ->
    code:add_path("beam"),
    code:add_path("../beam"),
    code:add_path("../../beam"),
    code:add_path("../../../beam"),
    c:pwd(),

    Env.
%%%===================================================================
%%% general server
%%%===================================================================
start_link() ->
    start_link([]).
start_link(Args) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Args, []).
init(_) ->
    {ok, []}.
handle_call(_Request, _From, State) ->
    {reply, ok, State}.
handle_cast(_Request, State) ->
    {noreply, State}.
handle_info(_Request, State) ->
    {noreply, State}.
terminate(_Reason, State) ->
    {ok, State}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
%%%===================================================================
%%% console debug assist
%%%===================================================================
%% @doc clear console
c() ->
    os(clear).

%% @doc recompile and reload module
cc() ->
    cc(?MODULE).
cc(Module) ->
    %% in config dir by default
    cc(Module, "src/", "include/", "beam/").
cc(Module, SrcPath, IncludePath, BeamPath) ->
    Command = os(where, [SrcPath, lists:concat([Module, ".erl"])]),
    %% recompile
    FilePath = [C || C <- os:cmd(Command), C =/= $\r andalso C =/= $\n],
    c:c(FilePath, [debug_info, {i, IncludePath}, {outdir, BeamPath}]),
    %% soft purge
    code:soft_purge(Module),
    %% load file
    code:load_file(Module).

%% @doc hot reload all module
r() ->
    %% in config dir by default
    r("beam").
r(BeamPath) ->
    {ok, LineList} = file:list_dir_all(BeamPath),
    [c:l(list_to_atom(filename:rootname(Line))) || Line <- LineList, string:str(Line, ".beam") =/= 0],
    ok.

%% @doc update all include
update_include() ->
    %% src/debug dir by default
    Name = escript:script_name(),
    Path = string:sub_string(Name, 1, max(string:rstr(Name, "/"), string:rstr(Name, "\\"))),
    update_include(Path ++ "user_default.erl", Path, "../../include/").
update_include(FilePath, ScriptPath, IncludePath) ->
    %% list all file
    {ok, LineList} = file:list_dir_all(ScriptPath ++ IncludePath),
    %% extract file name from file path
    {_, [[Name]]} = re:run(FilePath, "\\w+(?=\\.erl)", [global, {capture, first, list}]),
    %% construct include line
    Include = ["-include(\"" ++ IncludePath ++ Line ++ "\").\n" || Line <- LineList, string:str(Line, ".hrl") =/= 0],
    IncludePatten = "(?m)(^-include.+?)(?=\\.$)\\.\n?",
    %% construct data and patten
    %% module declare
    Module = "-module(" ++ Name ++ ").\n",
    ModulePatten = "-module\\(" ++ Name ++ "\\)\\.\n",
    %% no warn declare
    NoWarn = "-compile(nowarn_export_all).\n",
    NoWarnPatten = "-compile\\(nowarn_export_all\\)\\.\n",
    %% export declare
    Export = "-compile(export_all).\n",
    ExportPatten = "-compile\\(export_all\\)\\.\n",
    %% read file data
    Data = binary_to_list(max(element(2, file:read_file(FilePath)), <<>>)),
    %% remove old data
    NewData = lists:foldr(fun(P, L) -> re:replace(L, P, "", [global, {return, list}]) end, Data, [ModulePatten, NoWarnPatten, ExportPatten, IncludePatten]),
    %% concat head include and other origin code
    file:write_file(FilePath, Module ++ NoWarn ++ Export ++ Include ++ NewData),
    ok.

%% @doc os convert
os(Type) ->
    os(Type, []).
os(Type, Args) ->
    os(Type, Args, os:type()).
os(clear, _, {win32, _}) ->
    spawn(fun() -> os:cmd("powershell clear") end);
os(clear, _, {unix, _}) ->
    spawn(fun() -> io:format("\e[H\e[J") end);
os(list, _, {win32, _}) ->
    "powershell ls ";
os(list, _, {unix, _}) ->
    "ls -l ";
os(remove, _, {win32, _}) ->
    "del ";
os(remove, _, {unix, _}) ->
    "rm ";
os(line, _, {win32, _}) ->
    "\r\n";
os(line, _, {unix, _}) ->
    "\n";
os(path, [], {win32, _}) ->
    $\\;
os(path, [], {unix, _}) ->
    $/;
os(path, [list], {win32, _}) ->
    "\\";
os(path, [list], {unix, _}) ->
    "/";
os(path, [Path], {win32, _}) ->
    lists:foldr(fun($/, A) -> [$\\ | A];(C, A) -> [C | A] end, [], Path);
os(path, [Path], {unix, _}) ->
    lists:foldr(fun($\\, A) -> [$/ | A];(C, A) -> [C | A] end, [], Path);
os(where, [Path, Target], {win32, _}) ->
    lists:concat(["chcp 65001>nul && where /R ", os(path, [Path]), " ", Target]);
os(where, [Path, Target], {unix, _}) ->
    lists:concat(["find ", os(path, [Path]), " -name ", Target]).
%%%===================================================================
%%% End
%%%===================================================================

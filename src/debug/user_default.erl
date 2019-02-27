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
    WindowsPath = lists:foldr(fun($/, A) -> [$\\ | A];(C, A) -> [C | A] end, [], Path),
    lists:concat(["chcp 65001>nul && where /R ", WindowsPath, " ", Target]);
os(where, [Path, Target], {unix, _}) ->
    lists:concat(["find ", Path, " -name ", Target]).

%%%===================================================================
%%% End
%%%===================================================================

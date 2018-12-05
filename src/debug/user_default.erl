-module(user_default).
-compile(nowarn_export_all).
-compile(export_all).
-include("../../include/common.hrl").
-include("../../include/ets.hrl").
-include("../../include/extra.hrl").
-include("../../include/guild.hrl").
-include("../../include/item.hrl").
-include("../../include/notice.hrl").
-include("../../include/player.hrl").
-include("../../include/protocol.hrl").
-include("../../include/record.hrl").
-include("../../include/socket.hrl").
-include("../../include/table.hrl").
-include("../../include/trigger.hrl").

%%====================================================================
%% API functions
%%====================================================================



%%====================================================================
%% escript entry
%%====================================================================
%% @doc for e script
main(["update_include"]) ->
    %% update self all include
    update_include();
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
    cc(Module, "../src/", "../include/", "../beam/").
cc(Module, SrcPath, IncludePath, BeamPath) ->
    Command = os(where, [SrcPath, lists:concat([Module, ".erl"])]),
    %% recompile
    FilePath = [C || C <- os:cmd(Command), C =/= $\r andalso C =/= $\n],
    c:c(FilePath, [debug_info, {i, IncludePath}, {outdir, BeamPath}]),
    %% reload
    c:l(Module).

%% @doc hot reload all module
r() ->
    %% in config dir by default
    r("../beam").
r(BeamPath) ->
    ListCommand = os(list),
    LineEnding = os(line),
    LineList = string:tokens(os:cmd(ListCommand ++ BeamPath), LineEnding),
    [c:l(list_to_atom(hd(hd(element(2, re:run(Line, "\\w+(?=\\.beam)", [global, {capture, first, list}])))))) || Line <- LineList, string:str(Line, ".beam") =/= 0],
    ok.

%% @doc update self all include
update_include() ->
    %% src/debug dir by default
    Name = escript:script_name(),
    update_include(Name, string:sub_string(Name, 1, max(string:rstr(Name, "/"), string:rstr(Name, "\\"))), "../../include/").
update_include(FilePath, ScriptPath, IncludePath) ->
    ListCommand = os(list),
    LineEnding = os(line),
    LineList = string:tokens(os:cmd(ListCommand ++ ScriptPath ++ IncludePath), LineEnding),
    %% extract file name from file path
    {_, [[Name]]} = re:run(FilePath, "\\w+(?=\\.erl)", [global, {capture, first, list}]),
    %% construct include line (file in src/debug dir)
    Include = ["-include(\"" ++ IncludePath ++ hd(lists:reverse(string:tokens(X, " "))) ++ "\").\n" || X <- LineList, string:str(X, ".hrl") =/= 0],
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
%%% architecture plant
%%%===================================================================
%%IO(ok)
%%数据(ok)
%%协议
%%集群
%%通用工具(ok)
%%错误日志(ok)
%%构造器(敏感词/表到记录/表到sql/表到日志/表到数据/表到excel/协议)(ok)
%%
%%日志(模块数据)(ok)
%%背包(物品)
%%帮派(guild_handle,guild_server,guild)
%%任务(quest_handle,quest_check,quest)
%%好友()
%%聊天
%%商店
%%邮件
%%排行
%%公告
%%支付
%%敏感词(ok)

%% idea 2018.2 active code
%% EB101IWSWD-eyJsaWNlbnNlSWQiOiJFQjEwMUlXU1dEIiwibGljZW5zZWVOYW1lIjoibGFuIHl1IiwiYXNzaWduZWVOYW1lIjoiIiwiYXNzaWduZWVFbWFpbCI6IiIsImxpY2Vuc2VSZXN0cmljdGlvbiI6IkZvciBlZHVjYXRpb25hbCB1c2Ugb25seSIsImNoZWNrQ29uY3VycmVudFVzZSI6ZmFsc2UsInByb2R1Y3RzIjpbeyJjb2RlIjoiSUkiLCJwYWlkVXBUbyI6IjIwMTgtMTAtMTQifSx7ImNvZGUiOiJSUzAiLCJwYWlkVXBUbyI6IjIwMTgtMTAtMTQifSx7ImNvZGUiOiJXUyIsInBhaWRVcFRvIjoiMjAxOC0xMC0xNCJ9LHsiY29kZSI6IlJEIiwicGFpZFVwVG8iOiIyMDE4LTEwLTE0In0seyJjb2RlIjoiUkMiLCJwYWlkVXBUbyI6IjIwMTgtMTAtMTQifSx7ImNvZGUiOiJEQyIsInBhaWRVcFRvIjoiMjAxOC0xMC0xNCJ9LHsiY29kZSI6IkRCIiwicGFpZFVwVG8iOiIyMDE4LTEwLTE0In0seyJjb2RlIjoiUk0iLCJwYWlkVXBUbyI6IjIwMTgtMTAtMTQifSx7ImNvZGUiOiJETSIsInBhaWRVcFRvIjoiMjAxOC0xMC0xNCJ9LHsiY29kZSI6IkFDIiwicGFpZFVwVG8iOiIyMDE4LTEwLTE0In0seyJjb2RlIjoiRFBOIiwicGFpZFVwVG8iOiIyMDE4LTEwLTE0In0seyJjb2RlIjoiUFMiLCJwYWlkVXBUbyI6IjIwMTgtMTAtMTQifSx7ImNvZGUiOiJDTCIsInBhaWRVcFRvIjoiMjAxOC0xMC0xNCJ9LHsiY29kZSI6IlBDIiwicGFpZFVwVG8iOiIyMDE4LTEwLTE0In0seyJjb2RlIjoiUlNVIiwicGFpZFVwVG8iOiIyMDE4LTEwLTE0In1dLCJoYXNoIjoiNjk0NDAzMi8wIiwiZ3JhY2VQZXJpb2REYXlzIjowLCJhdXRvUHJvbG9uZ2F0ZWQiOmZhbHNlLCJpc0F1dG9Qcm9sb25nYXRlZCI6ZmFsc2V9-Gbb7jeR8JWOVxdUFaXfJzVU/O7c7xHQyaidCnhYLp7v32zdeXiHUU7vlrrm5y9ZX0lmQk3plCCsW+phrC9gGAPd6WDKhkal10qVNg0larCR2tQ3u8jfv1t2JAvWrMOJfFG9kKsJuw1P4TozZ/E7Qvj1cupf/rldhoOmaXMyABxNN1af1RV3bVhe4FFZe0p7xlIJF/ctZkFK62HYmh8V3AyhUNTzrvK2k+t/tlDJz2LnW7nYttBLHld8LabPlEEjpTHswhzlthzhVqALIgvF0uNbIJ5Uwpb7NqR4U/2ob0Z+FIcRpFUIAHEAw+RLGwkCge5DyZKfx+RoRJ/In4q/UpA==-MIIEPjCCAiagAwIBAgIBBTANBgkqhkiG9w0BAQsFADAYMRYwFAYDVQQDDA1KZXRQcm9maWxlIENBMB4XDTE1MTEwMjA4MjE0OFoXDTE4MTEwMTA4MjE0OFowETEPMA0GA1UEAwwGcHJvZDN5MIIBIjANBgkqhkiG9w0BAQEFAAOCAQ8AMIIBCgKCAQEAxcQkq+zdxlR2mmRYBPzGbUNdMN6OaXiXzxIWtMEkrJMO/5oUfQJbLLuMSMK0QHFmaI37WShyxZcfRCidwXjot4zmNBKnlyHodDij/78TmVqFl8nOeD5+07B8VEaIu7c3E1N+e1doC6wht4I4+IEmtsPAdoaj5WCQVQbrI8KeT8M9VcBIWX7fD0fhexfg3ZRt0xqwMcXGNp3DdJHiO0rCdU+Itv7EmtnSVq9jBG1usMSFvMowR25mju2JcPFp1+I4ZI+FqgR8gyG8oiNDyNEoAbsR3lOpI7grUYSvkB/xVy/VoklPCK2h0f0GJxFjnye8NT1PAywoyl7RmiAVRE/EKwIDAQABo4GZMIGWMAkGA1UdEwQCMAAwHQYDVR0OBBYEFGEpG9oZGcfLMGNBkY7SgHiMGgTcMEgGA1UdIwRBMD+AFKOetkhnQhI2Qb1t4Lm0oFKLl/GzoRykGjAYMRYwFAYDVQQDDA1KZXRQcm9maWxlIENBggkA0myxg7KDeeEwEwYDVR0lBAwwCgYIKwYBBQUHAwEwCwYDVR0PBAQDAgWgMA0GCSqGSIb3DQEBCwUAA4ICAQC9WZuYgQedSuOc5TOUSrRigMw4/+wuC5EtZBfvdl4HT/8vzMW/oUlIP4YCvA0XKyBaCJ2iX+ZCDKoPfiYXiaSiH+HxAPV6J79vvouxKrWg2XV6ShFtPLP+0gPdGq3x9R3+kJbmAm8w+FOdlWqAfJrLvpzMGNeDU14YGXiZ9bVzmIQbwrBA+c/F4tlK/DV07dsNExihqFoibnqDiVNTGombaU2dDup2gwKdL81ua8EIcGNExHe82kjF4zwfadHk3bQVvbfdAwxcDy4xBjs3L4raPLU3yenSzr/OEur1+jfOxnQSmEcMXKXgrAQ9U55gwjcOFKrgOxEdek/Sk1VfOjvS+nuM4eyEruFMfaZHzoQiuw4IqgGc45ohFH0UUyjYcuFxxDSU9lMCv8qdHKm+wnPRb0l9l5vXsCBDuhAGYD6ss+Ga+aDY6f/qXZuUCEUOH3QUNbbCUlviSz6+GiRnt1kA9N2Qachl+2yBfaqUqr8h7Z2gsx5LcIf5kYNsqJ0GavXTVyWh7PYiKX4bs354ZQLUwwa/cG++2+wNWP+HtBhVxMRNTdVhSm38AknZlD+PTAsWGu9GyLmhti2EnVwGybSD2Dxmhxk3IPCkhKAK+pl0eWYGZWG3tJ9mZ7SowcXLWDFAk0lRJnKGFMTggrWjV8GYpw5bq23VmIqqDLgkNzuoog==
%%
%%%-------------------------------------------------------------------
%%% @doc
%%% module ctx (context)
%%% @end
%%%-------------------------------------------------------------------
-module(ctx).
-compile(nowarn_export_all).
-compile(nowarn_deprecated_function).
%% API
-compile(export_all).

%% @doc for e script
main(_) ->
    code:add_path("beam"),
    code:add_path("../beam"),
    code:add_path("../../beam"),
    %io:format("~p~n", [word:sensitive("官方")]),

    io:format("~ts~n", [ts()]),
    
    ok.


append([H|T], Tail) ->
    [H|append(T, Tail)];
append([], Tail) ->
    Tail.


%% @doc file encoding test
tss() ->
    "一".
ts() ->
    case "一" of
        [14989440] ->
            utf8;
        [228, 184, 128] ->
            utf8;
        [19968] ->
            unicode;
        [78, 0] ->
            unicode;
        [53947] ->
            gbk;
        [210, 187] ->
            gbk
    end.

%% 一
%% <<228,184,128>>  .utf8      228*256*256 + 184*256 + 128   [14989440]
%% <<78,0>>         .unicode   78*256 + 0                    [19968]
%% <<210,187>>      .gbk       210*256+187                   [53947]


map_reduce(F, L) ->
    Parent = self(),
    [spawn(fun() -> catch Parent ! F(I) end) || I <- L],
    [receive R -> R end || _ <- L].


script_path() ->
    Name = lists:reverse(escript:script_name()),
    lists:reverse(trim_path(Name, [])) ++ "../../../".
trim_path([], List) ->
    List;
trim_path([$\\ | _] = List, _) ->
    List;
trim_path([$/ | _] = List, _) ->
    List;
trim_path([H | T], List) ->
    trim_path(T, [H | List]).


%% 32
%% #luxury{one = 32, ten = 32}
%% [{32, string, 32}]
%% [{32, _, 32, 16, _, string}]
%% [#award{one_price = 32, name = string}]

%%
%%format_list([[_] = H | _], Name) ->
%%    {P, M} = write(H),
%%    io_lib:format("(length(~s)):16, << <<~s>> ~s <- ~s>>", [Name, string:join(P, ", "), M, Name]).
%%
%%
%%
%%handle(List) ->
%%    handle(List, [], [], []).
%%handle([], Pack, Match, Expand) ->
%%    {lists:concat(Pack), lists:concat(Match), lists:concat(Expand)};
%%handle([#u8{} = H | T], Pack, Match, Expand) ->
%%    {P, M} = write([H]),
%%    handle(T, [P | Pack], [M | Match], Expand);
%%handle([[#u8{}] = H | T], Pack, Match, Expand) ->
%%    {P, M, E} = format_list([H], ""),
%%    handle(T, [P | Pack], [M | Match], [E | Expand]);
%%handle([H | T], Pack, Match, Expand) when is_tuple(H) ->
%%    {P, M, E} = format_tuple(H),
%%    handle(T, [P | Pack], [M | Match], [E | Expand]);
%%handle([[H] | T], Pack, Match, Expand) when is_tuple(H) ->
%%    {P, M, E} = format_tuple(H),
%%    handle(T, [P | Pack], [M | Match], [E | Expand]);
%%handle(W, _Pack, _Match, _Expand) ->
%%    erlang:throw(io_lib:format("unknown ~p~n", [W])).
%%
%%
%%%% format tuple
%%format_tuple(Tuple) ->
%%    {Pack, Match, Expand} = format_tuple(tuple_to_list(Tuple), [], [], []),
%%    {lists:concat(["{", string:join(lists:reverse(Match), ", "), "}"]), string:join(lists:reverse(Pack), ", "), lists:concat(Expand)}.
%%format_tuple([], Pack, Match, Expand) ->
%%    {Pack, Match, Expand};
%%format_tuple([#u8{} = H | T], Pack, Match, Expand) ->
%%    {P, M} = write([H]),
%%    format_tuple(T, [P | Pack], [M | Match], Expand);
%%format_tuple([#zero{} | T], Pack, Match, Expand) ->
%%    format_tuple(T, Pack, ["_" | Match], Expand);
%%format_tuple([[_] = H | T], Pack, Match, Expand) ->
%%    {P, M, E} = handle([H]),
%%    format_tuple(T, [P | Pack], ["~s" | Match], [M ++ E | Expand]);
%%format_tuple([H | T], Pack, Match, Expand) ->
%%    {P, M, E} = format_tuple(H),
%%    format_tuple(T, [P | Pack], [M | Match], [E | Expand]).



%% regular
%% {name, bit(integer/string), comment}
%% {{record, record_target}, {field, bit(integer/string), comment}, ...}
%% {{tuple, tuple_size}, {tuple_position, bit(integer/string), comment}}
%% [{name, bit(integer/string), comment}]
%% [{{record, record_target}, {field, bit(integer/string), comment}, ...}]
%% [{{tuple, tuple_size}, {tuple_position, bit(integer/string), comment}, ...}]

%% #record{name = record_target, bit = {{field, bit(integer/string), comment}, ...}}
%% #tuple{size = tuple_size, bit = {{tuple_position, bit(integer/string), comment}, ...}}
%% [#record{name = record_target, bit = {{field, bit(integer/string), comment}, ...}}]
%% [#tuple{size = tuple_size, bit = {{tuple_position, bit(integer/string), comment}, ...}}]

%%-record(ssr, {name, param, type, bit, comment}).
%%-record(record, {name, bit}).
%%-record(tuple, {size, bit}).

%%pack() ->
%%    param = #write{bit = {rank, 16, ""}},
%%    param = #write{bit = {hurt, 64, ""}},
%%    param = #write{bit = [#record{name = fuben_guild_rank_hurt, bit = {{pos, 16, ""}, {name, string, ""}, {amount, 64, ""}}}]},
%%
%%    param = #record{name = rank_yamen_week, bit = {{pos, 16, ""},{amount, 32, ""}}},
%%    param = [#tuple{size = 2, bit = {2, #record{name = rank_yamen_week, bit = {{pos,16,""},{name,string,""},{amount,32,""}}}}}],
%%    bit = [{tuple, 2}, {2, {{record,rank_yamen_week}, {pos,16,""},{name,string,""},{amount,32,""}}}],
%%    param = [#tuple{size = 1, bit = {1, #record{name = rank, bit = {{value,64},{time,32},{key,64}}}}}],
%%    ok.

%%%===================================================================
%%% code assist
%%%===================================================================
%% @doc fields to hump name
hump(Table) ->
    hump('game', Table).
hump(DataBase, Table) ->
    FieldsSql = io_lib:format(<<"SELECT `COLUMN_NAME`, `COLUMN_DEFAULT`, `DATA_TYPE`, `COLUMN_COMMENT`, `ORDINAL_POSITION`, `COLUMN_KEY`, `EXTRA` FROM information_schema.`COLUMNS` WHERE `TABLE_SCHEMA` = '~s' AND TABLE_NAME = '~s' ORDER BY ORDINAL_POSITION;">>, [DataBase, Table]),
    Fields = sql:select(FieldsSql),
    F = fun(Name) -> lists:concat([[case 96 < H andalso H < 123 of true -> H - 32; _ -> H end | T] || [H | T] <- string:tokens(Name, "_")]) end,
    string:join([F(binary_to_list(Name)) || [Name, _, _, _, _, _, _] <- Fields], ", ").

%% @doc code construct
make(Table) ->
    make('game', Table).
make(DataBase, Table) ->
    FieldsSql = io_lib:format(<<"SELECT `COLUMN_NAME`, `COLUMN_DEFAULT`, `DATA_TYPE`, `COLUMN_COMMENT`, `ORDINAL_POSITION`, `COLUMN_KEY`, `EXTRA` FROM information_schema.`COLUMNS` WHERE `TABLE_SCHEMA` = '~s' AND TABLE_NAME = '~s' ORDER BY ORDINAL_POSITION;">>, [DataBase, Table]),
    Fields = sql:select(FieldsSql),
    F = fun(Name) -> lists:concat([[case 96 < H andalso H < 123 of true -> H - 32; _ -> H end | T] || [H | T] <- string:tokens(Name, "_")]) end,
    Args = string:join([F(binary_to_list(Name)) || [Name, _, _, _, _, _, _] <- Fields], ", "),
    Fill = string:join([lists:concat(["        ", binary_to_list(Name), " = ", F(binary_to_list(Name))]) || [Name, _, _, _, _, _, _] <- Fields], ",\n"),
    Code = lists:concat(["make_", Table, "(", Args, ") ->\n    #", Table, "{\n", Fill, "\n    }."]),
    io:format("~s~n", [Code]).

%%%===================================================================
%%% regexp
%%%===================================================================
%% match record(multi line)
%% 跨行匹配左边不接非空白字符，名字开头，后接以.结尾或者后面是注释%的记录
%% (?s)(?<!\\S)(-record\\(~s\\s*,.+?)(?=\\.$|\\%)\\.

%% function(multi line)
%% 跨行匹配左边不接非空白字符，名字开头，后接以.结尾或者后面是注释%的函数
%% (?s)(?<!\\S)(~s.+?)(?=\\.$|\\%)\\.

%% define(single line)
%% 跨行匹配左边不接非空白字符，名字开头，后接以.结尾或者后面是注释%的定义
%% (?<!\\S)(-define\\s*\\(~s.+?)(?=\\.$|\\%)\\.

%% all include(single line)
%% 跨行匹配左边不接非空白字符，名字开头，后接以.结尾或者后面是注释%的依赖
%% (?<!\\S)(-include\\s*\\(~s\\s*\\.+?)(?=\\.$|\\%)\\.
%% 匹配所有include
%% (?<!\\S)(-include.+?)(?=\\.$|\\%)\\.

%% 匹配record
%% (?m)(?s)^-record\\(~s\s*,\s*\\{.+?^((?!%).)*?\\}\s*\\)\\.(?=$|\s|%)
%% 匹配函数
%% (?m)(?s)^~s\(.*?\)\s*->.+?^((?!%).)*?\.(?=$|\s|%)



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

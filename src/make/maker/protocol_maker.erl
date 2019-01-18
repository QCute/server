%%%-------------------------------------------------------------------
%%% @doc
%%% module protocol maker
%%% @end
%%%-------------------------------------------------------------------
-module(protocol_maker).
-include("serialize.hrl").
-export([parse/2]).
%%%===================================================================
%%% API
%%%===================================================================
parse(_, {_, #protocol{io = IO, include = Includes, file = File}}) ->
    [Module | _] = string:tokens(hd(lists:reverse(string:tokens(File, "/"))), "."),
    Include = [lists:flatten(io_lib:format("-include(\"~s\").\n", [X])) || X <- Includes],
    Code = collect_code(IO, [], []),
    Head = io_lib:format("-module(~s).\n-compile(nowarn_export_all).\n-compile(export_all).\n", [Module]),
    [{"(?s).*", Head ++ Include ++ Code}].

%% collect code
collect_code([], ReadList, WriteList) ->
    ReadMatch = "read(Code, Binary) ->\n    {error, Code, Binary}.\n\n",
    WriteMatch = "write(Code, Content) ->\n    {error, Code, Content}.\n",
    lists:concat(["\n\n", lists:reverse(ReadList), ReadMatch, "\n\n", lists:reverse(WriteList), WriteMatch]);
collect_code([#io{read = Read, write = Write, name = Name} | T], ReadList, WriteList) ->
    ReadCode = format_read(Name, Read),
    erase(),
    WriteCode = format_write(Name, Write),
    erase(),
    collect_code(T, [ReadCode | ReadList], [WriteCode | WriteList]).

%%====================================================================
%% read code part
%%====================================================================
format_read(_Code, []) ->
    "";
format_read(Code, List) ->
    {M, E, P} = format_read(List, [], [], []),
    lists:concat(["read(", Code, ", ", P, ") ->\n"]) ++ E ++ lists:concat(["    {ok, [", M, "]};\n\n"]).
format_read([], Match, Expression, Pack) ->
    {string:join(lists:reverse(Match), ", "), [X ++ ",\n" || X = [_ | _] <- lists:reverse(Expression)], lists:concat(["<<", string:join(lists:reverse(Pack), ", "), ">>"])};
format_read([H | T], Match, Expression, Pack) ->
    {MatchParam, PackInfo} = format_read_unit(H, undefined),
    case is_list(H) orelse is_record(H, list) of
        true ->
            ListLength = PackInfo ++ "Length",
            case re:run(MatchParam, "/binary", [global, {capture, all, list}]) =/= nomatch of
                true ->
                    %% with string can only use binary
                    ListMatch = format("Binary:~s/binary", [ListLength]);
                _ ->
                    %% without string calc binary unit
                    {match, Result} = re:run(MatchParam, "(?<=:)\\d+", [global, {capture, all, list}]),
                    ListMatch = format("Binary:~s/binary-unit:~p", [ListLength, lists:sum([list_to_integer(X) || [X] <- Result])])
            end,
            %% unpack binary to list
            E = lists:concat(["    ", PackInfo, " = [", MatchParam, "]"]),
            %% fill binary match param, list length use 16 bit constant
            P = ListLength ++ ":16, " ++ PackInfo ++ ListMatch,
            format_read(T, [PackInfo | Match], [E | Expression], [P | Pack]);
        _ ->
            format_read(T, [MatchParam | Match], Expression, [PackInfo | Pack])
    end.

%%====================================================================
%% write code part
%%====================================================================
format_write(_Code, []) ->
    "";
format_write(Code, List) ->
    {M, E, P} = format_write(List, [], [], []),
    lists:concat(["write(", Code, ", [", M, "]) ->\n"]) ++ E ++ lists:concat(["    {ok, protocol:pack(", Code, ", ", P, ")};\n\n"]).
format_write([], Match, Expression, Pack) ->
    {string:join(lists:reverse(Match), ", "), [X ++ ",\n" || X = [_ | _] <- lists:reverse(Expression)], lists:concat(["<<", string:join(lists:reverse(Pack), ", "), ">>"])};
format_write([H | T], Match, Expression, Pack) ->
    {MatchParam, PackInfo} = format_write_unit(H, undefined),
    case is_list(H) orelse is_record(H, list) of
        true ->
            %% pack list data do in function expression
            E = lists:concat(["    ", MatchParam, "Binary = <<", PackInfo, ">>"]),
            P = MatchParam ++ "Binary/binary",
            format_write(T, [MatchParam | Match], [E | Expression], [P | Pack]);
        _ when is_record(H, ets) ->
            %% pack ets list data do in function expression
            E = lists:concat(["    ", MatchParam, "Binary = ", PackInfo, ""]),
            P = MatchParam ++ "Binary/binary",
            format_write(T, [MatchParam | Match], [E | Expression], [P | Pack]);
        _ when length(PackInfo) >= 30 ->
            %% pack data do in function expression when code length great equal 30
            E = lists:concat(["    ", MatchParam, "Binary = <<", PackInfo, ">>"]),
            P = MatchParam ++ "Binary/binary",
            format_write(T, [MatchParam | Match], [E | Expression], [P | Pack]);
        _ ->
            format_write(T, [MatchParam | Match], Expression, [PackInfo | Pack])
    end.

%%====================================================================
%% read unit part
%%====================================================================
format_read_unit(#u8{name = Name}, Extra) ->
    HumpName = choose_name(Name, Extra),
    Param = format("~s", [HumpName]),
    Pack = format("~s:8", [HumpName]),
    {Param, Pack};
format_read_unit(#u16{name = Name}, Extra) ->
    HumpName = choose_name(Name, Extra),
    Param = format("~s", [HumpName]),
    Pack = format("~s:16", [HumpName]),
    {Param, Pack};
format_read_unit(#u32{name = Name}, Extra) ->
    HumpName = choose_name(Name, Extra),
    Param = format("~s", [HumpName]),
    Pack = format("~s:32", [HumpName]),
    {Param, Pack};
format_read_unit(#u64{name = Name}, Extra) ->
    HumpName = choose_name(Name, Extra),
    Param = format("~s", [HumpName]),
    Pack = format("~s:64", [HumpName]),
    {Param, Pack};
format_read_unit(#u128{name = Name}, Extra) ->
    HumpName = choose_name(Name, Extra),
    Param = format("~s", [HumpName]),
    Pack = format("~s:128", [HumpName]),
    {Param, Pack};
format_read_unit(#btr{name = Name}, Extra) ->
    HumpName = choose_name(Name, Extra),
    Param = format("~s", [HumpName]),
    Pack = format("~s:16, ~s:~s/binary", [HumpName ++ "Length", HumpName, HumpName ++ "Length"]),
    {Param, Pack};
format_read_unit(#str{name = Name}, Extra) ->
    HumpName = choose_name(Name, Extra),
    Param = format("binary_to_list(~s)", [HumpName]),
    Pack = format("~s:16, ~s:~s/binary", [HumpName ++ "Length", HumpName, HumpName ++ "Length"]),
    {Param, Pack};

%% structure unit
format_read_unit(#list{name = Name, desc = Desc}, Extra) ->
    %% hump name is unpack bit variable bind
    Hump = choose_name(Name, Extra),
    %% format subunit
    {ListParam, ListPack} = format_read_unit(Desc, Extra),
    %% format list pack info
    Param = format("~s || <<~s>> <= ~s", [ListParam, ListPack, Hump ++ "Binary"]),
    {Param, Hump};
format_read_unit([Desc], Extra) ->
    %% hump name is unpack bit variable bind
    Hump = choose_name(undefined, Extra),
    %% format subunit
    {ListParam, ListPack} = format_read_unit(Desc, Extra),
    %% format list pack info
    Param = format("~s || <<~s>> <= ~s", [ListParam, ListPack, Hump ++ "Binary"]),
    {Param, Hump};
format_read_unit(Record, _) when is_tuple(Record) andalso is_atom(element(1, Record)) ->
    %% get beam abstract code
    Tag = element(1, Record),
    NameList = beam:get(Tag),
    %% zip field value and field name
    ZipList = lists:zip(tuple_to_list(Record), NameList),
    %% format per unit
    List = [{format_read_unit(Desc, Name), Name} || {Desc, Name} <- ZipList, is_unit(Desc)],
    %% format function match param
    Param = lists:concat(["#", Tag, "{", string:join([format("~s = ~s", [Name, MatchParam]) || {{MatchParam = [_ | _], _}, Name} <- List], ", "), "}"]),
    %% format pack info
    Pack = string:join([PackInfo || {{_, PackInfo = [_ | _]}, _} <- List], ", "),
    {Param, Pack};
format_read_unit(Tuple, Extra) when is_tuple(Tuple) andalso tuple_size(Tuple) > 0 ->
    %% make tuple name list
    NameList = [format("~s", [choose_name(lists:concat([Extra, No]))]) || No <- lists:seq(1, tuple_size(Tuple))],
    %% zip field value and field name
    ZipList = lists:zip(tuple_to_list(Tuple), NameList),
    %% format per unit
    List = [format_read_unit(Desc, Name) || {Desc, Name} <- ZipList],
    %% format function match param
    case string:join([MatchParam || {MatchParam = [_ | _], _} <- List], ", ") of
        [] ->
            Param = [];
        MatchParam ->
            Param = lists:concat(["{", MatchParam, "}"])
    end,
    %% format pack info
    Pack = string:join([PackInfo || {_, PackInfo = [_ | _]} <- List], ", "),
    {Param, Pack};

format_read_unit(_, _) ->
    {"", ""}.

%%====================================================================
%% write unit part
%%====================================================================
format_write_unit(#zero{}, _) ->
    Param = format("_", []),
    Pack = format("", []),
    {Param, Pack};
format_write_unit(#u8{name = Name}, Extra) ->
    HumpName = choose_name(Name, Extra),
    Param = format("~s", [HumpName]),
    Pack = format("~s:8", [HumpName]),
    {Param, Pack};
format_write_unit(#u16{name = Name}, Extra) ->
    HumpName = choose_name(Name, Extra),
    Param = format("~s", [HumpName]),
    Pack = format("~s:16", [HumpName]),
    {Param, Pack};
format_write_unit(#u32{name = Name}, Extra) ->
    HumpName = choose_name(Name, Extra),
    Param = format("~s", [HumpName]),
    Pack = format("~s:32", [HumpName]),
    {Param, Pack};
format_write_unit(#u64{name = Name}, Extra) ->
    HumpName = choose_name(Name, Extra),
    Param = format("~s", [HumpName]),
    Pack = format("~s:64", [HumpName]),
    {Param, Pack};
format_write_unit(#u128{name = Name}, Extra) ->
    HumpName = choose_name(Name, Extra),
    Param = format("~s", [HumpName]),
    Pack = format("~s:128", [HumpName]),
    {Param, Pack};
format_write_unit(#btr{name = Name}, Extra) ->
    HumpName = choose_name(Name, Extra),
    Param = format("~s", [HumpName]),
    Pack = format("(byte_size(~s)):16, (~s)/binary", [HumpName, HumpName]),
    {Param, Pack};
format_write_unit(#str{name = Name}, Extra) ->
    HumpName = choose_name(Name, Extra),
    Param = format("~s", [HumpName]),
    Pack = format("(length(~s)):16, (iolist_to_binary(~s))/binary", [HumpName, HumpName]),
    {Param, Pack};

%% structure unit
format_write_unit(#ets{name = Name, desc = [Desc]}, Extra) ->
    %% auto make undefined name
    Hump = choose_name(Name, Extra),
    %% format subunit
    {ListParam, ListPack} = format_write_unit([Desc], Extra),
    %% format list pack info
    Pack = format("protocol:pack_ets(fun(~s) -> <<~s>> end, ~s)", [ListParam, ListPack, Hump]),
    {Hump, Pack};
format_write_unit(#ets{name = Name, desc = Desc}, Extra) ->
    %% auto make undefined name
    Hump = choose_name(Name, Extra),
    %% format subunit
    {ListParam, ListPack} = format_write_unit(Desc, Extra),
    %% format list pack info
    Pack = format("protocol:pack_ets(fun([~s]) -> <<~s>> end, ~s)", [ListParam, ListPack, Hump]),
    {Hump, Pack};
format_write_unit(#list{name = Name, desc = Desc}, Extra) ->
    %% auto make undefined name
    Hump = choose_name(Name, Extra),
    %% format subunit
    {ListParam, ListPack} = format_write_unit(Desc, Extra),
    %% format list pack info
    Pack = format("(length(~s)):16, <<<<~s>> || ~s <- ~s>>", [Hump, ListPack, ListParam, Hump]),
    {Hump, Pack};
format_write_unit([Desc], Extra) ->
    %% auto make undefined name
    Hump = choose_name(undefined, Extra),
    %% format subunit
    {ListParam, ListPack} = format_write_unit(Desc, Extra),
    %% format list pack info
    Pack = format("(length(~s)):16, <<<<~s>> || ~s <- ~s>>", [Hump, ListPack, ListParam, Hump]),
    {Hump, Pack};
format_write_unit(Record, _) when is_tuple(Record) andalso tuple_size(Record) > 0 andalso is_atom(element(1, Record)) ->
    %% get beam abstract code
    Tag = element(1, Record),
    NameList = beam:get(Tag),
    %% zip field value and field name
    ZipList = lists:zip(tuple_to_list(Record), NameList),
    %% format per unit
    List = [{format_write_unit(Desc, Name), Name} || {Desc, Name} <- ZipList, is_unit(Desc)],
    %% format function match param
    Param = lists:concat(["#", Tag, "{", string:join([format("~s = ~s", [Name, MatchParam]) || {{MatchParam = [_ | _], _}, Name} <- List], ", "), "}"]),
    %% format pack info
    Pack = string:join([PackInfo || {{_, PackInfo = [_ | _]}, _} <- List], ", "),
    {Param, Pack};
format_write_unit(Tuple, Extra) when is_tuple(Tuple) andalso tuple_size(Tuple) > 0 ->
    %% make tuple name list
    NameList = [format("~s", [choose_name(lists:concat([Extra, No]))]) || No <- lists:seq(1, tuple_size(Tuple))],
    %% zip field value and field name
    ZipList = lists:zip(tuple_to_list(Tuple), NameList),
    %% format per unit
    List = [format_write_unit(Desc, Name) || {Desc, Name} <- ZipList],
    %% format function match param
    case string:join([MatchParam || {MatchParam = [_ | _], _} <- List], ", ") of
        [] ->
            Param = [];
        MatchParam ->
            Param = lists:concat(["{", MatchParam, "}"])
    end,
    %% format pack info
    Pack = string:join([PackInfo || {_, PackInfo = [_ | _]} <- List], ", "),
    {Param, Pack};

format_write_unit(_, _) ->
    {"", ""}.


%%====================================================================
%% named tool
%%====================================================================
%% auto make name
choose_name(Name) ->
    choose_name(Name, undefined).
choose_name(undefined, undefined) ->
    case get('name') of
        undefined ->
            Name = lists:concat([undefined, 1]),
            put('name', 1),
            hump(Name);
        AI ->
            Name = lists:concat([undefined, AI + 1]),
            put('name', AI + 1),
            hump(Name)
    end;
choose_name(undefined, Outer) ->
    hump(Outer);
choose_name(Inner, _) ->
    hump(Inner).

%% hump name
hump(Binary) when is_binary(Binary) ->
    hump(binary_to_list(Binary));
hump(Atom) when is_atom(Atom) ->
    hump(atom_to_list(Atom));
hump(Name) ->
    lists:concat([[case 96 < H andalso H < 123 of true -> H - 32; _ -> H end | T] || [H | T] <- string:tokens(Name, "_")]).

%%====================================================================
%% common tool
%%====================================================================
%% format
format(F, A) ->
    binary_to_list(list_to_binary(io_lib:format(F, A))).

%% is bit unit
is_unit(Tag) when is_atom(Tag) ->
    false;
is_unit(Tag) when is_integer(Tag) ->
    false;
is_unit(Tag) when is_binary(Tag) ->
    false;
is_unit(Tag) when is_list(Tag) ->
    true;
is_unit(Tag) when is_tuple(Tag) andalso is_atom(element(1, Tag)) ->
    true;
is_unit(Tag) when is_tuple(Tag) ->
    true;
is_unit(_) ->
    false.
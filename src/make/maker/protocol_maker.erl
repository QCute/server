%%%-------------------------------------------------------------------
%%% @doc
%%% module protocol_maker
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
	{Code, Comment} = collect_code(IO, [], [], []),
	Head = io_lib:format("-module(~s).\n-compile(nowarn_export_all).\n-compile(export_all).\n", [Module]),
	Path = maker:script_path() ++ Module ++ ".txt",
	file:write_file(Path, Comment),
	[{"(?s).*", Head ++ Include ++ Code}].

%% collect code
collect_code([], ReadList, WriteList, CommentList) ->
	ReadMatch = "read(Code, Binary) ->\n    {error, Code, Binary}.\n\n",
	WriteMatch = "write(Code, Content) ->\n    {error, Code, Content}.\n",
	{lists:concat(["\n\n", lists:reverse(ReadList), ReadMatch, "\n\n", lists:reverse(WriteList), WriteMatch]), CommentList};
collect_code([#io{read = Read, write = Write, name = Name, comment = Comment} | T], ReadList, WriteList, CommentList) ->
	{ReadCode, ReadComment} = format_read(Name, Read),
	erase(),
	{WriteCode, WriteComment} = format_write(Name, Write),
	erase(),
	RC = collect_doc("request:" , ReadComment),
	WC = collect_doc("response:" , WriteComment),
	NewComment = lists:concat(["*** ", Comment, " ***", "\nProtocol:", Name, "\n", RC, "\n\n", WC, "\n\n"]),
	collect_code(T, [ReadCode | ReadList], [WriteCode | WriteList], [NewComment | CommentList]).

%% collect doc
collect_doc(Head, []) ->
	Head ++ "[]\n";
collect_doc(Head, List) ->
	Head ++ "\n" ++ collect_doc(List, 1, []).
collect_doc([], _, L) ->
	string:join(lists:reverse(L), "\n");
collect_doc([{I, B, O} | T], D, L) ->
	Name = choose_comment(O, I),
	Align = lists:duplicate(8 - length(B), " "),
	collect_doc(T, D, [format("~s~s~s~s", [lists:duplicate(D, "    "), B, Align, Name]) | L]);
collect_doc([[_ | _] = H | T], D, L) ->
	Doc = collect_doc(H, D + 1, []),
	Align = lists:duplicate(D, "    "),
	collect_doc(T, D, [format("~sarray(~n~s~n~s)", [Align, Doc, Align]) | L]).

%%%===================================================================
%%% Internal functions
%%%===================================================================
%% read code format
format_read(_Code, []) ->
	{"", ""};
format_read(Code, List) ->
	{M, E, P, C} = format_read(List, [], [], [], []),
	{lists:concat(["read(", Code, ", ", P, ") ->\n"]) ++ E ++ lists:concat(["    {ok, [", M, "]};\n\n"]), C}.
format_read([], Match, Expression, Pack, Comment) ->
	{string:join(lists:reverse(Match), ", "), [X ++ ",\n" || X = [_ | _] <- lists:reverse(Expression)], lists:concat(["<<", string:join(lists:reverse(Pack), ", "), ">>"]), lists:reverse(Comment)};
format_read([#param{name = Name} = H | T], Match, Expression, Pack, Comment) ->
	{MS, PS, C} = read(H),
	MM = choose_name(Name, undefined),
	NewComment = case C of [_ | _] -> lists:reverse(C) ++ Comment; _ -> [C | Comment] end,
	case H of
		#param{desc = [_]} ->
			ListLength = MM ++ "Length",
			case lists:keymember("string", 2, C) of
				true ->
					MX = format("Binary:~s/binary", [ListLength]);
				_ ->
					MX = format("Binary:~s/binary-unit:~p", [ListLength, lists:sum([list_to_integer(X) || {_, X, _} <- C])])
			end,
			E = lists:concat(["    ", MM, " = [", PS, "]"]),
			P = ListLength ++ ":16, " ++ MM ++ MX,
			format_read(T, [MS | Match], [E | Expression], [P | Pack], [C | Comment]);
		_ ->
			format_read(T, [MS | Match], Expression, [PS | Pack], NewComment)
	end.
%% format bit unit
read(#param{name = Outer, desc = 8, comment = O}) ->
	Hump = choose_name(Outer, undefined),
	{format("~s", [Hump]), format("~s:8", [Hump]), {"", "8", O}};
read(#param{name = Outer, desc = 16, comment = O}) ->
	Hump = choose_name(Outer, undefined),
	{format("~s", [Hump]), format("~s:16", [Hump]), {"", "16", O}};
read(#param{name = Outer, desc = 32, comment = O}) ->
	Hump = choose_name(Outer, undefined),
	{format("~s", [Hump]), format("~s:32", [Hump]), {"", "32", O}};
read(#param{name = Outer, desc = 64, comment = O}) ->
	Hump = choose_name(Outer, undefined),
	{format("~s", [Hump]), format("~s:64", [Hump]), {"", "64", O}};
read(#param{name = Outer, desc = 128, comment = O}) ->
	Hump = choose_name(Outer, undefined),
	{format("~s", [Hump]), format("~s:128", [Hump]), {"", "128", O}};
read(#param{name = Outer, desc = str, comment = O}) ->
	Hump = choose_name(Outer, undefined),
	{format("~s", [Hump]), format("~s:16, ~s:~s/binary", [Hump ++ "Length", Hump, Hump ++ "Length"]), {"", "string", O}};
read(#param{name = Outer, desc = btr, comment = O}) ->
	Hump = choose_name(Outer, undefined),
	{format("binary_to_list(~s)", [Hump]), format("~s:16, ~s:~s/binary", [Hump ++ "Length", Hump, Hump ++ "Length"]), {"", "string", O}};
read(#param{name = Outer, desc = #u8{name = Inner, comment = I}, comment = O}) ->
	Hump = choose_name(Outer, Inner),
	{format("~s", [Hump]), format("~s:8", [Hump]), {I, "8", O}};
read(#param{name = Outer, desc = #u16{name = Inner, comment = I}, comment = O}) ->
	Hump = choose_name(Outer, Inner),
	{format("~s", [Hump]), format("~s:16", [Hump]), {I, "16", O}};
read(#param{name = Outer, desc = #u32{name = Inner, comment = I}, comment = O}) ->
	Hump = choose_name(Outer, Inner),
	{format("~s", [Hump]), format("~s:32", [Hump]), {I, "32", O}};
read(#param{name = Outer, desc = #u64{name = Inner, comment = I}, comment = O}) ->
	Hump = choose_name(Outer, Inner),
	{format("~s", [Hump]), format("~s:64", [Hump]), {I, "64", O}};
read(#param{name = Outer, desc = #u128{name = Inner, comment = I}, comment = O}) ->
	Hump = choose_name(Outer, Inner),
	{format("~s", [Hump]), format("~s:128", [Hump]), {I, "128", O}};
read(#param{name = Outer, desc = #str{name = Inner, comment = I}, comment = O}) ->
	Hump = choose_name(Outer, Inner),
	{format("binary_to_list(~s)", [Hump]), format("~s:16, ~s:~s/binary", [Hump ++ "Length", Hump, Hump ++ "Length"]), {I, "string", O}};
read(#param{name = Outer, desc = #btr{name = Inner, comment = I}, comment = O}) ->
	Hump = choose_name(Outer, Inner),
	{format("~s", [Hump]), format("~s:16, ~s:~s/binary", [Hump ++ "Length", Hump, Hump ++ "Length"]), {I, "string", O}};
read(#param{name = Name, desc = [D], comment = OC}) ->
	Hump = choose_name(Name, undefined),
	{M, P, C} = read(#param{desc = D, comment = OC}),
	{format("~s", [Hump]), format("~s || <<~s>> <= ~s", [M, P, Hump ++ "Binary"]), C};
read(#param{desc = Record, comment = OC}) when is_tuple(Record) andalso is_atom(element(1, Record)) ->
	Tag = element(1, Record),
	NameList = case beam:find(Tag) of error -> erlang:throw("no such beam~n"); {_, NL} -> NL end,
	F = fun(#param{name = Name} = Param) -> case read(Param) of {M = [_ | _], P, X} -> {format("~s = ~s", [hump(Name), M]), P, X};{M, P, X} -> {M, P, X} end end,
	List = [F(#param{desc = Desc, name = Name, comment = OC}) || {Desc, Name} <- lists:zip(tuple_to_list(Record), NameList)],
	M = lists:concat(["#", Tag, "{", string:join([X || {X = [_ | _], _, _} <- List], ", "), "}"]),
	P = string:join([X || {_, X = [_ | _], _} <- List], ", "),
	C = [X || {_, _, X} <- List],
	{M, P, C};
read(#param{name = Name, desc = D, comment = OC}) when is_tuple(D) ->
	NameList = [format("~s", [choose_name(Name, undefined)]) || _ <- lists:seq(1, tuple_size(D))],
	List = [read(#param{desc = DD, name = N, comment = OC}) || {DD, N} <- lists:zip(tuple_to_list(D), NameList)],
	M = lists:concat(["{", string:join([X || {X = [_ | _], _, _} <- List], ", "), "}"]),
	P = string:join([X || {_, X = [_ | _], _} <- List], ", "),
	C = [X || {_, _, X} <- List],
	{M, P, C};
read(_) ->
	{"", "", ""}.


%% write code format
format_write(_Code, []) ->
	{"", ""};
format_write(Code, List) ->
	{M, E, P, C} = format_write(List, [], [], [], []),
	{lists:concat(["write(", Code, ", [", M, "]) ->\n"]) ++ E ++ lists:concat(["    {ok, protocol:pack(", Code, ", ", P, ")};\n\n"]), C}.
format_write([], Match, Expression, Pack, Comment) ->
	{string:join(lists:reverse(Match), ", "), [X ++ ",\n" || X = [_ | _] <- lists:reverse(Expression)], lists:concat(["<<", string:join(lists:reverse(Pack), ", "), ">>"]), lists:reverse(Comment)};
format_write([#param{name = Name} = H | T], Match, Expression, Pack, Comment) ->
	{MS, PS, C} = write(H),
	NewComment = case C of [_ | _] -> lists:reverse(C) ++ Comment; _ -> [C | Comment] end,
	MM = choose_name(Name, undefined),
	case H of
		#param{desc = [_]} ->
			E = lists:concat(["    ", MM, "Binary = <<", PS, ">>"]),
			P = MM ++ "Binary/binary",
			format_write(T, [MS | Match], [E | Expression], [P | Pack], [C | Comment]);
		_ when length(PS) >= 30 ->
			E = lists:concat(["    ", MM, "Binary = <<", PS, ">>"]),
			P = MM ++ "Binary/binary",
			format_write(T, [MS | Match], [E | Expression], [P | Pack], NewComment);
		_ ->
			format_write(T, [MS | Match], Expression, [PS | Pack], NewComment)
	end.
%% format bit unit
write(#param{desc = #zero{}}) ->
	{"_", "", ""};
write(#param{desc = 0, extra = tuple}) ->
	{"_", "", ""};
write(#param{desc = 0}) ->
	{"", "", ""};
write(#param{name = Outer, desc = 8, comment = O}) ->
	Hump = choose_name(Outer, undefined),
	{format("~s", [Hump]), format("~s:8", [Hump]), {"", "8", O}};
write(#param{name = Outer, desc = 16, comment = O}) ->
	Hump = choose_name(Outer, undefined),
	{format("~s", [Hump]), format("~s:16", [Hump]), {"", "16", O}};
write(#param{name = Outer, desc = 32, comment = O}) ->
	Hump = choose_name(Outer, undefined),
	{format("~s", [Hump]), format("~s:32", [Hump]), {"", "32", O}};
write(#param{name = Outer, desc = 64, comment = O}) ->
	Hump = choose_name(Outer, undefined),
	{format("~s", [Hump]), format("~s:64", [Hump]), {"", "64", O}};
write(#param{name = Outer, desc = 128, comment = O}) ->
	Hump = choose_name(Outer, undefined),
	{format("~s", [Hump]), format("~s:128", [Hump]), {"", "128", O}};
write(#param{name = Outer, desc = str, comment = O}) ->
	Hump = choose_name(Outer, undefined),
	{format("~s", [Hump]), format("(length(~s)):16, (iolist_to_binary(~s))/binary", [Hump, Hump]), {"", "string", O}};
write(#param{name = Outer, desc = btr, comment = O}) ->
	Hump = choose_name(Outer, undefined),
	{format("~s", [Hump]), format("(byte_size(~s)):16, (iolist_to_binary(~s))/binary", [Hump, Hump]), {"", "string", O}};
write(#param{name = Outer, desc = #u8{name = Inner, comment = I}, comment = O}) ->
	Hump = choose_name(Outer, Inner),
	{format("~s", [Hump]), format("~s:8", [Hump]), {I, "8", O}};
write(#param{name = Outer, desc = #u16{name = Inner, comment = I}, comment = O}) ->
	Hump = choose_name(Outer, Inner),
	{format("~s", [Hump]), format("~s:16", [Hump]), {I, "16", O}};
write(#param{name = Outer, desc = #u32{name = Inner, comment = I}, comment = O}) ->
	Hump = choose_name(Outer, Inner),
	{format("~s", [Hump]), format("~s:32", [Hump]), {I, "32", O}};
write(#param{name = Outer, desc = #u64{name = Inner, comment = I}, comment = O}) ->
	Hump = choose_name(Outer, Inner),
	{format("~s", [Hump]), format("~s:64", [Hump]), {I, "64", O}};
write(#param{name = Outer, desc = #u128{name = Inner, comment = I}, comment = O}) ->
	Hump = choose_name(Outer, Inner),
	{format("~s", [Hump]), format("~s:128", [Hump]), {I, "128", O}};
write(#param{name = Outer, desc = #str{name = Inner, comment = I}, comment = O}) ->
	Hump = choose_name(Outer, Inner),
	{format("~s", [Hump]), format("(length(~s)):16, (iolist_to_binary(~s))/binary", [Hump, Hump]), {I, "string", O}};
write(#param{name = Outer, desc = #btr{name = Inner, comment = I}, comment = O}) ->
	Hump = choose_name(Outer, Inner),
	{format("~s", [Hump]), format("(byte_size(~s)):16, (iolist_to_binary(~s))/binary", [Hump, Hump]), {I, "string", O}};
write(#param{name = Name, desc = [D], comment = OC}) ->
	Hump = choose_name(Name, undefined),
	{M, P, C} = write(#param{desc = D, comment = OC}),
	{format("~s", [Hump]), format("(length(~s)):16, <<<<~s>> || ~s <- ~s>>", [Hump, P, M, Hump]), C};
write(#param{desc = Record, comment = OC}) when is_tuple(Record) andalso is_atom(element(1, Record)) ->
	Tag = element(1, Record),
	NameList = case beam:find(Tag) of error -> erlang:throw("no such beam~n"); {_, NL} -> NL end,
	F = fun(#param{name = Name} = Param) -> case write(Param) of {M = [_ | _], P, X} -> {format("~s = ~s", [Name, M]), P, X};{M, P, X} -> {M, P, X} end end,
	List = [F(#param{desc = Desc, name = Name, comment = OC}) || {Desc, Name} <- lists:zip(tuple_to_list(Record), NameList)],
	M = lists:concat(["#", Tag, "{", string:join([X || {X = [_ | _], _, _} <- List], ", "), "}"]),
	P = string:join([X || {_, X = [_ | _], _} <- List], ", "),
	C = [X || {_, _, X} <- List],
	{M, P, C};
write(#param{name = Name, desc = D, comment = OC}) when is_tuple(D) ->
	NameList = [format("~s", [choose_name(Name, undefined)]) || _ <- lists:seq(1, tuple_size(D))],
	List = [write(#param{desc = DD, name = N, comment = OC, extra = tuple}) || {DD, N} <- lists:zip(tuple_to_list(D), NameList)],
	M = lists:concat(["{", string:join([X || {X = [_ | _], _, _} <- List], ", "), "}"]),
	P = string:join([X || {_, X = [_ | _], _} <- List], ", "),
	C = [X || {_, _, X} <- List],
	{M, P, C};
write(_) ->
	{"", "", ""}.


%% auto make name
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
choose_name(Outer, undefined) ->
	hump(Outer);
choose_name(undefined, Inner) ->
	hump(Inner);
choose_name(_, Inner) ->
	hump(Inner).

%% auto make comment
choose_comment(undefined, undefined) ->
	case get('comment') of
		undefined ->
			Name = lists:concat([undefined, 1]),
			put('comment', 1),
			hump(Name);
		AI ->
			Name = lists:concat([undefined, AI + 1]),
			put('comment', AI + 1),
			hump(Name)
	end;
choose_comment(Outer, undefined) ->
	hump(Outer);
choose_comment(undefined, Inner) ->
	hump(Inner);
choose_comment(_, Inner) ->
	hump(Inner).

%% hump name
hump(Binary) when is_binary(Binary) ->
	hump(binary_to_list(Binary));
hump(Atom) when is_atom(Atom) ->
	hump(atom_to_list(Atom));
hump(Name) ->
	lists:concat([[case 96 < H andalso H < 123 of true -> H - 32; _ -> H end | T] || [H | T] <- string:tokens(Name, "_")]).

%% format
format(F, A) ->
	binary_to_list(list_to_binary(io_lib:format(F, A))).
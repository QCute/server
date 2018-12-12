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
	[{"(?s).*", Head ++ Include ++ Code}].

collect_code([], ReadList, WriteList, CommentList) ->
	ReadMatch = "read(Code, Binary) ->\n    {error, Code, Binary}.\n\n",
	WriteMatch = "write(Code, Content) ->\n    {error, Code, Content}.\n",
	{lists:concat(["\n\n", lists:reverse(ReadList), ReadMatch, "\n\n", lists:reverse(WriteList), WriteMatch]), CommentList};
collect_code([#io{read = Read, write = Write, name = Name} | T], ReadList, WriteList, CommentList) ->
	{ReadCode, ReadComment} = format_read(Name, Read),
	erase(),
	{WriteCode, WriteComment} = format_write(Name, Write),
	erase(),
	collect_code(T, [ReadCode | ReadList], [WriteCode | WriteList], [ReadComment ++ "\n<< == >>\n" ++ WriteComment | CommentList]).
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
	case H of
		#param{desc = [_]} ->
			BitList = string:tokens(C, ", "),
			ListLength = MM ++ "Length",
			case lists:member("string", BitList) of
				true ->
					MX = format("Binary:~s/binary", [ListLength]);
				_ ->
					MX = format("Binary:~s/binary-unit:~p", [ListLength, lists:sum([list_to_integer(X) || X <- BitList])])
			end,
			E = lists:concat(["    ", MM, " = [", PS, "]"]),
			P = ListLength ++ ":16, " ++ MM ++ MX,
			format_read(T, [MS | Match], [E | Expression], [P | Pack], [C | Comment]);
		_ ->
			format_read(T, [MS | Match], Expression, [PS | Pack], [C | Comment])
	end.
%% format bit unit
read(#param{name = Outer, desc = 8}) ->
	Hump = choose_name(Outer, undefined),
	{format("~s", [Hump]), format("~s:8", [Hump]), "8"};
read(#param{name = Outer, desc = 16}) ->
	Hump = choose_name(Outer, undefined),
	{format("~s", [Hump]), format("~s:16", [Hump]), "16"};
read(#param{name = Outer, desc = 32}) ->
	Hump = choose_name(Outer, undefined),
	{format("~s", [Hump]), format("~s:32", [Hump]), "32"};
read(#param{name = Outer, desc = 64}) ->
	Hump = choose_name(Outer, undefined),
	{format("~s", [Hump]), format("~s:64", [Hump]), "64"};
read(#param{name = Outer, desc = 128}) ->
	Hump = choose_name(Outer, undefined),
	{format("~s", [Hump]), format("~s:128", [Hump]), "128"};
read(#param{name = Outer, desc = str}) ->
	Hump = choose_name(Outer, undefined),
	{format("~s", [Hump]), format("~s:16, ~s:~s/binary", [Hump ++ "Length", Hump, Hump ++ "Length"]), "string"};
read(#param{name = Outer, desc = #u8{name = Inner}}) ->
	Hump = choose_name(Outer, Inner),
	{format("~s", [Hump]), format("~s:8", [Hump]), "8"};
read(#param{name = Outer, desc = #u16{name = Inner}}) ->
	Hump = choose_name(Outer, Inner),
	{format("~s", [Hump]), format("~s:16", [Hump]), "16"};
read(#param{name = Outer, desc = #u32{name = Inner}}) ->
	Hump = choose_name(Outer, Inner),
	{format("~s", [Hump]), format("~s:32", [Hump]), "32"};
read(#param{name = Outer, desc = #u64{name = Inner}}) ->
	Hump = choose_name(Outer, Inner),
	{format("~s", [Hump]), format("~s:64", [Hump]), "64"};
read(#param{name = Outer, desc = #u128{name = Inner}}) ->
	Hump = choose_name(Outer, Inner),
	{format("~s", [Hump]), format("~s:128", [Hump]), "128"};
read(#param{name = Outer, desc = #str{name = Inner}}) ->
	Hump = choose_name(Outer, Inner),
	{format("binary_to_list(~s)", [Hump]), format("~s:16, ~s:~s/binary", [Hump ++ "Length", Hump, Hump ++ "Length"]), "string"};
read(#param{name = Outer, desc = #btr{name = Inner}}) ->
	Hump = choose_name(Outer, Inner),
	{format("~s", [Hump]), format("~s:16, ~s:~s/binary", [Hump ++ "Length", Hump, Hump ++ "Length"]), "string"};
read(#param{name = Name, desc = [D]}) ->
	Hump = choose_name(Name, undefined),
	{M, P, C} = read(#param{desc = D}),
	{format("~s", [Hump]), format("~s || <<~s>> <= ~s", [M, P, Hump ++ "Binary"]), C};
read(#param{desc = Record}) when is_tuple(Record) andalso is_atom(element(1, Record)) ->
	Tag = element(1, Record),
	NameList = case beam:find(Tag) of error -> erlang:throw("no such beam~n"); {_, NL} -> NL end,
	F = fun(#param{name = Name} = Param) -> case read(Param) of {M = [_ | _], P, X} -> {format("~s = ~s", [hump(Name), M]), P, X};{M, P, X} -> {M, P, X} end end,
	List = [F(#param{desc = Desc, name = Name}) || {Desc, Name} <- lists:zip(tuple_to_list(Record), NameList)],
	M = lists:concat(["#", Tag, "{", string:join([X || {X = [_ | _], _, _} <- List], ", "), "}"]),
	P = string:join([X || {_, X = [_ | _], _} <- List], ", "),
	C = string:join([X || {_, _, X = [_ | _]} <- List], ", "),
	{M, P, C};
read(#param{name = Name, desc = D}) when is_tuple(D) ->
	NameList = [format("~s", [choose_name(Name, undefined)]) || _ <- lists:seq(1, tuple_size(D))],
	List = [read(#param{desc = DD, name = N}) || {DD, N} <- lists:zip(tuple_to_list(D), NameList)],
	M = lists:concat(["{", string:join([X || {X = [_ | _], _, _} <- List], ", "), "}"]),
	P = string:join([X || {_, X = [_ | _], _} <- List], ", "),
	C = string:join([X || {_, _, X = [_ | _]} <- List], ", "),
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
	MM = choose_name(Name, undefined),
	case H of
		#param{desc = [_]} ->
			E = lists:concat(["    ", MM, "Binary = <<", PS, ">>"]),
			P = MM ++ "Binary/binary",
			format_write(T, [MS | Match], [E | Expression], [P | Pack], [C | Comment]);
		_ when length(PS) >= 30 ->
			E = lists:concat(["    ", MM, "Binary = <<", PS, ">>"]),
			P = MM ++ "Binary/binary",
			format_write(T, [MS | Match], [E | Expression], [P | Pack], [C | Comment]);
		_ ->
			format_write(T, [MS | Match], Expression, [PS | Pack], [C | Comment])
	end.
%% format bit unit
write(#param{desc = #zero{}}) ->
	{"_", "", ""};
write(#param{desc = 0, extra = tuple}) ->
	{"_", "", ""};
write(#param{desc = 0}) ->
	{"", "", ""};
write(#param{name = Outer, desc = 8}) ->
	Hump = choose_name(Outer, undefined),
	{format("~s", [Hump]), format("~s:8", [Hump]), "8"};
write(#param{name = Outer, desc = 16}) ->
	Hump = choose_name(Outer, undefined),
	{format("~s", [Hump]), format("~s:16", [Hump]), "16"};
write(#param{name = Outer, desc = 32}) ->
	Hump = choose_name(Outer, undefined),
	{format("~s", [Hump]), format("~s:32", [Hump]), "32"};
write(#param{name = Outer, desc = 64}) ->
	Hump = choose_name(Outer, undefined),
	{format("~s", [Hump]), format("~s:64", [Hump]), "64"};
write(#param{name = Outer, desc = 128}) ->
	Hump = choose_name(Outer, undefined),
	{format("~s", [Hump]), format("~s:128", [Hump]), "128"};
write(#param{name = Outer, desc = str}) ->
	Hump = choose_name(Outer, undefined),
	{format("~s", [Hump]), format("(length(~s)):16, (iolist_to_binary(~s))/binary", [Hump, Hump]), "string"};
write(#param{name = Outer, desc = #u8{name = Inner}}) ->
	Hump = choose_name(Outer, Inner),
	{format("~s", [Hump]), format("~s:8", [Hump]), "8"};
write(#param{name = Outer, desc = #u16{name = Inner}}) ->
	Hump = choose_name(Outer, Inner),
	{format("~s", [Hump]), format("~s:16", [Hump]), "16"};
write(#param{name = Outer, desc = #u32{name = Inner}}) ->
	Hump = choose_name(Outer, Inner),
	{format("~s", [Hump]), format("~s:32", [Hump]), "32"};
write(#param{name = Outer, desc = #u64{name = Inner}}) ->
	Hump = choose_name(Outer, Inner),
	{format("~s", [Hump]), format("~s:64", [Hump]), "64"};
write(#param{name = Outer, desc = #u128{name = Inner}}) ->
	Hump = choose_name(Outer, Inner),
	{format("~s", [Hump]), format("~s:128", [Hump]), "128"};
write(#param{name = Outer, desc = #str{name = Inner}}) ->
	Hump = choose_name(Outer, Inner),
	{format("~s", [Hump]), format("(length(~s)):16, (iolist_to_binary(~s))/binary", [Hump, Hump]), "string"};
write(#param{name = Outer, desc = #btr{name = Inner}}) ->
	Hump = choose_name(Outer, Inner),
	{format("~s", [Hump]), format("(byte_size(~s)):16, (iolist_to_binary(~s))/binary", [Hump, Hump]), "string"};
write(#param{name = Name, desc = [D]}) ->
	Hump = choose_name(Name, undefined),
	{M, P, C} = write(#param{desc = D}),
	{format("~s", [Hump]), format("(length(~s)):16, <<<<~s>> || ~s <- ~s>>", [Hump, P, M, Hump]), C};
write(#param{desc = Record}) when is_tuple(Record) andalso is_atom(element(1, Record)) ->
	Tag = element(1, Record),
	NameList = case beam:find(Tag) of error -> erlang:throw("no such beam~n"); {_, NL} -> NL end,
	F = fun(#param{name = Name} = Param) -> case write(Param) of {M = [_ | _], P, X} -> {format("~s = ~s", [Name, M]), P, X};{M, P, X} -> {M, P, X} end end,
	List = [F(#param{desc = Desc, name = Name}) || {Desc, Name} <- lists:zip(tuple_to_list(Record), NameList)],
	M = lists:concat(["#", Tag, "{", string:join([X || {X = [_ | _], _, _} <- List], ", "), "}"]),
	P = string:join([X || {_, X = [_ | _], _} <- List], ", "),
	C = string:join([X || {_, _, X = [_ | _]} <- List], ", "),
	{M, P, C};
write(#param{name = Name, desc = D}) when is_tuple(D) ->
	NameList = [format("~s", [choose_name(Name, undefined)]) || _ <- lists:seq(1, tuple_size(D))],
	List = [write(#param{desc = DD, name = N, extra = tuple}) || {DD, N} <- lists:zip(tuple_to_list(D), NameList)],
	M = lists:concat(["{", string:join([X || {X = [_ | _], _, _} <- List], ", "), "}"]),
	P = string:join([X || {_, X = [_ | _], _} <- List], ", "),
	C = string:join([X || {_, _, X = [_ | _]} <- List], ", "),
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
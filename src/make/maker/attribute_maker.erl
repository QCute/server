%%%-------------------------------------------------------------------
%%% @doc
%%% module record maker
%%% database fields to record tool
%%% @end
%%%-------------------------------------------------------------------
-module(attribute_maker).
-export([start/1]).
-export([parse/2]).
%%%===================================================================
%%% API
%%%===================================================================
%% @doc for shell
start(List) ->
    maker:start(fun parse_table/2, List).

%% @doc parse
parse(DataBase, One) ->
    parse_table(DataBase, One).
%% ====================================================================
%% Internal functions
%% ====================================================================
%% parse per table
parse_table(DataBase, {File, Table, Name}) ->
    Data = maker:select(io_lib:format("SELECT `key`, `name`, `merge` FROM ~s.`~s`", [DataBase, Table])),
    case filename:extension(File) of
        ".erl" ->
            Hump = hump(Name),
            %% merge with k,v type data
            TypeKV = [io_lib:format("merge_kv({~p, Value}, ~s = #~s{~s = ~s}) ->~n    ~s#~s{~s = ~s + Value};~n", [K, Hump, Name, N, hump(N), Hump, Name, N, hump(M)]) || [K, N, M] <- Data, M =/= <<>>],
            KVCode = TypeKV ++ io_lib:format("merge_kv(_, ~s) ->~n    ~s.~n", [Hump, Hump]),
            %% merge with record type data
            TypeRecord = [io_lib:format("        ~s = X#~s.~s + Y#~s.~s", [N, Name, M, Name, N]) || [_, N, M] <- Data, M =/= <<>>],
            RecordCode = io_lib:format("merge_record(X, Y) ->\n    Y#~s{\n", [Name]) ++ string:join(TypeRecord, ",\n") ++ "\n    }.\n",
            %% replace data
            [{"(?m)(?s)(?<!\\S)(^merge_kv.+?)(?=\\.$|\\%)\\.\\n?", KVCode}, {"(?m)(?s)(?<!\\S)(^merge_record.+?)(?=\\.$|\\%)\\.\\n?", RecordCode}];
        ".hrl" ->
            CommentSql = io_lib:format(<<"SELECT `TABLE_COMMENT` FROM information_schema.`TABLES` WHERE `TABLE_SCHEMA` = '~s' AND `TABLE_NAME` = '~s';">>, [DataBase, Table]),
            %% fetch table comment
            [[CommentData]] = maker:select(CommentSql),
            %% write record data and table comment
            Comment = io_lib:format("%% ~s\n%% ~s =====> ~s", [CommentData, Table, Name]),
            RecordData = Comment ++ "\n-record(" ++ Name ++ ", {\n" ++ string:join([io_lib:format("    ~s = 0", [N]) || [_, N, _] <- Data], ",\n") ++ "\n}).\n",
            RecordPattern = io_lib:format("~s\n(?m)(?s)(?<!\\S)(-record\\s*\\(\\s*~s\\s*,.+?)(?=\\.$|\\.\\%)\\.\n?\n?", [Comment, Name]),
            [{RecordPattern, RecordData}]
    end.

%% hump name
hump(Binary) when is_binary(Binary) ->
    hump(binary_to_list(Binary));
hump(Atom) when is_atom(Atom) ->
    hump(atom_to_list(Atom));
hump(Name) ->
    lists:concat([[case 96 < H andalso H < 123 of true -> H - 32; _ -> H end | T] || [H | T] <- string:tokens(Name, "_")]).

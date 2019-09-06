%%%-------------------------------------------------------------------
%%% @doc
%%% module record maker
%%% database fields to record tool
%%% @end
%%%-------------------------------------------------------------------
-module(attribute_maker).
-export([start/1]).
%%%===================================================================
%%% API
%%%===================================================================
%% @doc for shell
start(List) ->
    maker:start(fun parse_table/2, List).

%% ====================================================================
%% Internal functions
%% ====================================================================
%% parse per table
parse_table(DataBase, {File, Table, Name}) ->
    Data = maker:select(io_lib:format("SELECT `attribute_id`, `attribute`, `merge`, `description` FROM ~s.`~s`", [DataBase, Table])),
    case filename:extension(File) of
        ".erl" ->
            Hump = maker:hump(Name),
            %% merge with k,v type data
            TypeKV = [io_lib:format("merge_kv({~p, Value}, ~s = #~s{~s = ~s}) ->~n    ~s#~s{~s = ~s + Value};~n", [I, Hump, Name, N, maker:hump(N), Hump, Name, N, maker:hump(M)]) || [I, N, M, _] <- Data, M =/= <<>>],
            KVCode = TypeKV ++ io_lib:format("merge_kv(_, ~s) ->~n    ~s.~n", [Hump, Hump]),
            %% merge with record type data
            TypeRecord = [io_lib:format("        ~s = X#~s.~s + Y#~s.~s", [N, Name, M, Name, N]) || [_, N, M, _] <- Data, M =/= <<>>],
            RecordCode = io_lib:format("merge_record(X, Y) ->\n    Y#~s{\n", [Name]) ++ string:join(TypeRecord, ",\n") ++ "\n    }.\n",
            %% replace data
            [{"(?m)(?s)(?<!\\S)(^merge_kv.+?)(?=\\.$|\\%)\\.\\n?", KVCode}, {"(?m)(?s)(?<!\\S)(^merge_record.+?)(?=\\.$|\\%)\\.\\n?", RecordCode}];
        ".hrl" ->
            CommentSql = io_lib:format(<<"SELECT `TABLE_COMMENT` FROM information_schema.`TABLES` WHERE `TABLE_SCHEMA` = '~s' AND `TABLE_NAME` = '~s';">>, [DataBase, Table]),
            %% fetch table comment
            [[CommentData]] = maker:select(CommentSql),
            %% write record data and table comment
            Comment = io_lib:format("%% ~s\n%% ~s =====> ~s", [CommentData, Table, Name]),
            RecordData = Comment ++ "\n-record(" ++ Name ++ ", {\n" ++ format_field(Data, []) ++ "}).\n",
            RecordPattern = io_lib:format("~s\n(?m)(?s)(?<!\\S)(-record\\s*\\(\\s*~s\\s*,.+?)(?=\\.$|\\.\\%)\\.\n?\n?", [Comment, Name]),
            [{RecordPattern, RecordData}]
    end.

%% format record field, default value and comment
format_field([], Result) ->
    lists:reverse(Result);
format_field([[_, Name, _, Description] | T], Result) ->
    %% record field end comma
    case T of
        [] ->
            Comma = "";
        _ ->
            Comma = ","
    end,
    FiledDefault = " = 0",
    %% format record field expression
    Expression = io_lib:format("~s~s~s", [Name, FiledDefault, Comma]),
    %% calculate alignment space
    Alignment = lists:duplicate(50 - length(lists:flatten(Expression)), " "),
    %% align comment
    Row = io_lib:format("    ~s~s%% ~s \n", [Expression, Alignment, Description]),
    format_field(T, [Row | Result]).


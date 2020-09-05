%%%-------------------------------------------------------------------
%%% @doc
%%% make attribute table value to attribute record and merge/subtract code
%%% @end
%%%-------------------------------------------------------------------
-module(attribute_maker).
-export([start/1]).
%%%===================================================================
%%% API functions
%%%===================================================================
%% @doc for shell
start(List) ->
    maker:start(fun parse_table/2, List).

%%%===================================================================
%%% Internal functions
%%%===================================================================
%% parse per table
parse_table(DataBase, {File, Table, Name}) ->
    Data = maker:select(io_lib:format("SELECT `attribute_id`, `attribute`, `merge`, `description` FROM ~s.`~s`", [DataBase, Table])),
    case filename:extension(File) of
        ".erl" ->
            Hump = word:to_hump(Name),
            %% merge with k,v type data
            MergeTypeKV = [io_lib:format("merge_kv({~w, Value}, ~s = #~s{~s = ~s}) ->~n    ~s#~s{~s = ~s + Value};~n", [I, Hump, Name, N, word:to_hump(N), Hump, Name, N, word:to_hump(M)]) || [I, N, M, _] <- Data, M =/= <<>>],
            MergeKVCode = MergeTypeKV ++ io_lib:format("merge_kv(_, ~s) ->~n    ~s.~n", [Hump, Hump]),
            %% merge with record type data
            MergeTypeRecord = [io_lib:format("        ~s = X#~s.~s + Y#~s.~s", [N, Name, M, Name, N]) || [_, N, M, _] <- Data, M =/= <<>>],
            MergeRecordCode = io_lib:format("merge_record(X, Y) ->\n    Y#~s{\n", [Name]) ++ string:join(MergeTypeRecord, ",\n") ++ "\n    }.\n",
            %% subtract with k,v type data
            SubtractTypeKV = [io_lib:format("subtract_kv({~w, Value}, ~s = #~s{~s = ~s}) ->~n    ~s#~s{~s = ~s - Value};~n", [I, Hump, Name, N, word:to_hump(N), Hump, Name, N, word:to_hump(M)]) || [I, N, M, _] <- Data, M =/= <<>>],
            SubtractKVCode = SubtractTypeKV ++ io_lib:format("subtract_kv(_, ~s) ->~n    ~s.~n", [Hump, Hump]),
            %% subtract with record type data
            SubtractTypeRecord = [io_lib:format("        ~s = X#~s.~s - Y#~s.~s", [N, Name, M, Name, N]) || [_, N, M, _] <- Data, M =/= <<>>],
            SubtractRecordCode = io_lib:format("subtract_record(X, Y) ->\n    Y#~s{\n", [Name]) ++ string:join(SubtractTypeRecord, ",\n") ++ "\n    }.\n",
            %% replace data
            [{"(?m)(?s)(?<!\\S)(^merge_kv.+?)(?=\\.$|\\%)\\.\\n?", MergeKVCode}, {"(?m)(?s)(?<!\\S)(^merge_record.+?)(?=\\.$|\\%)\\.\\n?", MergeRecordCode}, {"(?m)(?s)(?<!\\S)(^subtract_kv.+?)(?=\\.$|\\%)\\.\\n?", SubtractKVCode}, {"(?m)(?s)(?<!\\S)(^subtract_record.+?)(?=\\.$|\\%)\\.\\n?", SubtractRecordCode}];
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


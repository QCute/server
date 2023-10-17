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
    maker:start(fun parse_table/1, List).

%%%===================================================================
%%% Internal functions
%%%===================================================================
%% parse per table
parse_table(#{file := File, table := Table, name := Name}) ->
    Data = db:select("SELECT `attribute_id`, `attribute`, `merge`, `description` FROM `~s`", [Table]),
    case filename:extension(File) of
        ".erl" ->
            Hump = word:to_hump(Name),
            %% merge with k,v type data
            MergeTypeKVPattern = "(?m)(?s)(?<!\\S)(^merge_kv.+?)(?=\\.$|\\%)\\.\\n?",
            MergeTypeKV = [io_lib:format("merge_kv(~s = #~s{~s = ~s}, {~w, Value}) ->~n    ~s#~s{~s = ~s + Value};~n", [Hump, Name, N, word:to_hump(N), I, Hump, Name, N, word:to_hump(M)]) || [I, N, M, _] <- Data, M =/= <<>>],
            MergeKVCode = MergeTypeKV ++ io_lib:format("merge_kv(_, ~s) ->~n    ~s.~n", [Hump, Hump]),
            %% merge with record type data
            MergeTypeRecordPattern = "(?m)(?s)(?<!\\S)(^merge_record.+?)(?=\\.$|\\%)\\.\\n?",
            MergeTypeRecord = [io_lib:format("        ~s = X#~s.~s + Y#~s.~s", [N, Name, M, Name, N]) || [_, N, M, _] <- Data, M =/= <<>>],
            MergeRecordCode = io_lib:format("merge_record(X, Y) ->\n    Y#~s{\n", [Name]) ++ string:join(MergeTypeRecord, ",\n") ++ "\n    }.\n",
            %% subtract with k,v type data
            SubtractTypeKVPattern = "(?m)(?s)(?<!\\S)(^subtract_kv.+?)(?=\\.$|\\%)\\.\\n?",
            SubtractTypeKV = [io_lib:format("subtract_kv(~s = #~s{~s = ~s}, {~w, Value}) ->~n    ~s#~s{~s = ~s - Value};~n", [Hump, Name, N, word:to_hump(N), I, Hump, Name, N, word:to_hump(M)]) || [I, N, M, _] <- Data, M =/= <<>>],
            SubtractKVCode = SubtractTypeKV ++ io_lib:format("subtract_kv(_, ~s) ->~n    ~s.~n", [Hump, Hump]),
            %% subtract with record type data
            SubtractTypeRecordPattern = "(?m)(?s)(?<!\\S)(^subtract_record.+?)(?=\\.$|\\%)\\.\\n?",
            SubtractTypeRecord = [io_lib:format("        ~s = X#~s.~s - Y#~s.~s", [N, Name, M, Name, N]) || [_, N, M, _] <- Data, M =/= <<>>],
            SubtractRecordCode = io_lib:format("subtract_record(X, Y) ->\n    Y#~s{\n", [Name]) ++ string:join(SubtractTypeRecord, ",\n") ++ "\n    }.\n",
            %% replace data
            [#{pattern => MergeTypeKVPattern, code => MergeKVCode}, #{pattern => MergeTypeRecordPattern, code => MergeRecordCode}, #{pattern => SubtractTypeKVPattern, code => SubtractKVCode}, #{pattern => SubtractTypeRecordPattern, code => SubtractRecordCode}];
        ".hrl" ->
            %% fetch table comment
            [[CommentData]] = db:select(<<"SELECT `TABLE_COMMENT` FROM information_schema.`TABLES` WHERE `TABLE_SCHEMA` = DATABASE() AND `TABLE_NAME` = '~s';">>, [Table]),
            %% write record data and table comment
            Comment = io_lib:format("%% ~s\n%% ~s =====> ~s", [CommentData, Table, Name]),
            RecordPattern = io_lib:format("~s\n(?m)(?s)(?<!\\S)(-record\\s*\\(\\s*~s\\s*,.+?)(?=\\.$|\\.\\%)\\.\n?\n?", [Comment, Name]),
            RecordCode = Comment ++ "\n-record(" ++ Name ++ ", {\n" ++ format_field(Data, []) ++ "}).\n",
            [#{pattern => RecordPattern, code => RecordCode}]
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

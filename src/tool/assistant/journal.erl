%%%-------------------------------------------------------------------
%%% @doc
%%% application runtime journal tool
%%% @end
%%%-------------------------------------------------------------------
-module(journal).
%% API
-export([dump/1]).
-export([print/4, debug/4, info/4, warning/4, error/4]).
-export([print_stacktrace/5]).
-export([format_stacktrace/3, print_error_stacktrace/5]).
-export([format/1, format/2]).
-export([print_row_table/1, print_column_table/1, stringify/1]).
-export([set_prompt/0, prompt_func/1]).
%%%===================================================================
%%% API functions
%%%===================================================================
%% @doc dump pretty
-spec dump(Term :: term()) -> ok.
dump(Term) ->
    error_logger:info_msg("~ts", [pretty:print(Term)]).

%% @doc print(default)
-spec print(Module :: atom(), Line :: pos_integer(), Format :: string(), Args :: [term()]) -> ok.
print(Module, Line, Format, Args) ->
    FormatList = lists:flatten(lists:concat(["[", Module, ":", Line, "] ", Format])),
    error_logger:info_msg(FormatList, Args).

%% @doc debug(blue)
-spec debug(Module :: atom(), Line :: pos_integer(), Format :: string(), Args :: [term()]) -> ok.
debug(Module, Line, Format, Args) ->
    FormatList = lists:flatten(lists:concat(["[", Module, ":", Line, "] ", color:blue(Format)])),
    error_logger:info_msg(FormatList, Args).

%% @doc info(green)
-spec info(Module :: atom(), Line :: pos_integer(), Format :: string(), Args :: [term()]) -> ok.
info(Module, Line, Format, Args) ->
    FormatList = lists:flatten(lists:concat(["[", Module, ":", Line, "] ", color:green(Format)])),
    error_logger:info_msg(FormatList, Args).

%% @doc warning(yellow)
-spec warning(Module :: atom(), Line :: pos_integer(), Format :: string(), Args :: [term()]) -> ok.
warning(Module, Line, Format, Args) ->
    FormatList = lists:flatten(lists:concat(["[", Module, ":", Line, "] ", color:yellow(Format)])),
    error_logger:warning_msg(FormatList, Args).

%% @doc error(red)
-spec error(Module :: atom(), Line :: pos_integer(), Format :: string(), Args :: [term()]) -> ok.
error(Module, Line, Format, Args) ->
    FormatList = lists:flatten(lists:concat(["[", Module, ":", Line, "] ", color:red(Format)])),
    error_logger:error_msg(FormatList, Args).

%% @doc print formatted stacktrace message
-spec print_stacktrace(Module :: atom(), Line :: non_neg_integer(), Class :: atom(), Reason :: term(), Stacktrace :: term()) -> ok.
print_stacktrace(Module, Line, Class, Reason, Stacktrace) ->
    String = format_stacktrace(Class, Reason, Stacktrace),
    notifier:notify(Module, Line, String),
    error_logger:error_msg(String).

%% @doc print formatted stacktrace message to standard error
-spec print_error_stacktrace(Module :: atom(), Line :: non_neg_integer(), Class :: atom(), Reason :: term(), Stacktrace :: term()) -> ok.
print_error_stacktrace(Module, Line, Class, Reason, Stacktrace) ->
    String = journal:format_stacktrace(Class, Reason, Stacktrace),
    notifier:notify(Module, Line, String),
    io:format(standard_error, "~ts~n", [String]).

%% @doc format stacktrace message
-spec format_stacktrace(Class :: atom(), Reason :: term(), Stacktrace :: term()) -> string().
format_stacktrace(Class, Reason, StackTrace) ->
    Option = maps:put(format_fun, fun(T, _) -> io_lib:format("~tp", [T]) end, maps:new()),
    erl_error:format_exception(Class, Reason, StackTrace, Option).

%% @doc print to remote tty
-spec format(Format :: string()) -> ok.
format(Format) ->
    format(Format, []).

%% @doc print to remote tty
-spec format(Format :: string(), Args :: [term()]) -> ok.
format(Format, Args) ->
    %% find remote group leader list
    %% Process Dictionary
    %%     {evaluator, Pid}
    %%     {group_leader, Pid}
    LeaderList = lists:usort([element(2, erlang:process_info(element(2, lists:keyfind(evaluator, 1, element(2, process_info(Pid, dictionary)))), group_leader)) || Pid <- erlang:processes(), lists:keyfind(evaluator, 1, element(2, process_info(Pid, dictionary))) =/= false]),
    %% io Request
    PidList = [spawn(fun() -> io:format(Leader, Format, Args) end) || Leader <- LeaderList],
    %% kill it after 3 second if process block on io Request
    spawn(fun() -> receive _ -> ok after 3000 -> [erlang:exit(Pid, kill) || Pid <- PidList] end end),
    ok.

%% @doc print row table
-spec print_row_table(Table :: [list() | tuple()]) -> [list() | tuple()].
print_row_table(Table) ->
    {WidthList, DataList} = calculate_row(Table, array:new([{default, 0}]), []),
    _ = [[begin lists:zipwith(fun(Width, Data) -> io:format("~ts~s", [Data, lists:duplicate(Width - calculate_char_width(Data) + 4, 16#20)]) end, lists:sublist(WidthList, length(Row)), Row), io:format("~n") end] || Row <- DataList],
    Table.

calculate_row([], WidthArray, DataList) ->
    {array:to_list(WidthArray), lists:reverse(DataList)};
calculate_row([H | T], WidthArray, DataList) when is_tuple(H) ->
    {NewWidthArray, Data} = calculate_column(tuple_to_list(H), 0, WidthArray, array:new([{default, []}])),
    calculate_row(T, NewWidthArray, [Data | DataList]);
calculate_row([H | T], WidthArray, DataList) ->
    {NewWidthArray, Data} = calculate_column(H, 0, WidthArray, array:new([{default, []}])),
    calculate_row(T, NewWidthArray, [Data | DataList]).

calculate_column([], _, WidthArray, DataArray) ->
    {WidthArray, array:to_list(DataArray)};
calculate_column([H | T], Index, WidthArray, DataArray) ->
    String = lists:flatten(stringify(H)),
    Width = calculate_char_width(String),
    calculate_column(T, Index + 1, array:set(Index, max(array:get(Index, WidthArray), Width), WidthArray), array:set(Index, String, DataArray)).

calculate_char_width(List) ->
    calculate_char_width(List, 0).
calculate_char_width([], Width) ->
    Width;
calculate_char_width([Char | T], Width) when Char > 16#FF ->
    calculate_char_width(T, Width + 2);
calculate_char_width([_ | T], Width) ->
    calculate_char_width(T, Width + 1).

%% @doc print column table
-spec print_column_table(Table :: [list() | tuple()]) -> [list() | tuple()].
print_column_table(Table) ->
    Depth = lists:foldl(fun(R, A) when is_tuple(R) -> max(tuple_size(R), A) ; (R, A) when is_list(R) -> max(length(R), A) end, 0, Table),
    {WidthList, DataList} = calculate_row(transpose_column(Table, Depth, []), array:new([{default, 0}]), []),
    _ = [[begin lists:zipwith(fun(Width, Data) -> io:format("~ts~s", [Data, lists:duplicate(Width - calculate_char_width(Data) + 4, 16#20)]) end, lists:sublist(WidthList, length(Row)), Row), io:format("~n") end] || Row <- DataList],
    Table.

transpose_column(_, 0, List) ->
    List;
transpose_column(Table, Depth, List) ->
    Row = transpose_row(Table, Depth, []),
    transpose_column(Table, Depth - 1, [Row | List]).

transpose_row([], _, List) ->
    lists:reverse(List);
transpose_row([Column | T], Index, List) when is_tuple(Column) andalso Index =< tuple_size(Column) ->
    transpose_row(T, Index, [element(Index, Column) | List]);
transpose_row([Column | T], Index, List) when is_list(Column) andalso Index =< length(Column) ->
    transpose_row(T, Index, [lists:nth(Index, Column) | List]);
transpose_row([_ | T], Index, List) ->
    transpose_row(T, Index, [[] | List]).

%% @doc term stringify
-spec stringify(Data :: term()) -> string().
stringify(Data) when is_list(Data) ->
    case io_lib:printable_unicode_list(Data) of
        true ->
            io_lib:format("\"~ts\"", [Data]);
        false ->
            io_lib:format("~0tp", [Data])
    end;
stringify(Data) when is_atom(Data) ->
    io_lib:format("'~ts'", [Data]);
stringify(Data) ->
    io_lib:format("~0tp", [Data]).

%% @doc set shell prompt
-spec set_prompt() -> ok.
set_prompt() ->
    shell:catch_exception(true),
    shell:prompt_func({?MODULE, prompt_func}),
    ok.

%% @doc shell prompt_func
-spec prompt_func([{history, non_neg_integer()}]) -> string().
prompt_func([{history, N}]) ->
    io:format("[~s](~s)~n", [color:blue(atom_to_list(node())), color:cyan(integer_to_list(N))]),
    io_lib:format("~s ", [color:green(">>")]).

%%%-------------------------------------------------------------------
%%% @doc
%%% make config file to get config code
%%% @end
%%%-------------------------------------------------------------------
-module(config_maker).
-export([start/1]).
%%%===================================================================
%%% API functions
%%%===================================================================
%% @doc for shell
start(List) ->
    maker:start(fun parse_file/1, List).

%%%===================================================================
%%% Internal functions
%%%===================================================================
parse_file(#{config := Config}) ->
    {Flag, Result} = file:consult(maker:relative_path(Config)),
    Flag =/= ok andalso erlang:throw(lists:flatten(io_lib:format("Could Not Consult Config File: ~tp", [Result]))),
    NameList = filelib:wildcard(maker:relative_path("config/app/*.app")),
    Form = lists:flatten([loop(filename:basename(Name, ".app"), "", element(2, proplists:get_value(list_to_atom(filename:basename(Name, ".app")), hd(Result), {filename:basename(Name, ".app"), []})), []) || Name <- NameList]),
    Export = [Export || {Export, _} <- Form],
    Function = [Function || {_, Function} <- Form],
    Code = lists:concat(["-module(config).\n", Export, "\n\n", Function]),
    [#{pattern => "(?s).*", code => Code}].

%% k/v type
%% generate loop
loop(_, _, [], List) ->
    lists:reverse(List);
loop(Env, Name, [{Key, Value} | T], List) ->
    RecursiveList = make_function(Env, false, [Key], Value, []),
    loop(Env, Name, T, [RecursiveList | List]).

%% extract keys/value to construct one function
make_function(Env, _, KeyList, [], []) ->
    make_function_loop(Env, KeyList, KeyList, [], []);
make_function(_, _, _, [], List) ->
    lists:reverse(List);
make_function(Env, false, KeyList, RootValue = [{Key, Value} | T], List) ->
    %% recursive key/value
    %% no cross, make root function
    Root = make_function_loop(Env, KeyList, KeyList, [], RootValue),
    RecursiveList = make_function(Env, false, [Key | KeyList], Value, []),
    make_function(Env, true, KeyList, T, [RecursiveList, Root | List]);
make_function(Env, true, KeyList, [{Key, Value} | T], List) ->
    %% cross recursive key/value
    RecursiveList = make_function(Env, false, [Key | KeyList], Value, []),
    make_function(Env, true, KeyList, T, [RecursiveList | List]);
make_function(Env, _, KeyList = [_ | _], Value, _) ->
    make_function_loop(Env, KeyList, KeyList, [], Value).

%% one function construct recursive
make_function_loop(Env, KeyList, [Key], [], Value) ->
    FunctionName = string:join([atom_to_list(X) || X <- lists:reverse(KeyList)], "_"),
    %% base padding is 3 * (4 space)
    Child = lists:concat(lists:duplicate(3, "    ")) ++ word:to_hump(Key),
    format_function(Env, FunctionName, word:to_hump(Key), Key, Child, Value);
make_function_loop(Env, KeyList, [Key], Child, Value) ->
    FunctionName = string:join([atom_to_list(X) || X <- lists:reverse(KeyList)], "_"),
    format_function(Env, FunctionName, word:to_hump(Key), Key, Child, Value);
make_function_loop(Env, KeyList, [Key, Parent | T], Child, Value) ->
    %% base padding is 3 * (4 space)
    New = format_clause(length(T) * 2 + 3, Key, Parent, Child),
    make_function_loop(Env, KeyList, [Parent | T], New, Value).

%% format function
format_function(Env, FunctionName, Name, Key, Child, Value) ->
    BasePadding = "    ",
    MatchPadding = BasePadding ++ "    ",
    ValuePadding = MatchPadding ++ "    ",
    %% value type spec
    ValueSpec = value_spec(Value),
    WithDefaultSpec = io_lib:format("-spec ~ts() -> ~s.\n", [FunctionName, ValueSpec]),
    WithDefaultFunction = io_lib:format("~ts() ->\n~ts~ts(~0tp).\n\n", [FunctionName, BasePadding, FunctionName, Value]),
    WithoutDefaultSpec = io_lib:format("-spec ~ts(Default :: ~s) -> ~s.\n", [FunctionName, ValueSpec, ValueSpec]),
    WithoutDefaultFunction = io_lib:format("~ts(Default) ->\n~tscase application:get_env(~ts, ~ts) of\n~ts{ok, ~ts} ->\n~ts;\n~ts_ ->\n~tsDefault\n~tsend.\n\n", [FunctionName, BasePadding, Env, Key, MatchPadding, Name, Child, MatchPadding, ValuePadding, BasePadding]),
    {lists:flatten(["-export([", FunctionName, "/0", ", ", FunctionName, "/1", "]).\n"]), lists:flatten(io_lib:format("~ts~ts~ts~ts", [WithDefaultSpec, WithDefaultFunction, WithoutDefaultSpec, WithoutDefaultFunction]))}.

%% format clause
format_clause(Depth, Key, Parent, []) ->
    BasePadding = lists:concat(lists:duplicate(Depth, "    ")),
    MatchPadding = BasePadding ++ "    ",
    ValuePadding = MatchPadding ++ "    ",
    ParentName = word:to_hump(Parent),
    Name = word:to_hump(Key),
    lists:flatten(io_lib:format("~tscase lists:keyfind(~ts, 1, ~ts) of\n~ts{~ts, ~ts} ->\n~ts~ts;\n~ts_ ->\n~tsDefault\n~tsend", [BasePadding, Key, ParentName, MatchPadding, Key, Name, ValuePadding, Name, MatchPadding, ValuePadding, BasePadding]));
format_clause(Depth, Key, Parent, Child) ->
    BasePadding = lists:concat(lists:duplicate(Depth, "    ")),
    MatchPadding = BasePadding ++ "    ",
    ValuePadding = MatchPadding ++ "    ",
    ParentName = word:to_hump(Parent),
    Name = word:to_hump(Key),
    lists:flatten(io_lib:format("~tscase lists:keyfind(~ts, 1, ~ts) of\n~ts{~ts, ~ts} ->\n~ts;\n~ts_ ->\n~tsDefault\n~tsend", [BasePadding, Key, ParentName, MatchPadding, Key, Name, Child, MatchPadding, ValuePadding, BasePadding])).

%% the value spec
value_spec(Value) when is_list(Value) ->
    case io_lib:printable_unicode_list(Value) of
        true ->
            "string()";
        false ->
            "proplists:proplist()"
    end;
value_spec(Value) ->
    type:spec(Value).

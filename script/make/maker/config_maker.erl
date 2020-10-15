%%%-------------------------------------------------------------------
%%% @doc
%%% make config file to get config code
%%% @end
%%%-------------------------------------------------------------------
-module(config_maker).
-export([start/2]).
%%%===================================================================
%%% API functions
%%%===================================================================
%% @doc for shell
start(InFile, OutFile) ->
    case file:consult(maker:relative_path(InFile)) of
        {ok, [Terms]} ->
            %% without sasl and kernel config
            Result = [loop(filename:basename(Name, ".app"), "", element(2, listing:key_find(list_to_atom(filename:basename(Name, ".app")), 1, Terms, {filename:basename(Name, ".app"), []})), []) || Name <- filelib:wildcard(maker:relative_path("config/app/*.app"))],
            Head = "-module(config).\n-compile(nowarn_export_all).\n-compile(export_all).\n\n",
            file:write_file(maker:relative_path(OutFile), Head ++ lists:flatten(Result));
        Error ->
            Error
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================
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
    make_function(Env, true, KeyList, T, [[Root | RecursiveList] | List]);
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
    New = format_clause(length(T) * 2 + 3, Key, Parent, Child, Value),
    make_function_loop(Env, KeyList, [Parent | T], New, Value).

%% format function
format_function(Env, FunctionName, Name, Key, Child, Value) ->
    BasePadding = "    ",
    MatchPadding = BasePadding ++ "    ",
    ValuePadding = MatchPadding ++ "    ",
    WithDefaultFunction = io_lib:format("~s() ->~n~s~s(~0p).~n~n", [FunctionName, BasePadding, FunctionName, Value]),
    WithoutDefaultFunction = io_lib:format("~s(Default) ->~n~scase application:get_env(~s, ~s) of~n~s{ok, ~s} ->~n~s;~n~s_ ->~n~sDefault~n~send.~n~n", [FunctionName, BasePadding, Env, Key, MatchPadding, Name, Child, MatchPadding, ValuePadding, BasePadding]),
    lists:flatten(io_lib:format("~s~s", [WithDefaultFunction, WithoutDefaultFunction])).

%% format clause
format_clause(Depth, Key, Parent, [], Value) ->
    BasePadding = lists:concat(lists:duplicate(Depth, "    ")),
    MatchPadding = BasePadding ++ "    ",
    ValuePadding = MatchPadding ++ "    ",
    ParentName = word:to_hump(Parent),
    Name = word:to_hump(Key),
    lists:flatten(io_lib:format("~scase lists:keyfind(~s, 1, ~s) of~n~s{~s, ~s} ->~n~s~s;~n~s_ ->~n~s~p~n~send", [BasePadding, Key, ParentName, MatchPadding, Key, Name, ValuePadding, Name, MatchPadding, ValuePadding, Value, BasePadding]));
format_clause(Depth, Key, Parent, Child, Value) ->
    BasePadding = lists:concat(lists:duplicate(Depth, "    ")),
    MatchPadding = BasePadding ++ "    ",
    ValuePadding = MatchPadding ++ "    ",
    ParentName = word:to_hump(Parent),
    Name = word:to_hump(Key),
    lists:flatten(io_lib:format("~scase lists:keyfind(~s, 1, ~s) of~n~s{~s, ~s} ->~n~s;~n~s_ ->~n~s~p~n~send", [BasePadding, Key, ParentName, MatchPadding, Key, Name, Child, MatchPadding, ValuePadding, Value, BasePadding])).

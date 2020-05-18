%%%-------------------------------------------------------------------
%%% @doc
%%% module config maker
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
            Name = filename:basename(InFile, ".config"),
            List = element(2, listing:key_find(list_to_atom(Name), 1, Terms, {Name, []})),
            Result = loop(Name, "", List, []),
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
    RecursiveList = make_function(Env, [Key], Value, []),
    loop(Env, Name, T, [RecursiveList | List]).

%% extract keys/value to construct one function
make_function(_, _, [], List) ->
    lists:reverse(List);
make_function(Env, KeyList, [{Key, Value} | T], List) ->
    RecursiveList = make_function(Env, [Key | KeyList], Value, []),
    make_function(Env, KeyList, T, [RecursiveList | List]);
make_function(Env, KeyList = [_ | _], Value, _) ->
    make_function_loop(Env, KeyList, KeyList, [], Value).

%% one function construct recursive
make_function_loop(Env, KeyList, [Key], [], Value) ->
    FunctionName = string:join([atom_to_list(X) || X <- lists:reverse(KeyList)], "_"),
    %% base padding is 3 * (4 space)
    Child = lists:concat(lists:duplicate(3, "    ")) ++ word:to_hump(Key),
    format_root(Env, FunctionName, word:to_hump(Key), Key, Child, Value);
make_function_loop(Env, KeyList, [Key], Child, Value) ->
    FunctionName = string:join([atom_to_list(X) || X <- lists:reverse(KeyList)], "_"),
    format_root(Env, FunctionName, word:to_hump(Key), Key, Child, Value);
make_function_loop(Env, KeyList, [Key, Parent | T], Child, Value) ->
    %% base padding is 3 * (4 space)
    New = format_child(length(T) * 2 + 3, Key, Parent, Child, Value),
    make_function_loop(Env, KeyList, [Parent | T], New, Value).

%% format root
format_root(Env, FunctionName, Name, Key, Child, Value) ->
    BasePadding = "    ",
    MatchPadding = BasePadding ++ "    ",
    ValuePadding = MatchPadding ++ "    ",
    lists:flatten(io_lib:format("~s() ->~n~scase application:get_env(~s, ~s) of~n~s{ok, ~s} ->~n~s;~n~s_ ->~n~s~p~n~send.~n~n", [FunctionName, BasePadding, Env, Key, MatchPadding, Name, Child, MatchPadding, ValuePadding, Value, BasePadding])).

%% format child
format_child(Depth, Key, Parent, [], Value) ->
    BasePadding = lists:concat(lists:duplicate(Depth, "    ")),
    MatchPadding = BasePadding ++ "    ",
    ValuePadding = MatchPadding ++ "    ",
    ParentName = word:to_hump(Parent),
    Name = word:to_hump(Key),
    lists:flatten(io_lib:format("~scase lists:keyfind(~s, 1, ~s) of~n~s{~s, ~s} ->~n~s~s;~n~s_ ->~n~s~p~n~send", [BasePadding, Key, ParentName, MatchPadding, Key, Name, ValuePadding, Name, MatchPadding, ValuePadding, Value, BasePadding]));
format_child(Depth, Key, Parent, Child, Value) ->
    BasePadding = lists:concat(lists:duplicate(Depth, "    ")),
    MatchPadding = BasePadding ++ "    ",
    ValuePadding = MatchPadding ++ "    ",
    ParentName = word:to_hump(Parent),
    Name = word:to_hump(Key),
    lists:flatten(io_lib:format("~scase lists:keyfind(~s, 1, ~s) of~n~s{~s, ~s} ->~n~s;~n~s_ ->~n~s~p~n~send", [BasePadding, Key, ParentName, MatchPadding, Key, Name, Child, MatchPadding, ValuePadding, Value, BasePadding])).

%%%-------------------------------------------------------------------
%%% @doc
%%% unicode/string extended library
%%% @end
%%%-------------------------------------------------------------------
-module(word).
-compile({no_auto_import, [length/1]}).
%% API
-export([validate/1, validate/2, byte/1, length/1, sensitive/1]).
-export([to_hump/1, to_lower_hump/1]).
-export([to_snake/1]).
-export([to_hex/1, to_char/1]).
%%%===================================================================
%%% API functions
%%%===================================================================
%% @doc string check
-spec validate(String :: binary()) -> true | {false, Reason :: term()} | {false, atom(), Reason :: term()}.
validate(String) ->
    validate_loop([{length, 1, 6}, sensitive], String).

-spec validate(String :: binary(), ConditionList :: [{length, non_neg_integer(), non_neg_integer()} | sensitive | {sql, binary()}]) -> true | {false, Reason :: term()} | {false, atom(), Reason :: term()}.
validate(String, ConditionList) ->
    validate_loop(ConditionList, String).

validate_loop([], _String) ->
    true;
validate_loop([{length, Min, Max} | T], String) ->
    case length(String) of
        {ok, Length} when Min =< Length andalso Length =< Max ->
            validate_loop(T, String);
        {ok, Length} ->
            {false, length, Length};
        {error, _} ->
            {false, asn1, bad_utf8_character_encoding}
    end;
validate_loop([sensitive | T], String) ->
    case sensitive(String) of
        false ->
            validate_loop(T, String);
        true ->
            {false, sensitive}
    end;
validate_loop([{sql, Sql} | T], String) ->
    case db:select(Sql) of
        [] ->
            validate_loop(T, String);
        Data ->
            {false, duplicate, Data}
    end.

%% @doc word byte size
-spec byte(String :: binary()) -> {ok, Length :: non_neg_integer()} | {error, Reason :: term()}.
byte(String) ->
    case unicode:characters_to_list(String) of
        List when is_list(List) ->
            {ok, erlang:length(List)};
        _ ->
            {error, not_utf8_encoding}
    end.

%% @doc length
-spec length(String :: binary()) -> {ok, non_neg_integer()} | {error, term()}.
length(String) ->
    case unicode:characters_to_list(String) of
        {error, _, _} ->
            {error, asn1};
        {incomplete, _, _} ->
            {error, incomplete};
        List ->
            {ok, erlang:length(List)}
    end.

%% @doc sensitive word
-spec sensitive(String :: binary()) -> boolean().
sensitive(String) ->
    sensitive_word_data:word(String).

%% @doc hump name
%% hump_name -> HumpName
-spec to_hump(atom() | binary() | string()) -> string().
to_hump(Atom) when is_atom(Atom) ->
    to_hump(atom_to_list(Atom));
to_hump(Binary) when is_binary(Binary) ->
    to_hump(binary_to_list(Binary));
to_hump([]) ->
    [];
to_hump(Name) when is_list(Name) ->
    case lists:any(fun(Item) -> is_list(Item) orelse is_atom(Item) orelse is_binary(Item) end, Name) of
        true ->
            lists:flatten([to_hump(N) || N <- Name]);
        false ->
            to_hump_loop(Name, [], [], [])
    end.

to_hump_loop([], [], [], List) ->
    lists:flatten(lists:reverse(List));

%% end, tail push underline
to_hump_loop([], UnderLine, [], List) ->
    lists:flatten(lists:reverse([UnderLine | List]));

%% end, push word
to_hump_loop([], [], Word, List) ->
    [First | Rest] = lists:reverse(Word),
    lists:flatten(lists:reverse([[string:to_upper(First) | Rest] | List]));

%% head
to_hump_loop([H = $_ | T], UnderLine, [], List) ->
    to_hump_loop(T, [H | UnderLine], [], List);

%% gap or tail
to_hump_loop([H = $_ | T], UnderLine, Word, List) ->
    [First | Rest] = lists:reverse(Word),
    to_hump_loop(T, [H | UnderLine], [], [[string:to_upper(First) | Rest] | List]);

%% word, push word
to_hump_loop([H | T], UnderLine = [], Word, List) ->
    to_hump_loop(T, UnderLine, [H | Word], List);

%% word, head, push underline
to_hump_loop([H | T], UnderLine, Word, []) ->
    to_hump_loop(T, [], [H | Word], [UnderLine]);

to_hump_loop([H | T], _, Word, List) ->
    to_hump_loop(T, [], [H | Word], List).

%% @doc lower_hump
%% lower_hump/LowerHump -> lowerHump
-spec to_lower_hump(atom() | binary() | string()) -> string().
to_lower_hump(Name) ->
    case to_hump(Name) of
        [] ->
            [];
        [Head | Tail] ->
            [string:to_lower(Head) | Tail]
    end.

%% @doc snake name
%% HumpName -> hump_name
-spec to_snake(atom() | binary() | string()) -> string().
to_snake(Atom) when is_atom(Atom) ->
    to_snake(atom_to_list(Atom));
to_snake(Binary) when is_binary(Binary) ->
    to_snake(binary_to_list(Binary));
to_snake([]) ->
    [];
to_snake(Name) when is_list(Name) ->
    to_snake_loop(Name, [], false).

to_snake_loop([], S, _) ->
    lists:reverse(S);
to_snake_loop([H | T], S, IsUpper) ->
    case string:to_lower(H) of
        H ->
            to_snake_loop(T, [H | S], false);
        L when S == [] ->
            to_snake_loop(T, [L], true);
        L when IsUpper == true ->
            to_snake_loop(T, [L | S], true);
        L ->
            to_snake_loop(T, [L, $_ | S], true)
    end.

%% @doc to hex
-spec to_hex(list()) -> [string()].
to_hex(Term) when is_atom(Term) ->
    [io_lib:format("~4.16.0B", [Code]) || Code <- atom_to_list(Term)];
to_hex(Term) when is_binary(Term) ->
    [io_lib:format("~4.16.0B", [Code]) || Code <- unicode:characters_to_list(Term)];
to_hex(Term) when is_list(Term) ->
    [io_lib:format("~4.16.0B", [Code]) || Code <- Term].

%% @doc to unicode char
-spec to_char([string()]) -> [non_neg_integer()].
to_char(List) ->
    [erlang:list_to_integer(Code, 16) || Code <- List].

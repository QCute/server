%%%-------------------------------------------------------------------
%%% @doc
%%% module activation key maker
%%% @end
%%%-------------------------------------------------------------------
-module(key_maker).
-export([start/1]).
-export([parse/2]).
%%%===================================================================
%%% API
%%%===================================================================
%% @doc for shell
start(List) ->
    maker:start(fun parse_table/2, [List]).

%% @doc parse
parse(DataBase, One) ->
    parse_table(DataBase, One).
%% ====================================================================
%% Internal functions
%% ====================================================================
%% @doc
parse_table(DataBase, {_, Table, Amount, Type, Prefix}) ->
    CorrectDict = load_existing(DataBase, Table),
    List = loop(Prefix, type:to_integer(Amount), CorrectDict),
    IntegerType = type:to_integer(Type),
    Sql = lists:concat(["INSERT INTO ", DataBase, ".", Table, " (`key`, `type`) VALUES ", string:join([io_lib:format("('~s', '~p')", [Key, IntegerType]) || Key <- List], ", ")]),
    maker:insert(Sql),
    ok.

loop(Prefix, Amount, CorrectDict) ->
    loop(Prefix, dict:new(), CorrectDict, Amount).
loop(Prefix, Dict, CorrectDict, Amount) ->
    Key = generate(Prefix),
    case dict:find(Key, CorrectDict) of
        'error' ->
            New = dict:store(Key, 0, Dict),
            case dict:size(New) >= Amount of
                true ->
                    List = dict:to_list(New),
                    [K || {K, _} <- List];
                _ ->
                    loop(Prefix, New, CorrectDict, Amount)
            end;
        _ ->
            loop(Prefix, Dict, CorrectDict, Amount)
    end.

%% load existing data for correct use
load_existing(_DataBase, Table) ->
    Sql = io_lib:format("SELECT `key`, `type` FROM ~s", [Table]),
    Data = maker:select(Sql),

    dict:from_list([{K, 0} || [K | _] <- Data]).

%% generate random key with prefix
generate([]) ->
    generate([], 12);
generate(Prefix) ->
    generate(Prefix, 8).
generate(Prefix, Bit) ->
    %% rand bytes
    Bytes = crypto:rand_bytes(Bit),
    %% base64 encode, 4 byte end
    Encode = binary_to_list(base64:encode(Bytes)),
    %% concat prefix
    Full = lists:append(Prefix, Encode),
    %% revise encode
    Corrected = revise(Full),
    %% string to lower and convert to bit string
    list_to_binary(string:to_lower(Corrected)).

%% revise base64 charset +-/= to letter
revise(List) ->
    revise(List, []).
revise([], List) ->
    lists:reverse(List);
revise([$+ | T], List) ->
    revise(T, [rand($a, $z) | List]);
revise([$- | T], List) ->
    revise(T, [rand($a, $z) | List]);
revise([$/ | T], List) ->
    revise(T, [rand($a, $z) | List]);
revise([$= | T], List) ->
    revise(T, [rand($a, $z) | List]);
revise([H | T], List) ->
    revise(T, [H | List]).

%% re rand letter
rand(Same, Same) -> Same;
rand(Min, Max) when Max < Min -> 0;
rand(Min, Max) ->
    random:seed(os:timestamp()),
    M = Min - 1,
    random:uniform(Max - M) + M.
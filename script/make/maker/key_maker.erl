%%%-------------------------------------------------------------------
%%% @doc
%%% module key maker
%%% game award activation key
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
parse_table(DataBase, {_, Table, Amount, Type, Prefix, Length}) ->
    CorrectDict = load_existing(DataBase, Table),
    List = loop(Prefix, Length, type:to_integer(Amount), CorrectDict),
    IntegerType = type:to_integer(Type),
    Sql = lists:concat(["INSERT INTO ", DataBase, ".", Table, " (`key`, `type`) VALUES ", string:join([io_lib:format("('~s', '~p')", [Key, IntegerType]) || Key <- List], ", ")]),
    maker:insert(Sql),
    ok.

loop(Prefix, Length, Amount, CorrectDict) ->
    loop(Prefix, Length, dict:new(), CorrectDict, Amount).
loop(Prefix, Length, Dict, CorrectDict, Amount) ->
    Key = generate(Prefix, Length),
    case dict:find(Key, CorrectDict) of
        'error' ->
            New = dict:store(Key, 0, Dict),
            case dict:size(New) >= Amount of
                true ->
                    List = dict:to_list(New),
                    [K || {K, _} <- List];
                _ ->
                    loop(Prefix, Length, New, CorrectDict, Amount)
            end;
        _ ->
            loop(Prefix, Length, Dict, CorrectDict, Amount)
    end.

%% load existing data for correct use
load_existing(_DataBase, Table) ->
    Sql = io_lib:format("SELECT `key`, `type` FROM ~s", [Table]),
    Data = maker:select(Sql),
    dict:from_list([{K, 0} || [K | _] <- Data]).

%% generate random key with prefix
generate(Prefix, Length) ->
    %% rand bytes
    Bytes = crypto:strong_rand_bytes(Length),
    %% base64 encode, 4 byte end
    Encode = binary_to_list(base64:encode(Bytes)),
    %% concat prefix
    Full = lists:append(Prefix, Encode),
    %% revise encode
    [H | Corrected] = revise(Full),
    %% revise head
    Head = revise_head(H),
    %% string to lower and convert to bit string
    list_to_binary(string:to_lower([Head | Corrected])).

%% revise encode head
revise_head(C) when $0 =< C andalso C =< $9 ->
    rand();
revise_head(C) ->
    C.

%% revise base64 charset +-/= to letter
revise(List) ->
    revise(List, []).
revise([], List) ->
    lists:reverse(List);
revise([$+ | T], List) ->
    revise(T, [rand() | List]);
revise([$- | T], List) ->
    revise(T, [rand() | List]);
revise([$/ | T], List) ->
    revise(T, [rand() | List]);
revise([$= | T], List) ->
    revise(T, [rand() | List]);
revise([H | T], List) ->
    revise(T, [H | List]).

%% re rand letter
rand() ->
    <<Random:8>> = crypto:strong_rand_bytes(1),
    $a + trunc(Random / 256 * 26).

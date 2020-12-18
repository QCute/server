%%%-------------------------------------------------------------------
%%% @doc
%%% generate game award activation key
%%% @end
%%%-------------------------------------------------------------------
-module(key_maker).
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
%% @doc
parse_table({_, Table, Number, Type, Prefix, Length}) ->
    CorrectDict = load_existing(Table),
    List = loop(Prefix, Length, Number, CorrectDict),
    Sql = lists:concat(["INSERT INTO ", Table, " (`key`, `type`) VALUES ", string:join([io_lib:format("('~s', '~w')", [Key, Type]) || Key <- List], ", ")]),
    db:insert(Sql),
    ok.

loop(Prefix, Length, Number, CorrectDict) ->
    loop(Prefix, Length, dict:new(), CorrectDict, Number).
loop(Prefix, Length, Dict, CorrectDict, Number) ->
    Key = generate(Prefix, Length),
    case dict:find(Key, CorrectDict) of
        error ->
            New = dict:store(Key, 0, Dict),
            case dict:size(New) >= Number of
                true ->
                    List = dict:to_list(New),
                    [K || {K, _} <- List];
                _ ->
                    loop(Prefix, Length, New, CorrectDict, Number)
            end;
        _ ->
            loop(Prefix, Length, Dict, CorrectDict, Number)
    end.

%% load existing data for correct use
load_existing(Table) ->
    Sql = io_lib:format("SELECT `key`, `type` FROM ~s", [Table]),
    Data = db:select(Sql),
    dict:from_list([{K, 0} || [K | _] <- Data]).

%% generate random key with prefix
generate(Prefix, Length) ->
    %% rand bytes
    Bytes = crypto:strong_rand_bytes(Length),
    %% base64 encode, 4 bytes ending
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
    $a + erlang:round(Random / 256 * 26).

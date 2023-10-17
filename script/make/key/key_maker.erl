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
start(#{table := Table, number := Number, type := Type, prefix := Prefix, length := Length}) ->
    maker:connect_database(),
    CorrectDict = load_existing(Table),
    List = loop(Type, Prefix, Length, Number, CorrectDict),
    Sql = parser:collect(List, {<<"INSERT INTO `", (type:to_binary(Table))/binary, "` (`key`, `type`) VALUES ">>, <<"('~s', ~w)">>}),
    db:insert(Sql),
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================
loop(Type, Prefix, Length, Number, CorrectDict) ->
    loop(Type, Prefix, Length, dict:new(), CorrectDict, Number).
loop(Type, Prefix, Length, Dict, CorrectDict, Number) ->
    Key = generate(Prefix, Length),
    case dict:is_key(Key, CorrectDict) of
        false ->
            New = dict:store(Key, Type, Dict),
            case dict:size(New) >= Number of
                true ->
                    dict:to_list(New);
                _ ->
                    loop(Type, Prefix, Length, New, CorrectDict, Number)
            end;
        _ ->
            loop(Type, Prefix, Length, Dict, CorrectDict, Number)
    end.

%% load existing data for correct use
load_existing(Table) ->
    Data = db:select(<<"SELECT `key`, `type` FROM `~s`">>, [Table]),
    dict:from_list(lists:map(fun erlang:list_to_tuple/1, Data)).

%% generate random key with prefix
generate(Prefix, Length) ->
    %% rand bytes
    Bytes = crypto:strong_rand_bytes(Length),
    %% base64 encode, 4 bytes ending
    Encode = base64:encode(Bytes),
    %% revise encode
    Corrected = revise(Encode),
    %% to lower
    <<<<(string:to_lower(C)):8>> || <<C:8>> <= <<(iolist_to_binary(Prefix))/binary, Corrected/binary>>>>.

%% revise base64 charset +-/= to letter
revise(Binary) ->
    revise(Binary, <<>>).
revise(<<>>, Binary) ->
    Binary;
revise(<<$+, T/binary>>, Binary) ->
    revise(T, <<Binary/binary, (rand()):8>>);
revise(<<$-, T/binary>>, Binary) ->
    revise(T, <<Binary/binary, (rand()):8>>);
revise(<<$/, T/binary>>, Binary) ->
    revise(T, <<Binary/binary, (rand()):8>>);
revise(<<$=, T/binary>>, Binary) ->
    revise(T, <<Binary/binary, (rand()):8>>);
revise(<<H:8, T/binary>>, Binary) ->
    revise(T, <<Binary/binary, H:8>>).

%% re rand letter
rand() ->
    <<Random:8>> = crypto:strong_rand_bytes(1),
    $a + erlang:round(Random / 256 * 25).

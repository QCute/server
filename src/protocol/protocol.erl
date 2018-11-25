%%%-------------------------------------------------------------------
%%% @doc
%%% module protocol(binary read/write) tool
%%%
%%% Read bit field list recommend
%%% has binary list : Binary = <<1:8, 2:16, 3:32, 4:64, 5:128, O/binary>>,
%%% use expression  : [{A, B, C, D, E} || <<A:8, B:16, C:32, D:64, E:128>> <= Binary].
%%% use pattern split binary list <<Length:16, BinaryList:Length/binary-unit:248, Other/binary>>
%%%
%%% Write bit field list recommend
%%% has tuple list  : List = [{A, B, C, D, E} | _],
%%% use expression  : << <<A:8, B:16, C:32, D:64, E:128>> || {A, B, C, D, E} <- List >>.
%%% write binary with string
%%% use expression  : list_to_binary([begin S = write_string(C), <<A, B, S, D, E>> end || {A, B, C, D, E} <- List])
%%% @end
%%%-------------------------------------------------------------------
-module(protocol).
%% API
-export([read_string/1, read_string/2, write_string/1, pack/2]).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc read
-spec read_string(Binary::byte()) -> {[string()], Binary::byte()}.
read_string(Binary) ->
    read_string(1, [], Binary).
read_string(Amount, Binary) ->
    read_string(Amount, [], Binary).
read_string(0, Data, Binary) ->
    {lists:reverse(Data), Binary};
read_string(Amount, Data, <<Length:16, String:Length/binary-unit:8, Binary/binary>>) ->
    read_string(Amount - 1, [binary_to_list(String) | Data], Binary);
read_string(_, Data, Binary) ->
    {lists:reverse(Data), Binary}.


%% @doc write
-spec write_string(String::list()) -> binary().
write_string(String) ->
    Binary = list_to_binary(String),
    Length = byte_size(Binary),
    <<Length:16, Binary/binary>>.

%% @doc 打包信息，添加消息头
pack(Protocol, Data)->
    Length = byte_size(Data) + 2,
    <<Length:16, Protocol:16, Data/binary>>.
%%%===================================================================
%%% Internal functions
%%%===================================================================


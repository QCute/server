%%%-------------------------------------------------------------------
%%% @doc
%%% make tile map to walk/block point
%%% @end
%%%-------------------------------------------------------------------
-module(map_maker).
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
parse_file(#{file := File, path := Path}) ->
    {Flag, Result} = file:list_dir(maker:relative_path(Path)),
    Flag =/= ok andalso erlang:throw(lists:flatten(io_lib:format("Could Not List Path: ~tp", [Result]))),
    Head = lists:concat(["-module(", filename:basename(File, ".erl"), ").\n-export([get/1]).\n\n"]),
    Code = load_loop(Result, Path, []),
    [#{pattern => "(?s).*", code => lists:concat([Head, Code])}].

load_loop([], _, Code) ->
    %% add wildcard option
    lists:reverse(["get(_) ->\n    [].\n" | Code]);
load_loop([FileName | T], Path, Code) ->
    {ok, RawBinary} = file:read_file(maker:relative_path(Path ++ FileName)),
    <<Id:32, _Width:32, _Height:32, RowLength:16, ColumnLength:16, Rest/binary>> = RawBinary,
    TileLength = RowLength * ColumnLength * 3,
    <<TileBinary:TileLength/binary, _Rest/binary>> = Rest,
    String = load_tile_by_y(Id, TileBinary, 0, 0, RowLength, ColumnLength - 1, Code),
    load_loop(T, Path, String).

load_tile_by_y(_Id, _Rest, _X, Y, _MaxX, _MaxY = Y, Code) ->
    Code;
load_tile_by_y(Id, Binary, X, Y, MaxX, MaxY, Code) ->
    {Rest, NewCode} = load_tile_by_x(Id, Binary, X, Y, MaxX, MaxY, Code),
    load_tile_by_y(Id, Rest, 0, Y + 1, MaxX, MaxY, NewCode).

load_tile_by_x(_Id, Binary, X, _Y, _MaxX = X, _MaxY, Code) ->
    {Binary, Code};
load_tile_by_x(Id, <<0:8, _Vt:8, _Va:8, Rest/binary>>, X, Y, MaxX, MaxY, Code) ->
    load_tile_by_x(Id, Rest, X + 1, Y, MaxX, MaxY, Code);
load_tile_by_x(Id, <<_Type:8, _Vt:8, _Va:8, Rest/binary>>, X, Y, MaxX, MaxY, Code) ->
    %% code format
    String = lists:flatten(io_lib:format("get({~w, ~w, ~w}) ->\n    {~w, ~w, ~w};\n", [Id, X, Y, Id, X, Y])),
    load_tile_by_x(Id, Rest, X + 1, Y, MaxX, MaxY, [String | Code]).

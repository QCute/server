%%%-------------------------------------------------------------------
%%% @doc
%%% module map maker
%%% @end
%%%-------------------------------------------------------------------
-module(map_maker).
%% API
-export([start/3]).
%%====================================================================
%% API
%%====================================================================
start(Directory, ExtName, FileName) ->
    case file:list_dir(Directory) of
        {ok, List} ->
            Code = load_loop(List, Directory, ExtName, []),
            Head = lists:concat(["-module(", filename:basename(FileName, ".erl"), ").\n-compile(nowarn_export_all).\n-compile(export_all).\n\n"]),
            file:write_file(FileName, Head ++ Code);
        {error, Reason} ->
            {error, Reason}
    end.

%% ====================================================================
%% Internal functions
%% ====================================================================
load_loop([], _, _, Code) ->
    %% add wildcard option
    lists:reverse(["get(_) ->\n    [].\n" | Code]);
load_loop([FileName | T], Path, ExtName, Code) ->
    {ok, RawBinary} = file:read_file(Path ++ FileName),
    <<_TempMapID:32, _Width:32, _Height:32, RowLength:16, ColumnLength:16, Rest/binary>> = RawBinary,
    %% file name as map id
    Id = list_to_integer(filename:basename(FileName, ExtName)),
    TileLength = RowLength * ColumnLength * 3,
    <<TileBinary:TileLength/binary, _Rest/binary>> = Rest,
    String = load_tile_by_y(Id, TileBinary, 0, 0, RowLength, ColumnLength - 1, Code),
    load_loop(T, Path, ExtName, String).

load_tile_by_y(_Id, _Rest, _X, _Y, _MaxX, _MaxY = _Y, Code) ->
    Code;
load_tile_by_y(Id, Binary, X, Y, MaxX, MaxY, Code) ->
    {Rest, NewCode} = load_tile_by_x(Id, Binary, X, Y, MaxX, MaxY, Code),
    load_tile_by_y(Id, Rest, 0, Y + 1, MaxX, MaxY, NewCode).

load_tile_by_x(_Id, Binary, _X, _Y, _MaxX = _X, _MaxY, Code) ->
    {Binary, Code};
load_tile_by_x(Id, <<0:8, _Vt:8, _Va:8, Rest/binary>>, X, Y, MaxX, MaxY, Code) ->
    load_tile_by_x(Id, Rest, X + 1, Y, MaxX, MaxY, Code);
load_tile_by_x(Id, <<_Type:8, _Vt:8, _Va:8, Rest/binary>>, X, Y, MaxX, MaxY, Code) ->
    %% code format
    String = lists:flatten(io_lib:format("get(~p) ->\n    ~p;\n", [{Id, X, Y}, {Id, X, Y}])),
    load_tile_by_x(Id, Rest, X + 1, Y, MaxX, MaxY, [String | Code]).

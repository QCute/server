%%%-------------------------------------------------------------------
%%% @doc
%%% module excel script
%%% @end
%%%-------------------------------------------------------------------
-module(excel_script).
-export([main/1]).
%%%===================================================================
%%% API
%%%===================================================================
main(Args) ->
    code:add_path("beam"),
    code:add_path("../beam"),
    code:add_path("../../beam"),
    code:add_path("../../../beam"),
    {ok, DataBase} = console:stack_trace(maker:start_pool()),
    io:format("~p~n", [parse(DataBase, Args)]).

%% make xml sheet file
parse(DataBase, ["xml", Table | _]) ->
    excel_maker:to_xml(DataBase, Table);
parse(DataBase, ["table", "list" | List]) ->
	File = [list_to_integer(I) || I <- List],
    excel_maker:to_table(DataBase, File);
%% import xml sheet data to database
parse(DataBase, ["table", File | _]) ->
    excel_maker:to_table(DataBase, File);
%% argument error
parse(_, _) ->
        io:format("invail argument~n").
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
    code:add_path(filename:dirname(escript:script_name()) ++ "/../../../beam/"),
    {ok, DataBase} = maker:connect_database(),
    Result = console:stacktrace(catch parse(DataBase, Args)),
    io:format("~p~n", [Result]).

%% make xml sheet file
parse(DataBase, ["xml", Table | _]) ->
    excel_maker:to_xml(DataBase, Table);
parse(DataBase, ["table", "list" | List]) ->
    %% windows nt file name param list convert to integer list
    File = [list_to_integer(I) || I <- List],
    excel_maker:to_table(DataBase, File);
%% import xml sheet data to database
parse(DataBase, ["table", File | _]) ->
    excel_maker:to_table(DataBase, File);
%% argument error
parse(_, _) ->
        io:format("invail argument~n").
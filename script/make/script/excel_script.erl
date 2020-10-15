%%%-------------------------------------------------------------------
%%% @doc
%%% excel script for excel maker
%%% @end
%%%-------------------------------------------------------------------
-module(excel_script).
-export([main/1]).
%%%===================================================================
%%% API functions
%%%===================================================================
main(Args) ->
    code:add_path(filename:dirname(escript:script_name()) ++ "/../../../beam/"),
    io:format("~p~n", [catch parse(Args)]).

%% make xml sheet file from database table
parse(["xml", Table]) ->
    excel_maker:to_xml(Table);
parse(["xml", Table, Path | _]) ->
    excel_maker:to_xml(Table, Path);
%% import xml sheet data to database
parse(["table", File | _]) ->
    excel_maker:to_table(File);
%% argument error
parse(_) ->
    io:format("invalid arguments~n").

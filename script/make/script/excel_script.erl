%%%-------------------------------------------------------------------
%%! +pc unicode
%%% @doc
%%% excel script for excel maker
%%% @end
%%%-------------------------------------------------------------------
-module(excel_script).
-export([main/1]).
-include("../../../include/journal.hrl").
%%%===================================================================
%%% API functions
%%%===================================================================
main(Args) ->
    code:add_path(filename:dirname(escript:script_name()) ++ "/../../../beam/"),
    try
        io:format("~p~n", [parse(Args)])
    catch ?EXCEPTION(Class, Reason, Stacktrace) ->
        ?ERROR_STACKTRACE(Class, Reason, Stacktrace)
    end.

%% make xml sheet file from database table
parse(["xml", Table]) ->
    excel_maker:to_xml(Table);
parse(["xml", Table, Path | _]) ->
    excel_maker:to_xml(Table, Path);
%% import xml sheet data to database
parse(["table", File | _]) ->
    excel_maker:to_table(File);
%% argument error
parse(Args) ->
    io:format(standard_error, "invalid arguments: ~p~n", Args).

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
main(["xml", Table]) ->
    io:setopts([{encoding, unicode}]),
    code:add_path(filename:dirname(escript:script_name()) ++ "/../../../beam/"),
    try
        io:format("~tp~n", [excel_maker:to_xml(Table, "")])
    catch ?EXCEPTION(Class, Reason, Stacktrace) ->
        ?HALT(Class, Reason, Stacktrace)
    end;
main(["xml", Table, Path]) ->
    io:setopts([{encoding, unicode}]),
    code:add_path(filename:dirname(escript:script_name()) ++ "/../../../beam/"),
    try
        io:format("~tp~n", [excel_maker:to_xml(Table, Path)])
    catch ?EXCEPTION(Class, Reason, Stacktrace) ->
        ?HALT(Class, Reason, Stacktrace)
    end;
main(["table", File]) ->
    io:setopts([{encoding, unicode}]),
    code:add_path(filename:dirname(escript:script_name()) ++ "/../../../beam/"),
    try
        io:format("~tp~n", [excel_maker:to_table(File, filename:basename(File, ".xml"))])
    catch ?EXCEPTION(Class, Reason, Stacktrace) ->
        ?HALT(Class, Reason, Stacktrace)
    end;
main(["table", File, Name]) ->
    io:setopts([{encoding, unicode}]),
    code:add_path(filename:dirname(escript:script_name()) ++ "/../../../beam/"),
    try
        io:format("~tp~n", [excel_maker:to_table(File, Name)])
    catch ?EXCEPTION(Class, Reason, Stacktrace) ->
        ?HALT(Class, Reason, Stacktrace)
    end;
main(Args) ->
    io:format(standard_error, "invalid arguments: ~tp~n", [Args]).

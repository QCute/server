%%%-------------------------------------------------------------------
%%! +pc unicode
%%% @doc
%%% attribute script for attribute maker
%%% @end
%%%-------------------------------------------------------------------
-module(attribute_script).
-export([main/1]).
-include("../../../include/journal.hrl").
%%%===================================================================
%%% API functions
%%%===================================================================
main(_) ->
    code:add_path(filename:dirname(escript:script_name()) ++ "/../../../beam/"),
    try
        io:format("~p~n", [attribute_maker:start(attribute())])
    catch ?EXCEPTION(Class, Reason, Stacktrace) ->
        ?ERROR_STACKTRACE(Class, Reason, Stacktrace)
    end.

%%%===================================================================
%%% attribute data
%%%===================================================================
attribute() ->
    [
        {"src/module/attribute/attribute.erl", attribute_data, "attribute"},
        {"include/attribute.hrl", attribute_data, "attribute"}
    ].

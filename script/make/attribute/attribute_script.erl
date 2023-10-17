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
    io:setopts([{encoding, unicode}]),
    io:setopts(standard_error, [{encoding, unicode}]),
    code:add_path(filename:dirname(escript:script_name()) ++ "/../../../beam/"),
    try
        io:format("~tp~n", [attribute_maker:start(attribute())])
    catch ?EXCEPTION(Class, Reason, Stacktrace) ->
        ?HALT(Class, Reason, Stacktrace)
    end.

%%%===================================================================
%%% attribute data
%%%===================================================================
attribute() ->
    [
        #{
            file => "src/module/attribute/attribute.erl",
            table => attribute_data,
            name => "attribute"
        },
        #{
            file => "include/attribute.hrl",
            table => attribute_data,
            name => "attribute"
        }
    ].

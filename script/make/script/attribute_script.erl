%%%-------------------------------------------------------------------
%%% @doc
%%% attribute script for attribute maker
%%% @end
%%%-------------------------------------------------------------------
-module(attribute_script).
-export([main/1]).
%%%===================================================================
%%% API functions
%%%===================================================================
main(_) ->
    code:add_path(filename:dirname(escript:script_name()) ++ "/../../../beam/"),
    io:format("~p~n", [catch attribute_maker:start(attribute())]).

%%%===================================================================
%%% record data
%%%===================================================================
attribute() ->
    [
        {"src/module/attribute/attribute.erl", attribute_data, "attribute"},
        {"include/attribute.hrl", attribute_data, "attribute"}
    ].

%%%-------------------------------------------------------------------
%%% @doc
%%% event script for event maker
%%% @end
%%%-------------------------------------------------------------------
-module(event_script).
-export([main/1]).
-include("../../../include/journal.hrl").
%%%===================================================================
%%% API functions
%%%===================================================================
main(_) ->
    code:add_path(filename:dirname(escript:script_name()) ++ "/../../../beam/"),
    try
        io:format("~p~n", [event_maker:start(event())])
    catch ?EXCEPTION(_Class, Reason, Stacktrace) ->
        ?ERROR_STACKTRACE(Reason, Stacktrace)
    end.

%%%===================================================================
%%% event data
%%%===================================================================
event() ->
    [
        {"src/module/user/user_event.erl", "include/", {user, event}, "src/module/*/*.erl"}
    ].

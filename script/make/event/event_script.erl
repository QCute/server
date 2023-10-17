%%%-------------------------------------------------------------------
%%! +pc unicode
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
    io:setopts([{encoding, unicode}]),
    io:setopts(standard_error, [{encoding, unicode}]),
    code:add_path(filename:dirname(escript:script_name()) ++ "/../../../beam/"),
    try
        io:format("~tp~n", [event_maker:start(event())])
    catch ?EXCEPTION(Class, Reason, Stacktrace) ->
        ?HALT(Class, Reason, Stacktrace)
    end.

%%%===================================================================
%%% event data
%%%===================================================================
event() ->
    [
        #{
            file => "src/module/user/user_event.erl",
            include => "include/",
            name => {user, event},
            wildcard => "src/module/*/*.erl"
        }
    ].

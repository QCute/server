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
    ets:insert(ets:new(shell_records, [set, public]), [{Tag, Form} || Form = {attribute, _, record, {Tag, _}} <- lists:append([element(2, epp:parse_file(Header, [], [])) || Header <- filelib:wildcard(filename:dirname(escript:script_name()) ++ "/../../../include/*.hrl")])]),
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
            file => "src/module/event/dispatcher.erl",
            include => "include/",
            name => {user, event},
            wildcard => "src/module/*/*.erl"
        }
    ].

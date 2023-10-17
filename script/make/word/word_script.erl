%%%-------------------------------------------------------------------
%%! +pc unicode
%%% @doc
%%% word script for word maker
%%% @end
%%%-------------------------------------------------------------------
-module(word_script).
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
        io:format("~tp~n", [word_maker:start(words())])
    catch ?EXCEPTION(Class, Reason, Stacktrace) ->
        ?HALT(Class, Reason, Stacktrace)
    end.

%%%===================================================================
%%% words data
%%%===================================================================
words() ->
    [
        #{
            file => "src/tool/extension/sensitive_word_data.erl",
            table => sensitive_word_data
        }
    ].

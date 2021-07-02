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
    code:add_path(filename:dirname(escript:script_name()) ++ "/../../../beam/"),
    try
        io:format("~p~n", [word_maker:start(words())])
    catch ?EXCEPTION(Class, Reason, Stacktrace) ->
        ?ERROR_STACKTRACE(Class, Reason, Stacktrace)
    end.

%%%===================================================================
%%% words data
%%%===================================================================
words() ->
    [{"src/tool/extension/sensitive_word_data.erl", sensitive_word_data}].
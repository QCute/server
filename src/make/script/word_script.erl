%%%-------------------------------------------------------------------
%%% @doc
%%% module word script
%%% @end
%%%-------------------------------------------------------------------
-module(word_script).
-export([main/1]).
%%%===================================================================
%%% API
%%%===================================================================
main(_) ->
    code:add_path(filename:dirname(escript:script_name()) ++ "/../../../beam/"),
    console:stacktrace(catch maker:start(fun word_maker:parse/2, words())),
    ok.

%%%===================================================================
%%% words data
%%%===================================================================
words() ->
    [{"src/data/data_sensitive_word.erl", data_sensitive_word}].
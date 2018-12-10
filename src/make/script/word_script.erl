%%%-------------------------------------------------------------------
%%% @doc
%%% module data script
%%% @end
%%%-------------------------------------------------------------------
-module(word_script).
-export([main/1]).
%%%===================================================================
%%% API
%%%===================================================================
main(_) ->
    code:add_path("beam"),
    code:add_path("../beam"),
    code:add_path("../../beam"),
    code:add_path("../../../beam"),
    console:stack_trace(catch maker:start(fun word_maker:parse/2, words())),
    ok.

%%%===================================================================
%%% words data
%%%===================================================================
words() ->
    [{"src/tool/word.erl", words}].
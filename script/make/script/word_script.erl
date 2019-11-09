%%%------------------------------------------------------------------
%%% @doc
%%% module word script
%%% @end
%%%------------------------------------------------------------------
-module(word_script).
-export([main/1]).
%%%==================================================================
%%% API functions
%%%==================================================================
main(_) ->
    code:add_path(filename:dirname(escript:script_name()) ++ "/../../../beam/"),
    console:stacktrace(catch word_maker:start(words())).

%%%==================================================================
%%% words data
%%%==================================================================
words() ->
    [{"src/tool/extension/sensitive_word_data.erl", sensitive_word_data}].
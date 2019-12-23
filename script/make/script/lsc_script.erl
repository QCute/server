%%%------------------------------------------------------------------
%%% @doc
%%% module user load/save/clean script
%%% @end
%%%------------------------------------------------------------------
-module(lsc_script).
-export([main/1]).
%%%==================================================================
%%% API functions
%%%==================================================================
main(_) ->
    code:add_path(filename:dirname(escript:script_name()) ++ "/../../../beam/"),
    Result = lsc_maker:start("include/user.hrl", "src/module/user/user_loader.erl", "src/module/user/user_saver.erl", "src/module/user/user_cleaner.erl"),
    io:format("~p~n", [Result]).

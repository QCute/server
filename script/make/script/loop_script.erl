%%%-------------------------------------------------------------------
%%% @doc
%%% module user load/save/clean script
%%% @end
%%%-------------------------------------------------------------------
-module(loop_script).
-export([main/1]).
%%%===================================================================
%%% API functions
%%%===================================================================
main(_) ->
    code:add_path(filename:dirname(escript:script_name()) ++ "/../../../beam/"),
    Result = loop_maker:start([{"src/module/user/user_loop.erl", "include/user.hrl"}]),
    io:format("~p~n", [Result]).

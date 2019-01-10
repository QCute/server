%%%-------------------------------------------------------------------
%%% @doc
%%% module data script
%%% @end
%%%-------------------------------------------------------------------
-module(log_script).
-export([main/1]).
%%%===================================================================
%%% API
%%%===================================================================
main([Key]) ->
    code:add_path("beam"),
    code:add_path("../beam"),
    code:add_path("../../beam"),
    code:add_path("../../../beam"),
    List = [X || X <- log(), string:str(atom_to_list(element(3, X)), Key) =/= 0],
    console:stack_trace(catch maker:start(fun log_maker:parse/2, List)),
    ok;
main(_) ->
    io:format("invail argument~n").

%%%===================================================================
%%% log data
%%%===================================================================
log() ->
    [
        {"src/module/log/log.erl", log, log_player},
        {"src/module/log/log_sql.erl", sql, log_player}
    ].
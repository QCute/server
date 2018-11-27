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
	List = [X || X <- log(), string:str(atom_to_list(element(2, X)), Key) =/= 0],
	console:stack_trace(catch maker:start(fun log_maker:parse/2, List)),
	ok;
main(_) ->
	io:format("invail argument~n").

%%%===================================================================
%%% log data
%%%===================================================================
log() ->
	[
		{"src/module/log/log_server.erl", log_player}
	].
%%%-------------------------------------------------------------------
%%% @doc
%%% module data script
%%% @end
%%%-------------------------------------------------------------------
-module(record_script).
-export([main/1]).
%%%===================================================================
%%% API
%%%===================================================================
main([Key]) ->
	code:add_path("beam"),
	code:add_path("../beam"),
	code:add_path("../../beam"),
	code:add_path("../../../beam"),
	List = [X || X <- record(), string:str(element(1, X), Key) =/= 0],
	console:stack_trace(catch maker:start(fun record_maker:parse/2, List)),
	c:pwd(),
	ok;
main(_) ->
	io:format("invail argument~n").

%%%===================================================================
%%% record data
%%%===================================================================
record() ->
	[
		{"include/player.hrl", user},
		{"include/player.hrl", player},
		{"include/player.hrl", online},
		{"include/guild.hrl", guild},
		{"include/guild.hrl", guild_player}
	].

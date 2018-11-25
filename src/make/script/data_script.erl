%%%-------------------------------------------------------------------
%%% @doc
%%% module data script
%%% @end
%%%-------------------------------------------------------------------
-module(data_script).
-export([main/1]).
%%%===================================================================
%%% API
%%%===================================================================
main([Key]) ->
	code:add_path("beam"),
	code:add_path("../beam"),
	code:add_path("../../beam"),
	code:add_path("../../../beam"),
	List = [X || X <- data(), string:str(element(1, X), Key) =/= 0],
	console:stack_trace(catch maker:start(fun data_maker:parse/2, List)),
	ok;
main(_) ->
	io:format("invail argument~n").

%%%===================================================================
%%% base data
%%%===================================================================
data() ->
	[
		{"../../src/data/data_vip.erl", ["vip.hrl"],
			[
				{"SELECT `vip` FROM `base_vip` group by `vip` order by `money` desc;", "get", [], []},
				{"SELECT `vip` FROM `base_vip` where `money` >= 'Money' group by `vip` order by `money` desc;", "get", [], []}
			]
		},
		{"../../src/data/data_yamen.erl", ["yamen.hrl"], [{"SELECT * FROM `base_yamen_guild_rank_award`", "get", [], []}]}
	].
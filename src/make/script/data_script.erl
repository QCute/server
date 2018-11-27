%%%-------------------------------------------------------------------
%%% @doc
%%% module data script
%%% @end
%%%-------------------------------------------------------------------
-module(data_script).
-export([main/1]).
%% ------------------------ user guide -------------------------------
%% 
%% sql      :: auto group by key(when key reduplicated)
%% type     :: [] | record | maps | tuple | list | origin
%% default  :: [] | record | maps | tuple | list | (specified value)
%% includes :: ["*.erl", "*.erl"]
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
		{"src/data/data_vip.erl", ["vip.hrl"],
			[
				{"SELECT `vip` FROM `data_vip` group by `vip` order by `money` desc;", "get", [], []},
				{"SELECT `vip` FROM `data_vip` where `money` >= 'Money' group by `vip` order by `money` desc;", "get", [], []}
			]
		},
		{"src/data/data_player.erl", ["player.hrl"], [{"SELECT `level` FROM `data_level` where Exp < `exp` order by `exp` asc;", "level", origin, 0}]}
	].
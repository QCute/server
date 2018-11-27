%%%-------------------------------------------------------------------
%%% @doc
%%% module data script
%%% @end
%%%-------------------------------------------------------------------
-module(sql_script).
-export([main/1]).
%%%===================================================================
%%% API
%%%===================================================================
main([Key | T]) ->
	code:add_path("beam"),
	code:add_path("../beam"),
	code:add_path("../../beam"),
	code:add_path("../../../beam"),
	List = [X || X <- sql(), string:str(element(1, X), Key) =/= 0],
	maker:save_param(T),
	console:stack_trace(catch maker:start(fun sql_maker:parse/2, List)),
	ok;
main(_) ->
	io:format("invail argument~n").

%%%===================================================================
%%% sql data
%%%===================================================================
sql() ->
	[
		{"src/module/player/player_sql.erl", player, ["common.hrl", "player.hrl"]},
		{"src/module/item/item_sql.erl", item, ["common.hrl", "item.hrl"]},
		{"src/module/guild/guild_player_sql.erl", guild_player, ["common.hrl", "guild.hrl"]}
	].
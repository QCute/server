%%%-------------------------------------------------------------------
%%% @doc
%%% module data script
%%% @end
%%%-------------------------------------------------------------------
-module(record_script).
-export([main/1]).
%% ------------------------ user guide -------------------------------
%%
%% default value guide
%% varchar/char                             => <<>>
%% varchar/char with (convert) specified    => []
%% varchar/char with (null) specified       => undefined
%% varchar/char with (number) specified     => number
%% 
%%%===================================================================
%%% API
%%%===================================================================
main([Key | T]) ->
    code:add_path("beam"),
    code:add_path("../beam"),
    code:add_path("../../beam"),
    code:add_path("../../../beam"),
    maker:save_param_list(T),
    List = [X || X <- record(), string:str(element(1, X), Key) =/= 0],
    console:stack_trace(catch maker:start(fun record_maker:parse/2, List)),
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
        {"include/player.hrl", assets},
        {"include/player.hrl", vip},
        {"include/player.hrl", online},
        {"include/item.hrl", item},
        {"include/item.hrl", data_item},
        {"include/fashion.hrl", fashion},
        {"include/guild.hrl", guild_status},
        {"include/guild.hrl", guild},
        {"include/guild.hrl", guild_player},
        {"include/key.hrl", key},
        {"include/key.hrl", data_key},
        {"include/key.hrl", data_key_award},
        {"include/rank.hrl", rank}
    ].

%%%-------------------------------------------------------------------
%%% @doc
%%% module record script
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
    code:add_path(filename:dirname(escript:script_name()) ++ "/../../../beam/"),
    maker:save_param_list(T),
    List = [X || X <- record(), filename:basename(element(1, X), ".hrl") == Key],
    console:stacktrace(catch maker:start(fun record_maker:parse/2, List)),
    ok;
main(_) ->
    io:format("invail argument~n").

%%%===================================================================
%%% record data
%%%===================================================================
record() ->
    [
        {"include/account.hrl", account},
        {"include/role.hrl", role},
        {"include/asset.hrl", asset},
        {"include/vip.hrl", vip},
        {"include/item.hrl", item},
        {"include/item.hrl", data_item},
        {"include/fashion.hrl", fashion},
        {"include/guild.hrl", guild},
        {"include/guild.hrl", guild_role},
        {"include/guild.hrl", guild_apply},
        {"include/key.hrl", key},
        {"include/key.hrl", data_key},
        {"include/key.hrl", data_key_award},
        {"include/rank.hrl", rank},
        {"include/quest.hrl", quest},
        {"include/quest.hrl", data_quest},
        {"include/mail.hrl", mail},
        {"include/shop.hrl", shop},
        {"include/shop.hrl", data_shop},
        {"include/friend.hrl", friend}
    ].

%%%-------------------------------------------------------------------
%%% @doc
%%% module cheat
%%% @end
%%%-------------------------------------------------------------------
-module(cheat).
%% API
-export([cheat/2]).
%% Includes
-include("../../../include/asset.hrl").
-include("../../../include/attribute.hrl").
-include("../../../include/auction.hrl").
-include("../../../include/buff.hrl").
-include("../../../include/common.hrl").
-include("../../../include/event.hrl").
-include("../../../include/friend.hrl").
-include("../../../include/guild.hrl").
-include("../../../include/item.hrl").
-include("../../../include/key.hrl").
-include("../../../include/mail.hrl").
-include("../../../include/map.hrl").
-include("../../../include/monster.hrl").
-include("../../../include/notice.hrl").
-include("../../../include/online.hrl").
-include("../../../include/protocol.hrl").
-include("../../../include/quest.hrl").
-include("../../../include/rank.hrl").
-include("../../../include/role.hrl").
-include("../../../include/serialize.hrl").
-include("../../../include/shop.hrl").
-include("../../../include/skill.hrl").
-include("../../../include/socket.hrl").
-include("../../../include/sorter.hrl").
-include("../../../include/user.hrl").
-include("../../../include/vip.hrl").
%% Macros
-ifdef(DEBUG).
-define(CHEAT, 1).
-else.
-define(CHEAT, 0).
-endif.
%%%===================================================================
%%% API
%%%===================================================================
%% @doc cheat
-spec cheat(User :: #user{}, Command :: string()) -> ok() | error().
cheat(User, Command) ->
    case do_cheat(User, Command, ?CHEAT) of
        {ok, NewUser = #user{}} ->
            {ok, [1, Command], NewUser};
        _ ->
            {error, [0, Command]}
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================
do_cheat(_User, _Command, 0) ->
    ok;
do_cheat(User, Command, _) ->
    case string:tokens(Command, "_") of
        ["add", "gold", Value] ->
            asset:add(User, [{gold, type:to_integer(Value)}]);
        ["add", "sliver", Value] ->
            asset:add(User, [{sliver, type:to_integer(Value)}]);
        _ ->
            {error, no_such_command}
    end.

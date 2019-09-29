%%%-------------------------------------------------------------------
%%% @doc
%%% module secret
%%% @end
%%%-------------------------------------------------------------------
-module(secret).
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
%%%===================================================================
%%% API
%%%===================================================================
%% @doc cheat
cheat(User, Command) ->
    case string:tokens(Command, "_") of
        ["add", "gold", Value] ->
            asset:add(User, [{gold, type:to_integer(Value)}]);
        ["add", "sliver", Value] ->
            asset:add(User, [{sliver, type:to_integer(Value)}]);
        _ ->
            {error, [1, Command]}
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

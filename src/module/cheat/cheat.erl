%%%------------------------------------------------------------------
%%% @doc
%%% module cheat
%%% @end
%%%------------------------------------------------------------------
-module(cheat).
%% API
-export([cheat/2]).
%% Includes
-include("../../../include/activity.hrl").
-include("../../../include/asset.hrl").
-include("../../../include/attribute.hrl").
-include("../../../include/auction.hrl").
-include("../../../include/boss.hrl").
-include("../../../include/buff.hrl").
-include("../../../include/common.hrl").
-include("../../../include/count.hrl").
-include("../../../include/effect.hrl").
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
%%%==================================================================
%%% API functions
%%%==================================================================
%% @doc cheat
-spec cheat(User :: #user{}, Command :: string()) -> ok() | error().
cheat(User, Command) ->
    case execute_command(User, Command, ?CHEAT) of
        {ok, NewUser = #user{}} ->
            {ok, [ok, Command], NewUser};
        Error ->
            Error
    end.

%%%==================================================================
%%% Internal functions
%%%==================================================================
execute_command(_User, _Command, 0) ->
    ok;
execute_command(User, Command, _) ->
    case string:tokens(Command, "_") of
        ["add", "gold", Value] ->
            asset:add(User, [{gold, type:to_integer(Value)}], ?MODULE);
        ["add", "sliver", Value] ->
            asset:add(User, [{sliver, type:to_integer(Value)}], ?MODULE);
        _ ->
            {error, no_such_command}
    end.

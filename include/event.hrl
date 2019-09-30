%%%-------------------------------------------------------------------
%%% @doc
%%% event define
%%% @end
%%%-------------------------------------------------------------------

%% 升级
-record(event_level_upgrade, {level = 0}).
%% 通关副本
-record(event_pass_dungeon, {dungeon_id = 0}).
%% 商店购买
-record(event_shop_buy, {shop_id = 0, number = 0}).
%% 加入公会
-record(event_guild_join, {}).
%% 杀怪
-record(event_kill_monster, {monster_id = 0, group_id = 0, number = 0}).

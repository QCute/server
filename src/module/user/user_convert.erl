%%%-------------------------------------------------------------------
%%% @doc
%%% user convert
%%% @end
%%%-------------------------------------------------------------------
-module(user_convert).
-compile(nowarn_export_all).
-compile(export_all).
%% Includes
-include("common.hrl").
-include("user.hrl").
-include("online.hrl").
-include("role.hrl").
-include("vip.hrl").
-include("chat.hrl").
-include("map.hrl").
-include("attribute.hrl").
%%%===================================================================
%%% API functions
%%%===================================================================
%% @doc to online digest
-spec to_online(User :: #user{}) -> #online{}.
to_online(#user{role_id = RoleId, sender_pid = SenderPid}) ->
    #online{role_id = RoleId, pid = self(), sender_pid = SenderPid, state = online}.

%% @doc to online(hosting) digest
-spec to_hosting(User :: #user{}) -> #online{}.
to_hosting(#user{role_id = RoleId}) ->
    #online{role_id = RoleId, pid = self(), sender_pid = undefined, state = hosting}.

%% @doc to fighter
-spec to_fighter(User :: #user{}) -> #fighter{}.
to_fighter(#user{role_id = RoleId, role_name = RoleName, sender_pid = SenderPid, role = #role{level = Level, sex = Sex, classes = Classes, map = #map{x = X, y = Y}}, skill = Skill, buff = Buff, total_attribute = TotalAttribute}) ->
    Skills = skill:to_battle_skill(Skill),
    Buffs = buff:to_battle_buff(Buff),
    Data = #fighter_role{role_name = RoleName, level = Level, sex = Sex, classes = Classes, pid = self(), sender_pid = SenderPid},
    #fighter{id = RoleId, type = ?MAP_OBJECT_ROLE, attribute = TotalAttribute, skill = Skills, buff = Buffs, x = X, y = Y, data = Data}.

%% @doc to world chat
-spec to_world_chat(User :: #user{}, Args :: [term()]) -> #world_chat{}.
to_world_chat(#user{role = #role{role_id = RoleId, role_name = RoleName, sex = Sex, avatar = Avatar, classes = Classes, level = Level}, vip = #vip{vip_level = VipLevel}, guild_id = GuildId, guild_name = GuildName}, [Type, Message]) ->
    #world_chat{id = increment_server:next(), role_id = RoleId, role_name = RoleName, sex = Sex, avatar = Avatar, classes = Classes, level = Level, vip_level = VipLevel, guild_id = GuildId, guild_name = GuildName, type = Type, message = Message, time = time:now()};

to_world_chat(#user{role = #role{role_id = RoleId, role_name = RoleName, sex = Sex, avatar = Avatar, classes = Classes, level = Level}, vip = #vip{vip_level = VipLevel}, guild_id = GuildId, guild_name = GuildName}, [Skin, LuckyMoneyId, From, Type, Message]) ->
    #world_chat{id = increment_server:next(), role_id = RoleId, role_name = RoleName, sex = Sex, avatar = Avatar, classes = Classes, level = Level, vip_level = VipLevel, guild_id = GuildId, guild_name = GuildName, skin = Skin, lucky_money_id = LuckyMoneyId, from = From, type = Type, message = Message, time = time:now()}.

%% @doc to guild chat
-spec to_guild_chat(User :: #user{}, Args :: [term()]) -> #guild_chat{}.
to_guild_chat(#user{role = #role{role_id = RoleId, role_name = RoleName, sex = Sex, avatar = Avatar, classes = Classes, level = Level}, vip = #vip{vip_level = VipLevel}, guild_id = GuildId, guild_name = GuildName, guild_job = GuildJob}, [Type, Message]) ->
    #guild_chat{id = increment_server:next(), role_id = RoleId, role_name = RoleName, sex = Sex, avatar = Avatar, classes = Classes, level = Level, vip_level = VipLevel, guild_id = GuildId, guild_name = GuildName, guild_job = GuildJob, type = Type, message = Message, time = time:now()};

to_guild_chat(#user{role = #role{role_id = RoleId, role_name = RoleName, sex = Sex, avatar = Avatar, classes = Classes, level = Level}, vip = #vip{vip_level = VipLevel}, guild_id = GuildId, guild_name = GuildName, guild_job = GuildJob}, [Skin, LuckyMoneyId, From, Type, Message]) ->
    #guild_chat{id = increment_server:next(), role_id = RoleId, role_name = RoleName, sex = Sex, avatar = Avatar, classes = Classes, level = Level, vip_level = VipLevel, guild_id = GuildId, guild_name = GuildName, guild_job = GuildJob, skin = Skin, lucky_money_id = LuckyMoneyId, from = From, type = Type, message = Message, time = time:now()}.

%% @doc to private chat
-spec to_private_chat(User :: #user{}, Args :: [term()]) -> #private_chat{}.
to_private_chat(#user{role_id = RoleId}, [ReceiverId, Type, Message]) ->
    #private_chat{sender_id = RoleId, receiver_id = ReceiverId, type = Type, message = Message, time = time:now()};

to_private_chat(#user{role_id = RoleId}, [ReceiverId, Skin, LuckyMoneyId, Type, Message]) ->
    #private_chat{sender_id = RoleId, receiver_id = ReceiverId, skin = Skin, lucky_money_id = LuckyMoneyId, type = Type, message = Message, time = time:now()}.



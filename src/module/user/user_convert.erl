%%%-------------------------------------------------------------------
%%% @doc
%%% user convert
%%% @end
%%%-------------------------------------------------------------------
-module(user_convert).
-export([to_online/1]).
-export([to_hosting/1]).
-export([to_fighter/1]).
-export([to_world_chat/2]).
-export([to_guild_chat/2]).
-export([to_private_chat/2]).
-export([to_self_friend/2]).
-export([to_friend/2]).
%% Includes
-include("common.hrl").
-include("user.hrl").
-include("online.hrl").
-include("role.hrl").
-include("vip.hrl").
-include("chat.hrl").
-include("friend.hrl").
-include("guild.hrl").
-include("map.hrl").
-include("attribute.hrl").
%%%===================================================================
%%% API functions
%%%===================================================================
%% @doc to online digest
-spec to_online(User :: #user{}) -> #online{}.
to_online(#user{role_id = RoleId, sender_pid = SenderPid}) ->
    #online{
        role_id = RoleId,
        pid = self(),
        sender_pid = SenderPid,
        state = online
    }.

%% @doc to online(hosting) digest
-spec to_hosting(User :: #user{}) -> #online{}.
to_hosting(#user{role_id = RoleId}) ->
    #online{
        role_id = RoleId,
        pid = self(),
        sender_pid = undefined,
        state = hosting
    }.

%% @doc to fighter
-spec to_fighter(User :: #user{}) -> #fighter{}.
to_fighter(User) ->
    #user{
        sender_pid = SenderPid,
        role = #role{
            role_id = RoleId,
            role_name = RoleName,
            level = Level,
            sex = Sex,
            classes = Classes,
            map = #map{
                x = X,
                y = Y
            }
        },
        skill = Skill,
        buff = Buff,
        total_attribute = TotalAttribute
    } = User,
    Skills = skill:to_battle_skill(Skill),
    Buffs = buff:to_battle_buff(Buff),
    Data = #fighter_role{
        role_name = RoleName,
        level = Level,
        sex = Sex,
        classes = Classes,
        pid = self(),
        sender_pid = SenderPid
    },
    #fighter{
        id = RoleId,
        type = ?MAP_OBJECT_ROLE,
        attribute = TotalAttribute,
        skill = Skills,
        buff = Buffs,
        x = X,
        y = Y,
        data = Data
    }.

%% @doc to world chat
-spec to_world_chat(User :: #user{}, Args :: [term()]) -> #world_chat{}.
to_world_chat(User, [Type, Message]) ->
    #user{
        role = #role{
            role_id = RoleId,
            role_name = RoleName,
            sex = Sex,
            avatar = Avatar,
            classes = Classes,
            level = Level
        },
        vip = #vip{
            vip_level = VipLevel
        },
        guild = #guild_role{
            guild_id = GuildId,
            guild_name = GuildName
        }
    } = User,
    #world_chat{
        id = increment_server:next(),
        role_id = RoleId,
        role_name = RoleName,
        sex = Sex,
        avatar = Avatar,
        classes = Classes,
        level = Level,
        vip_level = VipLevel,
        guild_id = GuildId,
        guild_name = GuildName,
        type = Type,
        message = Message,
        time = time:now()
    };

to_world_chat(User, [Skin, LuckyMoneyNo, From, Type, Message]) ->
    #user{
        role = #role{
            role_id = RoleId,
            role_name = RoleName,
            sex = Sex,
            avatar = Avatar,
            classes = Classes,
            level = Level
        },
        vip = #vip{
            vip_level = VipLevel
        },
        guild = #guild_role{
            guild_id = GuildId,
            guild_name = GuildName
        }
    } = User,
    #world_chat{
        id = increment_server:next(),
        role_id = RoleId,
        role_name = RoleName,
        sex = Sex,
        avatar = Avatar,
        classes = Classes,
        level = Level,
        vip_level = VipLevel,
        guild_id = GuildId,
        guild_name = GuildName,
        skin = Skin,
        lucky_money_no = LuckyMoneyNo,
        from = From,
        type = Type,
        message = Message,
        time = time:now()
    }.

%% @doc to guild chat
-spec to_guild_chat(User :: #user{}, Args :: [term()]) -> #guild_chat{}.
to_guild_chat(User, [Type, Message]) ->
    #user{
        role = #role{
            role_id = RoleId,
            role_name = RoleName,
            sex = Sex,
            avatar = Avatar,
            classes = Classes,
            level = Level
        },
        vip = #vip{
            vip_level = VipLevel
        },
        guild = #guild_role{
            guild_id = GuildId,
            guild_name = GuildName,
            job = GuildJob
        }
    } = User,
    #guild_chat{
        id = increment_server:next(),
        role_id = RoleId,
        role_name = RoleName,
        sex = Sex,
        avatar = Avatar,
        classes = Classes,
        level = Level,
        vip_level = VipLevel,
        guild_id = GuildId,
        guild_name = GuildName,
        guild_job = GuildJob,
        type = Type,
        message = Message,
        time = time:now()
    };
to_guild_chat(User, [Skin, LuckyMoneyNo, From, Type, Message]) ->
    #user{
        role = #role{
            role_id = RoleId,
            role_name = RoleName,
            sex = Sex,
            avatar = Avatar,
            classes = Classes,
            level = Level
        },
        vip = #vip{
            vip_level = VipLevel
        },
        guild = #guild_role{
            guild_id = GuildId,
            guild_name = GuildName,
            job = GuildJob
        }
    } = User,
    #guild_chat{
        id = increment_server:next(),
        role_id = RoleId,
        role_name = RoleName,
        sex = Sex,
        avatar = Avatar,
        classes = Classes,
        level = Level,
        vip_level = VipLevel,
        guild_id = GuildId,
        guild_name = GuildName,
        guild_job = GuildJob,
        skin = Skin,
        lucky_money_no = LuckyMoneyNo,
        from = From,
        type = Type,
        message = Message,
        time = time:now()
    }.

%% @doc to private chat
-spec to_private_chat(User :: #user{}, Args :: [term()]) -> #private_chat{}.
to_private_chat(#user{role_id = RoleId}, [ReceiverId, Type, Message]) ->
    #private_chat{
        sender_id = RoleId,
        receiver_id = ReceiverId,
        type = Type,
        message = Message,
        time = time:now()
    };
to_private_chat(#user{role_id = RoleId}, [ReceiverId, Skin, LuckyMoneyNo, Type, Message]) ->
    #private_chat{
        sender_id = RoleId,
        receiver_id = ReceiverId,
        skin = Skin,
        lucky_money_no = LuckyMoneyNo,
        type = Type,
        message = Message,
        time = time:now()
    }.

%% @doc to self friend
-spec to_self_friend(User :: #user{}, Online :: #online{}) -> Friend :: #friend{}.
to_self_friend(#user{role_id = RoleId}, Online) ->
    #online{
        role_id = FriendRoleId,
        role_name = FriendName,
        sex = Sex,
        avatar = Avatar,
        classes = Classes,
        level = Level,
        vip_level = VipLevel
    } = Online,
    #friend{
        role_id = RoleId,
        friend_role_id = FriendRoleId,
        friend_name = FriendName,
        sex = Sex,
        avatar = Avatar,
        classes = Classes,
        level = Level,
        vip_level = VipLevel,
        is_online = 1,
        relation = ?FRIEND_RELATION_APPLY,
        flag = 1
    }.

%% @doc to friend
-spec to_friend(User :: #user{}, FriendRoleId :: non_neg_integer()) -> Friend :: #friend{}.
to_friend(User, FriendRoleId) ->
    #user{
        role = #role{
            role_id = RoleId,
            role_name = RoleName,
            sex = Sex,
            avatar = Avatar,
            classes = Classes,
            level = Level
        },
        vip = #vip{
            vip_level = VipLevel
        }
    } = User,
    #friend{
        role_id = FriendRoleId,
        friend_role_id = RoleId,
        friend_name = RoleName,
        sex = Sex,
        avatar = Avatar,
        classes = Classes,
        level = Level,
        vip_level = VipLevel,
        is_online = 1,
        relation = ?FRIEND_RELATION_APPLY,
        time = time:now(),
        flag = 1
    }.

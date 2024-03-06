-module(role_sql).
-export([insert/1]).
-export([select/1]).
-export([update/1]).
-export([update_name/1]).
-include("role.hrl").

%% @doc insert into role
-spec insert(Role :: #role{}) -> InsertIdOrAffectedRows :: non_neg_integer().
insert(#role{role_name = RoleName, server_id = ServerId, account_name = AccountName, origin_server_id = OriginServerId, type = Type, status = Status, sex = Sex, avatar = Avatar, classes = Classes, level = Level, is_online = IsOnline, register_time = RegisterTime, login_time = LoginTime, logout_time = LogoutTime, world_chat_time = WorldChatTime, guild_chat_time = GuildChatTime, first_charge_time = FirstChargeTime, last_charge_time = LastChargeTime, charge_total = ChargeTotal, item_size = ItemSize, bag_size = BagSize, store_size = StoreSize, map = Map, channel = Channel, device_id = DeviceId, device_type = DeviceType, mac = Mac, ip = Ip}) ->
    db:insert(<<"INSERT INTO `role` (`role_name`, `server_id`, `account_name`, `origin_server_id`, `type`, `status`, `sex`, `avatar`, `classes`, `level`, `is_online`, `register_time`, `login_time`, `logout_time`, `world_chat_time`, `guild_chat_time`, `first_charge_time`, `last_charge_time`, `charge_total`, `item_size`, `bag_size`, `store_size`, `map`, `channel`, `device_id`, `device_type`, `mac`, `ip`) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)">>, [RoleName, ServerId, AccountName, OriginServerId, Type, Status, Sex, Avatar, Classes, Level, IsOnline, RegisterTime, LoginTime, LogoutTime, WorldChatTime, GuildChatTime, FirstChargeTime, LastChargeTime, ChargeTotal, ItemSize, BagSize, StoreSize, Map, Channel, DeviceId, DeviceType, Mac, Ip]).

%% @doc select from role
-spec select(RoleId :: non_neg_integer()) -> Rows :: [#role{}].
select(RoleId) ->
    Data = db:select(<<"SELECT `role_id`, `role_name`, `server_id`, `account_name`, `origin_server_id`, `type`, `status`, `sex`, `avatar`, `classes`, `level`, `is_online`, `register_time`, `login_time`, `logout_time`, `world_chat_time`, `guild_chat_time`, `first_charge_time`, `last_charge_time`, `charge_total`, `item_size`, `bag_size`, `store_size`, `map`, `channel`, `device_id`, `device_type`, `mac`, `ip` FROM `role` WHERE `role_id` = ?">>, [RoleId]),
    parser:convert(Data, role).

%% @doc update into role
-spec update(#role{}) -> AffectedRows :: non_neg_integer().
update(#role{role_id = RoleId, role_name = RoleName, server_id = ServerId, account_name = AccountName, origin_server_id = OriginServerId, type = Type, status = Status, sex = Sex, avatar = Avatar, classes = Classes, level = Level, is_online = IsOnline, register_time = RegisterTime, login_time = LoginTime, logout_time = LogoutTime, world_chat_time = WorldChatTime, guild_chat_time = GuildChatTime, first_charge_time = FirstChargeTime, last_charge_time = LastChargeTime, charge_total = ChargeTotal, item_size = ItemSize, bag_size = BagSize, store_size = StoreSize, map = Map, channel = Channel, device_id = DeviceId, device_type = DeviceType, mac = Mac, ip = Ip, role_id = RoleId}) ->
    db:update(<<"UPDATE `role` SET `role_name` = ?, `server_id` = ?, `account_name` = ?, `origin_server_id` = ?, `type` = ?, `status` = ?, `sex` = ?, `avatar` = ?, `classes` = ?, `level` = ?, `is_online` = ?, `register_time` = ?, `login_time` = ?, `logout_time` = ?, `world_chat_time` = ?, `guild_chat_time` = ?, `first_charge_time` = ?, `last_charge_time` = ?, `charge_total` = ?, `item_size` = ?, `bag_size` = ?, `store_size` = ?, `map` = ?, `channel` = ?, `device_id` = ?, `device_type` = ?, `mac` = ?, `ip` = ? WHERE `role_id` = ?">>, [RoleId, RoleName, ServerId, AccountName, OriginServerId, Type, Status, Sex, Avatar, Classes, Level, IsOnline, RegisterTime, LoginTime, LogoutTime, WorldChatTime, GuildChatTime, FirstChargeTime, LastChargeTime, ChargeTotal, ItemSize, BagSize, StoreSize, Map, Channel, DeviceId, DeviceType, Mac, Ip, RoleId]).

%% @doc update into role
-spec update_name(#role{}) -> AffectedRows :: non_neg_integer().
update_name(#role{role_name = RoleName, role_id = RoleId}) ->
    db:update(<<"UPDATE `role` SET `role_name` = ? WHERE `role_id` = ?">>, [RoleName, RoleId]).

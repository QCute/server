-module(device_sql).
-export([insert/1]).
-export([select/1]).
-export([update/1]).
-include("device.hrl").

%% @doc insert into device
-spec insert(Device :: #device{}) -> InsertIdOrAffectedRows :: non_neg_integer().
insert(#device{role_id = RoleId, os = Os, name = Name, device_id = DeviceId, mac = Mac, ip = Ip}) ->
    db:insert(<<"INSERT INTO `device` (`role_id`, `os`, `name`, `device_id`, `mac`, `ip`) VALUES (?, ?, ?, ?, ?, ?)">>, [RoleId, Os, Name, DeviceId, Mac, Ip]).

%% @doc select from device
-spec select(RoleId :: non_neg_integer()) -> Rows :: [#device{}].
select(RoleId) ->
    Data = db:select(<<"SELECT `role_id`, `os`, `name`, `device_id`, `mac`, `ip` FROM `device` WHERE `role_id` = ?">>, [RoleId]),
    parser:convert(Data, device).

%% @doc update into device
-spec update(#device{}) -> AffectedRows :: non_neg_integer().
update(#device{os = Os, name = Name, device_id = DeviceId, mac = Mac, ip = Ip, role_id = RoleId}) ->
    db:update(<<"UPDATE `device` SET `os` = ?, `name` = ?, `device_id` = ?, `mac` = ?, `ip` = ? WHERE `role_id` = ?">>, [Os, Name, DeviceId, Mac, Ip, RoleId]).

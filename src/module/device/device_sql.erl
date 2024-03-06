-module(device_sql).
-export([insert/1]).
-export([select/1]).
-export([update/1]).
-include("device.hrl").

%% @doc insert into device
-spec insert(Device :: #device{}) -> InsertIdOrAffectedRows :: non_neg_integer().
insert(Device) ->
    db:insert(<<"INSERT INTO `device` (`role_id`, `os`, `name`, `device_id`, `mac`, `ip`) VALUES (:1:, :2:, :3:, :4:, :5:, :6:)">>, Device).

%% @doc select from device
-spec select(RoleId :: non_neg_integer()) -> Rows :: [#device{}].
select(RoleId) ->
    Data = db:select(<<"SELECT `role_id`, `os`, `name`, `device_id`, `mac`, `ip` FROM `device` WHERE `role_id` = ?">>, [RoleId]),
    parser:convert(Data, device).

%% @doc update into device
-spec update(Device :: #device{}) -> AffectedRows :: non_neg_integer().
update(Device) ->
    db:update(<<"UPDATE `device` SET `os` = :2:, `name` = :3:, `device_id` = :4:, `mac` = :5:, `ip` = :6: WHERE `role_id` = :1:">>, Device).

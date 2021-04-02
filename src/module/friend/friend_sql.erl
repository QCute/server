-module(friend_sql).
-compile(nowarn_export_all).
-compile(export_all).
-include("friend.hrl").
-define(INSERT_FRIEND, <<"INSERT INTO `friend` (`role_id`, `friend_id`, `relation`, `time`) VALUES (~i~w, ~w, ~i~i~i~i~i~i~i~w, ~w~i)">>).
-define(SELECT_FRIEND, <<"SELECT `role_id`, `friend_id`, '' AS `friend_name`, 0 AS `sex`, 0 AS `avatar`, 0 AS `classes`, 0 AS `level`, 0 AS `vip_level`, 0 AS `is_online`, `relation`, `time`, 0 AS `flag` FROM `friend` WHERE `role_id` = ~w AND `friend_id` = ~w">>).
-define(UPDATE_FRIEND, {<<"UPDATE `friend` SET ~i~i~i~i~i~i~i~i~i~i`relation` = ~w, `time` = ~w~i ">>, <<"WHERE `role_id` = ~w AND `friend_id` = ~w">>}).
-define(DELETE_FRIEND, <<"DELETE  FROM `friend` WHERE `role_id` = ~w AND `friend_id` = ~w">>).
-define(INSERT_UPDATE_FRIEND, {<<"INSERT INTO `friend` (`role_id`, `friend_id`, `relation`, `time`) VALUES ">>, <<"(~i~w, ~w, ~i~i~i~i~i~i~i~w, ~w~i)">>, <<" ON DUPLICATE KEY UPDATE `relation` = VALUES(`relation`), `time` = VALUES(`time`)">>}).
-define(SELECT_JOIN_FRIEND, <<"SELECT `friend`.`role_id`, `friend`.`friend_id`, IFNULL(`role`.`role_name`, '') AS `friend_name`, IFNULL(`role`.`sex`, 0) AS `sex`, IFNULL(`role`.`avatar`, 0) AS `avatar`, IFNULL(`role`.`classes`, 0) AS `classes`, IFNULL(`role`.`level`, 0) AS `level`, IFNULL(`vip`.`vip_level`, 0) AS `vip_level`, IFNULL(`role`.`is_online`, 0) AS `is_online`, `friend`.`relation`, `friend`.`time`, IFNULL(`friend`.`flag`, 0) AS `flag` FROM `friend` LEFT JOIN `role` ON `friend`.`friend_id` = `role`.`role_id` LEFT JOIN `vip` ON `friend`.`friend_id` = `vip`.`role_id` WHERE `friend`.`role_id` = ~w AND `friend`.`friend_id` = ~w">>).
-define(SELECT_BY_ROLE_ID, <<"SELECT `role_id`, `friend_id`, '' AS `friend_name`, 0 AS `sex`, 0 AS `avatar`, 0 AS `classes`, 0 AS `level`, 0 AS `vip_level`, 0 AS `is_online`, `relation`, `time`, 0 AS `flag` FROM `friend` WHERE `role_id` = ~w">>).
-define(SELECT_JOIN_BY_ROLE_ID, <<"SELECT `friend`.`role_id`, `friend`.`friend_id`, IFNULL(`role`.`role_name`, '') AS `friend_name`, IFNULL(`role`.`sex`, 0) AS `sex`, IFNULL(`role`.`avatar`, 0) AS `avatar`, IFNULL(`role`.`classes`, 0) AS `classes`, IFNULL(`role`.`level`, 0) AS `level`, IFNULL(`vip`.`vip_level`, 0) AS `vip_level`, IFNULL(`role`.`is_online`, 0) AS `is_online`, `friend`.`relation`, `friend`.`time`, IFNULL(`friend`.`flag`, 0) AS `flag` FROM `friend` LEFT JOIN `role` ON `friend`.`friend_id` = `role`.`role_id` LEFT JOIN `vip` ON `friend`.`friend_id` = `vip`.`role_id` WHERE `friend`.`role_id` = ~w">>).

%% @doc insert
insert(Friend) ->
    Sql = parser:format(?INSERT_FRIEND, Friend),
    db:insert(Sql).

%% @doc select
select(RoleId, FriendId) ->
    Sql = parser:format(?SELECT_FRIEND, [RoleId, FriendId]),
    Data = db:select(Sql),
    parser:convert(Data, friend).

%% @doc update
update(Friend) ->
    Sql = <<(parser:format(element(1, ?UPDATE_FRIEND), Friend))/binary, (parser:format(element(2, ?UPDATE_FRIEND), [Friend#friend.role_id, Friend#friend.friend_id]))/binary>>,
    db:update(Sql).

%% @doc delete
delete(RoleId, FriendId) ->
    Sql = parser:format(?DELETE_FRIEND, [RoleId, FriendId]),
    db:delete(Sql).


%% @doc insert_update
insert_update(Data) ->
    {Sql, NewData} = parser:collect_into(Data, ?INSERT_UPDATE_FRIEND, #friend.flag),
    db:insert(Sql),
    NewData.

%% @doc select join
select_join(RoleId, FriendId) ->
    Sql = parser:format(?SELECT_JOIN_FRIEND, [RoleId, FriendId]),
    Data = db:select(Sql),
    parser:convert(Data, friend).

%% @doc select
select_by_role_id(RoleId) ->
    Sql = parser:format(?SELECT_BY_ROLE_ID, [RoleId]),
    Data = db:select(Sql),
    parser:convert(Data, friend).

%% @doc select join
select_join_by_role_id(RoleId) ->
    Sql = parser:format(?SELECT_JOIN_BY_ROLE_ID, [RoleId]),
    Data = db:select(Sql),
    parser:convert(Data, by_role_id).


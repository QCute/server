-module(friend_sql).
-compile(nowarn_export_all).
-compile(export_all).
-include("friend.hrl").
-define(INSERT_FRIEND, <<"INSERT INTO `friend` (`role_id`, `friend_id`, `state`, `time`) VALUES ('~w', '~w', '~w', '~w')">>).
-define(DELETE_FRIEND, <<"DELETE  FROM `friend` WHERE `role_id` = '~w' AND `friend_id` = '~w'">>).
-define(SELECT_FRIEND, <<"SELECT * FROM `friend` WHERE `role_id` = '~w'">>).
-define(UPDATE_FRIEND, <<"UPDATE `friend` SET `state` = '~w', `time` = '~w' WHERE `role_id` = '~w' AND `friend_id` = '~w'">>).
-define(INSERT_UPDATE_FRIEND, {<<"INSERT INTO `friend` (`role_id`, `friend_id`, `state`, `time`) VALUES ">>, <<"('~w', '~w', '~w', '~w')">>, <<" ON DUPLICATE KEY UPDATE `state` = '~w', `time` = '~w'">>}).
-define(SELECT_JOIN_FRIEND, <<"SELECT `friend`.`role_id`, `role`.`role_id`, `friend`.`state`, `friend`.`time`, `friend`.`friend_name`, `friend`.`online`, `friend`.`flag` FROM `friend`LEFT JOIN `role` ON`friend`.`friend_id` = `role`.`role_id` WHERE `friend`.`role_id` = '~w'">>).

%% @doc insert
insert(Friend) ->
    Sql = parser:format(?INSERT_FRIEND, [
        Friend#friend.role_id,
        Friend#friend.friend_id,
        Friend#friend.state,
        Friend#friend.time
    ]),
    sql:insert(Sql).

%% @doc delete
delete(RoleId, FriendId) ->
    Sql = parser:format(?DELETE_FRIEND, [RoleId, FriendId]),
    sql:delete(Sql).

%% @doc select
select(RoleId) ->
    Sql = parser:format(?SELECT_FRIEND, [RoleId]),
    sql:select(Sql).

%% @doc update
update(Friend) ->
    Sql = parser:format(?UPDATE_FRIEND, [
        Friend#friend.state,
        Friend#friend.time
    ]),
    sql:update(Sql).


%% @doc insert_update
insert_update(Data) ->
    F = fun(Friend) -> [
        Friend#friend.role_id,
        Friend#friend.friend_id,
        Friend#friend.state,
        Friend#friend.time
    ] end,
    {Sql, NewData} = parser:collect(Data, F, ?INSERT_UPDATE_FRIEND, flag),
    sql:insert(Sql),
    NewData.

%% @doc select join
select_join(RoleId) ->
    Sql = parser:format(?SELECT_JOIN_FRIEND, [RoleId]),
    sql:select(Sql).


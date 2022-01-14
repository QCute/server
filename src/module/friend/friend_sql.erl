-module(friend_sql).
-export([insert/1]).
-export([select/2]).
-export([update/1]).
-export([delete/2]).
-export([insert_update/1]).
-export([select_join/2]).
-export([select_by_role_id/1]).
-export([select_join_by_role_id/1]).
-export([update_relation/3]).
-include("friend.hrl").

-define(INSERT_FRIEND, <<"INSERT INTO `friend` (`role_id`, `friend_role_id`, `relation`, `time`) VALUES (~i~w, ~w, ~i~i~i~i~i~i~i~w, ~w~i)">>).
-define(SELECT_FRIEND, <<"SELECT `role_id`, `friend_role_id`, '' AS `friend_name`, 0 AS `sex`, 0 AS `avatar`, 0 AS `classes`, 0 AS `level`, 0 AS `vip_level`, 0 AS `is_online`, `relation`, `time`, 0 AS `flag` FROM `friend` WHERE `role_id` = ~w AND `friend_role_id` = ~w">>).
-define(UPDATE_FRIEND, {<<"UPDATE `friend` SET ~i~i~i~i~i~i~i~i~i~i`relation` = ~w, `time` = ~w~i ">>, <<"WHERE `role_id` = ~w AND `friend_role_id` = ~w">>}).
-define(DELETE_FRIEND, <<"DELETE FROM `friend` WHERE `role_id` = ~w AND `friend_role_id` = ~w">>).
-define(INSERT_UPDATE_FRIEND, {<<"INSERT INTO `friend` (`role_id`, `friend_role_id`, `relation`, `time`) VALUES ">>, <<"(~i~w, ~w, ~i~i~i~i~i~i~i~w, ~w~i)">>, <<" ON DUPLICATE KEY UPDATE `relation` = VALUES(`relation`), `time` = VALUES(`time`)">>}).
-define(SELECT_JOIN_FRIEND, <<"SELECT `friend`.`role_id`, `friend`.`friend_role_id`, IFNULL(`role`.`role_name`, '') AS `friend_name`, IFNULL(`role`.`sex`, 0) AS `sex`, IFNULL(`role`.`avatar`, 0) AS `avatar`, IFNULL(`role`.`classes`, 0) AS `classes`, IFNULL(`role`.`level`, 0) AS `level`, IFNULL(`vip`.`vip_level`, 0) AS `vip_level`, IFNULL(`role`.`is_online`, 0) AS `is_online`, `friend`.`relation`, `friend`.`time`, IFNULL(`friend`.`flag`, 0) AS `flag` FROM `friend` LEFT JOIN `role` ON `friend`.`friend_role_id` = `role`.`role_id` LEFT JOIN `vip` ON `friend`.`friend_role_id` = `vip`.`role_id` WHERE `friend`.`role_id` = ~w AND `friend`.`friend_role_id` = ~w">>).
-define(SELECT_BY_ROLE_ID, <<"SELECT `role_id`, `friend_role_id`, '' AS `friend_name`, 0 AS `sex`, 0 AS `avatar`, 0 AS `classes`, 0 AS `level`, 0 AS `vip_level`, 0 AS `is_online`, `relation`, `time`, 0 AS `flag` FROM `friend` WHERE `role_id` = ~w">>).
-define(SELECT_JOIN_BY_ROLE_ID, <<"SELECT `friend`.`role_id`, `friend`.`friend_role_id`, IFNULL(`role`.`role_name`, '') AS `friend_name`, IFNULL(`role`.`sex`, 0) AS `sex`, IFNULL(`role`.`avatar`, 0) AS `avatar`, IFNULL(`role`.`classes`, 0) AS `classes`, IFNULL(`role`.`level`, 0) AS `level`, IFNULL(`vip`.`vip_level`, 0) AS `vip_level`, IFNULL(`role`.`is_online`, 0) AS `is_online`, `friend`.`relation`, `friend`.`time`, IFNULL(`friend`.`flag`, 0) AS `flag` FROM `friend` LEFT JOIN `role` ON `friend`.`friend_role_id` = `role`.`role_id` LEFT JOIN `vip` ON `friend`.`friend_role_id` = `vip`.`role_id` WHERE `friend`.`role_id` = ~w">>).
-define(UPDATE_RELATION, <<"UPDATE `friend` SET `relation` = ~w WHERE `role_id` = ~w AND `friend_role_id` = ~w">>).

%% @doc insert
-spec insert(Friend :: #friend{}) -> InsertIdOrAffectedRows :: non_neg_integer().
insert(Friend) ->
    Sql = parser:format(?INSERT_FRIEND, Friend),
    db:insert(Sql).

%% @doc select
-spec select(RoleId :: integer(), FriendRoleId :: integer()) -> FriendList :: [#friend{}].
select(RoleId, FriendRoleId) ->
    Sql = parser:format(?SELECT_FRIEND, [RoleId, FriendRoleId]),
    Data = db:select(Sql),
    parser:convert(Data, friend).

%% @doc update
-spec update(Friend :: #friend{}) -> AffectedRows :: non_neg_integer().
update(Friend) ->
    Sql = <<(parser:format(element(1, ?UPDATE_FRIEND), Friend))/binary, (parser:format(element(2, ?UPDATE_FRIEND), [Friend#friend.role_id, Friend#friend.friend_role_id]))/binary>>,
    db:update(Sql).

%% @doc delete
-spec delete(RoleId :: integer(), FriendRoleId :: integer()) -> AffectedRows :: non_neg_integer().
delete(RoleId, FriendRoleId) ->
    Sql = parser:format(?DELETE_FRIEND, [RoleId, FriendRoleId]),
    db:delete(Sql).


%% @doc insert_update
-spec insert_update(FriendList :: [#friend{}] | ets:tab()) -> NewFriendList :: [#friend{}].
insert_update(FriendList) ->
    {Sql, NewFriendList} = parser:collect_into(FriendList, ?INSERT_UPDATE_FRIEND, #friend.flag),
    db:insert(Sql),
    NewFriendList.

%% @doc select join
-spec select_join(RoleId :: integer(), FriendRoleId :: integer()) -> FriendList :: [#friend{}].
select_join(RoleId, FriendRoleId) ->
    Sql = parser:format(?SELECT_JOIN_FRIEND, [RoleId, FriendRoleId]),
    Data = db:select(Sql),
    parser:convert(Data, friend).

%% @doc select
-spec select_by_role_id(RoleId :: integer()) -> FriendList :: [#friend{}].
select_by_role_id(RoleId) ->
    Sql = parser:format(?SELECT_BY_ROLE_ID, [RoleId]),
    Data = db:select(Sql),
    parser:convert(Data, friend).

%% @doc select join
-spec select_join_by_role_id(RoleId :: integer()) -> FriendList :: [#friend{}].
select_join_by_role_id(RoleId) ->
    Sql = parser:format(?SELECT_JOIN_BY_ROLE_ID, [RoleId]),
    Data = db:select(Sql),
    parser:convert(Data, friend).

%% @doc update
-spec update_relation(UpdateRelation :: integer(), RoleId :: integer(), FriendRoleId :: integer()) -> non_neg_integer().
update_relation(UpdateRelation, RoleId, FriendRoleId) ->
    Sql = parser:format(?UPDATE_RELATION, [UpdateRelation, RoleId, FriendRoleId]),
    db:update(Sql).


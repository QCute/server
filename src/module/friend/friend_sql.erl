-module(friend_sql).
-export([save/1]).
-export([select/1]).
-export([update_relation/1]).
-export([delete/2]).
-include("friend.hrl").

%% @doc insert into friend
-spec save(FriendList :: [#friend{}] | ets:tab()) -> NewFriendList :: [#friend{}].
save(FriendList) ->
    db:save_into(<<"INSERT INTO `friend` (`role_id`, `friend_role_id`, `relation`, `time`) VALUES">>, <<"(?, ?, ?, ?)">>, <<"ON DUPLICATE KEY UPDATE `role_id` = VALUES(`role_id`), `friend_role_id` = VALUES(`friend_role_id`), `relation` = VALUES(`relation`), `time` = VALUES(`time`)">>, FriendList, fun(#friend{role_id = RoleId, friend_role_id = FriendRoleId, relation = Relation, time = Time}) -> [RoleId, FriendRoleId, Relation, Time] end, #friend.flag).

%% @doc select from friend
-spec select(FriendRoleId :: non_neg_integer()) -> Rows :: [#friend{}].
select(FriendRoleId) ->
    Data = db:select(<<"SELECT `friend`.`role_id`, `friend`.`friend_role_id`, `role`.`role_name`, `role`.`sex`, `role`.`avatar`, `role`.`classes`, `role`.`level`, `vip`.`vip_level`, `role`.`is_online`, `friend`.`relation`, `friend`.`time`, `friend`.`flag` FROM `friend` INNER JOIN `role` ON `role`.`role_id` = `friend`.`role_id` INNER JOIN `vip` ON `vip`.`role_id` = `friend`.`role_id` WHERE `friend`.`role_id` = ?">>, [FriendRoleId]),
    parser:convert(Data, friend).

%% @doc update into friend
-spec update_relation(#friend{}) -> AffectedRows :: non_neg_integer().
update_relation(#friend{relation = Relation, role_id = RoleId, friend_role_id = FriendRoleId}) ->
    db:update(<<"UPDATE `friend` SET `relation` = ? WHERE `role_id` = ? AND `friend_role_id` = ?">>, [Relation, RoleId, FriendRoleId]).

%% @doc delete row from friend
-spec delete(RoleId :: non_neg_integer(), FriendRoleId :: non_neg_integer()) -> AffectedRows :: non_neg_integer().
delete(RoleId, FriendRoleId) ->
    db:delete(<<"DELETE FROM `friend` WHERE `role_id` = ? AND `friend_role_id` = ?">>, [RoleId, FriendRoleId]).

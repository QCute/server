-module(buff_sql).
-export([save/1]).
-export([select/1]).
-export([delete/2]).
-include("buff.hrl").

%% @doc insert into buff
-spec save(BuffList :: [#buff{}] | ets:tab()) -> NewBuffList :: [#buff{}].
save(BuffList) ->
    db:save_into(<<"INSERT INTO `buff` (`role_id`, `buff_id`, `expire_time`, `overlap`) VALUES">>, <<"(?, ?, ?, ?)">>, <<"ON DUPLICATE KEY UPDATE `role_id` = VALUES(`role_id`), `buff_id` = VALUES(`buff_id`), `expire_time` = VALUES(`expire_time`), `overlap` = VALUES(`overlap`)">>, BuffList, fun(#buff{role_id = RoleId, buff_id = BuffId, expire_time = ExpireTime, overlap = Overlap}) -> [RoleId, BuffId, ExpireTime, Overlap] end, #buff.flag).

%% @doc select from buff
-spec select(RoleId :: non_neg_integer()) -> Rows :: [#buff{}].
select(RoleId) ->
    Data = db:select(<<"SELECT `role_id`, `buff_id`, `expire_time`, `overlap`, `flag` FROM `buff` WHERE `role_id` = ?">>, [RoleId]),
    parser:convert(Data, buff).

%% @doc delete row from buff
-spec delete(RoleId :: non_neg_integer(), BuffId :: non_neg_integer()) -> AffectedRows :: non_neg_integer().
delete(RoleId, BuffId) ->
    db:delete(<<"DELETE FROM `buff` WHERE `role_id` = ? AND `buff_id` = ?">>, [RoleId, BuffId]).

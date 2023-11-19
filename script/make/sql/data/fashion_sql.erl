-module(fashion_sql).
-export([save/1]).
-export([select/1]).
-export([select_by_fashion_id/1]).
-export([update_role_id/3]).
-export([delete/2]).
-include("fashion.hrl").

%% @doc insert into fashion
-spec save(FashionList :: [#fashion{}] | ets:tab()) -> NewFashionList :: [#fashion{}].
save(FashionList) ->
    db:save_into(<<"INSERT INTO `fashion` (`role_id`, `fashion_id`, `type`, `expire_time`) VALUES">>, <<"(:2:, :3:, :4:, :5:)">>, <<"ON DUPLICATE KEY UPDATE `role_id` = VALUES(`role_id`), `fashion_id` = VALUES(`fashion_id`), `type` = VALUES(`type`), `expire_time` = VALUES(`expire_time`)">>, FashionList, #fashion.flag).

%% @doc select from fashion
-spec select(RoleId :: non_neg_integer()) -> Rows :: [#fashion{}].
select(RoleId) ->
    Data = db:select(<<"SELECT `role_id`, `fashion_id`, `type`, `expire_time`, `flag` FROM `fashion` WHERE `role_id` = ?">>, [RoleId]),
    parser:convert(Data, fashion).

%% @doc select from fashion
-spec select_by_fashion_id(FashionId :: non_neg_integer()) -> Rows :: [#fashion{}].
select_by_fashion_id(FashionId) ->
    Data = db:select(<<"SELECT `role_id`, `fashion_id`, `type`, `expire_time`, `flag` FROM `fashion` WHERE `fashion_id` = ?">>, [FashionId]),
    parser:convert(Data, fashion).

%% @doc update into fashion
-spec update_role_id(Fashion :: #fashion{}, ByRoleId :: non_neg_integer(), ByFashionId :: non_neg_integer()) -> AffectedRows :: non_neg_integer().
update_role_id(#fashion{role_id = RoleId}, ByRoleId, ByFashionId) ->
    db:update(<<"UPDATE `fashion` SET `role_id` = ? WHERE `role_id` = ? AND `fashion_id` = ?">>, [RoleId, ByRoleId, ByFashionId]).

%% @doc delete row from fashion
-spec delete(RoleId :: non_neg_integer(), FashionId :: non_neg_integer()) -> AffectedRows :: non_neg_integer().
delete(RoleId, FashionId) ->
    db:delete(<<"DELETE FROM `fashion` WHERE `role_id` = ? AND `fashion_id` = ?">>, [RoleId, FashionId]).

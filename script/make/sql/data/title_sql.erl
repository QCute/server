-module(title_sql).
-export([save/1]).
-export([select/1]).
-export([select_by_title_id/1]).
-export([update_role_id/3]).
-export([delete/2]).
-include("title.hrl").

%% @doc insert into title
-spec save(TitleList :: [#title{}] | ets:tab()) -> NewTitleList :: [#title{}].
save(TitleList) ->
    db:save_into(<<"INSERT INTO `title` (`role_id`, `title_id`, `type`, `expire_time`) VALUES">>, <<"(:2:, :3:, :4:, :5:)">>, <<"ON DUPLICATE KEY UPDATE `role_id` = VALUES(`role_id`), `title_id` = VALUES(`title_id`), `type` = VALUES(`type`), `expire_time` = VALUES(`expire_time`)">>, TitleList, #title.flag).

%% @doc select from title
-spec select(RoleId :: non_neg_integer()) -> Rows :: [#title{}].
select(RoleId) ->
    Data = db:select(<<"SELECT `role_id`, `title_id`, `type`, `expire_time`, `flag` FROM `title` WHERE `role_id` = ?">>, [RoleId]),
    parser:convert(Data, title).

%% @doc select from title
-spec select_by_title_id(TitleId :: non_neg_integer()) -> Rows :: [#title{}].
select_by_title_id(TitleId) ->
    Data = db:select(<<"SELECT `role_id`, `title_id`, `type`, `expire_time`, `flag` FROM `title` WHERE `title_id` = ?">>, [TitleId]),
    parser:convert(Data, title).

%% @doc update into title
-spec update_role_id(Title :: #title{}, ByRoleId :: non_neg_integer(), ByTitleId :: non_neg_integer()) -> AffectedRows :: non_neg_integer().
update_role_id(#title{role_id = RoleId}, ByRoleId, ByTitleId) ->
    db:update(<<"UPDATE `title` SET `role_id` = ? WHERE `role_id` = ? AND `title_id` = ?">>, [RoleId, ByRoleId, ByTitleId]).

%% @doc delete row from title
-spec delete(RoleId :: non_neg_integer(), TitleId :: non_neg_integer()) -> AffectedRows :: non_neg_integer().
delete(RoleId, TitleId) ->
    db:delete(<<"DELETE FROM `title` WHERE `role_id` = ? AND `title_id` = ?">>, [RoleId, TitleId]).

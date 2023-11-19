-module(sign_sql).
-export([insert/1]).
-export([select/1]).
-export([update/1]).
-include("sign.hrl").

%% @doc insert into sign
-spec insert(Sign :: #sign{}) -> InsertIdOrAffectedRows :: non_neg_integer().
insert(Sign) ->
    db:insert(<<"INSERT INTO `sign` (`role_id`, `login_day`, `sign_total`, `is_sign_today`) VALUES (:2:, :3:, :4:, :5:)">>, Sign).

%% @doc select from sign
-spec select(RoleId :: non_neg_integer()) -> Rows :: [#sign{}].
select(RoleId) ->
    Data = db:select(<<"SELECT `role_id`, `login_day`, `sign_total`, `is_sign_today` FROM `sign` WHERE `role_id` = ?">>, [RoleId]),
    parser:convert(Data, sign).

%% @doc update into sign
-spec update(Sign :: #sign{}) -> AffectedRows :: non_neg_integer().
update(Sign) ->
    db:update(<<"UPDATE `sign` SET `login_day` = :3:, `sign_total` = :4:, `is_sign_today` = :5: WHERE `role_id` = :1:">>, Sign).

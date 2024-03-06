-module(sign_sql).
-export([insert/1]).
-export([select/1]).
-export([update/1]).
-include("sign.hrl").

%% @doc insert into sign
-spec insert(Sign :: #sign{}) -> InsertIdOrAffectedRows :: non_neg_integer().
insert(#sign{role_id = RoleId, login_day = LoginDay, sign_total = SignTotal, is_sign_today = IsSignToday}) ->
    db:insert(<<"INSERT INTO `sign` (`role_id`, `login_day`, `sign_total`, `is_sign_today`) VALUES (?, ?, ?, ?)">>, [RoleId, LoginDay, SignTotal, IsSignToday]).

%% @doc select from sign
-spec select(RoleId :: non_neg_integer()) -> Rows :: [#sign{}].
select(RoleId) ->
    Data = db:select(<<"SELECT `role_id`, `login_day`, `sign_total`, `is_sign_today` FROM `sign` WHERE `role_id` = ?">>, [RoleId]),
    parser:convert(Data, sign).

%% @doc update into sign
-spec update(#sign{}) -> AffectedRows :: non_neg_integer().
update(#sign{login_day = LoginDay, sign_total = SignTotal, is_sign_today = IsSignToday, role_id = RoleId}) ->
    db:update(<<"UPDATE `sign` SET `login_day` = ?, `sign_total` = ?, `is_sign_today` = ? WHERE `role_id` = ?">>, [LoginDay, SignTotal, IsSignToday, RoleId]).

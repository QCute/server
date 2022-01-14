-module(sign_sql).
-export([insert/1]).
-export([select/1]).
-export([update/1]).
-export([delete/1]).
-include("sign.hrl").

-define(INSERT_SIGN, <<"INSERT INTO `sign` (`role_id`, `login_day`, `sign_total`, `is_sign_today`) VALUES (~i~w, ~w, ~w, ~w)">>).
-define(SELECT_SIGN, <<"SELECT `role_id`, `login_day`, `sign_total`, `is_sign_today` FROM `sign` WHERE `role_id` = ~w">>).
-define(UPDATE_SIGN, {<<"UPDATE `sign` SET ~i~i`login_day` = ~w, `sign_total` = ~w, `is_sign_today` = ~w ">>, <<"WHERE `role_id` = ~w">>}).
-define(DELETE_SIGN, <<"DELETE FROM `sign` WHERE `role_id` = ~w">>).

%% @doc insert
-spec insert(Sign :: #sign{}) -> InsertIdOrAffectedRows :: non_neg_integer().
insert(Sign) ->
    Sql = parser:format(?INSERT_SIGN, Sign),
    db:insert(Sql).

%% @doc select
-spec select(RoleId :: integer()) -> SignList :: [#sign{}].
select(RoleId) ->
    Sql = parser:format(?SELECT_SIGN, [RoleId]),
    Data = db:select(Sql),
    parser:convert(Data, sign).

%% @doc update
-spec update(Sign :: #sign{}) -> AffectedRows :: non_neg_integer().
update(Sign) ->
    Sql = <<(parser:format(element(1, ?UPDATE_SIGN), Sign))/binary, (parser:format(element(2, ?UPDATE_SIGN), [Sign#sign.role_id]))/binary>>,
    db:update(Sql).

%% @doc delete
-spec delete(RoleId :: integer()) -> AffectedRows :: non_neg_integer().
delete(RoleId) ->
    Sql = parser:format(?DELETE_SIGN, [RoleId]),
    db:delete(Sql).


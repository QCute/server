-module(vip_sql).
-export([insert/1]).
-export([select/1]).
-export([update/1]).
-include("vip.hrl").

%% @doc insert into vip
-spec insert(Vip :: #vip{}) -> InsertIdOrAffectedRows :: non_neg_integer().
insert(Vip) ->
    db:insert(<<"INSERT INTO `vip` (`role_id`, `vip_level`, `exp`, `expire_time`) VALUES (:2:, :3:, :4:, :5:)">>, Vip).

%% @doc select from vip
-spec select(RoleId :: non_neg_integer()) -> Rows :: [#vip{}].
select(RoleId) ->
    Data = db:select(<<"SELECT `role_id`, `vip_level`, `exp`, `expire_time` FROM `vip` WHERE `role_id` = ?">>, [RoleId]),
    parser:convert(Data, vip).

%% @doc update into vip
-spec update(Vip :: #vip{}) -> AffectedRows :: non_neg_integer().
update(Vip) ->
    db:update(<<"UPDATE `vip` SET `vip_level` = :3:, `exp` = :4:, `expire_time` = :5: WHERE `role_id` = :1:">>, Vip).

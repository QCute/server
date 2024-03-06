-module(vip_sql).
-export([insert/1]).
-export([select/1]).
-export([update/1]).
-include("vip.hrl").

%% @doc insert into vip
-spec insert(Vip :: #vip{}) -> InsertIdOrAffectedRows :: non_neg_integer().
insert(#vip{role_id = RoleId, vip_level = VipLevel, exp = Exp, expire_time = ExpireTime}) ->
    db:insert(<<"INSERT INTO `vip` (`role_id`, `vip_level`, `exp`, `expire_time`) VALUES (?, ?, ?, ?)">>, [RoleId, VipLevel, Exp, ExpireTime]).

%% @doc select from vip
-spec select(RoleId :: non_neg_integer()) -> Rows :: [#vip{}].
select(RoleId) ->
    Data = db:select(<<"SELECT `role_id`, `vip_level`, `exp`, `expire_time` FROM `vip` WHERE `role_id` = ?">>, [RoleId]),
    parser:convert(Data, vip).

%% @doc update into vip
-spec update(#vip{}) -> AffectedRows :: non_neg_integer().
update(#vip{vip_level = VipLevel, exp = Exp, expire_time = ExpireTime, role_id = RoleId}) ->
    db:update(<<"UPDATE `vip` SET `vip_level` = ?, `exp` = ?, `expire_time` = ? WHERE `role_id` = ?">>, [VipLevel, Exp, ExpireTime, RoleId]).

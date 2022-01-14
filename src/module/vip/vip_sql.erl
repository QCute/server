-module(vip_sql).
-export([insert/1]).
-export([select/1]).
-export([update/1]).
-export([delete/1]).
-include("vip.hrl").

-define(INSERT_VIP, <<"INSERT INTO `vip` (`role_id`, `vip_level`, `exp`, `expire_time`) VALUES (~i~w, ~w, ~w, ~w)">>).
-define(SELECT_VIP, <<"SELECT `role_id`, `vip_level`, `exp`, `expire_time` FROM `vip` WHERE `role_id` = ~w">>).
-define(UPDATE_VIP, {<<"UPDATE `vip` SET ~i~i`vip_level` = ~w, `exp` = ~w, `expire_time` = ~w ">>, <<"WHERE `role_id` = ~w">>}).
-define(DELETE_VIP, <<"DELETE FROM `vip` WHERE `role_id` = ~w">>).

%% @doc insert
-spec insert(Vip :: #vip{}) -> InsertIdOrAffectedRows :: non_neg_integer().
insert(Vip) ->
    Sql = parser:format(?INSERT_VIP, Vip),
    db:insert(Sql).

%% @doc select
-spec select(RoleId :: integer()) -> VipList :: [#vip{}].
select(RoleId) ->
    Sql = parser:format(?SELECT_VIP, [RoleId]),
    Data = db:select(Sql),
    parser:convert(Data, vip).

%% @doc update
-spec update(Vip :: #vip{}) -> AffectedRows :: non_neg_integer().
update(Vip) ->
    Sql = <<(parser:format(element(1, ?UPDATE_VIP), Vip))/binary, (parser:format(element(2, ?UPDATE_VIP), [Vip#vip.role_id]))/binary>>,
    db:update(Sql).

%% @doc delete
-spec delete(RoleId :: integer()) -> AffectedRows :: non_neg_integer().
delete(RoleId) ->
    Sql = parser:format(?DELETE_VIP, [RoleId]),
    db:delete(Sql).


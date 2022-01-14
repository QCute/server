-module(bubble_sql).
-export([insert/1]).
-export([select/2]).
-export([update/1]).
-export([delete/2]).
-export([insert_update/1]).
-export([select_by_role_id/1]).
-include("bubble.hrl").

-define(INSERT_BUBBLE, <<"INSERT INTO `bubble` (`role_id`, `bubble_id`, `type`, `expire_time`) VALUES (~i~w, ~w, ~w, ~w~i)">>).
-define(SELECT_BUBBLE, <<"SELECT `role_id`, `bubble_id`, `type`, `expire_time`, 0 AS `flag` FROM `bubble` WHERE `role_id` = ~w AND `bubble_id` = ~w">>).
-define(UPDATE_BUBBLE, {<<"UPDATE `bubble` SET ~i~i~i`type` = ~w, `expire_time` = ~w~i ">>, <<"WHERE `role_id` = ~w AND `bubble_id` = ~w">>}).
-define(DELETE_BUBBLE, <<"DELETE FROM `bubble` WHERE `role_id` = ~w AND `bubble_id` = ~w">>).
-define(INSERT_UPDATE_BUBBLE, {<<"INSERT INTO `bubble` (`role_id`, `bubble_id`, `type`, `expire_time`) VALUES ">>, <<"(~i~w, ~w, ~w, ~w~i)">>, <<" ON DUPLICATE KEY UPDATE `type` = VALUES(`type`), `expire_time` = VALUES(`expire_time`)">>}).
-define(SELECT_BY_ROLE_ID, <<"SELECT `role_id`, `bubble_id`, `type`, `expire_time`, 0 AS `flag` FROM `bubble` WHERE `role_id` = ~w">>).
-define(SELECT_JOIN_BY_ROLE_ID, <<"SELECT `bubble`.`role_id`, `bubble`.`bubble_id`, `bubble`.`type`, `bubble`.`expire_time`, IFNULL(`bubble`.`flag`, 0) AS `flag` FROM `bubble` WHERE `bubble`.`role_id` = ~w">>).

%% @doc insert
-spec insert(Bubble :: #bubble{}) -> InsertIdOrAffectedRows :: non_neg_integer().
insert(Bubble) ->
    Sql = parser:format(?INSERT_BUBBLE, Bubble),
    db:insert(Sql).

%% @doc select
-spec select(RoleId :: integer(), BubbleId :: integer()) -> BubbleList :: [#bubble{}].
select(RoleId, BubbleId) ->
    Sql = parser:format(?SELECT_BUBBLE, [RoleId, BubbleId]),
    Data = db:select(Sql),
    parser:convert(Data, bubble).

%% @doc update
-spec update(Bubble :: #bubble{}) -> AffectedRows :: non_neg_integer().
update(Bubble) ->
    Sql = <<(parser:format(element(1, ?UPDATE_BUBBLE), Bubble))/binary, (parser:format(element(2, ?UPDATE_BUBBLE), [Bubble#bubble.role_id, Bubble#bubble.bubble_id]))/binary>>,
    db:update(Sql).

%% @doc delete
-spec delete(RoleId :: integer(), BubbleId :: integer()) -> AffectedRows :: non_neg_integer().
delete(RoleId, BubbleId) ->
    Sql = parser:format(?DELETE_BUBBLE, [RoleId, BubbleId]),
    db:delete(Sql).


%% @doc insert_update
-spec insert_update(BubbleList :: [#bubble{}] | ets:tab()) -> NewBubbleList :: [#bubble{}].
insert_update(BubbleList) ->
    {Sql, NewBubbleList} = parser:collect_into(BubbleList, ?INSERT_UPDATE_BUBBLE, #bubble.flag),
    db:insert(Sql),
    NewBubbleList.

%% @doc select
-spec select_by_role_id(RoleId :: integer()) -> BubbleList :: [#bubble{}].
select_by_role_id(RoleId) ->
    Sql = parser:format(?SELECT_BY_ROLE_ID, [RoleId]),
    Data = db:select(Sql),
    parser:convert(Data, bubble).


-module(bubble_sql).
-compile(nowarn_export_all).
-compile(export_all).
-include("bubble.hrl").
-define(INSERT_BUBBLE, <<"INSERT INTO `bubble` (`role_id`, `bubble_id`, `type`, `expire_time`) VALUES (~i~w, ~w, ~w, ~w~i)">>).
-define(SELECT_BUBBLE, <<"SELECT `role_id`, `bubble_id`, `type`, `expire_time`, 0 AS `flag` FROM `bubble` WHERE `role_id` = ~w AND `bubble_id` = ~w">>).
-define(UPDATE_BUBBLE, {<<"UPDATE `bubble` SET ~i~i~i`type` = ~w, `expire_time` = ~w~i ">>, <<"WHERE `role_id` = ~w AND `bubble_id` = ~w">>}).
-define(DELETE_BUBBLE, <<"DELETE  FROM `bubble` WHERE `role_id` = ~w AND `bubble_id` = ~w">>).
-define(INSERT_UPDATE_BUBBLE, {<<"INSERT INTO `bubble` (`role_id`, `bubble_id`, `type`, `expire_time`) VALUES ">>, <<"(~i~w, ~w, ~w, ~w~i)">>, <<" ON DUPLICATE KEY UPDATE `type` = VALUES(`type`), `expire_time` = VALUES(`expire_time`)">>}).
-define(SELECT_BY_ROLE_ID, <<"SELECT `role_id`, `bubble_id`, `type`, `expire_time`, 0 AS `flag` FROM `bubble` WHERE `role_id` = ~w">>).
-define(SELECT_JOIN_BY_ROLE_ID, <<"SELECT `bubble`.`role_id`, `bubble`.`bubble_id`, `bubble`.`type`, `bubble`.`expire_time`, IFNULL(`bubble`.`flag`, 0) AS `flag` FROM `bubble` WHERE `bubble`.`role_id` = ~w">>).

%% @doc insert
insert(Bubble) ->
    Sql = parser:format(?INSERT_BUBBLE, Bubble),
    db:insert(Sql).

%% @doc select
select(RoleId, BubbleId) ->
    Sql = parser:format(?SELECT_BUBBLE, [RoleId, BubbleId]),
    Data = db:select(Sql),
    parser:convert(Data, bubble).

%% @doc update
update(Bubble) ->
    Sql = <<(parser:format(element(1, ?UPDATE_BUBBLE), Bubble))/binary, (parser:format(element(2, ?UPDATE_BUBBLE), [Bubble#bubble.role_id, Bubble#bubble.bubble_id]))/binary>>,
    db:update(Sql).

%% @doc delete
delete(RoleId, BubbleId) ->
    Sql = parser:format(?DELETE_BUBBLE, [RoleId, BubbleId]),
    db:delete(Sql).


%% @doc insert_update
insert_update(Data) ->
    {Sql, NewData} = parser:collect_into(Data, ?INSERT_UPDATE_BUBBLE, #bubble.flag),
    db:insert(Sql),
    NewData.

%% @doc select
select_by_role_id(RoleId) ->
    Sql = parser:format(?SELECT_BY_ROLE_ID, [RoleId]),
    Data = db:select(Sql),
    parser:convert(Data, bubble).


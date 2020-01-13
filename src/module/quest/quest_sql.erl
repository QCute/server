-module(quest_sql).
-compile(nowarn_export_all).
-compile(export_all).
-include("quest.hrl").
-define(INSERT_QUEST, <<"INSERT INTO `quest` (`role_id`, `quest_id`, `type`, `event`, `target`, `number`, `compare`, `award`) VALUES (~w, ~w, ~w, '~w', ~w, ~w, '~w', ~w)">>).
-define(SELECT_QUEST, <<"SELECT `role_id`, `quest_id`, `type`, `event`, `target`, `number`, `compare`, `award`, 0 AS `flag` FROM `quest` WHERE `role_id` = ~w">>).
-define(UPDATE_QUEST, <<"UPDATE `quest` SET `quest_id` = ~w, `event` = '~w', `target` = ~w, `number` = ~w, `compare` = '~w', `award` = ~w WHERE `role_id` = ~w AND `type` = ~w">>).
-define(DELETE_QUEST, <<"DELETE  FROM `quest` WHERE `role_id` = ~w AND `type` = ~w">>).
-define(INSERT_UPDATE_QUEST, {<<"INSERT INTO `quest` (`role_id`, `quest_id`, `type`, `event`, `target`, `number`, `compare`, `award`) VALUES ">>, <<"(~w, ~w, ~w, '~w', ~w, ~w, '~w', ~w)">>, <<" ON DUPLICATE KEY UPDATE `quest_id` = VALUES(`quest_id`), `event` = VALUES(`event`), `target` = VALUES(`target`), `number` = VALUES(`number`), `compare` = VALUES(`compare`), `award` = VALUES(`award`)">>}).

%% @doc insert
insert(Quest) ->
    Sql = parser:format(?INSERT_QUEST, [
        Quest#quest.role_id,
        Quest#quest.quest_id,
        Quest#quest.type,
        Quest#quest.event,
        Quest#quest.target,
        Quest#quest.number,
        Quest#quest.compare,
        Quest#quest.award
    ]),
    sql:insert(Sql).

%% @doc select
select(RoleId) ->
    Sql = parser:format(?SELECT_QUEST, [RoleId]),
    Data = sql:select(Sql),
    F = fun(Quest = #quest{event = Event, compare = Compare}) -> Quest#quest{event = parser:to_term(Event), compare = parser:to_term(Compare)} end,
    parser:convert(Data, quest, F).

%% @doc update
update(Quest) ->
    Sql = parser:format(?UPDATE_QUEST, [
        Quest#quest.quest_id,
        Quest#quest.event,
        Quest#quest.target,
        Quest#quest.number,
        Quest#quest.compare,
        Quest#quest.award,
        Quest#quest.role_id,
        Quest#quest.type
    ]),
    sql:update(Sql).

%% @doc delete
delete(RoleId, Type) ->
    Sql = parser:format(?DELETE_QUEST, [RoleId, Type]),
    sql:delete(Sql).


%% @doc insert_update
insert_update(Data) ->
    F = fun(Quest) -> [
        Quest#quest.role_id,
        Quest#quest.quest_id,
        Quest#quest.type,
        Quest#quest.event,
        Quest#quest.target,
        Quest#quest.number,
        Quest#quest.compare,
        Quest#quest.award
    ] end,
    {Sql, NewData} = parser:collect_into(Data, F, ?INSERT_UPDATE_QUEST, #quest.flag),
    sql:insert(Sql),
    NewData.


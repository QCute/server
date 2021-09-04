-module(daily_active_sql).
-compile(nowarn_export_all).
-compile(export_all).
-include("daily.hrl").
-define(INSERT_DAILY_ACTIVE, <<"INSERT INTO `daily_active` (`role_id`, `stage_id`, `score`) VALUES (~i~w, ~w, ~w)">>).
-define(SELECT_DAILY_ACTIVE, <<"SELECT `role_id`, `stage_id`, `score` FROM `daily_active` WHERE `role_id` = ~w">>).
-define(UPDATE_DAILY_ACTIVE, {<<"UPDATE `daily_active` SET ~i~i`stage_id` = ~w, `score` = ~w ">>, <<"WHERE `role_id` = ~w">>}).
-define(DELETE_DAILY_ACTIVE, <<"DELETE  FROM `daily_active` WHERE `role_id` = ~w">>).

%% @doc insert
insert(DailyActive) ->
    Sql = parser:format(?INSERT_DAILY_ACTIVE, DailyActive),
    db:insert(Sql).

%% @doc select
select(RoleId) ->
    Sql = parser:format(?SELECT_DAILY_ACTIVE, [RoleId]),
    Data = db:select(Sql),
    parser:convert(Data, daily_active).

%% @doc update
update(DailyActive) ->
    Sql = <<(parser:format(element(1, ?UPDATE_DAILY_ACTIVE), DailyActive))/binary, (parser:format(element(2, ?UPDATE_DAILY_ACTIVE), [DailyActive#daily_active.role_id]))/binary>>,
    db:update(Sql).

%% @doc delete
delete(RoleId) ->
    Sql = parser:format(?DELETE_DAILY_ACTIVE, [RoleId]),
    db:delete(Sql).


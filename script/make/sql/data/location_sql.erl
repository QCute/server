-module(location_sql).
-export([insert/1]).
-export([select/1]).
-export([update/1]).
-include("map.hrl").

%% @doc insert into location
-spec insert(Location :: #location{}) -> InsertIdOrAffectedRows :: non_neg_integer().
insert(Location) ->
    db:insert(<<"INSERT INTO `location` (`role_id`, `map_no`, `map_id`, `pid`, `type`, `x`, `y`) VALUES (:1:, :2:, :3:, :4:, :5:, :6:, :7:)">>, Location).

%% @doc select from location
-spec select(RoleId :: non_neg_integer()) -> Rows :: [#location{}].
select(RoleId) ->
    Data = db:select(<<"SELECT `role_id`, `map_no`, `map_id`, `pid`, `type`, `x`, `y` FROM `location` WHERE `role_id` = ?">>, [RoleId]),
    parser:convert(Data, location, fun(Location = #location{pid = Pid, type = Type}) -> Location#location{pid = parser:to_term(Pid), type = parser:to_term(Type)} end).

%% @doc update into location
-spec update(Location :: #location{}) -> AffectedRows :: non_neg_integer().
update(Location) ->
    db:update(<<"UPDATE `location` SET `map_no` = :2:, `map_id` = :3:, `pid` = :4:, `type` = :5:, `x` = :6:, `y` = :7: WHERE `role_id` = :1:">>, Location).

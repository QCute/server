%%%-------------------------------------------------------------------
%%% @doc
%%% boss map
%%% @end
%%%-------------------------------------------------------------------
-module(boss_map).
%% API
-export([start/1]).
-export([update_hp/2]).
-export([award/2]).
%% Includes
-include("common.hrl").
-include("time.hrl").
-include("protocol.hrl").
-include("user.hrl").
-include("event.hrl").
-include("rank.hrl").
-include("map.hrl").
-include("attribute.hrl").
-include("monster.hrl").
%%%===================================================================
%%% API functions
%%%===================================================================
%% @doc start
-spec start(State :: #map{}) -> {ok, #map{}}.
start(State) ->
    %% add hurt/monster dead event
    {ok, battle_event:add_trigger(State, [#trigger{name = battle_monster_hurt, module = ?MODULE, function = update_hp}])}.

%% @doc handle battle hurt event
-spec update_hp(State :: #map{}, #battle_event{}) -> ok.
update_hp(_, #battle_event{target = #fighter{attribute = #attribute{hp = Hp}, data = #fighter_monster{monster_id = MonsterId}}}) when Hp > 0 ->
    boss_server:update_hp(MonsterId, Hp);
update_hp(State = #map{pid = Pid}, #battle_event{target = #fighter{attribute = #attribute{hp = Hp}, data = #fighter_monster{monster_id = MonsterId}}}) ->
    boss_server:update_hp(MonsterId, Hp),
    %% battle success
    #monster_data{award = Award} = monster_data:get(MonsterId),
    RankList = battle_rank:data(State),
    %% award
    [user_server:apply_cast(RoleId, ?MODULE, award, [listing:range_find(Rank, 1, 2, Award, [])]) || #rank{key = RoleId, order = Rank} <- RankList],
    %% stop map server
    map_server:stop(Pid, ?SECOND_MILLISECONDS),
    ok.

%% @doc award
-spec award(#user{}, list()) -> ok() | error().
award(User, AwardList) ->
    %% leave map, return main city
    NewUser = map_server:enter(User, map_server:city_map_no()),
    %% give award
    item:add(NewUser, AwardList, ?MODULE).

%%%===================================================================
%%% Internal functions
%%%===================================================================

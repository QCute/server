%%----------------------------------------------------
%% @doc
%% module player
%% @end
%%----------------------------------------------------
-module(player_condition).
-include("player.hrl").
-export([check/2]).
%%%===================================================================
%%% API
%%%===================================================================
%% @doc check user condition
-spec check(User :: #user{}, ConditionList :: list()) -> true | {false, non_neg_integer()}.
check(_, []) ->
    true;
%% no error code
check(User = #user{vip = #vip{vip = Vip}}, [{vip, Target} | T]) when Target =< Vip ->
    check(User, T);
check(User = #user{player = #assets{gold = Gold}}, [{gold, Target} | T]) when Target =< Gold ->
    check(User, T);
check(User = #user{player = #assets{silver = Silver}}, [{silver, Target} | T]) when Target =< Silver ->
    check(User, T);
check(User = #user{player = #assets{copper = Copper}}, [{copper, Target} | T]) when Target =< Copper ->
    check(User, T);
check(User = #user{player = #player{level = Level}}, [{level, Target} | T]) when Target =< Level ->
    check(User, T);
check(User = #user{player = #player{sex = Sex}}, [{sex, Sex} | T]) ->
    check(User, T);
check(User = #user{player = #player{classes = Classes}}, [{classes, Classes} | T]) ->
    check(User, T);
%% with error code
check(User = #user{vip = #vip{vip = Vip}}, [{vip, Target, _} | T]) when Target =< Vip ->
    check(User, T);
check(User = #user{player = #assets{gold = Gold}}, [{gold, Target, _} | T]) when Target =< Gold ->
    check(User, T);
check(User = #user{player = #assets{silver = Silver}}, [{silver, Target, _} | T]) when Target =< Silver ->
    check(User, T);
check(User = #user{player = #assets{copper = Copper}}, [{copper, Target, _} | T]) when Target =< Copper ->
    check(User, T);
check(User = #user{player = #player{level = Level}}, [{level, Target, _} | T]) when Target =< Level ->
    check(User, T);
check(User = #user{player = #player{sex = Sex}}, [{sex, Sex, _} | T]) ->
    check(User, T);
check(User = #user{player = #player{classes = Classes}}, [{classes, Classes, _} | T]) ->
    check(User, T);
%% return error code
check(_, [{_, _, Code} | _]) ->
    {false, Code};
%% default false
check(_, _) ->
    {false, 0}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
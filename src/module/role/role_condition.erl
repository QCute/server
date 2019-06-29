%%%-------------------------------------------------------------------
%%% @doc
%%% module role condition
%%% @end
%%%-------------------------------------------------------------------
-module(role_condition).
%% API
-export([check/2]).
%% Includes
-include("user.hrl").
-include("role.hrl").
-include("assets.hrl").
-include("vip.hrl").
%%%===================================================================
%%% API
%%%===================================================================
%% @doc check user condition
-spec check(User :: #user{}, ConditionList :: list()) -> ok | {error, non_neg_integer()}.
check(_, []) ->
    ok;
%% no error code
check(User = #user{vip = #vip{level = Level}}, [{vip, Target} | T]) when Target =< Level ->
    check(User, T);
check(User = #user{assets = #assets{gold = Gold}}, [{gold, Target} | T]) when Target =< Gold ->
    check(User, T);
check(User = #user{assets = #assets{silver = Silver}}, [{silver, Target} | T]) when Target =< Silver ->
    check(User, T);
check(User = #user{assets = #assets{copper = Copper}}, [{copper, Target} | T]) when Target =< Copper ->
    check(User, T);
check(User = #user{role = #role{level = Level}}, [{level, Target} | T]) when Target =< Level ->
    check(User, T);
check(User = #user{role = #role{sex = Sex}}, [{sex, Sex} | T]) ->
    check(User, T);
check(User = #user{role = #role{classes = Classes}}, [{classes, Classes} | T]) ->
    check(User, T);
%% with error code
check(User = #user{vip = #vip{level = Level}}, [{vip, Target, _} | T]) when Target =< Level ->
    check(User, T);
check(User = #user{assets = #assets{gold = Gold}}, [{gold, Target, _} | T]) when Target =< Gold ->
    check(User, T);
check(User = #user{assets = #assets{silver = Silver}}, [{silver, Target, _} | T]) when Target =< Silver ->
    check(User, T);
check(User = #user{assets = #assets{copper = Copper}}, [{copper, Target, _} | T]) when Target =< Copper ->
    check(User, T);
check(User = #user{role = #role{level = Level}}, [{level, Target, _} | T]) when Target =< Level ->
    check(User, T);
check(User = #user{role = #role{sex = Sex}}, [{sex, Sex, _} | T]) ->
    check(User, T);
check(User = #user{role = #role{classes = Classes}}, [{classes, Classes, _} | T]) ->
    check(User, T);

check(User, [{eq, X, X} | T]) ->
    check(User, T);
check(User, [{ne, X, Y} | T]) when X =/= Y ->
    check(User, T);
check(User, [{gt, X, Y} | T]) when X > Y ->
    check(User, T);
check(User, [{lt, X, Y} | T]) when X < Y ->
    check(User, T);
check(User, [{ge, X, Y} | T]) when X >= Y ->
    check(User, T);
check(User, [{le, X, Y} | T]) when X =< Y ->
    check(User, T);

check(User, [{eq, X, X, _} | T]) ->
    check(User, T);
check(User, [{ne, X, Y, _} | T]) when X =/= Y ->
    check(User, T);
check(User, [{gt, X, Y, _} | T]) when X > Y ->
    check(User, T);
check(User, [{lt, X, Y, _} | T]) when X < Y ->
    check(User, T);
check(User, [{ge, X, Y, _} | T]) when X >= Y ->
    check(User, T);
check(User, [{le, X, Y, _} | T]) when X =< Y ->
    check(User, T);

check(_User, [{r, eq, X, X} | _]) ->
    {error, 0};
check(_User, [{r, ne, X, Y} | _]) when X =/= Y ->
    {error, 0};
check(_User, [{r, gt, X, Y} | _]) when X > Y ->
    {error, 0};
check(_User, [{r, lt, X, Y} | _]) when X < Y ->
    {error, 0};
check(_User, [{r, ge, X, Y} | _]) when X >= Y ->
    {error, 0};
check(_User, [{r, le, X, Y} | _]) when X =< Y ->
    {error, 0};

check(_User, [{r, eq, X, X, Code} | _]) ->
    {error, Code};
check(_User, [{r, ne, X, Y, Code} | _]) when X =/= Y ->
    {error, Code};
check(_User, [{r, gt, X, Y, Code} | _]) when X > Y ->
    {error, Code};
check(_User, [{r, lt, X, Y, Code} | _]) when X < Y ->
    {error, Code};
check(_User, [{r, ge, X, Y, Code} | _]) when X >= Y ->
    {error, Code};
check(_User, [{r, le, X, Y, Code} | _]) when X =< Y ->
    {error, Code};

%% return error code
check(_, [{_, _, Code} | _]) ->
    {error, Code};
%% default false
check(_, _) ->
    {error, 0}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
%% normal compare check mode: (if condition true, continue, else return error with code(if given))
%% with (r) reverse mode: (if condition true, return error with code(if given), else continue)
%% eq     ==     equal
%% ne     =/=    not equal
%% gt     >      greater than
%% lt     <      less than
%% ge     >=     greater than or equal
%% le     =<     less than or equal
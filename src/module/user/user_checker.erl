%%%-------------------------------------------------------------------
%%% @doc
%%% module role condition
%%% @end
%%%-------------------------------------------------------------------
-module(user_checker).
%% API
-export([check/2]).
%% Includes
-include("user.hrl").
-include("role.hrl").
-include("asset.hrl").
-include("vip.hrl").
%%%===================================================================
%%% API
%%%===================================================================
%% @doc check user condition
-spec check(User :: #user{}, Condition :: [{Type :: atom(), Amount :: non_neg_integer()}] | [{Type :: atom(), Amount :: non_neg_integer(), Code :: non_neg_integer()}]) -> ok | {error, non_neg_integer()} | {error, atom()}.
check(_, []) ->
    ok;

%% no error code
check(User = #user{vip = #vip{level = Level}}, [{vip, Target} | T]) when Target =< Level ->
    check(User, T);
check(User = #user{asset = #asset{gold = Gold}}, [{gold, Target} | T]) when Target =< Gold ->
    check(User, T);
check(User = #user{asset = #asset{silver = Silver}}, [{silver, Target} | T]) when Target =< Silver ->
    check(User, T);
check(User = #user{asset = #asset{copper = Copper}}, [{copper, Target} | T]) when Target =< Copper ->
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
check(User = #user{asset = #asset{gold = Gold}}, [{gold, Target, _} | T]) when Target =< Gold ->
    check(User, T);
check(User = #user{asset = #asset{silver = Silver}}, [{silver, Target, _} | T]) when Target =< Silver ->
    check(User, T);
check(User = #user{asset = #asset{copper = Copper}}, [{copper, Target, _} | T]) when Target =< Copper ->
    check(User, T);
check(User = #user{role = #role{level = Level}}, [{level, Target, _} | T]) when Target =< Level ->
    check(User, T);
check(User = #user{role = #role{sex = Sex}}, [{sex, Sex, _} | T]) ->
    check(User, T);
check(User = #user{role = #role{classes = Classes}}, [{classes, Classes, _} | T]) ->
    check(User, T);

%% common compare mode
check(User, [{X, eq, X} | T]) ->
    check(User, T);
check(User, [{X, ne, Y} | T]) when X =/= Y ->
    check(User, T);
check(User, [{X, gt, Y} | T]) when X > Y ->
    check(User, T);
check(User, [{X, lt, Y} | T]) when X < Y ->
    check(User, T);
check(User, [{X, ge, Y} | T]) when X >= Y ->
    check(User, T);
check(User, [{X, le, Y} | T]) when X =< Y ->
    check(User, T);

%% common compare mode with error code
check(User, [{X, eq, X, _} | T]) ->
    check(User, T);
check(User, [{X, ne, Y, _} | T]) when X =/= Y ->
    check(User, T);
check(User, [{X, gt, Y, _} | T]) when X > Y ->
    check(User, T);
check(User, [{X, lt, Y, _} | T]) when X < Y ->
    check(User, T);
check(User, [{X, ge, Y, _} | T]) when X >= Y ->
    check(User, T);
check(User, [{X, le, Y, _} | T]) when X =< Y ->
    check(User, T);

%% return error code
check(_, [{_, _, _, Code} | _]) ->
    {error, Code};

%% return error code
check(_, [{_, _, Code} | _]) ->
    {error, Code};

%% default false
check(_, [{What, _} | _]) ->
    {error, What}.

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
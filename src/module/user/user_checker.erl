%%%------------------------------------------------------------------
%%% @doc
%%% module role condition
%%% @end
%%%------------------------------------------------------------------
-module(user_checker).
%% API
-export([check/2]).
%% Includes
-include("user.hrl").
-include("role.hrl").
-include("asset.hrl").
-include("vip.hrl").
%%%==================================================================
%%% API functions
%%%==================================================================
%% @doc check user condition
-spec check(User :: #user{}, Condition :: list()) -> {ok, list()} | {error, term()}.
check(User, Condition) ->
    check(User, Condition, []).

%%%==================================================================
%%% Internal functions
%%%==================================================================
check(_, [], Cost) ->
    {ok, Cost};
%% no error code
check(User = #user{vip = #vip{vip_level = VipLevel}}, [{vip, Value} | T], Cost) when Value =< VipLevel ->
    check(User, T, Cost);
check(User = #user{role = #role{level = Level}}, [{level, Value} | T], Cost) when Value =< Level ->
    check(User, T, Cost);
check(User = #user{role = #role{sex = Sex}}, [{sex, Sex} | T], Cost) ->
    check(User, T, Cost);
check(User = #user{role = #role{classes = Classes}}, [{classes, Classes} | T], Cost) ->
    check(User, T, Cost);

%% common compare mode
check(User, [{X, eq, X} | T], Cost) ->
    check(User, T, Cost);
check(User, [{X, ne, Y} | T], Cost) when X =/= Y ->
    check(User, T, Cost);
check(User, [{X, gt, Y} | T], Cost) when X > Y ->
    check(User, T, Cost);
check(User, [{X, lt, Y} | T], Cost) when X < Y ->
    check(User, T, Cost);
check(User, [{X, ge, Y} | T], Cost) when X >= Y ->
    check(User, T, Cost);
check(User, [{X, le, Y} | T], Cost) when X =< Y ->
    check(User, T, Cost);

%% common compare mode with error code
check(User, [{X, eq, X, _} | T], Cost) ->
    check(User, T, Cost);
check(User, [{X, ne, Y, _} | T], Cost) when X =/= Y ->
    check(User, T, Cost);
check(User, [{X, gt, Y, _} | T], Cost) when X > Y ->
    check(User, T, Cost);
check(User, [{X, lt, Y, _} | T], Cost) when X < Y ->
    check(User, T, Cost);
check(User, [{X, ge, Y, _} | T], Cost) when X >= Y ->
    check(User, T, Cost);
check(User, [{X, le, Y, _} | T], Cost) when X =< Y ->
    check(User, T, Cost);

%% default false
check(_, [{What, _} | _], _) ->
    {error, What};

%% return error reason
check(_, [{_, _, Reason} | _], _) ->
    {error, Reason};

%% return error reason
check(_, [{_, _, _, Reason} | _], _) ->
    {error, Reason}.


%% normal compare check mode: (if condition true, continue, else return error with code(if given))
%% with (r) reverse mode: (if condition true, return error with code(if given), else continue)
%% eq     ==     equal
%% ne     =/=    not equal
%% gt     >      greater than
%% lt     <      less than
%% ge     >=     greater than or equal
%% le     =<     less than or equal
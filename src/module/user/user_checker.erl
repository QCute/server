%%%-------------------------------------------------------------------
%%% @doc
%%% user condition checker
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
%%% API functions
%%%===================================================================
%% @doc check user condition
-spec check(User :: #user{}, Condition :: list()) -> ok | {error, term()}.
check(User, Condition) ->
    check_loop(User, Condition).

%%%===================================================================
%%% Internal functions
%%%===================================================================
check_loop(_, []) ->
    ok;

%% no error code
check_loop(User, [{Tag, Value} | T]) ->
    check_loop(User, [{Tag, Value, Tag} | T]);

check_loop(User = #user{vip = #vip{vip_level = VipLevel}}, [{vip, Value, _} | T]) when Value =< VipLevel ->
    check_loop(User, T);
check_loop(User = #user{role = #role{level = Level}}, [{level, Value, _} | T]) when Value =< Level ->
    check_loop(User, T);
check_loop(User = #user{role = #role{sex = Sex}}, [{sex, Sex, _} | T]) ->
    check_loop(User, T);
check_loop(User = #user{role = #role{classes = Classes}}, [{classes, Classes, _} | T]) ->
    check_loop(User, T);

%% common compare mode with error code
check_loop(User, [{X, eq, X, _} | T]) ->
    check_loop(User, T);
check_loop(User, [{X, '==', X, _} | T]) ->
    check_loop(User, T);
check_loop(User, [{X, ne, Y, _} | T]) when X =/= Y ->
    check_loop(User, T);
check_loop(User, [{X, '=/=', Y, _} | T]) when X =/= Y ->
    check_loop(User, T);
check_loop(User, [{X, gt, Y, _} | T]) when X > Y ->
    check_loop(User, T);
check_loop(User, [{X, '>', Y, _} | T]) when X > Y ->
    check_loop(User, T);
check_loop(User, [{X, lt, Y, _} | T]) when X < Y ->
    check_loop(User, T);
check_loop(User, [{X, '<', Y, _} | T]) when X < Y ->
    check_loop(User, T);
check_loop(User, [{X, ge, Y, _} | T]) when X >= Y ->
    check_loop(User, T);
check_loop(User, [{X, '>=', Y, _} | T]) when X >= Y ->
    check_loop(User, T);
check_loop(User, [{X, le, Y, _} | T]) when X =< Y ->
    check_loop(User, T);
check_loop(User, [{X, '=<', Y, _} | T]) when X =< Y ->
    check_loop(User, T);

%% return error reason
check_loop(_, [{_, _, Reason} | _]) ->
    {error, Reason};

%% return error reason
check_loop(_, [{_, _, _, Reason} | _]) ->
    {error, Reason}.


%% normal compare check mode: (if condition true, continue, else return error with code(if given))
%% with (r) reverse mode: (if condition true, return error with code(if given), else continue)
%% eq     ==     equal
%% ne     =/=    not equal
%% gt     >      greater than
%% lt     <      less than
%% ge     >=     greater than or equal
%% le     =<     less than or equal

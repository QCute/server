%%%------------------------------------------------------------------
%%% @doc
%%% module role condition
%%% @end
%%%------------------------------------------------------------------
-module(user_checker).
%% API
-export([check/2, check/3]).
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
    check(User, Condition, ?MODULE, []).

%% @doc check user condition
-spec check(User :: #user{}, Condition :: list(), From :: term()) -> {ok, list()} | {error, term()}.
check(User, Condition, From) ->
    check(User, Condition, From, []).

%%%==================================================================
%%% Internal functions
%%%==================================================================
check(_, [], _, Cost) ->
    {ok, Cost};
%% no error code
check(User = #user{vip = #vip{vip_level = VipLevel}}, [{vip, Value} | T], From, Cost) when Value =< VipLevel ->
    check(User, T, From, Cost);
check(User = #user{role = #role{level = Level}}, [{level, Value} | T], From, Cost) when Value =< Level ->
    check(User, T, From, Cost);
check(User = #user{role = #role{sex = Sex}}, [{sex, Sex} | T], From, Cost) ->
    check(User, T, From, Cost);
check(User = #user{role = #role{classes = Classes}}, [{classes, Classes} | T], From, Cost) ->
    check(User, T, From, Cost);

%% common compare mode
check(User, [{X, eq, X} | T], From, Cost) ->
    check(User, T, From, Cost);
check(User, [{X, ne, Y} | T], From, Cost) when X =/= Y ->
    check(User, T, From, Cost);
check(User, [{X, gt, Y} | T], From, Cost) when X > Y ->
    check(User, T, From, Cost);
check(User, [{X, lt, Y} | T], From, Cost) when X < Y ->
    check(User, T, From, Cost);
check(User, [{X, ge, Y} | T], From, Cost) when X >= Y ->
    check(User, T, From, Cost);
check(User, [{X, le, Y} | T], From, Cost) when X =< Y ->
    check(User, T, From, Cost);

%% common compare mode with error code
check(User, [{X, eq, X, _} | T], From, Cost) ->
    check(User, T, From, Cost);
check(User, [{X, ne, Y, _} | T], From, Cost) when X =/= Y ->
    check(User, T, From, Cost);
check(User, [{X, gt, Y, _} | T], From, Cost) when X > Y ->
    check(User, T, From, Cost);
check(User, [{X, lt, Y, _} | T], From, Cost) when X < Y ->
    check(User, T, From, Cost);
check(User, [{X, ge, Y, _} | T], From, Cost) when X >= Y ->
    check(User, T, From, Cost);
check(User, [{X, le, Y, _} | T], From, Cost) when X =< Y ->
    check(User, T, From, Cost);

%% seeming asset or item
check(User, [What = {_, _} | T], From, Cost) ->
    case item:check(User, [What], From) of
        {ok, Result} ->
            check(User, T, From, listing:merge(Result, Cost));
        Error ->
            Error
    end;

%% return error reason
check(_, [{_, _, Reason} | _], _, _) ->
    {error, Reason};

%% return error reason
check(_, [{_, _, _, Reason} | _], _, _) ->
    {error, Reason}.


%% normal compare check mode: (if condition true, continue, else return error with code(if given))
%% with (r) reverse mode: (if condition true, return error with code(if given), else continue)
%% eq     ==     equal
%% ne     =/=    not equal
%% gt     >      greater than
%% lt     <      less than
%% ge     >=     greater than or equal
%% le     =<     less than or equal

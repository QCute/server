%%%-------------------------------------------------------------------
%%% @doc
%%% sign
%%% @end
%%%-------------------------------------------------------------------
-module(sign).
%% API
-export([load/1]).
-export([reset/1]).
-export([sign/1]).
%% Includes
-include("common.hrl").
-include("user.hrl").
-include("sign.hrl").
%%%===================================================================
%%% API functions
%%%===================================================================
%% @doc load
-spec load(User :: #user{}) -> NewUser :: #user{}.
load(User = #user{role_id = RoleId}) ->
    case sign_sql:select(RoleId) of
        [Sign = #sign{login_day = LoginDay}] ->
            NewSign = Sign#sign{login_day = LoginDay + 1},
            sign_sql:update(NewSign);
        [] ->
            NewSign = #sign{role_id = RoleId, login_day = 1},
            sign_sql:insert(NewSign)
    end,
    User#user{sign = NewSign}.

%% @doc reset
-spec reset(User :: #user{}) -> NewUser :: #user{}.
reset(User = #user{sign = Sign}) ->
    NewSign = Sign#sign{is_sign_today = 0},
    sign_sql:update(NewSign),
    User#user{sign = NewSign}.

%% @doc sign
-spec sign(User :: #user{}) -> ok() | error().
sign(User = #user{sign = #sign{is_sign_today = SignToday}}) ->
    case SignToday of
        0 ->
            award(User);
        _ ->
            {error, signed_already}
    end.

award(User = #user{sign = Sign = #sign{sign_total = SignTotal}}) ->
    Award = sign_data:get(SignTotal rem 7 + 1),
    case item:add(User, Award, ?MODULE) of
        {ok, NewUser} ->
            NewSign = Sign#sign{sign_total = SignTotal + 1, is_sign_today = 1},
            sign_sql:update(NewSign),
            {ok, ok, NewUser#user{sign = NewSign}};
        _ ->
            {error, award_error}
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

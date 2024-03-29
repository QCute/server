-module(role_data).
-export([min_level/0]).
-export([max_level/0]).
-export([level/1]).
-export([exp/1]).
-export([sex/1]).
-export([classes/1]).

-include("role.hrl").

-spec min_level() -> MinLevel :: integer().
min_level() ->
    0.


-spec max_level() -> MaxLevel :: integer().
max_level() ->
    9.


-spec level(Exp :: integer()) -> Level :: integer() | Default :: integer().
level(Exp) when Exp > 1000 ->
    9;
level(Exp) when Exp > 900 ->
    8;
level(Exp) when Exp > 800 ->
    7;
level(Exp) when Exp > 700 ->
    6;
level(Exp) when Exp > 600 ->
    5;
level(Exp) when Exp > 500 ->
    4;
level(Exp) when Exp > 400 ->
    3;
level(Exp) when Exp > 300 ->
    2;
level(Exp) when Exp > 200 ->
    1;
level(Exp) when Exp > 100 ->
    0;
level(_Exp) ->
    0.


-spec exp(Level :: integer()) -> Exp :: integer() | Default :: integer().
exp(0) ->
    100;
exp(1) ->
    200;
exp(2) ->
    300;
exp(3) ->
    400;
exp(4) ->
    500;
exp(5) ->
    600;
exp(6) ->
    700;
exp(7) ->
    800;
exp(8) ->
    900;
exp(9) ->
    1000;
exp(_Level) ->
    0.


-spec sex(Sex :: integer()) -> Name :: binary() | Default :: binary().
sex(1) ->
    <<"男"/utf8>>;
sex(2) ->
    <<"女"/utf8>>;
sex(_Sex) ->
    <<>>.


-spec classes(Classes :: integer()) -> Name :: binary() | Default :: binary().
classes(1) ->
    <<"七杀"/utf8>>;
classes(2) ->
    <<"天师"/utf8>>;
classes(3) ->
    <<"飞羽"/utf8>>;
classes(4) ->
    <<"御灵"/utf8>>;
classes(5) ->
    <<"妙音"/utf8>>;
classes(6) ->
    <<"星术"/utf8>>;
classes(_Classes) ->
    <<>>.


